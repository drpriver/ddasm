/*
 * Copyright © 2025, David Priver
 *
 * Float-aware native call trampoline.
 * Properly routes integer args to GPRs and float args to XMM/vector registers
 * according to platform ABI.
 *
 * arg_types encoding (2 bits per arg):
 *   00 = integer/pointer (64-bit, GPR)
 *   01 = float32 (32-bit, XMM)
 *   10 = float64/double (64-bit, XMM)
 *   11 = reserved
 */
module dvm.native_trampoline_float;

import dvm.dvm_defs: uintptr_t;

enum ArgType : ubyte {
    INT = 0,      // 00 - integer/pointer
    FLOAT32 = 1,  // 01 - float (32-bit)
    FLOAT64 = 2,  // 10 - double (64-bit)
}

// Extract type for arg N from mask
ArgType get_arg_type(ulong type_mask, size_t n) {
    return cast(ArgType)((type_mask >> (n * 2)) & 0x3);
}

// Check if type is a float type
bool is_float_type(ArgType t) {
    return t == ArgType.FLOAT32 || t == ArgType.FLOAT64;
}

version(X86_64) {
    version(Posix) {
        import ldc.llvmasm;

        // System V AMD64 ABI (Linux and macOS):
        // - Integer/pointer args in: rdi, rsi, rdx, rcx, r8, r9 (6 regs)
        // - Float/double args in: xmm0-xmm7 (8 regs)
        // - Integer and float registers are assigned independently
        // - AL = number of XMM registers used (for varargs)
        // - Integer return in RAX, float return in XMM0
        // - For structs <=16 bytes with floats: return in XMM0 and XMM1
        // - Stack must be 16-byte aligned before call
        // - Large structs (>16 bytes): MEMORY class, data copied to stack
        uintptr_t call_native_float(
            void* func_ptr,
            uintptr_t* args,
            size_t n_args,
            ulong arg_types,
            ubyte ret_types,
            uintptr_t* ret2,  // Optional: store second XMM return register here
            ushort* struct_arg_sizes  // Optional: sizes of struct args that need stack copy
        ) {
            // Separate args into integer and float categories
            uintptr_t[6] int_args = 0;
            uintptr_t[8] float_args = 0;
            ubyte[8] float_is_32 = 0;  // Track which float args are 32-bit
            size_t n_int = 0;
            size_t n_float = 0;

            // Stack args (when registers overflow)
            uintptr_t[32] stack_args = void;
            size_t n_stack = 0;

            // Large struct args: pointer and size pairs for stack copying
            // System V ABI: structs >16 bytes are MEMORY class, data copied to stack
            uintptr_t[16] struct_ptrs = void;
            ushort[16] struct_sizes = void;
            size_t n_structs = 0;
            size_t struct_total_bytes = 0;

            for (size_t i = 0; i < n_args && i < 32; i++) {
                // Check if this arg is a large struct that needs stack copying
                ushort struct_size = (struct_arg_sizes !is null) ? struct_arg_sizes[i] : 0;
                if (struct_size > 0) {
                    // Large struct: args[i] is a POINTER to the data
                    // Store for later stack copy
                    struct_ptrs[n_structs] = args[i];  // pointer to struct data
                    struct_sizes[n_structs] = struct_size;
                    struct_total_bytes += (struct_size + 7) & ~cast(size_t)7;  // 8-byte align
                    n_structs++;
                    continue;  // Don't put in register
                }

                ArgType t = get_arg_type(arg_types, i);
                if (is_float_type(t)) {
                    if (n_float < 8) {
                        float_args[n_float] = args[i];
                        float_is_32[n_float] = (t == ArgType.FLOAT32) ? 1 : 0;
                        n_float++;
                    } else {
                        stack_args[n_stack++] = args[i];
                    }
                } else {
                    if (n_int < 6) {
                        int_args[n_int++] = args[i];
                    } else {
                        stack_args[n_stack++] = args[i];
                    }
                }
            }

            // Calculate stack space: struct data + overflow args (16-byte aligned)
            size_t stack_bytes = struct_total_bytes + n_stack * 8;
            stack_bytes = (stack_bytes + 15) & ~cast(size_t)15;

            // If we have struct args, pre-copy all data into unified stack buffer
            // This buffer contains: [struct0 data][struct1 data]...[overflow arg0][overflow arg1]...
            ubyte[512] unified_stack = void;
            if (n_structs > 0) {
                size_t offset = 0;
                // Copy struct data first
                for (size_t i = 0; i < n_structs; i++) {
                    ubyte* src = cast(ubyte*)struct_ptrs[i];
                    ubyte* dst = unified_stack.ptr + offset;
                    size_t sz = struct_sizes[i];
                    for (size_t j = 0; j < sz; j++) {
                        dst[j] = src[j];
                    }
                    offset += (sz + 7) & ~cast(size_t)7;  // 8-byte align
                }
                // Copy overflow args
                for (size_t i = 0; i < n_stack; i++) {
                    *cast(uintptr_t*)(unified_stack.ptr + offset) = stack_args[i];
                    offset += 8;
                }
                // Update stack_args to point to unified buffer, and count to total qwords
                size_t total_qwords = (struct_total_bytes + n_stack * 8 + 7) / 8;
                for (size_t i = 0; i < total_qwords; i++) {
                    stack_args[i] = (cast(uintptr_t*)unified_stack.ptr)[i];
                }
                n_stack = total_qwords;
            }

            // We need to pass:
            // - 6 integer args in rdi, rsi, rdx, rcx, r8, r9
            // - 8 float args in xmm0-xmm7
            // - AL = n_float (capped at 8)
            // - Stack args
            // - func_ptr

            // Use inline asm to set up all registers and call
            // rbx = saved rsp (callee-saved)
            // r10 = func_ptr
            // r11 = stack_args ptr
            // r12 = n_stack
            // r13 = stack_bytes
            // r14, r15 = scratch for stack copy loop

            // Check if first return value is a float type
            bool float_ret = (ret_types & 0x3) != 0;
            // Check if there's a second float return value
            bool float_ret2 = ((ret_types >> 2) & 0x3) != 0;

            if (float_ret && float_ret2 && ret2 !is null) {
                // Two-register float return: capture both xmm0 and xmm1
                // Strategy: Append ret2 pointer to stack_args array, then after the
                // call, load it from there and write xmm1 to it.
                // Note: r11 is caller-saved (will be clobbered), r12/r13 are callee-saved.
                // Important: Must maintain 16-byte stack alignment for the call.
                stack_args[n_stack] = cast(uintptr_t)ret2;
                return __asm!uintptr_t(
                    "movq %rsp, %rbx\n" ~      // Save original rsp
                    // Allocate 16 bytes for r11 storage (maintain 16-byte alignment)
                    // We started with rsp=16n+8, subtracting 16 gives 16n+8-16=16(n-1)+8
                    // which is still the correct form for calling
                    "subq $$16, %rsp\n" ~
                    "movq %r11, (%rsp)\n" ~    // Save r11 (stack_args.ptr)
                    "subq %r13, %rsp\n" ~      // stack_bytes (already 16-byte aligned)
                    "testq %r12, %r12\n" ~
                    "jz 2f\n" ~
                    "xorq %r14, %r14\n" ~
                    "movq %rsp, %r15\n" ~
                    "1:\n" ~
                    "movq (%r11, %r14, 8), %rax\n" ~
                    "movq %rax, (%r15)\n" ~
                    "addq $$8, %r15\n" ~
                    "addq $$1, %r14\n" ~
                    "cmpq %r12, %r14\n" ~
                    "jb 1b\n" ~
                    "2:\n" ~
                    "callq *%r10\n" ~
                    // After call: xmm0 and xmm1 have return values
                    // Restore r11 from saved location (rbx - 16, where we saved it)
                    "movq -16(%rbx), %r11\n" ~
                    // r12 (n_stack) is callee-saved, so it's still valid
                    // Load ret2 pointer from stack_args[n_stack] (at offset r12*8 from r11)
                    "movq (%r11, %r12, 8), %rcx\n" ~
                    "movq %xmm1, (%rcx)\n" ~   // Store xmm1 to *ret2
                    "movq %xmm0, %rax\n" ~     // Return xmm0 in rax
                    "movq %rbx, %rsp",         // Restore stack
                    "={rax},{rdi},{rsi},{rdx},{rcx},{r8},{r9},{xmm0},{xmm1},{xmm2},{xmm3},{xmm4},{xmm5},{xmm6},{xmm7},{al},{r10},{r11},{r12},{r13},~{rbx},~{r14},~{r15},~{xmm0},~{xmm1},~{memory}",
                    int_args[0], int_args[1], int_args[2], int_args[3], int_args[4], int_args[5],
                    float_args[0], float_args[1], float_args[2], float_args[3],
                    float_args[4], float_args[5], float_args[6], float_args[7],
                    cast(ubyte)n_float,
                    func_ptr, stack_args.ptr, n_stack, stack_bytes
                );
            } else if (float_ret && !float_ret2 && ret2 !is null) {
                // Mixed return: XMM0 (floats) + RAX (ints)
                // First return in XMM0, second in RAX
                stack_args[n_stack] = cast(uintptr_t)ret2;
                return __asm!uintptr_t(
                    "movq %rsp, %rbx\n" ~
                    "subq $$16, %rsp\n" ~
                    "movq %r11, (%rsp)\n" ~
                    "subq %r13, %rsp\n" ~
                    "testq %r12, %r12\n" ~
                    "jz 2f\n" ~
                    "xorq %r14, %r14\n" ~
                    "movq %rsp, %r15\n" ~
                    "1:\n" ~
                    "movq (%r11, %r14, 8), %rcx\n" ~
                    "movq %rcx, (%r15)\n" ~
                    "addq $$8, %r15\n" ~
                    "addq $$1, %r14\n" ~
                    "cmpq %r12, %r14\n" ~
                    "jb 1b\n" ~
                    "2:\n" ~
                    "callq *%r10\n" ~
                    // After call: xmm0 has first return (floats), rax has second (ints)
                    "movq -16(%rbx), %r11\n" ~
                    "movq (%r11, %r12, 8), %rcx\n" ~
                    "movq %rax, (%rcx)\n" ~   // Store rax (ints) to *ret2
                    "movq %xmm0, %rax\n" ~     // Return xmm0 (floats) in rax
                    "movq %rbx, %rsp",
                    "={rax},{rdi},{rsi},{rdx},{rcx},{r8},{r9},{xmm0},{xmm1},{xmm2},{xmm3},{xmm4},{xmm5},{xmm6},{xmm7},{al},{r10},{r11},{r12},{r13},~{rbx},~{r14},~{r15},~{xmm0},~{memory}",
                    int_args[0], int_args[1], int_args[2], int_args[3], int_args[4], int_args[5],
                    float_args[0], float_args[1], float_args[2], float_args[3],
                    float_args[4], float_args[5], float_args[6], float_args[7],
                    cast(ubyte)n_float,
                    func_ptr, stack_args.ptr, n_stack, stack_bytes
                );
            } else if (float_ret) {
                // Single float return: move xmm0 to rax after call
                return __asm!uintptr_t(
                    "movq %rsp, %rbx\n" ~
                    "subq %r13, %rsp\n" ~
                    "testq %r12, %r12\n" ~
                    "jz 2f\n" ~
                    "xorq %r14, %r14\n" ~
                    "movq %rsp, %r15\n" ~
                    "1:\n" ~
                    "movq (%r11, %r14, 8), %rax\n" ~
                    "movq %rax, (%r15)\n" ~
                    "addq $$8, %r15\n" ~
                    "addq $$1, %r14\n" ~
                    "cmpq %r12, %r14\n" ~
                    "jb 1b\n" ~
                    "2:\n" ~
                    "callq *%r10\n" ~
                    "movq %xmm0, %rax\n" ~  // Move float return to rax
                    "movq %rbx, %rsp",
                    "={rax},{rdi},{rsi},{rdx},{rcx},{r8},{r9},{xmm0},{xmm1},{xmm2},{xmm3},{xmm4},{xmm5},{xmm6},{xmm7},{al},{r10},{r11},{r12},{r13},~{rbx},~{r14},~{r15},~{xmm0},~{memory}",
                    int_args[0], int_args[1], int_args[2], int_args[3], int_args[4], int_args[5],
                    float_args[0], float_args[1], float_args[2], float_args[3],
                    float_args[4], float_args[5], float_args[6], float_args[7],
                    cast(ubyte)n_float,
                    func_ptr, stack_args.ptr, n_stack, stack_bytes
                );
            } else {
                // Integer return: rax is already correct
                return __asm!uintptr_t(
                    "movq %rsp, %rbx\n" ~
                    "subq %r13, %rsp\n" ~
                    "testq %r12, %r12\n" ~
                    "jz 2f\n" ~
                    "xorq %r14, %r14\n" ~
                    "movq %rsp, %r15\n" ~
                    "1:\n" ~
                    "movq (%r11, %r14, 8), %rax\n" ~
                    "movq %rax, (%r15)\n" ~
                    "addq $$8, %r15\n" ~
                    "addq $$1, %r14\n" ~
                    "cmpq %r12, %r14\n" ~
                    "jb 1b\n" ~
                    "2:\n" ~
                    "callq *%r10\n" ~
                    "movq %rbx, %rsp",
                    "={rax},{rdi},{rsi},{rdx},{rcx},{r8},{r9},{xmm0},{xmm1},{xmm2},{xmm3},{xmm4},{xmm5},{xmm6},{xmm7},{al},{r10},{r11},{r12},{r13},~{rbx},~{r14},~{r15},~{memory}",
                    int_args[0], int_args[1], int_args[2], int_args[3], int_args[4], int_args[5],
                    float_args[0], float_args[1], float_args[2], float_args[3],
                    float_args[4], float_args[5], float_args[6], float_args[7],
                    cast(ubyte)n_float,
                    func_ptr, stack_args.ptr, n_stack, stack_bytes
                );
            }
        }
    }
    else version(Windows) {
        import ldc.llvmasm;

        // Microsoft x64 ABI:
        // - First 4 args use slots: rcx/xmm0, rdx/xmm1, r8/xmm2, r9/xmm3
        // - Type determines which register in the slot pair
        // - Additional args on stack after 32-byte shadow space
        // - For varargs: float values go in BOTH GPR and XMM
        // - Integer return in RAX, float return in XMM0
        // - Stack must be 16-byte aligned before call
        uintptr_t call_native_float(
            void* func_ptr,
            uintptr_t* args,
            size_t n_args,
            ulong arg_types,
            ubyte ret_types = 0
        ) {
            // Windows uses positional slots - arg N uses slot N's registers
            uintptr_t[4] gpr_args = 0;   // rcx, rdx, r8, r9
            uintptr_t[4] xmm_args = 0;   // xmm0, xmm1, xmm2, xmm3
            ubyte[4] slot_is_float = 0;  // Track which slots use XMM

            // Stack args (beyond first 4)
            uintptr_t[32] stack_args = void;
            size_t n_stack = 0;

            for (size_t i = 0; i < n_args && i < 32; i++) {
                ArgType t = get_arg_type(arg_types, i);
                if (i < 4) {
                    // First 4 args go in register slots
                    if (is_float_type(t)) {
                        xmm_args[i] = args[i];
                        slot_is_float[i] = 1;
                        // For varargs compatibility, also put in GPR
                        gpr_args[i] = args[i];
                    } else {
                        gpr_args[i] = args[i];
                    }
                } else {
                    stack_args[n_stack++] = args[i];
                }
            }

            // Calculate stack space: 32 bytes shadow + stack args (16-byte aligned)
            size_t stack_bytes = 32 + n_stack * 8;
            stack_bytes = (stack_bytes + 15) & ~cast(size_t)15;

            // On Windows, we always set both GPR and XMM for the first 4 args
            // This handles both varargs and non-varargs cases correctly
            bool float_ret = (ret_types & 0x3) != 0;

            if (float_ret) {
                return __asm!uintptr_t(
                    "movq %rsp, %rbx\n" ~
                    "subq %r13, %rsp\n" ~
                    "movq %rcx, 0(%rsp)\n" ~
                    "movq %rdx, 8(%rsp)\n" ~
                    "movq %r8, 16(%rsp)\n" ~
                    "movq %r9, 24(%rsp)\n" ~
                    "testq %r12, %r12\n" ~
                    "jz 2f\n" ~
                    "xorq %r14, %r14\n" ~
                    "leaq 32(%rsp), %r15\n" ~
                    "1:\n" ~
                    "movq (%r11, %r14, 8), %rax\n" ~
                    "movq %rax, (%r15)\n" ~
                    "addq $$8, %r15\n" ~
                    "addq $$1, %r14\n" ~
                    "cmpq %r12, %r14\n" ~
                    "jb 1b\n" ~
                    "2:\n" ~
                    "callq *%r10\n" ~
                    "movq %xmm0, %rax\n" ~  // Move float return to rax
                    "movq %rbx, %rsp",
                    "={rax},{rcx},{rdx},{r8},{r9},{xmm0},{xmm1},{xmm2},{xmm3},{r10},{r11},{r12},{r13},~{rbx},~{r14},~{r15},~{xmm0},~{memory}",
                    gpr_args[0], gpr_args[1], gpr_args[2], gpr_args[3],
                    xmm_args[0], xmm_args[1], xmm_args[2], xmm_args[3],
                    func_ptr, stack_args.ptr, n_stack, stack_bytes
                );
            } else {
                return __asm!uintptr_t(
                    "movq %rsp, %rbx\n" ~
                    "subq %r13, %rsp\n" ~
                    "movq %rcx, 0(%rsp)\n" ~
                    "movq %rdx, 8(%rsp)\n" ~
                    "movq %r8, 16(%rsp)\n" ~
                    "movq %r9, 24(%rsp)\n" ~
                    "testq %r12, %r12\n" ~
                    "jz 2f\n" ~
                    "xorq %r14, %r14\n" ~
                    "leaq 32(%rsp), %r15\n" ~
                    "1:\n" ~
                    "movq (%r11, %r14, 8), %rax\n" ~
                    "movq %rax, (%r15)\n" ~
                    "addq $$8, %r15\n" ~
                    "addq $$1, %r14\n" ~
                    "cmpq %r12, %r14\n" ~
                    "jb 1b\n" ~
                    "2:\n" ~
                    "callq *%r10\n" ~
                    "movq %rbx, %rsp",
                    "={rax},{rcx},{rdx},{r8},{r9},{xmm0},{xmm1},{xmm2},{xmm3},{r10},{r11},{r12},{r13},~{rbx},~{r14},~{r15},~{memory}",
                    gpr_args[0], gpr_args[1], gpr_args[2], gpr_args[3],
                    xmm_args[0], xmm_args[1], xmm_args[2], xmm_args[3],
                    func_ptr, stack_args.ptr, n_stack, stack_bytes
                );
            }
        }
    }
    else {
        uintptr_t call_native_float(void* func_ptr, uintptr_t* args, size_t n_args, ulong arg_types, ubyte ret_types = 0) {
            assert(0, "x86_64 float trampoline not implemented for this OS");
        }
    }
}
else version(AArch64) {
    import ldc.llvmasm;

    // ARM64 AAPCS64 (Linux) / Apple ARM64:
    // - Integer args in x0-x7 (8 regs)
    // - Float/double args in d0-d7 (8 regs)
    // - Integer and float registers assigned independently
    // - Integer return in x0, float return in d0
    // - Stack must be 16-byte aligned
    uintptr_t call_native_float(
        void* func_ptr,
        uintptr_t* args,
        size_t n_args,
        ulong arg_types,
        ubyte ret_types,
        uintptr_t* ret2, ushort* struct_arg_sizes,
    ) {
        // Separate args into integer and float categories
        uintptr_t[8] int_args = 0;
        uintptr_t[8] float_args = 0;
        size_t n_int = 0;
        size_t n_float = 0;

        // Stack args (when registers overflow)
        uintptr_t[32] stack_args = void;
        size_t n_stack = 0;

        for (size_t i = 0; i < n_args && i < 32; i++) {
            ArgType t = get_arg_type(arg_types, i);
            if (is_float_type(t)) {
                if (n_float < 8) {
                    float_args[n_float++] = args[i];
                } else {
                    stack_args[n_stack++] = args[i];
                }
            } else {
                if (n_int < 8) {
                    int_args[n_int++] = args[i];
                } else {
                    stack_args[n_stack++] = args[i];
                }
            }
        }

        // Calculate stack space (16-byte aligned)
        size_t stack_bytes = ((n_stack * 8) + 15) & ~cast(size_t)15;

        // x9 = func_ptr, x10 = stack_args, x11 = n_stack, x12 = stack_bytes
        // x19 = saved SP (callee-saved)
        bool float_ret = (ret_types & 0x3) != 0;

        if (float_ret) {
            // Float return: move d0 to x0 after call
            return __asm!uintptr_t(
                "mov x19, sp\n" ~
                "sub sp, sp, x12\n" ~
                "mov x13, sp\n" ~
                "mov x14, x10\n" ~
                "mov x15, x11\n" ~
                "cbz x15, 2f\n" ~
                "1:\n" ~
                "ldr x16, [x14], #8\n" ~
                "str x16, [x13], #8\n" ~
                "subs x15, x15, #1\n" ~
                "b.ne 1b\n" ~
                "2:\n" ~
                "blr x9\n" ~
                "fmov x0, d0\n" ~  // Move float return to x0
                "mov sp, x19",
                "={x0},{x0},{x1},{x2},{x3},{x4},{x5},{x6},{x7},{d0},{d1},{d2},{d3},{d4},{d5},{d6},{d7},{x9},{x10},{x11},{x12},~{x13},~{x14},~{x15},~{x16},~{x19},~{x30},~{d0},~{memory}",
                int_args[0], int_args[1], int_args[2], int_args[3],
                int_args[4], int_args[5], int_args[6], int_args[7],
                float_args[0], float_args[1], float_args[2], float_args[3],
                float_args[4], float_args[5], float_args[6], float_args[7],
                func_ptr, stack_args.ptr, n_stack, stack_bytes
            );
        } else {
            // Integer return: x0 is already correct
            return __asm!uintptr_t(
                "mov x19, sp\n" ~
                "sub sp, sp, x12\n" ~
                "mov x13, sp\n" ~
                "mov x14, x10\n" ~
                "mov x15, x11\n" ~
                "cbz x15, 2f\n" ~
                "1:\n" ~
                "ldr x16, [x14], #8\n" ~
                "str x16, [x13], #8\n" ~
                "subs x15, x15, #1\n" ~
                "b.ne 1b\n" ~
                "2:\n" ~
                "blr x9\n" ~
                "mov sp, x19",
                "={x0},{x0},{x1},{x2},{x3},{x4},{x5},{x6},{x7},{d0},{d1},{d2},{d3},{d4},{d5},{d6},{d7},{x9},{x10},{x11},{x12},~{x13},~{x14},~{x15},~{x16},~{x19},~{x30},~{memory}",
                int_args[0], int_args[1], int_args[2], int_args[3],
                int_args[4], int_args[5], int_args[6], int_args[7],
                float_args[0], float_args[1], float_args[2], float_args[3],
                float_args[4], float_args[5], float_args[6], float_args[7],
                func_ptr, stack_args.ptr, n_stack, stack_bytes
            );
        }
    }
}
else {
    uintptr_t call_native_float(void* func_ptr, uintptr_t* args, size_t n_args, ulong arg_types, ubyte ret_types = 0) {
        assert(0, "Float trampoline not supported on this platform");
    }
}

// Helper to build arg_types mask
ulong make_arg_types(ArgType[] types...) {
    ulong mask = 0;
    foreach (i, t; types) {
        if (i >= 32) break;
        mask |= (cast(ulong)t) << (i * 2);
    }
    return mask;
}
