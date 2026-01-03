/*
 * Copyright © 2025, David Priver
 *
 * Float-aware varargs trampoline.
 * Properly routes integer args to GPRs and float args to XMM/vector registers
 * according to platform ABI for variadic functions.
 *
 * arg_types encoding (2 bits per arg):
 *   00 = integer/pointer (64-bit, GPR)
 *   01 = float32 (32-bit, XMM) - promoted to double for varargs
 *   10 = float64/double (64-bit, XMM)
 *   11 = reserved
 */
module dvm.varargs_trampoline_float;

import dvm.dvm_defs: uintptr_t;
import dvm.native_trampoline_float: ArgType, get_arg_type, is_float_type;

version(X86_64) {
    version(Posix) {
        import ldc.llvmasm;

        // System V AMD64 ABI for varargs:
        // - Fixed args: first 6 ints in GPRs, first 8 floats in XMMs
        // - Variadic args: same rules, but AL must = number of XMM regs used
        // - Float varargs still go in XMM registers (up to 8 total)
        // - Integer return in RAX, float return in XMM0
        uintptr_t call_varargs_float(
            void* func_ptr,
            uintptr_t* args,
            size_t n_fixed,
            size_t n_total,
            uint arg_types,
            ubyte ret_types = 0
        ) {
            // Separate all args into integer and float categories
            uintptr_t[6] int_args = 0;
            uintptr_t[8] float_args = 0;
            size_t n_int = 0;
            size_t n_float = 0;

            // Stack args (when registers overflow)
            uintptr_t[32] stack_args = void;
            size_t n_stack = 0;

            for (size_t i = 0; i < n_total && i < 32; i++) {
                ArgType t = get_arg_type(arg_types, i);
                if (is_float_type(t)) {
                    if (n_float < 8) {
                        float_args[n_float++] = args[i];
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

            // Calculate stack space (16-byte aligned)
            size_t stack_bytes = n_stack * 8;
            stack_bytes = (stack_bytes + 15) & ~cast(size_t)15;

            bool float_ret = (ret_types & 0x3) != 0;

            if (float_ret) {
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
                    "movq %xmm0, %rax\n" ~
                    "movq %rbx, %rsp",
                    "={rax},{rdi},{rsi},{rdx},{rcx},{r8},{r9},{xmm0},{xmm1},{xmm2},{xmm3},{xmm4},{xmm5},{xmm6},{xmm7},{al},{r10},{r11},{r12},{r13},~{rbx},~{r14},~{r15},~{xmm0},~{memory}",
                    int_args[0], int_args[1], int_args[2], int_args[3], int_args[4], int_args[5],
                    float_args[0], float_args[1], float_args[2], float_args[3],
                    float_args[4], float_args[5], float_args[6], float_args[7],
                    cast(ubyte)n_float,
                    func_ptr, stack_args.ptr, n_stack, stack_bytes
                );
            } else {
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

        // Windows x64 varargs:
        // - First 4 args in slots (GPR or XMM based on type)
        // - Float values must be in BOTH GPR and XMM for varargs
        // - Additional args on stack after 32-byte shadow space
        uintptr_t call_varargs_float(
            void* func_ptr,
            uintptr_t* args,
            size_t n_fixed,
            size_t n_total,
            uint arg_types,
            ubyte ret_types = 0
        ) {
            uintptr_t[4] gpr_args = 0;
            uintptr_t[4] xmm_args = 0;

            uintptr_t[32] stack_args = void;
            size_t n_stack = 0;

            for (size_t i = 0; i < n_total && i < 32; i++) {
                ArgType t = get_arg_type(arg_types, i);
                if (i < 4) {
                    gpr_args[i] = args[i];
                    if (is_float_type(t)) {
                        xmm_args[i] = args[i];
                    }
                } else {
                    stack_args[n_stack++] = args[i];
                }
            }

            size_t stack_bytes = 32 + n_stack * 8;
            stack_bytes = (stack_bytes + 15) & ~cast(size_t)15;

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
                    "movq %xmm0, %rax\n" ~
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
        uintptr_t call_varargs_float(void* func_ptr, uintptr_t* args, size_t n_fixed, size_t n_total, uint arg_types, ubyte ret_types = 0) {
            assert(0, "x86_64 float varargs not implemented for this OS");
        }
    }
}
else version(AArch64) {
    import ldc.llvmasm;

    version(OSX) {
        // Apple ARM64 varargs: variadic args go on stack, not registers
        uintptr_t call_varargs_float(
            void* func_ptr,
            uintptr_t* args,
            size_t n_fixed,
            size_t n_total,
            uint arg_types,
            ubyte ret_types = 0
        ) {
            size_t n_variadic = n_total - n_fixed;

            uintptr_t[8] int_args = 0;
            uintptr_t[8] float_args = 0;
            size_t n_int = 0;
            size_t n_float = 0;

            for (size_t i = 0; i < n_fixed && i < 16; i++) {
                ArgType t = get_arg_type(arg_types, i);
                if (is_float_type(t)) {
                    if (n_float < 8) float_args[n_float++] = args[i];
                } else {
                    if (n_int < 8) int_args[n_int++] = args[i];
                }
            }

            uintptr_t* var_args = args + n_fixed;
            size_t stack_bytes = ((n_variadic * 8) + 15) & ~cast(size_t)15;

            bool float_ret = (ret_types & 0x3) != 0;

            if (float_ret) {
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
                    "fmov x0, d0\n" ~
                    "mov sp, x19",
                    "={x0},{x0},{x1},{x2},{x3},{x4},{x5},{x6},{x7},{d0},{d1},{d2},{d3},{d4},{d5},{d6},{d7},{x9},{x10},{x11},{x12},~{x13},~{x14},~{x15},~{x16},~{x19},~{x30},~{d0},~{memory}",
                    int_args[0], int_args[1], int_args[2], int_args[3],
                    int_args[4], int_args[5], int_args[6], int_args[7],
                    float_args[0], float_args[1], float_args[2], float_args[3],
                    float_args[4], float_args[5], float_args[6], float_args[7],
                    func_ptr, var_args, n_variadic, stack_bytes
                );
            } else {
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
                    func_ptr, var_args, n_variadic, stack_bytes
                );
            }
        }
    }
    else {
        // Linux ARM64: all args (fixed + variadic) use registers, overflow to stack
        uintptr_t call_varargs_float(
            void* func_ptr,
            uintptr_t* args,
            size_t n_fixed,
            size_t n_total,
            uint arg_types,
            ubyte ret_types = 0
        ) {
            uintptr_t[8] int_args = 0;
            uintptr_t[8] float_args = 0;
            size_t n_int = 0;
            size_t n_float = 0;

            uintptr_t[32] stack_args = void;
            size_t n_stack = 0;

            for (size_t i = 0; i < n_total && i < 32; i++) {
                ArgType t = get_arg_type(arg_types, i);
                if (is_float_type(t)) {
                    if (n_float < 8) float_args[n_float++] = args[i];
                    else stack_args[n_stack++] = args[i];
                } else {
                    if (n_int < 8) int_args[n_int++] = args[i];
                    else stack_args[n_stack++] = args[i];
                }
            }

            size_t stack_bytes = ((n_stack * 8) + 15) & ~cast(size_t)15;

            bool float_ret = (ret_types & 0x3) != 0;

            if (float_ret) {
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
                    "fmov x0, d0\n" ~
                    "mov sp, x19",
                    "={x0},{x0},{x1},{x2},{x3},{x4},{x5},{x6},{x7},{d0},{d1},{d2},{d3},{d4},{d5},{d6},{d7},{x9},{x10},{x11},{x12},~{x13},~{x14},~{x15},~{x16},~{x19},~{x30},~{d0},~{memory}",
                    int_args[0], int_args[1], int_args[2], int_args[3],
                    int_args[4], int_args[5], int_args[6], int_args[7],
                    float_args[0], float_args[1], float_args[2], float_args[3],
                    float_args[4], float_args[5], float_args[6], float_args[7],
                    func_ptr, stack_args.ptr, n_stack, stack_bytes
                );
            } else {
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
}
else {
    uintptr_t call_varargs_float(void* func_ptr, uintptr_t* args, size_t n_fixed, size_t n_total, uint arg_types, ubyte ret_types = 0) {
        assert(0, "Float varargs not supported on this platform");
    }
}
