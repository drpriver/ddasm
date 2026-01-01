/*
 * Copyright © 2025, David Priver
 */
module dvm.native_trampoline;

import dvm.dvm_defs: uintptr_t;

// Native call trampoline for non-varargs functions.
// Assumes all arguments have been lowered to integer/pointer types by the frontend.
// Handles arbitrary argument counts by placing first N in registers, rest on stack.

version(AArch64) {
    import ldc.llvmasm;

    // ARM64 ABI (all platforms for non-varargs):
    // - First 8 args in x0-x7
    // - Additional args on stack
    // - Stack must be 16-byte aligned
    uintptr_t call_native_trampoline(
        void* func_ptr,
        uintptr_t* args,
        size_t n_args
    ) {
        // Copy args to local storage for stable asm access
        uintptr_t[16] local_args = void;
        for (size_t i = 0; i < n_args && i < 16; i++) {
            local_args[i] = args[i];
        }

        // Load register args (up to 8)
        uintptr_t a0 = n_args > 0 ? local_args[0] : 0;
        uintptr_t a1 = n_args > 1 ? local_args[1] : 0;
        uintptr_t a2 = n_args > 2 ? local_args[2] : 0;
        uintptr_t a3 = n_args > 3 ? local_args[3] : 0;
        uintptr_t a4 = n_args > 4 ? local_args[4] : 0;
        uintptr_t a5 = n_args > 5 ? local_args[5] : 0;
        uintptr_t a6 = n_args > 6 ? local_args[6] : 0;
        uintptr_t a7 = n_args > 7 ? local_args[7] : 0;

        // Args beyond 8 go on stack
        size_t n_stack_args = n_args > 8 ? n_args - 8 : 0;
        uintptr_t* stack_args = local_args.ptr + 8;

        // Calculate stack space (16-byte aligned)
        size_t stack_bytes = ((n_stack_args * 8) + 15) & ~cast(size_t)15;

        // x9 = func_ptr, x10 = stack_args, x11 = n_stack_args, x12 = stack_bytes
        // x19 used to save original SP (callee-saved)
        return __asm!uintptr_t(
            // Save original SP in callee-saved register
            "mov x19, sp\n" ~
            // Allocate stack space
            "sub sp, sp, x12\n" ~
            // Copy stack args
            "mov x13, sp\n" ~            // x13 = dest
            "mov x14, x10\n" ~           // x14 = source
            "mov x15, x11\n" ~           // x15 = counter
            "cbz x15, 2f\n" ~            // skip if no stack args
            "1:\n" ~
            "ldr x16, [x14], #8\n" ~     // load arg, advance src
            "str x16, [x13], #8\n" ~     // store to stack, advance dest
            "subs x15, x15, #1\n" ~
            "b.ne 1b\n" ~
            "2:\n" ~
            // Call the function
            "blr x9\n" ~
            // Restore stack
            "mov sp, x19",
            "={x0},{x0},{x1},{x2},{x3},{x4},{x5},{x6},{x7},{x9},{x10},{x11},{x12},~{x13},~{x14},~{x15},~{x16},~{x19},~{x30},~{memory}",
            a0, a1, a2, a3, a4, a5, a6, a7, func_ptr, stack_args, n_stack_args, stack_bytes
        );
    }
}
else version(X86_64) {
    version(Posix) {
        import ldc.llvmasm;

        // System V AMD64 ABI (Linux and macOS):
        // - First 6 integer/pointer args in: rdi, rsi, rdx, rcx, r8, r9
        // - Additional args on stack (pushed right-to-left)
        // - Stack must be 16-byte aligned before call
        uintptr_t call_native_trampoline(
            void* func_ptr,
            uintptr_t* args,
            size_t n_args
        ) {
            // Copy args to local storage for stable asm access
            uintptr_t[16] local_args = void;
            for (size_t i = 0; i < n_args && i < 16; i++) {
                local_args[i] = args[i];
            }

            // Load register args (up to 6)
            uintptr_t a0 = n_args > 0 ? local_args[0] : 0;
            uintptr_t a1 = n_args > 1 ? local_args[1] : 0;
            uintptr_t a2 = n_args > 2 ? local_args[2] : 0;
            uintptr_t a3 = n_args > 3 ? local_args[3] : 0;
            uintptr_t a4 = n_args > 4 ? local_args[4] : 0;
            uintptr_t a5 = n_args > 5 ? local_args[5] : 0;

            // Args beyond 6 go on stack
            size_t n_stack_args = n_args > 6 ? n_args - 6 : 0;
            uintptr_t* stack_args = local_args.ptr + 6;

            // Calculate stack space (16-byte aligned)
            size_t stack_bytes = n_stack_args * 8;
            stack_bytes = (stack_bytes + 15) & ~cast(size_t)15;

            // r10 = func_ptr, r11 = stack_args ptr, r12 = n_stack_args, r13 = stack_bytes
            // rbx used to save original rsp (callee-saved)
            return __asm!uintptr_t(
                // Save original rsp in callee-saved register
                "movq %rsp, %rbx\n" ~
                // Allocate stack space
                "subq %r13, %rsp\n" ~
                // Push args right-to-left (stack_args[n-1] first)
                "testq %r12, %r12\n" ~
                "jz 2f\n" ~                         // skip if no stack args
                // r14 = index (start at n_stack_args - 1)
                "leaq -1(%r12), %r14\n" ~
                // r15 = dest on stack
                "movq %rsp, %r15\n" ~
                "1:\n" ~
                "movq (%r11, %r14, 8), %rax\n" ~    // load arg[index]
                "movq %rax, (%r15)\n" ~             // store to stack
                "addq $$8, %r15\n" ~                // advance dest
                "subq $$1, %r14\n" ~
                "jns 1b\n" ~                        // loop while index >= 0
                "2:\n" ~
                // Call the function
                "callq *%r10\n" ~
                // Restore stack
                "movq %rbx, %rsp",
                "={rax},{rdi},{rsi},{rdx},{rcx},{r8},{r9},{r10},{r11},{r12},{r13},~{rbx},~{r14},~{r15},~{memory}",
                a0, a1, a2, a3, a4, a5, func_ptr, stack_args, n_stack_args, stack_bytes
            );
        }
    }
    else version(Windows) {
        import ldc.llvmasm;

        // Microsoft x64 ABI:
        // - First 4 integer/pointer args in: rcx, rdx, r8, r9
        // - Additional args on stack
        // - 32 bytes "shadow space" always allocated before call
        // - Stack must be 16-byte aligned before call
        uintptr_t call_native_trampoline(
            void* func_ptr,
            uintptr_t* args,
            size_t n_args
        ) {
            // Copy args to local storage for stable asm access
            uintptr_t[16] local_args = void;
            for (size_t i = 0; i < n_args && i < 16; i++) {
                local_args[i] = args[i];
            }

            // Load register args (up to 4)
            uintptr_t a0 = n_args > 0 ? local_args[0] : 0;
            uintptr_t a1 = n_args > 1 ? local_args[1] : 0;
            uintptr_t a2 = n_args > 2 ? local_args[2] : 0;
            uintptr_t a3 = n_args > 3 ? local_args[3] : 0;

            // Args beyond 4 go on stack (after shadow space)
            size_t n_stack_args = n_args > 4 ? n_args - 4 : 0;
            uintptr_t* stack_args = local_args.ptr + 4;

            // Calculate stack space: 32 bytes shadow + stack args (16-byte aligned)
            size_t stack_bytes = 32 + n_stack_args * 8;
            stack_bytes = (stack_bytes + 15) & ~cast(size_t)15;

            // r10 = func_ptr, r11 = stack_args ptr, r12 = n_stack_args, r13 = stack_bytes
            // rbx used to save original rsp (callee-saved on Windows)
            return __asm!uintptr_t(
                // Save original rsp in callee-saved register
                "movq %rsp, %rbx\n" ~
                // Allocate stack space (shadow + extra args)
                "subq %r13, %rsp\n" ~
                // Copy extra args to stack (after shadow space)
                "testq %r12, %r12\n" ~
                "jz 2f\n" ~                         // skip if no stack args
                // r14 = index (start at n_stack_args - 1)
                "leaq -1(%r12), %r14\n" ~
                // r15 = dest on stack (after shadow space)
                "leaq 32(%rsp), %r15\n" ~
                "1:\n" ~
                "movq (%r11, %r14, 8), %rax\n" ~    // load arg[index]
                "movq %rax, (%r15)\n" ~             // store to stack
                "addq $$8, %r15\n" ~                // advance dest
                "subq $$1, %r14\n" ~
                "jns 1b\n" ~                        // loop while index >= 0
                "2:\n" ~
                // Call the function
                "callq *%r10\n" ~
                // Restore stack
                "movq %rbx, %rsp",
                "={rax},{rcx},{rdx},{r8},{r9},{r10},{r11},{r12},{r13},~{rbx},~{r14},~{r15},~{memory}",
                a0, a1, a2, a3, func_ptr, stack_args, n_stack_args, stack_bytes
            );
        }
    }
    else {
        uintptr_t call_native_trampoline(void* func_ptr, uintptr_t* args, size_t n_args) {
            assert(0, "x86_64 native trampoline not yet implemented for this OS");
        }
    }
}
else {
    uintptr_t call_native_trampoline(void* func_ptr, uintptr_t* args, size_t n_args) {
        assert(0, "native trampoline not supported on this platform");
    }
}
