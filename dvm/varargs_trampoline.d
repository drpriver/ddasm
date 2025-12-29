/*
 * Copyright © 2025, David Priver
 */
module dvm.varargs_trampoline;

import dvm.dvm_defs: uintptr_t;

// Apple ARM64 varargs calling convention (differs from standard AAPCS64):
// - Fixed (named) args go in registers x0-x7
// - Variadic args go on the stack (NOT in registers like Linux ARM64)
// - Stack must be 16-byte aligned

version(AArch64) {
    version(OSX) {
        import ldc.llvmasm;

        uintptr_t call_varargs(
        void* func_ptr,
        uintptr_t* args,
        size_t n_fixed,
        size_t n_total
    ) {
        size_t n_variadic = n_total - n_fixed;

        // Copy args to local storage for stable asm access
        uintptr_t[16] local_args = void;
        for (size_t i = 0; i < n_total && i < 16; i++) {
            local_args[i] = args[i];
        }

        // Load fixed args (up to 8)
        uintptr_t a0 = n_fixed > 0 ? local_args[0] : 0;
        uintptr_t a1 = n_fixed > 1 ? local_args[1] : 0;
        uintptr_t a2 = n_fixed > 2 ? local_args[2] : 0;
        uintptr_t a3 = n_fixed > 3 ? local_args[3] : 0;
        uintptr_t a4 = n_fixed > 4 ? local_args[4] : 0;
        uintptr_t a5 = n_fixed > 5 ? local_args[5] : 0;
        uintptr_t a6 = n_fixed > 6 ? local_args[6] : 0;
        uintptr_t a7 = n_fixed > 7 ? local_args[7] : 0;

        // Pointer to variadic args
        uintptr_t* var_args = local_args.ptr + n_fixed;

        // Calculate stack space (16-byte aligned)
        size_t stack_bytes = ((n_variadic * 8) + 15) & ~cast(size_t)15;

        // x9 = func_ptr, x10 = var_args, x11 = n_variadic, x12 = stack_bytes
        // x13-x16 used as temporaries for the copy loop
        // x19 used to save original SP (callee-saved, so LLVM will preserve it)
        return __asm!uintptr_t(
            // Save original SP in callee-saved register (x12 gets clobbered by callee)
            "mov x19, sp\n" ~
            // Allocate stack space for variadic args (0 if none)
            "sub sp, sp, x12\n" ~
            // Copy loop setup
            "mov x13, sp\n" ~            // x13 = dest (stack pointer)
            "mov x14, x10\n" ~           // x14 = source pointer
            "mov x15, x11\n" ~           // x15 = counter
            "cbz x15, 2f\n" ~            // skip loop if no variadic args
            "1:\n" ~
            "ldr x16, [x14], #8\n" ~     // load arg, advance src
            "str x16, [x13], #8\n" ~     // store to stack, advance dest
            "subs x15, x15, #1\n" ~
            "b.ne 1b\n" ~
            "2:\n" ~
            // Call the function
            "blr x9\n" ~
            // Restore stack from saved SP
            "mov sp, x19",
            "={x0},{x0},{x1},{x2},{x3},{x4},{x5},{x6},{x7},{x9},{x10},{x11},{x12},~{x13},~{x14},~{x15},~{x16},~{x19},~{x30},~{memory}",
            a0, a1, a2, a3, a4, a5, a6, a7, func_ptr, var_args, n_variadic, stack_bytes
        );
    }
    }
    else {
        // Linux ARM64 uses standard AAPCS64 where variadic args go in registers
        uintptr_t call_varargs(void* func_ptr, uintptr_t* args, size_t n_fixed, size_t n_total) {
            assert(0, "Linux ARM64 varargs not yet implemented");
        }
    }
}
else version(X86_64) {
    version(linux) {
        import ldc.llvmasm;

        // System V AMD64 ABI:
        // - First 6 integer/pointer args in: rdi, rsi, rdx, rcx, r8, r9
        // - Additional args on stack (pushed right-to-left)
        // - For varargs: al = number of vector registers used (0 for no floats)
        // - Stack must be 16-byte aligned before call
        uintptr_t call_varargs(
            void* func_ptr,
            uintptr_t* args,
            size_t n_fixed,
            size_t n_total
        ) {
            // Copy args to local storage for stable asm access
            uintptr_t[16] local_args = void;
            for (size_t i = 0; i < n_total && i < 16; i++) {
                local_args[i] = args[i];
            }

            // Load register args (up to 6)
            uintptr_t a0 = n_total > 0 ? local_args[0] : 0;
            uintptr_t a1 = n_total > 1 ? local_args[1] : 0;
            uintptr_t a2 = n_total > 2 ? local_args[2] : 0;
            uintptr_t a3 = n_total > 3 ? local_args[3] : 0;
            uintptr_t a4 = n_total > 4 ? local_args[4] : 0;
            uintptr_t a5 = n_total > 5 ? local_args[5] : 0;

            // Args beyond 6 go on stack
            size_t n_stack_args = n_total > 6 ? n_total - 6 : 0;
            uintptr_t* stack_args = local_args.ptr + 6;

            // Calculate stack space (16-byte aligned, plus 8 if odd number to maintain alignment)
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
                // Set al = 0 (no floating point variadic args)
                "xorl %eax, %eax\n" ~
                // Call the function
                "callq *%r10\n" ~
                // Restore stack
                "movq %rbx, %rsp",
                "={rax},{rdi},{rsi},{rdx},{rcx},{r8},{r9},{r10},{r11},{r12},{r13},~{rbx},~{r14},~{r15},~{memory}",
                a0, a1, a2, a3, a4, a5, func_ptr, stack_args, n_stack_args, stack_bytes
            );
        }
    }
    else {
        uintptr_t call_varargs(
            void* func_ptr,
            uintptr_t* args,
            size_t n_fixed,
            size_t n_total
        ) {
            assert(0, "x86_64 varargs not yet implemented for this OS");
        }
    }
}
else {
    uintptr_t call_varargs(
        void* func_ptr,
        uintptr_t* args,
        size_t n_fixed,
        size_t n_total
    ) {
        assert(0, "varargs not supported on this platform");
    }
}
