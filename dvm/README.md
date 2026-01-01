# DVM Calling Convention

This document describes the calling convention used by the DVM (Dasm Virtual Machine) for function calls between interpreted functions and to native functions.

## Registers

### General Purpose Registers

| Register | Purpose |
|----------|---------|
| R0-R7 | Argument registers (RARG1-RARG8) |
| R8-R14 | Scratch registers |
| R15-R16 | Return value registers (ROUT1-ROUT2) |
| R17 | Junk register (RJUNK) |
| R18 | Stack pointer (RSP) |
| R19 | Base pointer (RBP) |
| R20 | Instruction pointer (RIP) |
| R21 | Flags register (RFLAGS) |
| R22 | Error register (RERROR) |

### Argument Registers

The number of argument registers is configurable via `N_REG_ARGS` in `dvm_regs.d` (default: 8).

| Argument | Register | Alias |
|----------|----------|-------|
| 1 | R0 | RARG1 |
| 2 | R1 | RARG2 |
| 3 | R2 | RARG3 |
| 4 | R3 | RARG4 |
| 5 | R4 | RARG5 |
| 6 | R5 | RARG6 |
| 7 | R6 | RARG7 |
| 8 | R7 | RARG8 |
| 9+ | Stack | - |

## Scalar Argument Passing

### Caller Responsibilities

1. Evaluate arguments 1 through N_REG_ARGS into RARG1-RARG_N
2. Push arguments beyond N_REG_ARGS onto the stack (left-to-right order)
3. Execute `call function <name> <total_arg_count>`
4. **After call returns**: Pop stack arguments (caller cleanup)

```
; Example: 10-argument call with N_REG_ARGS=8
move rarg1 <arg1>
move rarg2 <arg2>
...
move rarg8 <arg8>
push <arg9>           ; Stack arg
push <arg10>          ; Stack arg
call function foo 10
sub rsp rsp 16        ; Caller cleanup (2 stack args * 8 bytes)
```

### Callee Responsibilities

1. Read arguments 1 through min(n_params, N_REG_ARGS) from RARG registers
2. Read remaining arguments from the stack
3. **Do NOT pop stack arguments** - caller handles cleanup
4. Return via `ret`

### Stack Layout

The DVM stack grows **upward** (push increases RSP).

```
Before call (after pushing stack args):
  [prior stack]
  arg_N+1      <- first stack arg (lower address)
  arg_N+2
  ...
  RSP ->       <- top of stack (higher address)

Callee reads stack_arg[i] at: RSP - (n_stack_args - i) * 8
```

### Arity Mismatch Safety

With caller cleanup, arity mismatches don't corrupt the stack:
- If caller passes N args but callee expects M:
  - Callee reads what it needs from registers/stack
  - Caller pops exactly what it pushed
  - Result: no stack corruption, just junk values if N < M

## Struct Passing

### Small Structs (≤16 bytes)

Passed and returned in registers:
- 1-8 bytes: 1 register (RARG_n or ROUT1)
- 9-16 bytes: 2 consecutive registers (RARG_n, RARG_n+1 or ROUT1, ROUT2)

The struct is zero-extended to fill the register(s).

### Large Structs (>16 bytes)

**Passed by hidden pointer:**
- Caller allocates space for the struct
- Passes pointer as a regular argument (occupies one RARG slot)
- Callee reads/writes through the pointer

**Returned by hidden pointer:**
- Caller allocates result space
- Passes hidden first argument (pointer to result space)
- Callee writes result through pointer, returns the pointer in ROUT1
- Original arguments shift: arg1 becomes arg2, etc.

```c
// Source:
BigStruct make_big(int x);
BigStruct s = make_big(42);

// Lowered to:
BigStruct result_space;
make_big(&result_space, 42);  // hidden first arg
```

## Varargs Functions

Varargs follow ARM64-style conventions: fixed arguments in registers, variadic arguments always on stack.

### Caller (vararg call)

1. Place fixed arguments in RARG1-RARG_N (up to N_REG_ARGS)
2. Push **all** variadic arguments onto the stack (even if registers are available)
3. Call with total argument count

```c
printf("%d %d", 1, 2);  // 1 fixed arg, 2 varargs
// RARG1 = format string pointer
// stack: [1] [2]  <- varargs always on stack
```

### Callee (vararg function)

- Read fixed arguments from RARG registers
- Read varargs from stack
- `va_start(ap, last_fixed)`: ap points to first vararg on stack
- `va_arg(ap, T)`: read from ap, advance by sizeof(T) aligned to 8

## Return Values

| Return Type | Location |
|-------------|----------|
| Scalar (≤8 bytes) | ROUT1 |
| Small struct (≤8 bytes) | ROUT1 |
| Small struct (9-16 bytes) | ROUT1, ROUT2 |
| Large struct (>16 bytes) | Via hidden pointer in RARG1 |
| void | Nothing |

## Register Preservation

All scratch registers are **caller-saved**:
- R0-R14 may be clobbered by any call
- Caller must save any values it needs across a call

No registers are callee-saved (callee can freely use all scratch registers).

## Native Calls

Native functions are called using platform-specific trampolines that marshal DVM registers to the native calling convention:

- **x86_64 SysV**: First 6 args in rdi, rsi, rdx, rcx, r8, r9
- **x86_64 Windows**: First 4 args in rcx, rdx, r8, r9 + shadow space
- **ARM64**: First 8 args in x0-x7

For varargs native calls, the trampoline handles the platform-specific requirements (e.g., ARM64 macOS requires varargs on stack).

## Configuration

The calling convention is configurable via `dvm_regs.d`:

```d
enum N_REG_ARGS = 8;  // Number of register arguments
```

Changing this value automatically updates:
- RARG register aliases
- Argument register allocation in compilers
- Stack argument handling
