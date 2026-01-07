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

### Scratch Registers

Scratch registers (R8-R14) have aliases for clarity. These registers are preserved across native calls but may be clobbered by DVM-to-DVM calls (caller-saved).

| Register | Alias |
|----------|-------|
| R8 | RS0 |
| R9 | RS1 |
| R10 | RS2 |
| R11 | RS3 |
| R12 | RS4 |
| R13 | RS5 |
| R14 | RS6 |

Use scratch registers for local variables that need to persist across function calls:

```
function example 2
    # Copy args to scratch registers if needed across calls
    move rs0 rarg1
    move rs1 rarg2

    # Make a call - rs0, rs1 survive (for native calls)
    move rarg1 "hello"
    call function io.puts

    # rs0, rs1 still have our values
    move rarg1 rs0
    call function io.puts
    ret
end
```

## Scalar Argument Passing

### Caller Responsibilities

1. Evaluate arguments 1 through N_REG_ARGS into RARG1-RARG_N
2. Push arguments beyond N_REG_ARGS onto the stack (left-to-right order)
3. Execute `call function <name> <total_arg_count>`
4. **After call returns**: Pop stack arguments (caller cleanup)

```
# Example: 10-argument call with N_REG_ARGS=8
move rarg1 <arg1>
move rarg2 <arg2>
...
move rarg8 <arg8>
push <arg9>           # Stack arg
push <arg10>          # Stack arg
call function foo 10
sub rsp rsp 16        # Caller cleanup (2 stack args * 8 bytes)
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

## Native Function Declarations (dlimport)

Native functions are declared in `dlimport` blocks with optional type encoding for proper ABI handling:

```
dlimport LibName
  path "libname.dylib"
  function func_name <n_args> <n_ret> [varargs] [<arg_types> [<ret_types>]] [struct_args [...]]
end
```

### Basic Declaration

```
function printf 1 1 varargs     # 1 fixed arg, 1 return, varargs
function strlen 1 1             # 1 arg, 1 return (both integers)
function exit 1 0               # 1 arg, no return
```

### Type Masks (arg_types and ret_types)

Type masks encode whether each argument/return slot is an integer, float, or double. Each slot uses 2 bits:

| Code | Type | Description |
|------|------|-------------|
| 00 | INT | Integer or pointer (default) |
| 01 | FLOAT32 | 32-bit float |
| 10 | FLOAT64 | 64-bit double |
| 11 | (reserved) | |

Bits are packed from LSB: slot 0 uses bits 0-1, slot 1 uses bits 2-3, etc.

**Examples:**
```
# float sinf(float x)
function sinf 1 1 0x01 0x01     # arg0=float(01), ret=float(01)

# double sin(double x)
function sin 1 1 0x02 0x02      # arg0=double(10), ret=double(10)

# float dot(float x, float y, float z, float a, float b, float c)
function dot 6 1 0x555 0x01     # 6 floats (010101010101 = 0x555), ret=float

# double mixed(int a, float b, double c)
# Slot 0: int (00), Slot 1: float (01), Slot 2: double (10)
# Mask = 0b100100 = 0x24
function mixed 3 1 0x24 0x02
```

**Computing type masks:**
```
mask = 0
for i in 0..n_args:
    if arg[i] is float:  mask |= (0x01 << (i * 2))
    if arg[i] is double: mask |= (0x02 << (i * 2))
```

### struct_args Array

The `struct_args` array provides additional information about struct arguments for proper native ABI handling. One entry per DVM argument slot.

| Value | Meaning |
|-------|---------|
| 0 | Not a struct, or small struct with no special handling needed |
| 1-4 | Float HFA (Homogeneous Floating-point Aggregate) with N float members |
| 5-8 | Double HFA with N-4 double members (5=1 double, 6=2 doubles, etc.) |
| 9 | Hidden return pointer (first arg is pointer for large struct return) |
| >16 | Large struct - value is byte size, copied to native stack |

**HFA Explanation (ARM64):**

On ARM64, structs containing only floats or only doubles are passed in FP registers, one member per register:

```c
typedef struct { float x, y; } Vec2;        // 2-member float HFA
typedef struct { float x, y, z; } Vec3;     // 3-member float HFA
typedef struct { double x, y, z; } Vec3d;   // 3-member double HFA
```

```
# Vec2 make_vec2(float x, float y) - returns 2-float HFA
function make_vec2 2 1 0x05 0x05            # arg/ret masks = float, float

# float vec2_length(Vec2 v) - takes 2-float HFA
function vec2_length 1 1 0x0 0x01 struct_args [2]
#                                            ^ HFA with 2 floats

# Vec3 vec3_add(Vec3 a, Vec3 b) - takes two 3-float HFAs, returns one
# Each Vec3 uses 2 DVM slots (12 bytes = 2 slots), second slot is 0 (continuation)
function vec3_add 4 2 0x0 0x15 struct_args [3 0 3 0]
#                                           ^ a  ^ b (HFA markers)
```

**Large Struct Example:**

```c
typedef struct { float m[16]; } Mat4x4;  // 64 bytes - too large for registers

// float mat4x4_trace(Mat4x4 m)
function mat4x4_trace 1 1 0x0 0x01 struct_args [64]
#                                              ^ 64-byte struct, copied to native stack
```

**Hidden Return Pointer:**

```c
typedef struct { float m[16]; } Mat4x4;

// Mat4x4 mat4x4_identity(void)
// Becomes: void mat4x4_identity(Mat4x4* result) with hidden first arg
function mat4x4_identity 1 0 0x0 0x0 struct_args [9]
#                                                ^ 9 = hidden return pointer
```

### Complete Examples

```
dlimport MathLib
  path "libm.dylib"

  # double sin(double x)
  function sin 1 1 0x02 0x02

  # float sinf(float x)
  function sinf 1 1 0x01 0x01

  # double pow(double base, double exp)
  function pow 2 1 0x0a 0x02

  # float fmaf(float a, float b, float c)
  function fmaf 3 1 0x15 0x01
end

dlimport VectorLib
  path "libvector.dylib"

  # Vec2 vec2_add(Vec2 a, Vec2 b) - 2-float HFAs
  function vec2_add 2 1 0x0 0x05 struct_args [2 2]

  # Vec3 vec3_normalize(Vec3 v) - 3-float HFA in, 3-float HFA out
  function vec3_normalize 2 2 0x0 0x15 struct_args [3 0]

  # Mat4x4 mat4x4_multiply(Mat4x4 a, Mat4x4 b) - large structs
  # Hidden return ptr + 2 large struct args
  function mat4x4_multiply 3 0 0x0 0x0 struct_args [9 64 64]
end
```

### Platform Notes

- **x86_64 SysV**: Integer args in rdi, rsi, rdx, rcx, r8, r9; floats in xmm0-xmm7
- **x86_64 Windows**: Integer args in rcx, rdx, r8, r9; floats share same slots
- **ARM64**: Integer args in x0-x7; floats in s0-s7/d0-d7; HFAs expand to multiple FP regs

The trampoline code (`dvm_trampoline.d`) uses these encodings to properly route arguments to the correct native registers.

## Configuration

The calling convention is configurable via `dvm_regs.d`:

```d
enum N_REG_ARGS = 8;  // Number of register arguments
```

Changing this value automatically updates:
- RARG register aliases
- Argument register allocation in compilers
- Stack argument handling
