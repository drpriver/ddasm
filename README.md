# Ddasm
## Dasm
Dasm (`.dasm`) is a made up assembly language that runs on a made up virtual machine.
The vm is a register-machine and is intentionally unsafe, with no
separation of code and data. Code is mutable and you can memcpy functions
onto your stack and execute them if you want. The instruction pointer is
a writable register, you can `move rip [an array of bytes here]` to start
executing arbitrary data, etc. The only safety feature is that a shadow
stack is used for return addresses so you can't do ROP (Not that that matters
if you can just overwrite the code...). That was only implemented so that
backtraces would work.

Dasm only works with machine-word sized values.

Dasm has trivial native interop. An adaptor function has to be written that casts
from `uintptr_t` to the actual type. You currently can't call native functions
with floating point arguments.

The Dasm vm has a debugger built in. You can step instructions and read
registers etc. It has a disassembler, etc. It doesn't try to catch faults in
native code, but you can run the vm in a native debugger for that. It is not
a source debugger yet, it just disassembles the code.

### Syntax

It has regular decimal literals, binary literals, hex literals.

It has pointer size literals (`0p41ad`), which are hex literals that
are multiplied by the size of `uintptr_t` at assembly time.

It has string number literals (`0sWhatever`), which are integer literals but
each character is converted to its ascii value and slotted into the resulting
number.  `0sWhatever` would be equivelant to `0x7265766574616857` in hex. They
can be up to the size of the word size of the host.


Code must be contained within functions. Functions have a name, an optional number
indicating the number of arguments to the function, a series of statements and then
an end. The last statement of a function must be one of `abort`, `halt` or `ret`, to
guard against just falling off the end of the function.

Hello world in dasm:
```
import io
function start 0
  move rarg1 "Hello World"
  call function io.puts
  ret
end
```

Hackerman programming in dasm:
```
import
function death
  abort
  move rarg1 "Life after Death"
  call function io.puts
  ret
end

function hackerman
  # functions are a pointer to a function object,
  # whose first member is a pointer to the instructions.
  read r0 function death
  # Add a word to that and store in rip, bypassing the
  # abort
  add rip r0 0p1
  # die if we get here, to show we don't return to
  # this function
  abort
end

function start
  call function hackerman
  halt
end
```



## Davescript
Davescript (`.ds`) is a barely implemented scripting language that can be executed
on the dasm vm.
Currently, this is implemented by compiling a `.ds` script to `.dasm` as text,
and then compiling that into the bytecode instructions that run on the actual
vm. This is silly and it should just compile directly to bytecode, but oh well.

The compiler is pretty buggy at the moment, it's not hard to get it to generate
wrong code.

### Syntax
Davescript is basically c-like.

Davescript has functions:

```
import io
function hello(){
    io.puts("hello")
}
```
Functions are called in the usual manner, as shown above.

It has declarations, at both file and function scope. Arguments to functions
are like declarations.

```
let x = 3

function foo(a){
    let b = a + x
    return b
}
```

It has control flow:
```
import io
function loops(n){
    for(let x = 0; x < n; x = x + 1){
        io.printf2("x: %zu\n", x)
    }
    let y = 3
    while(y < n){
        y = y * 2
    }
    if(y > 10){
         return 3
    }
    return 4
}

function start(){
    let x = loops(4)
    io.printf2("loops(4) = %zu\n", x)
}
```
It has guaranteed tail calls:
```
function fib(n){
    return fib_inner(0, 1, n)
}

function fib_inner(a, b, n){
    if(n <= 0) return a
    return fib_inner(b, a+b, n-1)
}
import io
function start(){
    for(let i = 0; i < 20; i = i + 1)
        io.printf2("fib(%zu) = %zu\n", i, fib(i))
}
```

It has regular decimal literals, binary literals, hex literals, pointer-size
literals and string number literals.

`start` is the entry point for a davescript. You need to define a
`start` function. The arguments to start are implementation defined.

All references to functions and variables are checked. Failure to define
one you use will result in a linking error. There are some builtins
that you don't have to define.

There are no guaranteed builtin functions. All builtin functions
are exposed in code. The current implementation exposes some
std c functions.

## Samples
A collection of sample dasm files and davescript files are in the
Sample folder.

Dasm currently has way more features than davescript.


## Misc

* The dasm was originally written in C and then ported to D, more or
  less the exact same way.
* ddasm should be split into a vm, an assembler, a linker and a default
  runtime.
* I don't know if it works on Windows.
* I don't use exceptions (in fact I only use -betterC) and so the code
  is totally exception-unsafe.

## Building

* The Makefile is with ldc2 on macOS. The only mac-specific thing it does is
  pass `-dead_strip` to the linker, so you can do `make LDSTRIP=` and it
  should build on linux without that probably. Might need to add libm to
  the link line.
* The Makefile doesn't work for Windows. It wouldn't be hard to make it work,
  but I haven't done it.
* There is also a meson build file. It works, but meson passes weird linker flags
  and the documentation is non-existant.
* You can also just build it by doing `ldc2 ddasm.d -i` and that'll just work.
  Throw in `-betterC` too.
* This only builds with ldc as it uses an ldc simd intrinsic to accelerate
  escaping characters in strings. It could be made to work without that
  by wrapping that in a `version(){}` block
* I made it build with dub at some point. I don't know what I'm doing as
  plain `dub` asserts, but `dub build` works.

## C Preprocessor (cpp)

The project includes a C preprocessor (`Bin/cpp`) that can be used standalone or as part of the C-to-dasm compiler (`c2dasm`).

### Debugging Pragmas

The preprocessor supports several debugging pragmas to help diagnose macro expansion issues:

#### `#pragma expand TOKENS`
Shows how tokens expand after macro substitution.

```c
#define FOO bar
#define ADD(a,b) ((a)+(b))
#pragma expand FOO          // file.c:3: FOO -> bar
#pragma expand ADD(1, 2)    // file.c:4: ADD(1, 2) -> ((1)+(2))
```

#### `#pragma eval EXPR`
Evaluates a preprocessor expression (like in `#if`) and prints the result.

```c
#define X 5
#pragma eval X + 3              // file.c:2: X + 3 = 8
#pragma eval 1 + 2 * 3          // file.c:3: 1 + 2 * 3 = 7
#pragma eval defined(X)         // file.c:4: defined(X) = 1
```

#### `#pragma reveal NAME`
Shows the definition of a macro and where it was defined.

```c
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#pragma reveal MAX       // file.c:1: #define MAX(a, b) ((a) > (b) ? (a) : (b))
#pragma reveal UNDEFINED // file.c:2: UNDEFINED is not defined
```

#### `#pragma message "text"`
Prints a message during preprocessing (GCC/Clang compatible, but with macro expansion).

```c
#define VERSION 2
#pragma message "Compiling version " VERSION  // file.c:2: note: Compiling version 2
```

All pragma output goes to stderr with file:line prefix.

### Include Path Pragmas

The preprocessor supports pragmas to dynamically modify include search paths:

#### `#pragma include_path push "path"`
Adds a path to the front of the include search list. Relative paths are resolved relative to the file containing the pragma.

```c
#pragma include_path push "../mylib"
#include "helper.h"  // searches ../mylib first
#pragma include_path pop
```

#### `#pragma include_path pop`
Removes the most recently pushed path. Warns if nothing was pushed.

#### `#pragma include_path reveal`
Prints all current include paths to stderr for debugging.

```c
#pragma include_path reveal
// Output:
// file.c:1: include paths:
//   [pushed] /path/to/mylib
//   /usr/local/include
//   /usr/include
```

### C23 Support

The preprocessor supports C23 (and some C2Y) preprocessing features:
- `#elifdef` / `#elifndef` - shorthand for `#elif defined(...)`
- `#warning` - emit warning diagnostic
- `__has_include(<header>)` / `__has_include("header")` - check if header exists
- `__has_embed(<header>)` / `__has_embed("header")` - check if file exists for embedding
- `__VA_OPT__(content)` - conditional expansion in variadic macros
- `_Pragma("...")` - pragma operator for use in macros
- `#line` - override `__LINE__` and `__FILE__`
- `#embed` - embed binary files (see below)

### `#embed` Support

The `#embed` directive (C23) embeds binary file contents directly into an array initializer:

```c
const unsigned char icon[] = {
#embed "icon.png"
};
```

This is equivalent to writing out all the bytes as comma-separated integers, but without generating megabytes of text for large files.

#### Supported Features

- Basic embedding: `#embed "path"` or `#embed <path>`
- `limit(N)` parameter: `#embed "file" limit(16)` - embed only first N bytes
- `__has_embed("path")` - returns 1 if file exists, 0 otherwise

#### Restrictions

Due to the word-oriented nature of DASM variables, `#embed` has some restrictions:

1. **Word-aligned position**: Any initializers before `#embed` must total a multiple of 8 bytes
   ```c
   // OK: no prefix
   const char data[] = { #embed "file.bin" };

   // OK: 8-byte prefix
   const long data[] = { 0x1234, #embed "file.bin" };

   // ERROR: 3-byte prefix not word-aligned
   const char data[] = { 1, 2, 3, #embed "file.bin" };
   ```

2. **Trailing data**: Non-word-aligned embed length is only allowed at the end of the array
   ```c
   // OK: 5-byte file at end, zero-padded to 8 bytes
   const char data[8] = { #embed "5bytes.bin" };

   // ERROR: can't have suffix after non-aligned embed
   const char data[] = { #embed "5bytes.bin", 0xFF };
   ```

#### DASM Syntax

At the DASM level, embeds use the `embed` initializer:

```dasm
var data 1 embed "path/to/file" 0 8 end
```

Format: `embed "path" offset length`
- `path` - file path (relative to working directory)
- `offset` - byte offset into file
- `length` - number of bytes to embed (zero-padded to word boundary)

### Magic Macros

Beyond the standard `__FILE__` and `__LINE__`, the preprocessor supports:

| Macro | Description |
|-------|-------------|
| `__DATE__` | Compilation date as `"Mmm dd yyyy"` |
| `__TIME__` | Compilation time as `"hh:mm:ss"` |
| `__COUNTER__` | Auto-incrementing integer (0, 1, 2, ...) |
| `__COUNTER__(name)` | Named counter stream (independent per name) |
| `__INCLUDE_DEPTH__` | Nesting depth in #include stack (0 = top level) |
| `__BASE_FILE__` | The root file being compiled (not includes) |
| `__DIR__` | Directory of current file |
| `__RANDOM__` | Random integer (different each expansion) |
| `__ENV__(NAME)` | Environment variable as string (see below) |
| `__EXPAND__(str)` | Destringify string into tokens (see below) |

Example:
```c
int id1 = __COUNTER__;  // 0
int id2 = __COUNTER__;  // 1
const char* dir = __DIR__;  // "src"
#include "header.h"  // inside header: __INCLUDE_DEPTH__ = 1, __BASE_FILE__ = main file

// Named counters are independent streams
int foo0 = __COUNTER__(foo);  // 0
int bar0 = __COUNTER__(bar);  // 0
int foo1 = __COUNTER__(foo);  // 1
```

### `__EXPAND__(string-literal)`

Destringifies a string literal into preprocessor tokens - the inverse of `#` stringification.

```c
int x = __EXPAND__("1 + 2");  // → int x = 1 + 2;

#define CODE "int y = 42;"
__EXPAND__(CODE)              // → int y = 42;

// Works in #if too:
#define DEBUG "1"
#if __EXPAND__(DEBUG)
// ...
#endif
```

This is the primitive that enables `__ENV__` to work with `#if`.

### `__ENV__(NAME)` and `__ENV__(NAME, "default")`

Gets an environment variable as a string literal.

```c
const char* home = __ENV__(HOME);           // → "/home/user"
const char* user = __ENV__(USER);           // → "username"
const char* missing = __ENV__(NOTSET);      // → "" (empty string)
const char* safe = __ENV__(NOTSET, "fallback");  // → "fallback"
```

Combine with `__EXPAND__` for conditional compilation based on environment:

```c
#define BUILD_TYPE __ENV__(BUILD, "release")
#if __EXPAND__(BUILD_TYPE) == release
// release build
#endif

// Or use numeric env vars:
#if __EXPAND__(__ENV__(DEBUG, "0"))
int debug_mode = 1;
#endif
```
