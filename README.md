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
Which compiles to:
```
import io
function fib 1
  move r0 r10
  move r10 0
  move r11 1
  move r12 r0
  tail_call function fib_inner
end

function fib_inner 3
  move r0 r10
  move r1 r11
  move r2 r12
  move r3 r2
  scmp r3 0
  jump gt label L0
  move r3 r0
  move r15 r3
  ret
  label L0
  move r10 r1
  move r11 r0
  move r3 r1
  add r11 r11 r3
  move r12 r2
  sub r12 r12 1
  tail_call function fib_inner
end

function start 0
  move r0 0
  label L0
  move r1 r0
  scmp r1 20
  jump ge label L1
  move r10 "fib(%zu) = %zu\n"
  push r10
  move r11 r0
  push r11
  move r10 r0
  push r0
  call function fib
  pop r0
  move r12 rout1
  pop r11
  pop r10
  push r0
  call function io.printf2
  pop r0
  move r1 r0
  add r1 r1 1
  move r0 r1
  move rip label L0
  label L1
  ret
end
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
