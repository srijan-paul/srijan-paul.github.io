---
template: post
tags:
  - compilers 
  - assembly 
  - zig 
  - programming-languages
date: 2024-11-30
title: 'JIT compiler from scratch – 2/3'
meta: Exploring the Arm architecture in part 2 of the JIT compilation series 
is_blog_post: true
---

In the [previous post](/jit-01/), we set up a simple stack-based bytecode interpreter.
Now, we'll study the Arm instruction-set, then generate and execute machine code
while a program is running.

Know that while I'm on an Apple Silicon laptop, 
you can still use [qemu](https://www.qemu.org/docs/master/system/target-arm.html) to run ARM binaries x86 machines.
Better yet, you can grab the x86-64 manual and follow along with that instead.
Lastly, if you're not intimately familiar with the relationship between machine code, assembly files, and C source,
you can read [this short guide](/blog/c-source-to-machine-code/) I wrote as a supplement.

Everything I explain here was sourced from [the Arm reference manual](https://developer.arm.com/documentation/ddi0487/latest).
If you're going to follow along, I recommend having a local copy of the manual handy.

## The Arm instruction set.

An Arm CPU can execute code in two modes: 64-bit, called Aarch64, and 32-bit.
Only the 64-bit mode is relevant for today's desktop computers.

An Arm machine has 31 general purpose registers, labelled `x0` through `x30`.
Besides those, there are two "special" registers:

1. SP – The stack pointer.
2. ZR – A read-only register that always stores the constant `0`. 

There are also SIMD, floating point, and SVE registers,
but we do not need them for our simple interpreter.
In fact, we only need to reference two sections of the manual:
C3, the A64 instruction set overview, and C4, the instruction set encoding.

Every ARM instruction is a 32-bit integer,
and the CPU will use its value to figure out what it's instructed to do.
For example, the instruction used to return from a function, `ret`, is encoded as 
the integer `0xd65f03c0`.

Some instructions have "operands", which can be registers or constant values.
The instruction for adding two registers is represented as `ADD [Rd] [Rm] [Rn]`,
where `Rd` is the destination register, and `Rm` and `Rn` are the two source registers.
To explain the encoding of `ADD`, I've made this table where 
the left column lists the bit positions, and the right shows the value
stored in those bits of the machine code instruction.

| Bit Position | Meaning |
|--------------|---------|
| 0-4          | `Rd`      |
| 5-9          | `Rn`      |
| 10-15        | `imm`     |
| 16-20        | `Rm`      |
| 21           | `0`       |
| 22-23        | `shift`   |
| 24-30        | `1101000` |
| 31           | `sf`      |

So in the 32-bit integer used to encode an ADD instruction,
the lowest five bits store the destination register, the next five 
store the first source register, and bits `16-20` store the other source register.

We can ignore the `shift` and `imm` bits.
These are used to scale the addition by 2, 4, 8, etc.
when computing array indices or doing pointer arithmetic.
In our case, both of them will always be set to 0.

The `sf` bit is used to differentiate between A64 and A32 modes,
and will be `1` for us, since we only ever want A64.

Finally, the `01101000` in bits 24-30 is a unique bit sequence that the CPU
associates with an ADD instruction; think of it as the opcode.

Let's try encoding `ADD x0 x1 x0` using the table above.
By plugging in `00000`, `00001`, and `00000` in the slots `Rd`, `Rm`,
and `Rn`, and setting `shift` and `imm` to 0, we get:

```
0b1_00_01011_00_0_00000_000000_00001_00000
```

The hexadecimal value for this binary integer is `0x8B000020`, which
happens to be the exact op-code for `add x0 x1 x0`.

To verify this, we can write some assembly code:

```asm
; compile using: as sum.s -o sum.o
sum:
    add x0, x1, x0
    ret
```

Using `objdump -d ./sum.o`, you'll see: 

```
Disassembly of section __TEXT,__text:

0000000000000000 <sum>:
       0: 8b000020     	add	x0, x1, x0
       4: d65f03c0     	ret
```

And there it is, the `0x8B000020` right next to our instruction.

## The A64 calling convention

Finally, let's brush up on some rules around function-calling.
There are more of them than what I will cover, but here is all we need to keep in mind:

1. Function arguments are passed in registers X0-X7.
2. Return values are also stored in X0-X7.
3. To return to caller, the `ret` instruction is used.

## Generating machine code at runtime

Since machine code is merely a sequence of integers,
does that mean we can generate stuff a bunch of them
somewhere in memory, then ask the CPU to start executing
the said memory location as if it were machine code?

We can try:

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define ADD_X0_X0_X0 0x8b000000
#define RET          0xd65f03c0

typedef int64_t (*twice_fn)(int64_t);

int main() {
    // 1. allocate a buffer
    int* instrs = malloc(sizeof(int) * 2);
    
    // 2. write our instructions into that buffer
    instrs[0] = ADD_X0_X0_X0;
    instrs[1] = RET;
    
    // 3. cast the heap alloc'ed int* 
    // to a function pointer, then call it.
    twice_fn twice = (*twice_fn)instrs;
    printf("2 * 5 = %lld\n", twice(5));

    free(instrs);
    return 0;
}
```

Attempting to run this program will most likely end in a `bus error`.
This is because of page protection. In most operating systems, memory
is split up into chunks called "pages", and not all pages are created equal.
When an executable is loaded into memory, only the `.text` section—the part
that stores the code—has the privilege to run as machine code.

The heap, where `instrs` points, doesn't have the necessary permissions.
To execute arbitrary code generated at runtime, we need to request
the operating system for a page that has the permission to execute.
On Linux and MacOS, we can do that with the [mmap](https://man7.org/linux/man-pages/man2/mmap.2.html) syscall,
by passing it flags describing the permissions we want our page to have:

```c
#ifndef __APPLE__
    // This is a MacOS only flag.
    // On other OSes, we can default it to 0.
    #define MAP_JIT 0
#endif

int *alloc_code_buf(size_t size) {
  int const prot = PROT_EXEC | PROT_WRITE;
  int const flags = MAP_ANON | MAP_PRIVATE | MAP_JIT;
  void *buf = mmap(NULL, size, prot, flags, -1, 0);
  return buf;
}

int dealloc_code_buf(int *buf, size_t size) {
  return munmap(buf, size);
}
```

Now, we can replace `malloc` and `free` with
`alloc_code_buf` and `dealloc_code_buf`:

```c
int main() {
  const size_t codesize = 2;

  int *code = alloc_code_buf(codesize);
  if (code == MAP_FAILED) {
    perror("mmap");
    return 1;
  }
    
  #ifdef __APPLE__
      // On MacOS, you'll want to do this
      // before writing memory allocated
      // with MAP_JIT.
      pthread_jit_write_protect_np(false);
  #endif

  code[0] = ADD_X0_X0_X0; // add x0, x0, x0
  code[1] = RET;          // ret
    
  #ifdef __APPLE__
      pthread_jit_write_protect_np(true);
  #endif

  int2intfn f = (int2intfn)code;
  printf("twice of x is %lld\n", f(100));

  if (dealloc_code_buf(code, codesize) != 0) {
    perror("munmap");
    return 1;
  }

  return 0;
}
```

Now, let's run this program again:

```
$ gcc -O2 ./main.c -o ./main && ./main
twice of x is 200
```

Brilliant!
Using `mmap` is key to the JIT compiler we'll implement in the next part.
You probably already have an inkling of what's coming next, feel free to
try yourself.
