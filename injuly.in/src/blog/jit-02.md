---
template: post
is_blog_post: false
tags:
  - compilers 
  - assembly 
  - zig 
  - programming-languages
date: 2024-11-19
title: 'JIT compiler from scratch – 2/3'
meta: Exploring the Arm architecture in part 2 of the JIT compilation series 
---

In the [previous post](/jit-01/), we set up a simple bytecode interpreter for a stack-based language.
Now, we'll study the Arm instruction-set, and see how we can generate and execute machine code
while a program is running.

I am on an Apple Silicon laptop, but you may not be.
To run Arm code on your machine, you can use [qemu](https://www.qemu.org/download/).

Alternatively, you can grab the x86-64 manual, and follow along with that instead.
If you're not intimately familiar with the relationship between machine code, assembly files, and C source,
it might be a good idea to read [this short post](/blog/c-source-to-machine-code/) I wrote as a supplement to this guide.

## The Arm instruction set.

Everything I explain here was originally sourced from [the Arm reference manual](https://developer.arm.com/documentation/ddi0487/latest).
If you're going to follow along, I recommend having a local copy of the manual handy.

An Arm CPU can execute code in two modes: 64 and 32-bit.
For today's desktop computers, only the 64-bit mode is relevant, and is referred to as "Aarch64" in the manual.

An Arm machine has 31 general purpose registers, labelled `x0` through `x30`.
We'll refer to them as "GPR"s from here on out.
Apart from these, there are two "special" registers:

1. SP – The stack pointer.
2. ZR – A read-only register that always stores the constant `0`. 

There are also SIMD, floating point, and SVE registers,
but we do not need them for our simple interpreter.
In fact, we only need to reference two sections of the manual:
C3, the A64 instruction set overview, and C4, the instruction set encoding.

The manual explains that every ARM instruction is a 32-bit integer,
and the CPU will decode this integer to figure out what its supposed to do.

For example, the `ret` instruction (used to return from a function) is encoded as 
the integer `0xd65f03c0`.

The instruction used to add two registers together is represented as `ADD [Rd] [Rm] [Rn]`,
where `Rd` is the destination register, and `Rm` and `Rn` are the two source registers.

To explain the encoding of `ADD`, I've made this table where 
the left column lists the bit positions, and the right shows the value
stored in that slot of the machine code instruction.

| Bit position | Meaning |
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

The `shift` and `imm` bits can be safely ignored.
These are used for bit shifting, a.k.a scaling the addition by 2, 4, 8, etc.,
when computing array indices or adding pointers.
In our case, both of them will always be set to 0.

The `sf` bit is used to differentiate between A64 and A32 modes,
and will be `1` for us, since we only ever want A64.

Finally, the `01101000` in bits 24-30 is a unique bit sequence that the CPU
associates with an ADD instruction.
Think of it as the opcode.

Let's try encoding `ADD x0 x1 x0` using the table above.
By plugging in `00000`, `00001`, and `00000` in the slots for `Rd`, `Rm`,
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

And there it is, the `0x8B000020` right next to our code.

## The A64 calling convention

Finally, let's brush up on some rules around function-calling.
There are more rules than what I will cover here, but here is all we need to keep in mind:

1. Function arguments are passed in registers X0-X7.
2. Return values are also stored in X0-X7.
3. To return to caller, the `ret` instruction is used.

## Generating machine code at runtime

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
    
    // 3. cast the int* to a function pointer, 
    // then call it.
    twice_fn twice = (*twice_fn)instrs;
    printf("2 * 5 = %lld\n", twice(5));

    free(instrs);
    return 0;
}
```

