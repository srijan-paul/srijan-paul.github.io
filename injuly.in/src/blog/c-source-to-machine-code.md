---
template: post
is_blog_post: true
tags:
  - assembler
  - linker
  - C programming
  - low-level 
date: 2024-11-04
title: 'C Source to machine code – step by step'
meta: Some basic terminology that you'll come across when programming in systems languages. 
---

<!--
In the [previous post](/jit-01/), we set up a simple bytecode interpreter for a stack-based language.
Now, we'll study the ARM instruction-set, and see how we can generate and execute machine code at runtime. 

I am on an apple silicon laptop, but you may not be.
To run ARM code on your machine, you can use [qemu](https://www.qemu.org/download/).

Alternatively, you can grab the x86-64 manual, and follow along with that instead.
-->

Originally, I wrote this short article as a part of my [JIT fundamentals](/blog/jit-01/) series.
In retrospect, this may be useful as a quick-reference to beginner programmers
wanting to gain clarity about how a computer runs C programs, or how a compiler goes from
source text to machine code.

We'll write an extremely simple  C-program,
and see how it goes from a text to assembly, then to an object file, and finally an executable.

If you don't know what architecture your computer is based on, try running the `arch` command:

```sh
$ arch
arm64
```

You may see `x86_64` instead, and that just means your CPU has a different architecture than mine.

At the end of the day,
both our CPUs only understand machine code expressed in raw bytes.
Well-formed instructions are expected to be laid out in a specific format.
For instance, when an ARM-64 CPU sees a 4-byte integer with the value `0xd65f03c0`, it
knows to *return* from the current function.

Take this C program as an example that simply exits with 5 as the exit code:
```sh
echo 'int main(){ return 5; }' > hello.c
```

A compiler like gcc or clang can convert this high-level program to 
readable *<sup>(citation needed)</sup>* assembly:

```sh
gcc -S -O2 -fverbose-asm ./hello.c -o hello.s
```

Allow me a moment to explain what some of these flags do:

1. `-S`: Do not produce an executable or `.o` file, translate to assembly instead.
2. `-O2`: Optimize the code. Unoptimized assembly is usually long and verbose.
3. `-fverbose-asm`: Insert comments where necessary.

Within the `hello.s` file you'll find this snippet of assembly code
with some safe-to-ignore debugging noise strewn about: 

```asm
_main:
	mov	w0, #5
	ret
```

If you're on an x86-64 machine, you will see this instead:

```asm
main:
  mov  eax,5
  ret
```

In the first line, we move the constant integer `5` into the 32-bit register `w0` (`eax` for x86).
Then, the `ret` instruction is used to exit the current function.

The respective manuals for ARM and x86 will tell you that the return value for functions are
placed in the `w0` register for ARM, and `eax` for x86.

This assembly then naturally follows from our `return 5;` statement in `main()`.

Moving on, lets use [as – the GNU assembler](https://en.wikipedia.org/wiki/GNU_Assembler) to
convert the text-file with assembly code to raw CPU instructions:

```sh
as ./hello.s -o ./hello.o
```

What you have now is an *object file* that contains the machine instructions.
We can use the `objdump` utility to disassemble the file and make sense of its contents:

```sh
objdump -d ./hello.o
```

```
./hello.o:	file format mach-o arm64

Disassembly of section __TEXT,__text:

0000000000000000 <ltmp0>:
       0: 528000a0     	mov	w0, #0x5                ; =5
       4: d65f03c0     	ret
```

At the bottom we find the `0xd65f03c0` number once again – the ARM machine code instruction for `ret`.
The `4: ` prefix is the byte-offset of that instruction from the beginning of the file. 
In this case, there are only two four-byte instructions (`mov w0 #5` and `ret`),
so our file should be 8 bytes total.

Let's verify:

```sh
$ du ./hello.o
8	./hello.o
```

Looking good so far!

But the operating wouldn't know how to make sense of this file.
It doesn't know where to start executing the code from, or whether this file
is even meant to be run directly.
For all the OS knows, it could be a random string of bytes saved to a file that happens
to resemble machine instructions by sheer chance.

To convince the operating system that it's a program worth running,
we have to fit our CPU instructions inside a larger file that the OS
knows to read.

To do so, we will use the GNU linker, or [ld](https://ftp.gnu.org/old-gnu/Manuals/ld-2.9.1/html_node/ld_toc.html).
A linker's job is to patch multiple object files together to produce a binary that OS can interpret.
For a single object file, we can pass it to the linker directly:

```sh
ld  ./hello.o -o hello
```

Using the `file` command, we can see how our OS interprets this file:

```
$ file ./hello
./hello: Mach-O 64-bit executable arm64
```

Finally, we have a valid executable in a binary format that Apple calls `Mach-O`.
You may see `ELF` on Linux, or `PE` on Windows.
We can now run this binary and check the exit code:

```
$ ./hello 
$ echo $?
5
```

We're done, congratulations if you made it this far.
If tools like `objdump`, `ld`, and `file` were new to you, play with them some more to gain familiarity.

## Further reading

If anything in here was new to you, it might be a good idea to see if you can use or understand 
these closely related terms:

- Static and dynamic linking 
- Shared libraries (`.dll`, `.dylib`, and `.so`) v/s static libraries (`.a` and `.lib`).
- Word sizes, `QWORD` and `DWORD` – helpful when reading assembly, necessary when writing it.
- Stack v/s Heap – is one really slower than the other? Can the heap be equivalent in speed?
- Hardware traps and interrupts.
- Page protection and virtual memory. What does a segfault *really* mean?

