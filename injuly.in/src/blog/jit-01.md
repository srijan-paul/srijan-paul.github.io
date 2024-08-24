---
template: post
is_blog_post: false
tags:
  - compilers 
  - zig
  - programming-languages
date: 2024-08-16
title: JIT Compiler from scratch – 1/3
meta: Write a JIT compiler from first principles in Zig. 
---

Have you perhaps written a programming language interpreter, or a bytecode VM?
Me too.

So then, tried to squeeze more performance out of it by 
adding computed-gotos, pointer tagging, a peephole optimizer,
or instruction re-ordering?
Yeah... me neither.
Vanilla bytecode VMs are okay for many workloads – 
databases still use them.

But it doesn't hurt to get on with the times, right?
Even Python—the language that stores *integers on the heap*(!)—is getting
a JIT compiler.
Think about it, what mainstream interpreted language *doesn't* have a JIT today?[^1]

Bytecode-only VMs will soon be a thing of past, and download links to them will re-direct to
the wayback machine.
But you don't want your interpreter on the internet's cemetery, do you?

Thought so. Well, I have you covered.

We'll learn the ins-and-outs of a JIT compiler by implementing one from scratch
with zero dependencies.
No LLVM.

# A Bytecode Machine

## SlackJaw

For the rest of this guide, we will be optimizing a friendly stack based
assembly-like language called SlackJaw.

To focus on implementing a JIT from first principles, I've kept the language simple.
This will help speed past the parser and bytecode VM implementation.
I should caution you though, do not be fooled by the small instruction set!
It is capable of non-trivial programs, and can be extended with higher level
syntax for statements, loops, and functions if you so desire.

As we draw closer to the final chapter, we'll see how features like I/O
and native functions can be implemented to further the horizon for our toy language.
By the end, you'll have enough knowledge to write a JIT for a sufficiently complex language. 

And let's be honest, you've probably already read [crafting interpreters](https://craftinginterpreters.com/)
(or something similar) if you're learning about JITs.

Here's a slackjaw program that sums all natural numbers less than one million:

```asm
push x 0
push i 0

loop:
  push (i < 10000000)
  cjmp loop_end

  push (x + i)
  set  x
    
  add (i + 1)
  set  i

  jump loop

loop_end:
  print
```

If you've seen assembly code out in the wild,
you may find the syntax vaguely familiar.

As we learned earlier, SlackJaw is a *stack-machine* –
the instructions in this program manipulate the stack to crunch numbers.

Lets go through the above program with a fine-toothed comb.

In the first two lines, we push two zeros onto the stack, and "remember" the slots
they're stored in as `x` and `i`.

```asm
push x 0 ; x = 0
push i 0 ; i = 0
```


This is the equivalent of a variable declaration in, say, Python.

In what follows, we have a "label" – a named block of code in a program:

```asm
loop:  ; while (i <= 10000000) {
    push (i > 10000000)
    cjmp loop_end
```

We will use the label name `loop` later when we want to jump back to the third line.
`push (i > 10000000)` will push `1` onto the stack if the condition is true, and `0` otherwise.

The `cjmp` instruction will pop a value off the stack,
and jump the program to the specified label if the popped is non-zero.

By now, you should be able to make sense of the entailing lines.
`set` modifies the value in a named stack slot—in this case, `x` and `i`—
and `jump` unconditionally jumps the program to a labelled block.

I will introduce a few more instructions as we advance, and you can always
add more to suit your needs.

## The SlackVM

Unlike regular assembly programs, Slackjaw code cannot natively run on a CPU
that understands x86, x64, or ARM instructions.
So we write a "virtual" machine to execute our custom instruction set.

First, an enum to represent every instruction in the VM:

```rust
pub const Opcode = enum(u8) {
    push,
    add,
    eq,
    jump_nz,
    jump,
    load_var,
    store_var,
};
```

The `enum(u8)` orders Zig to use a byte-wide unsigned integer to represent
the instructions.
This allows us to cast a `u8` to an `Opcode` and vice-versa.

Next, a small helper to spare you the pain of writing `@intFromEnum` to convert an op-code to a number:

```rust

/// Shorthand to convert a u8 to an Opcode
pub inline fn Op(num: u8) Opcode {
    return @enumFromInt(num);
}
```

Let's imagine how one might call an interpreter to execute some code,
and bang out a main function:

```zig
pub fn main() void {
    const program = [_]Opcode{
       .push, Op(2),
       .push, Op(3),
       .add,
       .print
    };

    const vm = Interpreter.init(program);
    vm.run();
}
```


Notice how when referencing the instructions, we prefix them with a dot.
Zig enum types are inferred based on usage,
so the compiler will interpret `.push` as `Opcode.push`.

Finally, the skeleton for the interpreter:

```zig
const Interpreter = struct {
    const Self = @This();
    stack: [32000]i64 = undefined,
    program: []Opcode,
    /// Index of next free stack slot.
    stack_pos: usize = 0,
    /// Index of the next instruction to execute.
    instr_pos: usize = 0,

    pub fn init(program: []Opcode) Self {
       return Self{ .program = program }; 
    }

    pub fn run() void {
        std.debug.panic("Not implemented!", .{});
    }
};
```

If you plop this into your `main.zig` file and hit `zig build run`, you should see an
error message that says "Not Implemented!" somewhere in it.

`@This` is a builtin function that returns the nearest surrounding type –
`Interpreter` in this case.
Within the struct's scope, `Self` and `Interpreter` are the same thing.
We don't necessarily need this alias, but it's a common convention and saves us a few keystrokes.

Remember, Zig does not make a semantic distinction between types and values, so we 
can assign a type to a variable, and then use it in type annotations.
It is no different from assigning the struct type definition to the `Interpreter` variable.

Before we flesh out the `run` function, we'll need a few more helpers:

```zig
// Inside `Interpreter`:

pub inline fn push(self: *Self, value: i64) void {
    self.stack[self.stack_ptr] = value;
    self.stack_ptr -= 1;
}

pub inline fn pop(self: *Self) i64 {
    self.stack_ptr -= 1;
    const value = self.stack[self.stack_ptr];
    return value;
}

/// Read one byte from the instructions array and cast to u8
pub inline fn operand(self: *Self) u8 {
    const op = self.instructions[self.instr_ptr];
    self.instr_ptr += 1;
    return @intFromEnum(op);
}
```

With these, we can implement a basic interpreter loop:

```zig
pub fn run(self: *Self) void {
    while (self.instr_pos < self.program.len) {
        const op = self.program[self.instr_ps];
        self.instr_pos += 1;

        switch (op) {
            .print => std.debug.print("{d}\n", .{self.pop()}),
            .add  => self.push(self.pop() + self.pop()),
            .push => self.push(self.operand()),
            else => std.debug.panic("not implemented!", .{});
        } 
    }
}
```

After these changes, you should see `5` in your terminal upon running the code.

### Setting up

[^1]: Technically, PHP, Perl, and Lua without the JIT are still mainstream.
