---
template: post
is_blog_post: false
tags:
  - compilers 
  - zig
  - programming-languages
date: 2024-08-16
title: JIT Compiler from scratch – 1/2
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

## The Instruction set

For the rest of this guide, we will be optimizing a friendly stack based
assembly-like language.

To focus on implementing a JIT from first principles, I've kept the language simple.
This will help speed past the parser and bytecode VM implementation.
I should caution you though, do not be fooled by the small instruction set;
It is capable of non-trivial programs, and can be extended with higher level
syntax for statements, loops, and functions if you so desire.

And let's be honest, you've probably already read [crafting interpreters](https://craftinginterpreters.com/)
(or something similar) if you're learning about JITs.

We'll be writing a stack-based VM. Here's the instruction set:

- `push <value>`: Pushes a value onto the stack.
- `add`: Pops two values, pushes their sum.
- `eq`: Pops two values, pushes 1 if they are equal, 0 otherwise.
- `jump <block-id>`: jumps to the beginning of a block labeled with the ID.
- `jump_nz <block-id>`: pops a value, jumps to the block if it is is non-zero.
- `load_var <var-id>`: copies the value at stack index `var-id` back to the top of stack.
- `store_var <var-id>`: pops a value and stores it at stack index `var-id`.


## The Virtual Machine

Unlike regular assembly programs, our bytecode cannot natively run on a CPU
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

```rs
/// Shorthand to convert an Opcode to a number
pub inline fn Op(num: Opcode) u8 {
    return @intFromEnum(num);
}
```

Let's imagine how one might call an interpreter to execute some code,
and bang out a main function:

```rs
const std = @import("std");
pub fn main() void {
    // constant values used in the program
    const constants = [_]i64{10, 20};
    const code = [_]u8{
       Op(.push), 0, // push 10
       Op(.push), 1, // push 20
       Op(.add),     // push(pop() + pop()) ;=30
    };

    const program = [_]CodeBlock{
        .{
            .instructions = code,
            .constants    = constants,
        }
    };

    const vm = Interpreter.init(program);
    vm.run();
    std.debug.print("{d}\n", .{vm.stack[vm.stack_pos]});
}
```

A program in our virtual machine is represented as a list of blocks.
This way, jump instructions can reference blocks using their index in the program array.
We store the constants separately, and instructions refer to them using their index (
`push 0` will push `constants[0]` onto the stack).
This is because our instructions are 64-bit, but an operand can only store 8 bits of data.

Notice how when referencing the instructions, we prefix them with a dot.
Zig enum types are inferred based on usage,
so the compiler will interpret `.push` as `Opcode.push`.


Finally, the skeleton for the interpreter:

```zig
const CodeBlock = struct {
    instructions: []u8,
    constants:    []i64
};

const Interpreter = struct {
    const Self = @This();
    stack: [32000]i64 = undefined,
    program: []CodeBlock,
    current_block: *CodeBlock = undefined,
    /// Index of next free stack slot.
    stack_ptr: usize = 0,
    /// Index of the next instruction to execute.
    instr_ptr: usize = 0,

    pub fn init(program: []CodeBlock) Self {
       return Self{
           .program = program, 
           .current_block = &program[0],
       }; 
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

Zig does not make a semantic distinction between types and values, so we 
can assign a type to a variable, and then use it in type annotations.
It is no different from assigning the struct type definition to the `Interpreter` variable.

Before we flesh out the `run` function, we'll need a few more helpers:

```rs
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

```rs
pub fn run(self: *Self) void {
    while (self.instr_ptr < self.current_block.instructions.len) {
        const op = self.current_block.instructions[self.instr_ptr];
        self.instr_ptr += 1;

        switch (op) {
            .add  => self.push(self.pop() + self.pop()),
            .push => self.push(self.operand()),
            else =>  @panic("Not implemented"),
        } 
    }
}
```


### Setting up

[^1]: Technically, PHP, Perl, and Lua without the JIT are still mainstream.
