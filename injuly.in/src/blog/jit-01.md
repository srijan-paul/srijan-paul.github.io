---
template: post
is_blog_post: false
tags:
  - compilers 
  - zig
  - programming-languages
date: 2024-09-18
title: JIT compiler from scratch – 1/3
meta: Write a JIT compiler from first principles in Zig. 
---

In one of my discord livestreams, I did this little challenge
where I write a JIT compiler from scratch using only four tools at
my disposal: The Zig compiler, the ARM reference manual, `objdump`,
and the `man` pages.
The code is now available [on my GitHub](https://github.com/srijan-paul/tinyjit).

Last month, I also gave [a talk](/jit-basics) on the fundamentals of JIT compilation,
using the repo as a reference implementation.
Neither of these sessions we're recorded,
so for completeness sake, I'm writing this three piece guide to explain
how JIT compilers work without any fancy optimizations or live profiling.

In three small steps, we'll write a working JIT compiler:

1. Describe the language, then write a bytecode VM for it.
2. Experiment with ARM assembly instructions and the `mmap` syscall.
3. Add a JIT compiler and measure performance.

For every step of the process, I'll leave a link to the code at that stage.
I'll be using Zig, but you could use any language you like to follow along.

# A Bytecode interpreter

This is the easiest part of the process,
yet the one I'll expend the most words on.
No matter how simple, understanding the interpreter's model
thoroughly will allow us to focus on implementing the JIT compiler.

For similar reasons, I've kept the language simple.
I should caution you though, do not be fooled by the small instruction set;
It is capable of non-trivial programs, and can be extended with higher level
syntax for statements, loops, and functions if you so desire.

And let's be honest, you've probably already read [crafting interpreters](https://craftinginterpreters.com/)
(or something similar) if you're learning about JITs.

## The Instruction set

For the rest of this guide, we will be optimizing a stack based
assembly-like language.
Here's the instruction set:

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
/// Shorthand to convert an Opcode to a `u8`.
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
We store the constants separately, and instructions refer to them using their index
(`push 0` will push `constants[0]` onto the stack).
This is because our instructions are 64-bit integers,
but an operand can only store 8 bits of data.

Notice how when referencing the instructions, we prefix them with a dot.
Zig enum types are inferred based on usage,
so the compiler will interpret `.push` as `Opcode.push`.


Finally, a skeleton for the interpreter:

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
// main.zig -> struct Interpreter

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
If you run the program again with `zig build run`,
you should see `30` printed to the console.

The `eq`, `load_var`, and `store_var` instructions are just as trivial to implement:

```rs
// main.zig -> struct Interpreter -> fn run
switch (op) {
     // .add, .push -> implemented above
    .eq => self.push(if self.pop() == self.pop() 1 else 0),
    .load_var => {
        const stack_index = self.operand();
        self.push(self.stakc[stack_index]);
    },
    .store_var => {
        const stack_index = self.operand();
        self.stack[stack_index] = self.pop();
    },
    else => @panic("Not implemented"),
}
```

The jump instructions are only slightly more complex.
Since we have two jump instructions, we'll use a helper function:

```rs
// main.zig -> struct Interpreter
fn jump(self: *Interpreter) void {
    var block_index = self.operand();
    self.current_block = self.program[block_index];
    // start from the first instruction in the new block
    self.instr_ptr = 0;
}
```

With that, the interpreter loop is complete:

```rs
// main.zig -> struct Interpreter -> fn run

// while (..) {
// switch (..) {
.jump => try self.jump(),
.jump_nz => {
    if (self.pop() != 0) {
        try self.jump();
    } else {
        // skip the block index operand
        _ = self.operand(); 
    }
},
```

You can now run a slightly more compelx program,
such as [this one](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/main.zig#L26-L67)
that computes the sum of the first million natural numbers.

[^1]: Technically, PHP, Perl, and Lua without the JIT are still mainstream.
