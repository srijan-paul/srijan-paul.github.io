---
template: post
is_blog_post: true
tags:
  - compilers 
  - zig
  - programming-languages
date: 2024-10-03
title: JIT compiler from scratch – 1/3
meta: Write a JIT compiler from first principles in Zig. 
---

On a discord call with a friend, I did this little challenge
where I attempt to write a JIT compiler from scratch with only four tools at
my disposal: a Zig compiler, the ARM reference manual,
`objdump`, and the man pages.
The code is now available [on GitHub](https://github.com/srijan-paul/tinyjit).

Last month, I also gave [a talk](/jit-basics.html) on the fundamentals of JIT compilation,
using the repo as a reference implementation.
Neither of these sessions we're recorded, though.
So for completeness's sake, I'm writing this three-part guide to explain
how JIT compilers work, barring any fancy optimizations or live profiling.

We'll break this process down into three small steps:

1. Describe our language, then write a bytecode VM for it.
2. Experiment with ARM assembly instructions and the `mmap` syscall.
3. Add a JIT compiler to our VM and measure performance.

For each step, I'll leave a link to the code at the end of that stage.
Although we're using Zig, the concepts are language-agnostic, and you can follow
along with something like C or Rust.

# A Bytecode interpreter

This is the easiest step,
yet the one I'll expend the most words on.
No matter how simple, understanding the interpreter's model
thoroughly will allow us to focus on implementing the compiler.

For similar reasons, I've kept the language simple.
I should caution you though, do not be fooled by the small instruction set;
It is capable of non-trivial programs, and can be extended with higher level
syntax for statements, loops, and functions if you so desire.

And let's be honest, you've probably already read [crafting interpreters](https://craftinginterpreters.com/)
(or something similar) if you care about JITs.

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
So we write a "virtual" machine to execute our custom instruction set,
starting with an enum to represent the instructions:

```zig
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

Because of the `enum(u8)` syntax,
the enum values will be represented with `u8`s at runtime.
This allows us to freely cast between `u8`s and `Opcode`s.

Next, a small helper to spare you the pain of writing `@intFromEnum`:

```zig
/// Shorthand to convert an Opcode to a `u8`.
pub inline fn Op(num: Opcode) u8 {
  return @intFromEnum(num);
}
```

Let's imagine how one might want to call an interpreter,
and bang out `main` function:

```zig
const std = @import("std");

pub fn main() void {
  // constant values used in the program
  const constants = [_]i64{10, 20};
  const code = [_]u8{
    Op(.push), 0, // push 10 (constant #0)
    Op(.push), 1, // push 20 (constant #1)
    Op(.add),     // push(pop() + pop())
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

Obviously, this won't compile just yet.
We've dreamt up an imaginary API to guide our implementation.

A program in our virtual machine is represented as an array of `CodeBlock`s.
This way, jump instructions can reference a block using its index in the program array.
We store the constants separately, and instructions refer to them using their index
– so `push 0` will push `constants[0]` onto the stack. [^1]

Notice how the instructions are prefixed with a dot.
Zig enums can be inferred based on usage,
so the compiler will infer `.push` as `Opcode.push`.


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
error with the phrase "Not Implemented!" somewhere in it.

`@This` is a builtin function that returns the nearest surrounding type –
`Interpreter` in this case.
Within the struct's scope, `Self` and `Interpreter` are the same type.
We don't necessarily need this alias,
but it's a common convention and saves us a few keystrokes.

Zig does not make much of a distinction between types and values,
so we can assign a type to a variable,
then use it in annotations [^2].

Before we flesh out the `run` function, we'll need a few more helpers:

```zig
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

/// Read one byte from the instructions array, and cast to u8
pub inline fn operand(self: *Self) u8 {
  const op = self.instructions[self.instr_ptr];
  self.instr_ptr += 1;
  return @intFromEnum(op);
}
```

With these, we can rewrite `run` to have a basic interpreter loop:

```zig
// main.zig -> struct Interpreter
pub fn run(self: *Self) void {
  while (self.instr_ptr < self.current_block.instructions.len) {
    const op = self.current_block.instructions[self.instr_ptr];
    self.instr_ptr += 1;
    switch (op) {
      .add  => self.push(self.pop() + self.pop()),
      .push => self.push(self.operand()),
      else  =>  @panic("Not implemented"),
    } 
  }
}
```
If you run the program again with `zig build run`,
you should see `30` printed to the console.

The `eq`, `load_var`, and `store_var` instructions
are similarly trivial:

```zig
// main.zig -> struct Interpreter -> fn run
.eq => self.push(if self.pop() == self.pop() 1 else 0),
.load_var => {
  const stack_index = self.operand();
  self.push(self.stack[stack_index]);
},
.store_var => {
  const stack_index = self.operand();
  self.stack[stack_index] = self.pop();
},
else => @panic("Not implemented"),
```

The jump instructions are only slightly more complex.
Since we have two jump instructions, we'll use a helper function:

```zig
// main.zig -> struct Interpreter
fn jump(self: *Self) void {
  var block_index = self.operand();
  self.current_block = self.program[block_index];
  // start from the first instruction in the new block
  self.instr_ptr = 0;
}
```

With that, the interpreter loop is complete:

```zig
// main.zig -> struct Interpreter -> fn run
.jump => try self.jump(),
.jump_nz => {
  if (self.pop() != 0) {
      try self.jump();
  } else {
      // skip the block index
      _ = self.operand(); 
  }
},
```

You can now run more complex programs,
like [summing up the first million natural numbers](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/main.zig#L26-L67).

[^1]: This indirection is necessary because while values are 64-bit integers, the opcodes are only 8-bit.
[^2]: In fact, the only way to declare types in zig is by assigning them to a variable.
Even our interpreter is defined like that: `const Interpreter = struct { .. };`
