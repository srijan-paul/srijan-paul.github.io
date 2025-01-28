---
template: post
tags:
  - compilers 
  - assembly 
  - zig 
  - programming-languages
date: 2025-01-28
title: 'JIT compiler from scratch – 3/3'
meta: Wrapping up our JIT compiler in Zig.
is_blog_post: true
---

After [part 2](/jit-02/), you should have a decent understanding of instruction encoding
for your machine's CPU architecture—ARMv8a in my case—and how to generate and execute machine instructions
at runtime with `mmap`. That we will map our VM opcodes to machine instructions should come at no surprise, then.

## The unit of compilation 

Remember that we use `mmap` to allocate an executable buffer of instructions,
then cast it to a function pointer and call it. 
Currently, the biggest overhead in our VM is the `switch` dispatch.
The core interpreter loop is roughly this
(find
[loadConst](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/interpreter.zig#L71)
and 
[pop](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/interpreter.zig#L83)
on the GH repo):

```zig
while (self.pc < self.current_block.instructions.len) {
    const instr: Opcode = @enumFromInt(self.nextOp());
    switch (instr) {
        .push => self.push(self.loadConst()),
        .add => {
            const a = self.pop();
            const b = self.pop();
            self.push(a + b);
        }
        // ... other opcodes 
    }
}
```

Meaning that for a block like this:

```asm
; CONSTANTS = [10, 20]
b0:
    PUSH 0 ;pushes 10
    PUSH 1 ;pushes 20
    ADD    ;pops twice, pushes 30
```

Our interpreter performs ~7 memory reads to fetch the opcodes 
and their operands, then a few branches to determine what to do for each 
opcode, as can be seen in the `switch` statement above.

If we remove all the op-code fetching and branching,
our entire block maps to this zig snippet:

```zig
self.push(self.loadConst()); // PUSH 0
self.push(self.loadConst()); // PUSH 1
// ADD
const a = self.pop();
const b = self.pop();
self.push(a + b);
```

For a second, lets ignore the interpreter state
and write the `b0` block as if it were a C function:

```c
/// @param stack Stack of values 
///
/// @param sp Index of the topmost item in the stack.
/// Modified when the stack grows or shrinks.
///
/// @param instrs Array of instructions
///
/// @param ip Index of the current instruction.
/// Modified when a new instruction is fetched.
///
/// @param K Array of constants in the program.
void b0(
    long *stack, size_t *sp, 
    long *instrs, size_t *ip, 
    long* K
) {
    // PUSH 0
    ++*ip; // fetch operand for push
    stack[*sp] = K[instrs[*ip]]; // put constant on stack
    ++*sp; // increment stack size

    ++*ip; // advance instruction pointer

    // PUSH 1
    ++*ip; // fetch operand for push
    stack[*sp] = K[instrs[*ip]]; // put constant on stack
    ++*sp; // increment stack size

    ++*ip; // advance instruction pointer

    // ADD
    long a = stack[*sp]; --*sp; // a = pop 
    long b = stack[*sp]; --*sp; // a = pop
    stack[*sp] = a + b;         // push(a + b)
}
```

Notice that we've removed any branching that our `switch` based interpreter loop had.
To verify, we can compile compile this function with gcc:

```asm
; Register | Parameter
; ---------|-----------
; x0       | stack
; x1       | sp
; x2       | instrs
; x3       | ip
; x4       | K
b0:
  ; "load" the PUSH instruction
  ; by incrementing the instruction pointer:
  ; ++*ip;
  ldr     x8, [x3]    ; x8 <- *ip
  add     x8, x8, #1  ; x8 <- x8 + 1
  str     x8, [x3]    ; *ip <- x8

  ; fetch the operand for PUSH
  ; stack[*sp] = K[instrs[*ip]];
  ldr     x8, [x2, x8, lsl #3] ; x8 <- instrs[*ip]
  ldr     x9, [x1]             ; x9 <- *sp
  ldr     x8, [x4, x8, lsl #3] ; x8 <- K[x8]
  str     x8, [x0, x9, lsl #3] ; stack[*sp] <- x8
    
  ; increment the stack size
  ; ++*sp;
  ldr     x8, [x1]   ; x8 <- *sp
  add     x8, x8, #1 ; x8 <- x8 + 1
  str     x8, [x1]   ; *sp <- x8
    
  ; Advance instruction pointer 
  ; ++*ip;
  ldr     x8, [x3]
  add     x8, x8, #2
  str     x8, [x3]
    
  ; Repeat for PUSH 1
  ldr     x8, [x2, x8, lsl #3]
  ldr     x9, [x1]
  ldr     x8, [x4, x8, lsl #3]
  str     x8, [x0, x9, lsl #3]
  ldr     x8, [x1]
  add     x8, x8, #1
  str     x8, [x1]
  ldr     x8, [x3]
  add     x8, x8, #1
  str     x8, [x3]

  ; load the first operand for add into x10
  ldr     x8, [x1]
  sub     x9, x8, #1
  ldr     x10, [x0, x8, lsl #3]
  sub     x8, x8, #2
  str     x9, [x1]
  ; load second operand for add into x9
  ldr     x9, [x0, x9, lsl #3]
  str     x8, [x1]
  ; x9 = x9 + x10
  add     x9, x9, x10
  ; push result to stack
  str     x9, [x0, x8, lsl #3]
  ret
```

Our strategy for JIT compilation is simple:

1. Monitor the interpreter's execution, and every time a block
   loops back to *itself*, we recognize that block as the body of a *hot loop*.
2. Extract the instructions in that block and compile them to a `mmap`ed
   function equivalent to `b0`.
3. Whenever the block is reached next, call the compiled function instead
   of interpreting the instructions.

Before we make any changes to the interpreter, we first need an assembler to compile opcodes to machine code.
We'll keep the assembler in a separate file called [`jit.zig`](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/jit.zig):

```zig
// src/jit.zig
const std = @import("std");
const interp = @import("interpreter.zig");
const mman = @cImport(@cInclude("sys/mman.h"));
const pthread = @cImport(@cInclude("pthread.h"));

const CodeBlock = interp.CodeBlock;
const Interpreter = interp.Interpreter;
const Opcode = interp.Opcode;
const Op = interp.Op;
```


Recall that previously, the `JUMP` opcode called this helper function:

```zig
fn jump(self: *Interpreter) !void {
    // block index is the next "instruction".
    const block_idx = self.nextOp();
    self.pc = 0; // start from first instr in the next block
    const dst_block = &self.blocks[block_idx];
    self.current_block = dst_block;
}
```

With JIT compilation, this changes: 

```zig
fn jump(self: *Interpreter) !void {
    const block_idx = self.nextOp();
    self.pc = 0;
    const dst_block = &self.blocks[block_idx];
    
    if (self.is_jit_enabled) {
        // check if the block has been compiled before.
        if (self.jit_blocks[block_idx]) |*compiled| {
            self.callJit(compiled);
            return;
        }

        if (self.current_block == dst_block) {
            // jump destination is the same as the current
            // block potentially a hot loop, so compile this
            try self.doJit(block_idx);
        } else {
            // Not a self-referencing block, interpret.
            self.current_block = dst_block;
        }
    } else {
        self.current_block = dst_block;
    }
}
```

The `doJit` and `callJit` functions are straightforward as well:

```zig
/// Call a JIT compiled function
fn callJit(self: *Interpreter, compiled: *const jit.CompiledFunction) void {
    var next_block_idx: usize = 0; // inout parameter for JITted function
    compiled.func(
        (&self.stack).ptr,
        self.current_block.instructions.ptr,
        &self.stack_pos,
        &self.pc,
        &next_block_idx,
        self.current_block.constants.ptr,
    );

    self.current_block = &self.blocks[next_block_idx];
    // self.pc = 0;
}

fn doJit(self: *Interpreter, block_index: usize) !void {
    const block = &self.blocks[block_index];
    const compiled = try self.jit_compiler.compileBlock(block_index, block);

    self.jit_blocks[block_index] = compiled;
    self.callJit(&compiled);
}
```
