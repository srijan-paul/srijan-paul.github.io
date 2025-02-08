---
template: post
tags:
  - compilers 
  - assembly 
  - zig 
  - programming-languages
date: 2025-02-07
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
But how do we decide how many instructions to compile at a time?
The simplest answer to compile one block of instructions at a time,
where a block is represented using the `CodeBlock` struct in our toy interpreter.

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
To verify, we can compile compile this function with gcc (comments added for clarity):

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

We get rid of two kinds of overheads immediately –
fetching the opcode from memory, and
branching to the correct opcode handler, like with the `switch` statement.

Our strategy for JIT compilation is simple:

1. Monitor the interpreter's execution, and every time a block
   loops back to *itself*, we recognize that block as the body of a *hot loop*.
2. Extract the instructions in that block and compile them to a `mmap`ed
   function equivalent to `b0`.
3. Whenever the block is reached next, call the compiled function instead
   of interpreting the instructions.

## Assembling instructions on the fly

Before we make any changes to the interpreter, we first need an assembler to compile opcodes to machine code.
We'll keep this in a separate file called [`jit.zig`](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/jit.zig).
Starting with the imports:

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

For architecture specific magic, we define a struct to generate Armv8a instructions,
and an enum to represent the registers we will be using:

```zig
// src/jit.zig
const Armv8a = struct {
    const Reg = enum(u32) { 
        x0 = 0, x1, x2,
        x3, x4, x5, x6,
        x7, x8, x9, x10,
        x11, x12
    };
};
```

In the previous entry, I had mentioned that the `ret` instruction has the opcode `0xd65f03c0` in Armv8a,
making it the simplest instruction to generate:

```diff
// src/jit.zig
 const Armv8a = struct {
     const Reg = enum(u32) { 
         x0 = 0, x1, x2,
         x3, x4, x5, x6,
         x7, x8, x9, x10,
         x11, x12
     };
+    pub const ret = 0xd65f03c0;
 };
```

The `ADD` instruction is slightly more involved, as it requires three *register* operands:
a destination and two sources: `ADD <dst> <src1> <src2>`.
I had briefly mentioned instruction encoding in the previous post, to recap:
 
> In the 32-bit integer used to encode an ADD instruction,
> the lowest five bits store the destination register, the next five 
> store the first source register, and bits `16-20` store the other source register.

> We can ignore the `shift` and `imm` bits.
> These are used to scale the addition by 2, 4, 8, etc.
> when computing array indices or doing pointer arithmetic.
> In our case, both of them will always be set to 0.

> The `sf` bit is used to differentiate between A64 and A32 modes,
> and will be `1` for us, since we only ever want A64.

> Finally, the `01101000` in bits 24-30 is a unique bit sequence that the CPU
> associates with an ADD instruction; think of it as the opcode.


So to generate an `ADD` instruction, we just need to set the registers in the right bit positions:

```zig
// src/jit.zig - struct Armv8a {
pub inline fn addRegs(
    dst_reg: Reg, reg_a: Reg, reg_b: Reg
) u32 {
    const a = @intFromEnum(reg_a);
    const b = @intFromEnum(reg_b);
    const dst = @intFromEnum(dst_reg);
    
    // 0x8b000000 is the opcode for ADD
    // (or the hex for 0b1101000 << 24).
    return 0x8b000000 | (b << 16) | (a << 5) | dst;
}
```

In the call site where we want to generate an `ADD` instruction,
we just pass the registers to this helper like so:

```zig
const Reg = Armv8a.Reg;
const add_x0_x1_x0 = Armv8a.addRegs(Reg.x0, Reg.x1, Reg.x0);
```

Obviously, we'll need more than just the `add` and `ret` instructions.
To save you the trouble of reading through all of it, I'll just link to
[the final implementation](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/jit.zig#L11)
of the `Armv8a` struct, which a bunch of functions to generate instructions for branching, comparisons, etc.

To assert the assembler's correctness, we can use `gcc` or `as` to compile some assembly code on our machine,
then compare gcc's output with that of our assembler:

```zig
test "Armv8a code generation" {
    // add x9, x11, x9
    // The "0x8b090169" was taken from using `objdump -d` on 
    // a binary compiled with `as`.
    try std.testing.expectEqual(
        0x8b090169,
        Armv8a.addRegs(.x9, .x11, .x9)
    );
}
```

## The JIT compiler

The functions generated by our JIT compiler will have the same signature as
the `b0` function in C  that we wrote earlier.
We'll represent this function type with a type alias:

```zig
// src/jit.zig
pub const JitFunction = *fn (
    stack: [*]i64, // x0
    instructions: [*]const u8, // x1
    stack_ptr: *usize, // x2
    instr_ptr: *usize, // x3
    current_block_index: *usize, // x5
    constants: [*]const i64, // x6
) callconv(.C) void;
```

Notice the `callconv(.C)` attribute, which tells Zig to use the C calling convention for this function.
Zig is not guaranteed to follow the calling convention we covered in the previous post,
so we need to be explicit with the annotation.

Finally, we define the `JITCompiler` struct, which will hold the state of our JIT compiler:
```zig
pub const JITCompiler = struct {
    allocator: std.mem.Allocator,
    /// The interpreter instance that we are compiling for.
    /// We need this to access the stack, blocks, and constants.
    interpreter: *const Interpreter,
    /// The compiled instructions go here before we
    /// place them in an `mmap`-ed buffer. 
    machine_code: std.ArrayList(u32), 
    
    /// Register names where the arguments are stored
    const ArgReg = struct {
        const stackAddr = .x0;
        const instructionsAddr = .x1;
        const stackIndexPtr = .x2;
        const instrIndexPtr = .x3;
        const currentBlockNumber = .x4;
        const constantsAddr = .x5;
    };
    
    /// Register names for local variables
    const VarReg = struct {
        const stackIndex = .x8;
        const instrIndex = .x12;
        const tempA = .x9;
        const tempB = .x10;
        const tempC = .x11;
    };
    
    pub fn init(
        allocator: std.mem.Allocator,
        interpreter: *Interpreter
    ) JITCompiler {
        return .{
            .allocator = allocator,
            .interpreter = interpreter,
            .machine_code = std.ArrayList(u32).init(allocator),
        };
    }

    pub fn deinit(self: *const JITCompiler) void {
        self.machine_code.deinit();
    }
};
```

Let's plop in some code to allocate and free the `mmap`ed buffers:

```zig
// src/jit.zig -> struct JITCompiler {
fn allocJitBuf(nbytes: usize) [*]u32 {
    const prot_flags = mman.PROT_WRITE | mman.PROT_EXEC;
    const mmap_flags = 
        mman.MAP_PRIVATE 
        | mman.MAP_ANONYMOUS 
        | mman.MAP_JIT;

    const buf: *anyopaque = mman.mmap(
        null, nbytes, prot_flags,  mmap_flags, -1, 0
    ) orelse unreachable; // WARN: ideally, return an error 

    if (buf == mman.MAP_FAILED) {
        std.debug.panic("mmap failed\n", .{});
    }

    return @ptrCast(@alignCast(buf));
}

fn deallocJitBuf(buf: [*]u32, size: usize) void {
    if (mman.munmap(buf, size) != 0) {
        std.debug.panic("munmap failed\n", .{});
    }
}
```

This is nearly identical to the `alloc_code_buf` and `dealloc_code_buf` functions in C from the previous post.


Let's also write a simple helper that takes a machine code instruction, and writes it to the `ArrayList`
that stores all instructions for the block we're compiling:
```zig
/// src/jit.zig -> struct JITCompiler {
fn emit(self: *JITCompiler, instr: u32) !void {
    try self.machine_code.append(instr);
}
```

To perform fewer memory accesses,
we will load the stack and instruction pointers into machine registers when the function is called,
modify them as needed during execution, and then write them back to memory before returning.

The `emitPrologue` and `emitEpilogue` functions will take care of loading and storing the registers respectively:

```zig
// int sp = *stack_ptr;
// int ip = *instr_ptr;
// where `sp` and `ip` are stored in 
// the registers `.x8` and `.x12` respectively
// (aliased as `VarReg.stackIndex` and `VarReg.instrIndex`)
fn emitPrelude(self: *JITCompiler) !void {
    // deref the stack pointer, store it in a local var
    try self.emit(
        Armv8a.ldrReg(VarReg.stackIndex, ArgReg.stackIndexPtr)
    );
    // deref the instruction pointer, store it in a local var
    try self.emit(
        Armv8a.ldrReg(VarReg.instrIndex, ArgReg.instrIndexPtr)
    );
}

/// Restore the stack and instruction pointers
fn emitEpilogue(self: *JITCompiler) !void {
    try self.emit(
        Armv8a.strReg(
            VarReg.stackIndex,
            ArgReg.stackIndexPtr,
            0,
        )
    );
    try self.emit(
        Armv8a.strReg(
            VarReg.instrIndex,
            ArgReg.instrIndexPtr,
            0,
        )
    );
}
```
 

We'll also want some helpers to generate machine code that performs
common operations like pushing and popping from the stack:
```zig
// src/jit.zig -> struct JITCompiler {

/// emit `sp = sp - 1`
/// Where `sp` is stored in the local variable `stackIndex`
fn emitPop(self: *JITCompiler) !void {
    try self.emit(Armv8a.subRegImm(
        VarReg.stackIndex,
        VarReg.stackIndex,
        1,
    ));
}

/// emit the `ret` instruction
fn emitReturn(self: *JITCompiler) !void {
    try self.emitEpilogue();
    try self.emit(Armv8a.ret);
}
```

In addition to these, we'll want four more helpers:

1. [emitPushReg](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/jit.zig#L242): Push the contents of machine register onto the stack.
2. [emitAdvanceIp](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/jit.zig#L261): Increment the instruction pointer (`VarReg.instrIndex`).
3. [readInstruction](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/jit.zig#L269): Load the next instruction to a machine register.
4. [readStackTop](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/jit.zig#L279): Load the topmost item in the stack to a machine register.

With these helpers in place, we can now write the `compileBlock` function that takes a block of instructions
and compiles them to a function:

```zig
pub fn compileBlock(self: *JITCompiler, block_number: usize, block: *const CodeBlock) !CompiledFunction {
    try self.emitPrelude();
    
    // for each instruction in the block,
    // increment the instruction pointer, and equivalent generate machine 
    var i: usize = 0;
    while (i < block.instructions.len) {
        const instruction = block.instructions[i];
        const op: Opcode = @enumFromInt(instruction);

        // ip += 1;
        try self.emit(Armv8a.addRegImm(
            VarReg.instrIndex,
            VarReg.instrIndex,
            1,
        ));

        i += 1;

        switch (op) {
            else => unreachable
        }
    }
    
    // *stack_ptr = sp;
    // *instr_ptr = ip;
    // return
    try self.emitReturn();
    return self.getCompiledFunction();
}
```

The `getCompiledFunction` called at the end is a helper that takes the machine code generated so far
in the `ArrayList` and copies it to a `mmap`ed buffer, then returns it:

```zig
// src/jit.zig -> struct JITCompiler {

/// Take all the machine code instructions emitted so far,
/// compile them into a function, then return the function.
fn getCompiledFunction(self: *JITCompiler) CompiledFunction {
    const num_instructions = self.machine_code.items.len;
    const bufsize = num_instructions;
    const buf = allocJitBuf(bufsize);

    pthread.pthread_jit_write_protect_np(0);
    @memcpy(buf, self.machine_code.items);
    pthread.pthread_jit_write_protect_np(1);

    const func: JitFunction = @ptrCast(buf);
    return CompiledFunction.init(func, buf, bufsize);
}
```

For the `loadConst` instruction, we will:

1. Store the next instruction in the register `.x9` (aliased as `VarReg.tempA`).
2. Load the constant at the address `stack[stakcAddr]` into the register `.x10` (aliased as `VarReg.tempB`).
3. Push the value in `.x10` onto the stack.

```diff
 switch (op) {
+    .load_var => {
+        // a = instructions[ip]; ip++;
+        try self.readInstruction(VarReg.tempA);
+        i += 1;
+
+        try self.emit(Armv8a.ldrRegScaled(
+            VarReg.tempB,
+            ArgReg.stackAddr,
+            VarReg.tempA,
+        )); // b = stack[a]
+        try self.emitPushReg(VarReg.tempB); // push(stack[a]);
+    },
     else => unreachable,
 }
```

All other opcodes are similar in complexity, except perhaps `jump`, which needs
special care to handle the case where the jump destination is the same as the current block:


```zig
// src/jit.zig -> struct JITCompiler -> fn compileBlock

.jump => {
    // load the destination block index into
    // .x9 (VarReg.tempA)
    try self.readInstruction(VarReg.tempA);
    const dst_block = block.instructions[i];
    i += 1;

    if (dst_block == block_number) {
        // we're jumping back the same block. reset ip to 0,
        try self.emit(Armv8a.movRegImm(VarReg.instrIndex, 0));
        const distance: i32 
            = @intCast(self.machine_code.items.len);
        try self.emit(Armv8a.branchIfEq(-distance));
    } else {
        // set the next block index (stored in .x5,
        // named .currentBlockNumber), to the destination 
        // block index (stored in .x9, named .tempA) and return
        try self.emit(
            Armv8a.strReg(
                VarReg.tempA,
                ArgReg.currentBlockNumber,
                0),
        );
        // ip = 0
        try self.emit(Armv8a.movRegImm(VarReg.instrIndex, 0));
        try self.emitReturn();
    }
},
```

Notice that we're using the [branchIfEq](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/jit.zig#L112)
helper to generate a `b.eq`  instruction that jumps back to the start of the block if the condition is true.
To do so, we must know the distance between the current instruction and the start of the block.
Fortunately, this is simply the number of machine instructions generated so far (as every ARM instruction is
is the same size).

In either case, we want to start from the first instruction in the destination block,
so we set the instruction pointer to 0. 

Once again, I'll leave a [link to the implementaton](https://github.com/srijan-paul/tinyjit/blob/1dabf1cb9bec88edcd7054bca5fe2c99294fa435/src/jit.zig#L304)
for all other opcodes, as they're not particularly interesting once you've seen the two examples above.

## Interpreter changes

Recall that previously, the `JUMP` opcode called this helper function:

```zig
// OLD jump helper (src/interpreter.zig):
fn jump(self: *Interpreter) !void {
    // block index is the next "instruction".
    const block_idx = self.nextOp();
    self.pc = 0; // start from first instr in the next block
    const dst_block = &self.blocks[block_idx];
    self.current_block = dst_block;
}
```

When JIT compilation enabled,  we want to try calling the pre-compiled function instead:

```zig
// src/interpreter.zig -> struct Interpreter

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

The `doJit` helper compiles a block of instructions to a function, caches it for future re-use,
and then calls it with `callJit`:


```zig
// src/interpreter.zig, struct Interpreter {

fn doJit(self: *Interpreter, block_index: usize) !void {
    const block = &self.blocks[block_index];
    const compiled = try self
        .jit_compiler
        .compileBlock(block_index, block);

    self.jit_blocks[block_index] = compiled;
    self.callJit(&compiled);
}
```

And `callJit` takes care of calling the compiled function, passing along the interpreter's state:

```zig
// src/interpreter.zig, struct Interpreter { ...

/// Call a JIT compiled function
fn callJit(
    self: *Interpreter, 
    compiled: *const jit.CompiledFunction,
) void {
    // inout parameter for JITted function
    var next_block_idx: usize = 0;
    compiled.func(
        (&self.stack).ptr,
        self.current_block.instructions.ptr,
        &self.stack_pos,
        &self.pc,
        &next_block_idx,
        self.current_block.constants.ptr,
    );

    self.current_block = &self.blocks[next_block_idx];
}
```

Finally, when destroying the interpreter, we need to free the JIT compiled functions:

```diff
 pub fn deinit(self: *const Interpreter) void {
     self.jit_compiler.deinit();
 
+    // unmap all the JIT functions
+    for (self.jit_blocks) |maybe_jit_block| {
+        if (maybe_jit_block) |*jit_block| {
+            jit_block.deinit();
+        }
+    }
 
     self.allocator.free(self.jit_blocks);
 }
```

## Closing thoughts

If you had trouble following parts of this article,
I'd recommend taking a look at the [full implementation on GitHub](https://github.com/srijan-paul/tinyjit),
and cross-referencing it with the explanations here.

Finally, if you'd like to explore more advanced topics in JIT compilation,
you can either extend the current interpreter with:

- More opcodes
- Some optimizations for both the stack machine and the JIT compiled code.
- Support more than one architecture with some kind of an interface.

Or, read through any of the following resources:

1. [The V8 blog](https://v8.dev/blog).
3. [A tour of inline caching](https://webkit.org/blog/10298/inline-caching-delete/) – from the Webkit blog.
2. [How the Cinder JIT inliner works](https://bernsteinbear.com/blog/cinder-jit-inliner/) – Max Bernstein.
4. [Source code for V8](https://github.com/v8/v8) – If you're familiar with C++, this is surprisingly readable.

If you have any other resources that belong here, feel free to write to me,
or edit this blog post directly on [the repository for this site](https://github.cm/srijan-paul/srijan-paul.github.io).
