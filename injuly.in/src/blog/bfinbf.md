---
template: "post"
tags: ["brainfuck", "programming-languages" ]
title: "Compiling to Brainf#ck - Meep."
date: "2020-11-22"
meta: "Writing a brainfuck interpreter in brainfuck, using a language that compiles down to brainfuck"
is_blog_post: true
---
## Alternate Title: Writing a brainfuck interpreter in brainfuck, the stupid way.

Brainfuck is one of the most popular esoteric programming languages.
Esoteric languages are "for fun only" and not for serious use (but that didn't stop a lot of people).
Most esoteric programming languages are simple, have a very limited instruction-set/syntax and most
importantly, they all are very, very different from your conventional programming language.

In this post, I attempt to write a programming language that compiles to brainfuck, and then write an
interepreter for brainfuck in our new language, compile it and have a brainfuck interpreter in brainfuck!
(Not the brightest means to the end, I know. But it's a good way of testing our implementation).

## The What - Brainfuck

If you're already familiar with brainfuck, you'll probably be better off skipping over
this part.

The Brainfuck programming language's syntax contains a total of 8 characters (yes, 8).
Brainfuck models an imaginary machine, with an infinitely long memory tape.

<img src="\assets\img\brainfuck\1.png"/>

Each cell in this tape is a byte long and initially set to zero.
We also have a data pointer, which I drew as an arrow below the tape. It points to the current data cell under inspection.
The programmer moves this data pointer around in the tape, incrementing and decrementing the values in the cells.

The 8 instructions we talked about earlier are the following:

`+` - increment the value at the current data pointer by one.

`-` - decrement the value at the current data pointer by one.

`>` - move the data pointer one cell to the right.

`<` - move the data pointer one cell to the left.

`.` - print the value at the current data pointer as an ascii character.

`,` - take one byte of input from the user and put it in the current cell.

`[` - if the value at the current data pointer is 0, skip the corresponding `]`, else continue from the next instruction.

`]` - if the value at the current data pointer is non-zero, jump back to the correspondong `[`, else continue on to the next instruction

And that's all. This might seem like a very limited instruction set, and it is, but brainfuck is a turing-complete programming language.
Meaning, if a solution exists to a computational problem, It is theoretically possible find it with brainfuck. To know more about brainfuck,
I'd advise you to skim over [this](https://esolangs.org/wiki/Brainfuck) page.

Creating an interpreter for brainfuck is trivial, and you can finish one in a couple of hours. Brainfuck is easy to compile, but hard to _compile to_. And so I spend a weekend challenging myself to attempt the latter.

## The Why

Because we can, really. Brainfuck **is** turing complete, so it _could_ serve as a good compilation target.
One weekend I found myself going through some brainfuck-derived languages, all of which added some spice to the language,
like having a 2D grid serve as a memory instead of the tape, and what not. Now programming in brainfuck is not my thing at all, I don't find
the idea of solving problems with a tape of unsigned bytes fun, nor would I be good at it. However, it could work as an interesting thought experiment to see if one can compile a programming language down to such a limited instruction set, and to top it all off,
We could write a brainfuck interpreter in the language we invent, because why not?

Now I had exams coming up, and a couple of other things to work on. So I wanted this to be done _fast_, meaning no kanban boards,
no to-dos and wishlists. Just a simple, single pass compiler cobbled up in a weekend as a little prototype.

## The How - Meep

Here is the weekend-long implementation plan I came up with:

<ol>
<li>Single pass, no AST involved, compile tokens to IR and IR to Brainfuck.</li> 
<li>Emulate a stack on brainfuck's memory tape, helps model a stack based VM-like architechture.</li> 
<li>Write a brainfuck interpreter in our language to test the implementation. </li> 
<li>Profit</li> 
</ol>

Now this is something I wanted to be done with in about 2-3 days at the longest, so I also put down some design goals for the language
that we're about to build.

- **As normal looking as possible** - It should look familiar to existing languages.
- **Minimal feature set** - It should support arrays, numbers, strings, if-else, looping. Just about what we need to
  write a brainfuck interpreter, it's a prototype after all.
- **Single pass** - The compiler _must_ be single pass. That poses some difficulties, and some restrictions. But we'll get over those.

As I came to know later, such projects already exist. If you're looking for similar stuff for whatever reason, the
esolangs wiki is a good place to look :)

## A Tour of meep

Like I mentioned earlier, it has byte sized numbers, arrays, strings, if-elses and while loops.
To an experienced brainfuck programmer, all this is probably very easy to get by.

```js
var message = "Hello!";
print(message); // Hello!
set message[2] = 'y';
print(message); // Heylo!

print('0' + len(message)) // 6

var char = 'a';
print(char);
set char = 'b';
print(char);

var bool = input == 'a';

if bool
    print "you entered the letter 'a'";
else
    print "idk what you entered";

var i = 0;
while i < 10 {
    print 'a' + i;
    set i = i + 1;
}

```

No, it doesn't have functions, `print` is actually a statement, and `len` is a language feature.
Though I might write another version of this with an additional pass, _with_ functions and a simulated heap some day.
This particular language still remains rushed and largely prototypical.

## How does it work?

The path that a user written program takes in meep goes,

source code -> **Tokenizer** -> tokens -> **IRCompiler** -> IR -> **Generator** -> Brainfuck

The IR stands for intermediate representation, it's kind of like assembly but instead of targeting a
processor, it targets a compiler back end. It's dumbed down in complexity in comparison to an AST, and is more
lightweight. Usually, real programming language implementations first construct a syntax tree and then break it
down into IR code (Exceptions exist, like Lua). For a small language though, we should be fine.

Now here are all the IR instructions at a glance:

```c
  POP PUSH
  ADD SUB EQUALS
  SET_VAR GET_VAR
  FALSE_ TRUE_
  LOAD_BYTE PRINT START_IF CLOSE_IF_BODY
  END_IF START_ELSE END_ELSE START_LOOP END_LOOP
  POPN CMP_LESS CMP_GREATER LOAD_STRING MAKE_BUS
  INDEX_VAR NOT MAKE_SIZED_BUS SET_AT_INDEX INPUT
  LEN
```

Let's walk through a little program here and see the transformations it goes through :

```js
var bool = false;
if bool print 'a' else print 'b';
```

first it tokenizes to:

```js
[
  'var',
  'bool',
  '=',
  'false',
  ';',
  'if',
  'bool',
  'print',
  "'a'",
  'else',
  'print',
  "'b'",
  ';',
];
```

The tokens are then picked up by the IRCompiler which does the parsing and spits out
the following IR:

```c
// First, we push a `false` onto the stack,
// this is the local variable `'bool'`.
// stack state: [0]
FALSE_
// Then we read from the variable, meaning we take the value at index 0
// in the stack and push it again to the top [0, 0]
GET_VAR 0
// marks the beginning of an if-block,
// the part after this is executed if
// the value on top of the stack is
// true (non-zero)
START_IF
// push an 'a' onto the stack
LOAD_BYTE 97
// print the value at the top of the
// stack and pop it.
PRINT
CLOSE_IF_BODY
// the else block
// excutes of the if block didn't
// I'll get to how I did this down later.
START_ELSE
// push 'b'
LOAD_BYTE 98
// print value at
// stack top
PRINT
END_ELSE
END_IF
```

And that finally turns into a bloated brainfuck file:

```js
>><[>+>+<<-]>>[<<+>>-]<>+<[>>+++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++.[-]<-<[-]]>[>++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.[
-]<[-]][-]<[-]<
```

The idea is to get the brainfuck memory tape to act like a stack that we can push to and pop from,
then it's possible to get it to act like a stack based VM.

I'll explain some of the challenges I came across and ones that you may have to fight
if you ever choose to try something similar for whatever reason.

## Under the hood.

Lets get a few things established before moving on to the implementation.

1. The data pointer is used to point to the top of the stack, it is what you would call a "stack pointer".
   It points to the top most value in the stack.

2. All the cells are initially zeroed out, so we have an empty stack which is about 30k bytes long in most
   implementations.

3. All statemnents must have a stack-effect of 0, meaning they shouldn't leave any unpopped values on the
   stack after they finish execution.

4. All expressions must have a stack effect of +1, meaning they must leave the stack one value (not 1 byte or cell)
   larger than it was before the expression was evaluated.

That's really just the basics of how a stack based VM works.

### Variables.

This is fairly easy, just push a value onto the stack (write `">" * n` where n is the value of the variable).
All statements _must_ always leave the stack as big as it was before the statement was evaluated.
This allows us to just use the `<` and `>` instructions to look up variable values. The number of variables
is always known at compile time, as is the order they appear in, so we can substitute every variable look-up
as just copying byte from a certain depth in the stack to the top.

```js
var c = 'a';      // 1
print(c + 1);     // 2
set c = 'b';      // 3
print(c + 1 - 2); // 4
```

Line 1 - First, `'a'` is pushed to the stack (`[97]`). Since this value is never popped, it's a byte sized variable.

Line 2 - Then, we get the variable at index `0` and push it to the top (`[97, 97]`).
Push a `1` to the top of the stack (`[97, 97, 1]`), then pop the top two values, add them and push the result back (`[97, 98]`).
Now we print the value at the top of the stack, and pop it right after (`[97]`)

See how we executed an entire statement, and yet the stack is exactly as long as it was on line 1?
Try going through the next 2 statements and you'll see the effect is exactly the same. This is true for
all statements except variable declarations, which have a stack effect of +1, as they push one value onto it that just
stays there until the program terminates.

So how exactly do we push values on top of the stack, when the stack is really the brainfuck memory tape?
Simple, we use `>`, to move one step forward and then `+` to increment from 0 to the value of the variable.
So pushing a `5` is as simple as `">+++++"`.

Popping a byte however, can't just be done with `"<"` because when a new stack slot is occupied, it's assumed to be 0.
So we zero out all values before popping off. Henceforth, our instruction for popping a byte becomes `"[-]<"`.
Basically, decrement the value at stack pointer until it is 0, then move the stack pointer on step back.

### Strings and arrays.

Strings and arrays behave mostly identically in Meep, they're both just streams of bytes.
I didn't bother adding 2D arrays, though they'd be trivial given most of the scaffolding needed is already
present. The trickiest part was to support indexing arrays.

Arrays can be indexed with values that aren't known at compile time.

```js
var array = {'a', 'b', 'c'};
var index = input();
if index > len(array) {
  print "Index out of bounds";
} else {
  print array[index];
}
```

I googled around a bit to find what brainfuck programmers usually do, one solution involved
having a flag next to every single array element, doubling the size of arrays, it was also tough
to work with. Another one I found was to pad an array with 2 zeros in the beginning and one at the end,
but this required that no elements in the array be 0, which is a bit inconvinient.

I however had the advantage of getting the memory tape to follow stack semantics, so I came up with a solution
which works pretty well at the moment. Let's first see how we would get an element from an array.
The information we have at compile time is:

- The index we need to visit is currently present at the top of the stack.
- We know the size of the stack.
- We know the index of the array and it's depth from the stack top.

What we need to do is:

- Pop the index from the stack's top, then fetch the value at that index, and push
  the value to the stack's top.

So if `array` is `{1, 2, 3, 4}` at depth **D**, and the index **i** is `2`, the memory tape looks like this:

<img src="\assets\img\brainfuck\2.png"/>

The idea is:

1. Move the pointer to the right by one cell and leave behind a `0` to mark the current stack top.
2. Move the pointer to the right again by **D** cells, leaving behind a trail of `1`s in the process.
3. Now, if we copy a value which is at a depth of **D** cells from the current position, we have succesfully copied
   the value at index **i** to the current stack pointer.
4. Keep moving the value back as long as we see a `1` in the cell to the left.
5. Once a `0` is seen on the left, shift the value to that cell.
6. Done.

Here is a very artistic potrayal of the steps to explain it better:

<img src="\assets\img\brainfuck\3.png"/>

Modifying the value at an array's index is similar, using the index move the value **i** cells to the
right, then copy the value at the stack's top to the value at depth **D** from here. Move the stack pointer
back just like we did before and we're done.

The actual code for moving a value **v** to the right by **v** steps after planting a marker is:

```js
[-  >+<]> // place a marker 0 by moving index one cell to the right.
[    // until the index is 0
  [- >+<]+> - // move it one step to the right, place a 1 at
              // it's previous location, and decrement it's value by 1.
]
```

After copying the value, moving it back to the left is similary achieved with `<[->[-<+>]<<]>[-<+>]<`.

### If statements.

If statements behave as one would expect. We first push a condition onto the stack.
If the condition is non-zero, we execute the if body and skip over the else block, if not
then we skip over the consequent block and execute the else. At first glance, it would appear
that a conditional jump can be easily implemented in brainfuck using `[]`, however we also
have to skip over the else-block if the condition does turn out to be false (zero).

The solution I came up with, again invovled leveraging the stack semantics of our bloated runtime code a
slight bit. This time around, we use a flag placed right after the condition's value on the memory tape,
the flag tells us whether or not to execute the `else` block.

1. Push a 1 bit flag right next to the condition on the memory tape, this indicates whether
   the else block will be executed. Intially, set it's value to `1`.
2. Move the data pointer one cell left and go back to the condition.
3. Use `[ if_block ]` to conditionally evaluate the if-block, inside the `[]`,
   place some code that flips the flag to `0` to indicate the if block has been executed and the
   `else` block no longer needs to run.
4. After exiting the if-block the data pointer returns back to the flag automatically, since the
   body of an if-statement is a block statement, which has a stack effect of 0.
5. Use `[ else_block ]` again to conditionally evaluate the if-block depending on the flag.
6. Pop both the flag and the condition off the stack.

So right after pushing the condition, this is what the memory tape looks like:

<img src="\assets\img\brainfuck\4.png"/>

And the generated brainfuck code looks somewhat like this:

```py
#code to put the condition on stack goes here
> + <  #put a 1 bit flag on the stack and go back to the condition
[  #if the condition is true
  > - # set the flag to false, no need to run the else block.
      # then-block's body goes here
  <   # move the pointer back to the condition.
  [-] # zero the condition so we can exit.
] > # move the pointer to the flag
[     # if the flag is true
      # else-block's code goes here
    - # zero out the flag now so we can exit.
] # exit else-block
[-]< # pop the flag off.
[-]< # pop the condition off.
```

It's kind of a pain because there is no notion of _"skip n instructions"_ or _"jump to instruction n"_
in brainfuck, and we always have to act relative to the data pointer's current location. This one took
me a couple of hours.

### While loops

This was the most straight forward and easy control flow to implement.
Brainfuck's native `[]` does exactly what we want a while loop to do, the
only difference is we want to evaluate the condition every time we are done
executing the body. Pretty simple:

```py
# condition expression
[
  [-]< # pop the condition
  # code to loop over.
  # the same condition expression again.
]

```

If you're wondering how it's parsed and compiled, honestly my approach
was a pretty stupid and "hacky" one, but it get's the job done so what gives.
Basically, I have a class called `IRCompiler`
which has a data member `this.ir`, an array of IR codes. `this.emit(o)`, pushes
`o` to the IR array.

```js
class IRCompiler {
  constructor() {
    this.ir = [];
  }

  // other code

  whileStmt() {
    // store the length of the IR before
    // the condition is compiled
    let before = this.ir.length - 1;
    this.expression(); // compile condition.
    // length after the condition is compiled.
    let after = this.ir.length;
    this.emit(IR.start_loop);
    this.statement();
    // emit the bytes for the condition again
    // since it needs to be re-evaluated.
    // To do this, we take the code for the
    // condition, and re-insert it again.
    for (let i = before + 1; i < after; i++) {
      this.emit(this.ir[i]);
    }
    this.emit(IR.end_loop);
  }

  // other code
}
```

That's about it for all the challenging parts.
This section would have been a **lot** longer had someone tried this as a more featured language.
Meep still leaves many features to be desired:

- Support for numbers larger than 1 byte.
- N-d arrays (this is an easy one to implement).
- Functions, more flow control.
- Heap allocation and passing/copying strings and arrays by reference.

But this is still a fine little experiment to make use of brainfuck's turing completeness,
and show that it can be used as a compilation target. One could even make use of meep
on of those crazy computers that run brainfuck natively ! Which reminds me...

## BFinBF

There are many bootstrapping brainfuck interpreters, one of the reasons I made
meep was to write one of those myself. So here it is:

```js
var memory = bus 250; // the memory tape
var mptr = 0;         // data pointer
var code = bus 250;   // the user's code

var i = 0; // current position in the source code.

// this is a very unusual and wonky way of getting
// user input but since we can only get one byte of input
// at a time so...
while i != len(code) {
    set code[i] = input;
    set i = i + 1;
    if (code[i] == 'x') set i = len(code);
}


set i = 0; // reset position
var c = 0; // current code character.
var bctr = 0;
while i != len(code) {
    set c = code[i]

    if      c == '+' set memory[mptr] = memory[mptr] + 1;
    else if c == '.' print memory[mptr];
    else if c == '-' set memory[mptr] = memory[mptr] - 1;
    else if c == '<' set mptr = mptr - 1;
    else if c == '>' set mptr = mptr + 1;
    else if c == ',' set memory[mptr] = input;
    else if c == '[' { // skip to corresponding ']' if current cell is 0
        if memory[mptr] == 0 {
            set i = i + 1;
            while ((bctr != 0) + (code[i] != ']')) != 0 {
                if      code[i] == '[' set bctr = bctr - 1;
                else if code[i] == ']' set bctr = bctr + 1;
                set i = i + 1;
            }
        }
    // jump back to the opening '[' if current cell is nonzero
    } else if c == ']' {
        if (memory[mptr] != 0) {
            set i = i - 1;
            while ((bctr != 0) + (code[i] != '[')) != 0 {
                if      code[i] == ']' set bctr = bctr + 1;
                else if code[i] == '[' set bctr = bctr - 1;
                set i = i - 1;
            }
            set i = i - 1;
        }
    }
    set i = i + 1;
}
```

Hmm, It honestly doesn't look too bad!

Until... you compile it and it generates a
[850 line file](https://github.com/srijan-paul/BFinBF/blob/main/bf_interpreter.bf) with thousands
of characters.For reference, the [shortest known BF interpreter](https://esolangs.org/wiki/Dbfi) is slightly
over 400 characters.
Time for some hindsight:

- Is Meep efficient? **No**. It doesn't optimize, single pass.
- Does the brainfuck interpreter work? **Yes**.
- Can it interpret itself? **No**. That's a bit sad.
- Was it worth it? **Absolutely**. I still think this is cool :)

## Acknowledgement.

A very odd placement for a something you'd expect in the beginning. Regardless,
while tinkering on this monstrosity, I found out that I was by no means the first
one to try compiling to brainfuck. On the esolangs wiki, I happened to come across
[C2BF](https://github.com/benjojo/c2bf), a C compiler targeting brainfuck. It's
a lot more capable than Meep is at the moment.
There are also some projects attempting to compile asm
to brainfuck, and one even compiles Python, but both only support a very small subset
of the languages and are a lot more primitive.

## Come take a look !

Interested in the code? As usual, it lives [here](https://github.com/srijan-paul/meep) on github.
Having been botched together in a weekend, the code isn't the prettiest but you'll most likely find
your way around since I commented a lot of stuff for myself.

Adios.
