
In the [last post](../gsoc/), 
I went over the Pallene project and explained some parts of it that concerned my GSoC project.
In this one, I'll briefly go over our approach to the problem, and some relevant details.

<div class="toc">

| Table of Contents               |
| ------------------------------- |
| [Get The Code](#get-code)       |
| [First Contact](#first-contact) |
| [Before GSoC](#before-gsoc)     |
| [The Idea](#idea)               |
| [Coding Period](#coding)        |
| [In Tomorrow's news](#tomorrow) |
| [Backmatter](#backmatter)       |

</div>


## Get The Code.

Let's get the obvious out of the way first.<br>
A list of all the PRs I made over the summer can be found on
[this](https://github.com/pallene-lang/pallene/pulls?q=is%3Apr+is%3Aclosed+author%3Asrijan-paul+created%3A%3E2021-04-27+merged%3A%3C2021-08-23) link.

## First contact. <a name="first-contact"></a>

(Flashback to how I came across Pallene, skip if not interested in misc. background stories).

I used Pallene for the first time around September (I think?) of 2020.
Around this time, I was making games with the [LÖVE](https://love2d.org/) game framework, which uses Lua for scripting.
I've made a [couple](https://github.com/srijan-paul/horror-game) - [of](https://github.com/srijan-paul/Bonkyon) - [games](https://github.com/srijan-paul/bullet_hell)
with it, but I realized one thing.
Every time the codebase started beefing up (4-6k LoC),
keeping track of different moving parts of a game started getting difficult. 

One small choice made in the entity management system caused wonky behavior
of physics code written a couple thousand lines down, 
scene transitions would mess up lighting effects,
field accesses on `nil` would show up every 5 minutes when prototyping new features, and the list goes on.

I realized that getting a static analyzer, or a statically typed language can probably help me manage my projects better.
So I started looking and found a [whole bunch of tools](https://srijan-paul.github.io/blog/gsoc/#luas_sister_languages) 
that could serve the purpose.
Unfortunately, dialects of Lua aren't as mature as say, JavaScript.
JS devs enjoy the benefits of Typescript, Babel, Purescript, ReasonML and the like.
The Lua equivalent of these tools aren't as mature as one would like them to be.
I tried using Teal, but ran into problems with it's VSCode tooling, which sometimes froze my entire computer.
Which is unfortunate, because it's a solid language.

I was too fond of LOVE to use a different framework like Heaps or SFML.
At this point, I saw two drastically different choices in front of me:

1. Write my own statically typed dialect of Lua.
2. Help improve upon existing open source tooling for the language.

Knowing myself, I would have taken the first route.
Fortunately however, this was around the time someone on discord had shown me this xkcd:

![XKCD](https://imgs.xkcd.com/comics/standards_2x.png)

So I decided against taking the blame for a 15th standard and started looking for tools that I could contribute to.
My first instinctive choice was to go with Teal or [TS2Lua](https://typescripttolua.github.io/).
However, both tools had their own problems:

1. **Teal**: I was excited to dig in - and I cloned the [repo]() -
only to see the whole codebase is a single file 6k+LoC ([1](#backmatter))<a name="1"></a>.
At the time, this was a deal breaker for me sadly. I couldn't possibly edit a file that huge without losing my mind.
It takes my editor and LSP an entire minute or two just to parse the file! Unfortunate.

2. **TS2Lua**: It was all roses in the beginning, 
	but I soon realized retrofitting a JS dialect to Lua isn't as pretty as it sounds. There exist weird issues like:
	1. Inconsistency of 0-indexed JS arrays vs 1-indexed Lua tables. I asked the developers and they wanted to keep things that way (Understandably so, there is no good solution for this that I can think of either).
	2. Operator overloading, which I deem a must-have for working with vectors is achieved via weird hacks, and doesn't type-check as neatly.
	3. Debugging the generated Lua source can sometimes be painful.

This was unfortunate.
Other than these caveats, TS2L's quality absolutely amazes me,
and Teal is the closest thing Lua has to it's own Typescript.

This made me look towards Pallene, a compiled language that makes some use of Lua's C-API.
It can optionally transpile to Lua and be used to author libraries for Lua!
The repo also got a good amount of activity, had a good installation guide and several open issues.

Unfortunately though, the language wasn't  completely ready for production use yet.
Meaning if I was going into it, I was comitting to it for a very long  term.
Since I was starting to dip my feet into type theory at the time anyway, this could have made for an interesting run.

## Before GSoC. <a name="before-gsoc"></a>

From fiddling around with Pallene in the past,
I knew there are some big enhancements that the project could benefit from (my wishlist, of sorts).
However, it would be a big undertaking to start with something sizeable for my first attempt at hacking on it.

It only took a day or two of surfing the internet to find out some 
interesting [papers](http://www.inf.puc-rio.br/~hgualandi/papers/Gualandi-2018-SBLP.pdf) about Pallene, 
and a [past GSoC project](https://summerofcode.withgoogle.com/archive/2020/projects/5667973756354560/).
I had heard of Google Summer of Code before, but I had no idea how the event  worked, or what it was exactly.

Usually  when an organization frequents to GSoC, it means the project is friendly open to outsiders.
That's obviously good news!

And so I started off with [issue 225](https://github.com/pallene-lang/pallene/issues/225), which basically said: 

> *"When there is an internal error in the compiler, we get a not-so-tasteful 'impossible' crash message.* 
> *It'd be nice to know more about the object that caused the crash.*"


A perfect issue for several reasons-
minor enough to approach for a first-time contributor and touches several parts of the codebase,
so I can get familiar with it in the process. And so - with the help of the maintainer -  it led me to spawn 
[PR 329](https://github.com/pallene-lang/pallene/pull/329).
Of course, my code wasn't very courteous to the rest of the codebase,
so it took some rounds of review before it could get merged ([2](#backmatter)) <a name="2"></a>.

There were a total of 4 PRs merged before GSoC. All of them can be viewed [here](https://github.com/pallene-lang/pallene/pulls?q=is%3Apr+is%3Aclosed+author%3Asrijan-paul+created%3A%3C2021-04-27+).
Moving on to GSoC...

## The Idea. <a name="idea"></a>

The proposal was to have higher order functions in Pallene.
Lua has support for lexical capturing and closures,
so it would make sense for Pallene to base it's semantics off of those set by Lua.
It all started with [issue 174](https://github.com/pallene-lang/pallene/issues/174), which discusses this very feature.

I briefly went over higher order functions and Pallene's calling convention in part 1 of the series -
so I'll be providing an eagle's eye view of the experience.
Of course, you don't necessarily have to read the first one to understand everything here.

You could follow the conversation in the aforementioned issue,
but being the ever so rightful and courteous gentleman I am, here is a summary:
* Closures require a blob of code encoding the logic + a bag of data representing the captured variables/upvalues.
* There are two widely popular ways to represent closures in memory: Flat closures and Linked closures.
* The representation of closures in memory can have implications on garbage collection.
* For optimality, it might be a good idea to treat immutable captured vars separately. (immutable = captured variables that are referenced after declaration but not updated).
* We could pass the read-only upvalues  as extra parameters to their C-entry points (discussed in last post).
* For the mutable upvalues, we could put them on the heap, and use a "box" to refer to to them.

For example, consider this simple Pallene snippet:

```lua
-- Returns a closure that increments a number by a fixed amount every call.
function make_counter(x: integer, dx: integer): (integer) -> integer
	return function ()
		x = x + dx
		return x
	end
end
```

According to our plan, it should turn into something like this (oversimplified for brevity): 
```c
// NOTE: I've intentionally left out a lot of details for simplicity's
// sake. You'll see descriptive function names that don't exist. Eg-
// `as_int` -> converts a dynamic lua value to a C integer.
// `push` -> pushes a lua value to the lua stack.
// `get_func` -> returns the currently executing lua function.


// An upvalue box that surrounds an integer.
typedef struct { int x; } Box;
// takes an integer and makes a box surrounding it.
Box* make_Box(int* x);

// C entry point for the closure returned by `make_counter`.
static int closure_0_c(Box* upvalue_x, int upvalue_dx) {
	upvalue_x->x = upvalue->x + upvalue_dx;
	return upvalue->x;
}

// Lua entry point for the closure returned by `make_counter`.
static int closure_0_lua(lua_State* L) {
	CClosure* current_func = get_func(L);
	// 1. Grab the upvalues
	Box* x  = as_box(current_func->upvalue[0]);
	int  dx = as_int(current_func->upvalue[1]);

	// 2. Pass them as additional parameters to the
	// C entry point.
	int ret_val = closure_0_c(x, dx);
	
	// 3. Push the result computed by the C entry point
	// over the stack.
	push(L, lua_value(ret_val));
	
	// we are returning one value.
	return 1;
}


// Lua entry point for `make_counter`. In real compiled code, it would
// defer execution to it's own C entry point, but to keep this snippet short, 
// I've omitted it.
static int make_counter(lua_State* L) {
	// 1. get the arguments and cast them down to C
	// data types.
	int unboxed_x  = as_int(get_first_arg());
	int dx         = as_int(get_second_arg());
	
	// 2. Make a 'box' around 'x'.
	Box* x = make_Box(unboxed_x);
	
	// 3. create a closure that uses 'closure_0_lua' as the 
	// "code" part, and has two captured variables.
	CClosure* closure = lua_new_cclosure(L, closure_0_lua);
	closure->upvalue[0] = lua_value(x); // first captured var
	closure->upvalue[1] = lua_value(dx); // second captured var
	
	// 4. Push the return value onto the stack (The closure).
	push(as_lua_integer(lua_value(closure));
	
	// 5. We are returning one value to the lua runtime via
	// the previous call to "push".
	return 1;
}
```

Here is a very neat diagram from [this](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.87.7741) paper that my mentor, Hugo posted in the issue:

![code-for-closure](https://user-images.githubusercontent.com/1452192/108609751-d6ca0680-73ae-11eb-83a6-568aa5e6fe14.png)

And if you find closures interesting, [this](https://sci-hub.se/10.1145/2661103.2661106) was a great read for me personally :D

Ooookay, so what does GSoC look like for a student?

1. **Write a proposal:** Once the organization list for GSoC was announced - and LabLua was one of the organizations this year - I wrote the skeleton of a proposal summarizing the ideas Hugo and me  had discussed in the issue. Once the draft started to take shape - I shared a google doc with him  so  that he could edit and suggest changes. In about 2-3 weeks, we managed to put together a proposal ready for submission.
2. **Cross fingers and wait:** Now we wait for the organization and Google to review the proposal. If everything goes well - I should get a green signal via mail. And everything did go well, so I did :)
3. **Community bonding:** The actual "summer of code" is padded with a period where we get our feet wet with the organizations, our mentors and the codebase.
4. **Start coding:** And this is where the interesting work finally starts.

In addition to the above, it helps to have a clear plan and some know-hows about the project you want to
work on. Getting in touch with the organization prior to GSoC and getting familiar can be a good step 0 :).

## Coding Period. <a name="coding"></a>

(Note: I use the terms "upvalue" and "captured variable" interchangeably).

**Support closures on the front-end.** 
This meant parsing closures from source text to an AST- and then type checking the AST.

This was less work than we initially anticipated, and ended up saving us a lot of time!
Pallene was already capable of parsing top-level function statements.
So a slight tweaking of the grammar and the parser's code was all it took.
The type checker didn't require any changes!

**Implement closures that don't  capture any variables** (yes, just lambda functions).
This was my first time tinkering with the Pallene IR. I had touched on it briefly in an attempt to introduce `ipairs` loops, but not in detail. We ended up having to add a new IR instruction called `NewClosure`, that -- when compiled -- would create a new closure object by calling a function from the Lua C-API, appropriately named [luaF_newCclosure](https://www.lua.org/source/5.1/lfunc.c.html#luaF_newCClosure).

**Implement read-only captured upvalues.** 
This took some more changes to the IR. Here, we introduced yet another instruction called `Upvalue <id>` -
where `<id>` is a number assigned by the compiler to an upvalue to identify it uniquely.
Using this IR- nested closures could now refer to non-local variables
(which - as described above - are just function parameters once compiled down to C).

**Implement mutable upvalues.**
For this, we had to introduce an entirely new compiler pass that fit between the type-checking pass and the IR generation pass.
This pass would inspect the AST and perform four main tasks:

1. Separate captured variables from regular  variables.
2. Separate mutable captured variables from read-only captured variables.
3. Encode record types needed for the upvalue boxes in the AST.
4. Modify the AST to represent captured variables as boxed values instead of  regular values. ([3](#backmatter)) <a name="3"></a>

After this compiler pass, the IR generator could easily transform an AST into IR -
and the code generator required fairly minimal changes too.

**First evaluation.** We were ahead of schedule! The first evaluation went well.

**Uh-oh!** Now my college decided to conduct exams at a weird time of the year, so development had to take a break for a while. Thanks to the progress made earlier, I shift focus and take some time off from coding.

**Deciding the milestones for second evaluation.**
The most beefy changes had been made at this point,
so we could use the remaining weeks to polish the changes introduced to the codebase.
There are 3 main facets to cover:

1. Optimizing closures by merging multiple upvalue boxes into one.
2. Fixing bugs (if any).
3. Getting rid of global variables altogether and representing them as captured variables.

For **1**, we had multiple meetings to discuss strategies and tradeoffs.
We realized that this might require several changes to the compiler and some not very trivial lifetime analysis of symbols.
There were several questions to answer such as:

- What criteria would we use to determine mergeability?
- What kind of performance gains are we looking at?
- Is the performance gain worth the investment?
- How likely is it for optimize-able code to occur in real codebases?
- From the closures that *can* be optimized, how many are called often enough to be performance critical?
- How do the benefits of merging boxes scale with regards to:
	- Number of closures made.
    - The number of variables captured.

At the time of writing this blog post, some of these questions are still unanswered.
However, we have some numbers to chew on in the mean time. We used two ways to collect some benchmark data:

-  Find popular Lua repositories on Github and run their source code through a hacky script to find out the number of upvalues referenced by closures in these codebases.
Once we have that data- we eyeball the codebase to check how often we'd be able to make box-merge optimizations.

-  Use the Pallene compiler to generate C files from Pallene source code that uses higher order functions.
   Then "optimize" the generated C code by hand. Once done, run both the compiler generated and hand-edited code through a benchmark tool like Hyperfine, and compare the results. 

Some of the benchmark data that we gathered can be seen on the gist [here](https://gist.github.com/srijan-paul/06d640db0b08086757687dbebffb7f1f) and this issue [here](https://github.com/pallene-lang/pallene/issues/426).
From what we know right now, there exist places where it's possible to gain a speed up of 110-120% 

Coming to **2**, we found 2-3 bugs of varying severities.
For example, Lua uses byte sized unsigned integers to index upvalues (and local variables).
This means we must restrict the number of upvalues a function can have in pallene.
We set this limit to 200.

As for the last bit, Pallene used to have a slightly odd representation for global variables.
There used to be big table named `G` which would hold all the global variables and constant values. 
The `G` would then be passed around from function to function so that the functions could access the globals they need.
Now that we have upvalues, functions could simply capture the upvalues they need upon initialization.
So we decided to remove the `G` table and instead use a new table called `K` which would only contain constant values like strings.

In summary, we made some the following changes to Pallene's calling convention:
- Local functions are treated uniformly regardless of the style of declaration.
  (`local f = function() end` vs `local function f() end`).
  The only difference being function statements cannot be re-assigned.
- Global varibales are treated as regular upvalues that can be captured by toplevel functions that need them (similar to Lua).
- Reduce the number of instructions in the IR.
- Remove the global-var table `G`, and replace it with a constant-pool table `K`, containing constants like strings.

You can view the PRs made during GSoC by clicking [here](https://github.com/pallene-lang/pallene/pulls?q=is%3Apr+is%3Aclosed+author%3Asrijan-paul+created%3A%3E2021-04-27+merged%3A%3C2021-08-23).

## In Tomorrow's news <a name="tomorrow"></a>
Higher order functions are a much welcomed addition to Pallene. But there are some long term goals that I'd personally like the language to achieve. I've been trying to get it to a point where I can use it to write libraries for a WIP game framework of mine called [Wex](https://github.com/cpp-gamedev/wex/tree/dev) ([4](#backmatter)).<a name="4"></a>

1. **Better Embeddability**.  While it's possible to embed Pallene into C applications, the experience isn't exactly the easiest.  I want to see what we can do about the making the language more easily accessible into real applications. Note that all C applications that embed Pallene are essentially embedding a Lua interpreter. The Pallene compiler itself is called independently - which generates shared object files to be linked dynamically.
2. **Maps**. Passing structured data from a C script to a Pallene script is something that I believe should be feasible via tables. The problem here is Pallene represents table-like data as records, which compile down to C-structs. Whereas, Lua tables are basically separate chained hash maps.
3. **Union types**. This is an idea that I've thought about the least, however I believe it *should* be possible. If not union types, I'd like to have nilable ones! (`int?` vs `int`). In fact, nilable types could probably be implemented as unions (`T | nil`).
4. **Editor support**. For some colors and autocomplete. Hacking together an extension for VSCode shouldn't be too tough, but consistently covering multiple editors is painstaking.
5. **Modules**. Fortunately, this is already being considered and worked on! But I have no clue how they work :D

It is possible that by the time you're reading this, some stuff has already been marked off the list!

## On that note... <a name="end-note"></a>

Pallene is an interesting project that is different from the standard ways of evaluating programs (interpreters, JITs and AoT compilers). If you managed to stick around so far, then it's likely that it is of interest to you. 

I hope I could give a very hand-wavy idea of the workflow. I should have said it earlier - but if some of what's explained above comes off as confusing and complicated, it's really not. I've tried to compress *a lot* of information into two blog posts, so this result is natural. I highly encourage you to check it out! Who knows, you may end up becoming the next GSoC student - or long time contributor? Adios.

## Backmatter. <a name="backmatter"></a>

1. While talking with my mentor, I later learned there were good reasons for this.
  The author wanted to bootstrap the compiler soon after it was created, 
  and so to avoid fiddling with imports he decided have a single file compiler. As an added bonus,
  installing teal is incredibly simple compared to other transpilers/tools. [**(return ⮭)**](#1)

2. I wish I could say it got better with time,
  but as you'll notice- some of my PRs have as many as [71](https://github.com/pallene-lang/pallene/pull/334) comments on them.
  This is true even for the PR I made yesterday. But that's good!
  Should you ever try to tackle a *chonky* codebase, my mess-ups are evidence that the reviewers will be nice and offer help when needed.
  So don't be afraid to take a leap of faith! [**(return ⮭)**](#2)

3. A "boxed value" is simply a value inside a record. For instance:
```lua
local x: integer = 1 -- regular value

-- a "box" type can be thought of as a struct
-- surrounding a single, regular data type.
record Box
	value: integer
end

local y: Box = { value = 10 } -- "boxed value"
```
Here, `y` is a "boxed" integer, whereas x is a regular integer variable. Of course, this is just the terminology I choose to use. I don't know if there is an official name. [**(return ⮭)**](#3)

4.  Another one of my goals is to be able to script it with [Vyse]().
More on this language in a future post! [**(return ⮭)**](#4)
