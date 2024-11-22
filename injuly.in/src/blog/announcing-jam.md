---
template: "post"
tags: ["jam", "personal" ]
title: "Why am I writing a JavaScript toolchain in Zig?"
date: "2024-11-21"
meta: "Announcing the existence of Jam – An robust JS toolchain in Zig"
is_blog_post: true
---

<div class="note" style="float: left; width: 100%; margin-bottom: 20px;">
     <img
         src="/assets/img/oshiro-serious.png" width="100px"
         class="self-image"
         style="float: left;"
     />
     <div style="padding-top: 3px; line-height: 1.5;">
        <b style="font-size: 17px;">injuly:</b> I understand not everyone will be psyched about
        JavaScript.
        You can feel free to skip the prose
        and try the <a href="https://injuly.in/js-playground">online playground </a>
        for the parser (compiled to WASM),
        or browse the code on <a href="https://codeberg.org/injuly/jam">Codeberg</a>
        and <a href="https://github.com/srijan-paul/jam">GitHub</a>.
        All feedback is welcome!
     </div>
</div>

Few things keep me up at night more than the 
untapped potential in current developer tooling.
The research exists; all that's needed is focused effort to improve the tools we rely on every day.
For years, these thoughts have simmered in my mind, and
now they've led me to start building something tangible:
a new JS toolchain called Jam.
It includes a static analyzer, formatter, and code optimizer.

The JavaScript ecosystem—bundlers, formatters, linters,
and even TypeScript—has significant room for improvement.
We’ve either accepted some limitations as the standard,
or aren't cognizant to what better ecosystems are like.
Fortunately, these hurdles are nowhere near the frontiers of Computer Science,
meaning that I, the grug brained programmer, get to take a stab at it.

I'm thankful that people more competent than I have invested their time into the ecosystem.
Take Evan Wallace, for example, who put JavaScript build times on the chopping block 
with his amazing work on esbuild.
Compared to tools like webpack, esbuild finishes before I can lift my finger off the `Enter` key,
and has become my default choice for building JavaScript, JSX, TypeScript, [even PureScript](/blog/purescript-setup/).

Like esbuild, performance issues are steadily fading as tools are ported to Rust
([Biome](https://biomejs.dev), [Rolldown](https://rolldown.rs/))
or take a performance-conscious approach ([Vite](https://github.com/vitejs/vite), [Meriyah](https://github.com/meriyah/meriyah)).
So long as the release cadence of new JavaScript frameworks doesn't outpace 
the tools that have to support them,
the future looks promising for JS users.

So, why am I convinced we can do better still?

## Re-imagining JavaScript linters

JavaScript linters traditionally rely on a syntax tree
to analyze source code, with newer tools like Biome
supporting [control flow graphs](https://github.com/biomejs/biome/blob/main/crates/biome_control_flow/src/lib.rs).
Just using ASTs limits the extent to which source code can be examined, 
and often results in inefficient, cache-unfriendly traversals. 

As an example, consider this snippet:

```js
import express from "express";
import cp from "child_process";

const app = express();

app.get((req, res) => {
    const cmd = req.params.command;
    doCommand(cmd);
    res.json({});
})


const execCommand = cp.exec;
function doCommand(cmd) {
    if (typeof cmd === "string") {
        const fullCmd = `ls ${cmd}`;
        execCommand(fullCmd)
    } 
}

```

Take a moment to spot the security flaw here.
The URL parameters in `req.params` are passed to `exec` without any safety checks.
Ideally, a linter should flag this, but the possibility of arbitrary control flow
and function calls between `req.params` and `exec` makes it hard to track.
An AST alone isn't enough to reliably sniff out this flow of data.

Even with its security plugins, ESLint is incapable of doing sufficiently
advanced call-stack analysis to spot this, and probably won't be for at least a while.

<div class="note" style="float: left; width: 100%; margin-bottom: 20px;">
     <img
         src="/assets/img/oshiro-normal.png" width="100px"
         class="self-image"
         style="float: left;"
     />
     <div style="padding-top: 3px;">
        <b style="font-size: 17px;">injuly:</b> Don't get me wrong, ESLint is a fantastic
        linter that supports every JavaScript flavor under the sun.
        <a href="https://humanwhocodes.com/about/">N.C Zakas</a>'s
        foundational work has been a huge help at my day-job.
        But hey, nobody gets it right the first time.
        See: <a href="https://github.com/eslint/eslint/discussions/16557">#16557</a>
     </div>
</div>

I've worked on proprietary static analyzers that are capable of detecting security
hot-spots like the one you just saw[^1].
While writing them, I found that the source for [Pyre](https://github.com/facebook/pyre-check),
Facebook's Python static analyzer written in OCaml,
was an excellent reference.
Once you see what's possible with just a little more effort, it's hard to
sleep on this knowledge.
An open source tool capable of advanced static code analysis and taint checking
would be a great asset to the ecosystem.

There are several more improvements that can be made to current linting techniques.
I'll list a few of them. 

### Lossless, cache efficient syntax trees

I believe it was [Carbon](https://github.com/carbon-language/carbon-lang) that popularized 
data-oriented design for compilers.
This approach has since been adopted by several other tools 
(see the "Futher Reading" section of [this write-up](https://www.cs.cornell.edu/~asampson/blog/flattening.html)).
The idea is that you store your AST/IR nodes in memory 
so that the most common traversal order becomes a forward iteration over a buffer,
making the traversal a cache-friendly operation.

The standard format for representing JS syntax trees is called [ESTree](https://github.com/estree/estree).
While its design is neat, I've found that it tends to be inefficient for traversal in practice.
Most devtools do at least a few passes over the AST before anything can be reported
to the user[^2], and nothing makes a CPU stall like a tree that's a web of pointers.
Fortunately, compact AST representation
isn't a complex problem to solve, and the open source Carbon compiler 
has been a great reference for me.

The other issue with ASTs is loss of information. 
Being *abstract* by design, the full source cannot be re-constructed from a syntax tree alone.
This has implications when developing tools that need to generate code,
especially when dealing with whitespaces and comments.
[Red-green trees](https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees) are a well-known solution to this,
and are used by Biome, Roslyn, and Swift's LibSyntax.
For Jam's parser, I used the [the oilshell approach](https://www.oilshell.org/blog/2017/02/11.html) instead,
as it goes better with a flattened syntax tree.

### Optimizing code with data-flow aware IR 

Rather than explain myself in words, I'll flail my arms and gesture at the 
[Closure compiler for JavaScript](https://github.com/google/closure-compiler).
It is not as popular as it is useful, likely because it doesn't
gel well with existing build tools, occasionally errors out on some (rare) JS code patterns,
and doesn't support TypeScript or JSX[^3].

But the idea is there, a JavaScript code optimizer is certainly feasible,
and something that I'm intent on experimenting with.

### Compile time AST query processing 

Many linting rules are really just pattern detection,
where a part of the logic devolves into a series of nested if statements.
To simply writing these lints, ESLint uses a handy tool called [esquery](https://estools.github.io/esquery/),
which spares plugin authors from writing this: 

```js
if (
  node.type == "CallExpression" && 
  node.callee.type === "MemberExpression" &&
  node.callee.object.type === "Identifier" &&
  node.callee.object.name === "child_process"
) {
 // many of these checks are to satisfy typescript^
}
```

And lets them write this instead:

```js
if (matches(
    'CallExpression[callee.object.name = child_process]',
     node
  )) {
  // much better 
}
```

I owe thousands of saved keystrokes to this tool.
But every time I use an esquery match over if statements,
I wonder about the performance overhead of parsing query strings
at runtime.
Since each query is only compiled once, would
it make a measurable difference if the queries were processed at compile time?

Languages with pattern matching offer this feature for free—a nice property of 
the Rust based language tooling like Oxc and Ruff—
until you decide to represent your AST nodes using the data-oriented
approach I mentioned earlier.

Fortunately, Zig's `comptime` features makes it feasible,
easy even, to implement an esquery port where the queries are processed
and optimized at compile time, while still being able to operate on a compact
syntax tree.
I can also add custom syntax to the pattern language
that Scala/Rust/Haskell pattern matching can't have.

Although my intention isn't to compete with rust-based tools in speed,
I'd like to design the toolchain in a way that ensures no obvious performance
opportunities are left on the table, and users don't get surprise invoices after running
Jam on their CI.

## Opinionated code formatting 

Of all the formatters I've used across languages, I like
[gofumpt](https://github.com/mvdan/gofumpt) and `zig fmt` the most.
Say I have this (already formatted) snippet of code:

```zig
fn addFourAndPrint(firstNumber: u32, secondNumber: u32, thirdNumber: u32, fourthNumber: u32) void {
    std.debug.print(
        "sum={d}",
        .{firstNumber + secondNumber + thirdNumber + fourthNumber},
    );
}
```

You probably had to scroll to read the whole thing.
If I place a comma after the last parameter in the function, and the last argument in `print`,
the formatter prints it like this:

```zig
fn addFourAndPrint(
    firstNumber: u32,
    secondNumber: u32,
    thirdNumber: u32,
    fourthNumber: u32,
) void {
    std.debug.print(
        "sum={d}",
        .{firstNumber + secondNumber + thirdNumber + fourthNumber},
    );
}
```

This behavior is slightly different in `gofumpt`, where you can break after any number of parameters,
instead of breaking on all of them.

The thing I like about both these tools is that I can,
in some capacity, communicate with the formatter on a case-by-case basis,
without needing a config file or `// formatter-off` comments.
Both formatters are strict,
and projects using them will be fine without any configuration whatsoever.

Although I appreciate how malleable prettier is, I'd prefer a stricter formatter
that doesn't shy away from enforcing rules and has a rigid style.
I realize that this might make Jam's formatter less appealing for some, in which case, 
it's a great thing that prettier and biome both exist.
Personally, I'd like to have fewer `rc` files if I can.

## Roadmap and progress

I drafted this essay after finishing up a fast[^4], 100% spec compliant JavaScript parser in Zig. 
Shortly after, I made an [an online playground](https://injuly.in/js-playground/) for the parser's WASM build.
I've since been working on adding JSX and TypeScript support,
and have set aside some time to work on the following tools as well:

1. A source-to-source JavaScript code optimizer
2. Performant Zig ports of [esquery](https://github.com/estools/esquery) and [eslint-scope](https://github.com/eslint/eslint-scope)
3. The runtime for Jam's analyzer
4. An import resolver

Eventually, I would like to build on the foundation laid by [stc](https://github.com/dudykr/stc/),
and implement a TypeScript type checker that is reasonably fast and doesn't hog all memory.
A faster compiler, and an accessible API for querying types would vastly improve the quality of TypeScript 
tooling across the board.

However, doing so is a gargantuan task for somebody with a day job,
so no grand promises about this one just yet.
It's entirely possible that I never get around to this.

<div class="note" style="float: left; width: 100%; margin-bottom: 20px;">
     <img
         src="/assets/img/oshiro-dramatic.png" width="100px"
         class="self-image"
         style="float: left;"
     />
     <div style="padding-top: 3px; line-height: 1.5;">
        <b style="font-size: 17px;">injuly:</b> Reading the <code>tsc</code> codebase,
        its clear the authors have tried to make it as performant
        as is feasible within the realm of JavaScript.
        Alas, swords don't win gunfights. 
     </div>
</div>

Jam is very early in its life, and it will be a while 
before I can recommend people use it on production projects,
(unless you need a fast JS parser in Zig, in which case,
I can recommend the Jam as a parsing library).
But if you have any thoughts what you read so far, I'd love to hear them.

## Contributing to existing tools 

Wouldn't it be easier to improve Oxc, Biome, or deno_lint,
than write another toolchain from the ground up?
For some parts, yes, it would be.
In fact, whenever I chance upon something that other tools
can benefit from, I send patches to them.

As an example, the [Kiesel JavaScript engine](https://kiesel.dev) uses Jam's 
[unicode-id](https://codeberg.org/injuly/unicode-id) library to 
efficiently parse identifiers.
Since that commit, I've been frequently contributing to Kiesel.
I've have also started to see if I can to contribute to [Rolldown](https://rolldown.rs)
and learn more about implementing a bundler in the process.

However, some ideas behind Jam—like the optimizing compiler and taint analyzer—
truly warrant an implementation built from the ground up.
I want to do this way I believe is right, even if it takes longer.
After all, [Rome](https://github.com/rome/tools) wasn't built in a day[^5].

## Backmatter

[^1]: An early prototype for Jam I wrote in TypeScript can also detect these vulnerabilities.

[^2]: As an example, ESLint does at least four passes, first to assign `parent` property to every AST node,
    then another for scope analysis to collect variable references, then a third
    to prepare a list of queries that might match every node, followed by one final pass to execute the lint rules.

[^3]: Is supporting JSX and TypeScript really necessary when the generated code can be optimized by Closure anyway?
    I believe that optics are important here.
    I'd prefer a tool that edits my TypeScript code directly over one that opaquely changes the generated code.
    It's also much harder to review changes made to machine generated JavaScript.

[^4]: I'll update the repository with some comparison benchmarks shortly after I publish this post.

[^5]: The title of word salad was taken from [a notgull post](https://notgull.net/announcing-dozer/) I liked. Give it a read.
   
