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
     <div style="padding-top: 3px;">
        <b style="font-size: 17px;">injuly:</b> I understand not everyone will be psyched about
        JavaScript tooling.
        Feel free to skip the prose,
        and try the <a href="https://injuly.in/js-playground">WASM playground </a> for the parser,
        or browse the code on <a href="https://codeberg.org/injuly/jam">Codeberg</a>
        and <a href="https://github.com/srijan-paul/jam">GitHub</a>.
        All feedback is welcome!
     </div>
</div>

Few things keep me up at night like knowing there are problems out there 
that we've learned to live with, simply because doing it right demands 
concentrated effort, but doesn't bear any monetary incentive.

The JavaScript ecosystem—bundlers, formatters, static analyzers, even TypeScript—
can be vastly better than what we've to come to accept as standard.

Fortunately, competent people have invested their time into the ecosystem 
for those of us who couldn't.
Evan Wallace, for example, significantly reduced JS build times 
with the [ESBuild bundler](https://esbuild.github.io/).
Compared to tools like webpack, esbuild finishes instantly,
and is my go-to build tool for JavaScript/JSX/TypeScript, and even PureScript. 

Like esbuild, performance issues are starting to disappear
as devtools are ported to Rust ([Biome](https://biomejs.dev), [Rolldown](https://rolldown.rs/)),
or are written with a performance conscious approach ([Vite](https://github.com/vitejs/vite), [Meriyah](https://github.com/meriyah/meriyah)).
As long as the [release cadence of new JS frameworks](https://dayssincelastjavascriptframework.com/) doesn't outpace
the tools that have to support them,
there is good reason to believe that JavaScript developers will have access 
to significantly better devtools in the future.

And yet, I believe we can do better.

## Linting with more than syntax trees

JavaScript linters solely rely on a syntax tree
for information about the source code, although newer tools like Biome
also support [control flow graphs](https://github.com/biomejs/biome/blob/main/crates/biome_control_flow/src/lib.rs).
Using only an AST severely limits the extent to which source code can be studied,
and often makes the linting process cache unfriendly and less performant.

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

function doCommand(cmd) {
    if (typeof cmd === "string") {
        const fullCmd = `ls ${cmd}`;
        execCommand(fullCmd)
    } 
}

const execCommand = () => cp.exec(fullCmd);
```

There is a security vulnerability here.
It might have taken you a second to notice that the URL parameters present in
`req.params` are being indirectly passed to `cp.exec` without any safety checks.
Ideally, a linter should warn you about this, but there can be arbitrary control flow
and function calls between `req.params` and `exec`. An AST isn't enough to
reliably sniff out the flow of data.

Even with security plugins, ESLint is currently incapable of doing sufficiently
advanced call-graph analysis. Even if it was, the current architecture of ESLint
would have to retrofit an implementation that isn't exactly ideal.

<div class="note" style="float: left; width: 100%; margin-bottom: 20px;">
     <img
         src="/assets/img/oshiro-normal.png" width="100px"
         class="self-image"
         style="float: left;"
     />
     <div style="padding-top: 3px;">
        <b style="font-size: 17px;">injuly:</b> Don't get me wrong, ESLint is fantastic,
        and the only linter that supports all JS flavors under the sun.
        <a href="https://humanwhocodes.com/about/">N.C Zakas</a>'s
        foundational work has tremendously helped me at my day-job.
        But nobody gets it right the first time.
     </div>
</div>

I've worked on proprietary static analyzers that are capable of detecting security
hot-spots like the one you just saw.
While working on those projects, the source code for [Pyre](https://github.com/facebook/pyre-check),
Facebook's Python static analyzer written in OCaml, turned out to be a great reference implementation.
Once you find out what's possible with just a little more effort, you can't really
sleep on this forbidden knowledge.
An open source tool capable of advanced static code analysis can be *extremely* useful
for the ecosystem as a whole.

There's many more ways a modern JS linter can improve over the norm:

1. Cache efficient and compact parse trees, like Jam's, for faster traversal.
2. Lossless syntax trees, like Oilshell or Rowan.
3. Data flow IR to optimize code, like [the Closure compiler for JS](https://github.com/google/closure-compiler).
4. An AST query language like [esquery](https://estools.github.io/esquery/), but with *zero* runtime overhead.

All of these are things that I'm actively working on, but it will be a while before I
can share significant updates, primarily because I'm more concerned with doing things
the way I believe is right, even if it takes time.

## Smart, opinionated code formatting 

Of all the formatters I've used across languages, I like
[gofumpt](https://github.com/mvdan/gofumpt) and `zig fmt` the most.

```zig
fn addFourAndPrint(firstNumber: u32, secondNumber: u32, thirdNumber: u32, fourthNumber: u32) void {
    std.debug.print(
        "sum={d}",
        .{firstNumber + secondNumber + thirdNumber + fourthNumber},
    );
}
```

You probably had to scroll to read the whole thing.
Now, if I place a comma after the last parameter in the function, and after the last argument in `print`,
the formatter prints the code like this:

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

The thing I like most about both these tools is that I can, in some capacity,
communicate with the formatter on a case-by-case basis, without needing a config file or `// formatter-off`
comments.
Both formatters are strict, and projects using them will be fine without any configuration whatsoever.

Although I appreciate how malleable prettier is, I'd prefer a stricter formatter
that doesn't shy away from enforcing rules and having a rigid style.
I realize that this might make Jam's formatter less appealing for some, in which case, 
it's a good thing that prettier and biome exist :)

## Jam's roadmap and progress
