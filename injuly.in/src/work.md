---
template: work
---

# Work

This is a publicly available record of my work that goes beyond 
[my resume](/resume.pdf),
with some personal commentary and highlights.
Most of my career has been spent working on **compilers** and
**web servers**; but I've have also made a good number of production UIs.

Day jobs notwithstanding, I've studied and built numerous things on the side
—systems programming, copious amounts of Zig and Haskell, game-development, blah blah—
but those records are tucked away in [my blog](/blog).

This page only includes projects and tools that I've worked
on *professionally*.
i.e, no FOSS work and personal projects.
Those can be found on my GitHub, and elsewhere on this site.

<div class="workplaceHeader">
  <div class="workplaceHeader-left">
    <div class="workplaceHeader-logo">
      <img 
        src="/assets/work/deepsource-logo.svg" 
        alt="DeepSource logo"
      />
    </div>
  </div>

  <div class="workplaceHeader-right">
    <h2>DeepSource</h2>
    <div class="workplaceHeader-designation">
      Software Engineer I -> II
    </div>
    <em class="workplaceHeader-duration">
      October 2021 – Present
    </em>
  </div>
</div>

***Tools**: Golang (for compiler tooling), TypeScript, PureScript, SolidJS, Bun, SQLite, gRPC.*

A YC backed code health monitoring platform.
During my time here, I've worked across the board on several projects
and used numerous tools.
It's hard to summarize all the work in a few bullet points.
Even still, here's what I consider to be my most impactful work:

- [JavaScript Static Analyzer](https://app.deepsource.com/directory/analyzers/javascript): Fast and efficient static analysis built from the ground up.
  The analyzer is much more capable than existing linters like ESLint,
  and supports React, VueJS, Angular, Node, TypeScript, and JSX out of the box with zero configuration.
- **Security coverage**: Implemented a data-flow analysis engine to perform taint analysis on JavaScript projects.
  A basic architecture is [described on the blog](https://deepsource.com/blog/scanning-javascript-code-for-security-vulnerabilities).
- [IDE integration](https://deepsource.com/platform/ide): Built and maintained the plugin, along with its relevant services.
- Wrote web-services for collecting metrics, tracing, and connecting user's machines to our cloud.
- Built [AI AutoFix](https://deepsource.com/ai): A tree-sitter based product that can generate fixes for issues reported by DeepSource.
- **Software Component Analysis**: Built a language agnostic framework that finds vulnerable dependencies,
  and performs reachability analysis to only report the ones that actually
  affect the user's codebase.

<div class="workplaceHeader">
  <div class="workplaceHeader-left">
    <div class="workplaceHeader-logo">
      <img 
        src="/assets/work/tezos-logo.svg" 
        alt="Tezos logo"
      />
    </div>
  </div>

  <div class="workplaceHeader-right">
    <h2>Tezos</h2>
    <div class="workplaceHeader-designation">
      Student Developer (Tezos Fellowship) 
    </div>
    <em class="workplaceHeader-duration">
      August 2021 – October 2021 
    </em>
  </div>
</div>

***Tools**: Haskell, SmartPy, TypeScript, React, NodeJS, Express, RabbitMQ.* 

Tezos is a well-known blockchain network.
I'm not particularly psyched about blockchain/crypto-currency, though.
In fact, my work here was more grounded in algorithms, programming
language runtimes, and web services.

- Using Haskell, I prototyped a formal verification tool for [Michelson](https://www.michelson.org/) – 
the bytecode format for Tezos.
- Implemented a genetic algorithm to breed and procedurally generate 
  digital pets with their custom avatars and varying attributes.
- Worked on a React based web client to interact with NFTs generated using
  the aforementioned genetic algorithm.

<div class="workplaceHeader">
  <div class="workplaceHeader-left">
    <div class="workplaceHeader-logo">
      <img 
        src="/assets/work/lablua-logo.png" 
        alt="LabLua logo"
      />
    </div>
  </div>

  <div class="workplaceHeader-right">
    <h2>Google Summer of Code</h2>
    <div class="workplaceHeader-designation">
      Student Developer @ LabLua
    </div>
    <em class="workplaceHeader-duration">
      May 2021 – August 2021 
    </em>
  </div>
</div>



***Tools**: C, Lua, and Python (for benchmark and codegen scripts, etc).* 

A Brazilian research lab that works on the Lua
programming language and its ecosystem.
This was my first time working on a production compiler,
and I'm still grateful to have had th 

- Implemented Higher Order Functions in the Pallene compiler.
- Implemented Closures and lexical capturing.
- Worked on some IR-level compiler optimizatons.
- Improved error messages and diagnostics.

