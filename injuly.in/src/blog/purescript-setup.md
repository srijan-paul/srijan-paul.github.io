---
template: "post"
tags: ["FP", "programming-languages", "purescript"]
title: "How I set up PureScript projects with ESBuild and Spago"
date: "2023-07-29"
meta: "Setting up a PureScript project with dev-server and JS interop"
is_blog_post: true
---
I've been working on a small research project for about two weeks.

Today, it reached an all-too-familiar phase when implementing research papers
– the part where I realise I've gone so far off the rails that my code no longer 
resembles anything within 5 citations of the paper that inspired it.

Fortunately for me, this particular project is tiny - standing at a mere ~2k lines of TypeScript.
After about two years of wrangling TypeScript professionally – and ~four years taming it with pet projects –
I've mastered a handful of tricks to combat tech-debt and bug prone code.
My go-to move?
Time Travel.
AKA Go back in time and choose a better language. 

Anyway, I'm now porting the project to PureScript, and couldn't find a decent reference that explains
how I can setup a dead simple web project in it with ESBuild.

I decided to document the (fairly easy) process for myself,
and anyone else who also likes Haskell but is a <s>soy</s>web-dev.

So here we are, I guess?

## Tools

I’m using the following tools as dev-dependencies: 

- Node v18
- ESBuild v0.18.1
- PureScript v0.15.9
- purs-tidy v0.10.0
- [Spago](https://github.com/purescript/spago) v0.21.0 (installed globally)

```bash
mkdir ps-project && cd ps-project
pnpm init
pnpm add -D purs-tidy purescript esbuild
```

While not necessary, I also recommend adding [purescript-backend-optimizer](https://github.com/aristanetworks/purescript-backend-optimizer) for generating faster JavaScript.

## Spago

As of me writing this, JavaScript and PureScript dependencies have to be managed separately.
All JS dependencies are managed by a JavaScript package manager (`pnpm`/`npm`/`yarn`),
while all PureScript packages are managed by [Spago](https://github.com/purescript/spago) (or pulp, if you prefer that).

Consequently, you’ll need to initialize your directory as a spago project:

```bash
spago init
```

You should now see a `spago.dhall` file in your project root.
This is the PureScript equivalent of a `package.json`.
There will also be a hello world program in `src/Main.purs`.

You can either build your project using `spago build`, or run directly it using `spago run`:

```bash
$ spago run
Hello, world
```

## ESBuild

PureScript v0.15 dropped support for CommonJS modules and the `purs bundle` command.
All generated JavaScript in the `output` directory now needs to be sewn together by an external bundler.
According to [the migration guide](https://github.com/purescript/documentation/blob/master/migration-guides/0.15-Migration-Guide.md)
— and to nobody’s surprise — [ESBuild outperforms all others](https://github.com/purescript/documentation/blob/master/migration-guides/0.15-Migration-Guide.md) for this task.

I use ESBuild for 3 things:
1. Importing PureScript functions in JavaScript files.
2. Bundling all files generated in #1 into a single [[1](#backmatter)] JavaScript file.
3. Serving the built project on localhost.


```bash
# Import purescript functions in `.js/.ts` files, and transpile `.purs` files.
pnpm add -D esbuild-plugin-purescript
# Copy static files to the build directory.
pnpm add -D esbuild-copy-static-files
```

ESBuild does *not* transpile PureScript.
To do that, you still use Spago.

For most projects, you’ll want to have a `build.mjs` file in your project root to save yourself
the trouble of passing a quintillion command line flags to `esbuild` every time.
Mine looks like this:

```javascript
import esbuild from "esbuild"
import pursPlugin from "esbuild-plugin-purescript"
import copyStaticFiles from "esbuild-copy-static-files"

const ctx = await esbuild
  .context({
    entryPoints: ["src/index.js"],
    bundle: true,
    outdir: "dist",
    plugins: [
      // allow importing Purescript modules in JavaScript files.
      pursPlugin(),
      // copy everything under `static` to `dist`.
      copyStaticFiles({ src: "./static", dest: "./dist" })
    ],
    logLevel: "debug"
  })
  .catch((e) => {
    console.error(e)
    process.exit(1)
  });

// you can use a CLI flag for this, 
// instead of unconditionally calling `watch` every time.
await ctx.watch()
// same applies to `serve`.
await ctx.serve({ servedir: "./dist", port: 3000 })
```

I keep all assets and HTML in a `static` directory, hence the `copyStaticFiles` call.
If you don't have static files, remove that call (or it'll throw an error saying "static" directory doesn't exist).

If you did everything correctly so far, simply entering `node ./build.mjs` in your shell should bundle the project [[2](#backmatter)] into `dist/index.js`.

Now, you can also call PureScript from JavaScript (or TypeScript):

```purescript
-- src/Main.purs
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

main = log "Hello, World!"
```

```javascript
// src/index.js
import { main } from "./Main.purs"

main(); // Hello, World!
```

In your `static` directory, you can have an HTML file that references the bundled JS file generated by ESBuild: 

```html
<html>
    <head>
        <title> TypeScript is cope </title>
    </head>
    <body>
        <script src="index.js" type="module"></script>
    </body>
</html>
```

## Watch mode and dev-server.

To run `spago` and `esbuild` in parallel, I use [concurrently](https://www.npmjs.com/package/concurrently):

```bash
pnpm add -D concurrently
```

In your `package.json` file, add a `"dev"` script:

```json
{
  "scripts": {
    "dev": "concurrently 'spago build --watch' 'node ./build.mjs --watch'" 
  }
}
```

You can run this pnpm command to have your project auto-build on every save:

```bash
pnpm dev
```

Next you'll want to... oh, we're done.

Well, If you've made it this far, I can already tell you're going to do well writing PureScript.
The error messages are longer than this post :)

## Backmatter

1. Or multiple, if you've multiple entry points.
2. Note that the build script ends in `.mjs`, and uses imports and top-level awaits.
   This might not work on older versions of node. You can either convert the script to a commonJS file,
   or upgrade your NodeJS version.
