Developers at Facebook have written a type checker for Python called [pyre-check](https://github.com/facebook/pyre-check).
It's written in OCaml - an excellent language for working with programming language sources.
On top of Pyre, sits Pysa - a tool that performs security related static analysis
on Python source code.

As a part of my job, I maintain a static code analyser for the JavaScript ecosystem.
I've written a major chunk of the linter/analyzer that powers DeepSource's [JavaScript analyzer]().
As of late, an idea for an ambitious project had been brewing in my mind -
A performant JS Linter with built-in data flow graphs ([1]()), and taint analysis.

## (Unsuccessfully) Writing a JavaScript linter in Haskell

Why Haskell?
Linters deal with syntax trees - data structures that are amenable to algebraic types.
Bulk of a linter's job is to efficiently traverse and query ASTs.
Inspecting an AST node's structure with Haskell's pattern matching is a breeze.
Of course, I could've used OCaml, Elm or PureScript, but I just like Haskell more.

First, I want to see if there are any decent parsers for JavaScript in Haskell.
A quick Google search tells me I have three options:
[language-javascript](https://hackage.haskell.org/package/language-javascript), 
[language-js](https://hackage.haskell.org/package/language-js),
and [language-ecmascript](https://hackage.haskell.org/package/language-ecmascript).

I tried using all of these.
To stress test language-javascript, I wrote a script to get it to parser minified sources of some popular libraries.
In the process, I found that it fails to parse arrow functions in object properties:

```javascript
const thisFails = {
  a: () => 1,
  b: () => 2
}
```

When on the `1` token, it sees a comma and tries to parse a comma separated expression (like `1, 2`),
and fails with a very unhelpful error message.

So this one's out of the equation.

What about `language-js`?
There is an [issue](https://github.com/diasbruno/language-js/issues/21) on GitHub saying it 
incorrectly parses arrow functions, so we'll cross this one out as well.

Finally, we're left with `language-ecmascript`.
I tried adding it to my project's `.cabal` file, and it failed to compile.

Alright, perhaps we should roll our own parser.
I would hate to hand roll a parser for the ecmascript grammar, and then maintain it.
That is just too much effort, and there are too many edge cases I can fail on.
It would be nice if I could use a parser generator like [Happy]().

All I need is a formal description of JavaScript's grammar.
Fortunately, this wasn't too difficult to find.

A half-decent JavaScript linter would support TypeScript.

## How hard is a linter? 

Writing a linter is not the most difficult job on the planet.
You merely walk over the AST and at every node, check if it matches an anti-pattern.
For example, using literals in an if-statement's condition is an anti-pattern:

```javascript
if (false) {
  // ... code
}
```

The AST for the above snippet would look like this:

```javascript
const ifStatAST = {
  // Every JS AST represents a "program",
  // which is a collection of statement nodes.
  type: "Program",
  statements: [
    {
      type: "IfStatement",
      test: {
        type: "Literal",
        value: false
      },
      consequent: {
        type: "BlockStatement",
        body: []
      },
      // there is no else-block
      alternate: null
    }
  ]
}
```

An ESLint rule that prevents you from having code like that is really an object that tells ESLint which nodes to visit,
and what to do on those nodes:

```javascript
const rule = {
  // .. boilerplate for ESLint rules,
  create(context) {
    return {
      IfStatement(ifNode) {
        if (ifNode.test.type === "Literal") {
          context.report({
            message: "Do not use literals in if-statement conditions",
            node: ifNode
          })
        }
      }
    }
  }
}
```

This object, called a "rule" by ESLint, communicates to the linter that it should run the provided logic 
when visiting `IfStatement` nodes.

Most lints, however, are not so simple.
You can only do so much by looking at the mere shape of a user's program.
Often times, you want to track where a value ends up.
Here is an example of SQL injection in an express.js codebase:

```javascript
const app = express()

app.get("/user", async (req, res) => {
  const name = req.params.username 
  const query = `select * from users where name = ${name}`
  const result = db.runQuery(query)
  if (!result) { res.statusEnd(404); return; }
  
  res.json(result)
})
```

If the `username` URL parameter contains a malicious string, this app is now exposed to SQL injection.
However, to prevent this from happening we have to:

1. Look for all function calls that match the shape of `db.runQuery`.
2. Ensure that `db` is an instance of a database cursor from a database library.
3. Ensure that the argument to `runQuery` is a value that contains tainted data in some form or another.

The 3rd step is the hardest.
<!--We need to know where `query` was last assigned, and the value it was assigned.-->
<!--In our case, it turns out to be a template string, so we go through all the interpolated expressions and run into -->
<!--the variable reference - `name`.-->
<!--Onec again, we follow `name` to see where it comes from, notice that is initialized to a property of the `req.params` object,-->
<!--which is controlled by the user.-->
<!---->
<!--At this point, it is safe to say that this is an SQL injection risk, as the user did not take any steps to sanitize the value before-->
<!--passing it to a database query.-->
<!---->

The linter performs AST-level taint analysis to determine if me some pattern in the user's codebase is a security vulnerability.
This requires a good amount of control flow analysis, tainted source and sink identification,
and sometimes, type information; none of these are available directly in the AST.

Ideally, you would want to:
  1. Traverse the AST and build a control flow graph.
  2. Traverse the AST again, and build a type information table. ([1]())
  3. Lint the co... just kidding, traverse the AST yet again, and identify the taint sources. 
  4. Lint the code, using all information gathered in steps 1-3.

That is a *lot* of tree traversal, and every iteration requires traversing almost the entire tree.
This spawns a gigantic codebase, of which 20% is pre/post-processing data, and 80% is walking over trees and recording information.
For every project our linter analyzes, there will be one AST per source file.
So, Traversing each AST 4 times in a project with 1000 source files would mean 4000 tree traversals (plus parsing, generating lint repors etc.).
To keep your users happy, your linter has to be fast.

While you're busy optimizing your tree traversal logic,
you receive an e-mail from an angry user: "My project uses React. Why is it not supported!?".
Time to stash your optimizations in the backlog, and begin adding support for react flavored JSX.

Halfway through your chore, rings another notification bell:
"A JS linter that does not support [AssemblyScript](https://www.assemblyscript.org/)? What a shame."

