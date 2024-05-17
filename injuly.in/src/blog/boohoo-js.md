---
template: "post"
tags: ["javascript", "programming-languages"]
title: "BoOHoO JaVaScRiPT!!"
date: "2021-07-18"
meta: "JavaScript may be terrible, but it's not as bad as you think"
is_blog_post: true
---

Here's a few lines of JavaScript code that, apparently, make it a bad programming language:

```javascript
0.1 + 0.2 == 0.3       // false
NaN == NaN             // false
typeof NaN             // "number"
true == 1              // true
[5, 4, 31].sort()      // [ 31, 4, 5 ]
Math.max()             // -Infinity
[1, 2, 3] == [1, 2, 3] // false
```


Or at least that's what today's twitter trending page told me (yes, JavaScript was trending on twitter, probably because of Hacktoberfest).
Unconvinced? Look up "JavaScript bad meme" in the search bar and see for yourself.
## Playing the devil's advocate

First, let's briefly dissect the expressions one by one, starting with the most common:

```javascript
0.1 + 0.2 == 0.3; // false
```

This is not JavaScript's fault, but a by-product of how computers represent floating point numbers.
You can't have infinite precision decimals within finite space, so we make a trade-off. We store all floating point numbers in 64-bits of space, and loose some precision in the process. This is standardized by the [IEEE 64 bit floating point representation](https://en.wikipedia.org/wiki/Double-precision_floating-point_format). The Wikipedia article does a better job of explaining it than I ever could.

Most other languages do the same thing. C, Java, C++, Lua, Python, you name it.
Note however that some do not, [Raku](https://www.raku.org/)  for example, correctly evaluates `0.1 + 0.2` to `0.3`. Instead of using IEEE floats, Raku uses rational numbers of the form `p/q` where `p` and `q` share no common factors barring 1.

So why don't all languages do what Raku does?
For one, you now store two numbers instead of one to represent a single float.
Secondly, most CPUs have hardware circuits that perform arithmetic on IEEE floats.
In a sufficiently CPU intensive application, losing out on hardware accelerated arithmetic can slow things down significantly.

Next up, we have:

```javascript
NaN == NaN; // false
```

This one is also simple, The aforementioned IEEE double precision floating point spec blithely states that `NaN` must never be equal to another `NaN`. That way, if you have two `NaN`s as the result of logic errors in your program, they won't pass any equality checks by mistake.

Moving on...

```javascript
typeof NaN; // number
```

`NaN` literally means "not a number", so why does JavaScript tell me otherwise? This could have been handled differently by having a separate `NaN` type. However it makes sense when you think about it this way: the `NaN` value always appears in places where you would expect numbers, as the result of numeric operations, `Math.sqrt(-1)` for example.

Most other languages also represent `NaN` in their floating point data types.

And now:

```javascript
true == 1; // true
```

This is debatable. Python, Ruby and C++ do what JavaScript here does, Lua however disagrees. Java croaks and throws a type error at compile time. If you ask me, all these approaches make sense. In duck typed languages, we're seldom concerned with an actual boolean value, and really care about its [truthy-ness](https://developer.mozilla.org/en-US/docs/Glossary/Truthy).

The following snippet passes in both C (and C++, sometimes with a compiler warning) and JavaScript:

```javascript
if (0) {
  /* stuff */
}
```

The `==` operator compares **truthiness** of values. i.e it makes a judgement based on a **property** of it's operands, and not the values themselves. Consider:

```javascript
"" == false; // true
0 == ""; // true
```

Note however that it's overloaded to act differently for numbers and strings (and this behaviour *is* bad design).
If you want strict value comparison, use `==`'s sister `===` instead.

```javascript
"" === false; // false
0 === ""; // false
```

Up next:

```java
[5, 4, 31].sort() // [ 31, 4, 5 ]
```

This questionable API design.
I don't have much to say about this, except that I'd prefer `sort` either default to numeric sorting, or mandate the comparator argument.

To do a number comparison-based sort, you'd have to call it like so:

```javascript
[5, 4, 31].sort((a, b) => a - b); // [4, 5, 31]
```


Now, `Math.max`:

```javascript
Math.max(); // -Infinity
```

`Math.max` is a variadic function, meaning you can call it with 1, 2, 3 or 0 arguments. When called with 0 arguments, it returns `-Infinity`. Why? Because maximum of an empty set is negative infinity.

Why not throw an arity error on 0 arguments you ask? Because there are cases where the number of arguments isn't known at compile time. For example, calling it with the spread operator like `Math.max(...myArgs)`. In fact, try implementing `Math.max` yourself, how would you do it?

```javascript
Math.max = (args...) => {
    let maxSoFar = /* what? */
    args.forEach(arg => {
       if (arg > maxSoFar) maxSoFar = arg;
    });
    return maxSoFar;
}
```

What should `maxSoFar` be initialized with? What is that one value which is guaranteed to be less than any number? `-Infinity`, of course. We can't initialize it with `args[0]` here because the index may be out of bounds, when `args` is empty. Another approach could be:

```javascript
Math.max = (args...) => {
   if (args.length < 1) throw new Error("Expected at least 1 argument");
   // ...
}
```

Personally, I'd prefer something like this, but I'm okay with the existing `max` function too.

Finally,

```javascript
[1, 2, 3] == [1, 2, 3];
```

This is a result of referential equality, heap allocation, and a lack of operator overriding.
This behaviour is common across most general purpose languages (with the exception of Python).

Beyond these common expressions, I've seen other misinformed takes on the language.
For instance:
## JavaScript is slow.

Programming languages do not have performance characteristics.
Their implementations do.
Python isn't inherently a slow language, though its interpreter(CPython) might be.
Using a different implementation (Numba, PyPy, Cinder, etc.) will always react differently to benchmarks.

That said, the design of a language can heavily influence its general performance, across implementations.
Languages that exhibit highly dynamic behaviour are difficult to optimize, and therefore tend to be slower than their compiled friends.

Some of the most vital JavaScript engines today, V8, JSC, Hermes, and SpiderMonkey are extremely performant, optimizing beasts. They're JIT compilers, the precise inner workings of which warrant an essay of their own.

Taking a look at some benchmarks from the [computer language benchmark games](https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html) site, it would appear that Node.js (which embeds V8) is actually not bad at all, even beating some implementations of Java, Haskell, and C++ (!?).

Of course, Benchmarks do not represent real world programs.
You don't use JavaScript to crunch high volumes of data, or do automatic differentiation (If you do, I urge you to reconsider).
You use JavaScript to color buttons on a webpage.
## JavaScript is untyped, and unsafe.

No. JavaScript is **dynamically checked** and **safe**. 
Take this excerpt out of Pierce's book, [Types and programming languages](https://www.cis.upenn.edu/~bcpierce/tapl/):

> Terms like "dynamically typed" are arguably misnomers and should probably be replaced by "dynamically checked," but the usage is standard.

**Dynamically checked:** Variables and symbols do **not** have types. The types are bound to **values** instead.

Let's try with an example. The number `100`, has a type of "number" at runtime. However in this statement:

```javascript
let x = 100;
```

The **symbol** "x" and in it's semantic definition, does not have a type. But it's still not wrong to say _"the type of 'x' is `number`"_. Why? Because when `x` is evaluated by the runtime, as a **value**, it does have a type (which can be seen by evaluating `typeof x`).

However the **name** "x" is void of any type. So we can do `x = 'xyz'` and the interpreter will happily run it.

The key is to understand the difference between the **name** ''x" which can be bound to any value, and the **value** "x", which always evaluates to whichever value the name is bound to at the time of evaluation.

**Safety**: The most commonly accepted definition of safety is:
_A language where every code snippet always is well-defined by the standard._

And this is indeed true of JavaScript. If you don't like that thought, pause for a second and try to come up with a JavaScript code snippet that is **undefined** under the ECMAScript spec. You'll find that none exist. It may error out, or produce wonky output, but the behaviour is **defined**, and will be the same for every standard-compliant JavaScript implementation.

In fact, C++ is **less safe** than JavaScript, despite being statically typed (which makes sense, because typing and safety are orthogonal). For example, the following C++ code does not have any defined behaviour:

```cpp
#include <iostream>

union IntOrPtr {
    int integer;
    const char* ptr;
};

int main() {
  IntOrPtr u = { .ptr = "some string" };
  std::cout << u.integer << '\n'; // <--- Whoops!
  return 0;
}
```

Accessing the uninitialized field of a union is undefined behavior, and may have different behavior on different compilers, hardware, OS, or different runs of the same executable.

|                      | Safe                                                                                                                            | Unsafe                                                                                                  |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| **Dynamic checking** | Easy to write erroneous code, but easier to debug due to well defined runtime behavior and inspection capability. | Don't know of any such languages except hobby projects.        |
| **Static checking**  | Best of both worlds. Most languages in this bucket are either functional, or r***-like.                                                              | Easier to write correct programs, but harder to debug as there are no types and bounds at runtime. |

There is always a tradeoff.
If you miss having compile time type checking when writing JS, maybe try Typescript/Purescript?
### JavaScript is single threaded, hence bad.

I don't disagree with this one.

Thankfully V8 (and ~~NodeJS~~ libuv) are designed well enough that this problem is easily circumvented with asynchronous code.

Some languages have built-in multithreading support, like atomic data types, or operators that spawn threads/processes.
Others bake threading support into their standard libraries (`pthread`).
JavaScript falls in the second category.
### Not all roses.

Ok, now that I'm done playing the devil's advocate,
I do have my own fair share of complaints.

1. Starting off on an easier foot, the `var` keyword. ECMA's unwillingness to forsake backwards compatibility has led to some very annoying legacy cruft in JS that just won't go away. **Function scope** is one of those.

2. **Too much implicit type-coercion**. The type system is incorrigibly broken. Everything can coerce into Everything, and the runtime will jump through all kinds of hoops before throwing an error.
   
3. **Wonky whitespace sensitivity.**
   JavaScript looks like it's not white-space sensitive at first glance. Catch this:

   ```javascript
   a = 124 // ok
   b = 100 // ok
   c = 124 d = 214 // NOT OK
   ```

   That's right, it isn't really white-space sensitive at all. I wish the language had simpler rules regarding this, but that's unfortunately not the case. So we rely on linters to warn us. (This ties into the next point).

4. **Automatic semi-colon insertion**. What you're seeing above is really an effect of [this](https://262.ecma-international.org/7.0/#sec-rules-of-automatic-semicolon-insertion) annoying ECMA ruleset. So behold:

   ```javascript
   function ten() {
     return
     10;
   }
   console.log(ten()); // undefined
   ```

   This not very obvious when your functions are deeply nested and the expression is big, like:

   ```javascript
   function foo() {
       if (..) {
           for (..) {
               while (...) {
                   if (...) {
                       // not doing what you think it's doing
                       return
                       	GetFooFactory.MakeFoo(MakeBar(Baz(Pop, Bop)))
                   }
               }
           }
       }
   }
   ```

5. **Contextual keywords**. `async`, `await` and even `let` are keywords under some contexts and identifiers under others. This leads to some funny possibilities like `var let = 100`. Although not inherently bad, I still dislike them personally because of it's inconsistent nature.

6. **Unused reserved words**. `enum`, `private`, `public` are reserved words that you cannot use. But they don't bring any features to the language. ECMA just _might_ one day add private members or enums and so they reserved these words. I see this as a good strategy, but why not do one thing? If you have contextual keywords, then make these keywords contextual too instead of "pre-reserving" them and making other future keywords contextual. That said, I see this as a reasonable approach. Although I don't quite understand why implementing enums is taking so long :p

7. **Hoisting**. Not a big deal, but not WSYIWYG. I like my scripting languages WSYIWYG, thank you.

If you want more, try [wtfjs](https://wtfjs.com/).
