*"JavaScript is the devilspawn!"* cried Dante, the average modern day web developer. *"The semantics make little sense, the spec is absolute heresy! Brendan Eich be crucified!"*

His voice echoed throughout the halls of the Purgatory's fifth circle. 
With his ally Virgilio by his side, the duo sat on the bank of Styx, gazing upon their image in the bloody river.

"*`0.1 + 0.2 != 0.3` evaluates to `false`!? `NaN != NaN` is `true`? With a curse like that, we shall forever be stuck in purgatorio."*

"*Art thou, too, of the other fools, Dante?* " Virgilio, the wise pagan soul broke his silence.

*"Pardon, Virgilio?"* 

*"Such rampant ignorance. My soul whimpers in despair."*  said the exasperated Virgilio.

*"What is this ignorance you speak of, partner?"* the unnerved Dante asked.

Virgilio faced him and continued...

*"It is true that JavaScript is second-rate. I could stake my life on it. But to compare it to the devil itself, is to manifest the greatest of sins!"*. Tears started pouring down Virgil's eyes, as he went on to explain why so much of the critique JavaScript receives is misplaced.

Dante's reflection in the river of Styx might be contrasted with that of a novice programmer, new to the delicate craft of forging computer programs out of several different languages. It is completely fair for the novice to critique Programming Languages, as it is for anyone else. However, it is in everyone's best interest if they do some reading up before presenting their criticisms to the world. 

In this post, I want to:

1. Take a look at popularly misplaced critiques of JavaScript.
2. Provide some critiques rooted somewhat more in reason.

Below is a list of JavaScript expressions, along with the values they evaluate to.  JavaScript receives crazy amount of slack for it's design choices, however it may surprise some to know that many of these mistakes aren't JavaScript's mistakes in the first place!

```javascript
0.1 + 0.2 == 0.3       // false
NaN == NaN             // false
typeof NaN             // "number"
true == 1		       // true
[5, 4, 31].sort()      // [ 31, 4, 5 ]
Math.max() 			   // -Infinity
[1, 2, 3] == [1, 2, 3] // false
```

Moreover:
1. JavaScript is slow.
2. Untyped and extremely unsafe.
3. Single threaded.

If you're anything but a very new programmer, the above might come off as obvious to you, and you might be better of skipping half of this post! However, it really is true that the above are some of the most  "meme"-d  takes on JavaScript. Type  "JavaScript bad meme" into your search engine of choice to convince yourself, that's how I got to the bunch above.

"JavaScript bad because `0.1 + 0.2 != 0.3`" is tech-twitter's favorite game to play on weekends!

Before we move on, Let's establish this:

> I want to pick on JavaScript's **design**, not it's **ecosystem**.

## Playing the devil's advocate

First, let's briefly dissect the expressions one by one, starting with the most common:

```javascript
0.1 + 0.2 == 0.3 // false
```

This is not JavaScript's fault, but a by-product of how computers represent floating point numbers.
You see, we can't have infinite precision decimals within finite space, so we make a trade-off. We store all floating point numbers in 64-bits of space, and loose some precision in the process. This is standardized by the [IEEE 64 bit floating point representation](https://en.wikipedia.org/wiki/Double-precision_floating-point_format). The Wikipedia article does a better job of explaining it than I ever could, so I refer you to that in case you want to know more.

Most other languages do the same thing. C, Java, C++, Lua, Python and friends.
Note however that some do not, [Raku](https://www.raku.org/) (also called Perl-6 by some) for example, correctly evaluates `0.1 + 0.2` to `0.3`.
Magic?  Nope, just a different representation written in software. Instead of using IEEE floats, Raku uses rational numbers of the form `p/q` where `p` and `q` share no common factors barring 1.

Why don't all languages do what Raku does?
This has some trade-offs, speed is the very obvious first candidate, but also space. Now we have to store 2 numbers, `p` and `q`. This not only means twice the space, but the number may no longer fit in CPU registers.

Next up, we have:

```javascript
NaN == NaN // false
```

This one is also simple, The aforementioned IEEE double precision floating point spec simply states that `NaN` must never be equal to another `NaN`. To my knowledge, this is mostly to immediately raise alarms instead of quietly passing through conditions of `if` statements and such.


Moving on...

```javascript
typeof NaN // number
```

`NaN` literally means "not a number", so why does JavaScript tell me otherwise? This could have been handled differently by having a separate `"NaN"` type. However it does make sense when you think about it this way, the `NaN` value always appears in places where you would expect numbers. `Math.sqrt(-1)` for example.

And now:

```javascript
true == 1 // true
```

This is debatable. Python, Ruby and C++ do what JavaScript here does, Lua however disagrees. Java rebels and throws a type error at compile time. If you ask me, all these approaches make sense. In duck typed languages, we're seldom concerned with an actual boolean value, and really care about [truthy-ness](https://developer.mozilla.org/en-US/docs/Glossary/Truthy).

In case of JavaScript, if we complain about the above, then why not  maintain the same energy for this:

```javascript
if (0) { /* stuff */ }
```

Why is it okay to substitute `0` (or `''`) for `false` but not `true` for `1`?

The `==` operator compares **truthiness** of values. i.e it makes a judgement based on a **property** of it's operands, and not the values themselves. Take a look:

```javascript
''   == false // true
0    == ''    // true
```

Note however that it's overloaded to act differently for numbers and strings (and this weird polymorphism is a fair critique, which we'll address).
If you want strict value comparison, use `==`'s sister `===` instead.

```javascript
''   === false // false
0    === ''    // false
```

Up next:

```java
[5, 4, 31].sort() // [ 31, 4, 5 ]
```

Now this, is questionable API design choice. I completely agree with that. However, it doesn't tie into the semantics of JavaScript as a language in any way. In the same spirit, one could pick bones with C++'s poorly named standard library entities. (`std::empty` instead of `std::is_empty`).

To do a number comparison-based sort, you'd have to call it like so:

```javascript
[5, 4, 31].sort((a, b) => a - b) // [4, 5, 31]
```



Just to be clear, I believe this is a completely fair and valid criticism, just not of the language, but it's standard library.

Following up with:

```javascript
Math.max() // -Infinity
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

What should `maxSoFar` be initialized with? What is that one value which is guaranteed to be less than anything in `args`? Why `-Infinity` of course! Note that we can't initialize it with `args[0]` here because the index may be out of bounds, when `args` is empty. Another approach could be:

```javascript
Math.max = (args...) => {
   if (args.length < 1) throw new Error("Expected at least 1 argument");
   // ...
}
```

This is a completely valid approach as well, just different from what JavaScript does. I hope given the perspective above, you can see why neither of them are better or worse than the other. Neither is void of reason, they just have different conventions.



Finally,

```javascript
[1, 2, 3] == [1, 2, 3]
```

This is the simplest, most trivial snippet. It's just heap allocation. The two arrays are different objects living at different places in memory. When you modify one, the other doesn't change. If JavaScript had operator overloading, one could perhaps override `==` for `Array.prototype` and achieve element-by-element comparison? Who knows.

Now to tackle the rest:

## JavaScript is slow.

Is it really? We'll discuss that.

But first, I don't like using the phrase *"<**Insert language**> is slow"*.
Languages are defined by their syntax + semantics + standards. It is impossible to quantify a language's speed because that depends on it's **implementation**. 

*"Python is slow!"*

This statement gives rise to two obvious questions:

1. Which implementation of Python? CPython? PyPy? Numba? RPython? MicroPython? Jython?
2. Slow compared to what? x86 Assembly? C++? Ruby? Chuck?

A better way to put it would be:

*"CPython is slow"*

The more context, the better:

*"CPython is slower than V8 JavaScript"*.

This statement is much more meaningful, we can now write programs, run benchmarks and make real comparisons. However, we must not forget that not all languages cover the same domains. Scripting languages have a lot more reason and leeway to be slower owing to the environments they are generally embedded in. C++ vs JavaScript is simply an apples to oranges comparison.

That said, is JavaScript *actually slow*?

Many tend to assume that JS must be slow since it is capable of exhibiting such highly dynamic behavior. 
However, some of the most vital JavaScript engines today, V8, JSC and SpiderMonkey are extremely performant, heavily optimizing beasts. They're JIT engines, the precise inner workings of which deserve a post of their own, but perhaps taking a look at numbers may help move the argument forward.

Taking a look at some benchmarks from the [computer language benchmark games](https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html) site, it would appear that Node.js (which embeds V8) is actually not bad at all, even beating some implementations of Java and Haskell and C++ at times!

Now it is true that benchmarks are nowhere close to real programs, but the JavaScript interpreter being a bottleneck in web-apps is not something you hear often. So I'd say

**"JavaScript is fast enough for what it needs to do"**

## JavaScript is untyped and unsafe.

I read this in someone's blog, and a supporting twitter thread and it's uuh... not true maybe? 

JavaScript is **dynamically checked** and **safe**. But I do not blame the people who make this claim at all. In fact, I believe the problem is with misnomers and poor definition of terms like "loosely typed", "dynamically typed" and "safe" outside of academia.

I'll take an excerpt out of Pierce's great book, [Types and programming languages](https://www.cis.upenn.edu/~bcpierce/tapl/):

>  Terms like "dynamically typed" are arguably misnomers and should probably be replaced by "dynamically checked," but the usage is standard.

**Dynamically checked:** Variables and symbols do **not** have types. The types are bound to **values** instead.

*...What?*

Let's try with an example. The number `100`, has a type of "number" at runtime. However in this statement:

```javascript
let x = 100;
```

The **symbol** "x" and in it's semantic  definition, does not have a type. But it's still not wrong to say *"the type of 'x' is `number`"*. Why? Because when `x` is evaluated by the runtime, as a **value**, it does have a type (which can be seen by `typeof x`). 

However the **name** "x" is void of any type. So we can do `x = 'xyz'` and still be good, law-abiding JavaScript programmers. 

The key is understanding the difference between the **name** ''x" which can be bound to any value, and the **value** "x", which always evaluates to whichever value the name is bound to at the time of evaluation.

And now, I back my wild claim, *"JavaScript is safe"*.

I sound like a total nutjob wo should be banned from touching a keyboard ever again, but bear with me! A very simple definition of the word **safe** in context of programming languages can be overly simplified to: 

*A language where every code snippet always is well-defined by the standard.* 

And this is indeed true of JavaScript. If you don't like that thought, pause for a second and try to come up with a JavaScript code snippet that is **undefined** under the ECMAScript spec. I bet you there isn't any. It may error out, or produce wonky output, but the behavior is **defined**, and will be the same for every standard-compliant JavaScript implementation.

In fact, C++ is **less safe** than JavaScript, despite being statically typed (which makes sense, because the two properties aren't always locked in pair). For example, the following C++ code does not have any defined behavior:

```cpp
#include <iostream>

union IntOrPtr {
    int integer;
    const char* ptr;
};

int main() {
  IntOrPtr u = { .ptr = "C++ gang" };  
  std::cout << u.integer << std::endl; // <--- Whoops!
  return 0;
}
```

Accessing the uninitialized field of a union is undefined behavior, and may have different behavior on different compilers, hardware, OS or even different runs of the same executable!

Relying on such behavior can be lethal for your program (and I bet you I've seen industry codebases that relies on such behavior!).

My personal opinion:

|                      | Safe                                                         | Unsafe                                                       |
| -------------------- | ------------------------------------------------------------ | ------------------------------------------------------------ |
| **Dynamic checking** | Very easy to write erroneous programs, but much easier to debug due to well defined runtime behavior and inspection capability. | Don't know of any such languages except hobby projects. Let me know if you have some in mind! |
| **Static checking**  | Best of both worlds. Most languages in this bucket are functional. | Easier to write correct programs, but much harder to debug as there are no types and bounds at runtime. |

IMO, there is always a tradeoff. If you miss having compile time type checking, maybe checkout Typescript/Purescript? I'm a huge fan of both.

So what did we learn? Instead of saying:

*"JavaScript is unsafe"*

Say:

*"JavaScript is unsafe... for your mental health"*

##### JavaScript is single threaded = bad.

To be honest, I wish things were different too. But thankfully V8 (and ~~NodeJS~~ libuv) are designed well enough that this problem is easily circumvented with asynchronous code. JavaScript's async support is commendable. This is one of the few design choices that I actually fully support.

Threading is a bit of a gray area, some people would argue it's a part of the language, some would say it's a part of the implementation. I fall in the latter camp. 

Besides, for all of it's currently popular use-cases, JavaScript's threading facility (or the lack thereof) is circumvented to a small extent by `async` code and sometimes even available because of good design choices of the embedding application.

That said, this critique has the most substance behind it so far (IMO).

#### You sicken me! JavaScript fanboy

Virgilio shared his wisdom, and the flames of wrath abandoned Dante's mind. He learned to reason with a deeper conscience. 

*"I understand now, Virgilio."* said Dante.

*"The devil is not as black as he is painted"* he continued. *"JavaScript is indeed a blessing from the highest sphere of Paradise!"* filled with joy and hope, he started debugging his application again.

*"I am not finished, Dante."* Virgilio stopped him at once.
*"There are reasons as to why JavaScript resides in the 5th circle, the land of wrathful and the sullen."*

#### Not all roses.

Yes, I did defend JavaScript quite a bit. But I still do have my own fair share of complaints. Let's see.

1. Starting off on an easier foot, the `var` keyword. ECMA's unwillingness to forsake backwards compatibility has led to some very annoying legacy cruft in JS that just won't go away. **function scope** is one of those.

2. **Too much implicit type-coercion**. Yes, I did defend mis-categorization of JavaScript's typed-ness earlier, but I am still of the opinion that the JS type system is absolutely borked, and beyond saving. Some of these rules make no sense. While `true == 1` isn't that whack, this certainly is:

   ```javascript
   ![] == [] // true
   ```

   That's just one of the infinitely many examples that I wish JS got right. It's one of those things that makes up for 50% of why JavaScript is popularly hated.

3.  **Wonky whitespace sensitivity.** 
    JavaScript looks like it's not white-space sensitive at first     glance. Catch this:
    ```javascript
    a = 124 // ok
    b = 100 // ok
    c = 124 d = 214 // NOT OK
    ```
    That's right, it isn't really white-space sensitive at all. I wish the language had simpler rules regarding this, but that's unfortunately not the case. So we rely on linters to warn us. (This ties into the next point).

4. **Automatic semi-colon insertion**. What you're seeing above is really an effect of [this](https://262.ecma-international.org/7.0/#sec-rules-of-automatic-semicolon-insertion) annoying ECMA ruleset. So behold: 

   ```javascript
   function ten () {
       return
       	10
   }
   console.log(ten()) // undefined
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

6. **Unused reserved words**. `enum`, `private`, `public` are reserved words that you cannot use. But they don't bring any features to the language. ECMA just *might* one day add private members or enums and so they reserved these words. I see this as a good strategy, but why not do one thing? If you have contextual keywords, then make these keywords contextual too instead of "pre-reserving" them and making other future keywords contextual. That said, I see this as a reasonable approach. Although I don't quite understand why implementing enums is taking so long :p

7. **Hoisting**. Not a big deal, but not WSYIWYG. I like my scripting languages WSYIWYG, thank you.



There's some other smaller problems like no native integers etc. But I can live with those. In contrast, I *have* to live with the above.

## What does the future hold?

Pain and suffering. Because JS will always have to be backwards compatible.

But also some nice stuff, because the newer additions to JavaScript have been fairly nice! Besides, we are still in the early dawn of programming languages. I don't know if there will ever be a language that is both popularly used and popularly liked.


> *"There are only two kinds of languages: the ones people complain about and the ones nobody uses."*
> **<div style ="text-align: right">- Bjarne Stroustrup </div>**

## FAQ

- **So do you like JS or not?** No comments.
- **Why did you feel the need to write this blog post?** I have time, and no job. Go figure.
- **Why is there an FAQ section when clearly nobody is reading this ever?** Rude :(
- **Show me more wonky JavaScript.** Go to [wtfjs](https://wtfjs.com/) and knock yourself out.
- **JavaScript done right?** I have searched for ages, but couldn't find anything better than PureScript or Dart (when transpiled). it is extremely difficult for scripting languages to enter browsers, unless the transpile to the unholy devilspawn.
- **Why are there references to Dante's inferno?** That is the closest entertaining piece of literature in my recent memory and I don't want a completely bland wall of text.