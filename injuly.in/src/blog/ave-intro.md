---
template: "post" 
tags: ["ave", "programming-languages", "type-system"]
title: "Ave - JS with static types"
date: "2020-10-01"
meta: "A statically typed language that compiles to JavaScript"
is_blog_post: true
---

This is the first in a series of blog posts to come about how I made a statically type checked, scripting
language that compiles to Javascript. The intent is to offer python/coffeescript's minimal syntax
with Typescript's type system. All compiling down to ES6 Javascript.

## The problem with plain Javascript.

Lets first explore _why_ the world needs another programming language when there are already so many.
Here is something funny, copy paste the following code snippet in your browser's console (`F12`) and
hit enter.

```js
const fruit = 'b' + 'a' + +'a' + 'a';
console.log(fruit.toLowerCase());
```

Surprised?
Probably not if you're used to Javascript. But many new users are, and this is just one of the many annoying eccentricities
of Javascript's weird, implicit coercion loving "type system".

In case you didn't bother with running it, it spits out, `'banana'` even though there is no `'n'` in the expression on line 1.
How? Try `console.log(+'a')` and you'll have a clue.

Notice the unary `+` before the 3rd `'a'`? Welp, turns out Javascript doesn't complain when you use any of it's operators on
strings but will croak and return a `NaN` at runtime.

Since addition associates to the left, it roughly evaluates like this:

```js
'b' + 'a' -> 'ba'
'ba' + (+ 'a') -> 'ba' + NaN -> 'baNaN' // NaN coerces to string.
'baNaN' + 'a' -> 'baNaNa'
```

and finally, `.toLowerCase()` turns "baNaNa" into "banana". This is one of the many problems with Javascript,
and unfortunately, the other bugs you will encouter won't be as fun to explore. Moreover, it's much harder to get your code to
follow a proper schema/structure when writing Javascript. Which is why tools like Typescript and Flow exist to address this, and many other
problems. So I took my own shot at this, and here we are.

## Introducing Ave.

Yeah I couldn't think of a name that wasn't already taken, so I went with 'Ave'. The word means 'a warm greeting' (I think?).
If you have a better name in mind, let me know.

Anyway, this is what it looks like:

```py
# Variable declaration
myNumber := 1
myString := 'this is a string'

myString := 123 # TypeError: Cannot assign value of type 'num' to 'str'.

# Type annotations
name: str = 'Hello World !'
name += [1, 2, 3]
# TypeError: Cannot use operator '+=' on values of type 'str' and 'Array<num>'
```

Don't like the way variable declaration looks ? You can do it the JS/TS way too !

```ts
let a: num = 123;
let b = true;
a += b;
```

This won't compile. Because You're trying to add a boolean to a number.
Plain JS would accept this and `a` would be `124`. Now let's see what basic control flow looks like.

```lua
for i = 1, 10
  if i % 2 console.log('i is odd')
  else console.log('i is even !')
```

Yeah, the syntax is whitespace sensitive like Python. I know that's a turn off for many, but I'd
rather have it this way in my own language. Moving on,
Did I mention record data types ?

```hs
record Person
  name: str
  age : num

let bob: Person = {
  name: "Bob",
  age: 12
}
```

Kind of like structs in C or interfaces in Typescript.
We also have generics.
They can be recursive too !

```hs
record LLNode<E>
  value: E
  next: LLNode<E> | nil

let head: LLNode<num> =
  value: 1
  next:
    value: 23
    next: nil
```

Notice how you can create objects even without the `{}` ? I personally prefer braces around them,
but at least the user has that choice.

And functions:

```go
func fib(n: num): num
  if n == 0 or n == 1
    return 1
  return fib(n - 1) + fib(n - 2)

fib10 := fib(10)
```

Obviously there is a lot more to the language that has been implemented so far,
and a lot that is still on the to-do list (classes, inheritance, import/exports). But hopefully
this gave you a "feel" for the language.

Finally, if you want to take a look at the source code, it lives [here](https://github.com/srijan-paul/AveTS) on github.
The docs and README aren't exactly polished, but the code is well organized for the most part.

In the next post I'll explain how I implemented the type system, and hopefully you end up taking something away from it.
