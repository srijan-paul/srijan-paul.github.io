---
template: post
is_blog_post: true
tags:
  - haskell
  - FP
date: 2024-05-09
title: Compiling higher order functions with GADTs.
meta: De-functionalization is a method of converting a program with higher order functions to one that uses regular functions.
---


Implementing first class functions in a bytecode interpreter is trivial.

But how do compilers that generate machine code (or lower to C, or SSA) implement higher order functions?
Back in 2021, I found an answer when contributing closures to the Pallene compiler.

Today I was researching something loosely related, and learned about yet another neat trick called *defunctionalization* in [this paper](https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf).

Defunctionalization is a *transform* – a way to re-write the original program without using higher order functions so that it can be trivially compiled to a flat IR in subsequent compiler passes.

The paper uses an OCaml example, and I'll be porting the same program to Haskell.
Our implementation will assume that the language being compiled supports GADTs, though its certainly possible to defunctionalize without them.
##  Rewriting programs to remove higher order functions

Consider the following snippet:

```hs
import Prelude hiding (fold)

fold :: (a -> b -> b) -> b -> [a] -> b
fold _ b [] = b
fold f b (x : xs) = f x (fold f b xs)

sum xs = fold (+) 0 xs
add n xs = fold (\x l' -> (x + n) : l') [] xs
```

We have a simple implementation of `fold`, and two functions that call it.
`sum` adds up all items in a list, and `add n xs`  adds `n` to each item.
In `sum`'s case we pass the "+" operator to `fold`, which has type `Int -> Int -> Int`.
Inside `add`, the lambda passed to `fold` has type `Int -> [Int] -> [Int]`.

To remove the lambdas passed by both functions, we first introduce a GADT called "arrow" in the original program:

```hs
data Arrow p r where
	FnPlus :: Arrow (Int, Int) Int
	FnPlusCons :: Int -> Arrow (Int, [Int]) Int
```

`Arrow p r` represents a higher order function that takes parameters of type `p` and has a return type `r`.
When there are multiple parameters, `p` becomes a tuple, as seen in both cases.

Arrow has two data constructors – one for every unique HOF seen in the source program.

When invoked, the `FnPlus` data constructor will create a value of type `Arrow (Int, Int) Int` to represent the "+" operator.
The `FnPlusCons` data constructor takes an argument of type `Int` which represents the "n" that is captured by the lambda in `add`:

```hs
(\x l' -> (x + n):l') -- notice the free variable "n"
```


So we've settled on representing functions with `Arrow`, but how do we call such a function?
Simple, we write another function for that:

```hs
apply :: Arrow p r -> p -> r
apply FnPlus (x, y) -> x + y
apply (FnPlusCons n) (x, xs) = (n + x):xs
```

The helper `apply` knows how to map a data constructor to its corresponding function body.
So, whenever the source program refers to `(+)` by value, we replace it with `FnPlus`.
For example:

```hs
-- Before:
let add = (+)
	five = add 3 2

-- After:
let add = FnPlus
	five = apply FnPlus 3 2
```

At the end of the day, the original program is transformed to look like this:


```hs
{-# LANGUAGE GADTs #-}

import Prelude hiding (fold)

data Arrow p r where
  FnPlus :: Arrow (Int, Int) Int
  FnPlusCons :: Int -> Arrow (Int, [Int]) [Int]

apply :: Arrow a b -> a -> b
apply FnPlus (x, y) = x + y
apply (FnPlusCons n) (x, xs) = (n + x) : xs

fold :: Arrow (a, b) b -> b ->[a] -> b
fold fun b [] = b
fold fun b (x : xs) = apply fun (x, fold' fun b xs)

sum = fold' FnPlus 0
add n = fold' (FnPlusCons n) []
```

Notice how there are no higher order functions involved in this new program, even though the logic is identical.

## Mutable captures

Defunctionalization can transform first class functions and immutable variable captures.
Many languages allow mutable captures from surrounding scopes:

```ts
function counter(x: number) {
	return function() {
		x++;
		return x;
	}
}

const f = counter(0);
f(); // 1
f(); // 2
```

In such cases, defunctionalization must be preceded by [lambda lifting](https://en.wikipedia.org/wiki/Lambda_lifting)
Since that isn't the focus of this post, I'll  just a leave a link to [my PR](https://github.com/pallene-lang/pallene/pull/402) that adds support for mutable captures to the Pallene language.

## References
1. [Lightweight higher-kinded polymorphism](https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf) – Section 1.2.
2. [Definitional interpreters for higher-order programming languages](https://surface.syr.edu/cgi/viewcontent.cgi?article=1012&context=lcsmith_other)
3. [Implementation of closures in Pallene](https://injuly.in/blog/gsoc-2/)