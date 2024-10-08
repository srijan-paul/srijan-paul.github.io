---
template: post
is_blog_post: true
tags:
  - haskell
  - FP
  - compilers
date: 2024-05-09
title: Compiling higher order functions with GADTs
meta: Compile higher order functions using the defunctionalization transform. Compiler authors HATE this one weird trick!
---

Implementing first class functions in a bytecode interpreter is trivial.

But how do compilers that generate machine code (or lower to C, or SSA) implement higher order functions?
Back in 2021, I found an answer when contributing closures to the Pallene compiler.

Today I was researching something loosely related, and found yet another neat trick called *defunctionalization* in [this paper](https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf).

Defunctionalization is a *transform* – a way to re-write the original program without using higher order functions such that it can be trivially compiled to a flat IR in subsequent compiler passes.

The paper uses an OCaml example, and I'll be porting the same program to Haskell.
Our implementation will assume that the language being compiled supports GADTs, though it's certainly possible to defunctionalize without them.

## Rewriting programs to remove higher order functions

Consider the following snippet:

```hs
import Prelude hiding (fold)

fold :: (a -> b -> b) -> b -> [a] -> b
fold _ b [] = b
fold f b (x : xs) = f x (fold f b xs)

sum xs = fold (+) 0 xs
add n xs = fold (\x l' -> (x + n) : l') [] xs
```

We have a simple implementation of `fold`, and two functions that use it.
`sum` adds up all items in a list, and `add` increments every item in a list by `n`.
In `sum`, we pass the "+" operator to `fold`, which has type `Int -> Int -> Int`.
While in `add`, the the folding function has type `Int -> [Int] -> [Int]`.

To remove the lambdas passed by both functions, we first introduce a GADT called "arrow" in the original program:

```hs
data Arrow p r where
	FnPlus :: Arrow (Int, Int) Int
	FnPlusCons :: Int -> Arrow (Int, [Int]) Int
```

`Arrow p r` represents a higher order function that takes parameters of type `p` and has a return type `r`.
When there are multiple parameters, `p` becomes a tuple, as seen in both cases.

Arrow has two data constructors – one for each function in the source program that is passed around as a value,
like both the lambdas passed to fold.

When invoked, the `FnPlus` data constructor will create a value of type `Arrow (Int, Int) Int` to represent the "+" operator.
Since `(+)` does not capture any variables, `FnPlus` needs no arguments.
By contrast, the `FnPlusCons` data constructor takes an `Int` argument, representing the captured binding "n":

```hs
(\x l' -> (x + n):l') -- "n" is a free variable
```

This function is represented with `FnPlusCons n`.

So we've settled on representing functions with a GADT, but how do we call a value like `FnPlus`?
We can use a helper function that pattern matches on the data constructors of `Arrow` to call the corresponding function body:

```hs
apply :: Arrow p r -> p -> r
apply FnPlus (x, y) -> x + y
apply (FnPlusCons n) (x, xs) = (n + x):xs
```

At this point, I'd like to gently remind you that both `Arrow` and `apply` are not a part of the original program,
but generated by the compiler when defunctionalizing.

So, whenever the source program refers to `(+)` by value, we replace it with `FnPlus`.
For example:

```hs
-- Before:
let add = (+)
    five = add 3 2

-- After:
let add = FnPlus
    five = apply add 3 2
```

Applying the same conversion to the original program, we get the following program that only uses top-level functions:

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

The logic is identical, and not one lambda in sight.

## Mutable captures

Defunctionalization can transform first class functions with immutable variable captures (as seen in `FnPlusCons`),
but many languages allow mutable captures from surrounding scopes:

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

In such cases, defunctionalization must be preceded by [lambda lifting](https://en.wikipedia.org/wiki/Lambda_lifting).
But that isn't the focus of this post, so I'll  just a leave a link to [my PR](https://github.com/pallene-lang/pallene/pull/402) that adds support for mutable captures to the Pallene language.

## References

If you want to read up some more on defunctionalization, here are some resources:

1. [Lightweight higher-kinded polymorphism](https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf) – Section 1.2.
2. [Definitional interpreters for higher-order programming languages](https://surface.syr.edu/cgi/viewcontent.cgi?article=1012&context=lcsmith_other)
3. [Implementation of closures in Pallene](https://injuly.in/blog/gsoc-2/)

