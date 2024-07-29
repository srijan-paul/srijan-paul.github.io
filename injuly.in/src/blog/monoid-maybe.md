---
template: post
tags: ["haskell", "FP"]
title: Data.Maybe, and thoughts on library design
date: "2024-07-29"
meta: "No program comes out right the first time. Libraries aren't an exception, and shouldn't be treated as such"
is_blog_post: true
---

Earlier today, I was reading [a paper about monoids](https://core.ac.uk/download/pdf/76383233.pdf)
that briefly highlighted an interesting flaw in older versions of Haskell's standard library.
The problem—although fixed later—still serves as an interesting case study for library design.
So I took a break from the study session to log my thoughts here before I forget. 

## Semigroups and Monoids

In Haskell, a Monoid is any type `a` that has:

1. An identity element called `mempty`, like `0` for `Int`.
2. A function `mappend` (or `<>`) that combines two `a`s, e.g: `+`.

In the base library, `Monoid` is defined roughly like so:

```haskell
class Monoid a where
  mempty :: a
  (<>)   :: a -> a -> a

-- E.g: All lists are monoidal under concatenation
instance Monoid (List a) where
  mempty = []
  a <> b = (++)
```

Alongside this, we also have the `Semigroup` typeclass that
represents any type with an `mappend` function,
but not necessarily an `mempty`.
Naturally, its definition is the same as `Monoid`'s minus the `mempty :: a`:

```haskell
class Semigroup a where
  mappend :: a -> a -> a
```

Any Monoid is also a Semigroup by virtue of having an `mappend`.
But for a Semigroup to be a Monoid, it has to have an identity element.

## Maybe as a Monoid

Any `Semigroup a` can be turned into a `Monoid (Maybe a)` by declaring `Nothing` as the identity element.
This way, we can "lift" a type that forms a semigroup under some operation, into an optional type
that forms a monoid under the same operation.

The definition of `Monoid (Maybe a)` in the standard library, however, was:

```haskell
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing 

  Nothing <> y = y
  x <> Nothing = x
  (Just x) <> (Just y) = x <> y
```

This completely overlooks the existence of semigroups,
mostly because they didn't exist in `base` until much later.

To understand the problem, take a closer look at the constraint:

```haskell
instance Monoid a => Monoid (Maybe a)
--       ^^^^^^^^ 
```
For `Maybe a` to be a monoid, `a` must be a monoid first.
But in the instance body, we only ever use `<>` to combine `x` and `y`,
so a `Semigroup a => ...` constraint would've made more sense:

```diff
+ instance Semigroup a => Monoid (Maybe a) where
- instance Monoid a => Monoid (Maybe a) where
```

This fix was finally made in [base-4.11](https://hackage.haskell.org/package/base-4.11.1.0/changelog).

Before the `Data.Semigroup` module was added to base,
programmers would use the [semigroups](https://hackage.haskell.org/package/semigroups-0.18.1/docs/Data-Semigroup.html) package
and its `Option` data type, which is described as:

> `Option` is effectively `Maybe` with a better instance of `Monoid`,
> built off of an underlying `Semigroup` instead of an underlying `Monoid`.
> Ideally, this type would not exist at all and we would just fix the `Monoid` instance of `Maybe`

```haskell
newtype Option a = Option { getOption :: Maybe a }
instance Semigroup a => Monoid (Option a) where
  -- ...
```

`getOption` is a zero-cost abstraction, since `Option` is a simple `newtype` wrapper.
Fortunately, this workaround is no longer necessary.

## Designing libraries for the general case

Libraries that cover more ground are also harder to design.

Imagine you're writing a parsing library.
What kind of input is considered valid for your parser?
Is it only `String`? What about `Text` and `ByteString`?
Perhaps it should be `IsString a => a`, then?
But what if the data is chunked, or in some binary format?

The megaparsec library uses a [Stream](https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec-Stream.html#t:Stream)
typeclass to represent all "parse-able" inputs, and has a note about moving away
from an older design decision in future versions.

I like the way Bartosz Milewski says it in his book:

> A good library writer should be able to identify the bare minimum of
> constraints that make the library work.

Identifying the limits of a library's use case,
then fitting them to an API is *incredibly hard*.
And yet, when improvements are made in future versions or succeeding libraries,
the fix seems obvious in hindsight.
Even the experts don't get it right on their first try.

The Haskell prelude still has partials like `head`,
JavaScript's date API was broken for decades until [temporal](https://tc39.es/proposal-temporal/docs/) turned things around,
Python has an [entire proposal](https://peps.python.org/pep-0594/) to remove "dead batteries"
from the standard library,
and Xorg's API and security issues still spark debate in Linux forums. 

Libraries evolve just as any other software project,
and when a soft deprecation isn't enough,
breaking changes clear the slate.

Building atop APIs that exist purely for legacy reasons can often lead to
a kind of bloat that hurts a project's adoption and ergonomics.
The C++ standard is—in my personal opinion—a victim of this style of maintenance.

I don't have a bigger picture to paint, so I'll conclude this post rather abruptly.
Now if you'll excuse me, I should return to my study.

