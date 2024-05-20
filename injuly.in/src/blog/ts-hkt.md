---
template: "post" 
tags: ["FP", "TypeScript"]
title: "Advanced TypeScript #1: Algebraic Data Types"
meta: Introduction to typeclasses, ADTs, and HKTs TypeScript
is_blog_post: false
date: 2024-05-20
---

I use Go and TypeScript everyday at work.
In the last four years, I have written close to 100k lines of TypeScript and 30k lines of Go.
With both of them, I often reach for patterns that would be obvious to a programmer more proficient in
, say, Haskell, OCaml, or F#.

This is the first in a series of articles about functional tools that help curb the complexity
in large TypeScript codebases.
In later entries, I will cover:

  1. Higher Kinded Types.
  2. Type classes (called "traits" in Rust).
  3. Generalized Algebraic Data Types.

We'll start ADTs – a simple concept for newcomers to both TypeScript and FP.

In functional programming, algebraic types are so ubiquitous that you would be hard pressed
to find a language that doesn't support them.
Strangely, nothing about this feature demands that a language be functional.
That imperative languages have avoided ADTs for decades seems almost an accident.

## Motivation

You have inherited a React dashboard behemoth at your new job.
For your first Jira ticket, you will track every move the user makes and send it to a telemetry client.

The task is simple, model three kinds of events – a key press, mouse click, or scroll;
then write a function that serializes any event to a string.
Each event type might have its own unique meta data,
like mouse coordinates for a click event, or the key code for a keyboard event.
You have to include this information in the serialized string.

This is a very simple exercise, try it before reading ahead:

```ts
type UiEvent = ...
const serialize = (e: UiEvent): string => { ... }
```

## ADTs in Haskell 

A Haskell encoding of this flows almost from muscle memory:

```hs
data UiEvent 
  = Click (Float, Float)  -- (x, y)
  | KeyPress Float -- key code 
  | Scroll

serialize :: UiEvent -> String
serialize e = case e of 
  Click (x, y)  -> "Clicked: " ++ show x ++ " " ++ show y
  Keypress code -> "Pressed: " ++ show code
  Scroll        -> "Mouse scroll"
```

The first line declares a data type with three possible *tags*.
Each tag—called a data constructor in Haskell—uniquely defines the shape of an event.
For example, the `Click` tag implies the existence of a pair of floats.

The `serialize` function can inspect the shape of a `UiEvent`, and carry out different control
flow for each branch.
This is fairly straightforward.

## ADTs in TypeScript

> *Behold Perry the Platypus, my latest invention – the Discriminator!*

TypeScript has something called a *[discriminator](https://www.typescriptlang.org/docs/handbook/2/narrowing.html#discriminated-unions)* for union types.
Technically, this is a consequence of another feature called [type narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html).

Together, these make ADTs almost natural.
First, We declare a helper higher order type called `Tagged` that represents a named value:

```ts
type Tagged<T, V = undefined>
	= V extends undefined
	? { tag: T }
	: { tag: T, value: V }
```


You can read the type declaration like this:

`Tagged` takes two types as arguments—`T` and `V`—and returns a new one.
When `V` is undefined, the returned type is `{ tag: T }`.
For any other `V`, the type `{ tag: T, value: V }` is generated instead.

Note that the default value for `V` is the undefined *type*, not to be confused with a term level `undefined`.
This type only has one member – the `undefined` value.

With the `Tagged` helper, we can model an ADT:

```ts
type Keypress = Tagged<'KeyPress', { keyCode: number }>
type Click = Tagged<'Click', { x: number, y: number }>
type Scroll = Tagged<'Scroll'>

type UiEvent = Click | Keypress | Scroll
```

The implementation for `serialize` follows naturally from the type declaration:

```ts
function serialize(e: UiEvent): string {
  switch (e.tag) {
    case "KeyPress": return `Pressed: ${e.keyCode}` 
    case "Click":    return `Clicked: ${e.x} ${e.y}`
    case "Scroll":   return "Scroll"
  }
}
```

At this point, you might be compelled to point out some shortcomings of this approach:

1. The discriminators are identifiers in Haskell, but strings in TypeScript.
3. Every value in the TS version contains a heap allocated string tag.
2. Values are more verbose – `{tag: "Scroll"}` vs just `Scroll`.

In any real programming environment, most of these won't matter.
IDEs will auto-complete the type tags as soon as you type in the quotes,
and JS runtimes will apply [string interning](https://en.wikipedia.org/wiki/String_interning) optimizations to 
de-duplicate the tags, making them all point to the same heap address.

Having to type `{ tag: ..., value: ... }` is indeed worse ergonomically,
but there is no good workaround for this, except perhaps a function like 
`tagged("Pressed", {...})`.

If the string tags still disgust you, consider an enum:

```ts
const enum EventType { click, keyPress, scroll }

type Event =
  | Tagged<EventType.click, { x: number, y: number }>
  | Tagged<EventType.keyPress,  { keyCode: number }>
  | Tagged<EventType.scroll }

```

Thanks again to type literals, we can use enum members as types.
The members of a `const` enum assume numeric values,
and get compiled down to number literals in the generated JavaScript code:

```ts
// in TS:
{ tag:  EventType.click }

// After compilation to JS:
{ tag: 0 }
```

I'll concede that you may not need a `Tagged` helper for a union of a mere three types.
However,you'll find that a real application has patterns that fall into tagged unions in several places.
Seeing a `Tagged<A, ..> | Tagged<B, ..>` makes the discriminator immediately obvious.

More importantly, this was a simpler 

