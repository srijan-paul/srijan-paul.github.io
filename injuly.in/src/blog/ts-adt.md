---
template: "post" 
tags: ["FP", "TypeScript"]
title: "Functional TypeScript #1: Algebraic Data Types"
meta: Introduction to typeclasses, ADTs, and HKTs TypeScript
is_blog_post: true
date: 2024-05-20
---

I write TypeScript and Go at my job.
I've written close to ~100k lines of TS and ~30k lines of Go in some four odd years.
In both these languages, I often reach for patterns that I had originally learned while using Haskell.
Some of these tools deserve more publicity, so I'm writing a series of articles about them.

In later entries, I will cover:

  1. Higher Kinded Types.
  2. Type classes (called "traits" in Rust).
  3. Generalized Algebraic Data Types.

As an introduction, we'll pick a simpler concept – [ADTs](https://en.wikipedia.org/wiki/Algebraic_data_type).

In functional programming, algebraic types are so ubiquitous that you would be hard pressed
to find a language that doesn't support them.
Strangely though, nothing about them demands that a language be functional by paradigm.
That imperative languages have avoided ADTs for decades seems almost an accident.

Fortunately, TypeScript *almost* supports them as a first class feature.

## Motivation

You have inherited a React SPA behemoth at your new job.
For your first Jira ticket, you will track every move the user makes and send it to a telemetry client.

The task is simple, model three event types – key press, mouse click, and scroll;
then write a function that serializes any event of these events to a string.
Each event type might have its own unique meta data, like the
coordinates for a mouse click, or the key code for a keyboard event.
You have to include this information in the serialized string.

This is a very simple exercise, try it before reading ahead:

```ts
type UiEvent = ...
const serialize = (e: UiEvent): string => { ... }
```

## ADTs in Haskell 

We'll start with a reference implementation in Haskell.
There isn't much to say, the encoding for this flows almost from muscle memory:

```hs
data UiEvent 
  = Click (Float, Float)  -- (x, y)
  | KeyPress Float        -- key code 
  | Scroll

serialize :: UiEvent -> String
serialize e = case e of 
  Click (x, y)  -> "Clicked: " ++ show x ++ " " ++ show y
  Keypress code -> "Pressed: " ++ show code
  Scroll        -> "Mouse scroll"
```

## ADTs in TypeScript

> *Behold Perry the Platypus, my latest invention – the Discriminator!*

TypeScript has something called a *[discriminator](https://www.typescriptlang.org/docs/handbook/2/narrowing.html#discriminated-unions)* for union types.
Technically, this is a consequence of another feature called [type narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html).

Together, these make ADTs almost natural.
First, we declare a higher order type that represents a tagged value:

```ts
type Tagged<T, V = undefined>
  = V extends undefined
      ? { tag: T }
      : { tag: T, value: V }
```

You can read the type declaration like this:

`Tagged` takes two types as arguments—`T` and `V`—and returns a new type.
When `V` is [the undefined type](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#null-and-undefined), the returned type is `{ tag: T }`.
For any other `V`, the type `{ tag: T, value: V }` is generated instead.

With the `Tagged` helper, we can define an ADT:

```ts
type Keypress = Tagged<'KeyPress', { keyCode: number }>
type Click = Tagged<'Click', { x: number, y: number }>
type Scroll = Tagged<'Scroll'>

type UiEvent = Click | Keypress | Scroll

function serialize(e: UiEvent): string {
  switch (e.tag) {
    case "KeyPress": return `Pressed: ${e.keyCode}` 
    case "Click":    return `Clicked: ${e.x} ${e.y}`
    case "Scroll":   return "Scroll"
  }
}
```

One might be compelled to point out some shortcomings of this approach:

1. The discriminators are identifiers in Haskell, but strings in TypeScript.
3. Every value in the TS version contains a heap allocated string tag.
2. Values are more verbose – `{tag: "Scroll"}` vs just `Scroll`.

In a real programming environment, most of these won't matter.
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
  | Tagged<EventType.keyPress, { keyCode: number }>
  | Tagged<EventType.scroll }

```

The members of a `const` enum assume numeric values,
and get compiled down to number literals in the generated JavaScript code:

```ts
// in TS:
{ tag:  EventType.click }

// After compilation to JS:
{ tag: 0 }
```

Finally, I'll concede that you may not need a `Tagged` helper for a union of just three types.
But you'll find that a real application naturally evolves into several patterns that are easily 
modelled by ADTs.
In fact, the example in this article is an simplified version of an SPA I maintain at my job.

In the next article, we will use ADTs again to demonstrate type classes and HKTs.

