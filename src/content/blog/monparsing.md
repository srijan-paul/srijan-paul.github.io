A [Parser combinator](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf), as wikipedia describes it,
is a _higher-order function that accepts several parsers as input and returns a new parser as its output_.

They can be very powerful when you want to build modular parsers and leave them open for further extension.
But it can be tricky to get the error reporting right when using a 3rd party combinator library,
and they tend to be slower in imperative languages.
Nonetheless, it is an interesting cornerstone in both functional programming and PLT, so it shouldn't
hurt for us to learn about them by building one on our own.

To keep you from dozing off, this will be a 2-part series.

In this first part, we're going to write a library that describes several tiny parsers
which will allow us to write one-liners capable of accepting strings
belonging to a [regular language](https://en.wikipedia.org/wiki/Regular_language).
In other words, we're building a more verbose version of a regex library :^).
Here is an example of a parser that matches C style identifiers:

```hs
-- matches strings that satisfy [a-zA-Z][a-zA-Z0-9]+
parseId :: Parser String
-- One letter or '_', followed by zero of more '_', letters or digits
parseId = alpha_ `then_` many (alpha_ `or` digit)
  where alpha_ = letter `or` char '_'
```

Before reading any further, It is recommended for you to have some basic understanding of:

- Parsers.
- Monads and fundamentals of Haskell.

## The Parser type

There are several ways to represent a Parser.

A parser takes a string and produces an output that can be just about anything.
A list parser will produce a list as it's output,
an integer parser will produce `Int`s,
A JSON parser data might return a custom [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) representing a JSON.

Therefore, it makes sense to make Parser a polymorphic type.
It also makes sense to return a list of results instead of a single result,
since grammars can be ambigious and there may be several ways to parse the same input string.

An empty list, then implies the parser failed to parse the provided input.

{TODO@injuly : add a bullet point to the back matter explaining alternative type definitions}.

```hs
newtype Parser a = Parser { parse :: String -> [(a, String)] }
```

You might be wondering why we return the tuple `(a, String)`, and not just `a`.
Well, a parser might not be able to parse the entire input string.
Often a parser is only intended to parse some prefix of the input, and let another parser do the rest of the parsing.
Thus, we return a pair containing the parse result `a`, and the unconsumed string that can be used by subsequent parsers.

We could have used the `type` keyword let `Parser` be an alias for `String -> [(a, String)]`,
but having a unique data type has the advantage that it can be turned into a typeclass instance, which we'll do later on.

## Baby parsers

We can start by describing some basic parsers that do very little work.
A `result` parser always returns the same value and doesn't touch the input string.
{TODO@injuly: backmatter explaining how this can be written in a pointfree style using TupleSections}

```hs
result :: Parser a
result val = Parser $ \inp -> [(val, inp)]
```

The parser `null` will always fail by returning an empty list.

```hs
null :: Parser a
null = Parser $ const []
```

`item` unconditionally accepts the first character of an input string.

```hs
item :: Parser Char
item = Parser $ parseItem
  where
    parseItem [] = []
    parseItem (x:xs) = [(x, xs)]
```

## Building parsers on demand

The basic parsers we defined above are of very little use.
Ideally, we would want parsers that accept input strings which satisfy certain constraints.
Say we want a parser that consumes a string if its first character satisfies a predicate.
We can generalize this idea by writing a function that takes a `(Char -> Bool)` predicate and
returns a parser that only consumes an input string
if its first character when supplied to the predicate returns `True`.

The simplest solution for this is simply:
```hs
sat :: (Char -> Bool) -> Parser Char
sat p (x:xs) = if p x
  then [(x, xs)]
  else []
sat _ [] = []
```

However, since we already have an `item` parser that unconditionally extracts
the first character from a string, we could use this as an opportunity create a basic parser combinator.

Before writing a combinator, we must first instantiate `Parser` as a `Monad`.
{TODO@injuly}: shortlinks that instantiate `Parser` from `Applicative, Functor`.
```hs
instance Monad Parser where
  -- (>>=) : Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \inp ->
    concat [parse (f v) inp' | (v, inp') <- parse p inp]
  -- a -> Parser a
  return = result
```

The `bind` operation takes a `Parser a` (p) and a function `a -> Parser b` (f), and returns a `Parser b`.
The idea is to apply `p`, if it fails then we have an empty list which results in `concat [[]]` = `[]`.
If `p` successfully parses `inp` into one more possible parse results,
we apply `f` to each of the results to get corresponding `Parser b`s and then apply those to the rest of the input.

With this new extension, our `sat` parser can be re-written as:

```hs
sat p =
-- 1. Apply `item`, if it fails on an empty string, we simply short circuit and get `[]`.
  item >>= \x -> 
    if p x
      then result x
      else null 
```

Now we can use the `sat` combinator to describe several useful parsers.
For example, a `char` parser that only consumes a string beginning with a specific character.

```hs
char :: Char -> Parser Char
char x  = sat (== x)
```

A parser for decimal digits:

```hs
-- import Data.Char (isDigit, isLower, isUpper)
digit :: Parser Char
digit = sat isDigit
```

And similarly small but useful parsers:

```hs
lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper
```

Now that we have `upper`, `lower` and `digit` this opens up new possibilities for combinations:
- An `alphabet` parser that accepts a char that is consumable by either `upper` or `lower`.
- An `alphanumeric` parser that accepts a char which is either `alphabet` or `digit`.

An `or` combinator that captures this recurring pattern can come in handy.

Let us begin by describing a `plus` combinator that concatenates the result returned by two parsers:
```hs
-- Applies two parsers to the same input, then returns a list
-- containing results returned by both of them.
plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ \inp -> parse p inp ++ parse q inp
```

Since `Parser` is already a monad, we can instantiate `MonadPlus` typeclass to enforce
this idea:
```hs
instance MonadPlus Parser where
  mzero = zero
  mplus = plus
```

{TODO@injuly: describe `alphabet` etc here}

The `or` combinator can then be simply:

```hs
or :: Parser a -> Parser a -> Parser a
p `or` q = Parser $ \inp -> case parse (p `plus` q) inp of
    [] -> []
    (x : xs) -> [x]
```

In fact, the `Alternative` typeclass already defines this functionality.

```hs
instance Alternative Parser where
  empty = zero

  p <|> q = Parser $ \inp -> case parse (p `mplus` q) inp of
    [] -> []
    (x : xs) -> [x]
```

Finally, we can returned

```hs
alphanum :: Parser Char
alphanum = letter <|> digit
```