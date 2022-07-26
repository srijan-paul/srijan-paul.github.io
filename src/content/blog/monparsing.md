A [Parser combinator](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf), as wikipedia describes it,
is a _higher-order function that accepts several parsers as input and returns a new parser as its output_.

They can be very powerful when you want to build modular parsers and leave them open for further extension.
But it can be tricky to get the error reporting right when using a 3<sup>rd</sup> party combinator library, and they tend to be slower in imperative languages.
Nonetheless, it is an interesting cornerstone in functional programming and [PLT](https://en.wikipedia.org/wiki/Programming_language_theory), so it shouldn't hurt to learn about them by building one on our own.

We're going to start by writing a library that describes several tiny parsers and functions that operate on those parsers.
Then, we're going to build some parsers to demonstrate the usefulness of our work.

To give you a small flash forward, here is a parser that accepts C-style identifiers,
written with the help of our handy combinators:

```hs
-- matches strings that satisfy [a-zA-Z][a-zA-Z0-9]+
ident :: Parser String
-- One letter or '_', followed by zero of more '_', letters or digits
ident = alpha_ `thenList` many' (alpha_ <|> digit)
  where
    alpha_ = letter <|> char '_'
```

It is recommended for you to have some basic understanding of:

- Parsers
- Monads in functional programming
- Haskell

This post is a derivative of two papers I had read recently ([1](https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf), [2](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf)).
If you like a denser reading, you can go through the papers instead.
The full code for this blog can be found [here on GitHub](https://gist.github.com/srijan-paul/87abfeacb84f0d862d093b3ae899cf67).

## The Parser type

Before we begin to define combinators that act on parsers,
we must choose a representation for a parser first.

A parser takes a string and produces an output that can be just about anything.
A list parser will produce a list as it's output,
an integer parser will produce `Int`s,
a JSON parser might return a custom [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) representing a JSON.

Therefore, it makes sense to make Parser a polymorphic type.
It also makes sense to return a list of results instead of a single result
since grammar can be ambiguous, and there may be several ways to parse the same input string.

An empty list, then, implies the parser failed to parse the provided input. ([1](#backmatter))

```hs
newtype Parser a = Parser { parse :: String -> [(a, String)] }
```

You might wonder why we return the tuple `(a, String)`, not just `a`.
Well, a parser might not be able to parse the entire input string.
Often, a parser is only intended to parse some prefix of the input, and let another parser do the rest of the parsing.
Thus, we return a pair containing the parse result `a` and the unconsumed string subsequent parsers can use.

We could have used the `type` keyword let `Parser` be an alias for `String -> [(a, String)]`,
but having a unique data type lends us the ability to instantiate it as a typeclass,
which is something we'll do later on.

## Baby parsers

We can start by describing some basic parsers that do very little work.
A `result` parser always succeeds in parsing without consuming the input string.

```hs
result :: a -> Parser a
result val = Parser $ \inp -> [(val, inp)]
```

The parser `zero` will always fail by returning an empty list.

```hs
zero :: Parser a
zero = Parser $ const []
```

`item` unconditionally accepts the first character of any input string.

```hs
item :: Parser Char
item = Parser parseItem
  where
    parseItem [] = []
    parseItem (x:xs) = [(x, xs)]
```

Let's try some of these parsers in GHCi:

```hs
*Main> parse (result 42) "abc"
[(42, "abc")]
*Main> parse item "abc"
[('a', "bc")]
```

## Building parsers on demand

The basic parsers we defined above are of very little use.
Ideally, we would want parsers that accept input strings that satisfy certain constraints.
Say we want a parser that consumes a string if its first character satisfies a predicate.
We can generalize this idea by writing a function that takes a `(Char -> Bool)` predicate and returns a parser that only consumes an input string if its first character returns `True` when supplied to the predicate.

The simplest solution for this would be:

```hs
sat :: (Char -> Bool) -> Parser Char
sat p = Parser parseIfSat
  where
    parseIfSat (x : xs) = if p x then [(x, xs)] else []
    parseIfSat [] = []
```

However, since we already have an `item` parser that unconditionally extracts
the first character from a string, we could use this as an opportunity to create a basic parser combinator.

Before writing a combinator, we must first instantiate `Parser` as a `Monad`. ([2](#backmatter))

```hs
instance Monad Parser where
  p >>= f = Parser $ \inp ->
    concat [parse (f v) inp' | (v, inp') <- parse p inp]
  -- a -> Parser a
  return = result
```

The `bind` operation takes a `Parser a` (p) and a function `a -> Parser b` (f), and returns a `Parser b`.
The idea is to apply `p`, if it fails, then we have an empty list which results in `concat [[]]` = `[]`.
If `p` successfully parses `inp` into one or more possible parse results,
we apply `f` to each of the results to get corresponding `Parser b`s and then apply those to the rest of the input.

With this new extension, our `sat` parser can be re-written as:

```hs
sat p =
-- Apply `item`, if it fails on an empty string, we simply short circuit and get `[]`.
  item >>= \x ->
    if p x
      then result x
      else zero
```

Now we can use the `sat` combinator to describe several useful parsers.
For example, a `char` parser that only consumes a string beginning with a specific character.

```hs
char :: Char -> Parser Char
char x = sat (== x)
```

```hs
*Main> parse (char 'a') "abc"
[('a', "bc")]
```

A parser for ASCII digits:

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

```hs
$ ghci -i main.hs
*Main> parse lower "aQuickBrownFox"
[('a',"QuickBrownFox")]
```

Now that we have `upper`, `lower` and `digit` this opens up new possibilities for combinations:

- An `alphabet` parser that accepts a char that is consumable by either `upper` or `lower`.
- An `alphanumeric` parser that accepts a char, either `alphabet` or `digit`.

Clearly, an `or'` combinator that captures this recurring pattern will come in handy.

Let us begin by describing a `plus` combinator that concatenates the result returned by two parsers:

```hs
-- Applies two parsers to the same input, then returns a list
-- containing results returned by both of them.
plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ \inp -> parse p inp ++ parse q inp
```

Haskell has a [MonadPlus](https://wiki.haskell.org/MonadPlus) typeclass defined in the prelude like so:

```hs
class (Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
```

`mzero` represents failure, and `mplus` represents combination of two monads.
Since `Parser` is already a monad, we can instantiate the `MonadPlus` typeclass to enforce
this idea:

```hs
-- Add this to the list of imports:
-- import Control.Monad (MonadPlus (..))

instance MonadPlus Parser where
  mzero = zero
  mplus = plus
```

The `or'` combinator can then be:

```hs
or' :: Parser a -> Parser a -> Parser a
p `or'` q = Parser $ \inp -> case parse (p `plus` q) inp of
    [] -> []
    (x:xs) -> [x]
```

In fact, the `Alternative` typeclass already defines this functionality with the choice (`<|>`) operator:

```hs
instance Alternative Parser where
  empty = zero
  (<|>) = or'
```

Finally, we can return to the `letter` and `alphanum` parsers:

```hs
letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit
```

We can now take them for a spin in GHCi:

```hs
*Main> parse letter "p0p3y3"
[('p',"0p3y3")]
*Main> parse letter "30p3y3"
[]
*Main> parse alphanum "foobar"
[('f',"oobar")]
```

As an aside, we can use the
[sequencing (>>) operator](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:-62--62-) to write more concise code at times.
Consider the function `string` for example, where `string "foo"` returns a parser that only accepts strings which begin with "foo".

```hs
string :: String -> Parser String
string "" = result ""
string (x:xs) =
  char x >> string xs >> result (x:xs)
```

Using `>>=` notation, we would have had to write:

```hs
string (x:xs) =
  char x
    >>= const string xs -- same as \_ -> string xs
    >>= const result (x:xs) -- same as \_ -> result (x:xs)
```

```hs
*Main> parse (string "prefix") "prefixxxxx"
[("prefix", "xxxx")]
```

## Using the do notation

Haskell provides a handy [do notation](https://en.wikibooks.org/wiki/Haskell/do_notation)
for readably sequencing monadic computations.
This is useful when composing monadic actions becomes a bit gnarly looking.
Consider this example that composes the outputs of several parsers:

```hs
parser = parser1 >>= \x1 -> -- 1. apply parser1
  make_parser2 x1 >>= \x2 -> -- 2. use parser1's output to make parser2
    make_parser3 x2 >>= \x3 -> -- 3. Use parser2's output to make parser3
      return (f x1 x2 x3) -- 4. Combine all parse results to form the final result
```

Using the `do` notation, the above code snippet becomes:

```hs
parser = do
  x1 <- parser1
  x2 <- make_parser2 x1
  x3 <- make_parser3 x2
  return (f x1 x2 x3)
```

Moving forward, we will prefer the `do` notation over `>>=` wherever it improves readability.

## Combinators for repetition

You may be familiar with the regex matchers `+` and `*`.
`a*` matches zero or more occurrences of the letter 'a', whereas `a+` expects one or more occurrences of the letter 'a'.

We can represent the `*` matcher as a combinator like so:

```hs
many' :: Parser a -> Parser [a]
many' p = do
  x  <- p -- apply p once
  xs <- many' p -- recursively apply `p` as many times as possible
  return (x:xs)
```

Looks decent, but when run in GHCi, it fails to produce the expected result:

```hs
*Main> parse (many' $ char 'x') "xx"
[]
```

If you try to work out the application of this parser by hand, you'll notice a flaw in our base case:
In the final recursive call, when the input string is `""`, `x <- p` fails, and we short circuit to return `[]`.

To handle this scenario, we can use our `or'` combinator:

```hs
many' :: Parser a -> Parser [a]
many' p =
  do
    x <- p -- apply p once
    xs <- many' p -- recursively apply `p` as many times as possible
    return (x : xs)
    <|> return []

  -- In case `p` fails either in the initial call, or in one of the
  -- recursive calls to itself, we return an empty list as the parse result.
```

And we're golden:

```hs
*Main> parse (many' $ char 'x') "xxx123"
[("xxx","123")]
```

If the use of `<|>` is still confusing to you, try working it out on paper.

Analogous to the regex `+` matcher, we can write a `many1` combinator that accepts one or more occurrences of an input sequence.
Piggybacking off of `many'`, this can be simply written as:

```hs
many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many' p
  return (x:xs)
```

## Parsing a list of identifiers

If you haven't realized by now, we've built some combinators capable of parsing regular languages.
Circling back to the beginning of this post, here is a combinator that parses a valid C-style identifier:

```hs
ident :: Parser String
ident = do
  x <- alpha_
  xs <- many' (alpha_ <|> digit)
  return (x : xs)
  where
    alpha_ = letter <|> char '_'
```

```hs
*Main> parse ident "hello_123_ = 5"
[("hello_123_"," = 5")]
```

To make it even more concise, we can define a `then'` combinator which combines
the result produced by two parsers using a caller provided function.

```hs
then' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
then' combine p q =
  p >>= \x ->
    q >>= \xs ->
      result $ combine x xs
```

A `thenList` combinator can then combine to parse results of type `a` and `[a]` using `(:)`.

```hs
thenList :: Parser a -> Parser[a] -> Parser[a]
thenList = then' (:)
```

Now our identifier parser becomes even shorter:

```hs
ident :: Parser String
ident = alpha_ `thenList` many' (alpha_ <|> digit)
  where
    alpha_ = letter <|> char '_'
```

Now, lets take our combinations a step further.
Say we want to parse a list of comma-separated identifiers,
Here is one way to do that:

```hs
idList :: Parser [String]
idList = do
  firstId <- ident
  restIds <- many' (char ',' >> ident)
  return (firstId : restIds)
```

A token-separated list of items is a commonly occurring pattern in language grammar.
As such, we can abstract away this idea with a `sepBy` combinator:

```hs
-- Accept a list of sequences forming an `a`, separated by sequences forming a `b`.
sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = do
  x <- p
  xs <- many' (sep >> p)
  return (x : xs)

idList = ident `sepBy` char ','
```

Now, what if the list of identifiers was enclosed in braces like in an array?
We can define another combinator, `bracket`, to parse strings enclosed within specific sequences.

```hs
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  _ <- open
  x <- p
  _ <- close
  return x
```

Sequencing operators can be used to write `bracket` in a slightly more elegant manner:

```hs
bracket open p close = open >> p <* close
```

Using this, our parser for a list of items can be written as:

```hs
idList = bracket (char '[') ids (char ']')
  where
    ids = ident `sepBy` char ','
```

Let's test this implementation in GHCi:

```
*Main> parse idList "[foo,bar,baz]"
[(["foo","bar","baz"],"")]
```

Perfect!

## Parsing natural numbers

Since our parsers are polymorphic, we can return a parse result containing an input string's evaluated value.
Here is a parser that consumes and evaluates the value of a natural number:

```hs
-- Add this to the list of imports:
-- import Data.Text.Internal.Read (digitToInt)
nat :: Parser Int
nat =
  many1 digit >>= eval
  where
    eval xs = result $ foldl1 op [digitToInt x | x <- xs]
    m `op` n = 10 * m + n
```

A natural number is one or more decimal digits, which we then fold to produce a base 10 value.
Alternatively, we can use the builtin `read` to implement `nat`:

```hs
nat = read <$> many1 digit
```

## Handling whitespace

As you may already have noticed, the parsers we've written so far aren't great at dealing with whitespace.

```hs
*Main> parse idList "[a, b, c]"
[]
```

Ideally, we should ignore any whitespace before or after tokens.
Generally, it is a tokenizer's job to handle whitespaces and return a list of tokens that the parser can then use.
However, it is possible to skip a tokenizer completely when using combinators.

We can define a `token` combinator that takes care of all trailing whitespace:

```hs
-- Add 'void' and 'isSpace' to import lists.
-- import Control.Monad (MonadPlus (..), void)
-- import Data.Char (isDigit, isLower, isUpper, isSpace)

spaces :: Parser ()
spaces = void $ many' $ sat isSpace

token :: Parser a -> Parser a
token p = p <* spaces
```

And a `parse'` combinator that removes all leading whitespace:

```hs
parse' :: Parser a -> Parser a
parse' p = spaces >> p
```

The `parse'` combinator is applied to the final parser once to ensure there is no leading whitespace.
The `token` combinator consumes all trailing whitespace,
hence ensuring there is no leading whitespace left for the subsequent parsers.

We can now write parsers that disregard whitespace:

```hs
identifier :: Parser String
identifier = token ident
```

At this point, we have atomic parsers that can be plugged in several places.
One such place can be an arithmetic expression evaluator:

## An expression parser

Finally, to demonstrate the usefulness of combinators we have defined so far, we build a basic arithmetic expression parser.
We will support the binary `+` and `-` operators, parenthesized expressions and integer literals.
By the end, we will have an `eval` function that can evaluate expressions like so:

```hs
*Main> eval "1 + 2 - 3 - 4 + 10"
6
```

```hs

-- consume a character and discard all trailing whitespace
charToken :: Char -> Parser Char
charToken = token <$> char

-- an ADT representing a parse tree for expressions
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Par Expr
          | Lit Int
          deriving (Show)

eval' :: Expr -> Int
eval' (Add a b) = eval' a + eval' b
eval' (Sub a b) = eval' a - eval' b
eval' (Par a)   = eval' a
eval' (Lit a)   = a

eval :: String -> Int
eval =  fst . Bifunctor.first eval' . head <$> parse expr

-- Our expression parser expects a string of the following grammar:
-- expr ::= term (op term)*
-- op ::= '+' | '-'
-- term ::= int | '(' expr ')'
-- int ::= [0-9]*

-- The `expr` parser first consumes an atomic term - <X>, then it
-- consumes a series of "<op> <operand>"s and packs them into tuples like ((+), 2)
-- We then fold the list of tuples using <X> as the initial value to produce the result.
expr :: Parser Expr
expr = do
  x <- term
  rest <- many' parseRest
  return $
    foldl (\x (op, y) -> x `op` y) x rest
  where
    parseRest = do
      f <- op
      y <- term
      return (f, y)

-- term := int | parens
term :: Parser Expr
term = int <|> parens

-- parens := '(' expr ')'
parens :: Parser Expr
parens = bracket (char '(') expr (char ')')

-- int := [0-9]*
int :: Parser Expr
int = Lit <$> token nat

-- op := '+' | '-'
op :: Parser (Expr -> Expr -> Expr)
op = makeOp '+' Add <|> makeOp '-' Sub
    where makeOp x f = charToken x >> return f
```

Spin up GHCi, and there we have it:

```hs
*Main> eval "1 + 2 - (3 - 1)"
3

*Main> eval "1 + 2 + 3"
6
```

Our parser is decent, but it can be refactored a little further.
An expression is a list of parenthesized expressions and integer literals separated by `+` or `-`.

As it turns out, parsing a list of token delimited items is a common pattern captured by the `chainl1` combinator:

```hs
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  first <- p
  rest <- many' $ do
    f    <- op
    term <- p
    return (f, term)
  return $ foldl (\x (f, y) -> f x y) first rest

expr :: Parser Expr
expr = term `chainl1` op
```

And with that, we have a monadic expression parser composed of several tiny and modular parsers.
As an exercise, you can extend this parser and add more operators such as multiplication, division, and log.

## Further reading

There are already several parser combinator libraries for many languages, as you may have guessed.
[Parsec](https://hackage.haskell.org/package/parsec) in particular, is the most commonly used one among Haskell programmers.
Here are more resources for you to chew on:

1. [Monadic Parser Combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf)
2. [Functional Pearls - Monadic Parsing in Haskell](https://www.cs.nott.ac.uk/~pszgmh/pearl.pdf)
3. [Microsoft Research - Direct style monadic parser combinators for the real world](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf)

The first two should feel very familiar if you've followed the post so far.
The 3rd is a paper that attempts to provide a better alternative technique for parsing using monads.

At this point, parser combinators have become another tool in your functional programming arsenal.
Go forth and write some killer parsers!

## Backmatter

1. In most implementations, the parse result is a functor that can store an error message in case the parser fails.

   ```hs
   newtype Parser a = Parser { parse :: String -> ParseResult a }
   type ParesResult a = Either ParseError a
   type ParseError = String
   ```

2. In order to instantiate Parser as a Monad in Haskell, we also have to make it an instance of `Functor` and `Applicative`:

```hs
instance Functor Parser where
  fmap f p = Parser (fmap (Bifunctor.first f) . parse p)

instance Applicative Parser where
  pure = result
  p1 <*> p2 = Parser $ \inp -> do
      (f, inp') <- parse p1 inp
      (a, inp'') <- parse p2 inp'
      return (f a, inp'')
```
