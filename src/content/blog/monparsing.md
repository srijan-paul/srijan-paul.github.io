A [Parser combinator](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf), as wikipedia describes it,
is a _higher-order function that accepts several parsers as input and returns a new parser as its output_.

They can be very powerful when you want to build modular parsers and leave them open for further extension.
But it can be tricky to get the error reporting right when using a 3rd party combinator library, and they tend to be slower in imperative languages.
Nonetheless, it is an interesting cornerstone in both functional programming and PLT, so it shouldn't hurt for us to learn about them by building one on our own.

We're going to start by writing a library that describes several tiny parsers and functions that operate on those parsers.
Then, we're going to build some parsers to demonstrate the usefulness of our work.

To give you a small flash forward, here is a parser that accepts C-style identifiers,
written with the help of our handy combinators:
```hs
-- matches strings that satisfy [a-zA-Z][a-zA-Z0-9]+
ident :: Parser String
-- One letter or '_', followed by zero of more '_', letters or digits
ident = alpha_ `thenList` many (alpha_ <|> digit)
  where
    alpha_ = letter <|> char '_'
```

It is recommended for you to have some basic understanding of:

- Parsers.
- Monads in functional programming.
- Haskell.

This post is a derviative of two papers I had read recently ([1](https://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf), [2](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf)).
If you like yourself a denser reading, you can go through the papers instead.

## The Parser type

Before we begin to define combinators that act on parsers,
we must choose a representation for a parser first.

A parser takes a string and produces an output that can be just about anything.
A list parser will produce a list as it's output,
an integer parser will produce `Int`s,
a JSON parser might return a custom [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) representing a JSON.

Therefore, it makes sense to make Parser a polymorphic type.
It also makes sense to return a list of results instead of a single result,
since grammars can be ambigious and there may be several ways to parse the same input string.

An empty list, then, implies the parser failed to parse the provided input. ([1](#backmatter))

```hs
newtype Parser a = Parser { parse :: String -> [(a, String)] }
```

You might be wondering why we return the tuple `(a, String)`, and not just `a`.
Well, a parser might not be able to parse the entire input string.
Often, a parser is only intended to parse some prefix of the input, and let another parser do the rest of the parsing.
Thus, we return a pair containing the parse result `a`, and the unconsumed string that can be used by subsequent parsers.

We could have used the `type` keyword let `Parser` be an alias for `String -> [(a, String)]`,
but having a unique data type lends us the ability to instantiate it as a typeclass,
which is something we'll do later on.

## Baby parsers

We can start by describing some basic parsers that do very little work.
A `result` parser always returns the same value and doesn't touch the input string.

```hs
result :: Parser a
result val = Parser $ \inp -> [(val, inp)]
```

The parser `null` will always fail by returning an empty list.

```hs
null :: Parser a
null = Parser $ const []
```

`item` unconditionally accepts the first character of any input string.

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
We can generalize this idea by writing a function that takes a `(Char -> Bool)` predicate and returns a parser that only consumes an input string if its first character, when supplied to the predicate, returns `True`.

The simplest solution for this would be:

```hs
sat :: (Char -> Bool) -> Parser Char
sat p (x:xs) = if p x
  then [(x, xs)]
  else []
sat _ [] = []
```

However, since we already have an `item` parser that unconditionally extracts
the first character from a string, we could use this as an opportunity create a basic parser combinator.

Before writing a combinator, we must first instantiate `Parser` as a `Monad`. ([2](#backmatter))

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
If `p` successfully parses `inp` into one or more possible parse results,
we apply `f` to each of the results to get corresponding `Parser b`s and then apply those to the rest of the input.

With this new extension, our `sat` parser can be re-written as:

```hs
sat p =
-- Apply `item`, if it fails on an empty string, we simply short circuit and get `[]`.
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

```
*Main> parse lower "aQuickBrownFox"
[('a',"QuickBrownFox")]
```

Now that we have `upper`, `lower` and `digit` this opens up new possibilities for combinations:

- An `alphabet` parser that accepts a char that is consumable by either `upper` or `lower`.
- An `alphanumeric` parser that accepts a char which is either `alphabet` or `digit`.

Clearly, an `or` combinator that captures this recurring pattern will come in handy.

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
  mzero = null
  mplus = plus
```

The `or` combinator can then be:

```hs
or :: Parser a -> Parser a -> Parser a
p `or` q = Parser $ \inp -> case parse (p `plus` q) inp of
    [] -> []
    (x : xs) -> [x]
```

In fact, the `Alternative` typeclass already defines this functionality with the choice (`<|>`) operator:

```hs
instance Alternative Parser where
  empty = zero
  (<|>) = or
```

Finally, we can return to the `letter` and `alphanum` parsers:

```hs
letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit
```

We can now take them for a spin in GHCi:

```
*Main> parse letter "p0p3y3"
[('p',"0p3y3")]
*Main> parse letter "30p3y3"
[]
*Main> parse alphanum "foobar"
[('f',"oobar")]
```

As a random aside, we can use the
[sequencing (>>) operator](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:-62--62-) to write more concise code at times.
Consider the function `string` for example, where `string "foo"` returns a parser that only accepts strings which begin with "foo".

```hs
string :: String -> Parser String
string "" = result ""
string (x : xs) =
  char x >> string xs >> result (x:xs)
```

Using `>>=` notation, we would have had to write:

```hs
string (x:xs) =
  char x
    >>= const string xs -- same as \_ -> string xs
    >>= const result (x:xs) -- same as \_ -> result (x:xs)
```

```
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

Using the do notation, the above code snippet becomes:

```hs
parser = do
  x1 <- parser1
  x2 <- make_parser2 x1
  x3 <- make_parser3 x2
  return (f x1 x2 x3)
```

## Combinators for repition

You may be familiar with the regex matchers `+` and `*`.
`a*` matches 0 or more occurences of the letter 'a' whereas `a+` expects at least 1 'a'.

We can represent the `*` matcher as a combinator like so:

```hs
many :: Parser a -> Parser [a]
many p = do
  x  <- p -- apply p once
  xs <- many p -- recursively apply `p` as many times as possible
  return (x:xs)
```

Looks decent, but when run in GHCi, it fails to produce the expected result:

```
*Main> parse (many $ char 'x') "xx"
[]
```

If you try to work out the application of this parser by hand, you'll notice a flaw in or base case:
In the final recursive call, when the input string is `""`, `x <- p` fails and we short circuit to return `[]`.

To handle this scenario, we can use our `or` combinator:

```hs
many :: Parser a -> Parser [a]
many p =
  ( do
      x <- p -- apply `p` once
      xs <- many p -- recursively apply `p` as many times as possible
      return (x : xs) -- Combine the results returned by each parser
  ) <|> return []
  -- In case `p` fails either in the initial call, or in one of the
  -- recursive calls to itself, we return an empty list as the parse result.
```

And we're golden:

```
*Main> parse (many $ char 'x') "xxx123"
[("xxx","123")]
```

If the use of `<|>` is still confusing to you, try working it out on paper.

Anologous to the regex `+` matcher, we can write a `many1` combinator that accepts atleast 1 occurence of an input sequence.
Piggybacking off of `many`, this can be simply written as:

```hs
many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)
```

## Parsing a list of identifiers

If you haven't realized by now, we've built some combinator that are capable of parsing regular languages.
Circling back to the beginning of this post, here is a combinator that parses a valid C-style identifier:

```hs
ident :: Parser String
ident = do
  x <- alpha_
  xs <- many (alpha_ <|> digit)
  return (x : xs)
  where
    alpha_ = letter <|> char '_'
```

```
*Main> parse identifier "hello_123_ = 5"
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
ident = alpha_ `thenList` many (alpha_ <|> digit)
  where
    alpha_ = letter <|> char '_'
```

Now, lets take our combinations a step further.
Say we want to parse a list of comma separated identifiers,
Here is one way to do that:

```hs
idList :: Parser [String]
idList = do
  firstId <- identifier
  restIds <- many $ (char ',' >> identifier)
  return (firstId : restIds)
```

A token separated list of items is a very commonly occurring pattern in language grammars.
As such, we can abstract away this idea with a `sepBy` combinator:

```hs
-- Accept a list of sequences forming an `a`, separated by sequences forming a `b`.
sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)

idList = identifier `sepBy` char ','
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

Using this, our parser for a list of items can be written as:

```hs
idList = bracket (char '[') ids (char ']')
  where
    ids = identifier `sepBy` char ','
```

Let's test this implementation in GHCi:

```
*Main> parse idList "[foo,bar,baz]"
[(["foo","bar","baz"],"")]
```
Perfect!

## Parsing natural numbers
Since our parsers are polymorphic, we can return a parse result that contains the evaluated value of an input string.
Here is a parser that consumes and evaluates the value of a natural number:

```hs
nat :: Parser Int
nat =
  many1 digit >>= eval
  where
    eval xs = result $ foldl1 op [ord x - ord '0' | x <- xs]
    m `op` n = 10 * m + n
```

A natural number is one or more decimal digits, which we then fold to produce a base 10 value.

## Handling whitespace
As you may already have noticed already,
the parsers we've written so far aren't great at dealing with whitespace.

```hs
*Main> parse idList "[a, b, c]"
[]
```

Ideally, we should ignore any whitespace before or after tokens.
Generally, it is a tokenizer's job to handle whitespaces and return a list of tokens that the parser can then use.
However, it is possible to skip a tokenizer completely when using combinators.

We can define a `token` combinator that takes care of all trailing whitespace :

```hs
spaces :: Parse ()
spaces = void $ many $ sat isSpace

token :: Parser a -> Parser a
token p = do
  x <- p
  _ <- spaces
  return x
```

And a `parse'` combinator that removes all leading whitespace:

```hs
parse' :: Parser a -> Parser a
parse' p = spaces >> p
```

The `parse'` combinator is applied to the final parser once, to ensure there is no leading whitespace.
The `token` combinator consumes all trailing whitespace,
hence ensuring there is no leading whitespace left for the subsequent parsers.

We can now write parsers that disregard whitespace:

```hs
identifier :: Parser String
identifier = token ident
```

At this point, we have atomic parsers that can be be plugged in several places.
One such place can be an arithmetic expression evaluator:

## An expression parser.

Finally, to demonstrate the usefulness of combinators we have defined so far,
here is a parser capable of parsing (and evaluating) basic arithmetic expressions:

```hs
-- consume a character, and discard all trailing whitespace
charToken :: Char -> Parser Char
charToken = token <$> char

-- Our expression parser expects a string of the following grammar:
-- expr ::= term (sumOp term)*
-- sumOp ::= '+' | '-'
-- term ::= nat | '(' expr ')'
-- nat ::= [0-9]*

-- The `expr` parser first consumes an atomic term - <X>, then it
-- consumes a series of "<op> <operand>"s and packs them into tuples like ((+), 2)
-- We then fold the list of tuples using <X> as the initial value to produce the result.
expr :: Parser Int
expr = parse' $ do
  x <- term
  rest <- many parseRest
  return $
    foldl (\x (op, y) -> x `op` y) x rest
  where
    parseRest = do
      op <- sumOp
      y <- term
      return (op, y)

-- parses the `+` or `-` character into its equivalent haskell operator
sumOp :: Parser (Int -> Int -> Int)
sumOp = plusOp <|> minusOp
  where
    plusOp = makeOp '+' (+)
    minusOp = makeOp '-' (-)
    makeOp c fn = charToken c >> return fn

-- an atomic integer, or a parenthesized expression
term :: Parser Int
term = token nat <|> bracket (charToken '(') expr (charToken ')')
```

Spin up GHCi, and there we have it:

```hs
*Main> parse expr "1 + 2 - (3 - 1)"
[(1, "")]
```

As an exercise, you can extend this parser and add support for more operators such as multiplication, division and log.

## Further reading
As you may have guessed, there are already several parser combinator libraries for many languages.
[Parsec](https://hackage.haskell.org/package/parsec) in particular, is the most commonly used one among Haskell programmers.
Here are more resources for you to chew on:

1. [Monadic Parser Combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf)
2. [Functional Pearls - Monadic Parsing in Haskell](https://www.cs.nott.ac.uk/~pszgmh/pearl.pdf)
3. [Microsoft Research - Direct style monadic parser combinators for the real world](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf)

The first two should feel very familiar if you've followed the post so far.
The 3rd is a paper that attempts to provide a better alternative technique for parsing using monads.

At this point, parser combinators have become another tool in your functional programming aresenal.
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
  p <*> q = undefined
```
