In Haskell, polymorphic types can have unused type parameters.
The unused parameters are called [Phantom types]().
For example:

```haskell
newtype Spooky a ph = Spooky a
```

The Type `Spooky` has a data constructor of the same name.
When you call `Spooky v` – where `v` is some value of type `a` –
it will construct a type that wraps `v` inside it (`Spooky a ph`).

But what is the `ph` type?
The data constructor does not provide `ph`, so where does it come from?

Well, it is inferred from context.
To see it in action, let's spin un a REPL:

```haskell
ghci> x = Spooky "x"
ghci> :t Spooky x
Spooky "x" :: forall {k} {ph :: k}. Spooky String ph
```

(__citation needed__)
That's a polymorphic type (of order 1),
where the type `ph` is unspecified, and can be anything.
It turns out that you can use an explicit annotation to get the type
to adapt to any valid signature:

```haskell
ghci> y = x :: Spooky String Int
ghci> :t y
y :: Spooky String Int
```

However, once `y` is assigned a monomorphic type, its type cannot be narrowed down further.
The `Spooky String Int` is like a hitman tattoo that identifies `y` as a spooky-str-int, whatever that means.

## A toy library

Imagine that you're authoring a basic server-side library that allows the user to:

1. Fetch random data from somewhere on the internet.
2. Add any fetched data to some database. 
3. Log any received data onto a server's console for monitoring purposes.

To keep ourselves from going off on a tangent, we will heavily simplify the implementation of this library.
For starters, all data will be fetched from stdin, and we'll just pretend it's some web server.

```haskell
-- Lib.hs
module Lib ( ... ) where

fetchInput :: IO String
fetchInput = getLine
```

The "database" is just going to be a list of inputs received:

```haskell
-- Lib.hs
type Db = [String]

addToDb :: Db -> String -> Db
addToDb = flip (:)
```

Finally, we have a logger that prints any user provided input with a time stamp.
This function is only meant for use with input that was yielded by `fetchInput`.

```haskell
-- Lib.hs
logInput :: String -> IO ()
logInput input = do
  time <- Time.getCurrentTime
  print $ "[" ++ show time ++ "]: " ++ input
```

Passing any arbitrary string into a database query can be hazardous.
In fact, even logging untrusted data to a console runs the risk of [log injection] attacks.

Ideally, any string that is passed to `addToDb`, `logInput`, and 
other similarly sensitive functions, should first be validated for safety.

To enable this, we provide a function that checks if a string is properly escaped.
Since this a toy library, we'll assume any string that begins with `"malicious"` is a hacker trying to pwn us.

```haskell
-- Lib.hs
isValid :: String -> Bool
isValid = L.isPrefixOf "malicious:"
```


