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
To see it in action, spin up a REPL:

```haskell
ghci> x = Spooky "x"
ghci> :t Spooky x
Spooky "x" :: forall {k} {ph :: k}. Spooky String ph
```

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
The `Spooky String Int` is like a permanent tattoo that identifies `y` as a spooky-str-int, whatever that means.

## Case study: A database library 

Imagine you're authoring a backend library that offers – among other things – the following capabilities:

1. Fetching data from any URL.
2. Running database queries.
3. Logging timestamped strings (like HTTP requests, DB queries, etc.) to the console for monitoring purposes.

To keep things simple, we will ignore the implementation details, and only focus on type signatures.

```haskell
-- Lib.hs
module Lib ( runDbQuery, log, fetchData ) where

runDbQuery :: String -> IO ()
runDbQuery = undefined

log :: String -> IO ()
log = undefined

fetchData :: URL -> IO String
fetchData = undefined
```

Allowing arbitrary strings to be interpreted as database commands is an obvious security hazard.
Any string from an unsafe source (like an HTTP request parameter, or some third party server)
is a potential [SQL injection](https://en.wikipedia.org/wiki/SQL_injection) attempt.

Even carelessly logging such strings to the console can open us up to [log injection](https://owasp.org/www-community/attacks/Log_Injection).

These threats now posit a requirement: All strings passed to `runDbQuery` and `log` must be validated for safety. 
To keep us from getting pwned, we can implement a validator function that checks if the input is valid:

```haskell
-- Lib.hs
isSafe :: String -> Bool
isSafe input = undefined
```

`isSafe` returns `True` only if `input` is safe for consumption by the other functions that our library exports.
Now, we are faced with a design challenge: Who gets to call `isSafe`, and where is it called?
Two approaches come to mind:

The first, is where the user calls it to verify a string's safety before passing it to `runDbQuery` or `log`.
So the user would have to perform checks like this:

```haskell
-- Main.hs
import Lib (runDbQuery, log)

main = do
  query <- getLine
  if isSafe query
    then do 
      runDbQuery query
      log query
    else print "Invalid DB query"
```

But what happens when the user forgets to call `isSafe`?
Insecure use of library will still compile and spawn a vulnerable application binary:

```haskell
-- Main.hs

main =  do
  query <- getLine
  log query -- PWNED
  runDbQuery query -- PWNED x2
  return ()
```

In a production application, there will be multiple places to draw tainted strings from.
It's not uncommon for developers to forget sanitizing the tainted input.
A developer might erroneously remove a call to `isSafe` when refactoring some code.
The application is a single line of code away from a gazillion dollar AWS bill.
When something breaks because of tainted data having made it to a sensitive function call,
it's the users of our library who're held guilty.

Our library relies on human memory to perform data validation.
Any application using such a library is doomed to break before the user has even begun working on the project.

The possible code paths that tainted data can take are simply too many, and it's irresponsible
for us to expect users of our library to routinely call `isSafe` before every sensitive function call. 

A judicious library author should avoid transfering this burden of validation to users.

An alternative design choice therefore, is to call `isSafe` on the user's behalf:

```haskell
-- Lib.hs
runDbQuery :: String -> IO ()
runDbQuery query =
  if isSafe query
    then execute query
    else error "Unsafe database query"

log :: String -> IO ()
log s =
  if isSafe s
    then print $ formatWithTime s
    else error "Unsafe server log"
```

Now, even if the user passes an insecure string to one of these functions, it'll be rejected.

The security hole is patched,
and all strings are guaranteed to be routed via `isSafe` before hitting any safety critical paths.

However, this design has its own share of problems. 

The most glaringly obvious issue here is that of performance and redundancy.
Even when the user has taken measures to ensure that the input is safe, we're still performing redundant checks.
Moreover, passing the same string to `runDbQuery` and `log` will perform the same check twice.
Sometimes, validation isn't necessary because the input comes from within the program.
In the example below for instance, `input` is a string literal, and yet it gets validated twice:

```haskell
-- Main.hs
main = do
  let input = "insert into fruits value ('mango')"
  runDbQuery input -- Will run `isSafe` unneccessarily.
  log input        -- will run `isSafe` again.
```

And what if we forget to call `isSafe` when writing the library code?
Afterall, insecure code-paths in a real library greatly outnumber our toy example.
As seen earlier, it will still compile and we'll condemn all our users to security hazards.
In a way, this approach is *worse*.
Not only does it incur a higher performance cost,
it still runs the risk of someone (read: the library authors) forgetting to
call `isSafe` before shipping.


This approach has another issue which isn't apparent immediately: it lacks totality.
Currently, the functions `log` and `runDbQuery` are partial –
they simply reject some values in their domain by erroring out.

Some strings will be logged  successfully, while others will be rejected with an error (or a void return).
Our users, then, have to carefully handle these scenarios.

When writing functional code, it is ideal to prefer total functions wherever possible.
This prevents the user from having to handle cases where our functions bail out, and
the function can be expected to behave well for any input that satisfies its type signature.

Sadly, no matter which design choice we make, there will be unwelcome scenarios.

The conundrum we now face is very common in API design.
Our library offers functions that expect *valid*, well-formed data,
and a validate function that checks if some data is well-formed.
We want to **ensure that all input provided to a sink has been routed through a validator function**.

In a more abstract sense, the problem can be restated as follows:
"Before passing value to a function `f`, we want it to be given to a different function `g`".

In our use-case, `f` is `runDbQuery`, and `g` is `isSafe`.

Ideally, we want our solution to have as little overhead as possible.

## Phantom tags as proofs

There is a third approach that has ideal performance, doesn't allow insecure code to compile, and ensures totality. 
It turns out, we can leverage the type system (and some data hiding)
to ensure that all arguments to `log` and `runDbQuery` have been routed to `isSafe` first.

The core idea is to introduce a new validator function that "tags" the input as safe.
This "tag" is going to be a unique type that can only be produced by a validator function.

Other library functions will have their type signatures updated to only accept values that are accompanied with a type-level `tag` to attest for safety.
If a tag-less raw type is passed to one of these functions, the code will refuse to compile.

A 'tagged' value is expressed using a phantom type that sticks to it:

```haskell
-- Lib.hs

module Lib (
  -- other exports,
  SafeString,
) where

-- attach a phantom type `tag` to a value of type `a`
newtype Tagged a tag = Tagged a

tag :: a -> Tagged a tag
tag = Tagged

-- The type `Safe` should not be exported by Lib.
-- This is to make sure that the user cannot conjure
-- 'Safe strings' by fiat.
data Safe

-- A "safe" string is any string that has been tagged with the type `Safe`.
type SafeString = Tagged String Safe

extract :: Tagged a tag -> a
extract (Tagged a) = a
```

We introduce a new validator that returns the input with a proof of safety tattooed to it:

```haskell
-- Lib.hs

module Lib (
  -- other exports,
  validate,
) where

validate :: String -> Maybe SafeString
validate s = 
  if isSafe s
    then Just $ tag s
    else Nothing
```

Since the `Safe` data-type is not exported by our library, our users cannot create values of the type `Tagged a Safe` by fiat.
The only way to obtain a value of type `SafeString` is by calling `validate`.

Finally, we update the type signatures for our sinks to only accept `SafeString`s:

```haskell
runDbQuery :: SafeString -> IO ()
runDbQuery (Tagged s) = undefined

log :: SafeString -> IO ()
log (Tagged s) = undefined
```

Now, if the user attempts to call `queryDb` on a plain string before passing it to `validate` first,
it will refuse to compile. Consider this example:

```haskell
-- Lib.hs

main = do
  input <- getLine
  log input
  runDbQuery input
```

On compiling the above, we get:

```text
typecheck: Couldn't match type [Char] with: Tagged String Safe
```

The type system expected a `SafeString`, but was given a regular `String` type.
With our new API, the only way to log and query using a string, is to first pass it to the `validate` function:


```haskell
main = do
  input' <- getLine
  forM_ (validate input') $ \input->  do
    logStr input 
    runDbQuery input 
```

Since the return type of `validate` is a `Maybe` monad, we use `forM_` to handle the `Just` case.
The above code is equivalent to:

```haskell
case validate input' of
  Just input -> do
    logStr input
    runDbQuery input
  Nothing -> return ()
```

This is perfect.
There are no redundant safety checks, and yet, passing unsafe values to sensitive functions will stop our code from compiling.
The user can now use the `SafeString` data type in their own security critical functions.

In securing a toy library, we used the type system to guide our control flow.
All 'incorrect' control flows will be rejected by the type-checker.

This design philosophy of using types to encode logical constraints in a program is popularly called "type driven design".

## Further reading

- [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) - I *highly* recommend this one.
- [Typeful programming](http://www.lucacardelli.name/Papers/TypefulProg.pdf)
- [Designing with types, in F#](https://fsharpforfunandprofit.com/series/designing-with-types/)

## Backmatter

[TODO]

