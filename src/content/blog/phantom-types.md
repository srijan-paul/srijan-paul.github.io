In most compiled languages, code compilation offers two guarantees.
First is the guarnatee that our code is syntactically valid.
The second guarantee is that our code is soundly typed, and does not have any semantic errors
that may break the language's execution logic, like adding an `Int` with a `String`.

In this post, I'll present a way to offer a third guarantee -
one that is surprisingly good in keeping logical errors at bay.
With meticulous use of Haskell's type system,
we can deter some logic mistakes that are easy to make in imperative languages.

The core idea is making invalid states un-representable, and using types to guide our program's logic.
If that does not make sense to you now, it will after we go over an example.

In a follow-up post, I will show an elegant way of building type-level proofs that ensure
logical correctness of programs.

## Phantom types

In Haskell, polymorphic types can have unused type parameters.
The unused parameters are called [Phantom types](https://wiki.haskell.org/Phantom_type).
For example:

```haskell
newtype Attach a ph = Attach a
```

The Type `Attach` has a data constructor of the same name.
When you call `Attach v` – where `v` is some value of type `a` –
it will construct a type that wraps `v` inside it (`Attach a ph`).

But what is the `ph` type?
The data constructor does not provide `ph`, so where does it come from?

Let's spin up a REPL and find out:

```haskell
ghci> x = Attach "x"
ghci> :t Attach x
Attach "x" :: forall {k} {ph :: k}. Spooky String ph
```

The variable `x` has a polymorphic type where the type `ph` is unspecified, and can be anything.
We can use an explicit annotation to get the type to adapt to any valid signature:

```haskell
ghci> y = x :: Attach String Int
ghci> :t y
y :: Attach String Int
```

However, once `y` is assigned a monomorphic type, it cannot be narrowed down further.
The `Attach String Int` is like a permanent tattoo that identifies `y` as a value of type `String`,
that has the ghost of an `Int` latching onto it.

As it turns out, phantom types are incredibly useful in type-driven development.
We can use them to represent invariants in our programs, and stop logically incorrect code from compiling.

## Case study: A database library 

Imagine you're authoring a backend library that offers – among other things – the following capabilities:

1. Fetching data from any URL.
2. Running database queries.
3. Logging timestamped strings (like HTTP requests, DB queries, etc.) to the console for monitoring purposes.

To keep things simple, we will ignore the implementation details and only focus on type signatures.
Say our library has the following structure:

```haskell
-- file: Lib.hs
-- All function bodies have been replaced with
-- simple one-liners for simplicity.
module Lib ( runDbQuery, log, fetchData ) where

-- run a database query on some imaginary database client
runDbQuery :: String -> IO ()
runDbQuery _ = return ()

-- log input to the console with a time-stamp.
log :: String -> IO ()
log _ = return ()

-- fetch data from url
fetchData :: URL -> IO String
fetchData _ = return ()
```

Allowing arbitrary strings to be interpreted as database commands is an obvious security hazard.
Any string from an unsafe source (like an HTTP request parameter, a third party web-server, etc.)
is a potential [SQL injection](https://en.wikipedia.org/wiki/SQL_injection) attempt.

Even logging such strings to the console can open us up to [log injection](https://owasp.org/www-community/attacks/Log_Injection).

These threats now posit a requirement: A string must be validated for safety before being passed to `runDbQuery` and `log`.
To avoid getting pwned, we can implement a validation function that checks if the input is secure for consumption:

```haskell
-- file: Lib.hs
isSafe :: String -> Bool
isSafe input = undefined
```

`isSafe` returns `True` only if `input` is safe for consumption by the other functions that our library exports.
Now, we are faced with a design challenge: Who gets to call `isSafe`? And where is it called?
Two approaches come to mind:

The first is where the user calls `isSafe` to verify a string's safety before passing it to `runDbQuery` or `log`.
So the user would have to perform checks in the following manner:

```haskell
-- file: Main.hs
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
Incorrect use of our library still compiles and spawns a vulnerable application binary:

```haskell
-- file: Main.hs

main =  do
  query <- getLine
  log query -- PWNED
  runDbQuery query -- PWNED x2
```

In a production application, there will be multiple places to draw tainted strings from.
It's not uncommon for developers to forget sanitizing the tainted input.
Even if the code is written perfectly in the beginning,
someone might erroneously remove a call to `isSafe` during a refactor.
Users of our library are now one line of code away from a gazillion dollar AWS bill.
When something breaks because of tainted data having made it to a security critical function call,
it's the consumers of `Lib` who're held guilty.

Our design relies on human memory to perform data validation.
Any application using such a library is doomed to break before the user has even begun working on the project.

The possible code paths that unsafe data can take are simply too many, and it's irresponsible
for us to expect users to routinely call `isSafe` before every sensitive function call.

A judicious library author should avoid transfering this burden of validation to users.

An alternative design choice, therefore, is to call `isSafe` on the user's behalf:

```haskell
-- file: Lib.hs
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
Even when the user has taken steps to ensure that the input is safe, we're performing redundant checks.
Moreover, passing the same string to `runDbQuery` and `log` will perform the `isSafe` check twice.

Sometimes, validation isn't necessary because the input comes from within the program.
In the example below for instance, `input` is a string literal, and yet it gets validated twice:

```haskell
-- file: Main.hs
main = do
  let input = "insert into fruits value ('mango')"
  runDbQuery input -- Will run `isSafe` unneccessarily.
  log input        -- will run `isSafe` again.
```

And what if we forget to call `isSafe` when writing the library code?
Afterall, the possible code-paths in a real library massively outnumber our toy example.
As seen earlier, the library still compiles,
and we condemn our users to eternal pwnage from script kiddies on internet[dot]com.

In a way, this approach is *worse*.
Not only does it incur a higher performance cost,
it still runs the risk of someone (read: us) forgetting to
call `isSafe` before shipping our library.

There is another issue with this desing which isn't apparent immediately: the functions aren't total.
Currently, the functions `log` and `runDbQuery` are partial –
they simply reject some values in their domain by erroring out.

Some strings will be logged  successfully, while others will be rejected with an error (or a void return).
Our users, then, have to carefully handle these scenarios.

When writing functional code, it is ideal to prefer total functions wherever possible.
This prevents the user from having to handle cases where our functions bail out, and
the function can be expected to behave well for any input that satisfies its type signature.

Sadly, no matter which design choice we make, there will be unwelcome scenarios.

The conundrum we now face is very common in API design.
Our library exports functions (called "data sinks") that expect *valid*, well-formed data;
A validator is also exposed, to check if some data is well-formed.
We want to **ensure that all input provided to a sink has been routed through a validator function**.

In a more abstract sense, the problem can be restated as follows:
"Any data that is passed to `f`, must first be passed to `g`".

In our use-case, `f` is `runDbQuery`, and `g` is `isSafe`.

Ideally, we want our solution to have as little overhead as possible.

## Phantom tags as proofs

There is a third approach that has ideal performance, doesn't allow insecure code to compile, and ensures totality.
It turns out, we can leverage the type system (and some data hiding)
to ensure that all arguments to `log` and `runDbQuery` have been routed to `isSafe` first.

The core idea is to introduce a new validator that "tags" the input as safe.
This "tag" is going to be a unique type that can only be produced by our validation function.

Other library functions will have their type signatures updated to only accept values that are accompanied with a type-level `tag` to attest for safety.
If a tag-less raw type is passed to one of these functions, the code will refuse to compile.

A 'tagged' value is expressed using a phantom type that sticks to it:

```haskell
-- file: Lib.hs
module Lib (
  -- ... other exports,
  SafeString
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

Since the `Safe` data-type is not exported by `Lib`, our users cannot create values of the type `Tagged a Safe` by fiat.
The only way to obtain a value of type `SafeString` is by calling `validate`.

Finally, we update the type signatures for all sinks to only accept `SafeString`s:

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
-- file: Main.hs
main = do
  input' <- getLine
  forM_ (validate input') $ \input->  do
    log input
    runDbQuery input 
```

Since the return type of `validate` is a `Maybe` monad, we use `forM_` to handle the `Just` case.
The above code is equivalent to:

```haskell
case validate input' of
  Just input -> do
    log input
    runDbQuery input
  Nothing -> return ()
```

This is perfect.
There are no redundant safety checks, and yet, passing unsafe values to sensitive functions will stop our code from compiling.
The user can now use the `SafeString` data type in their own security critical functions.

In securing a toy library, we used the type system to guide our control flow.
All 'incorrect' control flows will be rejected by the type-checker.
This design philosophy of using types to encode logical constraints in a program is popularly called "type driven design".

As it turns out, we can generalize this idea of checking for preconditions using type-level proofs.
In fact, we can allow the user to combine proofs to verify other properties about a program.
And if you're feeling adventurous, you can have an embedded DSL that operates on proofs to stop invalid program states from compiling.

I'll explore these ideas in a follow-up post.
Until then, I leave you with some resources to study type-driven design.

## Further reading

- [Alexis Kling: Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/) - I *highly* recommend this one.
- [Typeful programming](http://www.lucacardelli.name/Papers/TypefulProg.pdf)
- [Designing with types, in F#](https://fsharpforfunandprofit.com/series/designing-with-types/)

## Backmatter

[TODO]

