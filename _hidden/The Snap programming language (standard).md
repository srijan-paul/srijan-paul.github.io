Snap is a statically typed programming language with support for gradual typing.

Goals:
- Powerful type system with type inference, ADTs, and pattern matching
## Grammar
### Lexical grammar
Snap has the following keywords:
```
let, const, mut, return, if, else, match, fun, nil.
```
And the following operators:
```
Integer/Float arithmetic: + - / * % | & >> << ~ > < >= <= **
Boolean operators: ! and or && ||
String operators: <>
Compound assignment: += -= *= %= /=
Arrays: []
```

### Semantics

- Eager evaluation
- Full type inference.
- Algebraic data types and pattern matching.
- type traits.
## Type traits

```
trait Semigroup m
	mappend :: m -> m -> m

trait Semigroup m => Monoid m
	mempty :: m
```

```
import "std:List" as List

impl Semigroup List[Int] where
	mappend :: List[Int] -> List[Int] -> List[Int]
	mappend = List.concat

impl Monoid List[Int] where
	mempty :: List[Int]
	mempty = []
```