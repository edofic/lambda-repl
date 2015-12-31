# lambda-repl

This is a toy lambda calculus evaluator built to play around.

Easiest way to use is using [stack](http://haskellstack.org)

    stack build
    stack exec lambda-repl

Or manually configure and set up dependencies using cabal.

## Syntax

Backslash for abstraction and dot as delimiter.

    (\x.x x) (\x.x x)

### Extensions

Added stuff to make it more "practical"

#### Assignment

You can assign names to values for reuse

    Y = \f.(\x.f (x x)) (\x.f (x x))

#### Integers

You can use integer literals

    123

#### Native functions

Addition on integers

    plus 1 2

Equality checking: if first arg is equal to second it evaluates to the third else to the fourth

    eq 0 0 1 2 // evaluates to 1

Y combinator. Not acually a native function, just pre-defined

    sumup = Y (\f.\x.eq x 0 0 (plus x (f (plus x -1))))
