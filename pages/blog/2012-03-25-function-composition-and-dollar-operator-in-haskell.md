---
title: Function composition and $ operator in Haskell
date: 2012-03-25
---

Function composition and $ operator in Haskell
==============================================

Today I was trying to understand how function application, composition and $
work in Haskell. Consider two following lines of code:

```haskell
take 3 (reverse (filter even [1..10]))
take 3 . reverse . filter even $ [1..10]
```

These are equivalent and both produce a list `[10, 8, 6]`. First version is
based on grouping function calls with parentheses and this is pretty
straightforward. My problem was the second version which is based on function
composition and a special function application operator, denoted as `.` (dot)
and `$` respectively. This form is often used to write functions in a [pointfree
style](http://www.haskell.org/haskellwiki/Pointfree)[^1]. I was trying
to work out in what order function calls are executed and why do I have to use $
operator. Here's what I came up with.

The function composition (dot) operator is defined as:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f (g x)
```

This operator has priority of 9 and is right-associative[^2]. This means that `a
. b . c . d` is the same as `(a . (b . (c . d)))`. The `$` operator is defined
as:

```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

This operator simply applies a function to a given parameter. In contrast to
standard function application, which has highest possible priority of 10 and is
left-associative, the $ operator has priority of 0 and is right-associative
(that second property doesn't matter in my example). Such a low priority means
that all other operators on both sides of `$` will be evaluated before applying
the `$`. So the call

```haskell
take 3 . reverse . filter even $ [1..10]
```

will first evaluate `take 3 . reverse . filter even` constructing a partially
applied function that becomes fully applied when it receives one more
parameter. The missing parameter is the list on the right side of `$`. By
definition of dot operator, this call is therefore equivalent to `take 3
(reverse (filter even [1..10]))`. That's what we expected.

Why do we need the `$` operator? If it wasn't there then the function call
`filter even [1..10]` would evaluate first - remember that function application
has priority of 10, while function composition has a lower priority of 9. This
would lead to `take 3 . reverse . [2,4,6,8,10]`, but a list cannot be composed
with a function. The dot operator expects it's second argument to be a function
of one argument, not a list, and that's the reason we need `$` - to allow
function composition to evaluate first.

[^1]: The name "pointfree" doesn't come from the point operator used to compose
functions.

[^2]: You can verify this by typing `:i (.)` in ghci. This will display the type
definition, fixity (infixr in that case) and priority of the dot operator.

