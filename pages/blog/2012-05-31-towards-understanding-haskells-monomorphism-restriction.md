---
title: Towards understanding Haskell's monomorphism restriction
date: 2012-05-31
---

Towards understanding Haskell's monomorphism restriction
========================================================

Haskell has a very mysterious feature - it's not a bug :) - called the
monomorphism restriction. Every Haskell programmer will sooner or later hear
about its existence, but most likely will not stumble upon it in practice. The
explanations of the restriction that I've found so far were either unclear,
imprecise or inaccessible to a beginner. [Page on Haskell
Wiki](http://www.haskell.org/haskellwiki/Monomorphism_Restriction) doesn't
really explain much but instead turns into discussion between people that don't
fully understand the restriction[^1].  [Definition of monomorphism restriction in
Haskell 2010 report](http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-930004.5.5)
is not accessible to a beginner. It relies on knowledge of Haskell standard
(bindings in particular) and some theory behind type inference. The restriction
is also mentioned in [chapter 6 of Real World
Haskell](http://book.realworldhaskell.org/read/using-typeclasses.html#x_H11),
but the book doesn't even attempt to explain it. Only recently, while reading
["A history of Haskell: Being lazy with
class"](http://research.microsoft.com/en-us/um/people/simonpj/papers/history-of-haskell/),
I managed to partially understand what monomorphism restriction is about. Here's
an explanation that is suited for beginners. This is however not a complete
treatment of the subject - I still see some aspects of monomorphism restriction
that are unclear to me.

Let's begin by explaining what is monomorphism and what is polymorphism. If you
come from object-oriented programming community then no doubt you've heard about
polymorphism. That's bad, cause this term means a different thing in Haskell, as
most other terms do. A _term_ in Haskell is called _polymorphic_ if it can be
instantiated to values of different types, depending on the context in which it
is used. A good example is `genericLength` function from the `Data.List`
module. Its type signature is:

```haskell
genericLength :: Num a => [b] -> a
```

which means that `genericLength` returns value of type `a` that can be any type
which is an instance of `Num` type class. Hence, the return value of
`genericLength` is polymorphic. Here's an example:

```
ghci> genericLength [1,2,3] :: Int
3
ghci> genericLength [1,2,3] :: Double
3.0
```

Monomorphism, as you probably guess, is the opposite of polymorphism. A term is
monomorphic when it has a value of concrete type and this type doesn't depend on
term's context.

The term _monomorphism restriction_ means restricting polymorphic terms to a
single type, hence making them monomorphic. Why would anyone like to do that?
Consider this example[^2]:

```haskell
f xs = (len, len)
       where len = genericLength xs
```

From this code it looks that `len` will be computed only once. This would be
true if the expected return type of `f` would be `(Int,Int)` or
`(Double,Double)`. However, note that the result of `genericLength` is
polymorphic and hence it would be reasonable for someone to expect return type
of `f` to be `(Int,Double)`. This would however force the value of `len` to be
computed twice (no type casting in Haskell!). Let me to quote from section 6.2
of "A History of Haskell":

> Hughes argued strongly that it was unacceptable to silently duplicate
> computation in this way. His argument was motivated by a program he had
> written that ran exponentially slower than he expected. (This was admittedly
> with a very simple compiler, but we were reluctant to make performance
> differences as big as this dependent on compiler optimisations.)
>
> Following much debate, the committee adopted the now-notorious monomorphism
> restriction. Stated briefly, it says that a definition that does not look like
> a function (i.e. has no arguments on the left-hand side) should be monomorphic
> in any overloaded type variables. In this example, the rule forces `len` to be
> used at the same type at both its occurrences, which solves the performance
> problem. The programmer can supply an explicit type signature for `len` if
> polymorphic behaviour is required.

And so `len` becomes monomorphic, because it does not look like a function
(takes no parameters) and it is reasonable to expect that it has a concrete type
that doesn't change depending on the context.

Let's play with the `f` function a little bit. We begin by checking it's type
signature inferred by the compiler:

```
ghci> :t f
f :: Num t => [b] -> (t, t)
```

You can notice that both components of the result tuple must be of the same type
`t` that is an instance of `Num` type class. That's how it works in practice:

```
ghci> f [1,2,3] :: (Int,Int)
(3,3)
ghci> f [1,2,3] :: (Double,Double)
(3.0,3.0)
```

Attempt to force a polymorphic return type results in an error:

```
ghci> f [1,2,3] :: (Int,Double)

:1:1:
 Couldn't match expected type `Double' with actual type `Int'
      Expected type: (Int, Double)
        Actual type: (Int, Int)
 In the return type of a call of `f'
 In the expression: f [1, 2, 3] :: (Int, Double)
```

The compiler inferred the `t` to be `Int`, because the first element of a tuple
is declared to be an `Int`, and expects that the second element will also have
type `Int`. When it realizes that it's actually a `Double` it complains. This
does not result with an error message that says anything about monomorphism
restriction, but if I understand correctly - and I could be wrong with this one
- it's the monomorphism restriction that underlies this behaviour. Let's supply
a type signature for `f` that explicitly allows elements of the resulting tuple
to be of different types:

```haskell
f :: (Num b, Num c) => [a] -> (b, c)
f xs = (len, len)
       where len = genericLength xs
```

This definition will not compile producing a horrible error message:

```
Could not deduce (b ~ c)
from the context (Num b, Num c)
bound by the type signature for
      f :: (Num b, Num c) => [a] -> (b, c)
  at restriction.hs:(4,1)-(5,32)
  `b' is a rigid type variable bound by
      the type signature for f :: (Num b, Num c) => [a] -> (b, c)
      at restriction.hs:4:1
  `c' is a rigid type variable bound by
      the type signature for f :: (Num b, Num c) => [a] -> (b, c)
      at restriction.hs:4:1
```

The `(b ~ c)` means that `b` and `c` must be of the same type and the _rigid
type variables_ means that the programmer supplied a concrete type annotation
with the given type variable[^3]. The monomorphism restriction can be disabled
using `LANGUAGE` pragma. Here's the full code:

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
f :: (Num b, Num c) => [a] -> (b, c)
f xs = (len, len)
       where len = genericLength xs
```

This allows `len` to be polymorphic:

```
ghci> f [1,2,3] :: (Int,Double)
(3,3.0)
```

And that's about it. We forced the len function to be computed twice.

As I said in the beginning, I still don't understand everything about the
restriction. In particular I don't understand all the nuances that are in the
Haskell Report. If you take a look at the Haskell Wiki page mentioned earlier,
you'll see some examples of code where you actually get an error message about
the monomorphism restriction:

```haskell
-- This is allowed
f1 x = show x

-- This is not allowed
f2 = \x -> show x

-- ...but this is allowed
f3 :: (Show a) => a -> String
f3 = \x -> show x

-- This is not allowed
f4 = show

-- ...but this is allowed
f5 :: (Show a) => a -> String
f5 = show
```

This is of course consistent with the Haskell Report, which says:

> Anything defined with function syntax usually generalizes as a function is
> expected to. (...) However, the same function defined with pattern syntax
> requires a type signature if f is to be fully overloaded.

It is not yet clear to my whether the avoidance of duplicate computation was the
only motivation behind introducing the restriction. I'm definitely not the only
one that is confused with the monomorphism restriction and I think that this is
somewhat controversial feature:

> The monomorphism restriction is manifestly a wart on the language. It seems to
> bite every new Haskell programmer by giving rise to an unexpected or obscure
> error message. There has been much discussion of alternatives. (...) But in
> all this time, no truly satisfactory alternative has evolved. (from _"A
> History of Haskell"_)
>
> The consensus within the Haskell community is that it doesn't arise often; it
> is tricky to explain; it provides almost no practical benefit; and so it
> mostly serves to trip people up. (from _"Real World Haskell"_)

As a general rule, supplying explicit type signature will always allow to avoid
the restriction. It may be tricky to supply type signature in some cases,
e.g. functions defined locally within the `where` clause may require lexically
scoped type variables (see [here](http://www.haskell.org/haskellwiki/Scoped_type_variables)).

**UPDATE (01/06/2012):** Someone people at
[reddit](http://www.reddit.com/r/haskell/comments/udnx2/understanding_haskells_monomorphism_restriction/)
correctly pointed out that instead of enabling language extension it would be
more natural to deal with the restriction by giving type annotation to `f` (as
said in the last paragraph of the post). Here's the code that works without
language extension:

```haskell
f xs = (len, len)
    where
      len :: (Num a) => a
      len = genericLength xs
```

[^1]: Just to make things clear - I don't claim to fully understand it either.

[^2]: You have to import Data.List module for this to compile

[^3]: See: Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, and
Geoffrey Washburn. _Simple unification-based type inference for GADTs_. In ICFP,
pages 50-61, (2006).

