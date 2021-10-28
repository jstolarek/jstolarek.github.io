---
title: Injective type families for Haskell
date: 2015-05-26
---

Injective type families for Haskell
===================================

For the last few months I have been working on extending Glasgow Haskell
Compiler with injective type families. At first this seemed like a fairly simple
project but in the end it turned out to be much more interesting than initially
thought. (I suppose that's how research mostly turns out.) There are still some
rough edges in the implementation and it will take a few more weeks before my
branch gets merged into the master development branch of GHC. But for now there
is [a draft paper "Injective type families for
Haskell"](http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_peyton-jones_eisenberg_injectivity.pdf)
written by me, Simon Peyton Jones, and Richard Eisenberg, that we submitted for
this year's Haskell Symposium. This is not yet the final version of the paper so
any feedback will be appreciated.

The idea behind injective type families is to infer the arguments of a type
family from the result. For example, given a definition:

```haskell
type family F a = r | r -> a where
    F Char = Bool
    F Bool = Char
    F a    = a
```

if we know `(F a ~ Bool)`[^1] then we want to infer `(a ~ Char)`. And if we know
`(F a ~ Double)` then we want to infer `(a ~ Double)`. Going one step further
from this, if we know `(F a ~ F b)` then - knowing that `F` is injective - we
want to infer `(a ~ b)`.

Notice that in order to declare `F` as injective I used new syntax. Firstly, I
used "`= r`" to introduce a name for the result returned by the type
family. Secondly, I used syntax borrowed from functional dependencies to declare
injectivity. For multi-argument type families this syntax allows to declare
injectivity in only some of the arguments, e.g.:

```haskell
type family G a b c = r | r -> a c
```

Actually, you can even have kind injectivity, assuming that type arguments have
polymorphic kinds.

Obviously, to make use of injectivity declared by the user GHC needs to check
that the injectivity annotation is true. And that's the really tricky part that
the paper focuses on. Here's an example:

```haskell
type family T a = r | r -> a where
    T [a] = a
```

This type family returns the type of elements stored in a list. It certainly
looks injective. Surprisingly, it is not. Say we have `(T [T Int])`. By the only
equation of `T` this gives us `(T [T Int] ~ T Int)`. And by injectivity we have
`([T Int] ~ Int)`. We just proved that lists and integers are equal, which is a
disaster.

The above is only a short teaser. The paper covers much more: more corner cases,
our algorithm for verifying user's injectivity annotations, details of
exploiting knowledge of injectivity inside the compiler and relationship of
injective type families to functional dependencies. [Extended version of the
paper](http://ics.p.lodz.pl/~stolarek/_media/pl:research:stolarek_peyton-jones_eisenberg_injectivity_extended.pdf)
also comes with proofs of soundness and completeness of our algorithm.

[^1]: `~` means unification. Think of "`~`" as "having a proof that two types
      are equal".

