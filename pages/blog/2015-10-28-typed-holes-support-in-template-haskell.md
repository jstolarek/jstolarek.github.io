---
title: Typed holes support in Template Haskell
date: 2015-10-28
---

Typed holes support in Template Haskell
=======================================

Back in April I found myself in a need for typed holes in Template Haskell. To
my disappointment it turned out that typed holes are not implemented in TH.
Sadly, this happens too often: a feature is added to GHC but no Template Haskell
support is implemented for it. This was the time when I was working on injective
type families and I already had some experience in extending TH
implementation. I figured that adding support for typed holes should be a
trivial task, no more than 30 minutes of coding. I created a [feature request on
Trac](https://ghc.haskell.org/trac/ghc/ticket/10267) and started coding. I
quickly realized that it won't be that simple. Not that the amount of required
work was that extensive. I simply tripped over the way GHC handles names
internally. As a result the work got stalled for several months and I only
finished it two weeks ago thanks to help from Richard Eisenberg.

My patch allows you to do several interesting things. Firstly, it allows to
quote typed holes, ie. expressions with name starting with an underscore:

```haskell
[d| i :: a -> a
    i x = _ |]
```

This declaration quote will represent `_` using an `UnboundVarE`
constructor. Secondly, you can now splice unbound variables:

```haskell
i :: a -> a
i x = $( return $ VarE (mkName "_") )

j :: a -> a
j x = $( return $ UnboundVarE (mkName "_") )
```

Notice that in a splice you can use either `VarE` or `UnboundVarE` to represent
an unbound variable - they are treated the same.

A very important side-effect of my implementation is that you can actually quote
unbound variables. This means that you can now use nested pattern splices, as
demonstrated by one of the tests in GHC testsuite:

```haskell
baz = [| \ $( return $ VarP $ mkName "x" ) -> x |]
```

Previously this code was rejected. The reason is that:

  1. nested pattern splice is not compiled immediately, because it is possible
     that it refers to local variables defined outside of the bracket;

  2. the bracket is renamed immediately at the declaration site and all the
     variables were required to be in scope at that time.

The combination of the above means that the pattern splice does not bring
anything into scope (because it is not compiled until the outer bracket is
spliced in), which lead to `x` being out of scope. But now it is perfectly fine
to have unbound variables in a bracket. So the above definition of `baz` is now
accepted. When it is first renamed `x` is treated as an unbound variable, which
is now fine, and when the bracket is spliced in, the inner splice is compiled
and it correctly brings binding for `x` into scope. Getting nested pattern
splices to work was not my intention when I started implementing this patch but
it turned out we essentially got this feature for free.

One stumbling block during my work was typed Template Haskell. With normal,
untyped TH I can place a splice at top-level in a file:

```haskell
$$(return [
   SigD (mkName "m")
        (ForallT [PlainTV (mkName "a")]
                 []
                 (AppT (AppT ArrowT (VarT (mkName "a"))) (VarT (mkName "a"))))
 , FunD (mkName "m")
        [Clause [VarP (mkName "x")] (NormalB (VarE (mkName "x"))) [] ]
   ])
```

and this will build a definition that will be spliced into the source code. But
converting this into a typed splice, by saying `$$(return ....`, resulted in
compiler panic. I reported this as
[#10945](https://ghc.haskell.org/trac/ghc/ticket/10945). The reason turned out
to be quite tricky. When Template Haskell is enabled, top-level expressions are
allowed. Each such expression is treated as an implicit splice. The problem with
typed TH splice is that it doesn't really make sense at the top-level and it
should be treated as an implicit splice. Yet it was treated as an explicit
splice, which resulted in a panic later in the compiler pipeline.

Another issue that came up with typed TH was that typed holes cannot be quoted,
again leading to panic. I reported this as
[#10946](https://ghc.haskell.org/trac/ghc/ticket/10946).  This issue has not yet
been solved.

The above work is now [merged with
HEAD](https://git.haskell.org/ghc.git/commitdiff/75492e7467ff962f2f2e29e5c8b2c588c94ae8a7)
and will be available in GHC 8.0.

