---
title: Smarter conditionals with dependent types&#58; a quick case study
date: 2015-04-30
---

Smarter conditionals with dependent types: a quick case study
=============================================================

> Find the type error in the following Haskell expression:
>
> if null xs then tail xs else xs
>
> You can't, of course: this program is obviously nonsense unless you're a
> typechecker. The trouble is that only certain computations make sense if the
> `null xs` test is `True`, whilst others make sense if it is `False`. However,
> as far as the type system is concerned, the type of the then branch is the
> type of the else branch is the type of the entire conditional. Statically, the
> test is irrelevant. Which is odd, because if the test really were irrelevant,
> we wouldn't do it. Of course, `tail []` doesn't go wrong - well-typed programs
> don't go wrong - so we'd better pick a different word for the way they do go.

The above quote is an opening paragraph of Conor McBride's "Epigram: Practical
Programming with Dependent Types" paper. As always, Conor makes a good point -
this test is completely irrelevant for the typechecker although it is very
relevant at run time. Clearly the type system fails to accurately approximate
runtime behaviour of our program. In this short post I will show how to fix this
in Haskell using dependent types.

The problem is that the types used in this short program carry no information
about the manipulated data. This is true both for `Bool` returned by `null xs`,
which contains no evidence of the result, as well as lists, that store no
information about their length. As some of you probably realize the latter is
easily fixed by using vectors, ie. length-indexed lists:

```haskell
data N = Z | S N  -- natural numbers

data Vec a (n :: N) where
  Nil  :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)
```

The type of vector encodes its length, which means that the type checker can now
be aware whether it is dealing with an empty vector. Now let's write `null` and
`tail` functions that work on vectors:

```haskell
vecNull :: Vec a n -> Bool
vecNull Nil        = True
vecNull (Cons _ _) = False

vecTail :: Vec a (S n) -> Vec a n
vecTail (Cons _ tl) = tl
```

`vecNull` is nothing surprising - it returns `True` for empty vector and `False`
for non-empty one. But the tail function for vectors differs from its
implementation for lists. `tail` from Haskell's standard prelude is not defined
for an empty list so calling `tail []` results in an exception (that would be
the case in Conor's example). But the type signature of `vecTail` requires that
input vector is non-empty. As a result we can rule out the `Nil` case. That also
means that Conor's example will no longer typecheck (( Assuming we don't abuse
Haskell's unsoundness as logic, eg. by using `undefined`. )). But how can we
write a correct version of this example, one that removes first element of a
vector only when it is non-empty? Here's an attempt:

```haskell
shorten :: Vec a n -> Vec a m
shorten xs = case vecNull xs of
               True  -> xs
               False -> vecTail xs
```

That however won't compile: now that we written type-safe tail function
typechecker requires a proof that vector passed to it as an argument is non
empty. The weak link in this code is the `vecNull` function. It tests whether a
vector is empty but delivers no type-level proof of the result. In other words
we need:

```
vecNull' :: Vec a n -> IsNull n
```

ie. a function with result type carrying the information about the length of the
list. This data type will have the runtime representation isomorphic to `Bool`,
ie. it will be an enumeration with two constructors, and the type index will
correspond to length of a vector:

```haskell
data IsNull (n :: N) where
     Null    :: IsNull Z
     NotNull :: IsNull (S n)
```

`Null` represents empty vectors, `NotNull` represents non-empty ones. We can now
implement a version of `vecNull` that carries proof of the result at the type
level:

```haskell
vecNull' :: Vec a n -> IsNull n
vecNull' Nil        = Null
vecNull' (Cons _ _) = NotNull
```

The type signature of ``vecNull` `` says that the return type must have the same
index as the input vector. Pattern matching on the `Nil` case provides the type
checker with the information that the `n` index of `Vec` is `Z`. This means that
the return value in this case must be `Null` - the `NotNull` constructor is
indexed with `S` and that obviously does not match `Z`. Similarly in the `Cons`
case the return value must be `NotNull`. However, replacing `vecNull` in the
definition of `shorten` with our new ``vecNull` `` will again result in a type
error. The problem comes from the type signature of `shorten`:

```haskell
shorten :: Vec a n -> Vec a m
```

By indexing input and output vectors with different length indices (`n` and `m`)
we tell the typechecker that these are completely unrelated. But that is not
true! Knowing the input length `n` we know exactly what the result should be: if
the input vector is empty the result vector is also empty; if the input vector
is not empty it should be shortened by one. Since we need to express this at the
type level we will use a type family:

```haskell
type family Pred (n :: N) :: N where
    Pred Z     = Z
    Pred (S n) = n
```

(In a fully-fledged dependently-typed language we would write normal function
and then apply it at the type level.) Now we can finally write:

```
shorten :: Vec a n -> Vec a (Pred n)
shorten xs = case vecNull' xs of
               Null    -> xs
               NotNull -> vecTail xs
```

This definition should not go wrong. Trying to swap expression in the branches
will result in a type error.

