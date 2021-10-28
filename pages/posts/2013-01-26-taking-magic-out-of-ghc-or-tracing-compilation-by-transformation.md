---
title: Taking magic out of GHC or&#58; Tracing compilation by transformation
date: 2013-01-26
---

Taking magic out of GHC or: Tracing compilation by transformation
=================================================================

When I was planning to learn about compilers I heard one phrase that still
sticks in my mind:

> Taking compilers course is a good thing because it takes magic out of
> compilers.

It may have been put into words differently but this was the meaning. Having
taken compilers course and learning about GHC's internals I think this is so
true.  A compiler is no longer a black box. It's just a very complicated program
built according to some general rules. In this post I want to reveal a little
bit of magic that GHC does behind the scenes when compiling your Haskell
program.

Imagine you are writing an image processing algorithm and you want to check
whether pixel coordinates `x` and `y` are within the image. That's simple: if
any of these coordinates is less than `0` or if `x` equals or exceeds image
width or if `y` equals or exceeds image height than coordinates are not within
the image. Here's how we can express this condition in Haskell:

```haskell
case (x < 0) || (x >= width) || (y < 0) || (y >= height) of
    False -> e1 -- do this when (x,y) is within the image
    True  -> e2 -- do that when (x,y) is outside of image
```

When compiling a Haskell program GHC uses language called Core as an
intermediate representation. It's a very simplified ((We usually call it
"desugared", because simplifying Haskell to Core simply removes syntactic
sugar.)) form of Haskell that has only let and case expressions and type
annotations. You don't need any knowledge of Core to understand this post but if
you want to learn more I suggest to start with [a blog post by Gabriel
Gonzalez](http://www.haskellforall.com/2012/10/hello-core.html) and then take a
look at [Edward Z. Yang's
post](http://blog.ezyang.com/2011/04/tracing-the-compilation-of-hello-factorial/),
that also shows GHC's other intermediate languages: STG and Cmm. You can see
Core representation to which your program was transformed by passing
`-ddump-simpl` flag to GHC (I also use `-dsuppress-al`l to get rid of all type
informations that obscure the output). If you compile above case expression with
optimisations (pass the `-O2` option to GHC) you end up with following Core
representation:

```
case <# x 0# of _ {
  False ->
    case >=# x width of _ {
      False ->
        case <# y 0# of _ {
          False ->
            case >=# y height of _ {
              False -> e1;
              True  -> e2;
            };
          True -> e2;
        };
      True -> e2;
    };
  True -> e2;
}
```

GHC turned our infix comparison operators into prefix notation. It also unboxed
integer variables. This can be noticed by `#` appended to integer literals and
comparison operators. There's also some syntactic change in `case` expressions:
there are braces surrounding the branches, semicolon is used to delimit branches
from each other and there is a mysterious underscore after the keyword `of`. We
can rewrite this in a more familiar Haskell syntax (I will also reverse the
order of `True` and `False` branches - it will be more readable):

```
case x < 0 of
  True  -> e2
  False ->
    case x >= width of
      True  -> e2
      False ->
        case y < 0 of
          True  -> e2
          False ->
            case y >= height of
              True  -> e2
              False -> e1
```

The most noticeable thing however is that our original `case` expression has
suddenly turned into four nested `case`s, which resulted in duplicating
expression `e2` four times. In this post I will show you how GHC arrived at this
representation.

A bit of theory
===============

There are some things you need to know in order to understand how GHC
transformed the code. First is the definition of `(||)` operator (logical or):

```
(||) :: Bool -> Bool -> Bool
(||) x y = case x of
    True  -> True
    False -> y
```

When optimizations are turned on GHC performs inlining of short functions. This
means that function calls are replaced by function definitions and this will be
the case with `(||)` function.

Second thing is case-to-case code transformation. Imagine a code fragment like
this:

```
case (
  case C of
      B1 -> F1
      B2 -> F2
 ) of
    A1 -> E1
    A2 -> E2
```

We have a `case` expression nested within a scrutinee ((A scrutinee is an
expression which value is checked by the the `case` expression. Scrutinee is
placed between words 'case' and 'of'.)) of another `case` expression. You may be
thinking that you would never write such a code and you are right. GHC however
compiles programs by performing subsequent Core-to-Core transformations and such
nesting of `case` expressions is often generated during that process (as we will
see in a moment). If nested `case` expressions appear in the Core representation
of a program they are turned inside out by case-of-case transformation: the
nested `case` scrutinising `C` becomes the outer `case` expression and the outer
case expression is pushed into branches `B1` and `B2`:

```
case C of
    B1 -> case F1 of
              A1 -> E1
              A2 -> E2
    B2 -> case F2 of
              A1 -> E1
              A2 -> E2
```

You see that code for `E1` and `E2` has been duplicated. This is worst case
scenario. In real life programs one of the branches can often be simplified
using case-of-known-constructor transformation. See what happens when expression
returned by a branch of nested case is a constructor that is matched by outer
case (`A1` in this example):

```
case (
  case C of
      B1 -> A1
      B2 -> F2
 ) of
    A1 -> E1
    A2 -> E2
```

After performing case-of-case transformation we end up with:

```
case C of
    B1 -> case A1 of
              A1 -> E1
              A2 -> E2
    B2 -> case F2 of
              A1 -> E1
              A2 -> E2
```

In the first branch of outer case expression we are now matching `A1` against
`A1`. So we know that first branch will be taken and thus can get rid of this
`case` expression reducing it to `E1`:

```
case C of
    B1 -> E1
    B2 -> case F2 of
              A1 -> E1
              A2 -> E2
```

Thus only `E1` was duplicated. We will see that happen a lot in a moment.

The fun begins
==============

Knowing all this we can begin optimizing our code:

```haskell
case (x < 0) || (x >= width) || (y < 0) || (y >= height) of
    False -> e1
    True  -> e2
```

First thing that happens is inlining of `(||)` operators. The call to `(x < 0)
|| (x >= width)` is replaced by definition of `(||)`:

```
case (
    case (x < 0) of  -- this case comes from definition of ||
        True  -> True
        False -> (x >= width)
    ) || (y < 0) || (y >= height) of
    False -> e1
    True  -> e2
```

Let's inline next use of `(||)`:

```
case (
    case (  -- this case is introduced by inlining of second ||
        case (x < 0) of
            True  -> True
            False -> (x >= width)
        ) of
        True  -> True
        False -> (y < 0)
    ) || (y >= height) of
    False -> e1
    True  -> e2
```

One more inlining and where done with `(||)`:

```
case (
    case (  -- this case is introduced by inlining of last ||
        case (
            case (x < 0) of
                True  -> True
                False -> (x >= width)
            ) of
            True  -> True
            False -> (y < 0)
        ) of
        True  -> True
        False -> (y >= height)
    ) of
    False -> e1
    True  -> e2
```

We ended up with three `case`s nested as scrutinees of other `case`s - I told
you this will happen. Now GHC will start applying case-of-case transformation to
get rid of all this nesting. Let's focus on two most internal `case`s for
simplicity:

```
case (
    case (x < 0) of
        True  -> True
        False -> (x >= width)
    ) of
    True  -> True
    False -> (y < 0)
```

Performing case-of-case transformation on them gives:

```
case (x > 0) of  -- nested case is floated out
    True ->
        case True of  -- outer case is pushed into this branch...
            True  -> True
            False -> (y < 0)
    False ->
        case (x >= width) of -- ...and into this branch
            True  -> True
            False -> (y < 0)
```

Looking at first nested `case` we see that case-of-known-constructor
transformation can be applied:

```
case (x < 0) of
    True  -> True  -- case-of-known-constructor eliminated
                   -- case expression in this branch
    False ->
        case (x >= width) of
            True  -> True
            False -> (y < 0)
```

Now let's put these `case`s back into our expression:

```
case (
    case (
        case (x < 0) of
            True  -> True
            False ->
                case (x >= width) of
                    True  -> True
                    False -> (y < 0)
        ) of
        True  -> True
        False -> (y >= height)
    ) of
    False -> e1
    True  -> e2
```

Now we only have two `case`s nested as scrutinees of other `case`. Applying
case-of-case one more time will get rid of the first nesting:

```
case (
    case (x < 0) of
        True ->  -- we can use case-of-known-constructor here
            case True of
                True  -> True
                False -> (y >= height)
        False ->
            case ( -- these nested cases weren't here before!
                case (x >= width) of
                    True -> True
                    False -> (y < 0)
                ) of
                True  -> True
                False -> (y >= height)
    ) of
    False -> e1
    True  -> e2
```

Hey, that's something new here! We eliminated nested `case`s in one place only
to introduce them in another. But we know what to do with nested `case`s - use
case-of-case of course. Let's apply it to the second branch and
case-of-known-constructor to the first one:

```
case (
    case (x < 0) of
        True  -> True  -- case-of-known-constructor used here
        False ->
            case (x >= width) of    -- case-of-case used here
                True ->
                    case True of    -- what about this case?
                        True  -> True
                        False -> (y >= height)
                False ->
                    case (y < 0) of
                        True  -> True
                        False -> (y >= height)
    ) of
    False -> e1
    True  -> e2
```

We just got another chance to perform case-of-known-constructor:

```
case (
    case (x < 0) of
        True  -> True
        False ->
            case (x >= width) of
                True  -> True  -- case-of-known-constructor
                False ->
                   case (y < 0) of
                       True -> True
                       False -> (y >= height)
    ) of
    False -> e1
    True  -> e2
```

We have on more nested `case` to eliminate. Let's hit it:

```
case (x < 0) of
    True ->
        case True of
            False -> e1
            True  -> e2
    False ->
        case (
            case (x >= width) of
                True  -> True
                False ->
                    case (y < 0) of
                        True -> True
                        False -> (y >= height)
                ) of
                False -> e1
                True  -> e2
```

Do you see how expressions `e1` and `e2` got duplicated? Let's apply
case-of-known-constructor in the first branch and case-of-case +
case-of-known-constructor in the second one:

```
case (x < 0) of
    True  -> e2
    False ->
        case (x >= width) of
            True  -> e2
            False ->
                case (
                    case (y < 0) of
                        True  -> True
                        False -> (y >= height)
                    ) of
                    False -> e1
                    True  -> e2
```

One more case-of-case:

```
case (x < 0) of
    True  -> e2
    False ->
        case (x >= width) of
            True  -> e2
            False ->
                case (y < 0) of
                    True ->
                        case True of
                            False -> e1
                            True  -> e2
                    False ->
                        case (y >= height) of
                            False -> e1
                            True  -> e2
```

And one more case-of-known-constructor:

```
case (x < 0) of
    True  -> e2
    False ->
         case (x >= width) of
             True  -> e2
             False ->
                 case (y < 0) of
                     True  -> e2
                     False ->
                         case (y >= height) of
                             False -> e1
                             True  -> e2
```

And we're done! We arrived at the same expression that GHC compiled. Wasn't that
simple?

Summary
=======

This should give you an idea of how GHC's core-to-core transformations
work. I've only shown you two of them - case-of-case and
case-of-known-constructor - but there are many more. If you're interested in
learning others take a look at paper by Simon Peyton Jones and Andre Satnos ["A
transformation-based optimiser for
Haskell"](http://research.microsoft.com/pubs/67064/comp-by-trans-scp.ps.gz). If
you want to learn more details than the paper provides see Andre Santos' PhD
thesis ["Compilation by Transformation in Non-Strict Functional
Languages"](http://research.microsoft.com/en-us/um/people/simonpj/papers/santos-thesis.ps.gz).
You can also take a look at a discussion at GHC [ticket
6135](http://hackage.haskell.org/trac/ghc/ticket/6135).

