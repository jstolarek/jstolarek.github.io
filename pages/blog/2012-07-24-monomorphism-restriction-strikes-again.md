---
title: Monomorphism restriction strikes again
date: 2012-07-24
---

Monomorphism restriction strikes again
======================================

A while ago [I blogged about monomorphsim
restriction](2012-05-31-towards-understanding-haskells-monomorphism-restriction/).
I thought I understood most of it, but just yesterday I was surprisingly hit by
it. I was working my way through [chapter 9 of Real World
Haskell](http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html).
In this chapter authors present a case study of a library that is designed for
searching the file system. The central point in the design of this library are
predicates that are used to perform queries. A predicate function is given some
information about the file system entry: path, permissions (they allow to
determine if the entry is a file or a directory), size and modification
time. Based on that information a predicate function can return some
information, e.g. name of the file or `Bool` value stating if the file meets
some given criteria. To save a bit of typing RWH authors introduce a type
synonym:

```haskell
import System.Directory (Permissions(..))
import System.Time (ClockTime(..))

type InfoP a =  FilePath
             -> Permissions
             -> Maybe Integer
             -> ClockTime
             -> a
```

'P' at the end of name stands for 'predicate'. As you can see this type defines
a function type with polymorphic return value acting as type parameter. Here's
an example of usage:

```haskell
pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1
```

These functions retrieve path to a file and its size, respectively. We'd
certainly would like to do something with those information. We might want to
know if the file is larger than some specified size. We might also want to join
predicates using logical AND and OR. Here's an example:

```haskell
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k
```

This function takes a predicate and a value and compares them for equality,
e.g. `equalP sizeP 1024`. This can be generalized to any comparison operator by
introducing additional parameter (( My code slightly differs from what you'll
find in RWH)) :

```haskell
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k = \w x y z -> f w x y z \`q\` k

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP = liftP (==)
```

So `liftP` takes an operator `q` (infix notation is used to show that
explicitly) and applies it to the result of `f` and `k`. Note that both `liftP`
and previous version of `equalP` return anonymous functions that require four
more parameters to yield a result. So far so good. We can easily define other
predicates like `lesserP` and `greaterP` only by passing different comparison
operator to `liftP`. The `liftP` function can be further generalized to allow
logical operators:

```
liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g = \w x y z -> f w x y z \`q\` g w x y z

andP = liftP2 (&&)
orP  = liftP2 (||)
```

As you can see `liftP` and `liftP2` are very similar and in fact the former one
can be written in the terms of the latter:

```
constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k = liftP2 q f (constP k)
```

This part was very confusing to me at first, but it becomes clear when you
replace the InfoP type synonym in constP type declaration with actual type:

```
constP :: a -> FilePath -> Permissions -> Maybe Integer -> ClockTime -> a
constP k _ _ _ _ = k
```

Now it becomes clear that `constP` takes a constant and four other parameters
for the predicate, discards these parameters four and returns the constant.

Now comes the key part. Notice that logical predicates `andP` and `orP` were
written without type signature. That's perfectly fine. However, omitting type
signature for `equalP` function causes an error:

```
Ambiguous type variable \`b0' in the constraint:
  (Eq b0) arising from a use of \`=='
Possible cause: the monomorphism restriction applied to the following:
  equalP :: InfoP b0 -> b0 -> InfoP Bool (bound at rwh9.hs:20:1)
Probable fix: give these definition(s) an explicit type signature
              or use -XNoMonomorphismRestriction
In the first argument of \`liftP', namely \`(==)'
In the expression: liftP (==)
In an equation for \`equalP': equalP = liftP (==)
```

I'm not sure why this happens when we try to lift `(==)`, but doesn't happen
when `(&&)` is lifted in same manner. It seems that this happens because `(==)`
operator introduces additional type class constraints (`Eq` type class), but I
don't see why these constraints would cause monomorphsim restriction to kick
in. These mystery is yet to be solved.

