---
title: Why foldr works for infinite lists and foldl doesn't
date: 2012-09-20
---

Why foldr works for infinite lists and foldl doesn't
====================================================

About two months ago I wrote a post about [expressing `foldl` in terms of
`foldr`](/blog/2012-07-30-expressing-foldl-in-terms-of-foldr.html).  Back then I
left one question open - why does `foldr` work for infinite lists, while `foldl`
doesn't?  I finally found some time sit down and find the answer.

First of all the fact that `foldl` doesn't work for infinite lists, while
`foldr` does, was counter-intuitive for me. I thought that since `foldl`
consumes the list from the beginning it should be able to stop at some point and
return the result. On the other hand `foldr` was explained to me as consuming a
list from the right, that is from the end. Since infinite lists have no end it
seemed to me that `foldr` shouldn't be able to handle infinite lists.

Here are the definitions of folds from Haskell 2010 Language Report:

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
```

These two definitions supported my incorrect intuition. After all they show
clearly that `foldl` processes the first argument of a list immediately, while
`foldr` needs to process whole list with `foldr` before it can pass the result
to `f`. Or at least I thought that they show this.

As I already said all the above intuitions are wrong. My mistake became clear to
me when I explicitly wrote how recursion looks for each fold. For `foldl` the
recursion is:

```
f (... (f ( f (f z x1) x2) x3) ...) xn
```

For `foldr` recursion looks like this:

```
f x1 (f x2 (f x3 (...(f xn z) ...)))
```

Now you can see that for `foldl` you need to get to the end of the list to make
the most outer call. In case of `foldr` you need the first element of the list
and the result of processing the rest of the list with `foldr`. Unless you can
determine the value of `f` without the need for its second parameter! This is in
fact the case for some operators, logical conjunction for example - if first
parameter is False then we can conclude that the whole expression is False,
without the need to evaluate the second argument. Therefore `foldr` will work
for infinite lists if the accumulating function is lazy in its second
argument. One might ask if `foldl` will work for infinite lists if the
accumulating function is lazy in its first argument. The answer is no - you
still need the last element of a list to calculate the value of first call and
there is no last element for infinite lists.

Looking at the fold definitions given earlier I made one embarrassing
omission. Recursion in `foldl` is unconditional! The recursive call is being
made no matter what. The only way to stop the recursion is getting to the end of
a list, but for infinite lists this ain't gonna happen. In case of `foldr`
recursion is conditional - it depends on the first argument of `f`, assuming of
course that `f` is lazy for its second argument. Moreover, looking at the
implementation of `foldr` given above you can see that it in fact works from the
left! My intuition about `foldr` was so fixed on it consuming the list from the
right that I even missed this obvious fact when writing this post. Big thanks to
nh2, who pointed that out in his comment. So in the end _consuming a list from
the right_ is about grouping of terms with parentheses.

As a final remark let me note that definitions of `foldl` and `foldr` in Haskell
libraries are slightly different from those given in the Haskell
report. GHC.Base defines `foldr` as:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ z []     =  z
-- foldr f z (x:xs) =  f x (foldr f z xs)
{-# INLINE [0] foldr #-}
-- Inline only in the final stage, after the foldr/cons rule has had a chance
-- Also note that we inline it when it has \*two\* parameters, which are the
-- ones we are keen about specialising!
foldr k z = go
          where
            go []     = z
            go (y:ys) = y \`k\` go ys
```

While `Data.List` contains following definition of `foldl`:

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs
```

Semantics are of course identical with folds defined in the report.

