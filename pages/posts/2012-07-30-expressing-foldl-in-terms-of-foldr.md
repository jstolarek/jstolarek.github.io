---
title: Expressing foldl in terms of foldr
date: 2012-07-30
---

Expressing foldl in terms of foldr
==================================

In [chapter 4 of Real World
Haskell](http://book.realworldhaskell.org/read/functional-programming.html)
there is a challenge for the reader: express
[`foldl`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:foldl)
using [`foldr`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:foldr).
Authors warn that this is not trivial and I must admit that I have not attempted
this exercise leaving it for later. Yesterday I read Graham Hutton's ["Tutorial
on the universality and expressiveness of
fold"](http://www.cs.nott.ac.uk/~gmh/fold.pdf) and it happens that, among other
things, it presents an approach that can be applied to solve this
problem. Hutton first presents a specific case in which `sum` function is
defined using `foldl` expressed with `foldr`. Then he gives a general formula
for expressing `foldl` with `foldr`. Despite having a derivation for one
specific case and a ready result (without its derivation) it wasn't
straightforward to provide my own derivation of the general solution. This was a
mind bending exercise and I'd like to go through the details here.

The solution is based on using the so called _universal property_ of
`foldr`[^1].  This property states that if we have some function `g` defined as:

```haskell
g [] = v
g (x:xs) = f x (g xs)
```

then

```
g = foldr f v
```

Indeed, if we substitute `foldr f v` into definition of `g` we get a definition
of `foldr`:

```haskell
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)
```

Moreover, `foldr f v` is a unique solution to the defining equations of `g`.

Recall that `foldl` is defined as:

```
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs
```

The base case of `foldr` and `foldl` is identical, but the recursive one is
not. Moreover, the recursive case of `foldl` cannot be rewritten in the form `f
x (g xs)`. This means that we need to apply some transformation to definition of
`foldl` so it can be rewritten in that form. Let's create a function `foldl2`
defined as:

```haskell
foldl2 f [] v = v
foldl2 f (x:xs) v = foldl2 f xs (f v x)
```

Nothing special so far. We just made a function that is the same as `foldl`, but
has the last two parameters swapped. We can rewrite the base case as:

```
foldl2 f [] v = id v
```

`id` is the identity function that accepts one parameter and returns that
parameter unchanged. Now we remove the `v` parameter:

```
foldl2 f [] = id
```

Such transformation is known as η-reduction[^2].  Let us now concentrate on the
recursive case. It can be rewritten as

```haskell
foldl2 f (x:xs) v = (\w -> foldl2 f xs (f w x)) v
```

We created an anonymous function with one of the parameters of `f` factored out.
This expression can also be η-reduced:

```haskell
foldl2 f (x:xs) = \w -> foldl2 f xs (f w x)
```

Let's factor out second parameter of function `f` in the same manner:

```haskell
foldl2 f (x:xs) = (\y w -> foldl2 f xs (f w y)) x
```

And finally let's factor out `foldl2 f xs` and just pass it as another parameter
to the lambda expression:

```haskell
foldl2 f (x:xs) = (\y h w -> h (f w y)) x (foldl2 f xs)
```

We're almost there. Recall that the universal property requires function of the
form[^3]:

g (x:xs) = k x (g xs)

And it so happens that we just converted `foldl2` to that form, where `k = \y h
w -> h (f w y)`. Comparing last two equation we see that `g = foldl2 f`, but
from the universal property we also know that `g = foldr k v`, which means that
`foldl2 f = foldr k v`. The notation here might be a bit confusing. From the
base case we determined the value of `v` in the last equality to be equal to
`id` function, which yields `foldl2 f = foldr k id`. Substituting the value of
`k` we get:

```haskell
foldl2 f = foldr (\y h w -> h (f w y)) id
```

Original definition of `foldl2` had two more parameters, but they were removed
by η-reductions. Let's restore these two parameters by adding them to both lhs
and rhs:

```haskell
foldl2 f xs v = foldr (\y h w -> h (f w y)) id xs v
```

Recall that `foldl2` was only a convenience function we used for the
derivation. Going back to the original `foldl` function yields the final result:

```haskell
foldl f v xs = foldr (\y h w -> h (f w y)) id xs v
```

OK, formulas don't lie, but this result is definitely not an intuitive one and
deserves some good explanation. You may be surprised that four parameters are
passed into `foldr`, but this should become clear in a moment. We will play with
it to get some intuition on how this works.

Let us begin with verifying that this expression is type-correct. Type of
`foldr` is:

```
ghci> :t foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
```

So the first parameter to `foldr` should be of type `(a -> b -> b)`. Lambda that
we pass to `foldr` as the first parameter uses `f`. This is the function that is
passed as first parameter to `foldl`. Since `foldl` has type:

```
ghci> :t foldl
foldl :: (a -> b -> a) -> a -> [b] -> a
```

We require that `f` has type `a -> b -> a`. Let's define simplest possible
function that has that type and then check the type of lambda passed to `foldr`:

```
ghci> let f = \a b -> a
ghci> :t (\y h w -> h (f w y))
(\y h w -> h (f w y)) :: t2 -> (t1 -> t) -> t1 -> t
```

Recall that `->` is right-associative, which means that above type is equivalent
to `t2 -> (t1 -> t) -> (t1 -> t)`. Parentheses at the end can be added and the
meaning is the same. This corresponds to our expected type of `(a -> b ->
b)`. Here, the value of `b` is assumed to be `t1 -> t`. If we substitute (`t1 ->
t`) for `b` in the type signature of `foldr` we get

```
(a -> (t1 -> t) -> (t1 -> t)) -> (t1 -> t) -> [a] -> (t1 -> t)
```

Note that last parentheses can be dropped, which result in function that has
four parameters:

```
(a -> (t1 -> t) -> (t1 -> t)) -> (t1 -> t) -> [a] -> t1 -> t
```

We already verified that lambda passed to `foldr` is of type `(a -> (t1 -> t) ->
(t1 -> t))`. The second parameter, `id` function, is of type `(a -> a)`, which
corresponds to `(t1 -> t)` in the type signature. Therefore usage of `id`
imposes additional restriction that `t1` ~ `t` ((~ notation means that two types
are the same.)), which means that type signature can be rewritten as:

```
(a -> (t -> t) -> (t -> t)) -> (t -> t) -> [a] -> t -> t
```

`[a]` corresponds to parameter `xs`, the list that we are folding. `t`
corresponds to initial value of accumulator and the last `t` is the return type.

Now that we have verified type-correctness of the solution, let's see how it
works in practice. Let's say we want to fold a list of three elements using
`(+)` as folding function and `0` as initial value of the accumulator. In other
words, we want to calculate the sum of elements in a list. If we use `foldr`,
the evaluation process will look like this:

```
foldr (+) 0 [1,2,3] =
(+) 1 (foldr (+) 0 [2,3]) =
(+) 1 ((+) 2 (foldr (+) 0 [3])) =
(+) 1 ((+) 2 ((+) 3 (foldr (+) 0 []))) =
(+) 1 ((+) 2 ((+) 3 0 )) =
(+) 1 ((+) 2 3) =
(+) 1 5 =
6
```

If we instead use `foldl`, evaluation will look like this:

```
foldl (+) 0 [1,2,3] =
foldl ((+) 0 1) [2,3] =
foldl ((+) ((+) 0 1) 2) [3] =
foldl ((+) ((+) ((+) 0 1) 2) 3) [] =
((+) ((+) ((+) 0 1) 2) 3) =
((+) ((+) 1 2) 3) =
((+) 3 3) =
6
```

Both folds produce the same result, which is a direct consequence of first
duality theorem for folds[^4]. Now let's see how evaluation will proceed if we
use `foldl` expressed using `foldr`:

```
foldl (+) 0 [1,2,3] =
foldr (\y h w -> h ((+) w y)) id [1,2,3] 0 =
(\h w -> h ((+) w 1)) (foldr (\y h w -> h ((+) w y)) id [2,3]) 0 =
(\h w -> h ((+) w 1)) (\h w -> h ((+) w 2)) (foldr (\y h w -> h ((+) w y)) id [3]) 0 =
(\h w -> h ((+) w 1)) (\h w -> h ((+) w 2)) (\h w -> h ( (+) w 3) ) (foldr (\y h w -> h ( (+) w y) ) id []) 0 =
(\h w -> h ((+) w 1)) (\h w -> h ((+) w 2)) (\h w -> h ((+) w 3)) id 0 =
(\h w -> h ((+) w 1)) (\h w -> h ((+) w 2)) (w -> id ((+) w 3)) 0 =
(\h w -> h ((+) w 1)) (w -> id ((+) ((+) w 2) 3)) 0 =
(w -> id ((+) ((+) ((+) w 1) 2) 3)) 0 =
id ((+) ((+) ((+) 0 1) 2) 3))
```

We've reached expression that is the same as the one we reached when evaluating
`foldl`. Well, in fact that is what we expected. After all this is also `foldl`!
So the whole trick is based on using `foldr` to generate a function that accepts
initial value of the accumulator and produces the same expression we would get
when using `foldl` (plus the identity function).

I hope that this post made it clear how to express `foldl` using `foldr`. This
is of course by no means an exhaustive treatment of the subject of
folds. There's a lot more. I think that Hutton's paper is a good starting
point. Bird's and Wadler's "Introduction to Functional Programming" also seems
to be a very valuable resource, though I've read only chapter about folds[^5].
There's still some more stuff to figure out about folds, like the difference in
behaviour of `foldr` and `foldl` for infinite lists or expressing `foldr` using
`foldl` (for finite lists only).

[^1]: Graham Hutton in his paper uses the name _fold_ to denote `foldr`. I'm
sticking with `foldr` so my derivation remains consistent with Haskell.

[^2]: η is pronounced _eta_.

[^3]: Notice that I renamed the name of the first function from `f` to `k` to
prevent the name clash.

[^4]: See: Bird, R., Wadler, P. "Introduction to functional programming", 1st
ed., p. 68

[^5]: There's also a second edition of this book authored only by Bird, but I
wasn't able to find it.

