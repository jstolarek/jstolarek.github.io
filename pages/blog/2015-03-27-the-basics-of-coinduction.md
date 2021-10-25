---
title: The basics of coinduction
date: 2015-03-27
---

The basics of coinduction
=========================

I don't remember when I first heard the terms "coinduction" and "corecursion"
but it must have been quite long ago. I had this impression that they are yet
another of these difficult theoretical concepts and that I should learn about
them one day. That "one day" happened recently while reading [chapter 5 of
"Certified Programming with Dependent
Types"](http://adam.chlipala.net/cpdt/html/Coinductive.html). It turns out that
basics of coinduction are actually quite simple. In this post I'll share with
you what I already know on the subject.

Recursion in Haskell
====================

Let's begin with looking at Haskell because it is a good example of language not
formalizing coinduction in any way. Two features of Haskell are of interest to
us. First one is laziness. Thanks to Haskell being lazy we can write definitions
like these (in GHCi):

```
ghci> let ones = 1 : ones
ghci> let fib = zipWith (+) (1:fib) (1:1:fib)
```

`ones` is - as the name implies - an infinite sequence (list) of ones. `fib` is
a sequence of Fibonacci numbers. Both these definitions produce infinite lists
but we can use these definitions safely because laziness allows us to force a
finite number of elements in the sequence:

```
ghci> take 5 ones
[1,1,1,1,1]
ghci> take 10 fib
[2,3,5,8,13,21,34,55,89,144]
```

Now consider this definition:

```
ghci> let inf = 1 + inf
```

No matter how hard we try there is no way to use the definition of `inf` in a
safe way. It always causes an infinite loop:

```
ghci> (0 /= inf)
*** Exception: <\>
```

The difference between definitions of `ones` or `fib` an the definition of `inf`
is that the former use something what is called a _guarded recursion_. The term
_guarded_ comes from the fact that recursive reference to self is hidden under
datatype constructor (or: guarded by a constructor). The way lazy evaluation is
implemented gives a guarantee that we can stop the recursion by not evaluating
the recursive constructor argument. This kind of infinite recursion can also be
called _productive recursion_, which means that although recursion is infinite
each recursive call is guaranteed to produce something (in my examples either a
1 or next Fibonacci number). By contrast recursion in the definition of `inf` is
not guarded or productive in any way.

Haskell happily accepts the definition of `inf` even though it is completely
useless. When we write Haskell programs we of course don't want them to fall
into silly infinite loops but the only tool we have to prevent us from writing
such code is our intelligence. Situation changes when it comes to....

Dependently-typed programming languages
=======================================

These languages deeply care about termination. By "termination" I mean ensuring
that a program written by the user is guaranteed to terminate for any input. I
am aware of two reasons why these languages care about termination. First reason
is theoretical: without termination the resulting language is inconsistent as
logic. This happens because non-terminating term can prove any
proposition. Consider this non-terminating Coq definition:

```
Fixpoint evil (A : Prop) : A := evil A.
```

If that definition was accepted we could use it to prove any proposition. Recall
that when it comes to viewing types as proofs and programs as evidence "proving
a proposition" means constructing a term of a given type. `evil` would allow to
construct a term inhabiting any type `A`. (`Prop` is a _kind_ of logical
propositions so `A` is a type.) Since dependently-typed languages aim to be
consistent logics they must reject non-terminating programs. Second reason for
checking termination is practical: dependently typed languages admit functions
in type signatures. If we allowed non-terminating functions then typechecking
would also become non-terminating and again this is something we don't
want. (Note that Haskell gives you `UndecidableInstances` that can cause
typechecking to fall into an infinite loop).

Now, if you paid attention on your Theoretical Computer Science classes all of
this should ring a bell: the halting problem! The halting problem says that the
problem of determining whether a given Turing machine (read: a given computer
program) will ever terminate is undecidable. So how is that possible that
languages like Agda, Coq or Idris can answer that question? That's simple: they
are not Turing-complete (or at least their terminating subsets are not Turing
complete). (**UPDATE:** but see Conor McBride's comment below[^1].)  They
prohibit user from using some constructs, probably the most important one being
_general recursion_. Think of general recursion as any kind of recursion
imaginable. Dependently typed languages require structural recursion on subterms
of the arguments. That means that if a function receives an argument of an
inductive data type (think: algebraic data type/generalized algebraic data type)
then you can only make recursive calls on terms that are syntactic subcomponents
of the argument. Consider this definition of `map` in Idris:

```idris
map : (a -> b) -> List a -> List b
map f []      = []
map f (x::xs) = f x :: map f xs
```

In the second equation we use pattern matching to deconstruct the list
argument. The recursive call is made on `xs`, which is structurally smaller then
the original argument. This guarantees that any call to `map` will
terminate. There is a silent assumption here that the `List A` argument passed
to `map` is finite, but with the rules given so far it is not possible to
construct infinite list.

So we just eliminated non-termination by limiting what can be done with
recursion. This means that our Haskell definitions of `ones` and `fib` would not
be accepted in a dependently-typed language because they don't recurse on an
argument that gets smaller and as a result they construct an infinite data
structure. Does that mean we are stuck with having only finite data structures?
Luckily, no.

Coinduction to the rescue
=========================

Coinduction provides a way of defining and operating on infinite data structures
as long as we can prove that our operations are safe, that is they are guarded
and productive. In what follows I will use Coq because it seems that it has
better support for coinduction than Agda or Idris (and if I'm wrong here please
correct me).

Coq, Agda and Idris all require that a datatype that can contain infinite values
has a special declaration. Coq uses `CoInductive` keyword instead of `Inductive`
keyword used for standard inductive data types. In a similar fashion Idris uses
`codata` instead of `data`, while Agda requires ? annotation on a coinductive
constructor argument.

Let's define a type of infinite `nat` streams in Coq:

```coq
CoInductive stream : Set :=
| Cons : nat -> stream -> stream.
```

I could have defined a polymorphic stream but for the purpose of this post
stream of nats will do. I could have also defined a `Nil` constructor to allow
finite coinductive streams - declaring data as coinductive means it _can_ have
infinite values, not that it _must_ have infinite values.

Now that we have infinite streams let's revisit our examples from Haskell:
`ones` and `fib`. `ones` is simple:

```coq
CoFixpoint ones : stream := Cons 1 ones.
```

We just had to use `CoFixpoint` keyword to tell Coq that our definition will be
corecursive and it is happily accepted even though a similar recursive
definition (ie. using `Fixpoint` keyword) would be rejected. Allow me to quote
directly from CPDT:

> whereas recursive definitions were necessary to _use_ values of recursive
> inductive types effectively, here we find that we need _co-recursive
> definitions_ to _build_ values of co-inductive types effectively.

That one sentence pins down an important difference between induction and
coinduction.

Now let's define `zipWith` and try our second example `fib`:

```coq
CoFixpoint zipWith (f : nat -> nat -> nat) (a : stream)
                   (b : stream) : stream :=
  match a, b with
    | Cons x xs, Cons y ys => Cons (f x y) (zipWith f xs ys)
  end.

CoFixpoint fib : stream :=
   zipWith plus (Cons 1 fib) (Cons 1 (Cons 1 fib)).
```

Unfortunately this definition is rejected by Coq due to "unguarded recursive
call". What exactly goes wrong? Coq requires that all recursive calls in a
corecursive definition are:

  1. direct arguments to a data constructor
  2. not inside function arguments

Our definition of `fib` violates the second condition - both recursive calls to
`fib` are hidden inside arguments to `zipWith` function. Why does Coq enforce
such a restriction? Consider this simple example:

```coq
Definition tl (s : stream) : stream :=
  match s with
    | Cons _ tl' => tl'
  end.

CoFixpoint bad : stream := tl (Cons 1 bad).
```

`tl` is a standard tail function that discards the first element of a stream and
returns its tail. Just like our definition of `fib` the definition of `bad`
places the corecursive call inside a function argument. I hope it is easy to see
that accepting the definition of `bad` would lead to non-termination - inlining
definition of `tl` and simplifying it leads us to:

```coq
CoFixpoint bad : stream := bad.
```

and that is bad. You might be thinking that the definition of `bad` really has
no chance of working whereas our definition of `fib` could in fact be run safely
without the risk of non-termination. So how do we persuade Coq that our
corecursive definition of `fib` is in fact valid? Unfortunately there seems to
be no simple answer. What was meant to be a simple exercise in coinduction
turned out to be a real research problem. This past Monday I spent well over an
hour with my friend staring at the code and trying to come up with a
solution. We didn't find one but instead we found a really nice paper "Using
Structural Recursion for Corecursion" by Yves Bertot and Ekaterina
Komendantskaya. The paper presents a way of converting definitions like `fib` to
a guarded and productive form accepted by Coq. Unfortunately the converted
definition looses the linear computational complexity of the original definition
so the conversion method is far from perfect. I encourage to read the paper. It
is not long and is written in a very accessible way. Another set of possible
solutions is given in [chapter 7 of
CPDT](http://adam.chlipala.net/cpdt/html/GeneralRec.html) but I am very far from
labelling them as "accessible".

I hope this post demonstrates that basics ideas behind coinduction are actually
quite simple. For me this whole subject of coinduction looks really fascinating
and I plan to dive deeper into it. I already have my eyes set on several
research papers about coinduction so there's a good chance that I'll write more
about it in future posts.

[^1]: Conor says: *"Now represent the semantics of Turing machines as
  coinductive processes and review your hasty and inaccurate repetition of the
  common falsehood that totality prevents Turing-completeness. You exactly get
  to say “we know how to run it for as long as we’re willing to wait, but we
  can’t promise you it will stop”, which is both the truth, and exactly the deal
  when you work in a partial language. The only difference is that when you
  promise something does work, you’re believable. The expressive weakness is on
  the partial side."*

