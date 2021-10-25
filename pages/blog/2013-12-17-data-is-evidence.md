---
title: Data is evidence
date: 2013-12-17
---

Data is evidence
================

Recently I've been reading ["Types and Programming
Langauages"](http://www.cis.upenn.edu/~bcpierce/tapl/) book by Benjamin
C. Pierce. It's a great introduction to theory behind type systems of both
functional and object-oriented languages. In the first chapter there's this
really brilliant sentence that says what a type system does:

> A type system can be regarded as calculating a kind of static approximation to
> the run-time behaviours of the terms in a program.

So if a type system is a static approximation of program's behaviour at runtime
a natural question to ask is: "how accurate this approximation can be?" Turns
out it can be very accurate.

Let's assume that we have following definition of natural numbers[^1]:

```agda
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat
```

First constructor - `zero` - says that zero is a natural number. Second - `suc`
- says that successor of any natural number is also a natural number. This
representation allows to encode `0` as `zero`, `1` as `suc zero`, `2` as `suc
(suc zero)` and so on[^2]. Let's also define a type of booleans to represent
logical true and false:

```agda
data Bool : Set where
  false : Bool
  true  : Bool
```

We can now define a `≥` operator that returns `true` if its arguments are in
greater-equal relation and `false` if they are not:

```agda
_≥_ : Nat → Nat → Bool
m     ≥ zero  = true
zero  ≥ suc n = false
suc m ≥ suc n = m ≥ n
```

This definition has three cases. First says that any natural number is greater
than or equal to zero. Second says that zero is not greater than any successor.
Final case says that two non-zero natural numbers are in ≥ relation if their
predecessors are also in that relation. What if we replace `false` with `true`
in our definition?

```agda
_≥_ : Nat → Nat → Bool
m     ≥ zero  = true
zero  ≥ suc n = true
suc m ≥ suc n = m ≥ n
```

Well... nothing. We get a function that has nonsense semantics but other than
that it is well-typed. The type system won't catch this mistake. The reason for
this is that our function returns a result but it doesn't say why that result is
true. And since `≥` doesn't give us any evidence that result is correct there is
no way of statically checking whether the implementation is correct or not.

But it turns out that we can do better using dependent types. We can write a
comparison function that proves its result correct. Let's forget our definition
of `≥` and instead define datatype called `≥`:

```agda
data _≥_ : Nat → Nat → Set where
  ge0 : {  y : Nat}         → y     → zero
  geS : {x y : Nat} → x → y → suc x → suc y
```

This type has two `Nat` indices that parametrize it. For example: `5 ≥ 3` and `2
≥ 0` are two distinct types. Notice that each constructor can only be used to
construct values of a specific type: `ge0` constructs a value that belongs to
types like `0 ≥ 0`, `1 ≥ 0`, `3 ≥ 0` and so on. `geS` given a value of type `x ≥
y` constructs a value of type `suc x ≥ suc y`.

There are a few interesting properties of `≥` datatype. Notice that not only
`ge0` can construct value of types `y ≥ 0`, but it is also the only possible
value of such types. In other words the only value of `0 ≥ 0`, `1 ≥ 0` or `3 ≥
0` is `ge0`. Types like `5 ≥ 3` also have only one value (in case of `5 ≥ 3` it
is `geS (geS (geS ge0))`). That's why we call `≥` a _singleton type_. Note also
that there is no way to construct values of type `0 ≥ 3` or `5 ≥ 2` - there are
no constructors that we could use to get a value of that type. We will thus say
that `≥` datatype is a witness (or evidence): if we can construct a value for a
given two indices then this value is a witness that relation represented by the
`≥` datatype holds. For example `geS (geS ge0))` is a witness that relations `2
≥ 2` and `2 ≥ 5` hold but there is no way to provide evidence that `0 ≥ 1`
holds. Notice that previous definition of `≥` function had three cases: one base
case for `true`, one base case for `false` and one inductive case. The `≥`
datatype has only two cases: one being equivalent of `true` and one
inductive. Because the value of `≥` exists if and only if its two parameters are
in ≥ relation there is no need to represent `false` explicitly.

We have a way to express proof that one value is greater than another. Let's now
construct a datatype that can say whether one value is greater than another and
supply us with a proof of that fact:

```agda
data Order : Nat → Nat → Set where
  ge : {x : Nat} {y : Nat} → x → y → Order x y
  le : {x : Nat} {y : Nat} → y → x → Order x y
```

Order is indexed by two natural numbers. These numbers can be anything - there
is no restriction on any of the constructors. We can construct values of Order
using one of two constructors: `ge` and `le`. Constructing value of `Order`
using `ge` constructor requires a value of type `x ≥ y`. In other words it
requires a proof that `x` is greater than or equal to `y`. Constructing value of
`Order` using `le` constructor requires the opposite proof - that `y ≥ x`.
`Order` datatype is equivalent of `Bool` except that it is specialized to one
particular relation instead of being a general statement of truth or false. It
also carries a proof of the fact that it states.

Now we can write a function that compares two natural numbers and returns a
result that says whether first number is greater than or equal to the second
one[^3]:

```agda
order : (x : Nat) → (y : Nat) → Order x y
order x       zero    = ge ge0
order zero    (suc b) = le ge0
order (suc a) (suc b) with order a b
order (suc a) (suc b) | ge a≥b = ge (geS a≥b)
order (suc a) (suc b) | le b≥a = le (geS b≥a)
```

In this implementation `ge` plays the role of `true` and `le` plays the role of
`false`. But if we try to replace `le` with `ge` the way we previously replaced
`false` with `true` the result will not be well-typed:

```agda
order : (x : Nat) → (y : Nat) → Order x y
order x       zero    = ge ge0
order zero    (suc b) = ge ge0 -- TYPE ERROR
order (suc a) (suc b) with order a b
order (suc a) (suc b) | ge a≥b = ge (geS a≥b)
order (suc a) (suc b) | le b≥a = le (geS b≥a)
```

Why? It is a direct result of the definitions that we used. In the second
equation of `order`, `x` is `zero` and `y` is `suc b`. To construct a value of
`Order x y` using `ge` constructor we must provide a proof that `x ≥ y`. In this
case we would have to prove that `zero ≥ suc b`, but as discussed previously
there is no constructor of `≥` that could construct value of this type. Thus the
whole expression is ill-typed and the incorrectness of our definition is caught
at compile time.

Summary
=======

The idea that types can represent logical propositions and values can be viewed
as proofs of these propositions is not new - it is known as [Curry-Howard
correspondence](http://en.wikipedia.org/wiki/Curry_Howard_isomorphism) (or
isomorphism) and I bet many of you have heard that name. Example presented here
is taken from "Why Dependent Types Matter". See this [recent
post](2013-11-07-why-dependent-types-matter-in-agda/) for a few more words about
this paper.

[^1]: All code in this post is in Agda.

[^2]: For the sake of readability I will write Nats as numerals, not as
      applications of suc and zero. So remember that whenever I write 2 I mean
      `suc (suc zero)`.

[^3]: Note that in Agda `a≥b` is a valid identifier, not an application of `≥`.

