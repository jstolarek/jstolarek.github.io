---
title: Is recursion good?
date: 2012-03-22
---

Is recursion good?
==================

In today's post I'm going to write a little bit about recursion. This text is
aimed at programmers familiar with structural and object-oriented programming in
languages like C/C++ or Java. I'm going to briefly summarize what recursion is,
what happens when a recursive function is called, then I'll explain what is
proper tail recursion, tail call optimization and how to convert normal
recursion into proper tail recursion using accumulator. Ready? Then let's begin.

A brief overview
================

"Recursion in computer science is a method where the solution to a problem
depends on solutions to smaller instances of the same problem" - that's what
Wikipedia says. The easiest example is the definition of a factorial:

```haskell
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

This means that factorial of `0` is `1` (this is called a base case, which must
exist in order to prevent infinite recursion), and factorial of 3 is `3 *
factorial 2`, which is `3 * 2 * factorial 1`, which is `3 * 2 * 1 * factorial
0`, which is `3 * 2 * 1 * 1` which is 6. The above code is also a valid Haskell
program. Many problems in computer science are recursive by nature. Although
every recursive algorithm can be converted into an iterative one - otherwise it
wouldn't be possible to perform recursion on sequential computers like the ones
we have today - it often turn out that the recursive algorithm is much easier to
write and understand[^1]. Nevertheless, recursive definitions are often
considered bad. Why? Let's take a look at factorial definition in C:

```cpp
int factorial( int n ) {
  if ( n == 0 ) return 1;
  return n * factorial( n - 1 );
}
```

When a recursive call is made - in fact when any function call is made - a new
stack frame is created. Stack frame contains copies of arguments passed to
function, return address of the procedure and local variables of a
function. Creating a stack frame takes a bit of time and if there are many
recursive calls there is a risk of overflowing the stack. There are other risk
as well, e.g. unnecessary recalculation of some values as in the case of
Fibonacci sequence. Many programmers therefore consider that it is better to
avoid recursive algorithms and replace them with iterative ones even if they are
harder to write and understand. Looking from a Java or C/C++ point of view this
might be the right thinking. But what if we could avoid creating a stack frame
during the recursive call? Well, sometimes we can.

Proper tail recursion
=====================

Let's rewrite our factorial function in C:

```cpp
int factorial( int n ) {
  if ( n == 0 ) return 1;
  int temp = factorial( n - 1 );
  return n * temp;
}
```

Recursive call is made somewhere in the middle of a function. The result from
this call is then used to perform more computations and produce a new
result. This is called "body recursion". Now consider something like this:

```cpp
int print_loop( int n ) {
  if ( n == 0 ) return 0;
  int m = 2 * n;
  printf( "i%\n", m );
  return print_loop( n - 1 );
}
```

This function prints a decreasing sequence of even numbers from `2n` to `2`
(inclusive) and then returns 0. In this example when we reach the stopping
condition (i.e. `n == 0`) we return a result. This result is then returned by
the recursive functions without further processing. It means that recursive call
is the last thing that is done within a function and when that call returns, the
calling function itself will also return immediately. We call this "tail
recursion". When a function returns, its stack frame is removed. The return
address from that frame is used to jump to the original calling point and since
the return is the last thing we do in a tail recursive function this means that
the remaining elements in the stack (arguments, locals) are
discarded. Therefore, the call stack that we build is mostly useless - the only
thing we need is the return address which is used only to get back to another
return address. This can be optimized. In properly tail recursive function,
instead of creating new stack frame when making a call, we can overwrite
existing stack frame: replace old copies of arguments with the new ones, old
local variables will be reused and the original return address is kept. This is
called "tail call optimization" or "tail call elimination".

Tail recursion using accumulator
================================

As we've seen, our factorial definition isn't tail recursive. Luckily, there is
a technique that allows us to convert body recursion into tail recursion. It is
based on using additional parameter - called the accumulator - that accumulates
the result of calculations. Let's rewrite our C function once more, this time to
introduce additional parameter which holds the results of computations up to a
given call:

```cpp
int factorial( int n ) {
  return factorial_help( n, 1 );
}

int factorial_helper( int n, int acc ) {
  if ( n == 0 ) return acc;
  return factorial_helper( n - 1, n * acc );
}
```

Let's see how it works:

```
factorial( 3 ) ->
  factorial_helper( 3, 1 ) ->
  factorial_helper( 2, 3 * 1 ) ->
  factorial_helper( 1, 3 * 2 ) ->
  factorial_helper( 0, 6 ) ->
  6
```

That was simple, wasn't it? This technique can be used practically always, but
it's not always effective (e.g. when building lists). Does this mean you can use
this trick to in your C or Java programs? Well, you could write your recursion
using accumulator, but neither Java nor C are required to perform tail call
optimization (they might do it in some cases, but they don't have to in
general), so performance of your programs could decrease (more parameters to
copy). This is especially a problem for Java, since there are functional
languages that run on Java Virtual machine (Scala and Clojure) and that cannot
use tail call optimization[^2]. Let my quote a paper "The role of study of
programming languages in the education of a programmer" by Daniel P. Friedman:

> We don't want to be bothered with design flaws that have been dropped into
> languages by well-meaning designers and implementators. Some example of this
> are (...) Java's lack of support for tail calls. There are likely very
> well-intentioned reasons for these mistakes, but mistakes they are,
> nonetheless. Guy Steele, (...) co-author of "Java Language Specification" now
> works for SUN and he has communicated with me that he was promised back in
> 1997 that this flaw would be fixed. Here it is 2001 and there is still no
> resolution of this problem on the horizon.

It is 2012 and still no solution. I won't get deeper into this matter (I don't
feel sufficiently competent for that), but if you're a Java programmer than
reading [this](http://www.ibm.com/developerworks/java/library/j-diag8/index.html)
might be interesting.

"What should I do now?"
=======================

So what languages are properly tail recursive? That's a good question and I'm
still trying to figure out the answer. Certainly Scheme (see: [R5RS, section
3.5](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-6.html#%_sec_3.5))
and Erlang (but see [the Erlang
myths](http://www.erlang.org/doc/efficiency_guide/myths.html)). Haskell
is... ummm... different and I don't yet fully understand it's memory model
(check [here](http://www.haskell.org/haskellwiki/Tail_recursion) and
[here](http://www.haskell.org/haskellwiki/Stack_overflow)). As I said, some
compilers of C and Java can sometimes optimize tail recursion.

The most important question is: should we really care to use recursion? As I
said in the beginning, many computer science problems are recursive by nature,
but whether you approach them in an iterative or recursive approach mostly
depends on your language. In Java it would be unnatural to process a list using
recursion. In Haskell or Erlang it's idiomatic, since these languages have the
operator to get the head of the list (tail of the list is the processed
recursively) but they don't have looping constructs. Most of us programmers
expect a simple answer and when we have it we follow assume-don't-verify
policy. I think there is no general rule for tail recursion. There are cases
when it's faster, there are cases when it's slower so you really have to measure
the performance and choose the faster algorithm. You also should be aware of
compilers internals, how it optimizes your code, how it calls functions, manages
parameters and so on.

I've been learning FP for about 3 months now and I must say that approaching
problems in a recursive manner have changed my point of view greatly. I find
recursive solutions much easier to understand because they are constructed by
decomposing the problem into simple cases. This is easier to code and therefore
less error prone, so the more I program in Scheme or Haskell the more I wish
Java had features of functional languages. If you're still not convinced about
power and usefulness of recursion then perhaps you should [read
this](2012-03-22-recursion-is-good/).

**UPDATE (05/04/2012):** Tail recursion and TCO is also discussed in fourth
chapter of Real World Haskell (read it
[here](http://book.realworldhaskell.org/read/functional-programming.html#x_j7)).

[^1]: Try googling iterative version of Quick-sort algorithm to see a good
example of this.

[^2]: Clojure [has a workaround](http://clojure.org/functional_programming#Functional%20Programming--Recursive%20Looping)
for this.

