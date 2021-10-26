---
title: To Mock a Mockingbird or&#58; How I learned to stop worrying and learned combinatory logic
date: 2013-02-06
---

To Mock a Mockingbird or: How I learned to stop worrying and learned combinatory logic
======================================================================================

[![mockingbird](/images/blog/mockingbird-194x300.jpg)](/images/blog/mockingbird.jpg)

Yesterday I finished reading one of the most remarkable books I read in my life:
["To Mock a Mockingbird and Other Logic Puzzles: Including an Amazing Adventure
in Combinatory Logic"](http://en.wikipedia.org/wiki/To_Mock_a_Mockingbird) by
Raymond Smullyan. When I finished reading [The Little
Schemer](2013-01-08-the-little-schemer-book-review/) that book was listed as one
of the suggested further readings. The title was quite intriguing so I got the
book and started reading it. That was a year ago and I finished the book
yesterday. Why? Because I got stuck and couldn't understand some of the
material. Luckily I now had some time to approach the book once again and grok
it.

As the title suggests the book is a collection of logic puzzles. Out of six
parts of the book - 25 chapters total - two are devoted to general logic
puzzles, many of them about different aspects of truth telling. These can be
regarded as a warm-up because in the third part the book makes a sudden turn
towards combinatory logic. And this is the moment I found difficult in the book.
Of course Smullyan doesn't expect that readers work with combinators so he
camouflages them as singing birds. Having some mathematical background I
rejected this cover and tried to approach problems formally. Now, after reading
the book, I think this was a major mistake that lead to my failure. I wasn't
able to deal with first 10 puzzles but I was more or less able to follow the
solutions.  Still I felt that reading solutions without being able to solve
puzzles by myself was cheating so I gave up. A few months later I made another
approach to the book but the results were exactly the same. Three weeks ago I
made a third attempt, but I decided not to give up even if I won't be able to
come up with my own solutions. I figured that being able to only understand
given solutions is completely fine. That decision turned out to be a good one.
Although at first I wasn't able to solve puzzles on my own at some point things
just clicked. I solved one puzzle, then another and another and I realized that
I know how to solve most of the puzzles. From now on the book went quite
smoothly. Part four about logical paradoxes and inconsistencies in logical
systems gave me some problems and I was afraid that each subsequent part will be
equally challenging but it turned out that it was not the case. Part five gives
a nice overview of computations using [SKI
combinators](http://en.wikipedia.org/wiki/SKI_combinator_calculus), while part
six presents [Church encoding](http://en.wikipedia.org/wiki/Church_encoding) of
natural numbers and culminates with a proof of [Gödel's
theorem](http://en.wikipedia.org/wiki/G%C3%B6del%27s_incompleteness_theorems).

Some advice
===========

The book is challenging on the one hand but it is also very rewarding. One of
the most satisfying moments was realizing that I am able to write expressions
like

$g\tilde{a}\tilde{b} = Z\tilde{a}f(Z\tilde{b}t(g(P\tilde{a})(P\tilde{b})))$

and then seeing that these are in fact nothing more but recursive programs.
However it took me some work to get to this point and I think I can give future
readers some advice about approaching the book. I've seen people on the internet
saying they don't understand parts of the book about combinators. In fact this
is the same problem I faced on my initial contact. So my advice number one is:
**don't give up**. It looks like working with combinators just isn't an inborn
talent for most of us. It's something that we just need to learn and it's
completely fine if you can't figure out solutions on your own.  It is important
however to follow solutions given in the book. At some point you should
understand how all of this works. Also **don't make my mistake of trying too
formalize things to much**. Don't try to apply presented facts about combinators
to anything else you know like maths or functional programming. This seems to
only bring confusion. At some point things will become clear, but until then be
patient. I wasn't and in my attempts I even tried to introduce universal and
existential quantification, which was a complete overkill. Also, it is very
important to **make notes**. Keep conclusions of the puzzles in a notebook so
you have easy access to a list of known facts. This is crucial since some
solutions are based on facts proved 100 pages earlier. I also suggest having **a
separate scratch-pad**.

Summary
=======

To "Mock a Mockingbrd" was a very insightful book and one of the most unusual
ones I have read. It presents difficult material in a fun and entertaining way,
but don't be fooled - you will spend hours with pen and paper to complete this
book. I highly recommend it to anyone interested in logic and functional
programming. What functional programming has to do with it? Unrelated as it may
seem I feel that this book has given me knowledge necessary to tackle
[type-level programming in
Haskell](http://www.haskell.org/haskellwiki/Type_arithmetic). Just three weeks
ago this seemed like a complete black magic and now it looks comprehensible.

