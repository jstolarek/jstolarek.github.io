---
title: Weight-biased leftist heaps verified in Haskell using dependent types
date: 2014-10-07
---

Weight-biased leftist heaps verified in Haskell using dependent types
=====================================================================

In January [I announced my
implementation](/posts/2014-01-16-verifying-weight-biased-leftist-heaps-using-dependent-types-a-draft.html)
of [weight-biased leftist heaps verified with dependent types in
Agda](https://github.com/jstolarek/dep-typed-wbl-heaps). This was part of my
work on a paper submitted to CAV'14 conference. The paper got rejected and I
decided not to resubmit it anywhere else. At this year's ICFP listening to
[Stephanie Weirich's keynote speech](http://www.youtube.com/watch?v=rhWMhTjQzsU)
motivated me to finally port that implementation to Haskell, something that I
had planned for a couple of months now. You can take a look at the result [on
github](https://github.com/jstolarek/dep-typed-wbl-heaps-hs). Here I want to
share some of my experiences and insights.

My overall impression is that porting from Agda to Haskell turned out to be
fairly straightforward. It was definitely not a complete rewrite. More like
syntax adjustments here and there. There were of course some surprises and bumps
along the way but nothing too problematic. More precise details are given in the
code comments.

Agda beats Haskell
==================

When it comes to programming with dependent types Agda, being a fully-fledged
dependently-typed language, beats Haskell in many aspects:

  - Agda has the same language for terms and types. Haskell separates these
    languages, which means that if I want to have addition for natural numbers
    then I need to have two separate definitions for terms and types. Moreover,
    to tie types and terms together I need singleton types. And once I have
    singleton types then I need to write third definition of addition that works
    on singletons. All of this is troublesome to write and use. (This tedious
    process can be automated by using
    [singletons](http://hackage.haskell.org/package/singletons) package.)

  - interactive agda-mode for Emacs makes writing code much simpler in
    Agda. Here I was porting code that was already written so having an
    interactive Emacs mode for Haskell was not at all important. But if I were
    to write all that dependently-typed code from scratch in Haskell this would
    be painful. We definitely need better tools for dependently-typed
    programming in Haskell.

  - Agda admits Unicode identifiers. This allows me to have type constructors
    like `?` or variables like `p?b`. In Haskell I have `GEq` and `pgeb`,
    respectively. I find that less readable. (This is very subjective.)

  - Agda has implicit arguments that can be deduced from types. Haskell does
    not, which makes some function calls more difficult. Surprisingly that was
    not as huge problem as I initially thought it will be.

  - Agda is total, while Haskell is not. Since there are bottoms in Haskell it
    is not sound as a logic. In other words we can prove false eg. by using
    undefined.

Haskell beats Agda
==================

The list is noticeably shorter:

  - Haskell has much better term-level syntax. In many places this resulted in
    significantly shorter code than in Agda.

  - Haskell is not total. As stated earlier this has its drawbacks but it also
    has a good side: we don't need to struggle with convincing the termination
    checker that our code does actually terminate. This was painful in Agda
    since it required using sized types.

  - Haskell's `gcastWith` function is much better than Agda's `subst`. Both
    these functions allow type-safe casts given the proof that the cast is
    safe. The difference is that Agda's `subst` requires more explicit arguments
    (as I noted earlier the opposite is usually the case) and restricts the cast
    to the last type parameter (Haskell allows cast for any type parameter).

Summary
=======

While the list of wins is longer for Agda than it is for Haskell I'm actually
very happy with Haskell's performance in this task. The verification in Haskell
is as powerful as it is in Agda. No compromises required.

It's worth remarking that my implementation works with GHC 7.6, so you don't
need the latest fancy type-level features like closed type families. The really
essential part are the promoted data types.

