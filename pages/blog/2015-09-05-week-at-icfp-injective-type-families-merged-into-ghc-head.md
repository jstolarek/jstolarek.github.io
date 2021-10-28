---
title: Week at ICFP&#58; Injective Type Families merged into GHC HEAD
date: 2015-09-05
---

Week at ICFP: Injective Type Families merged into GHC HEAD
==========================================================

I spent last week at Vancouver, Canada attending Haskell Implementors Workshop,
ICFP and the Haskell Symposium. Yesterday I gave a talk on injective type
families, [described also in my previous post](/blog/2015-05-26-injective-type-families-for-haskell.html).
[Slides are on my web page](http://ics.p.lodz.pl/~stolarek/_media/pl:research:injectivity-haskell15-slides.pdf)
but the talk itself is not yet online. Day prior to the talk I finally managed
to [merge the injective type families
branch](https://github.com/ghc/ghc/commit/374457809de343f409fbeea0a885877947a133a2),
which means that this feature is definitely making it into the next stable
release of GHC. (In case you haven't heard this will be GHC 8.0, not 7.12.)

My next plans include extending injective type families to fully match
expressiveness of functional dependencies, as outlined in Section 7.2 of the
injectivity paper. I also hope to finally implement [support for typed holes in
Template Haskell](https://ghc.haskell.org/trac/ghc/ticket/10267). The patch was
supposed to be trivial and I started working on it several months ago. But then
I ran into several problems with it and abandoned it to focus on ITFs.

