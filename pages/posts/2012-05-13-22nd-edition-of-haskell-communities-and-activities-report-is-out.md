---
title: 22nd edition of Haskell Communities and Activities Report is out
date: 2012-05-13
---

22nd edition of Haskell Communities and Activities Report is out
================================================================

22nd edition of Haskell Communities and Activities Report has just been
released.  Here's an overview of new informations:

  * [Learn You a Haskell](http://learnyouahaskell.com/) was translated [into
    Japanese](http://ssl.ohmsha.co.jp/cgi-bin/menu.cgi?ISBN=978-4-274-06885-0).
    Japan is lucky, they already have [edition of Real World
    Haskell](http://www.oreilly.co.jp/books/9784873114231/).

  * [Edward Z. Yang](http://ezyang.com/) is now the editor of [The Monad
    Reader](http://themonadreader.wordpress.com/).

  * There is a lot of progress on GHC development. Some features will be present
    in 7.6 release, some are work in progress with no release date yet. Among
    them are:

  * adding support for holes: you could write incomplete code with some
    fragments missing (these are holes) and GHC would report the type that hole
    can have. This is inspired by Agda.

  * There is work on new code generator, though I'm not sure how this relates to
    the [LLVM backend](http://blog.llvm.org/2010/05/glasgow-haskell-compiler-and-llvm.html).

  * There is possibility to change number of utilized cores at runtime.
    Previously it was possible only when starting the program and it couldn't be
    changed once the application started.

  * SIMD instructions support for LLVM backend is on the way, hopefully will
    make it into 7.6.1. Since I'm doing mostly numerical computations I'm
    looking forward to it.

  * There's a lot of progress on Haskell web frameworks:
    [Yesod](http://www.yesodweb.com/) (stable version was released about a month
    ago) and [Snap](http://snapframework.com/snaplets) (two major releases since
    the last report).

  * [Portackage](http://www.fremissant.net/portackage/portackage.php) is a new
    portal that gathers informations form
    [Hackage](http://hackage.haskell.org/packages/hackage.html). It definitely
    needs more development work, but I think it has potential to become useful.

  * And last, but not least: Yet Another Haskell Blog is also mentioned in the
    report.

I've found some other interesting things in the report, mostly informations
about projects that concentrate on parallel computations in Haskell: Data
Parallel Haskell, Glasgow Parallel Haskell and Parallel GHC Project. I'm
especially interested in these - though I don't have any knowledge in that area
yet - because it looks that parallelization of computations can be done a whole
lot easier within the functional programming paradigm. There are many new
Haskell projects mentioned in the report as well, but they are beyond the scope
of my interests so I didn't mention them. Read the full report
[here](http://www.haskell.org/communities/05-2012/report.pdf) - you'll most
likely find other interesting stuff.

