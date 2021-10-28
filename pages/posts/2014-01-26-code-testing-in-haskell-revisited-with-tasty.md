---
title: Code testing in Haskell revisited (with Tasty)
date: 2014-01-26
---

Code testing in Haskell revisited (with Tasty)
==============================================

About 1,5 year ago I wrote a post about [code testing in
Haskell](/posts/2012-10-05-code-testing-in-haskell.html).  Post was accompanied
by [haskell-testing-stub](https://github.com/jstolarek/haskell-testing-stub): a
small project showing how to organize tests [and
benchmarks](/posts/2012-10-21-code-benchmarking-in-haskell.html) in Haskell. I
used [test-framework](http://hackage.haskell.org/package/test-framework) package
to gather tests written in different testing frameworks
([QuickCheck](http://hackage.haskell.org/package/QuickCheck) and
[HUnit](http://hackage.haskell.org/package/HUnit) in my case) in a coherent
suite. Test-framework might have been a good choice in 2012 but that might not
be the case today. The major issue is that it has been abandoned by and was
unmaintained for several months. Recently the project [has been taken up by the
community](https://github.com/haskell/test-framework) and received some updates
but for now it looks like there won't be any major development.

As a response to the test-framework package being unmaintained [Roman
Cheplyaka](http://ro-che.info/) has released
[tasty](http://hackage.haskell.org/package/tasty) (original announcement
[here](http://www.haskell.org/pipermail/haskell-cafe/2013-August/109565.html)).
Since its release in August 2013 tasty has received packages supporting
integration with QuickCheck, HUnit, SmallCheck, hspec as well as support for
golden testing and few others. I decided to give tasty a try and use it in my
haskell-testing-stub project. Tasty turned out to be almost a drop-in
replacement for test-framework. I had to update cabal file (quite obviously),
change imports to point to tasty rather than test-framework and replace usage of
`[Test]` type with `TestTree`. The only problem I encountered was adapting tests
from HUnit. It turns out that tasty-hunit package does not have a function that
allows to use an existing suite of HUnit tests. That feature was present in
test-framework-hunit as `hUnitTestToTests` function. I mailed Roman about this
and his reply was that this was intentional as he does not "believe it adds
anything useful to the API (i.e. the way to *write* code)." That's not a big
issue though as it was easy to adapt the missing function (although I think I'll
just put it in a separate package and release it so others don't have to
reinvent the wheel).

I admit that at this point I am not sure whether switching from test-framework
to tasty is a good move. The fact that tasty is actively developed is a huge
plus although test-framework has reached a mature state so perhaps active
development is no longer of key importance. Also, test-framework still has more
supporting libraries than tasty. Migrating them should be easy but up till now
no one has done it. So I'm not arguing heavily for tasty. This is more like an
experiment to see how it works.

