---
title: Code testing in Haskell
date: 2012-10-05
---

Code testing in Haskell
=======================

A few weeks ago [I wrote that for 3 years I used Matlab as my main programming
language](/posts/2012-09-09-y-combinator-in-matlab.html).  Before that however I
was developing in Java. While Java can be criticized for its design - and I
noticed that some Haskallers look down on it - it learned me one very important
thing: code testing. Java's JUnit was very important in popularizing Test Driven
Development (TDD) and code testing in general. Personally, I never used JUnit
and relied on TestNG instead. At the time I was learning TDD TestNG's features
were way ahead of JUnit (and it is possible that they still are). Having learned
TestNG, testing became very important for me. For the most time I was creating
code that did numerical computations according to some sophisticated
algorithm. It was very easy to make a mistake and tests allowed me to make my
code reliable. Later I used TDD approach in Matlab thanks to Steve Eddins'
[xUnit Framework](http://www.mathworks.com/matlabcentral/fileexchange/22846-matlab-xunit-test-framework).
After starting with Haskell I knew one thing - I need a way to test my
code. Luckily Haskell provides even better ways of creating tests than other
languages I've seen so far. Still, I had some problems with organizing my
tests. I spent about a week reading tutorials, talking with people on #haskell
IRC channel, mailing on Haskell-cafe list and in the end even mailing authors of
particular libraries. This post summarizes all my efforts. It is not meant to be
a comprehensive tutorial on using testing libraries, though I will mention some
of the basics. I will mostly focus on logical organization of tests, automating
tests and adding some enhancements that improve capabilities of existing testing
libraries. For demonstration purposes I set up [a very simple stub project on
github](https://github.com/jstolarek/haskell-testing-stub) so you can clone it
and see how my approach works in practice.

Why?
====

Before I begin with presenting my approach to testing in Haskell it is important
to say why did I even bother to spend so much time and effort trying to figure
out test organization. The answer is very simple: none of the approaches I was
able to find in Haskell community suited my needs. I wanted to have three
things:

  * separate tests from the actual source code so that release version of my
    software doesn't depend on testing libraries,
  * organize tests in such a way that they are easy to manage (e.g. I wanted to
    be able to quickly locate tests for a particular module),
  * automate my tests.

I read some blog posts discussing how to test code in Haskell but none of
demonstrated approaches met all the above criteria. For example I noticed that
many people advocate putting tests in the same source file as the tested code,
arguing that tests act as a specification and should be bundled with the
code. Tests are part of specification, that is true. Nevertheless this is not
the reason to make our final executable (or library) depend on testing
libraries! That is why I had to find my own way of doing things.

Project overview
================

I will create a very simple library with tests to demonstrate all the
concepts. The library provides two basic signal processing operations - cyclic
shifts. A left cyclic shift by one moves all elements in a signal (list in our
case) to the left and the formerly first element becomes the last one. For
example a cyclic shift to the left of `[1,2,3,4]` produces `[2,3,4,1]`.  Right
shift works in similar way, only it shifts elements in opposite direction.
Shifts by one can be extended to shift signal by any natural number, e.g. shift
right by 3 of a `[1,2,3,4,5]` signal yields `[3,4,5,1,2]`.  Here's the complete
implementation of all these functions:

```haskell
module Signal.Utils where

cyclicOneShiftLeft :: (Num a) => [a] -> [a]
cyclicOneShiftLeft (x:xs) = xs ++ [x]

cyclicOneShiftRight :: (Num a) => [a] -> [a]
cyclicOneShiftRight xs = last xs : init xs

cyclicShiftLeft :: (Num a) => Int -> [a] -> [a]
cyclicShiftLeft _ [] = []
cyclicShiftLeft n xs
    | n > 0     = cyclicShiftLeft (n - 1) . cyclicOneShiftLeft $ xs
    | otherwise = xs

cyclicShiftRight :: (Num a) => Int -> [a] -> [a]
cyclicShiftRight _ [] = []
cyclicShiftRight n xs
    | n > 0     = cyclicShiftRight (n - 1) . cyclicOneShiftRight $ xs
    | otherwise = xs
```

Note that `cyclicOneShiftLeft` and `cyclicOneShiftRight` are partial
functions. They do not work for empty lists (the former one will cause a warning
about non-exhaustive pattern match). On the other hand `cyclicShiftLeft` and
`cyclicShiftRight` are total functions. They work for any list and any shift
value. These two functions will thus constitute external API of our library.

The above code is placed into module `Signal.Utils`. This module - and generally
all modules in a library - exports all its internal functions, thus breaking the
encapsulation principle. The library contains one main module (`Signal`) that
imports all modules of the library and exports only those functions that are
meant to be the part of library's public API. Thus Signal.hs file looks like
this:

```haskell
module Signal (
    cyclicShiftLeft
  , cyclicShiftRight
) where

import Signal.Utils
```

Finally, the .cabal file for the library contains such entries:

```
library
  hs-source-dirs:      src
  exposed-modules:     Signal
  other-modules:       Signal.Utils
  build-depends:       base
  ghc-options:         -Wall
```

This ensures that users will have access only to functions that we exposed via
`Signal` module. Internal functions of our library will remain hidden. Why did
we give up on module encapsulation within library? This will become clear in a
moment, when we talk about automating tests.

Overview of Haskell testing libraries
=====================================

Haskell offers [quite a few testing
libraries](http://hackage.haskell.org/packages/archive/pkg-list.html#cat:testing).
Among them there are two that seem to be in wide use and are in fact a standard
- [HUnit](http://hackage.haskell.org/package/HUnit) and
[QuickCheck](http://hackage.haskell.org/package/QuickCheck). HUnit, as the name
suggests, is a library providing xUnit capabilities in Haskell. The idea of
using HUnit is to feed some data to functions that we are testing and compare
the actual result returned by them to the result that we expect. If expected and
actual results differ the test fails. Here's a simple example:

```
testCyclicOneShiftRightHU :: Test
testCyclicOneShiftRightHU =
    "Cyclic one shift right" ~: [4,1,2,3]  @=? cyclicOneShiftRight [1,2,3,4]
```

This code creates an assertion that checks if the result of applying function
`cyclicShiftLeft` to list [1,2,3,4] returns [2,3,4,1]. This assertion is given a
name and assigned to a test. The test is run and if the assertion is true the
test succeeds. Otherwise it fails. That's all there is to it. If you used any
testing framework that uses the xUnit approach then you already know what HUnit
is all about. Note also, that we will NOT create tests in the form given
above. Instead we will create tests that create `Assertion`:

```
testCyclicOneShiftLeftAssertion :: Assertion
testCyclicOneShiftLeftAssertion =
    [4,1,2,3] @=? cyclicOneShiftRight [1,2,3,4]
```

This is required for integration with
[test-framework](http://batterseapower.github.com/test-framework/) library,
which I will discuss in a moment.

One thing to note is that HUnit lacks assertions that would allow to compare
floating point numbers effectively. A problem with floating points in any
language, not only Haskell, is that comparing them using equality sign my give
unexpected results due to round-off errors. Every xUnit testing framework I've
seen so far provided an "almost equal" assertion that allowed to compare floats
with some given precision. Since there is no such assertion in HUnit I created
it myself and placed in the `Test.Utils` module:

```haskell
class AEq a where
    (=~) :: a -> a -> Bool

instance AEq Double where
    x =~ y = abs ( x - y ) < (1.0e-8 :: Double)

(@=~?) :: (Show a, AEq a) => a -> a -> HU.Assertion
(@=~?) expected actual  = expected =~ actual HU.@? assertionMsg
    where
      assertionMsg = "Expected : " ++ show expected ++
                     "\nActual   : " ++ show actual
```

I created `AEq` (Almost Equal) type class defining "almost equal" operator and
created instances for `Double`, lists and `Maybe` (see source code) and then
created HUnit assertion that works just like other assertions. In our code this
assertion is not really necessary, but I included it since I think it is very
helpful if you want to test functions performing numerical computations.

Another approach to testing is offered by QuickCheck.  Instead of creating test
data a programmer defines properties that tested functions should always obey
and QuickCheck library takes care of automatically generating test data. An
example property is that if we take a signal of length n and shift it by n
(either left or right) we should get the original signal as a result. Here's how
this property looks in QuickCheck:

```haskell
propLeftShiftIdentity :: [Double] -> Bool
propLeftShiftIdentity xs =
    cyclicShiftLeft (length xs) xs == xs
```

Another property that we can define is that composition of left shift by one and
right shift by one is an identity function. In case of our `cyclicOneShiftLeft`
and `cyclicOneShiftRight` functions this will not exactly be true, because these
functions don't work for empty lists. This means that empty lists must be
excluded from the test:

```haskell
propCyclicOneShiftIdentity1 :: [Double] -> Property
propCyclicOneShiftIdentity1 xs =
    not (null xs) ==>
        cyclicOneShiftLeft (cyclicOneShiftRight xs) == xs
```

As you can see QuickCheck properties return either `Bool` or `Property` ((You
can think of `Property` as something that can be evaluated to true or
false)). When these tests are run QuickCheck generates 100 random lists to see
if the property holds for them. If for some input data the property fails then
QuickCheck reports a failed test together with data that lead to failure.

We know how to write tests. Now it is time to run all of them in one coherent
testing suite. For this we will use
[test-framework](http://batterseapower.github.com/test-framework/). This
framework was designed to allow using HUnit and QuickCheck tests together in a
uniform fashion. I think this is not the only such framework, but I think that
it does its job very well so I did not feel the need to look for anything
different. Here is main testing module responsible for running all tests:

```haskell
module Main (
    main
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Signal.UtilsTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "Signal shifts"
    [
       testGroup "Migrated from HUnit" $ hUnitTestToTests testCyclicOneShiftRightHU
     , testProperty "L/R one shift composition" propCyclicOneShiftIdentity1
     , testProperty "Left shift identity" propLeftShiftIdentity
    ]
  ]
```

The `tests` function is the most important one. It groups tests into groups and
assigns names to both groups and individual tests. These names will be useful
for locating tests that failed. Notice the test group named "Migrated from
HUnit". As the name suggests these are HUnit tests that were adjusted to work
with test-framework, which means that if you already have HUnit tests you can
easily migrate to test-framework. Nevertheless test-framework expects an
`Assertion` by default and that is why we created such test earlier. Notice also
that in the project on github there are more tests than shown above. These are
however very similar to the functions already shown.

Automating tests using cabal
============================

It is time to automate our tests so that they can be easily rerun. For that we
will use `cabal`, but before we start we need to discuss how to organize our
tests and place them within project's directories.

In Java it is a standard practice to put source and tests into two separate
directories located in the project root. These directories have identical
internal structure. This is due to two facts. First, Java names packages
according to directory in which they are located ((Haskell uses the same
approach and if I remember correctly it was adapted from Java. )), so files
`src/SomePackage/SomeSubpackage/SomeClass.java` and
`tests/SomePackage/SomeSubpackage/SomeClassTest.java` are considered to be
in the same package. The second reason is that classes located in the same
package can have access to their protected fields, which allows tests to access
internals of a class. This approach breaks object encapsulation within a single
package, but this is generally acceptable and not a real problem.

I decided to follow similar approach in Haskell. In the project directory I have
`src` and `tests` directories that allow me to separate application/library code
from tests. Both directories have the same internal structure. Files containing
tests for a module are named like that module but with "Test" appended before
file extension. In my simple project this is demonstrated by file
`src/Signal/Utils.hs` and `tests/Signal/UtilsTest.hs`. This way it is easy to
locate tests for a particular module. This mimics approach used in Java, but
there is one important difference. In Java tests organized in such a way have
access to unexposed internals of a class, but this does not happen in
Haskell. If a module does not expose its internal functions there is no way for
tests to reach them. I know two solutions to this problem. First is the one I
used - export everything from the modules. It was suggested to me by Matthew
West on Haskell-Cafe. The second one is using CPP language extension, which will
cause source files to be processed by C preprocessor. To use this method our
`Signal.Utils` would have to be modified like this:

```
{-# LANGUAGE CPP #-}
module Signal.Utils (
    cyclicShiftLeft
  , cyclicShiftRight
#ifdef TEST
  , cyclicOneShiftLeft
  , cyclicOneShiftRight
#endif
  ) where
```

We also have to add `cpp-options: -DTEST` entry in test section of project's
.cabal file (this will be explained in next paragraph). It might also be
convenient to create `.ghci` file in the project directory containing `:set
-DTEST -isrc -itest`, which will enable `TEST` flag within ghci. This solution
was pointed to me by Simon Hengel, also on Haskell-Cafe. I didn't use it because
it doesn't look very well and feels more like a hack than a real
solution. Nevertheless this is also a way of doing things and it may better suit
your needs than the one I chose.

With all this knowledge we can finally use [`cabal`'s support for
testing](http://cabaltest.blogspot.com/)[^1].  For this we must add another
section to .cabal file of our project:

```
test-suite signal-tests
  type:              exitcode-stdio-1.0
  hs-source-dirs:    tests, src
  main-is:           MainTestSuite.hs
  build-depends:     base,
                     HUnit,
                     QuickCheck,
                     test-framework,
                     test-framework-hunit,
                     test-framework-quickcheck2
```

Let's walk through this configuration and see what it does. The `type` field
defines testing interface used by tests. Theoretically there are two accepted
values: `exitcode-stdio-1.0` and `detailed-1.0`. First one means that test
executable works by displaying test results on the screen and indicates possible
failure by non-zero exit code. Second option, `detailed-1.0`, is meant for test
suites that export some special symbols that allow test results to be
intercepted and further processed by Cabal. Sadly, while this second options
seems very interesting, it is not fully implemented yet and there is no way to
make use of it. Thus, for the time being, we are left with
`exitcode-stdio-1.0`. Rest of the entries should be self-explanatory. The
`hs-source-dirs` option points to source directories. Note that it includes both
the `src` and `tests` directories. Next entry defines a file containing `main ::
IO ()`. Finally there are dependencies on external libraries.

To run tests you need to perform:

```
cabal configure --enable-tests
cabal build
cabal test
```

This will build both the library and testing binary and run the tests. Here's
how the test output looks like:

```
[killy@xerxes : ~] cabal test
Running 1 test suites...
Test suite wavelet-hs-test: RUNNING...
Test suite wavelet-hs-test: PASS
Test suite logged to: dist/test/haskell-testing-stub-1.0.0-signal-tests.log
1 of 1 test suites (1 of 1 test cases) passed.
```

The detailed result is logged to a file. If any of the tests fails then whole
output from the suite is displayed on the screen (try it by supplying incorrect
expected value in a HUnit test).

Cabal has also support for testing code coverage with HPC. To use it run `cabal
configure --enable-tests --enable-library-coverage`. This should enable HPC when
running tests, automatically exclude testing code from the coverage summaries
and generate HTML files. Sadly, I've been affected by some bug which results in
HTML files not being generated and testing code not being excluded from the
report. I reported this to the author so I hope it will get fixed some day.

Enhancing HUnit tests with data providers
=========================================

In the beginning of my post I mentioned that TestNG library for Java offered
better capabilities than JUnit. To me one of key features of TestNG were
DataProviders. They allowed user to define a parametrized test function that
contained test logic with assertions. For each such parametrized test user had
to supply a data provider, that is a function that returned many sets of testing
data that could be passed to this single test. This allowed to neatly separate
test logic from test data. TestNG of course treated such tests as many different
tests and it was possible for one test set to fail and others to pass. This was
a big step forward, because earlier solutions to such problems lead either to
duplication of test logic (violation of
[DRY](http://en.wikipedia.org/wiki/Don%27t_repeat_yourself)) or locked multiple
test data within one test, which caused whole test to fail on first data set
that caused failure.

There are no built-in data providers in HUnit but we can easily add them. In
`Test.Utils` module I created a function for this:

```haskell
import qualified Test.Framework                 as TF
import qualified Test.Framework.Providers.HUnit as TFH
import qualified Test.HUnit                     as HU

testWithProvider :: String -> (a -> HU.Assertion) -> [a] -> TF.Test
testWithProvider testGroupName testFunction =
    TF.testGroup testGroupName . map createTest . zipWith assignName [1::Int ..]
      where
        createTest (name, dataSet)   = TFH.testCase name $ testFunction dataSet
        assignName setNumber dataSet = ("Data set " ++ show setNumber, dataSet)
```

This function is very similar to other functions defined within test-framework
and thus should be considered more an enhancement to test-framework than
HUnit. The `testWithProvider` function takes name for a group of tests, a test
function, a list of test data (that's the data provider) and returns a
Test. Note that last parameter is omitted due to currying. Tests within created
group are named "Dataset n", where n is the number. This allows to easily locate
failing test data set. Now we can write HUnit tests like this:

```haskell
testCyclicShiftLeft :: (Int, [Double], [Double]) -> Assertion
testCyclicShiftLeft (n, xs, expected) =
    expected @=~? cyclicShiftLeft n xs

dataCyclicShiftLeft :: [(Int, [Double], [Double])]
dataCyclicShiftLeft =
    [
       ( 0, [],        []        )
     , ( 2, [1,2,3,4], [3,4,1,2] )
     , ( 4, [1,2],     [1,2]     )
    ]
```

Notice that test data are passed as tuples. Finally, we can add these tests to a
suite like this:

```haskell
tests :: [Test]
tests =
  [
    testGroup "Signal shifts"
    [
      ....
      , testWithProvider "Cyclic left shift" testCyclicShiftLeft
                                             dataCyclicShiftLeft
    ]
  ]
```

One might argue that we really don't need data providers, since there is
QuickCheck that generates test data automatically and there is no need for
programmer to do it. That is a good point, but I think that data provider
capability comes in handy when we want to be sure that border cases of an
algorithm are properly tested.

Summary
=======

When I started with code testing in Haskell I had three goals in my mind:
separation of tests from the code, organizing them in a manageable and flexible
way and finally automating tests. The approach I demonstrated meats all these
goals and is based on my experience in other programming languages. So far it
works very well for me, but I dare not argue that this is the only way of doing
things, not even to say that it's the best one. As always I'm open to discussion
and suggestions for improvements.

**LAST MINUTE NEWS:** As I was finishing writing of this post, Roman Cheplyaka
announced on Haskell-Cafe release of his
[test-framework-golden](http://hackage.haskell.org/package/test-framework-golden)
library. This library is meant for "Golden testing" which works by writing test
output to a file and comparing it with some expected ("golden") file. I never
used this approach, but this library also integrates with test-framework so it
could be used in my sample project without problems. That's the kind of test
extensibility I like!

**UPDATE (21/10/2012):** Read a follow-up post about [code benchmarking in
Haskell](/posts/2012-10-21-code-benchmarking-in-haskell.html).

[^1]: When I say cabal I really mean cabal-install, a command-line tool used for
  installing Haskell packages, not the Cabal library. The confusion arises
  because cabal-install executable is named cabal.

