---
title: Two new libraries to extend tasty testing framework
date: 2014-02-04
---

Two new libraries to extend tasty testing framework
===================================================

When I [recently wrote about porting my haskell-testing-stub project to
tasty](2014-01-26-code-testing-in-haskell-revisited-with-tasty/) I mentioned
that [test-framework](http://hackage.haskell.org/package/test-framework) still
has more libraries than [tasty](http://hackage.haskell.org/package/tasty). I
decided to contribute to changing that and released two small packages that
extend tasty with extra functionality:

  - `tasty-hunit-adapter` allows to import existing HUnit tests into tasty
    ([hackage](http://hackage.haskell.org/package/tasty-hunit-adapter),
    [github](https://github.com/jstolarek/tasty-hunit-adapter)):

    ```haskell
    module Main where

    import Test.HUnit               ( (~:), (@=?)            )
    import Test.Tasty               ( defaultMain, testGroup )
    import Test.Tasty.HUnit.Adapter ( hUnitTestToTestTree    )

    main :: IO ()
    main = defaultMain $ testGroup "Migrated from HUnit" $
           hUnitTestToTestTree ("HUnit test" ~: 2 + 2 @=? 4)
    ```

  - `tasty-program` allows to run external program and test whether it
    terminates successfully
    ([hackage](http://hackage.haskell.org/package/tasty-program),
    [github](https://github.com/jstolarek/tasty-program)):

    ```haskell
    module Main (
      main
     ) where

    import Test.Tasty
    import Test.Tasty.Program

    main :: IO ()
    main = defaultMain $ testGroup "Compilation with GHC" $ [
        testProgram "Foo" "ghc" ["-fforce-recomp", "foo.hs"]
                    Nothing
      ]
    ```

This package has only this basic functionality at the moment. A missing feature
is the possibility of logging stdout and stderr to a file so that it can later
be inspected or perhaps used by a golden test (but for the latter [tasty needs
test dependencies](https://github.com/feuerbach/tasty/issues/48)).

