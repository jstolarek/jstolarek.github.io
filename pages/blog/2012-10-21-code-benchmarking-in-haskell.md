---
title: Code benchmarking in Haskell
date: 2012-10-21
---

Code benchmarking in Haskell
============================

Three weeks ago I wrote about [code testing in
Haskell](2012-10-05-code-testing-in-haskell/).  Today I will discuss how to
benchmark code written in Haskell and how to integrate benchmarks into a
project. For demonstration purposes I extended [my sample project on
github](https://github.com/jstolarek/haskell-testing-stub) so now it shows how
to create both tests and benchmarks.

Overview of Criterion benchmarking library
==========================================

While there was a lot of testing libraries to choose from, it seems that
benchmarking is dominated by only one library - Bryan O'Sullivan's
[criterion](http://hackage.haskell.org/package/criterion). To get started with
it you should read [this post on Bryan's
blog](http://www.serpentine.com/blog/2009/09/29/criterion-a-new-benchmarking-library-for-haskell/).
I will present some of the basics in today's post and will also mention a couple
of things that are undocumented.

Writing benchmarks for a functional language like Haskell is a bit tricky. There
are no side effects in pure functional code, which means that after computing
value of a function once it can be memoized and reused later without need for
recomputing. This is of course not what we want during benchmarking. Criterion
takes care of that, but requires that benchmarks be written in a special
way. Let's look at an example banchmark for our shift function ((See my [post on
testing](2012-10-05-code-testing-in-haskell/) if you don't know what shift I'm
talking about)):

```haskell
bench "Left shift" $ nf (cyclicShiftLeft 2) [1..8192]
```

The [`nf`](http://hackage.haskell.org/packages/archive/criterion/latest/doc/html/Criterion-Types.html#v:nf)
function is the key here. It takes two parameters: first is the benchmarked
function saturated with all but its last argument; second is the last parameter
to the benchmarked function. The type of `nf` is:

```
ghci> :t nf
nf :: Control.DeepSeq.NFData b => (a -> b) -> a -> Pure
```

When the benchmark is run `nf` applies the argument to the function and
evaluates it to normal form, which means that the result gets fully
evaluated. This is needed to ensure that laziness doesn't distort the outcomes
of a benchmark.

Code shown above will work perfectly, but I find such way of creating benchmarks
very inconvenient due to four reasons:

  * presence of [magic numbers](http://en.wikipedia.org/wiki/Magic_number_%28programming%29#Unnamed_numerical_constants)
  * problems with creating more complex input data
  * verbosity of benchmarks when there are many parameters taken by the
    benchmarked function
  * problems of keeping consistency between benchmarks that should take the same
    inputs (it wouldn?t make sense to benchmark shift left and right functions
    with signals of different length)

To deal with these problems I decided to write my benchmarks using wrappers:

```haskell
bench "Left shift"  $ nf benchCyclicShiftLeft paramCyclicShift
```

The `benchCyclicShiftLeft` function takes a tuple containing all the data needed
for a benchmarked function:

```haskell
{-# INLINE benchCyclicShiftLeft #-}
benchCyclicShiftLeft :: (Int, [Double]) -> [Double]
benchCyclicShiftLeft (n, sig) = cyclicShiftLeft n sig
```

The `INLINE` pragma is used to make sure that the function doesn't add
unnecessary call overhead. As you have probably guessed, the `paramCyclicShift`
takes care of creating the tuple. In my code `paramCyclicShift` is actually a
wrapper around such function:

```
dataShift :: RandomGen g => g -> Int -> Int -> (Int, [Double])
dataShift gen n sigSize = (n, take sigSize $ randoms gen)
```

To keep benchmarking code easily manageable I organize it similarly to tests.
Project root contains `bench` directory with structure identical to `src` and
`tests` directories. File containing benchmarks for a module is named like that
module but with ?Bench? appended before file extension. For example
`benchCyclicShiftLeft` and `dataShift` functions needed to benchmark code in
`src/``Signal/``Utils.hs` are placed in `bench/``Signal/``UtilsBench.hs`. Just
like tests, benchmarks are assembled into one suite in
`bench/``MainBenchmarkSuite.hs` file:

```haskell
import qualified BenchParam        as P
import qualified Signal.UtilsBench as U

main :: IO ()
main = newStdGen >>= defaultMainWith benchConfig (return ()) . benchmarks

benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
    let paramCyclicShift = U.dataShift gen P.shiftSize P.sigSize
    in [
      bgroup "Signal shifts"
      [
        bench "Left shift"  $ nf U.benchCyclicShiftLeft  paramCyclicShift
      , bench "Right shift" $ nf U.benchCyclicShiftRight paramCyclicShift
      ]
    ]

benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }
```

The most important part is the `benchmarks` function, which takes a random
number generator and assembles all benchmarks into one suite (**UPDATE
(24/10/2012):** read a [follow-up post on random data
generation](2012-10-24-code-benchmarking-in-haskell-some-thoughts-about-random-data-generation/)).
Just as with tests we can create logical groups and assign names. A cool thing
is a [`bcompare`](http://hackage.haskell.org/packages/archive/criterion/latest/doc/html/Criterion-Types.html#v:bcompare)
function. It takes a list of benchmarks, assumes that the first one is the
reference one and reports the relative speed of other functions. In my code I
use let to introduce `paramCyclicShift` wrapper around `dataShift`
function. This allows to use the same input data for both benchmarks. Of course
let is not necessary, but it allows to avoid code repetition. I also use
`shiftSize` and `sigSize` functions from `BenchParam` module. These functions
are defined as constant values and ensure that there is a single source of
configuration. Using a separate module for this is a disputable choice - you may
as well define `shiftSize` and `sigSize` in the same let binding as
`paramCyclicShift`. The main function creates a random generator, uses bind to
pass it to benchmarks function and finally runs the created suite. I use custom
configuration created with `benchConfig` function to enable garbage collection
between benchmarks ((This can also be enabled with a command line switch -g, but
doing this in code ensures that it is always turned on.)). I noticed that
enabling GC is generally a good idea, because otherwise it will kick in during
the benchmarking and distort the results.

The good thing about this approach is that benchmarks are concise, follow the
structure of the project, magic numbers can be eliminated and it is easy to
ensure that benchmarked functions get the same data when needed.

Automating benchmarks using cabal
=================================

Guess what - [cabal has a built in support for
benchmarks](http://blog.johantibell.com/2012/04/cabal-bench.html)! All we need
to do is add one more entry to project's cabal file:

```
benchmark signal-bench
  type:             exitcode-stdio-1.0
  hs-source-dirs:   src, bench
  main-is:          MainBenchmarkSuite.hs
  build-depends:    base,
                    criterion,
                    random
  ghc-options:      -Wall
                    -O2
```

Structure of this entry is identical to the one related to tests, so I will skip
the discussion. To run the benchmarks issue these three commands:

```
cabal configure --enable-benchmarks
cabal build
cabal bench
```

This couldn't be easier. Criterion will produce quite a detailed output on the
console. As I already said, it's all explained [on Bryan's
blog](http://www.serpentine.com/blog/2009/09/29/criterion-a-new-benchmarking-library-for-haskell/),
but some of the mentioned features are not present any more in criterion. Most
importantly, you cannot display results in a window or save them to png
file. Luckily, there is an even fancier feature instead. Run benchmarks like
this:

```
cabal bench --benchmark-options="-o report.html"
```

And criterion will produce a nice html report with interactive graphs.

A few more comments on benchmarking
===================================

All of this looks very easy and straightforward, but I actually spent about
three days trying to figure out whether my code is benchmarked correctly. The
problem is laziness. When I call my `dataShift` function the random data isn't
created until it is demanded by the cyclic shift function. This means that the
time needed to actually create the random data would be incorporated in the
benchmark. It turns out that criterion is smart enough not to do so. The first
run of each benchmark forces the evaluation of lazily created data, but its run
time is discarded and not included in the final results. The evaluated data is
used in the subsequent runs of the benchmark. You can test this easily by doing
something like this:

```haskell
dataShift gen n sigSize = unsafePerformIO $ do
    delayThread 1000000
    return (n, take sigSize $ randoms gen)
```

This will cause a delay of 1 second each time `dataShift` function is
evaluated. When you run the benchmark you will notice that criterion will
estimate time needed to run the benchmarks to be over a hundred seconds (this
information is not displayed when the estimated time is short), but it will
finish much faster and there should be no difference in the performance of
benchmarked functions. This will even work if you create your benchmark like
this:

```haskell
bench "Left shift" $ nf U.benchCyclicShiftLeft
                     (U.dataShift gen P.shiftSize P.sigSize)
```

Another problem I stumbled upon quite quickly was type constraint on the `nf`
function: it requires that the return value belongs to `NFData`. This type class
represents data that can be evaluated to normal form (i.e. can be fully
evaluated). Most of standard Haskell data types belong to it, but this is not
the case with data containers like Vector or Repa arrays. For such cases there
is a [`whnf`](http://hackage.haskell.org/packages/archive/criterion/latest/doc/html/Criterion-Types.html#v:whnf)
function that doesn't have that constraint, but it only evaluates the result to
weak head normal form (i.e. to the first data constructor or lambda). Luckily,
for unboxed arrays containing primitive types weak head normal form is the same
as the normal form so the problem with Vectors is solved.

I also quickly realised that benchmarking results are not as repeatable as I
would like them to be. This is of course something I could expect in a
multitasking operating system. I guess that ultimate solution to this would be
booting into single user mode and closing all the background services like cron
and network.

I am also experimenting with tuning the options of the runtime system, as they
can also influence performance considerably. In a project I am currently working
on I benchmark parallel code and got better performance results by setting the
thread affinity option (`-qa` command line switch) and disabling parallel
garbage collection (`-g1` switch). I also found
[ThreadScope](http://www.haskell.org/haskellwiki/ThreadScope) to be extremely
useful for inspecting events occurring during program runtime. It works great
also for a single threaded application, as it shows when the garbage collection
happens and that alone is very useful information.

Summary
=======

Up till now I never wrote repeatable benchmarks for my code and relied on a
simple methods like measuring the wall time of a single run of my
application. Criterion seems like a big step forward and, thanks to an instant
feedback it provides, I already managed to speed up my parallel code by a factor
of 2. I guess that most of all I like the way cabal allows to seamlessly
integrate tests and benchmarks into my project - this speeds up development
considerably.

Remember that all the source code used in this post is available as a [project
on github](https://github.com/jstolarek/haskell-testing-stub). There are also
some additional comments in the code.

