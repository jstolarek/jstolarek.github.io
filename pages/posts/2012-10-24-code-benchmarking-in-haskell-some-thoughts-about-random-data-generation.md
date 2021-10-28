---
title: Code benchmarking in Haskell - some thoughts about random data generation
date: 2012-10-24
---

Code benchmarking in Haskell - some thoughts about random data generation
=========================================================================

In my last post I showed you how to use
[criterion](http://hackage.haskell.org/package/criterion) library to write
benchmarks for Haskell code. In [tutorial
project](https://github.com/jstolarek/haskell-testing-stub/) that I created to
demonstrate my ideas I decided to generate random data for benchmarking. [Bryan
O'Sullivan](http://www.serpentine.com/posts/) has [commented on my
approach](http://www.reddit.com/r/haskell/comments/11w5c1/code_benchmarking_in_haskell_using_criterion_and/)
that "the code (...) that generates random inputs on every run would be a good
antipattern for performance testing." After giving some thought to his words I
think I see his point.

The code that Bryan refers to looks like this:

```haskell
main :: IO ()
main = newStdGen >>= defaultMainWith benchConfig (return ()) . benchmarks

benchmarks :: RandomGen g => g -> \[Benchmark\]
benchmarks = ...
```

Each time a benchmark suite is run a different random numbers generator is
created with `newStdGen`. This generator is then used by the `benchmarks`
function to create values used for benchmarking. When I designed this **I made
an assumption that values of the data don't influence the flow of
computations**. I believe that this holds for the shift functions I benchmarked
in my tutorial. It doesn't really matter what values are in the shifted list. As
long as lists have the same length on different runs of the benchmark the
results are comparable, but if you want to have the same random values generated
on each run you can create a `StdGen` based on a seed that you supply. The
modified `main` function would look like this:

```
main = return (mkStdGen 123456) >>= defaultMainWith benchConfig (return ()) . benchmarks
```

What happens however when data values do influence the flow of computation? In
that case you definitely don't want `newStdGen`, as it would make results of
benchmark incomparable between different runs: you wouldn't know if the speed-up
is caused by changes in the code or data. It is also very likely that you don't
want to use `mkStdGen`. Why? Well, you would certainly get results comparable
between different runs. The problem is that you wouldn't know the
characteristics of the data used for this particular benchmark. For example
let's assume that your algorithm executes faster when the data it processes
contains many zeros. You benchmark the algorithm with random values created with
a fixed `StdGen` and get a very good result. But how many zeros were in the data
used for benchmarking? Perhaps 50% of input were zeros? You don't know that. In
this case you definitely want to prepare your own input data sets (e.g. one with
many zeros and one without any) to measure the performance of your code based on
input it receives. I guess Bryan is right here - careless use of random data
generation for benchmarking can be a shot in the foot.

