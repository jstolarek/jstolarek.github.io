---
title: Strange benchmarking results for FFI bindings
date: 2012-12-01
---

Strange benchmarking results for FFI bindings
=============================================

It looks like I am getting pretty good at getting hit by Haskell bugs. My
[previous post](/posts/2012-11-17-waiting-for-garbage-collection-can-kill-parallelism.html)
described behaviour that turned out to be [a bug in
GHC](http://hackage.haskell.org/trac/ghc/ticket/367) (thanks to Joachim Breitner
for pointing this out).  Now I found problems with benchmarking FFI bindings
using [method described a month
ago](/posts/2012-11-02-benchmarking-c-functions-using-foreign-function-interface.html).

I work on a project in which the same algorithm is implemented using different
data structures - one implementation is done in C, another using Vector library
and yet another using Repa. Everything is benchmarked with Criterion and C
implementation is the fastest one (look at first value after `mean` - this is
mean time of running a function):

```
benchmarking DWT/C1
mean: 87.26403 us, lb 86.50825 us, ub 90.05830 us, ci 0.950
std dev: 6.501161 us, lb 1.597160 us, ub 14.81257 us, ci 0.950

benchmarking DWT/Vector1
mean: 209.4814 us, lb 208.8169 us, ub 210.5628 us, ci 0.950
std dev: 4.270757 us, lb 2.978532 us, ub 6.790762 us, ci 0.950
```

This algorithm uses a simpler `lattice` function that is repeated a couple of
times. I wrote benchmarks that measure time needed by a single invocation of
`lattice`:

```
benchmarking C1/Lattice Seq
mean: 58.36111 us, lb 58.14981 us, ub 58.65387 us, ci 0.950
std dev: 1.260742 us, lb 978.6512 ns, ub 1.617153 us, ci 0.950

benchmarking Vector1/Lattice Seq
mean: 34.97816 us, lb 34.87454 us, ub 35.14377 us, ci 0.950
std dev: 661.5554 ns, lb 455.7412 ns, ub 1.013466 us, ci 0.950
```

Hey, what's this!? Vector implementation is suddenly faster than C? Not possible
given that DWT in C is faster than DWT using Vector. After some investigation it
turned out that the first C benchmark runs correctly while subsequent benchmarks
of C functions take performance hit. I managed to create a simple code that
demonstrates the problem in as few lines as possible. I implemented a copy
function in C that takes an array and copies it to another array. Here's
`copy.c`:

```c
#include
#include "copy.h"

double* c_copy( double* inArr, int arrLen ) {
  double* outArr = malloc( arrLen * sizeof( double ) );

  for ( int i = 0; i < arrLen; i++ ) {
    outArr[ i ] = inArr[ i ];
  }

  return outArr;
}
```

and `copy.h`:

```c
#ifndef _COPY_H_
#define _COPY_H_

double* c_copy( double*, int );

#endif
```

I wrote a simple binding for that function and benchmarked it multiple times in
a row:

```haskell
module Main where

import Criterion.Main
import Data.Vector.Storable hiding (copy)
import Control.Monad (liftM)
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "copy.h"
  c_copy :: Ptr CDouble -> CInt -> IO (Ptr CDouble)

signal :: Vector Double
signal = fromList [1.0 .. 16384.0]

copy :: Vector Double -> Vector Double
copy sig = unsafePerformIO $ do
    let (fpSig, _, lenSig) = unsafeToForeignPtr sig
    pLattice <- liftM castPtr $ withForeignPtr fpSig $ \\ptrSig ->
                c_copy (castPtr ptrSig) (fromIntegral lenSig)
    fpLattice <- newForeignPtr finalizerFree pLattice
    return $ unsafeFromForeignPtr0 fpLattice lenSig

main :: IO ()
main = defaultMain [
         bgroup "FFI" [
           bench "C binding" $ whnf copy signal
         , bench "C binding" $ whnf copy signal
         , bench "C binding" $ whnf copy signal
         , bench "C binding" $ whnf copy signal
         , bench "C binding" $ whnf copy signal
         , bench "C binding" $ whnf copy signal
         , bench "C binding" $ whnf copy signal
         , bench "C binding" $ whnf copy signal
         , bench "C binding" $ whnf copy signal
         ]
       ]
```

Compiling and running this benchmark with:

```
$ ghc -O2 -Wall -optc -std=c99 ffi_crit.hs copy.c
$ ./ffi_crit -g
```

gave me this results:

```
benchmarking FFI/C binding
mean: 17.44777 us, lb 16.82549 us, ub 19.84387 us, ci 0.950
std dev: 5.627304 us, lb 968.1911 ns, ub 13.18222 us, ci 0.950

benchmarking FFI/C binding
mean: 45.46269 us, lb 45.17545 us, ub 46.01435 us, ci 0.950
std dev: 1.950915 us, lb 1.169448 us, ub 3.201935 us, ci 0.950

benchmarking FFI/C binding
mean: 45.79727 us, lb 45.55681 us, ub 46.26911 us, ci 0.950
std dev: 1.669191 us, lb 1.029116 us, ub 3.098384 us, ci 0.950
```

The first run takes about 17µs, later runs take about 45µs.  I found this result
repeatable across different runs, although in about 10-20% of runs all
benchmarks - including the first one - took about 45µs. I obtained this results
on GHC 7.4.1, openSUSE 64-bit linux with 2.6.37 kernel, [Intel Core i7 M
620](http://ark.intel.com/products/43560/Intel-Core-i7-620M-Processor-4M-Cache-2_66-GHz)
CPU. I posted this on Haskell-cafe and #haskell. Surprisingly nobody could
replicate the result! I was confused so I gave it a try on my second machine:
Debian Squeeze, 64-bit, GHC 7.4.2, 2.6.32 kernel, [Intel Core 2 Due
T8300](http://ark.intel.com/products/33099/Intel-Core2-Duo-Processor-T8300-3M-Cache-2_40-GHz-800-MHz-FSB)
CPU. At first the problem did not appear:

```
benchmarking FFI/C binding
mean: 107.3837 us, lb 107.2013 us, ub 107.5862 us, ci 0.950
std dev: 983.6046 ns, lb 822.6750 ns, ub 1.292724 us, ci 0.950

benchmarking FFI/C binding
mean: 108.1152 us, lb 107.9457 us, ub 108.3052 us, ci 0.950
std dev: 916.2469 ns, lb 793.1004 ns, ub 1.122127 us, ci 0.950
```

All benchmarks took about 107µs. Now watch what happens when I increase size of
the copied vector from 16K elements to 32K:

```
benchmarking FFI/C binding
mean: 38.50100 us, lb 36.71525 us, ub 46.87665 us, ci 0.950
std dev: 16.93131 us, lb 1.033678 us, ub 40.23900 us, ci 0.950

benchmarking FFI/C binding
mean: 209.9733 us, lb 209.5316 us, ub 210.4680 us, ci 0.950
std dev: 2.401398 us, lb 2.052981 us, ub 2.889688 us, ci 0.950
```

This first run is 2.5 time faster (!), while all other runs are two times
slower. While the latter could be expected, the former certainly is not.

So what exactly is going on? I tried analysing eventlog of the program but I
wasn't able to figure out the cause of the problem. I noticed that if I comment
out the loop in C function so that it only allocates memory and returns an empty
vector then the problem disappears. Someone on Haskell-cafe suggested that these
are cache effects, but I am sceptical about this explanation. If this is caused
by cache then why did the first benchmark sped up when size of the vector was
increased? And why does this effect occur for 16K length vectors on a machine
with 4MB cache, while machine with 3MB cache needs twice longer vector for the
problem to occur? So if anyone has a clue what causes this strange behaviour
please let me know. I would be happy to resolve that since now result of my
benchmarks are distorted (perhaps yours are too only you didn't notice).

