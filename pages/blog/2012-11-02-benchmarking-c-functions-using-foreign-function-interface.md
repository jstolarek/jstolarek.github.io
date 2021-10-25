---
title: Benchmarking C functions using Foreign Function Interface
date: 2012-11-02
---

Benchmarking C functions using Foreign Function Interface
=========================================================

I am currently working on implementing Discrete Wavelet Transform (DWT) in
Haskell. I want to make use of Haskell's parallel programing capabilities to
implement an algorithm that can take advantage of multiple CPU cores. My
previous posts on [testing](2012-10-05-code-testing-in-haskell/) and
[benchmarking](2012-10-21-code-benchmarking-in-haskell/) were by-products of
this project, as I needed to ensure reliability of my implementation and to
measure its performance. The key question that is in my head all the time is
"can I write Haskell code that outperforms C when given more CPU cores?". To
answer this question I needed a way to benchmark performance of algorithm
written in C and I must admit that this problem was giving me a real
headache. One obvious solution was to implement the algorithm in C and measure
its running time. This didn't seem acceptable. I use
[Criterion](http://hackage.haskell.org/package/criterion) for benchmarking and
it does lots of fancy stuff like measuring clock resolution and calculating
[kernel density
estimation](http://en.wikipedia.org/wiki/Kernel_density_estimation).  So unless
I implemented this features in C (read: re-implement the whole library) the
results of measurements would not be comparable.

Luckily for me there is a better solution: Foreign Function Interface
(FFI). This is an extension of Haskell 98 standard - and part of Haskell 2010 -
that allows to call functions written in C ((Specification mentions also the
calling conventions for other languages and platforms (Java VM, .Net and C++)
but I think that currently there is no implementation of these.)). This means
that I could write my function in C, wrap it in a pure Haskell function and
benchmark that wrapper with Criterion. The results would be comparable with
Haskell implementation, but I was afraid that overheads related to data copying
would affect the performance measurements. As it turned out I was wrong.

I started with [chapter 17 of Real World
Haskell](http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html).
It presents a real world example - I guess that title of the book is very
adequate - of creating bindings for an already existing library. Sadly, after
reading it I felt very confused. I had a general idea of what should be done but
I didn't understand many of the details. I had serious doubts about proper usage
of `Ptr` and `ForeignPtr` data types and these are in fact very important when
working with FFI. Someone on #haskell advised me to read the [official
specification of FFI](http://www.cse.unsw.edu.au/~chak/haskell/ffi/) and this
was a spot-on. This is actually one of the few official specifications that are
a real pleasure to read (if you read
[R5RS](http://www.schemers.org/Documents/Standards/R5RS/) then you know what I
mean). It is concise (30 pages) and provides a comprehensive overview of all
data types and functions used for making foreign calls.

After reading the specification it was rather straightforward to write my own
bindings to C. Here's a prototype of called C function, located in `dwt.h`
header file:

```c
double* c_dwt(double* ls, int ln, double* xs, int xn);
```

The corresponding `dwt.c` source file contains:

```c
double* c_dwt( double* ls, int ln, double* xs, int xn ) {
  double* ds = malloc( xn * sizeof( double ) );

  // fill ds array with result

  return ds;
}
```

The important thing is that C function mallocates new memory which we will later
manage using Haskell's garbage collector. Haskell binding for such a function
looks like this:

```haskell
foreign import ccall unsafe "dwt.h"
  c_dwt :: Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO (Ptr CDouble)
```

Here's what it does: `ccall` denotes C calling convention, `unsafe` improves
performance of the call at the cost of safety ((Calls need to be safe only when
called C code calls Haskell code, which I think is rare)) and `"dwt.h"` points
to a header file. Finally, I define the name of the function and it's type. This
name is the same as the name of original C function, but if it were different I
would have to specify name of C function in the string that specifies name of
the header file. You probably already noticed that type `int` from C is
represented by `CInt` in Haskell and `double` by `CDouble`. You can convert
between `Int` and `CInt` with `fromIntegral` and between `Double` and `CDouble`
with `realToFrac`. Pointers from C became `Ptr`, so `double*` from C is
represented as `Ptr Double` in Haskell binding. What might be surprising about
this type signature is that the result is in the `IO` monad, that is our
function from C is denoted as impure. The reason for this is that every time we
run `c_dwt` function a different memory address will be allocated by `malloc`,
so indeed the function will return different results given the same input. In my
function however the array addressed by that pointer will always contain exactly
the same values (for the same input data), so in fact my function is pure. The
problem is that Haskell doesn't know that and we will have to fix that problem
using the infamous `unsafePerformIO`. For that we have to create a wrapper
function that has pure interface:

```haskell
import Control.Monad (liftM)
import Data.Vector.Storable
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe

dwt :: Vector Double -> Vector Double -> Vector Double
dwt ls sig = unsafePerformIO $ do
    let (fpLs , _, lenLs ) = unsafeToForeignPtr ls
        (fpSig, _, lenSig) = unsafeToForeignPtr sig
    pDwt <- liftM castPtr $ withForeignPtr fpLs $ \\ptrLs ->
            withForeignPtr fpSig $ \\ptrSig ->
                c_dwt (castPtr ptrLs ) (fromIntegral lenLs )
                      (castPtr ptrSig) (fromIntegral lenSig)
    fpDwt <- newForeignPtr finalizerFree pDwt
    return $ unsafeFromForeignPtr0 fpDwt lenSig
```

Our wrapper function takes two `Vector`s as input and returns a new `Vector`. To
interface with C we need to use
[storable](http://hackage.haskell.org/packages/archive/vector/0.10.0.1/doc/html/Data-Vector-Storable.html#t:Storable)
vectors, which store data that can be written to raw memory (that's what the C
function is doing). I wasn't able to figure out what is the difference between
storable and unboxed vectors. It seems that both store primitive values in
continuous memory block and therefore both offer similar performance (assumed,
not verified). First thing to do is to get `ForeignPtr`s out of input
vectors. `ForeignPtr` is a `Ptr` with a finalizer attached. Finalizer is a
function called when the object is no longer in use and needs to be garbage
collected. In this case we need a function that will free memory allocated with
`malloc`. This is a common task, so FFI implementation already provides a
`finalizerFree` function for that. The actual call to foreign function is made
on lines 11-14. We can operate on `Ptr` values stored in `ForeignPtr` using
`withForeignPtr` function. However, since we have vectors of `Double`s as input,
we also have `Ptr Double`, not `Ptr CDouble` that `c_dwt` function
expects. There are two possible solutions to that problem. One would be to copy
memory, converting every value in a vector using `realToFrac`. I did not try
that assuming this would kill performance. Instead I used `castPtr` which casts
pointer of one type to a pointer of another type. This is potentially dangerous
and relies on the fact that `Double` and `CDouble` have the same internal
structure. This is in fact expected, but by no means it is guaranteed by any
specification! I wouldn't be surprised it that didn't work on some sort of
exotic hardware architecture. Anyway, I written tests to make sure that this
cast does work the way I want it to. This little trick allows to avoid copying
the input data. The output pointer has to be cast from `Ptr CDouble` to `Ptr
Double` and since the result is in the `IO` monad the `castPtr` has to be lifted
with `liftM`. After getting the result as `Ptr Double` we wrap it in a
`ForeignPtr` with a memory-freeing finalizer (line 15) and use that foreign
pointer to construct the resulting vector of `Double`s.

Summary
=======

I had two concerns when writing this binding. First was the possible performance
overhead. Thanks to using pointer casts it was possible to avoid any sort of
data copying and that makes this binding real quick. Measuring execution time
with criterion shows that calling C function that does only memory allocation
(as shown in this post) takes about 250?s. After adding the rest of C code that
actually does computation the execution time jumps to about 55ms, so the FFI
calling overhead does not skew the performance tests. Big thanks go to Mikhail
Glushenkov who convinced me with [his answer on
StackOverflow](http://stackoverflow.com/questions/13009728/how-to-reliably-compare-runtime-of-haskell-and-c)
to use FFI. My second concern was the necessity to use many functions with the
word "unsafe", especially the `unsafePerformIO`. I googled a bit and it seems
that this is a normal thing when working with FFI and I guess there is no reason
to worry, provided that the binding is thoroughly tested. So in the end I am
very happy with the result. It is fast, Haskell manages garbage collection of
memory allocated with C and most importantly I can benchmark C code using
Criterion.

