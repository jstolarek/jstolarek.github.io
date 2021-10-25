---
title: Haskell as fast as C&#58; A case study
date: 2013-04-02
---

Haskell as fast as C: A case study
==================================

Once in a while someone, most likely new to Haskell, asks how does Haskell
performance compare with C. In fact, when I was beginning with Haskell, I asked
exactly the same question. During last couple of days I've been playing a bit
with squeezing out some performance from a very simple piece of Haskell
code. Turned out that the results I got are comparable with C so I thought I
might share this. This will be a short case study, so I don't intend to cover
the whole subject of Haskell vs. C performance. There was a lot written on this
already so I encourage to search through the Haskell-cafe archives as well as
some other blogs. Most of all I suggest reading
[this](http://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/)
and
[this](http://donsbot.wordpress.com/2008/06/04/haskell-as-fast-as-c-working-at-a-high-altitude-for-low-level-performance/)
post on Don Stewart's blog.

Here is my simple piece of code:

```haskell
sumSqrL :: [Int] -> Int
sumSqrL = sum . map (^2) . filter odd
```

It takes a list of `Int`s, removes all even numbers from it, squares the
remaining odd numbers and computes the sum. This is idiomatic Haskell code: it
uses built-in list processing functions from the standard Prelude and relies on
function composition to get code that is both readable and modular. So how can
we make that faster? The simplest thing to do is to switch to a more efficient
data structure, namely an unboxed `Vector`:

```haskell
import Data.Vector.Unboxed as U

sumSqrV :: U.Vector Int -> Int
sumSqrV = U.sum . U.map (^2) . U.filter odd
```

The code practically does not change, except for the type signature and
namespace prefix to avoid clashing with the names from Prelude. As you will see
in a moment this code is approximately three times faster than the one working
on lists.

Can we do better than that? Yes, we can. The code below is three times faster
than the one using `Vector`, but there is a price to pay. We need to sacrifice
modularity and elegance of the code:

```haskell
sumSqrPOp :: U.Vector Int -> Int
sumSqrPOp vec = runST $ do
  let add a x = do
        let !(I# v#) = x
            odd#     = v# `andI#` 1#
        return $ a + I# (odd# *# v# *# v#)
  foldM' add 0 vec
```

This code works on an unboxed vector. The `add` function, used to fold the
vector, takes an accumulator `a` (initiated to `0` in the call to `foldM'`) and
an element of the vector. To check parity of the element the function unboxes it
and zeros all its bits except the least significant one. If the vector element
is even then `odd#` will contain `0`, if the element is odd then `odd#` will
contain `1`. By multiplying square of the vector element by `odd#` we avoid a
conditional branch instruction at the expense of possibly performing unnecessary
multiplication and addition for even elements.

Let's see how these functions compile into Core intermediate language. The
`sumSqrV` looks like this:

```
$wa =
  \vec >
    case vec of _ { Vector vecAddressBase vecLength vecData ->
    letrec {
      workerLoop =
        \index acc ->
          case >=# index vecLength of _ {
            False ->
              case indexIntArray# vecData (+# vecAddressBase index)
              of element { __DEFAULT ->
              case remInt# element 2 of _ {
                __DEFAULT ->
                  workerLoop (+# index 1) (+# acc (*# element element));
                0 -> workerLoop (+# index 1) acc
              }
              };
            True -> acc
          }; } in
    workerLoop 0 0
    }
```

while `sumSqrPOp` compiles to:

```
$wsumSqrPrimOp =
  \ vec ->
    runSTRep
      ( (\ @ s_X1rU ->
          case vec of _ { Vector vecAddressBase vecLength vecData ->
          (\ w1_s37C ->
             letrec {
               workerLoop =
                 \ state index acc ->
                   case >=# index vecLength of _ {
                     False ->
                       case indexIntArray# vecData (+# vecAddressBase index)
                       of element { __DEFAULT ->
                       workerLoop
                         state
                         (+# index 1)
                         (+# acc (*# (*# (andI# element 1) element) element))
                       };
                     True -> (# state, I# acc #)
                   }; } in
             workerLoop w1_s37C 0 0)
          })
       )
```

I cleaned up the code a bit to make it easier to read. In the second version
there is some noise from the ST monad, but aside from that both pieces of code
are very similar. They differ in how the worker loop is called inside the most
nested case expression. First version does a conditional call of one of the two
possible calls to `workerLoop`, whereas the second version does an unconditional
call. This may seem not much, but it turns out that this makes the difference
between the code that is comparable in performance with C and code that is three
times slower.

Let's take a look at the assembly generated by the LLVM backend. The main loop
of `sumSqrV` compiles to:

```
LBB1_4:
    imulq    %rdx, %rdx
    addq     %rdx, %rbx
.LBB1_1:
    leaq    (%r8,%rsi), %rdx
    leaq    (%rcx,%rdx,8), %rdi
    .align  16, 0x90
.LBB1_2:
    cmpq    %rax, %rsi
    jge     .LBB1_5
    incq    %rsi
    movq    (%rdi), %rdx
    addq    $8, %rdi
    testb   $1, %dl
    je      .LBB1_2
    jmp     .LBB1_4
```

While the main loop of `sumSqrPOp` compiles to:

```
.LBB0_4:
    movq    (%rsi), %rbx
    movq    %rbx, %rax
    imulq   %rax, %rax
    andq    $1, %rbx
    negq    %rbx
    andq    %rax, %rbx
    addq    %rbx, %rcx
    addq    $8, %rsi
    decq    %rdi
    jne     .LBB0_4
```

No need to be an assembly expert to see that the second version is much more
dense.

I promised you comparison with C. Here's the code:

```c
long int c_sumSqrC( long int* xs, long int xn ) {
  long int index   = 0;
  long int result  = 0;
  long int element = 0;
 Loop:
  if (index == xn) goto Return;
  element = xs[index];
  index++;
  if ((0x1L & element) == 0) goto Loop;
  result += element * element;
  goto Loop;
 Return:
  return result;
}
```

You're probably wondering why the hell did I use `goto`s. The reason is that the
whole idea of this sum-square-of-odds function was taken from the paper
["Automatic transformation of series expressions into
loops"](http://dl.acm.org/citation.cfm?id=102806) by Richard Waters and I
intended to closely mimic the solution produced by his fusion framework.

I used criterion to compare the performance of four presented implementations:
based on list, base on vector, based on vector using `foldM`+primops and C. I
used FFI to call C implementation from Haskell so that I can benchmark it with
criterion as well. Here are the results for a list/vector containing one million
elements:

[![Performance of sumSqr](images/sumsqrperf.png)](images/sumsqrperf.png)

C version is still faster than the one based on primops by about 8%. I think
this is a very good achievement given that the version based on Vector library
is three times slower.

A few words of summary
======================

The [vector](http://hackage.haskell.org/package/vector) library uses stream
fusion under the hood to optimize the code working on vectors. In the blog posts
I mentioned in the beginning Don Stewart talks a bit about stream fusion, but if
you want to learn more you'll probably be interested in two papers: [Stream
Fusion. From Lists to Streams to Nothing at
All](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.104.7401) and
[Haskell Beats C Using Generalized Stream
Fusion](http://www.eecs.harvard.edu/~mainland/publications/mainland12simd.pdf).
My `sumSqrPOp` function, although as fast as C, is in fact pretty ugly and I
wouldn't recommend anyone to write Haskell code in such a way. You might have
realized that while efficiency of `sumSqrPOp` comes from avoiding the
conditional instruction within the loop, the C version does in fact use the
conditional instruction within the loop to determine the parity of the vector
element. The interesting thing is that this conditional is eliminated by `gcc`
during the compilation.

As you can see it might be possible to write Haskell code that is as fast as
C. The bad thing is that to get efficient code you might be forced to sacrifice
the elegance and abstraction of functional programming. I hope that one day
Haskell will have a fusion framework capable of doing more optimisations than
the frameworks existing today and that we will be able to have both the elegance
of code and high performance. After all, if `gcc` is able to get rid of
unnecessary conditional instructions then it should be possible to make GHC do
the same.

A short appendix
================

To dump Core produced by GHC use `-ddump-simpl` flag during compilation. I also
recommend using `-dsuppress-all` flag, which suppresses all information about
types - this makes the Core much easier to read.

To dump the assembly produced by GHC use `-ddump-asm` flag. When compiling with
LLVM backend you need to use `-keep-s-files` flag instead.

To disassemble compiled object files (e.g. compiled C files) use the `objdump
-d` command.

Update - discussion on Reddit
=============================

There was some discussion about this post on
[reddit](http://www.reddit.com/r/haskell/comments/1bikvs/yet_another_lambda_blog_haskell_as_fast_as_c_a/)
and I'd like to address some of the objections that were raised there and in the
comments below.

Mikhail Glushenkov pointed out that the following Haskell code produces the same
result as my `sumSqrPOp` function:

```haskell
sumSqrB :: U.Vector Int -> Int
sumSqrB = U.sum . U.map (\x -> (x .&. 1) * x * x)
```

I admit I didn't notice this simple solution and could have come with a better
example were such a solution would not be possible.

There was a request to compare performance with idiomatic C code, because the C
code I have shown clearly is not idiomatic. So here's the most idiomatic C code
I can come up with (not necessarily the fastest one):

```
long int c_sumSqrC( long int* xs, long int xn ) {
  long int result = 0;
  long int i = 0;
  long int e;
  for ( ; i < xn; i++ ) {
    e = xs[ i ];
    if ( e % 2 != 0 ) {
      result += e * e;
    }
  }
  return result;
}
```

The performance turns out to be the same as before ("Bits" represents Mikhail
Glushenkov's solution, "C" now represents the new C code):

[![sumsqrperf](images/sumsqrperf1.png)](images/sumsqrperf1.png)

There was a suggestion to use the following C code:

```c
for(int i = 0; i < xn; i++) {
    result += xs[i] * xs[i] * (xs[i] & 1);
}
```

Author claims that this code is faster than the version I proposed, but I cannot
confirm that on my machine - I get results that are noticeably slower (2.7ms vs
1.7ms for vectors of 1 million elements). Perhaps this comes from me using GCC
4.5, while the latest available version is 4.8.

Finally, there were questions about overhead added by calling C code via FFI. I
was concerned with this also when I first wanted to benchmark my C code via FFI.
After making some experiments it turned out that this overhead is so small that
it can be ignored. For more information see [this
post](2012-11-02-benchmarking-c-functions-using-foreign-function-interface/).

