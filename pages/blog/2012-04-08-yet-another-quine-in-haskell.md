---
title: Yet Another Quine In Haskell
date: 2012-04-08
---

Yet Another Quine In Haskell
============================

A few years ago I was asked by a student if I heard about a task to write a
program that outputs its own source code. Back then I didn't know about it and
sort of ignored that question, but later a friend of mine told me that such a
program is called a [quine](http://en.wikipedia.org/wiki/Quine_(computing)) and
that he had already solved that in couple of languages. Today I used my
beginner's knowledge of Haskell to write my first quine. If you haven't written
any quine by yourself yet then it may be better that you don't read this post -
knowing the solution will spoil the fun.

I started of with the general idea of defining a string that will contain the
text of the program and displaying that string. This is a strange kind of
recursion that I haven't met before. The idea itself seems rather simple, but it
took me about 40 minutes of hacking to get it right. I spent much of that time
struggling to get the quote signs display correctly. Anyway, here it is:

```haskell
module Main where --"
main = do putStr (quine ++ "\\nquine = \\"" ++ take 20 quine ++ "\\\\")
          print (drop 21 quine)
quine = "module Main where --\\"\\nmain = do putStr (quine ++ \\"\\\\nquine = \\\\\\"\\" ++ take 20 quine ++ \\"\\\\\\\\\\")\\n          print (drop 21 quine)"
```

This requires that there is a new line at the and of source file. It's rather
crappy but I'm still happy with it. I googled around for some other quines in
Haskell and, as expected, found [more elegant
solutions](http://switchb.org/kpreid/quines):

```haskell
(\\a -> a ++ show a) "(\\\\a -> a ++ show a) "
```

As [Shin-Cheng Mu notices](http://www.iis.sinica.edu.tw/~scm/2007/a-haskell-quine/)
this is based on the resemblance to lambda expression `(\x -> x x) (\x -> x x)`,
which reduces to itself. It also resembles the Y-combinator (on which I hope to
write soon). There is also a shorter version of the above.

```haskell
ap (++) show "ap (++) show "
```

It uses the
[ap](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Monad.html#v:ap)
function operating on a monad and frankly speaking I don't understand it. The
two above solutions are not stand-alone programs and work only in ghci, as
opposed to my solution. It is not difficult however to adapt the idea to a
stand-alone application:

```haskell
main = putStr (quine q)
quine s = s ++ show s
q = "main = putStr (quine q)\\nquine s = s ++ show s\\nq = "
```

The solution is by Jon Fairbairn and I found it
[here](http://www.nyx.net/~gthompso/quine.htm).  The same using a monad (found
[here](http://porg.es/blog/a-quine-in-haskell)):

```haskell
main = (putStr . ap (++) show) "main = (putStr . ap (++) show) "
```

Only this will not compile since ap is not in scope and would have to be
imported.  I don't know if this should count as a valid solution.

To me the conclusion is that I'm still thinking in an imperative style. Quine
solutions seem to be a lot easier in languages supporting higher order
functions, yet I solved it in a way typical to languages like Java or C.

