---
title: Architecture of Glasgow Haskell Compiler
date: 2012-05-22
---

Architecture of Glasgow Haskell Compiler
========================================

In one of the previous posts I mentioned that the second volume of [The
Architecture of Open Source Applications](http://www.aosabook.org/) contains
[chapter about Glasgow Haskell Compiler](http://www.aosabook.org/en/ghc.html) by
Simon Marlow and Simon Peyton Jones (( There's also [PDF draft
version](http://community.haskell.org/~simonmar/papers/aos.pdf) of this paper. I
didn't notice any differences between the draft and the final version )). I read
this chapter yesterday and I must say it was insightful and interesting. It
gives a general view of GHC architecture and it does so very effectively. Things
are explained starting from high-level structure and then going into details
about some selected parts of the compiler. There's a nice overview of the
compilation pipeline. This pipeline contains many steps that you won't find in
most books about compilers. Since I'm taking the Compilers course at Stanford it
is nice to see how theory differs form practice :) What I consider as the most
interesting in this paper is the discussion of the design choices made by the
GHC creators and how these choices affect what GHC and Haskell are
today. Authors also share some of their development practices, which I find very
valuable, mainly because GHC is a project developed mostly by 2-3 people for
over 20 years and it managed to be successful and extendible. This means that
authors got some real experience about what works in real project and what
doesn't - I'm willing to follow their advices.

The paper is 29 pages long. It reads fairly quickly. It took me about 3,5 hours
to get through, mostly because I was googling around trying to find out more
about things that were new to me or things I didn't understand. This means that
while reading the paper I was also trying to figure out more about
[Core](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType)
and find out what the hell is
[STG](http://research.microsoft.com/pubs/67083/spineless-tagless-gmachine.ps.gz)
and [Cmm](http://www.cminusminus.org/). I'd like to get deeper into the
architecture of GHC and I think this general overview is a nice introduction. In
fact all the information in the paper - and much much more - can be found in the
Commentary section of [GHC
wiki](http://hackage.haskell.org/trac/ghc/wiki/Commentary).

