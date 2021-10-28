---
title: Verifying weight biased leftist heaps using dependent types (a draft)
date: 2014-01-16
---

Verifying weight biased leftist heaps using dependent types (a draft)
=====================================================================

I recently wrote a paper for the [26th International Conference on Computer
Aided Verification (CAV?14)](http://cav2014.iaik.tugraz.at/), Vienna, Austria.
The paper is titled "Verifying weight biased leftist heaps using dependent
types".  In this paper I have taken a weight biased leftist heap data structure
presented by Okasaki in "Purely Functional Data Structures" and created its Agda
implementation, which uses dependent types to statically guarantee that rank and
priority invariants are maintained. My verification is based on techniques
demonstrated by Altenkirch, McKinna and McBride in "Why Dependent Types Matter"
but my focus is on things that were left out in that paper. In the end my paper
is an intermediate level tutorial on verification in Agda but also a case study
of a purely functional data structure implemented in a dependently typed
setting. Worth noting is the fact that despite title of Okasaki's book his
implementation is not purely functional as it uses exceptions to deal with edge
cases like taking the smallest element from an empty heap.

Draft version of the paper can be downloaded
[here](http://ics.p.lodz.pl/~stolarek/posts/wp-content/uploads/2014/01/dep-typed-wbl-heaps.pdf).
It comes with companion source code that contains a thorough discussion of
concepts presented in the paper as well as others that didn't make it into
publication due to space limitations. Companion code is available at
[GitHub](https://github.com/jstolarek/dep-typed-wbl-heaps) (tag
"blog-post-draft-release" points to today's version). The paper is mostly
finished. It should only receive small corrections and spelling fixes. However,
if you have any suggestions or comments please share them with me - submission
deadline is in three weeks so there is still time to include them.

