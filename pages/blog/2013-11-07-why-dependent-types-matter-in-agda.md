---
title: Why dependent types matter (in Agda)
date: 2013-11-07
---

Why dependent types matter (in Agda)
====================================

During my MSR internship I had a chance to learn from people working on type
systems. I already had interest in types before going to Cambridge, but somehow
I didn't put my full effort into that topic. I had only made some attempts at
learning dependent types but didn't get farther than standard vector examples.
The best thing that happened in Cambridge was Conor McBride's course on
[Dependently-typed metaprogramming (in
Agda)](http://www.cl.cam.ac.uk/~ok259/agda-course-13/) organized by [Ohad
Kammar](http://www.cl.cam.ac.uk/~ok259/) at the University's Computer
Laboratory. I learned about the course after it had started so I only had a
single day to learn basics of Agda before going to the second session of
lectures and labs. I admit that material covered by Conor was difficult for me
and I will probably need a few months to digest it.

Recently I decided to solidify my knowledge of basics of dependent types by
reading "Why Dependent Types Matter". This unpublished paper was written by
Thorsten Altenkirch, Conor McBride and James McKinna somewhere in 2006 I
believe. It gives a great overview of dependent types and various design
decisions related to their usage. But most of all this paper shows how to write
a provably correct merge-sort algorithm. Proving correctness of algorithms is
something I find very interesting, so this paper was a must-read for me.

There is only one catch with ["Why Dependent Types
Matter"](http://www.strictlypositive.org/ydtm.ps.gz). All the code is written in
Epigram, a dependently typed functional language designed by Conor McBride and
James McKinna. The problem is that Epigram's webpage has been offline for few
months now ((I recall Conor mentioning that Nottingham people, who were hosting
it Epigram's web page on their servers, sent him the hard drive with said web
page.)) and the language basically seems dead. Anyway, since my
dependently-typed language of choice is Agda (for the moment at least - I'm
thinking a lot about [Idris](http://www.idris-lang.org/) recently) I decided to
rewrite all the code in the paper to Agda. For the most part this was a
straightforward task, once I learned how to read Epigram's unusual syntax. There
were however a few bumps along the way. One problem I encountered early on was
Agda's termination checker complaining about some functions. Luckily, Agda
community is as helpful as Haskell's and within a day [I was given a detailed
explanation of what goes
wrong](https://lists.chalmers.se/pipermail/agda/2013/005948.html). A slightly
larger problem was that paper elides details of some proofs. If I wanted to have
working Agda code I had to fill in these details. Since I didn't know how to do
that I had to pause for one day and go through [online materials for Thorsten
Altenkirch's course on Computer Aided Formal
Reasoning](http://www.cs.nott.ac.uk/~txa/g53cfr/). In the end I managed to fill
in all the missing gaps. My code is [available on
github](https://github.com/jstolarek/why-dependent-types-matter). Now I feel
ready to prove correctness of a few more algorithms on my own.

Post scriptum
=============

Conor will be giving his course on "Dependently typed metaprogramming" in
November and December at University of Edinburgh. See
[here](http://homepages.inf.ed.ac.uk/s1225336/agda-course-13/) for details. Be
sure not to miss it if you have a chance to attend. Code repository for the
course is available [here](https://github.com/pigworker/MetaprogAgda).

Unofficial mirror of Epigram's sources is [available on
github](https://github.com/brixen/Epigram).

