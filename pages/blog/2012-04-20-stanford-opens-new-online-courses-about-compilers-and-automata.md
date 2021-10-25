---
title: Stanford opens new online courses about Compilers and Automata
date: 2012-04-20
---

Stanford opens new online courses about Compilers and Automata
==============================================================

A few months ago Stanford University started a great initiative: it offered free
online courses for everyone. Courses were mostly about computer science stuff
(Natural Language Processing, Machine Learning, Computer Vision etc.). If you
didn't take part in these courses, then you must know that they are just like
normal classes at the university. There are lectures (about 2 hours per week),
tests after each week, final exams and rather demanding programming assignments.
I enrolled to Natural Language Processing, but I have to admit that I couldn't
keep the pace and stopped following the course after about 3 weeks.  Perhaps I
wasn't motivated enough, since I NLP has no connection to my research and I
wouldn't be able to use the gained knowledge in practice. Yesterday however my
motivation jumped to maximum, because I got a newsletter about [a whole bunch of
new courses](https://www.coursera.org/courses). Among them are two courses,
which are of high interest two me:
[Compilers](https://www.coursera.org/course/compilers) and
[Automata](https://www.coursera.org/course/automata).

Recently I got very interested in the compiler construction subject. I got
myself the [Dragon Book](http://en.wikipedia.org/wiki/Compilers:_Principles,_Techniques,_and_Tools),
but it's really huge so I think I wouldn't read all of it. I also visited
#compilers IRC channel and got some very valuable hints on how to dig into the
subject of constructing a compiler. I was suggested to begin with reading some
sort of "Lisp interpreter in Lisp" tutorial. I chose Scheme as my Lisp dialect
and - knowing nothing about Scheme - I read the [R5RS language
specification](http://www.schemers.org/Documents/Standards/R5RS/) (only 50
pages!) and The Little Schemer (chapter 10 shows how to do a Scheme interpreter
in Scheme). I've also found [Write Yourself a Scheme in 48
Hours](http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html)
tutorial, which shows how to write Scheme interpreter in Haskell. This is a bit
more advanced and, despite what author claims, is not suitable for Haskell
newbies like myself. I decided to get back to that tutorial after learning some
more Haskell, especially monads and Parsec, both used heavily by the
author. Guys at #compilers further suggested that after reading this kind of
tutorial I should read Niklaus Wirth's ["Compiler
Construction"](http://www.ethoberon.ethz.ch/WirthPubl/CBEAll.pdf). Indeed, the
book looks very accessible. Another usefull advice I received was "the older the
better" since fundamentals of compilers haven't changed much and the only
substantial progress was achieved in the back-end (( back-end is the part of
compiler responsible for generating the final code (most often a machine code)
)) development. However, the most motivating was the estimation that writing a
simple compiler from scratch should take about 6 months. Nevertheless, learning
everything by myself from scratch was a very slow process and I wish I had
someone who would teach me. The course on Compilers from Stanford is therefore a
real blessing. Well, looks like my wish was granted.

The course about Automata is also something very exciting. I have a book
"Introduction to Automata Theory, Languages, and Computation" by John
E. Hopcroft, Rajeev Motwani and Jeffrey D. Ullman. I approached this book twice
but always got stuck around third chapter. Not that I couldn't understand it. It
was my motivation that failed. Anyway, I feel that as a computer scientist I
should have a basic knowledge of this subject. Believe it or not, but my studies
didn't cover the subject of automata and computations!

There's a lot of other courses, not only about computer science, so I suggest to
take a look. Perhaps you'll find something interesting for
yourself. Participating in NLP course had showed me that these courses are very
demanding and require a lot of time and effort. This means that I'm going to
pick only the Compilers course and concentrate solely on it, hoping that the
Automata course will be repeated in the future. Both courses start this Monday
(April 23rd).

