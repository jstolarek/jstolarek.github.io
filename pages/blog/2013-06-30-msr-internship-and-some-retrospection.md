---
title: MSR internship and some retrospection
date: 2013-06-30
---

MSR internship and some retrospection
=====================================

I feel I can finally write about: I got accepted for a three-month internship at
Microsoft Research Cambridge! This means I will be developing GHC and,
hopefully, doing some serious research on the subject of functional programming
and compiler implementation. My internship starts tomorrow, on 1st July. I'm not
yet 100% certain about the exact topic of my research, so I'll refrain from
going into any kind of technical details for now and I will focus on my personal
experience with functional programming. I feel this is really a good moment to
summarize the past 1,5 year. I learned about functional programming at the very
beginning of 2012 and since then I progressed from knowing completely nothing to
being in Cambridge - something I would have not imagined 18 months ago.

Somewhere around July 2011 I finished writing my PhD. I had yet to deal with
many formalities - which in the end took 8 months - but the most important part
of my work was done and I only continued research on a few minor subjects that I
ran into while writing a PhD. Somewhere in October I decided I need a break from
all my current research topic - I finally wanted some time to pursue topics that
interested me all along and for which I never had time. Compiler construction
and theory of automata were two main topics I had in mind. That was the plan,
but it wasn't meant to work out, at least not yet. Somewhere around December
2012 I stumbled upon a book ["Seven languages in seven
weeks"](/blog/2012-04-04-7-languages-in-7-weeks-book-review.html), which was my
first contact with functional programming. I didn't follow the book exactly.  I
read chapters about Ruby, Io, Prolog (so much fun!), Scala and Erlang, but
instead of reading chapter about Clojure I went for Scheme. I read
[R5RS](http://www.schemers.org/Documents/Standards/R5RS/) language specification
and [The Little Schemer](/blog/2013-01-08-the-little-schemer-book-review.html)
and when I reached the chapter about Haskell I decided to read [Learn You A
Haskell](http://learnyouahaskell.com/chapters) instead. At that point I already
knew that Haskell is _the_ functional programming language and I think that this
was the moment I started having some serious plans about functional
programming. But at the same time I was figuring out how to learn about
compilers. It was April when [Stanford University announced their two online
courses on Compilers and
Automata](/blog/2012-04-20-stanford-opens-new-online-courses-about-compilers-and-automata.html)
and these were really godsend. The Compilers course ended in late June. This
concludes my first six months of contact with FP and I think that these months
were extremely intense. I learned theoretical and practical foundations of
compilers, a new programming paradigm and some new languages designed in that
paradigm. I also started reading research papers on functional programming, with
a focus on implementation of GHC. At that point I didn't even try to work on the
source code, but I was trying to understand how the compiler is designed.

The next six months, from July to December, were not as fruitful. I picked up
interest in doing data-parallel computations in Haskell, as this seemed to be an
active topic of research and also related to my PhD work. I made a failed
attempt of an efficient parallel implementation of a wavelet transform. Although
I wasn't successful, my time was not wasted: I learned how to write, test and
benchmark libraries in Haskell and also read a lot of papers on FP. I also got
in touch with [Ben Lippmeier](http://www.cse.unsw.edu.au/~benl/), who pointed me
to one problem with GHC he needed fixed. This was somewhere in January 2013. I
already started reading the source code of GHC in December, but now I finally
had a particular problem to solve. It was the time to start working on GHC. That
is mostly what I did during the last six months, although I also managed to
spend some time on theory (more papers and [a book on combinatory
logic](/blog/2013-02-06-to-mock-a-mockingbird-or-how-i-learned-to-stop-worrying-and-learned-combinatory-logic.html)).

As for the internship, I decided to apply for it in February. I polished my CV
and cover letter (many thanks go to my friend [Marek](http://www.mareklab.org/)
for his help) and sent my application at the beginning of March. After an
interview with Geoffrey Mainland and Simon Peyton Jones I got acceptance
notification at the beginning of April. And here I am in Cambridge, over 1300km
from home, waiting for my first day at Microsoft Research.

