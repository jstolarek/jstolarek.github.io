---
title: Some books about compilers
date: 2012-04-25
---

Some books about compilers
==========================

[Online Compilers course at Stanford University](http://class.coursera.org/compilers/)
began last Monday. Although it should be self-containing I still prefer to have
some books at hand, especially that I'll be leaving for 10 days and I want to
self-study material for the next week.  On Monday I've paid a visit to my
university's library. Here's a quick glimpse at some literature that I've found.

First of all, there's the [Dragon Book](http://dragonbook.stanford.edu/),
officially known as "Compilers: Principles, Techniques, and Tools", by Aho, Lam,
Sethi, and Ullman. This is a classic originally published in 1986. There's
nothing to worry though - there was no revolution in the field of compiler
construction, so what was true in 1986 is mostly still valid now. The book was
re-released in 2006 with some updates about garbage collection and
code-optimization (OK, I lied, there was some progress in a few aspects of
compiler engineering). I have a Polish edition of the first edition. In fact I
borrowed it long before the course had started hoping to read it on my own. The
book is very detailed and extremely verbose (750 pages total) which is a bit
discouraging to me. I've read only a couple of sections so I don't have a clear
opinion about it yet.

The second book recommended as a textbook for the Compilers course is
"Engineering a compiler" by Cooper and Torczon. This one was more difficult to
find. I just got it today and haven't read it yet. With over 800 pages it's
longer than the Dragon Book. Skimming through the book I noticed that it seems
to be detailed and precise and puts emphasis on practical aspects. Chapter about
lexical analysis has a section about implementing a lexer. I'm looking forward
to this one, since I still don't have a clear vision on how to implement lexer
efficiently. These are only the first impressions of course. I'll verify them
when I start reading (expect a follow-up on this one [read a follow up on this
one](2012-06-27-some-impressions-on-stanfords-automata-and-compilers-online-courses/)).

Third of the recommended books - "Modern compiler implementation in C/Java/ML"
(there are three different editions of this book) by Appel - was also available
in my library. I've already read the chapter about lexical analysis and I think
that, contrary to the Dragon Book, it was a bit too concise. It covers the
subject of Deterministic Finite Automata (DFA) and Non-deterministic Finite
Automata (NFA), which luckily I am familiar with (more on that in a moment), so
that wasn't much of a problem. However if this was my first contact with DFAs
and NFAs I would have some trouble understanding what it's all about. I hope
that the next chapters will be more detailed. I was very happy to see that this
book has a chapters about functional languages and polymorphic types. The latter
one covers the subject of the Hindley-Milner type inference algorithm in a form
that seems to be accessible.

[![](images/img_1111-300x225.jpg "Compiler books")](images/img_1111.jpg)

Now, here's a surprise! Just a few days ago I found about a book [The
Implementation of Functional Programming
Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/index.htm)
by Simon Peyton Jones ([see my previous post](2012-04-22-sunday-web-overview/)).
To my greatest surprise it was in the library! I stumbled upon it by pure
accident while I was searching the shelves for Appel's book - I never would have
thought that it's available in the library of my university. Do I have to
mention that I borrowed it right away? I've read the Introduction and some
random parts of the first chapter about the lambda calculus. I must say that the
book looks very accessible so I hope to find some time to read at least couple
of chapters. What's interesting about the book (or rather about functional
languages) is the fact that Haskell didn't exist when this book was written, yet
much of the code in the book is almost Haskell (in fact it's
[Miranda](http://en.wikipedia.org/wiki/Miranda_%28programming_language%29)).

I've got one more book entitled "Compiler construction" by Waite and Goos. This
one is old (1984) and, aside from universal knowledge about parsing, semantic
analysis, error handling an so on, it seems to cover many aspects that are a bit
outdated. I think I'll just rely on previous books and skip reading this one.

I also enrolled to [Automata course](https://class.coursera.org/automata/).
Initially I didn't plan to do it, but I figured out that I can just give it a
try and, if I won't have enough time, I can simply download the videos and watch
them during summer holiday. Luckily the first week covers the DFAs and NFAs so I
got 100% from the first quiz without even watching the lectures. I'm afraid that
next weeks won't be that easy.

