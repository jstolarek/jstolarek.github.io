---
title: The Little Schemer.  Book review
date: 2013-01-08
---

The Little Schemer.  Book review
================================

Exactly one year ago I was in the middle of reading ["Seven languages in seven
weeks"](2012-04-04-7-languages-in-7-weeks-book-review/) by Bruce Tate. This book
introduced me to functional programming with languages like Erlang, Scala,
Haskell and Lisp. Now, the Lisp part is the interesting one. I was googling some
things about Lisp and somehow I landed on Paul Graham's page. [His essays on
Lisp](http://www.paulgraham.com/lisp.html) fascinated me so much that I decided
to learn me some Lisp for great good. So when I reached the chapter about
Clojure (Lisp dialect for JVM) I actually decided to skip it and learn more on
my own. After some googling I settled on Scheme (another alternative was Common
Lisp). I began with reading the official
[R5RS](http://www.schemers.org/Documents/Standards/R5RS/) specification (only
about 50 pages) but that definitely wasn't enough. On various blogs and sites on
the Internet I found people talking about this book The Little Schemer by Daniel
P. Friedman and Matthias Felleisen, how great and different from other books it
is and how it can change the way you think. I decided to give The Little Schemer
a try.

[![The_Little_Schemer](images/The_Little_Schemer-243x300.jpg)](images/The_Little_Schemer.jpg)

The first thing I noticed after opening the book is that it is indeed unlike
anything I have read so far. Instead of explaining things directly, the book
presents a dialogue between the authors and a reader. There are no paragraphs,
just two columns of text - left with authors' questions, the second with
reader's responses. This dialogue is very informal and usually ends up with
talking about food. Sounds strange? Well, it was at first but as I went on I
found this style very fun. Of course reading is not enough - you have to
actually work through the code on your own and I admit that I found that part
very entertaining and rewarding, especially when I was able to figure out
solutions before reading them.

The book consist of ten chapters. Eight of them are dedicated to general lisp
programming. Book starts with simple concepts like basic operations on lists and
moves on to more and more advanced aspects of recursion. I feel that it has
learned me how to think in a recursive way, which in turn made learning Haskell
much easier.  Last two chapters of the book are quite difficult though. Chapter
9 gives a step by step derivation of the
[Y-combinator](http://en.wikipedia.org/wiki/Y-combinator). Well, almost step by
step - I had a feeling that some parts were actually left out and I had problems
understanding what is going on. Luckily [Peteris Krumins has written a great
post in which he derives
Y-combinator](http://www.catonmat.net/blog/derivation-of-ycombinator/) and fills
the gaps present in The Little Schemer. Even after reading that post I still
needed about 3 days to understand what's going on in the Y-combinator, but once
things clicked it was my "Aha!" moment. That's probably the most fascinating
thing I've learned so far in computer science. Chapter 10 pursues the goal of
writing Lisp interpreter in Lisp. The beauty of Lisp lies in the fact that it is
actually a relatively easy task requiring surprisingly little lines of code.
Check out [this article by Paul
Graham](http://www.paulgraham.com/rootsoflisp.html) to see for yourself.
Solution from The Little Schemer is actually a bit more complex than the one
given by Graham and again it took me some considerable effort to work my way
through this chapter.

At the end of The Little Schemer there are some references to books that in the
authors' opinion are worth reading. This is not only computer science stuff -
among references is _The Annotated Alice: Alice's Adventures in Wonderland and
Through the Looking Glass_ by Lewis Carroll - and I found one of the listed
books to be very fascinating: _To Mock a Mockingbird_ by Raymond Smullyan. It's
an introduction to combinatory logic given in a form of riddles. I'm somewhere
in the middle and will hopefully finish reading it some day.

I guess the opinions on the internet were right - The Little Schemer is a great
book and it really teaches how to think in a recursive way. I suggest reading it
to anyone who is interested in being a better programmer, functional or not.
This will be a good investment, especially that reading the book doesn't take
much time (authors warn the reader not to read it in less than three sittings).

Finally, there's a sequel to The Little Schemer called The Seasoned Schemer.
Unfortunately I was unable to find it so if any of you has a spare copy and
would be willing to send it to Poland just let me know :-)

