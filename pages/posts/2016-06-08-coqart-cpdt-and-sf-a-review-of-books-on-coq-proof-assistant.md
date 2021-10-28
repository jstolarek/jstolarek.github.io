---
title: Coq'Art, CPDT and SF&#58; a review of books on Coq proof assistant
date: 2016-06-08
---

Coq'Art, CPDT and SF: a review of books on Coq proof assistant
==============================================================

I have been pretty quiet on the blog in the past couple of months. One of the
reasons for this is that I have spent most of my time learning Coq. I had my
first contact with Coq well over a year ago [when I started reading
CPDT](/posts/2015-03-24-first-impressions-of-coq-and-certified-programming-with-dependent-types.html).
Back then I only wanted to learn the basics of Coq to see how it works and what
it has to offer compared to other languages with dependent types. This time I
wanted to apply Coq to some ideas I had at work, so I was determined to be much
more thorough in my learning. Coq is far from being a mainstream language but
nevertheless it has some really good learning resources. Today I would like to
present a brief overview of what I believe are the three most important books on
Coq: _"Interactive Theorem Proving and Program Development. Coq'Art: The
Calculus of Inductive Constructions"_ (which I will briefly refer to as Coq'Art)
by Yves Bertot and Pierre Castéran, _"Certified Programming with Dependent
Types"_ (CPDT) by Adam Chlipala and _"Software Foundations"_ (SF for short) by
Benjamin Pierce and over a dozen over contributors. All three books
significantly differ in their scope and focus. CPDT and Coq'Art are standard,
printed books. CPDT is also [available online for
free](http://adam.chlipala.net/cpdt/). Software Foundations is only available
[as an online
book](http://www.cis.upenn.edu/~bcpierce/sf/current/toc.html). Interestingly,
[there is also a version of SF that seems to be in the process of being
revised](http://www.cis.upenn.edu/~bcpierce/sf/sf-4.0/).

I believe Coq'Art was the first book published on Coq. There are two editions -
2004 hardcover version and a 2010 paperback version - but as far as I know there
are no differences between them. Too bad the 2010 edition was not updated for
the newest versions of Coq - some of the code examples don't work in the newest
compiler. Coq'Art takes a theoretical approach, ie. it teaches Coq largely by
explaining how the rules of Calculus of Constructions work. There are also
practical elements like case studies and exercises but they do not dominate the
book. Personally I found Coq'Art to be a very hard read. Not because it dives
too much in theory - it doesn't - but because the presentation seems to be
chaotic. For example, description of a single tactic can be spread throughout
deveral places in the book. In principle, I don't object to extending earlier
presentation with new details once the reader gets a hold of some new concepts,
but I feel that Coq'Art definitely goes too far. Coq'Art also presents material
in a very unusual order. Almost every introduction to Coq or any other
functional language begins with defining data types. Coq'Art introduces them in
chapter 6. On the other hand sorts and universes - something I would consider an
advanced concept for anyone who is not familiar with type-level programming -
are presented in the second chapter. (Note that first chapter is a very brief
overview of the language.) By contrast, CPDT goes into detailed discussion of
universes in chapter 12 and SF does not seem to cover them at all. Overall,
Coq'Art is of limited usefulness to me. To tell the truth this is not because of
its focus on theory rather than practice, but because of language style, which I
find rather inaccessible. Many times I had problems understanding passages I was
reading, forcing me to re-read them again and again, trying to figure out what
is the message that the authors are trying to convey. I did not have such
problems with CPDT, SF, nor any other book I have read in the past few years. At
the moment I have given up on the idea of reading the book from cover to
cover. Nevertheless I find Coq'Art a good supplementary reading for SF. Most
importantly because of the sections that explain in detail the inner workings of
various tactics.

As mentioned at the beginning, I already wrote a [first impressions post about
CPDT](/posts/2015-03-24-first-impressions-of-coq-and-certified-programming-with-dependent-types.html).
Back then I said the book "is a great demonstration of what can be done in Coq
but not a good explanation of how it can be done". Having read all of it I
sustain my claim. CPDT does not provide a thorough and systematic coverage of
basics, but instead focuses on advanced topics. As such, it is not the best
place to start for beginners but it is a priceless resource for Coq
practitioners. The main focus of the book is proof automation with Ltac, Coq's
language for writing automated proof procedures. Reader is exposed to Ltac early
on in the book, but detailed treatment of Ltac is delayed until chapter
14. Quite surprisingly, given that it is hard to understand earlier chapters
without knowing Ltac. Luckily, the chapters are fairly independent of each other
and can be read in any order the reader wishes. Definitely it is worth to dive
into chapter 14 and fragments of apter 13 as early as possible - it makes
understanding the book a whole lot easier. So far I have already read chapter 14
three times. As I learn Coq more and more I discover new bits of knowledge with
each read. In fact, I expect to be going back regularly to CPDT.

Coq'Art and CPDT approach teaching Coq in totally different ways. It might then
be surprising that Software Foundations uses yet another approach. Unlike
Coq'Art it is focused on practice and unlike CPDT it places a very strong
emphasis on learning the basics. I feel that SF makes Coq learning curve as flat
as possible. The main focus of SF is applying Coq to formalizing programming
languages semantics, especially their type systems. This should not come as a
big surprise given that Benjamin Pierce, the author of SF, authored also "_Types
and Programming Languages"_ (TAPL), the best book on the topic of type systems
and programming language semantics I have seen. It should not also be surprising
that a huge chunk of material overlaps between TAPL and SF. I find this to be
amongst the best things about SF. All the proofs that I read in TAPL make a lot
more sense to me when I can convert them to a piece of code. This gives me a
much deeper insight into the meaning of lemmas and theorems. Also, when I get
stuck on an exercise I can take a look at TAPL to see what is the general idea
behind the proof I am implementing.

SF is packed with material and thus it is a very long read. Three months after
beginning the book and spending with it about two days a week I am halfway
through. The main strength of SF is a plethora of exercises. (Coq'Art has some
exercises, but not too many. CPDT has none). They can take a lot of time - and I
_really_ mean a lot - but I think this is the only way to learn a programming
language. Besides, the exercises are very rewarding. One downside of the
exercises is that the book provides no solutions, which is bad for
self-studying. Moreover, the authors ask people not to publish the solutions on
the internet, since "having solutions easily available makes \[SF\] much less
useful for courses, which typically have graded homework assignments". That
being said, there are plenty of github repositories that contain the solved
exercises (I also pledge guilty!). Although it goes against the authors' will I
consider it a really good thing for self-study: many times I have been stuck on
exercises and was able to make progress only by peeking at someone else's
solution. This doesn't mean I copied the solutions. I just used them to overcome
difficulties and in some cases ended up with proofs more elegant than the ones I
have found. As a side note I'll add that I do not share the belief that
publishing solutions on the web makes SF less useful for courses. Students who
want to cheat will get the solutions from other students anyway. At least that
has been my experience as an academic teacher.

To sum up, each of the books presents a different approach. Coq'Art focuses on
learning Coq by understanding its theoretical foundations. SF focuses on
learning Coq through practice. CPDT focuses on advanced techniques for proof
automation. Personally, I feel I've learned the most from SF, with CPDT closely
on the second place. YMMV

