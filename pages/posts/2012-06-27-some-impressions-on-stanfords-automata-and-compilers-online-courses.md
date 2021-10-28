---
title: Some impressions on Stanford's Automata and Compilers online courses
date: 2012-06-27
---

Some impressions on Stanford's Automata and Compilers online courses
====================================================================

Over two months ago Stanford opened first edition of online courses on Automata
and Compilers (see [this
post](/blog/2012-04-20-stanford-opens-new-online-courses-about-compilers-and-automata.html)).
I've participated in both. These courses cover basics of each discipline and
assume no previous knowledge of the subject. Now the courses are over and it's
time to share some impressions.

Automata course was six weeks long. It consisted of lectures and so-called
homeworks, which are quizzes that should be taken every week. There were some
optional programming assignments, but they didn't affect the final score. The
topics covered were: finite automata (deterministic, non-deterministic), regular
expressions, context-free grammars and languages, Turing machines, decidability
and finally P and NP problems. The course closely follows the book "Introduction
to Automata Theory, Languages, and Computation" by John Hopcroft, Rajeev Motwani
and Jeffrey Ullman. This shouldn't come as big surprise, since the course is
lectured by Jeffrey Ullman, one of the authors. For the first three weeks this
course very closely followed Compilers, as both discussed automata, regular
languages and context-free grammars. I initially enjoyed Automata course a lot,
despite lectures being a bit boring. I just read the book and then skimmed
through the lecture slides. Sadly the last two weeks of the course were
extremely hard to follow. Lectures were rushed and impossible for me to
comprehend. Reading the book didn't help a lot, so I didn't understand much of
decidability, P and NP problems. That said, I'm still very happy with this
course, since automata and context-free languages were top priority for me. Keep
also in mind that this is very subjective opinion of mine. Reading posts in the
course forums I saw that there were really mixed opinions. Some people
complained about the lectures being hard to follow, but others praised them for
being interesting.

Compilers course took 10 weeks and was much more demanding. It covered theory of
lexical analysis using automata, top-down parsing using recursive descent
algorithm, bottom-up parsing using LL(1), SLR, and LR algorithms, semantic
analysis, operational semantics, code generation, optimizations and garbage
collection. Believe me, that's a lot of theory! In contrast to Automata course,
it was all presented in very accessible and interesting way. Each week contained
up to 3 hours of lectures and a theoretical quiz covering presented
material. There were also four programming assignments which required to
implement four different parts of the compiler: lexical analyser (a.k.a. lexer),
parser, semantic analyser and code generator. First two exercises relied on
existing tools for generating lexers and parsers. The language implemented was
COOL - [Classroom Object-Oriented
Language](http://en.wikipedia.org/wiki/Cool_%28programming_language%29). I
consider programming assignments to be demanding, especially the last two. Third
took me about 25 hours to complete. I didn't manage to finish the fourth one,
but I'm still satisfied, since I'm interested mostly in the front-end parts of
the compiler (type checking especially). The best thing about Compilers course
is that it was motivating. It encouraged me to finally read some chapters from
the Dragon Book and "Engineering a Compiler". Which reminds me that [I promised
to write more about compiler
books](/blog/2012-04-25-some-books-about-compilers.html).

The Dragon Book turned out to be surprisingly interesting. It is verbose and
very dry, so it's not suitable for reading from cover to cover. Nevertheless, if
you have a general idea of what is going on in the compiler and need details on
some particular algorithm this is an invaluable resource. In short: bad for
introductory text, perfect for reference.

"Engineering a Compiler, 2nd edition" is different. It is very accessible
comparing to the Dragon Book. In every chapter the reader is given a clear
outline of what will be done and rationale why some particular theory is
needed. This greatly helps to understand what's going on in each phase of
compilation. At the end of each chapter there's an overview of literature on a
given topic, so it's also a great starting point for deeper research into each
subject. Thus I think that book makes a very good introductory text. I
wholeheartedly recommend it.

I haven't read Appel's "Modern Compiler Implementation in C" so I really can't
tell much about it. Hopefully I will find some time during the summer holiday to
read the chapter about Hindley-Milner type inference algorithm.

Coursera online courses were an awesome experience. Challenging and time
consuming, but also very motivating and rewarding. I'm waiting for more courses
in the future, especially for the new edition of the Game Theory course.

