---
title: Coders at work.  A short review
date: 2013-01-13
---

Coders at work.  A short review
===============================

<div class="thumbnail">
<figure>
[![CodersAtWork](/images/posts/CodersAtWork-200x300.jpg)](/images/posts/CodersAtWork.jpg)
</figure>
</div>

I was visiting my friend before Christmas and on his shelf I found ["Coders at
Work"](http://www.codersatwork.com/) by Peter Seibel[^1].  It's a collection of
interviews with fifteen world-known programmers (assuming a programmer can be
world-known). Among the names two have caught my eye: [Simon Peyton
Jones](http://research.microsoft.com/en-us/people/simonpj/) and [Guy
Steele](http://labs.oracle.com/people/mybio.php?uid=25706). I borrowed the book
and ended up reading interviews with these two researchers and [Donald
Knuth](http://www-cs-faculty.stanford.edu/~uno/). Not surprisingly, interview
with Simon Peyton Jones was the one that interested me the most. I found it very
enjoyable to read accounts of how these people became programmers in times where
computers where huge machines available only at universities. On the other hand
I also found some parts of the interviews to be a bit dry, e.g. SPJ goes into
the discussion of [Software Transactional
Memory](http://book.realworldhaskell.org/read/software-transactional-memory.html)
in Haskell and I think that reader not familiar with STM will not get much out
of this. Also, reader familiar with STM will probably not need this discussion.

There are two thoughts that stroke me during the reading. When reading interview
with Knuth I realized that there are actually people that are unable to learn
programming. I teach at the university and I've seen many first year students
who completely don't grasp programming but somehow it never occurred to me that
some of them might not ever be able to learn this. Second thought, the more
important one, is that all these famous programmers were extremely lucky
guys. They all got the chance to develop their talents. I believe that there are
even more very talented people who could have had even greater impact on
computer science, but they just didn't get their chance.

One last thing. SPJ mentions that his "Aha!" moment in functional programming
was Arthur Norman's demonstration of implementing double-linked list in a purely
functional fashion, that is without any side effects. I tried to google more
information on that but with no result. So if anyone has an idea how to
implement such a list please tell me. The only thing that comes to my mind are
[zippers](http://learnyouahaskell.com/zippers). They do allow traversal of list
in both directions and modifying its elements so this might be it.

[^1]: In fact I found [Polish edition](http://helion.pl/ksiazki/sztuka-kodowania-sekrety-wielkich-programistow-peter-seibel,sztkod.htm).
