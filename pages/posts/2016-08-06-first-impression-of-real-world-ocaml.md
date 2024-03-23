---
title: First impression of "Real World OCaml"
date: 2016-08-06
---

First impression of "Real World OCaml"
======================================

Tomorrow I will be flying to Cambridge to attend [International Summer School on
Metaprogramming](http://www.cl.cam.ac.uk/events/metaprog2016/). One of the
prerequisites required from the participants is basic knowledge of OCaml,
roughly the first nine chapters of "Real World OCaml" (RWO for short). I
finished reading them several days ago and thought I will share my impressions
about the book.

<div class="thumbnail">
<figure>
[![rwo](/images/posts/rwo1-228x300.png)](/images/posts/rwo1.png)
</figure>
</div>

RWO was written by Yaron Minsky, [Anil Madhavapeddy](http://anil.recoil.org/)
and Jason Hickey. It is one of a handful of books on OCaml. Other titles out
there are ["OCaml from the Very Beginning"](http://ocaml-book.com/) and ["More
OCaml: Algorithms, Methods and
Diversions"](http://ocaml-book.com/more-ocaml-algorithms-methods-diversions/) by
John Whitington and "Practical OCaml" by Joshua Smith. I decided to go with RWO
because when I asked "_what is the best book on OCaml_" on `#ocaml` IRC channel
RWO was an unanimous response from several users. The title itself is obviously
piggybacking on an earlier "Real World Haskell" released in the same series by
O'Reilly, which was in general a good book ([though it had its
flaws](/posts/2013-01-06-real-world-haskell-impressions-after-initial-chapters.html)).

The first nine chapters comprise about 40% of the book (190 pages out of 470
total) and cover the basics of OCaml: various data types (lists, records,
variants), error handling, imperative programming (eg. mutable variables and
data structures, I/O) and basics of the module system. Chapters 10 through 12
present advanced features of the module system and introduce object-oriented
aspects of OCaml. Language ecosystem (ie. tools and libraries) is discussed in
chapters 13 through 18. The remaining chapters 19 through 23 go into details of
OCaml compiler like garbage collector or Foreign Function Interface.

When I think back about reading "Real World Haskell" I recall that quite a lot
of space was dedicated to explaining in detail various basic functional
programming concepts. "Real World OCaml" is much more dense. It approaches
teaching OCaml just as if it was another programming language, without making
big deal of functional programming model. I am much more experienced now than
when reading RWH four years ago and this is exactly what I wanted. I wonder
however how will this approach work for people new to functional programming. It
reminds my of my early days as a functional programmer. I began learning Scala
having previously learned Scheme and Erlang (both unusual for functional
languages in lacking a type system). Both Scala and OCaml are not pure
functional languages: they allow free mixing of functional and imperative
(side-effecting) code. They also support object-oriented programming. My plan in
learning Scala was to learn functional programming and I quickly realized that I
was failing. Scala simply offered too many back-doors that allowed escaping into
the imperative world. So instead of forcing me to learn a new way of thinking it
allowed me to do things the old way. OCaml seems to be exactly the same in this
regard and RWO offers beginners little guidance to thinking
functionally. Instead, it gives them a full arsenal of imperative features early
on in the book. I am not entirely convinced that this approach will work well
for people new to FP.

"Real World OCaml" was published less than three years ago so it is a fairly
recent book. Quite surprisingly then several sections have already gone out of
date. The code does not work with the latest version of OCaml compiler and
requires non-obvious changes to work. (You can of course solve the problem by
working with the old version of OCaml compiler.) I was told on IRC that the
authors are already working on the second edition of the book to bring it to
date with today's OCaml implementation.

Given all the above my verdict on "Real World OCaml" is that it is a really good
book about OCaml itself (despite being slightly outdated) but not necessarily
the best book on basics of functional programming.

