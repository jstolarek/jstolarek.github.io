---
title: Promoting functions to type families in Haskell
date: 2014-09-08
---

Promoting functions to type families in Haskell
===============================================

It's been very quiet on the blog these past few months not because I'm spending
less time on functional programming but precisely for the opposite reason. Since
January I've been working together with [Richard
Eisenberg](http://www.cis.upenn.edu/~eir/) to extend his
[`singletons`](http://hackage.haskell.org/package/singletons) library. This work
was finished in June and last Friday I gave a talk about our research on
[Haskell Symposium 2014](http://www.haskell.org/haskell-symposium/2014/). This
was the first time I've been to the ICFP and Haskell Symposium. It was pretty
cool to finally meet all these people I know only from IRC. I also admit that
the atmosphere of the conference quite surprised me as it often felt like some
sort of fan convention rather than the biggest event in the field of functional
programming.

The paper Richard and I published is titled ["Promoting Functions to Type
Families in
Haskell"](http://ics.p.lodz.pl/~stolarek/_media/pl:research:eisenberg_stolarek_promotion.pdf).
This work is based on Richard's earlier paper ["Dependently typed programming
with
singletons"](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf)
presented two years ago on Haskell Symposium. Back then Richard presented the
`singletons` library that uses Template Haskell to generate singleton types and
functions that operate on them. Singleton types are types that have only one
value (aside from bottom) which allows to reason about runtime values during
compilation (some introduction to singletons can be found in [this
post](http://typesandkinds.wordpress.com/2013/04/01/defunctionalization-for-the-win/)
on Richard's blog). This smart encoding allows to simulate some of the features
of dependent types in Haskell. In our current work we extended promotion
capabilities of the library. Promotion is only concerned with generating
type-level definitions from term-level ones. Type-level language in GHC has
become quite expressive during the last couple of years but it is still missing
many features available in the term-level language. Richard and I have found
ways to encode almost all of these missing features using the already existing
type-level language features. What this means is that you can write normal
term-level definition and then our library will automatically generate an
equivalent type family. You're only forbidden from using infinite terms, the
`do`\-notation, and decomposing `String` literals to `Char`s. Numeric literals
are also very problematic and the support is very limited but some of the issues
can be worked around. What is really cool is that our library allows you to have
partial application at the type level, which GHC normally prohibits.

You can learn more by [watching my talk on
YouTube](https://www.youtube.com/watch?v=J47OTYArG08), [reading the
paper](http://ics.p.lodz.pl/~stolarek/_media/pl:research:eisenberg_stolarek_promotion.pdf)
or the [`singletons`
documentation](https://github.com/goldfirere/singletons/blob/master/README.md).
Here I'd like to add a few more information that are not present in the
paper. So first of all the paper was concerned only with promotion and didn't
say anything about singletonization. But as we enabled more and more language
constructs to be promoted we also made them singletonizable. So almost
everything that can be promoted can also be singletonized. The most notable
exception to this rule are type classes, which are not yet implemented at the
moment.

An interesting issue was raised by [Adam
Gundry](https://personal.cis.strath.ac.uk/adam.gundry/) in a question after the
talk: what about difference between lazy term-level semantics and strict
type-level semantics? You can listen to my answer in the video but I'll
elaborate some more on this here. At one point during our work we were wondering
about this issue and decided to demonstrate an example of an algorithm that
crucially relies on laziness to work, ie. fails to work with strict semantics. I
think it's not straightforward to come up with such an algorithm but luckily I
recalled the backwards state monad from Philip Wadler's paper "The essence of
functional programming" ((The awful truth is that this monad does not really
work with the released version of `singletons`. I only realized that when I was
writing this post. See issue
[#94](https://github.com/goldfirere/singletons/issues/94) on `singletons` bug
tracker. )). Bind operator of that monad looks like this (definition copied from
the paper):

```haskell
m `bindS` k = \s2 -> let (a,s0) = m s1
                         (b,s1) = k a s2
                     in  (b,s0)
```

The tricky part here is that the output of call to `m` becomes input to call to
`k`, while the output of call to `k` becomes the input of `m`. Implementing this
in a strict language does not at all look straightforward. So I promoted that
definition expecting it to fail spectacularly but to my surprised it worked
perfectly fine. After some investigation I understood what's going
on. Type-level computations performed by GHC are about constraint solving. It
turns out that GHC is able to figure out in which order to solve these
constraints and get the result. It's exactly analogous to what happens with the
term-level version at runtime: we have an order of dependencies between the
closures and there is a way in which we can run these closures to get the final
result.

All of this work is a small part of a larger endeavour to push Haskell's type
system towards dependent types. With singletons you can write type-level
functions easily by writing their definitions using the term-level language and
then promoting these definitions. And then you can singletonize your functions
to work on singleton types. There were two other talks about dependent types
during the conference: Stephanie Weirich's ["Depending on
Types"](https://www.youtube.com/watch?v=rhWMhTjQzsU) keynote lecture during ICPF
and Richard's ["Dependent Haskell"](https://www.youtube.com/watch?v=O805YjOsQjI)
talk during Haskell Implementators Workshop. I encourage everyone interested in
Haskell's type system to watch both of these talks.

