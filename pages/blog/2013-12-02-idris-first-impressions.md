---
title: Idris - first impressions
date: 2013-12-02
---

Idris - first impressions
=========================

During last few weeks I got a pretty good grip of basics of dependent types and
Agda. Programming in Agda is fun but nevertheless I decided to experiment with
other dependently-typed programming languages. Back in March I attempted to
learn [Idris](http://www.idris-lang.org/) from [one of Edwin Brady's
presentations](http://edwinb.wordpress.com/2013/03/15/idris-course-at-itu-slides-and-video/),
but having no knowledge of dependent types I had to give up after about 30
minutes of first video. Now that I know basics of Agda I decided to give Idris
another try. This time it was much simpler. Reading [official Idris
tutorial](http://eb.host.cs.st-andrews.ac.uk/writings/idris-tutorial.pdf) and
doing some experiments took me about 5 hours. Below are some of my first
impressions (I'm underlining that phrase to make it clear that some of these
opinions may change in the future).

  - Standard library in Idris feels friendlier than in Agda. It is bundled with
    the compiler and doesn't require additional installation (unlike Agda's).
    Prelude is by default imported into every module so programmer can use Nat,
    Bool, lists and so on out of the box. There are also some similarities with
    Haskell prelude. All in all, standard library in Idris is much less daunting
    than in Agda.

  - Idris is really a programming language, i.e. one can write programs that
    actually run. Agda feels more like a proof assistant. According to one of
    the tutorials I've read you can run programs written in Agda, but it is not
    as straightforward as in Idris. I personally I haven't run a single Agda
    program - I'm perfectly happy that they typecheck.

  - Compared to Agda Idris has limited Unicode support. I've never felt the need
    to use Unicode in my source code until I started programming in Agda - after
    just a few weeks it feels like an essential thing. I think Idris allows
    Unicode only in identifiers, but doesn't allow it in operators, which means
    I have to use awkward operators like `<!=` instead of ?. I recall seeing
    some discussions about Unicode at #idris channel, so I wouldn't be surprised
    if that changed soon.

  - One of the biggest differences between Agda and Idris is approach to
    proofs. In Agda a proof is part of function's code. Programmer is assisted
    by agda-mode (in Emacs) which guides code writing according to types (a
    common feature in dependently typed languages). Over the past few weeks I've
    come to appreciate convenience offered by agda-mode: automatic generation of
    case analysis, refinement of holes, autocompletion of code based on types to
    name a few. Idris-mode for Emacs doesn't support interactive development.
    One has to use interactive proof mode provided in Idris REPL - this means
    switching between terminal windows, which might be a bit inconvenient.
    Proofs in Idris can be separated from code they are proving. This allows to
    write code that is much clearer. In proof mode one can use tactics, which
    are methods used to convert proof terms in order to reach a certain goal.
    Generated proof can then be added to source file. It is hard for me to
    decide which method I prefer. The final result is more readable in Idris,
    but using tactics is not always straightforward. I also like interactive
    development offered by Agda. Tough choice.

  - Both languages are poorly documented. That said, Idris has much less
    documentation (mostly papers and presentations by Edwin Brady). I expect
    this to change, as the Idris community seems to be growing (slowly, but
    still).

  - One thing I didn't like in Idris are visibility qualifiers used to define
    how functions and datatypes are exported from the module. There are three
    available: public (export name and implementation), private (don't export
    anything) and abstract (export type signature, but don't export
    implementation).  This is slightly different than in Haskell - I think that
    difference comes from properties of dependent types. What I didn't like are
    rules and syntax used to define export visibility. Visibility for a function
    or datatype can be defined by annotating it with one of three keywords:
    public, private, abstract. If all definitions in a module are not annotated
    then everything is public. But if there is at least one annotation
    everything without annotation is private. Unless you changed the default
    visibility, in which case everything without annotation can be abstract! In
    other words if you see a definition without annotation it means that: a) it
    can be public, but you have to check if all other definitions are without
    annotations; b) private, if at least one other definition is annotated -
    again, you have to check whole file; c) but it can be abstract as well - you
    need to check the file to see if the default export level was set. The only
    way to be sure - except for [nuking the entire site from
    orbit](http://www.youtube.com/watch?v=aCbfMkh940Q) - is annotating every
    function with an export modifier, but that feels very verbose. I prefer
    Haskell's syntax for defining what is exported and what is not and I think
    it could be easily extended to support three possible levels of export
    visibility.

  - Unlike Agda, Idris has case expressions. They have some limitations however.
    I'm not sure whether these limitations come from properties of dependently
    typed languages or are they just simplifications in Idris implementation
    that could theoretically be avoided.

  - Idris has lots of other cool features. Idiom brackets are a syntactic sugar
    for applicative style: you can write `[| f a b c |]` instead of `pure f <*>
    a <*> b <$*gt; c`. Idris has syntax extensions designed to support
    development of EDSLs. Moreover tuples are available out of the box, there's
    do-notation for monadic expressions, there are list comprehensions and
    Foreign Function Interface.

  - One feature that I'm a bit sceptical about are "implicit conversions" that
    allow to define implicit casts between arguments and write expressions like
    `"Number " ++ x`, where `x` is an `Int`. I can imagine this could be a
    misfeature.

  - Idris has "using" notation that allows to introduce definitions that are
    visible throughout a block of code. Most common use seems to be in
    definition of data types. Agda does it better IMO by introducing type
    parameters into scope of data constructors.

  - Idris seems to be developed more actively. The repos are stored [on
    github](https://github.com/idris-lang/Idris-dev) so anyone can easily
    contribute. This is not the case with Agda, which has [Darcs
    repos](http://code.haskell.org/Agda/) and the whole process feels closed (in
    a sense "not opened to community"). On the other hand mailing list for Idris
    is set up on Google lists, which is a blocker for me.

All in all programming in Idris is also fun although it is slightly different
kind of fun than in Agda. I must say that I miss two features from Agda:
interactive development in Emacs and Unicode support. Given how actively Idris
is developed I imagine it could soon become more popular than Agda. Perhaps
these "missing" features will also be added one day?

As an exercise I rewrote code from "Why dependent types matter" paper from Agda
(see [my previous post](2013-11-07-why-dependent-types-matter-in-agda/)) to
Idris. Code is [available in on
github](https://github.com/jstolarek/why-dependent-types-matter).

