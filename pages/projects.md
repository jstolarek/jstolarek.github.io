---
title: Projects
---

Projects
========

Below is a list of projects and libraries I am or have been involved with.

  * **Skye: A programming language bridging theory and practice for scientific
    data curation**
    ([project homepage](http://homepages.inf.ed.ac.uk/jcheney/group/skye.html))
    - I worked together with Frank Emrich,
      [Sam Lindley](https://homepages.inf.ed.ac.uk/slindley/),
      [James Cheney](http://homepages.inf.ed.ac.uk/jcheney/) and Jonathan Coates
      on design of FreezeML, an extension of ML with first-class polymorphism.
      This system was then implemented in the [Links programming
      language](https://links-lang.org/).  Details are described in [FreezeML:
      Complete and Easy Type Inference for First-Class
      Polymorphism](/files/emrich_lindley_stolarek_cheney_coates_freezeml.pdf)
      paper.

    - I have developed a prototype implementation of provenance tracking as a
      library.  Details are described in [Language-integrated provenance in
      Haskell](/files/stolarek_cheney_language_integrated_provenance_in_haskell.pdf)
      paper.  Implementation is available on
      [github](https://github.com/jstolarek/skye-dsh).

    - I have worked together with
      [Wilmer Ricciotti](http://www.wilmer-ricciotti.net/),
      [Roly Perera](http://www.dcs.gla.ac.uk/~roly/) and
      [James Cheney](http://homepages.inf.ed.ac.uk/jcheney/) on slicing
      imperative functional programs.  Details are described in
      [Imperative Functional Programs That Explain Their
      Work](/files/ricciotti_stolarek_perera_cheney_icfp2017.pdf) paper.
      Implementation is available on
      [github](https://github.com/jstolarek/slicer).

    - I designed a slicing algorithm for Imp, an imperative toy programming
      language, and formally proved correctness of my algorithm in Coq. Details
      are described in [Verified Self-Explaining
      Computation](/files/stolarek_cheney_verified_self_explaining_computation.pdf)
      paper. Implementation is available on
      [bitbucket](https://bitbucket.org/jstolarek/gc_imp_slicing).

    - I contributed numerous patches to [Links programming
      language](https://links-lang.org/).

  * **Glasgow Haskell Compiler**
    ([project homepage](https://gitlab.haskell.org/ghc/ghc))\
    I have been involved in development of GHC between 2013 and 2016.

    - Together with [Simon Peyton
      Jones](http://research.microsoft.com/en-us/people/simonpj/) and [Richard
      Eisenberg](https://richarde.dev/) we extended GHC with injective type
      families.  Details are described in the paper [Injective Type Families for
      Haskell](/files/stolarek_peyton-jones_eisenberg_injectivity.pdf).

    - I contributed patches to Template Haskell.

    - I have implemented new PrimOps for comparing built-in primitive types.
      This allows to write branchless algorithms.  See [GHC
      wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/prim-bool) for an
      in-depth discussion of the problem and implementation details.

    - I worked on improving GHC's Cmm optimization pipeline.

  * **singletons** ([github](http://www.github.com/goldfirere/singletons),
    [hackage](http://hackage.haskell.org/package/singletons))\
    Together with [Richard Eisenberg](https://richarde.dev/) we developed a
    method of converting Haskell term-level functions to the type level.  Our
    approach is implemented in the `singletons` library.

  * **dep-typed-wbl-heaps** (bitbucket:
    [Agda code](https://bitbucket.org/jstolarek/dep-typed-wbl-heaps),
    [Haskell code](https://bitbucket.org/jstolarek/dep-typed-wbl-heaps-hs))\
    Verified implementation of a weight biased leftist heap in Agda and Haskell.
    This project is a an intermediate-level tutorial aimed at people who already
    have a basic knowledge on program verification using dependent types.

  * **haskell-testing-stub**
    ([bitbucket](https://bitbucket.org/jstolarek/haskell-testing-stub))\
    Demonstration of how to organize tests and benchmarks in a Haskell project.

  * **tasty-program**
    ([bitbucket](https://bitbucket.org/jstolarek/tasty-program),
    [hackage](http://hackage.haskell.org/package/tasty-program))\
    Library that extends [tasty](http://hackage.haskell.org/package/tasty)
    testing framework.  Allows to test whether a given program executes
    successfully.

  * **tasty-hunit-adapter**
    ([bitbucket](https://bitbucket.org/jstolarek/tasty-hunit-adapter/),
    [hackage](http://hackage.haskell.org/package/tasty-hunit-adapter))\
    Library that extends [tasty](http://hackage.haskell.org/package/tasty)
    testing framework.  Allows to import existing
    [HUnit](http://hackage.haskell.org/package/HUnit) tests.
