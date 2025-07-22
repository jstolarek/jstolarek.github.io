---
title: Projects
---

Projects
========

Below is a list of projects and libraries I have been involved with
professionally.

  * **Midnight**\
    I worked as a technical architect on the [Midnight](https://midnight.network/)
    blockchain, designing and implementing solutions that leverage Cardano as a
    trusted security layer.

  * **Cardano Partner Chains**
    ([github](https://github.com/input-output-hk/partner-chains-smart-contracts))\
    I have been involved in the IOG's [Cardano Partner
    Chains](https://github.com/input-output-hk/partner-chains) project as a
    system designer and developer of Plutus smart contracts.

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

    - I developed a prototype implementation of provenance tracking as a
      software library, which is unlike earlier implementations that were
      compiler extensions.  Details are described in [Language-integrated
      provenance in
      Haskell](/files/stolarek_cheney_language_integrated_provenance_in_haskell.pdf)
      paper.  Implementation is available on
      [github](https://github.com/jstolarek/skye-dsh).

    - I worked together with
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

  * **Links programming language**
    ([project homepage](https://links-lang.org/))\
    I have been involved with the development of Links programming language
    between 2016 and 2020, primarily as a compiler engineer.  I contributed
    various typechecker fixes and many code quality improvements.

  * **Glasgow Haskell Compiler**
    ([project homepage](https://gitlab.haskell.org/ghc/ghc))\
    I have been involved in development of GHC between 2013 and 2016.

    - Together with [Simon Peyton
      Jones](https://simon.peytonjones.org/) and [Richard
      Eisenberg](https://richarde.dev/) we extended GHC with injective type
      families.  Details are described in the paper [Injective Type Families for
      Haskell](/files/stolarek_peyton-jones_eisenberg_injectivity.pdf).

    - I contributed various patches to Template Haskell.

    - I implemented new PrimOps for comparing built-in primitive types.  These
      PrimOps allow writing branchless comparisons required in certain
      high-performace computations.  See [GHC
      wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/prim-bool) for an
      in-depth discussion of the problem and implementation details.

    - I worked on improving GHC's Cmm optimization pipeline.

  * **singletons** ([github](http://www.github.com/goldfirere/singletons),
    [hackage](http://hackage.haskell.org/package/singletons))\
    Together with [Richard Eisenberg](https://richarde.dev/) I developed a
    method of converting Haskell term-level functions to the type level.  Our
    approach is implemented in the `singletons` library.

I have also done some toy projects.

  * **Dreamcast GDMenu maker script for Linux**
    ([github](https://github.com/jstolarek/dc-card-maker-script))\
    A simple bash script for preparing an SD memory card to be used with GDEMU
    optical drive emulator for the Sega Dreamcast console.  Born out of
    frustration that the only existing tools were for Windows.

  * **Phantasy Star Online section ID calculator**
    ([github](https://github.com/jstolarek/pso-calc))\
    A simple utility for computing section ID assigned to player character in
    Phantasy Star Online.  Once again a product of frustration that there are
    only Windows and online tools, but no native Linux cli tools.

  * **Free IPS Patcher**
    ([bitbucket](https://bitbucket.org/jstolarek/free-ips-patcher))\
    A simple Linux cli tool for applying IPS patches.  Written because I was
    tired of using online patchers.

  * **dep-typed-wbl-heaps** ([tutorial (pdf)](/files/dep-typed-wbl-heaps.pdf),
    bitbucket: [Agda code](https://bitbucket.org/jstolarek/dep-typed-wbl-heaps),
    [Haskell code](https://bitbucket.org/jstolarek/dep-typed-wbl-heaps-hs))\
    Verified implementation of a weight biased leftist heap in Agda and Haskell.
    This project is a an intermediate-level tutorial aimed at people who already
    have a basic knowledge on program verification using dependent types.

  * **haskell-testing-stub**
    ([bitbucket](https://bitbucket.org/jstolarek/haskell-testing-stub))\
    A small stub project that demonstrates how to organize tests and benchmarks
    in a Haskell project.

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
