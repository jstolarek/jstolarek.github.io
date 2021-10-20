---
title: Publications
---

Publications
============

2020
----

  * Emrich, F., Lindley, S., Stolarek, J., Cheney J., Coates J.: FreezeML:
    Complete and Easy Type Inference for First-Class Polymorphism, ACM SIGPLAN
    Conference on Programming Language Design and Implementation 2020 (PLDI
    2020), London, UK. [full text (pdf)](/files/emrich_lindley_stolarek_cheney_coates_freezeml.pdf),
    [extended version (pdf)](/files/emrich_lindley_stolarek_cheney_coates_freezeml_extended.pdf),
    DOI: [10.1145/3385412.3386003](https://doi.org/10.1145/3385412.3386003),
    arXiv: [2004.00396](https://arxiv.org/abs/2004.00396)

    _**Abstract**: ML is remarkable in providing statically typed polymorphism
    without the programmer ever having to write any type annotations.  The cost
    of this parsimony is that the programmer is limited to a form of
    polymorphism in which quantifiers can occur only at the outermost level of a
    type and type variables can be instantiated only with monomorphic types.\
    Type inference for unrestricted System F-style polymorphism is undecidable
    in general. Nevertheless, the literature abounds with a range of proposals
    to bridge the gap between ML and System F.\
    We put forth a new proposal, FreezeML, a conservative extension of ML with
    two new features.  First, let- and lambda-binders may be annotated with
    arbitrary System F types. Second, variable occurrences may be _frozen_,
    explicitly disabling instantiation.  FreezeML is equipped with
    type-preserving translations back and forth between System F and admits a
    type inference algorithm, an extension of algorithm W, that is sound and
    complete and which yields principal types._

  * Stolarek, J. and Nowak, P.: A Modular, Practical Test for a Programming
    Course, Proceedings of the 51st ACM Technical Symposium on Computer Science
    Education, SIGCSE '20, Portland, Oregon 2020,
    [full text (pdf)](/files/stolarek_nowak_sigcse2020.pdf),
    [video (mkv)](/files/stolarek_nowak_sigcse2020_video.mkv),
    [slides (pdf)](/files/stolarek_nowak_sigcse2020_slides.pdf),
    DOI: [10.1145/3328778.3366886](https://doi.org/10.1145/3328778.3366886),
    [citation (BibTeX)](/files/stolarek_nowak_sigcse2020.bib)

    _**Abstract**: In order to evaluate students' programming skills during a
    university course, a practical programming test can be administered, in
    which students are required to implement a short yet complete program
    according to a provided specification. However, such tests often suffer from
    drawbacks that prevent comprehensive and accurate assessment of students'
    abilities.  In this paper we identify these drawbacks and then present a
    modular, practical test that avoids common testing pitfalls, as well as show
    how to design such a test based on course learning outcomes.  A key aspect
    of our approach is adoption of modularity, which ensures independent and
    comprehensive verification of learning outcomes.  We have used our method to
    evaluate object-oriented programming skills of undergraduate students over
    several years and have found that our testing approach has proven its
    validity and superiority over approaches employed previously._


2019
----

  * Stolarek, J. and Cheney, J.: Veriﬁed Self-Explaining Computation, Hutton
    G. (eds) Mathematics of Program Construction. MPC 2019. Lecture Notes in
    Computer Science, vol 11825. Springer, Cham. (presented at: 13th
    International Conference on Mathematics of Program Construction (MPC 2019),
    Porto, Portugal, 2019),
    [full text (pdf)](/files/stolarek_cheney_verified_self_explaining_computation.pdf),
    [slides (pdf)](/files/stolarek_cheney_verified_self_explaining_computation_slides.pdf),
    [BitBucket repository](https://bitbucket.org/jstolarek/gc_imp_slicing), DOI:
    [10.1007/978-3-030-33636-3_4](https://doi.org/10.1007/978-3-030-33636-3_4),
    [BibTeX citation](/files/stolarek_cheney_verified_self_explaining_computation.bib)

    _**Abstract**: Common programming tools, like compilers, debuggers, and
    IDEs, crucially rely on the ability to analyse program code to reason about
    its behaviour and properties.  There has been a great deal of work on
    verifying compilers and static analyses, but far less on verifying dynamic
    analyses such as program slicing.  Recently, a new mathematical framework
    for slicing was introduced in which forward and backward slicing are dual in
    the sense that they constitute a Galois connection.  This paper formalises
    forward and backward dynamic slicing algorithms for a simple imperative
    programming language, and formally verifies their duality using the Coq
    proof assistant._

  * Emrich, F., Lindley, S., Stolarek, J., Cheney J.: FreezeML: Complete and
    Easy Type Inference for First-Class Polymorphism, presentated at: 4th
    Workshop on Type-Driven Development (TyDe 2019), Berlin, Germany, 2019,
    [extended abstract (pdf)](/files/emrich_lindley_stolarek_cheney_freezeml_extended_abstract.pdf)

    _**Abstract**: ML is remarkable in providing statically typed polymorphism
    without the programmer ever having to write any type annotations.  The cost
    of this parsimony is that the programmer is limited to a form of
    polymorphism in which quantifiers can occur only at the outermost level of a
    type and type variables can be instantiated only with monomorphic
    types.\
    The general problem of type inference for unrestricted System
    F-style polymorphism is undecidable in general.  Nevertheless, the
    literature abounds with a range of proposals to bridge the gap between ML
    and System F by augmenting ML with type annotations or other features.\
    We present a new proposal, with different goals to much of the existing
    literature.  Our aim is to design a minimal extension to ML to support
    first-class polymorphism.  We err on the side of explicitness over
    parsimony, extending ML with two new features.  First, λ- and let-bindings
    may be annotated with arbitrary System F types.  Second, variable
    occurrences may be frozen, explicitly disabling instantiation.  The
    resulting language is not always as concise as more sophisticated systems,
    but in practice it does not appear to require a great deal more ink.\
    FreezeML is a conservative extension of ML, equipped with type-preserving
    translations back and forth between System F.  It admits a type inference
    algorithm, a mild extension of algorithm W, that is sound and complete and
    which yields principal types._


2018
----

  * Stolarek, J. and Cheney, J.: Language-integrated provenance in Haskell, The
    Art, Science, and Engineering of Programming, Vol. 2, No. 3 (presented at
    <Programming> 2018, Nice, France),
    [full text (pdf)](/files/stolarek_cheney_language_integrated_provenance_in_haskell.pdf),
    [slides (pdf)](/files/stolarek_cheney_language_integrated_provenance_in_haskell_slides.pdf),
    [GitHub repository](https://github.com/jstolarek/skye-dsh),
    DOI: [10.22152/programming-journal.org/2018/2/11](https://doi.org/10.22152/programming-journal.org/2018/2/11),
    [BibTeX citation](/files/stolarek_cheney_language_integrated_provenance_in_haskell.bib)

    _**Abstract**: Scientific progress increasingly depends on data management,
    particularly to clean and curate data so that it can be systematically
    analyzed and reused.  A wealth of techniques for managing and curating data
    (and its provenance) have been proposed, largely in the database community.
    In particular, a number of influential papers have proposed collecting
    provenance information explaining where a piece of data was copied from, or
    what other records were used to derive it.  Most of these techniques,
    however, exist only as research prototypes and are not available in
    mainstream database systems.  This means scientists must either implement
    such techniques themselves or (all too often) go without.\
    This is essentially a code reuse problem: provenance techniques currently
    cannot be implemented reusably, only as ad hoc, usually unmaintained
    extensions to standard databases.  An alternative, relatively unexplored
    approach is to support such techniques at a higher abstraction level, using
    metaprogramming or reflection techniques.  Can advanced programming
    techniques make it easier to transfer provenance research results into
    practice?\
    We build on a recent approach called _language-integrated provenance_, which
    extends language-integrated query techniques with source-to-source query
    translations that record provenance.  In previous work, a proof of concept
    was developed in a research programming language called Links, which
    supports sophisticated Web and database programming.  In this paper, we show
    how to adapt this approach to work in Haskell building on top of the
    Database-Supported Haskell (DSH) library.\
    Even though it seemed clear in principle that Haskell's rich programming
    features ought to be sufficient, implementing language-integrated provenance
    in Haskell required overcoming a number of technical challenges due to
    interactions between these capabilities.  Our implementation serves as a
    proof of concept showing how this combination of metaprogramming features
    can, for the first time, make data provenance facilities available to
    programmers as a library in a widely-used, general-purpose language.</br> In
    our work we were successful in implementing forms of provenance known as
    where-provenance and lineage.  We have tested our implementation using a
    simple database and query set and established that the resulting queries are
    executed correctly on the database.  Our implementation is publicly
    available on GitHub.\
    Our work makes provenance tracking available to users of DSH at little cost.
    Although Haskell is not widely used for scientific database development, our
    work suggests which languages features are necessary to support provenance
    as library.  We also highlight how combining Haskell's advanced type
    programming features can lead to unexpected complications, which may
    motivate further research into type system expressiveness._


2017
----

  * Ricciotti, W., Stolarek, J., Perera, R. and Cheney, J.: Imperative
    Functional Programs That Explain Their Work, Proceedings of the ACM on
    Programming Languages, Vol. 1, No. ICFP, Article 14 (presented at
    International Conference on Functional Programming 2017, Oxford, United
    Kingdom),
    [full text (pdf)](/files/ricciotti_stolarek_perera_cheney_icfp2017.pdf),
    [extended version (pdf)](/files/ricciotti_stolarek_perera_cheney_icfp2017_extended.pdf),
    [video (Youtube)](https://www.youtube.com/watch?v=6_ISyRsAhEY),
    [slides (pdf)](/files/ricciotti_stolarek_perera_cheney_icfp2017_slides.pdf),
    [GitHub repository](https://github.com/jstolarek/slicer),
    DOI: [10.1145/3110258](https://doi.org/10.1145/3110258),
    [citation (BibTeX)](/files/ricciotti_stolarek_perera_cheney_icfp2017.bib)

    _**Abstract**: Program slicing provides explanations that illustrate how
    program outputs were produced from inputs.  We build on an approach
    introduced in prior work, where dynamic slicing was defined for pure
    higher-order functional programs as a Galois connection between lattices of
    partial inputs and partial outputs.  We extend this approach to _imperative
    functional programs_ that combine higher-order programming with references
    and exceptions.  We present proofs of correctness and optimality of our
    approach and a proof-of-concept implementation and experimental
    evaluation._


2015
----

  * Stolarek, J., Peyton Jones, S. and Eisenberg, R. A.: Injective Type Families
    for Haskell, ACM SIGPLAN Notices 50(12):118-128 (presented at ACM SIGPLAN
    Haskell Symposium 2015, Vancouver, Canada),
    [full text (pdf)](/files/stolarek_peyton-jones_eisenberg_injectivity.pdf),
    [extended version (pdf)](/files/stolarek_peyton-jones_eisenberg_injectivity_extended.pdf),
    [video (Youtube)](https://www.youtube.com/watch?v=s0wkCKZU3WI),
    [slides (pdf)](/files/injectivity-haskell15-slides.pdf),
    DOI: [10.1145/2804302.2804314](http://dx.doi.org/10.1145/2804302.2804314),
    [citation (BibTeX)](/files/stolarek_peyton-jones_eisenberg_injectivity.bib)

    _**Abstract**: Haskell, as implemented by the Glasgow Haskell Compiler
    (GHC), allows expressive type-level programming.  The most popular
    type-level programming extension is `TypeFamilies`, which allows users to
    write functions on types.  Yet, using type functions can cripple type
    inference in certain situations. In particular, lack of injectivity in type
    functions means that GHC can never infer an instantiation of a type variable
    appearing only under type functions.\
    In this paper, we describe a small modification to GHC that allows type
    functions to be annotated as injective. GHC naturally must check validity of
    the injectivity annotations. The algorithm to do so is surprisingly
    subtle. We prove soundness for a simplification of our algorithm, and state
    and prove a completeness property, though the algorithm is not fully
    complete.\
    As much of our reasoning surrounds functions defined by a simple
    pattern-matching structure, we believe our results extend beyond just
    Haskell. We have implemented our solution on a branch of GHC and plan to
    make it available to regular users with the next stable release of the
    compiler._


2014
----

  * Eisenberg, R. A., and Stolarek, J.: Promoting Functions to Type Families in
    Haskell, ACM SIGPLAN Notices 49(12):95-106 (presented at ACM SIGPLAN Haskell
    Symposium 2014, Gothenburg, Sweden),
    [full text (pdf)](/files/eisenberg_stolarek_promotion.pdf),
    [extended version (pdf)](/files/eisenberg_stolarek_promotion_extended.pdf),
    [video (Youtube)](https://www.youtube.com/watch?v=J47OTYArG08),
    [slides (pdf)](/files/promotion-haskell14-slides.pdf),
    DOI: [10.1145/2633357.2633361](http://dx.doi.org/10.1145/2633357.2633361),
    [GitHub repository](http://www.github.com/goldfirere/singletons),
    [citation (BibTeX)](/files/eisenberg_stolarek_promotion.bib)

    _**Abstract**: Haskell, as implemented in the Glasgow Haskell Compiler
    (GHC), is enriched with many extensions that support type-level programming,
    such as promoted datatypes, kind polymorphism, and type families. Yet, the
    expressiveness of the type-level language remains limited. It is missing
    many features present at the term level, including `case` expressions,
    anonymous functions, partially-applied functions, and `let` expressions. In
    this paper, we present an algorithm - with a proof of correctness - to
    encode these term-level constructs at the type level. Our approach is
    automated and capable of promoting a wide array of functions to type
    families. We also highlight and discuss those term-level features that are
    not promotable. In so doing, we offer a critique on GHC's existing type
    system, showing what it is already capable of and where it may want
    improvement. We believe that delineating the mismatch between GHC's term
    level and its type level is a key step toward supporting dependently typed
    programming.\
    We have implemented our approach as part of the `singletons` package,
    available online._


2013
----

  * Stolarek, J.: Verifying weight biased leftist heaps using dependent types,
    unpublished,
    [full text (pdf)](/files/dep-typed-wbl-heaps.pdf),
    [companion code (tar.gz)](/files/dep-typed-wbl-heaps.tar.gz),
    BitBucket repositories:
    [Agda code](http://www.github.com/goldfirere/singletons),
    [Haskell code](https://bitbucket.org/jstolarek/dep-typed-wbl-heaps-hs)

    _**Abstract**: This paper is an intermediate level tutorial on veriﬁcation
    using dependent types in Agda.  It is also a case study of weight biased
    leftist heap data structure in a purely functional, dependently typed
    setting.  Paper demonstrates how to write a formally veriﬁed implementation
    that is guaranteed to maintain that structure’s invariants.  The reader will
    learn how to construct complex equality proofs by building them from smaller
    parts.  This knowledge will enable the reader to understand more advanced
    veriﬁcation techniques, eg. equational reasoning provided by Agda’s standard
    library or tactics system found in Idris and Coq programming languages._


2012
----

  * Stolarek, J.: Understanding Basic Haskell Error Messages, The Monad.Reader
    20, 2012,
    [full text (pdf)](/files/stolarek_understanding_basic_haskell_error_messages.pdf),
    [citation (BibTeX)](/files/stolarek_understanding_basic_haskell_error_messages.bib)

    _**Abstract**: Haskell is a language that differs greatly from the
    mainstream languages of today.  An emphasis on pure functions, a strong
    typing system, and a lack of loops and other conventional features make it
    harder to learn for programmers familiar only with imperative programming.
    One particular problem I faced during my initial contact with Haskell was
    unclear error messages. Later, seeing some discussions on #haskell, I
    noticed that I wasn't the only one.  Correcting a program without
    understanding error messages is not an easy task.  In this tutorial, I aim
    to remedy this problem by explaining how to understand Haskell's error
    messages.  I will present code snippets that generate errors, followed by
    explanations and solutions.  I used GHC 7.4.1 and Haskell Platform
    2012.2.0.0 for demonstration.  I assume reader's working knowledge of GHCi.
    I also assume that reader knows how data types are constructed, what type
    classes are and how they are used.  Knowledge of monads and language
    extensions is not required._

  * Stolarek, J.: Adaptive wavelet synthesis for improving digital image
    watermarking, In: _Towards Modern Collaborative Knowledge Sharing Systems_,
    Springer, 133–144, Eds: Lipiński, P., and Świrski, K., 2012,
    [full text (pdf)](/files/stolarek_adaptive_wavelet_synthesis_for_watermarking.pdf),
    [citation (BibTeX)](/files/stolarek_adaptive_wavelet_synthesis_for_watermarking.bib)

    _**Abstract:** Discrete Wavelet Transform is one of the most popular tools
    of digital signal processing.  Many different wavelet functions have been
    proposed so far, however there is no wavelet that would be the most suitable
    for every task.  Therefore a method allowing to adaptively synthesize the
    most suitable wavelet for a given task must be developed.  In this paper a
    general outline of such method will be discussed.  A concept of tools used
    for analysis of adaptive wavelets will be presented._

  * Stolarek, J., and Lipiński, P.: Improving watermark resistance against
    removal attacks using orthogonal wavelet adaptation, 38th Conference on
    Current Trends in Theory and Practice of Computer Science, volume 7147,
    Springer, 588–599, 2012,
    [full text (pdf)](/files/stolarek_lipinski_improving_watermark_resistance_against_removal_attacks.pdf),
    [BibTeX citation](/files/stolarek_lipinski_improving_watermark_resistance_against_removal_attacks.bib)

    _**Abstract:** This paper proposes a new approach for enhancing the
    robustness of wavelet-based image watermarking algorithms.  The method
    adjusts wavelet used in the process of image watermarking in order to
    maximize resistance against image processing operations.  Effectiveness of
    the approach is demonstrated using blind multiplicative watermarking
    algorithm, but it can easily be generalized to all wavelet-based
    watermarking algorithms.  Presented results demonstrate that wavelets
    generated using proposed approach outperform other wavelet bases commonly
    used in image watermarking in terms of robustness to removal attacks._

  * Stolarek, J.: Synteza falek ortogonalnych na podstawie oceny przetworzonego
    sygnału (Orthogonal wavelet synthesis based on signal processing outcome),
    PhD thesis, Łódź 2011 (defended in 2012),
    [full text (pdf)](/files/stolarek_rozprawa_doktorska.pdf),
    [presentation (pdf)](/files/stolarek_obrona_dr_prezentacja.pdf),
    [citation (BibTeX)](/files/stolarek_rozprawa_doktorska.bib)


2011
----

  * Stolarek, J., and Byczkowska-Lipińska, L.: Wavelet adaptation based on
    signal processing outcome, System Modelling and Control 2011, 2011,
    [full text (pdf)](/files/stolarek_byczkowska_wavelet_adaptation_based_on_signal_processing_outcome.pdf),
    [citation (BibTeX)](/files/stolarek_byczkowska_wavelet_adaptation_based_on_signal_processing_outcome.bib)

    _**Abstract:** The problem of wavelet synthesis is a crucial part of wavelet
    theory.  In this paper an overview of wavelet synthesis methods used in the
    literature up till now is given.  It is demonstrated that these well known
    approaches suffer from some major drawbacks.  A new approach to adaptive
    wavelet synthesis that overcomes these drawbacks and has not been exploited
    before is proposed.  This approach is based on adapting wavelet to one
    particular signal and specific signal processing algorithm._

  * Stolarek, J.: Adaptive synthesis of a wavelet transform using fast neural
    network, Bulletin of Polish Academy of Sciences. Technical Sciences 59(1),
    2011,
    [full text (pdf)](/files/stolarek_adaptive_synthesis_of_a_wavelet_transform.pdf),
    [citation (BibTeX)](/files/stolarek_adaptive_synthesis_of_a_wavelet_transform.bib)

    _**Abstract:** This paper introduces a new method for adaptive synthesis of
    a wavelet transform using fast neural network with topology based on lattice
    structure.  Lattice structure and orthogonal lattice structure are presented
    and their properties are discussed.  A novel method for unsupervised
    training of the neural network is introduced.  Proposed approach is tested
    by synthesizing new wavelets with expected energy distribution between low-
    and high-pass filters.  Energy compaction of proposed method and Daubechies
    wavelets is compared. Tests are performed using sound and image signals._

  * Stolarek, J.: On the properties of a lattice structure for a wavelet filter
    bank implementation: Part I, Journal of Applied Computer Science 19(1),
    85-116, 2011,
    [full text (pdf)](/files/stolarek_properties_of_lattice_structure_part1.pdf),
    [citation (BibTeX)](/files/stolarek_properties_of_lattice_structure_part1.bib)

    _**Abstract:** This paper presents concept of a lattice structure for
    parametrization and implementation of a Discrete Wavelet Transform.
    Theoretical properties of the lattice structure are discussed in detail.  An
    algorithm for converting the lattice structure to a wavelet filter bank
    coefficients is constructed.  A theoretical proof demonstrating that filters
    implemented by the lattice structure fulfil conditions imposed on an
    orthogonal wavelet filter bank is conducted._

  * Stolarek, J.: On the properties of a lattice structure for a wavelet filter
    bank implementation: Part II, Journal of Applied Computer Science 19(2),
    125-139, 2011,
    [full text (pdf)](/files/stolarek_properties_of_lattice_structure_part2.pdf),
    [citation (BibTeX)](/files/stolarek_properties_of_lattice_structure_part2.bib)

    _**Abstract:** This paper continues discussion of a lattice structure for
    parametrization and implementation of a Discrete Wavelet Transform.  Based
    on an algorithm for converting the lattice structure to a wavelet filter
    bank coefficients, developed in the first part of this paper, second part of
    the proof demonstrating that filters implemented by the lattice structure
    fulfil conditions imposed on an orthogonal wavelet filter bank is carried
    out._

  * Lipiński, P., and Stolarek, J.: Digital watermarking enhancement using
    wavelet filter parametrization, Adaptive and Natural Computing Algorithms
    (10th ICANNGA, 2011), volume 1, 330-339, Eds: Dobnikar, A., Lotrič, U., and
    Šter, B., 2011, [full text (pdf)](/files/lipinski_stolarek_icannga2011.pdf),
    [citation (BibTeX)](/files/lipinski_stolarek_icannga2011.bib)

    _**Abstract:** In this paper a genetic-based enhancement of digital image
    watermarking in the Discrete Wavelet Transform domain is presented.  The
    proposed method is based on adaptive synthesis of a mother wavelet used for
    image decomposition.  Wavelet synthesis is performed using parametrization
    based on an orthogonal lattice structure.  A genetic algorithm is applied as
    an optimization method to synthesize a wavelet that provides the best
    watermarking quality in respect to the given optimality criteria.
    Effectiveness of the proposed method is demonstrated by comparing
    watermarking results using synthesized wavelets and the most commonly used
    Daubechies wavelets.  Experiments demonstrate that mother wavelet selection
    is an important part of a watermark embedding process and can influence
    watermarking robustness, separability and fidelity._


2010
----

  * Stolarek, J.: Adaptive Wavelet Synthesis For Improving Digital Image
    Processing, 1st TEWI Conference, 2010,
    [presentation (pdf)](/files/stolarek_tewi2010.pdf),
    [citation (BibTeX)](/files/stolarek_tewi2010.bib)

    _**Abstract:** Discrete Wavelet Transform is one of the most popular tools
    of digital signal processing.  Many different wavelet functions have been
    proposed so far, however there is no wavelet that would be the most suitable
    for every task.  Therefore a method allowing to adaptively synthesize the
    most suitable wavelet for a given task must be developed.  In this paper a
    general outline of such method will be discussed.  A concept of tools used
    for analysis of adaptive wavelets will be presented._

  * Stolarek, J.: Improving energy compaction of a wavelet transform using
    genetic algorithm and fast neural network, Archives of Control Sciences
    20(4), 417-433, December 2010,
    [full text (pdf)](/files/stolarek_improving_energy_compaction_of_a_wavelet_transform.pdf),
    [citation (BibTeX)](/files/stolarek_improving_energy_compaction_of_a_wavelet_transform.bib)

    _**Abstract:** In this paper a new method for adaptive synthesis of a smooth
    orthogonal wavelet, using fast neural network and genetic algorithm, is
    introduced.  Orthogonal lattice structure is presented.  A new method of
    supervised training of fast neural network is introduced to synthesize a
    wavelet with desired energy distribution between output signals from
    low-pass and high-pass filters on subsequent levels of a Discrete Wavelet
    Transform.  Genetic algorithm is proposed as a global optimization method
    for defined objective function, while neural network is used as a local
    optimization method to further improve the result.  Proposed approach is
    tested by synthesizing wavelets with expected energy distribution between
    low- and high-pass filters.  Energy compaction of proposed method and
    Daubechies wavelets is compared.  Tests are performed using image signals._

  * Stolarek, J., and Lipiński, P.: Improving Digital Watermarking Fidelity
    Using Fast Neural Network for Adaptive Wavelet Synthesis, Journal of Applied
    Computer Science 18(1), 61-74, 2010,
    [full text (pdf)](/files/stolarek_lipinski_improving_digital_watermarking_fidelity_using_fast_neural_network.pdf),
    [citation (BibTeX)](/files/stolarek_lipinski_improving_digital_watermarking_fidelity_using_fast_neural_network.bib)

    _**Abstract:** This paper introduces a new adaptive algorithm for digital
    watermark embedding in wavelet domain.  The proposed algorithm performs
    adaptive mother wavelet synthesis based on a low frequency component energy
    maximization.  The algorithm is based on an orthogonal neural network.  We
    demonstrate that the presented adaptive method can improve both the
    correlation between an extracted watermark and an embedded watermark, as
    well as the fidelity of an image.  The proposed algorithm is applied to
    improve well known wavelet based embedding algorithms._


2009
----

  * Stolarek, J.: Synthesis of a wavelet transform using neural network, XI
    International PhD Workshop OWD, Conference Archives PTETiS, volume 26,
    71-74, 2009,
    [full text (pdf)](/files/stolarek_owd2009.pdf),
    [presentation (pdf)](/files/stolarek_owd2009_slides.pdf),
    [citation (BibTeX)](/files/stolarek_owd2009.bib)

    _**Abstract:** Wavelet transform has a wide area of application in signal
    processing.  However there is no single wavelet perfectly suitable for every
    task.  In practice Daubechies 4 is the most commonly used wavelet, since it
    is well suited for analysis of many natural signals and it offers a
    straightforward interpretation of the results.  It would be very useful to
    develop a method for adaptive synthesis of a wavelet transform suitable for
    particular task.  Artificial neural networks offer such ability.  So far
    this approach wasn't explored.  This paper presents neural network for
    synthesis of orthogonal wavelet transform and a method of unsupervised
    training of this network._

  * Stolarek, J., and Yatsymirskyy, M.: Fast neural network for synthesis and
    implementation of orthogonal wavelet transform, Image Processing &
    Communications Challenges, AOW EXIT, 87-94, 2009,
    [full text (pdf)](/files/stolarek_yatsymirskyy_ipc2009.pdf),
    [citation (BibTeX)](/files/stolarek_yatsymirskyy_ipc2009.bib)

    _**Abstract:** This paper presents lattice structure and orthogonal lattice
    structure for synthesis of a wavelet transform.  Lattice structure is based
    on parametrized 2x2 operations.  Adjustment of parameter values leads to
    synthesis of a new wavelet transform.  Fast neural network with topology
    based on lattice structure is applied to determine values of the parameters.
    Factorization-based reduction in number of arithmetic operations is
    demonstrated._

  * Stolarek, J.: Realization of Daubechies transform using lattice structure,
    The Collection of Scientific Works of ISDMCI 2009, 188-192, 2009,
    [full text (pdf)](/files/stolarek_isdmci2009.pdf),
    [presentation (pdf)](/files/stolarek_isdmci2009_slides.pdf),
    [citation (BibTeX)](/files/stolarek_isdmci2009.bib)

    _**Abstract:** This paper presents a new approach to wavelet synthesis by
    using fast neural networks.  Fast neural network architecture and the
    training process are discussed.  Training results are presented showing that
    the proposed network is able to learn the Daubechies wavelet transform._


2008
----

  * Stolarek, J.: Nowe kryteria wykrywania minucji w algorytmie rozpoznawania
    odcisków palców, XVI Konferencja Sieci i Systemy Informatyczne. Teoria,
    Projekty, Wdrożenia, Aplikacje [Electronic document], 1 optical disc
    (CD-ROM) D:/referaty/16003.pdf s.[1-6], Łódź, Polska, 2008,
    [wersja online (pdf)](/files/stolarek_sis2008.pdf),
    [presentation (pdf)](/files/stolarek_sis2008_slides.pdf),
    [citation (BibTeX)](/files/stolarek_sis2008.bib)

    _**Abstract:** W artykule przedstawiono ulepszenie algorytmu rozpoznawania
    użytkownika na podstawie analizy odcisków palców. Algorytm polega na
    śledzeniu przebiegu linii papilarnych w celu wykrycia minucji, czyli miejsc
    w których linie papilarne kończą się lub rozgałęziają. Autor prezentuje nowe
    kryteria pozwalające stwierdzić istnienie w danym punkcie minucji. Układ
    minucji pozwala na jednoznaczne rozpoznanie osoby._

  * Stolarek, J.: Identyﬁkacja użytkownika na podstawie analizy linii
    papilarnych (User identification based on fingerprint analysis), MSc thesis,
    Łódź 2008, [full text (pdf)](/files/stolarek_praca_magisterska.pdf),
    [citation (BibTeX)](/files/stolarek_praca_magisterska.bib)
