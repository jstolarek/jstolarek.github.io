---
title: DeepSpec Summer School 2017 - a summary
date: 2017-07-30
---

DeepSpec Summer School 2017 - a summary
=======================================

I have spent the last two and a half week in Philadelphia attending the first
[DeepSpec Summer School](https://deepspec.org/event/dsss17/), In this post I
want to summarize the event and give an overview of all the courses.

[The DeepSpec Project](https://deepspec.org) is a research project lead by
several US East Coast universities (University of Pennsylvania, MIT, Yale
University and Princeton University) and aims to _"push forward the state of the
art in applying computer proof assistants to verify realistic software and
hardware stacks at scale"_. It consists of [several smaller
projects](https://deepspec.org/page/Project/), including a formal verification
of a hypervisor ([CertiKOS](http://flint.cs.yale.edu/certikos/)), LLVM
([Vellvm](http://www.cis.upenn.edu/~stevez/vellvm/)), Coq compiler
([CertiCoq](https://www.cs.princeton.edu/~appel/certicoq/)) and GHC's Core
language ([CoreSpec](https://deepspec.org/entry/Project/Haskell+CoreSpec)).

The goal of DeepSpec Summer School was to introduce people to real-life formal
verification using Coq proof assistant. School was divided into three parts. All
the lectures [can be found on a YouTube
channel](https://www.youtube.com/channel/UC5yB0ZRgc4A99ttkwer-dDw). Coq code for
the courses [is available on GitHub](https://github.com/DeepSpec/dsss17). Summer
school's web page also provides [installation
instructions](https://deepspec.org/event/dsss17/installation.html) as well as
[other supplementary material](https://deepspec.org/event/dsss17/schedule.html)
(click on a given lecture or select from "Lectures" tab).

Week 0: Coq Intensive
=====================

First three days of the summer school were a very intensive introductory course
on Coq lead by Benjamin Pierce. This essentially covered the first volume of
Software Foundations. (Aside: For those of you who don't know yet, [original
Software Foundations online book has been split into two
volumes](https://deepspec.org/page/SF/): Logical Foundations and Programming
Language Foundations. Also, a third volume has been added to the series:
Verified Functional Algorithms by Andrew Appel. All three volumes can be found
[here](https://softwarefoundations.cis.upenn.edu/draft/), although expect that
this link will likely become broken soon when this draft versions become an
official release. There are also plans for two more volumes, one on Separation
Logic and another one on Systems Verification.)

Week 1: Programming Language Verification
=========================================

First full week of the school consisted of four courses centred around
programming language verification:

  - _Property-based random testing with QuickChick_ by Benjamin Pierce. I assume
    many of you heard about Haskell library called QuickCh**e**ck. It offers
    property-based testing: programmer writes properties that the should hold
    for a given piece of code and QuickCheck tests whether they hold for
    randomly generated test data. QuickCh**i**ck is implementation of the same
    idea in Coq. Now, you might wonder what is the point of doing such a thing
    in Coq. After all, Coq is about formally proving that a given property is
    always true, not randomly testing whether it holds. I was sceptical about
    this as well, but it actually turns to be quite a good idea. The point is,
    specifications are difficult to write and often even more difficult to
    prove. They are especially difficult to prove when they are false ;-) And
    this is exactly when QuickChick can be beneficial: by trying to find a
    counter-example for which a stated property does not hold. This can indeed
    save programmer from spending hours on trying to prove something that is
    false. If QuickChick doesn't find a counter-example we can start writing a
    formal proof. This course also gives a nice overview of type classes in Coq.

  - _The structure of verified compiler_ by Xavier Leroy. This series of
    lectures was based on [CompCert](http://compcert.inria.fr/), which is a
    formally verified C compiler. The ideas behind formal verification of a
    compiler were presented on a compiler of Imp (a toy imperative language used
    in Software Foundations) to a simple virtual machine. Fourth, final lecture
    covered the CompCert project itself. To me this was the most interesting
    course of the summer school.

  - _Language specification and variable binding_ by Stephanie Weirich. Software
    Foundations is a great book, but it completely omits one topic that is very
    important in formalizing programming languages: dealing with variable
    bindings. In this courses Stephanie presented "locally nameless"
    representation of variable bindings. This is something I had planned to
    learn for a very long time but couldn't find the time.

  - _Vellvm: Verifying the LLVM_ by Steve Zdancewic. For a change, in this
    course Imp was compiled to a simplified variant of LLVM, the compilation
    process being verified of course. Also, a nice introduction to LLVM.

Week 2: Systems Verification
============================

Courses during the second week put more focus on verifying computer
systems. Again, there were four courses:

  - _Certifying software with crashes_ by Frans Kaashoek and Nickolai Zeldovich.
    The topic of this course was certification of a hard-drive operating
    routines, including bad-sector remapping and a simple virtual RAID 1
    implementation. Although still using toy examples, specifications presented
    during this course were much more abstract giving a good idea how to scale
    to a real-world system verification. I found this course very difficult to
    follow, although the lectures were really superb. Note: materials for this
    one course are available [in a separate GitHub
    repo](https://github.com/mit-pdos/deepspec-pocs).

  - _CertiKOS: Certified kit operating systems_ by Zhong Shao. Ok, I admit I was
    completely unable to follow this series of lectures. Way to difficult. In
    fact, I skipped two out of four lectures because I figured out it will make
    more sense to work on homework assignments for other lectures.

  - _Program-specific proof automation_ by Adam Chlipala. Unsurprisingly to
    those who know Adam's _"Certified Programming with Dependent Types"_ book,
    his course focused on proof automation using Ltac. One lecture was
    specifically dedicated to proofs by reflection.

  - _Verified Functional Algorithms_ by Andrew Appel. This course covered a
    majority of third volume of new Software Foundations.

Summary
=======

First and foremost let me say this: DeepSpec Summer School was the best research
meeting I have ever attended. The courses were really good and inspiring, but
the most important thing that made this summer school so great were fantastic
people who attended it. Spending evening hours together working on homework
assignments was especially enjoyable.

There might be a 2018 edition of the summer school so be on the lookout - this
is a really great event for anyone interested in Coq and formal verification.

