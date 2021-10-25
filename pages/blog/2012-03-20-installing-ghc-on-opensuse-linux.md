---
title: Installing GHC on openSUSE Linux
date: 2012-03-20
---

Installing GHC on openSUSE Linux
================================

The first thing you do when starting out with a new programming language is of
course installing a compiler and an IDE. Although this first step should be
easy, it took me a lot of work to get Haskell running on my openSUSE Linux. The
solution to my problems turned out to be obvious, as all solutions are when you
already know them. Nevertheless, 5 hours of my life went to waste and all I can
hope for right know is saving others from repeating my mistakes.

There are two main Haskell compilers: [GHC](http://www.haskell.org/ghc/)
(Glasgow Haskell Compiler) and [Hugs](http://www.haskell.org/hugs/). I decided
to use GHC, as it seems to be the more popular one. As a Linux user you have
three options of installing GHC:

  1. Use the package repository
  2. Install [precompiled binaries](http://www.haskell.org/ghc/download)
  3. Compile from source

The third option is not really a sane option - GHC is written in Haskell so you
would need working Haskell compiler to compile it. The first option seems to be
the most natural for Linux user. Don't we like to have our software managed from
one central package management application? Good news: there is [Haskell
repository for openSUSE](http://download.opensuse.org/repositories/devel:/languages:/haskell/).
However, after I installed GHC and some libraries from this repo it turned out
the the compiler doesn't recognize the libraries. Solution: **for each library
install both the normal and the devel package**. Obvious, isn't it? Well, it
wasn't obvious to me at first and I still think it's a bit tricky. You install
the compiler (which means that you most likely want to compile programs), YaST
resolves the dependencies by installing additional GHC libraries, but compiler
doesn't have access to these libraries (since devel packages were not installed)
and in the end GHC is totally useless. Where's the logic?

There is one more catch with the repo. Haskell community offers a web repository
of additional Haskell packages called [Hackage](http://hackage.haskell.org/).
Not every package available via Hackage is also available in the openSUSE
repository. Most notably [Leksah](http://leksah.org/), the Haskell IDE, is not
available. So what now?  Luckily with Haskell comes [Cabal](http://www.haskell.org/cabal/)
(Common Architecture for Building Applications and Libraries). Cabal is a
package manager that manages packages available to GHC. It resolves package
dependencies, downloads packages from the web, compiles and installs them. This
however leads you to mixing two ways of managing GHC libraries. Some libraries
will be managed by Cabal and some by YaST (or other rpm/deb management tool if
you're using different brand of Linux). This, at some point, may cause problems
which leads us to the last of three installation option: using precompiled
binaries. If you choose this option, carefully to chose which GHC version to
download. The most obvious choice is using the latest available version, right?
Wrong! You see, there is a thing called [The Haskell Platform](http://hackage.haskell.org/platform),
which is a set of basic Haskell libraries required for development. New
versions of Haskell Platform are released twice a year and each version is
compatible with a particular version of GHC (most likely not the latest one). At
this moment the latest version of GHC is 7.4.1, but the latest Haskell Platform
supports GHC 7.0.4. If you're a newbie, just like me, it is very reasonable to
set up your basic Haskell environment by installing the Haskell Platform.

So, my advice on installing GHC on openSUSE (and other Linux distros) is:
**don't use the repo, use the precompiled binaries**. Instead, get the Haskell
Platform, get the supported GHC version, install GHC, install Platform and then
fell free to use Cabal to install any additional libraries from Hackage.

**UPDATE (06/06/2012):** The installation method described above can be
enhanced.  Read [this](2012-06-06-upgrading-haskell-platform-on-opensuse)
follow-up for more information.

