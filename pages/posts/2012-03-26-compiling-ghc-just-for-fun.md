---
title: Compiling GHC. Just for fun
date: 2012-03-26
---

Compiling GHC. Just for fun
===========================

A few days ago [I wrote about installing GHC on openSUSE
Linux](/blog/2012-03-20-installing-ghc-on-opensuse-linux.html).  I stated that
"[compiling GHC from source] _is not really a sane option_". I did that based on
opinion heard from others. In the [next post about
recursion](/blog/2012-03-22-recursion-is-good.html) I complained that "_most of
us programmers (...) follow assume-don't-verify policy_". Oops, I fell into my
own trap! It's time to fix that. Today I decided to compile GHC from
source. Just for fun.

OK, in fact I didn't plan to do it. It just happened :) I went to github.com to
see what projects are developed using Haskell. Finding a [GHC
repository](https://github.com/ghc/ghc) wasn't a big surprise. I cloned the repo
just to check the size of the source. It was 17 MB - not much, really. That was
intriguing, but the description of the repo at github provides a link to
[another GHC source repository](http://darcs.haskell.org/ghc.git/). I cloned
that one and got 85 megs of source (including repo data). That's definitely more
then in the previous case. The next step is running the script that fetches data
from additional repositories (libraries, I guess). This increased the source
size to around 250MB. Having the source I simply could not resist to build it.

First step was running some perl script and then it is a standard configure -
make - make install procedure. First attempt on the building process failed. It
turned out that I was missing header files for ncurses, but configure script
missed that fact. After installing ncurses-devel the build was successful. [GHC
building instructions](http://hackage.haskell.org/trac/ghc/wiki/Building/Hacking)
say:

> You need to configure your build: which things to build, how much optimisation
> to use, whether to build profiling libraries, and so on. If you don't do this,
> then you get _everything_, and it will be _optimised to the hilt_, which means
> **the build will take a Very Long Time**. **This is fine if you wanted to
> build GHC for installation and use**, but not if you're building GHC to do
> some development work on it.

I didn't customize my build and it took 1,5 hour on an iCore7 processor
(2,66GHz). Too bad the build uses only one core, but it reminded of the times
when I was compiling Linux kernel or MPlayer on a 333MHz Celeron - these were
sometimes even longer. After the build finished the source directory grew to
2,5GB. I didn't build the documentation and I skipped the `make install` part
since I wanted only to play around with the building process, not messing up my
system's setup.

Now a few more notes. GHC uses a technique called _bootstraping_. It means that
the compiler itself is written in Haskell and it needs a Haskell compiler to
compile (I think that's the main reason why compiling from source is not
suitable for newbies). The build itself is [divided into
stages](http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture/Idiom/Stages).
Stage 0 is the compiler present in the system. Stage 1 is the first build of the
new compiler that is later used during stage 2 to compile packages and build the
compiler once more. Documentation explains the reason for rebuilding the
compiler in the second stage in a way that is not yet fully clear to me:

> Stage 1 does not support interactive execution (GHCi) and Template
> Haskell. The reason being that when running byte code we must dynamically link
> the packages, and only in stage 2 and later can we guarantee that the packages
> we dynamically link are compatible with those that GHC was built against
> (because they are the very same packages).

Generally compilation turned out to be relatively easy, although I think that
people not developing the compiler (and newbies especially) should just rely on
the binaries provided on the GHC site.

