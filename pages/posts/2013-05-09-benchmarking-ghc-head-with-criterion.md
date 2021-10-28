---
title: Benchmarking GHC HEAD with Criterion
date: 2013-05-09
---

Benchmarking GHC HEAD with Criterion
====================================

So you're developing GHC. You make some changes that affect performance of
compiled programs, but how do you check whether the performance is really
improved? Well, if you're making some general optimisations - a new Core-to-Core
transformation perhaps - than you can use the
[NoFib](http://hackage.haskell.org/trac/ghc/wiki/Building/RunningNoFib)
benchmark suite, which is a commonly accepted method of measuring GHC
performance. But what if you're developing some very specific optimisations that
are unlikely to be benchmarked by NoFib? What if you extended the compiler in a
way that allows you to write faster code in a way that was previously impossible
and there is now way for NoFib to measure your improvements? Sounds like writing
some [criterion](http://hackage.haskell.org/package/criterion) benchmarks would
be a Good Thing. There's a problem though - installing criterion with GHC
HEAD. Criterion has lots of dependencies, but you cannot install them
automatically with cabal-install, because cabal-install usually doesn't work
with GHC HEAD (although the Cabal library is one of GHC boot libraries). On the
other hand installing dependencies manually is a pain. Besides, many libraries
will not compile with GHC HEAD. So how to write criterion benchmarks for HEAD? I
faced this problem some time ago and found a solution which, although not
perfect, works fine for me.

In principle my idea is nothing fancy:

  1. download all the required dependencies from hackage to the disk and extract
     them in a single directory,
  2. determine the order in which they need to be installed,
  3. build each library with GHC HEAD, resolving the build errors if necessary
  4. register each library with GHC HEAD (see Appendix below)

Doing these things for the first time was very tedious and took me about 2-3
hours. Determining package dependencies was probably the most time
consuming. Resolving build errors wasn't that bad, though there were a couple of
difficulties. It turned out that many packages put an upper bound on the version
of the base package and removing these dependency is the only change required to
build that package.

The key to my solution is that once you figure out in what order packages should
be installed and remove the build errors, you can write a shell script that
builds and installs packages automatically. This means that after installing GHC
HEAD in a sandbox (see Appendix below) you can run the script to build and
install all the packages. This will give you a fully working GHC installation in
which you can write Criterion benchmarks for new features that you implemented
in the compiler. Here's what the script looks like (full version available
[here](https://gist.github.com/jstolarek/5546184)):

```bash
#!/bin/bash

PKGS="\
primitive-0.5.0.1 \
vector-0.10.0.1 \
dlist-0.5 \
vector-algorithms-0.5.4.2 \
..." # more packages in this list

if [[ $# -gt 1 ]]; then
    echo "Too many parameters"
    exit
elif [[ $# -eq 1 ]]; then
    if [[ $1 == "clean" ]]; then
        echo -n "Cleaning"
        for i in $PKGS
        do
            echo -n "."
            cd $i
            rm -rf dist
            rm -f Setup Setup.o Setup.hi
            cd ..
        done
        echo "done"
    else
        echo "Invalid parameter: $1"
        exit
    fi
else
    for i in $PKGS
    do
        echo "Installing package $i"
        cd $i
        ((if [[ -f Setup.lhs ]]; then ghc Setup.lhs; else ghc Setup.hs; fi) && \
            ./Setup configure --user --enable-shared \
            && ./Setup build && ./Setup install) \
            || exit
        cd ..
    done
fi
```

The script is nothing elaborate. Running without any parameters will build and
install all packages on the list. If you run it with "`clean`" parameter it will
remove build artefacts from package directories. If for some reason the script
fails -- e.g. one of the libraries fails to build - you can comment out already
installed libraries so that the script resumes from the point it previously
stopped.

Summary
=======

Using the approach described above I can finally write criterion benchmarks for
GHC HEAD. There are a couple of considerations though:

  - things are likely to break as HEAD gets updated. Be prepared to add new
    libraries as dependencies, change compilation parameters or fix new build
    errors,

  - since some time you need to pass `--enable-shared` flag to `cabal configure`
    when building a shared library. This causes every library to be compiled
    twice. I don't know if there's anything one can do about that,

  - you need to manually download new versions of libraries,

  - fixing build errors manually may not be easy,

  - rerunning the script when something fails may be tedious,

  - changes in HEAD might cause performance problems in libraries you are
    using. If this goes unnoticed the benchmarking results might be invalid (I
    think this problem is hypothetical).

You can download my script and the source code for all the modified packages
[here](http://ics.p.lodz.pl/~stolarek/posts/downloads/ghc-head-pkgs.tar.gz). I'm
not giving you any guarantee that it will work for you, since HEAD changes all
the time. It's also quite possible that you don't need some of the libraries I'm
using, for example [Repa](http://hackage.haskell.org/package/repa).

Appendix: Sandboxing GHC
========================

For the above method to work effectively you need to have a sandboxed
installation of GHC. There are tools designed for sandboxing GHC
(e.g. [hsenv](https://github.com/Paczesiowa/hsenv)) but I use a method described
[here](http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes/). It's
perfectly suited for my needs. I like to have full manual control when needed
but I also have this shell script to automate switching of sandboxes:

```bash
#!/bin/bash

SANDBOX_DIR="/path/to/ghc-sandbox/"
ACTIVE_SYMLINK="${SANDBOX_DIR}active"
STARTCOLOR="\e[32m";
ENDCOLOR="\e[0m";

active_link_name=\`readlink ${ACTIVE_SYMLINK}\`
active_name=\`basename ${active_link_name}\`

if [[ $# -lt 1 ]]; then
  for i in \`ls ${SANDBOX_DIR}\`; do
    if [[ $i != "active" ]]; then
      if [[ $i == $active_name ]]; then
        echo -e "* $STARTCOLOR$i$ENDCOLOR"
      else
        echo "  $i"
      fi
    fi
  done
  exit
fi

for i in \`ls ${SANDBOX_DIR}\`; do
  if [[ $i == $1 ]]; then
    cd $SANDBOX_DIR
    rm ${ACTIVE_SYMLINK}
    ln -s $1 ${ACTIVE_SYMLINK}
    exit
  fi
done

echo "Sandbox $1 not found"
```

It displays list of sandboxes when run without any parameter (the active sandbox
is displayed in green and marked with an asterisk) and switches the active
sandbox when given a command-line parameter. Together with bash auto completion
feature switching between different GHC versions is a matter of seconds.

