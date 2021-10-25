---
title: Don't panic! It's only an upgrade
date: 2013-02-15
---

Don't panic! It's only an upgrade
=================================

Time for another upgrade of my GHC installation. OK, I know I already posted
about this twice but yet again the process was different from the previous ones.

My [first attempts of installing GHC and the Haskell
Platform](2012-03-20-installing-ghc-on-opensuse-linux/) a year ago relied on
using packages from my distribution's repository. This quickly turned out to be
problematic so I decided for a direct installation of the Haskell Platform. This
worked perfectly fine except for the fact that Haskell packages were installed
in different subdirectories of `/usr/local`, which lead to a bit of a mess and
problems with controlling what is installed where (this is useful if you want to
remove a package). So the second time I was installing Haskell Platform [I was
smarter and refined the whole
process](2012-06-06-upgrading-haskell-platform-on-opensuse/).  This time I
confined the installation to a single directory so that both GHC and all the
packages are located in a single, easy to find place.

Yesterday I figured out it would be great to get a new version of GHC. GHC 7.6.1
was released on 6th September 2012 and the updated 7.6.2 version is only two
weeks old. While GHC 7.6.1 has been out for over 5 months it is still not part
of the Haskell Platform [and it won't be for the next three
months](http://trac.haskell.org/haskell-platform/wiki/ReleaseTimetable). That's
too long a wait for me so I decided to send the Platform to `/dev/null` and just
install GHC and its environment from scratch.

My plan to install GHC from precompiled binaries went up the spout:

> This build requires `libgmp.so.3`.

Watwatwat? Now what is that supposed to mean? Previously released binaries
didn't depend on one particular version of `libgmp` library. Of course my system
has `libgmp.so.10` and any attempt to install an older version results in
breaking package dependencies. I downloaded binaries anyway and tried to run
them:

```
[killy@xerxes : ~/ghc-7.6.2/ghc/stage2/build/tmp] ./ghc-stage2 --interactive
 ./ghc-stage2: error while loading shared libraries: libgmp.so.3: cannot open shared object file: No such file or directory
```

OK, so that requirement is true - you need the exact version of `libgmp`. So
what now? I know! Compilation from sources! I've been hacking on GHC recently so
I already have sources on my drive. Unfortunately it turned out that after
switching GHC repo and all its subrepos to `ghc-7.6` branch I get some
compilation errors. I wasn't in the mood for debugging this so I switched
everything back to master and [downloaded the source
snapshot](http://www.haskell.org/ghc/dist/stable/dist/).  From now on things are
easy, assuming that you already have an older version of GHC on your
system. After extracting the sources I copied `$(TOP)/mk/build.mk.sample` to
`$(TOP)/mk/build.mk` (`$(TOP)` refers to directory containing GHC sources) and
uncommented the line `BuildFlavour =` `perf-llvm`. This gives me fully optimized
build using LLVM. Now the compilation:

```
perl boot
./configure --prefix=/usr/local/ghc-7.6.2
make
```

This will build GHC and prepare it for installation in `/usr/local/ghc-7.6.2`.
Fully optimized build takes much over an hour on all 4 cores. After the build is
done all one needs to do is run `make install` as root. At this stage old GHC
can be removed. You of course need to add `/usr/``local/``ghc-7.6.2/bin` to
`PATH` environmental variable. As I already have mentioned I have the habit of
installing all the packages system-wide in a single directory. For that I need
to edit `/root/.cabal/config` file by adding the following entry:

```
install-dirs global
    prefix:/usr/local/ghc-7.6.2
```

All that is left now is installing
[cabal-install](http://hackage.haskell.org/package/cabal-install).  Grab the
sources from hackage, extract them and run (as root) `sh bootstrap.sh --global`
in the source directory.  This installs cabal-install with its dependencies. Now
you can start installing other packages that you need (a.k.a. compile the
World).

This completes Yet Another Installation of GHC.

