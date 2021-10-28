---
title: Upgrading Haskell Platform on openSUSE
date: 2012-06-06
---

Upgrading Haskell Platform on openSUSE
======================================

New version of [Haskell Platform](http://hackage.haskell.org/platform/) has been
released just a few days ago. It ships with the latest stable version of GHC
(7.4.1). [Here](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/release-7-4-1.html)
you can find release notes describing changes made to the compiler. The list is
long and I haven't read all of it but among the most important changes are:

  * the possibility of entering any top-level declarations in GHCi;
  * `Num` type class no longer has `Eq` and `Show` as its superclass;
  * Data Parallel Haskell has been improved

Three months ago I wrote about [installing Haskell Platform on
openSUSE](/posts/2012-03-20-installing-ghc-on-opensuse-linux.html).  I
recommended that GHC be installed from precompiled binaries and the platform be
installed from sources, instead of using packages from repository. Now that the
new version is out this post needs an addendum about updating the platform. If
the Platform was installed from the repo using a package manager everything
would be simple[^1].  An update of packages would be enough, providing that they
were updated by the maintainers of the repository (at the moment packages for
openSUSE still contain older version of the platform). With manual installation
this process is a bit more difficult.

First step is to remove the old installation. I figured out that it would be
good to first remove all the packages installed with cabal and then remove GHC.
There's a problem though. Cabal doesn't have uninstallation feature. This means
that each package has to be manually unregistered using ghc-pkg and then all the
files belonging to that package have to be removed. After spending about 30
minutes trying to figure out why I can remove one package using

```
ghc-pkg list | grep -v "^/" | sed -e "s/\[ {}\]//g" | head -n 1 | xargs ghc-pkg --force unregister
```

but can't remove all the packages using

```
ghc-pkg list | grep -v "^/" | sed -e "s/\[ {}\]//g" | xargs ghc-pkg --force unregister
```

I gave up and decided to simply remove all of GHC files. This wasn't easy since
they were scattered all over `/usr/local/``{bin,lib,share,doc}`, but in the end
I managed to remove everything.

I noticed that there is a lot of discussion in the community whether packages
installed with cabal should go to `/usr/local` or to user's home
directory. Surprisingly to me it seems that most people follow the home
directory approach. This approach doesn't suit me completely. I have a separate
home partition used only to store settings and email - which I've been told is a
"complex partition setup" :-o - and all the software is kept on `/` partition,
with all programs not installed from the packages being placed in `/usr/local`
(BTW. it would be nice to have a separate partition for that one
directory). This approach certainly wouldn't work in a multi-user environment
and I guess it could be problematic if I developed many projects, each with
different dependencies
([cabal-dev](http://corp.galois.com/posts/2010/12/20/cabal-dev-sandboxed-development-builds-for-haskell.html)
aims to solve that problem). As a side note, it seems to me that with hundreds
of packages available from Hackage and a management tool with rather limited
capabilities (cabal can't even automatically update installed packages!) Haskell
community is in a place where Linux community was over ten years ago. The
dependency hell, now gone from Linux, looms over Haskell world and if cabal
won't be enhanced I see this as a very huge problem hindering large Haskell
projects. It seems that Yesod team is particularly concerned about this - see
[here](http://www.yesodweb.com/posts/2012/04/cabal-meta) and
[here](http://www.yesodweb.com/posts/2012/03/cabal-nirvana).

Anyway, I decided to place my new installation of the platform in `/usr/local`,
but this time I was smarter by placing everything in a dedicated directory. Both
GHC and the platform can be installed within a specific path. This is done by
passing `--prefix=/some/path` to configure script. The only trick is that after
installation of the platform `~/.cabal/config` file in the /root directory has
to be edited to point to the directory in which installed packages are to be
placed. Of course, you have to also add the /your/haskell/platform/directory/bin
to the path, so that GHC executables are visible. Now, when the new platform
comes out I can simply remove the directory with the platform and install the
new version. I can also easily control the disk space used by the
installation. This tends to be rather huge. GHC, Platform and packages required
by EclipseFP use 1,8GB of disk space. I also noticed that binaries for programs
written in Haskell are rather large. The biggest one I have, buildwrapper, is
over 50MB. This is caused by the [inclusion of RTS (Run Time System) into the
binary](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/runtime-control.html)
but I wonder what else gets included (or is the RTS that large?).

[^1]: Read [this post](/posts/2012-03-20-installing-ghc-on-opensuse-linux.html),
if you're wondering why I decided not to use the package repository.

