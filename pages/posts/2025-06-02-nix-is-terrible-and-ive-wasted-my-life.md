---
title: Nix is terrible and I've wasted my life
date: 2025-06-02
---

Nix is terrible and I've wasted my life[^1]
===========================================

[Nix](https://github.com/NixOS/nix) is a package manager that derives its basic
principles from functional programming, and has thus gained some traction in the
Haskell community.  My first exposure to Nix was 3 years ago, when I was
assigned to work on Haskell projects that used Nix to manage their build
environment and dependencies.  Since then, I have met people who are very
enthusiastic about Nix.  Unfortunately, **my personal experiences are largely
negative and I find enthusiasm towards Nix unjustified**.  In this post, I want
to talk about Nix problems that are swept under the rug by many Nix enthusiasts
and that make Nix a major liability for a programmer.

Promising theory
----------------

Nix can be used either as a package manager, as is done by
[NixOS](https://nixos.org/) Linux distribution, or as a build environment and
dependency manager in a programming project.  My experience is with the latter,
so I will focus on that[^2].  Let's start by going over the main goals and
(claimed) features of Nix.  Let me quote from the project page (bold text not
mine):

> _Reproducible_: Nix builds packages in isolation from each other. This ensures
> that they are reproducible and don’t have undeclared dependencies, so **if a
> package works on one machine, it will also work on another**. \
> _Declarative_: Nix makes it **trivial to share development and build
> environments** for your projects, regardless of what programming languages and
> tools you’re using. \
> _Reliable_: Nix ensures that installing or upgrading one package **cannot
> break other packages**. It allows you to **roll back to previous versions**,
> and ensures that no package is in an inconsistent state during an upgrade.

When used as a dependency manager in a project, the main problem Nix attempts to
address is build reproducibility: given a project with Nix configuration, that
project should always build on every machine without requiring any extra
configuration[^3].  Nix configuration for a project &mdash; referred to as
_"derivation"_ &mdash; is written in a declarative language that specifies what
the dependencies are and where to find them, say a GitHub repository under a
specific commit hash.  Once a configuration is written, it is possible to launch
Nix shell from inside the project directory.  This shell loads all specified
dependencies, both libraries and executables.  The project can then be built
inside the Nix shell, and it should always build on every machine, regardless of
the operating system or any other factors.

If you have ever worked on a programming project, the above promises sound like
a dream come true.  I mean, no more problems with your code not compiling on
other people's machines because they have different library versions or because
they are missing the required tools.  Who wouldn't that?  Unfortunately, **Nix
demands an exorbitant price for delivering its promises**.  I claim that Nix is
a case of the cure being much worse than the disease itself.

Sad reality
-----------

Unfortunately, Nix comes with a whole bundle of issues, that seriously hinder
development.

The first problem with Nix is that it requires a lot of machine resources.
Firstly, disk space.  As mentioned in the previous section, Nix installs all the
required project dependencies into a so-called _store_.  As you use Nix, the
store grows in size, since many packages are installed multiple times as part of
different derivations.  Nix store on my PC had at one point grown to a whooping
200GB.  That's _two hundred gigabytes_ of libraries.  For comparison, my Linux
installation uses 25GB of disk space and includes a full desktop environment, a
suite of various desktop programs (office suite; graphics, sound, and video
editing; dozens of minor tools), and several development toolchains.  Nix store
can be purged (`nix-collect-garbage`) and compacted (`nix-store --optimise`),
which helps for a while, but it ultimately grows again.  Another machine
resource that Nix happily wastes is RAM memory.  Every Nix shell I launched in a
project I worked on demanded gigabytes of RAM memory.  I need at least two
shells for development, ideally three.  On a machine with 16GB of RAM, launching
three Nix shells would result in running out of RAM about 2&ndash;3 times a
week.  As soon as I started using Nix over three years ago, I had to upgrade to
32GB of RAM.  This mostly suffices, but on several occasions even that wasn't
enough.  Such disk and RAM requirements are not justified, since the same
libraries installed via the package manager do not take up some much space and
do not require so much RAM.

The above problems are annoying, but they can be mitigated with money: you can
always buy more disk space and more RAM memory.  However, Nix has a much more
fundamental problem.  **It wastes the most precious resource, that cannot be
bought for any money: time.**  Launching a Nix shell for the first time in a
given project, requires downloading and building dependencies.  For projects I
worked on, this typically took several hours.  Essentially, a whole day wasted
just to build gigabytes of libraries.  Many times I have heard Nix proponents
say, that this problem can be addressed by setting up binary caches.  These
apparently should allow skipping local compilation by downloading pre-built
libraries.  Despite hearing such claims multiple times, I have never seen such
binary caches set up in a way that mitigates the problem &mdash; and below I
will get to the problem of setting anything up with Nix.  Unfortunately,
compilation of dependencies is not a one-time thing.  Every now and then you
will be rebuilding project dependencies, either because they have changed or
because you garbage-collected your store and the already compiled dependencies
have been removed.  **I estimate that waiting for Nix to compile the
dependencies wasted about 7-8 working days every quarter.** To me, this one
thing in itself entirely disqualifies Nix.

Note that with Nix, you will not only waste your time when launching a Nix
shell, but also when switching between git branches that have different Nix
configurations.  Each switch to a branch with different configuration means
dependency reload.  Such reloads are usually faster than launching the Nix shell
from scratch, but even then calling `git status` after branch switch can require
a 2&ndash;3 minute wait.

Another fundamental problem with Nix is its complexity and the resulting
difficulty of managing a project configuration.  When I was first assigned to
work on a project that used Nix, I was determined to learn Nix.  I mean, I
should learn how to use the tools required in a project, right?  Unfortunately,
despite my best efforts, I never got to a point where I was able to competently
manage the project's dependencies.  Honestly, when I realized, that despite
trying to learn Nix, I am still unable to do basic things with it &mdash;
changing a dependency is a basic thing &mdash; I felt like I'm just stupid.  I
started to ask others for help, and it turned out that this isn't just me: most
programmers I worked with could not figure out how to manage dependencies with
Nix.  Thus, on every single Nix-based project I have been involved with, we had
one or two people who were the "Nix specialists" and whose job was to work on
the Nix configuration whenever it needed to be changed.  To me, as a programmer,
this is unacceptable.  **We can't have tools that are so complex, that they
prevent programmers from changing a project's most basic configuration.**

One final complaint is that Nix does not always deliver on its promises.  Though
it should guarantee that a project always builds on every machine, I have seen
situations where that was not the case.  For example, building Nix dependencies
would fail for someone using a Mac.  I am not going to complain about that,
though.  For most of the time, Nix builds are reproducible.  Even if "for the
most time" is not the same as "always", problems with Nix fundamentally lie
elsewhere.

Cure worse than the disease
---------------------------

I was recently creating a new Haskell project.  Some of the dependencies rely on
specialized cryptographic C libraries not widely available on current Linux
distributions.  Programmers who want to build my project have to install those
libraries from outside their package manager, and also update some path
variables, such as `LD_LIBRARY_PATH`.  Describing the exact process in a README
took me about 15 minutes.  Carrying out those project-specific installation
instructions also shouldn't take longer than 15 minutes.  It might be a slight
annoyance, but it needs to be done *once*.  Project's dependencies are then
managed via project's Cabal file in the usual way, so a programmer needs not to
take any extra steps.

Setting up a project in this way is a lot more time efficient than with Nix.
The price of installing dependencies is paid *once* at the beginning, and it is
a small price.  Or who knows, maybe this initial effort will not be small.  The
point is: without Nix, you pay the price once.  With Nix, you regularly pay a
price that is much higher.  The result is that **no other tool in my entire
programming career has wasted so much of my time**.  I just cannot afford myself
to use Nix if I want to get things done.  And I cannot have project dependencies
managed with a tool that is so complex, it requires dedicated specialists.


[^1]: For the lack of ideas, I borrowed the title from [Super Eyepatch
      Wolf](https://www.youtube.com/watch?v=FpNAKDx4CwY).

[^2]: If you are looking for a more comprehensive coverage of Nix, I highly
     recommend [Nix - Death by a thousand
     cuts](https://www.dgt.is/blog/2025-01-10-nix-death-by-a-thousand-cuts/).

[^3]: Note that Nix [does not provide Reproducible
      Builds](https://linderud.dev/blog/nixos-is-not-reproducible/), i.e. it
      does not guarantee that a build is always binary-identical.
