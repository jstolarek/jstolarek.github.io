---
title: Configuring GHC development environment - tips & tricks
date: 2013-12-23
---

Configuring GHC development environment - tips & tricks
=======================================================

I started working on GHC about a year ago. Since then I learned how important it
is to organize my working environment to relieve me of doing common and
repeatable tasks. In this post I describe some of my configuration and
practices. If you prefer reading code than lengthy blog posts you can go
directly to [this github repo](https://github.com/jstolarek/scripts) that
contains my scripts and configuration.

*nix for the win
================

First and foremost I am using Linux on all of my machines. Debian is my distro
of choice, but any *nix based system will do. That said I believe things I
describe below can't be done on Windows. Unless you're using Cygwin. But then
again if you work under Cygwin then maybe it's time to switch to Linux instead
of faking it?

Sandboxes
=========

One thing I quickly learned is that it is useful to have access to different
versions of GHC and - if you're working on the backend - LLVM. It is also useful
to be able to install latest GHC HEAD as your system-wide GHC installation. I
know there are tools designed to automate sandboxing, like
[hsenv](https://github.com/Paczesiowa/hsenv), but I decided to use [sandboxing
method described by
Edsko](http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes/). This
method is essentially based on setting your path to point to certain symlinks
and then switching these symlinks to point to different GHC installations. Since
I've been using this heavily I wrote a script that manages sandboxes in a neat
way. When run without parameters it displays list of sandboxes in a fashion
identical to `git branch` command. When given a sandbox name it makes that
sandbox active. It can also add new and remove existing sandboxes. It is even
smart enough to prevent removal of a default sandbox. Finally, I've set up my
`.bashrc` file to provide auto-completion of sandbox names. Here's how it looks
in practice (click to enlarge):

[![ghc-sandbox](/images/blog/ghc-sandbox-300x129.png)](/images/blog/ghc-sandbox.png)

Scripting for the win
=====================

This is probably obvious to anyone working under Linux: script as much as you
can. If you find yourself doing something for the second or third time then this
particular activity should be scripted. I know how hard it is to convince
yourself to dedicate 10 or 15 minutes to write a script when you can do the task
in 1 minute, but this effort will quickly pay off. I have scripts for pulling
the GHC source repositories (even though I do it really seldom), resetting the
[GHC build
tree](https://ghc.haskell.org/trac/ghc/wiki/Building/Using#Sourcetreesandbuildtrees),
starting tmux sessions and a couple of other things.

Environment variables
=====================

In the beginning I wrote my scripts in an ad-hoc way with all the paths
hardcoded. This turned out to be a pain when I decided to reorganize my
directory structure. The moral is: define paths to commonly used directories as
environment variables in your shell's configuration file (`~/.bashrc` in case of
bash). Once you've done that make your scripts dependent on that variables. This
will save you a lot of work when you decide to move your directories
around. I've also defined some assertion functions in my `.bashrc` file. I use
them to check whether the required variables are set and if not the script fails
gracefully.

Auto-completion
===============

Bash has a built-in auto-completion support. It allows you to get
auto-completion of parameters for the commonly used commands. I have
auto-completion for cabal and my sandbox management scripts. When GHC 7.8 comes
out it will have support for auto-completion as well.

Emacs
=====

I use Emacs for development despite [my initial
scepticism](2012-05-13-haskell-ide-emacs/).  Since [configuring Emacs is a
nightmare](2012-11-05-configuring-emacs-is-a-nightmare/) I started a [page on
GHC wiki](https://ghc.haskell.org/trac/ghc/wiki/Emacs) to gather useful tips,
tricks and configurations in one place so that others can benefit from
them. Whatever editor you are using make sure that you take as much advantage of
its features as possible.

Firefox
=======

GHC wiki [describes how to set up Firefox to quickly find tickets by
number](https://ghc.haskell.org/trac/ghc/wiki/BrowserTips). Use that to your
benefit.

Make
====

Geoffrey Mainland managed to convince me to use `make` and I thank him for that.
Makefiles are a great help if you're debugging GHC and need to repeatedly
recompile a test case and possibly analyse some Core or Cmm dumps. Writing the
first Makefile is probably the biggest pain but later you can reuse it as a
template. See [here](https://gist.github.com/jstolarek/8095793) for some example
Makefiles I used for debugging.

Summary
=======

The goal of this post was to convince you that spending time on configuring and
scripting your GHC development environment is an investment. It will return and
it will allow you to focus on important things that really require your
attention. Remember that most of my configuration and scripts described in this
post is [available on github](https://github.com/jstolarek/scripts).

