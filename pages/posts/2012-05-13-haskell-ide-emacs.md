---
title: Haskell IDEs, Part 1&#58; Emacs
date: 2012-05-13
---

Haskell IDEs, Part 1: Emacs
===========================

I've been working with Haskell for about 4 months now and I already have a bit
of experience with three Haskell development environments: Emacs,
[EclipseFP](http://eclipsefp.github.com/) and [Leksah](http://leksah.org/). I
wanted to give an overview of these three, but this turned out to be a lot of
writing so I decided to split this up into three separate posts, each describing
one IDE. We'll begin with Emacs.

Before I go into details of developing Haskell using Emacs let me write about
Emacs itself. Emacs is a text-based (i.e. no GUI i.e. not WYSIWYG) text-editor
for \*nix systems. I stayed away from it for years, but when I started with
Haskell and Lisp I realised that a lot of people regard Emacs as the best editor
for these languages. I decided that it will be best to learn it and see for
myself if it's really that good.

Emacs has extremely steep learning curve. It is (un)famous for its keyboard
shortcuts and I must say that this fame is well-deserved. First of all they are
not intuitive for people that are used only to modern GUIs. Forget about
Ctrl+C/Ctrl+V to copy-paste. Prepare to use C-w / C-y (( In Emacs convention C
denotes Ctrl key, M denotes Alt key. C-w means pressing Ctrl and 'w'
simultaneously, C-M-q means Ctrl+Alt+q. )) instead. That's very subjective
matter and after couple of weeks with Emacs these become quite intuitive. The
more objective problem with shortcuts is the fact that they are overcomplicated
and extremely hard to type. Unfortunately there's a lot of examples: C-x C-s is
required to save a file and nobody is going to convince me that this is better
than simply C-s (this is one of the basic commands, you do it once every few
minutes). Moving the cursor is even more insane: C-f / C-b to move it left and
right (f denotes forward, b denotes backward), while C-p / C-n moves it to
previous and next line respectively. OK, mnemonics are nice, but look where
these keys are on the keyboard! That's completely ridiculous, especially that
most of us doesn't care about mnemonics because we type without looking at the
keyboard anyway. That said, editing code in Emacs can be faster than in
graphical text editor. How come? Thanks to the keyboard shortcuts, but not
because they are good, but simply because they exist. Graphical editors usually
don't have that many shortcuts and even if they do they discourage us from using
them since everything can be selected from the menu. The problem is that using a
mouse is slower than even the most crazy key combinations (my favourite is
C-M-Space-u - that's four keys at once). Still, Emacs could be a whole lot
better if someone made a bold step to redesign its prehistoric shortcuts to
something more ergonomic and adjusted to keyboards of today ([Emacs shortcuts
were designed with different keyboards in
mind](http://xahlee.org/emacs/emacs_kb_shortcuts_pain.html)).

Another distinguishing feature of Emacs is the variety of different plugins
(called 'extensions'). They enhance the basic features of the editor with
anything you can possibly imagine, including reading and sending email. There
are two extensions that enhance Haskell editing in Emacs: [Haskell
mode](http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs) and
[ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/). Initially I used only the
former one. It gives you nice code highlighting (IMO better than EclipseFP and
Leksah), provides some basic code templates (guard insertion, where keyword and
few more), possibility of starting a Haskell interpreter and interacting with
it, e.g. executing code from the editor or automatic insertion of type
annotations. However, I wasn't able to get some of the features running. I'm not
an Emacs hacker and I don't have idea why they didn't work out of the box,
though I tried to change keyboard shortcuts of the non-working commands hoping
that they will work.

Haskell mode offers great indentation management: it has four of possible
'indentation policies' and you can cycle between them using TAB key. This works
great in practice. There are a few more neat features of Haskell-mode, like
support for automatic unit testing or Hoogle integration, but I haven't used
them. Here's a screenshot of Haskell code in Emacs with Haskell-mode enabled:

<div class="thumbnail">
<figure>
[![](/images/posts/emacs-1024x640.png)](/images/posts/emacs.png)
</figure>
</div>

A nice thing in Emacs is that it doesn't have any toolbars (except for the menu
at the top, but it can be switched off), so when you set your terminal emulator
to fullscreen mode you can see quite a lot of code at once. Another feature of
Emacs that comes in handy is ability to align text according to a given
regexp. This is very useful for aligning haddock comments - you can see an
example on the screenshot.

After using Haskell-mode for a while I decided to give ghc-mod a try. These two
extensions complement one another. Ghc-mod adds the possibility of code checking
using either GHC or HLint, completing keywords, inserting code templates and
inferred types of expressions. Unfortunately, most of the features of ghc-mod
didn't work OOTB and I couldn't make them work. One of the features that
unfortunately worked was code checking. It turned out to be a serious problem
when I realised that the offending code is highlighted with some horrible
background and this highlighting doesn't disappear until the problem is
fixed. Here's an example:

<div class="thumbnail">
<figure>
[![](/images/posts/ghc-mod-crap-1024x640.png)](/images/posts/ghc-mod-crap.png)
</figure>
</div>

Well, this was the end of my adventure with ghc-mod.

To sum up, Emacs offers nice syntax highlighting, very good indentation
management and support for basic operations. The biggest downside might be an
extremely steep learning curve. I enjoyed Emacs when I was learning the basics
and working with only one file at a time, but when I started a small project
that consisted of multiple files working in Emacs became a nightmare. Switching
between multiple files turned out to be very annoying, though this is probably
something you can get used to providing you have sufficiently strong will. I
don't, so I started looking for a full featured IDE. Occasionally you'll hear
hackers claiming that IDEs are completely unnecessary and Emacs combined with
\*nix shell will do just fine. Perhaps this is true for hackers and while I
appreciate power of my Linux shell, I still prefer to have a dedicated
development environment as most ordinary mortals do. The next post will cover
one of them: [EclipseFP](http://eclipsefp.github.com/).

