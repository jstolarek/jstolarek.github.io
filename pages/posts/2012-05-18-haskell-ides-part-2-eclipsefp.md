---
title: Haskell IDEs, Part 2&#58; EclipseFP
date: "2012-05-18"
---

Haskell IDEs, Part 2: EclipseFP
===============================

In [the previous post](/posts/2012-05-13-haskell-ide-emacs.html) I wrote about
using Emacs as a Haskell development environment.  Today we'll take a look at
[EclipseFP](http://eclipsefp.github.com/), a plugin that enhances Eclipse with
Haskell support. Let's begin with a brief overview of Eclipse.

If you ever programmed in Java than you almost certainly know Eclipse. This is
one of the most popular IDEs for that language and in my personal opinion it's
the best one, much better then crappy NetBeans. Originally it was a project
started by IBM, but it's currently managed by the Eclipse Foundation. Eclipse is
written in Java, uses native OS look-and-feel thanks to SWT (Standard Widget
Toolkit) and has modular architecture that allows it to be extended with
plugins. There are a lot of plugins for Eclipse, just as there are many
extensions for Emacs, e.g. they allow to extend basic installation with support
for version control systems or integrate with bug tracking and team
collaboration software. Most importantly however plugins allow to use Eclipse as
an IDE for languages other than Java (among them Haskell[^1]). When I learnt
Eclipse years ago it greatly boosted my efficiency as a programmer. Great
keyboard shortcuts, good Test Driven Development support, refactoring and code
generation capabilities allowed me to focus on important things relieving me of
many repeatable tasks. There are also some downsides, one of them being the
plugins: install too many and Eclipse will get cluttered with tens of additional
options and menus. This is not a major problem though and when I start with a
new programming language I usually begin by searching for Eclipse plugin (I
admit that recently I also check Emacs extensions). Since I'm very used to
Eclipse, with the learning curve already far behind me, I'm biased towards it.

Let's see what EclipseFP has to offer. Installation turned out to be tedious,
since the plugin requires many additional libraries from Hackage. If you already
have them - and chances are that you have Hoogle, HLint or QuickCheck - this
will be easier, but I had none, so it took some time (nothing complicated
though). First problem appeared right after installation. Each time I started
Eclipse, Hoogle reported that it has no database and asked if it should be
built. Telling Hoogle that it should do it gave no result and the message
reappeared each time Eclipse was started. It turned out that the database
couldn't be built because of permissions. Hoogle was installed system-wide and
required root privileges to write the database somewhere in /usr directory,
while Eclipse was run as normal user. Building the database as root from the
command line solved the problem.

[List of EclipseFP features](http://eclipsefp.github.com/features.html) is long:
syntax highlighting, autocompletion (could be better though), HLint and Hoogle
integration, GHCi integration, cabal file editor, profiling support, exporting
Haddock documentation - to name some of them.  You can of course benefit from
standard features of Eclipse as well as other plugins, e.g. using SVN or GIT
integration in your Haskell project. Here's a glimpse at EclipseFP in action:

<div class="thumbnail">
<figure>
[![](/images/posts/eclipsefp1.png)](/images/posts/eclipsefp1.png)
</figure>
</div>

No much surprise here: editor with multiple tabs, overview of project structure
on the left, GHCi at the bottom. The only thing you may find odd is lack of
menus and toolbars. These were disabled by a plugin that allows Eclipse to run
in fullscreen mode to use as much screen space as possible. Even with this
plugin enabled and editor window maximized (panes at bottom and left can be
closed) there's still less code on the screen comparing to Emacs. Emacs has 37
lines, EclipseFP and Leksah have 32 lines, but that of course depends strongly
on your settings. Syntax highlighting could be better in EclipseFP. Both Emacs
and Leksah offer more. An important thing to note is very good cabal build
system integration. First of all, cabal file editor is non intrusive, so if you
edit your files by hand, then use a graphical frontend, and then go back to hand
editing you won't notice anything (( As you'll see in the future post this is
not the case with Leksah )). EclipseFP also notices missing build dependencies
in the cabal file and can automatically add them for you.

Concluding, EclipseFP nicely integrates Haskell development within Eclipse
framework. If you ever developed Java and enjoyed Eclipse, then you must try out
EclipseFP. I found EclipseFP much easier to use than Emacs for developing a
larger project, most likely because I have many years of Eclipse experience
vs. couple of weeks of Emacs experience. I still prefer Emacs to code some
simple stuff that fits in one file, but I'm not giving up on Emacs as a project
development environment (( I installed some more plugins (thanks to people who
commented on my previous post), so expect a follow-up )). Some things in
EclipseFP could be improved - syntax highlighting is certainly one of them - but
I haven't found anything that I would consider a serious drawback. One thing
that concerns me is the fact that EclipseFP is at the moment a single-person
project (it is developed by [JP
Moresmau](http://jpmoresmau.blogspot.com/)). Currently the development cycle
seems to be quite stable, but I learnt that such projects can die instantly when
the sole developer abandons them. It's a pity there is no community around this
project. I think this can be caused by the fact that it's aimed at Haskell
programmers, but you actually have to be a decent Java developer with solid
knowledge of Eclipse RCP to contribute. I hope that I am wrong and no such thing
will happen, since EclipseFP seems to be the most reasonable choice for people
who want to develop Haskell but don't fancy learning Emacs.

[^1]:  In case you're wondering, there are plugins for other functional languages:
Scala (it's a mature plugin and works decently), Erlang (this plugin on the
other hand was quite buggy and I had to remove it), Clojure and Scheme (I use
Emacs for LISP so I don't know about these two).

