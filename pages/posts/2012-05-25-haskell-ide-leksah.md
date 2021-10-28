---
title: Haskell IDEs, Part 3&#58; Leksah
date: 2012-05-25
---

Haskell IDEs, Part 3: Leksah
============================

In the previous parts of this series I presented two development environments
for Haskell: [Emacs](/blog/2012-05-13-haskell-ide-emacs/) and
[EclipseFP](/blog/2012-05-18-haskell-ides-part-2-eclipsefp.html).  It's time to
finish presentation of IDEs with the last one, [Leksah](http://leksah.org/) - a
Haskell IDE written in Haskell.

Leksah was the first Haskell IDE I tried when I started learning the language. I
was a total newbie back then (not that I know much more now) and the
installation process was very tough going for me. I had all the GHC packages
installed from openSUSE repositories, but it turned out that Leksah is not in
the repo. This meant that I had to install it using cabal and I wasn't very keen
on idea of having GHC installation managed both by zypper[^1] and cabal.  I
described the full story in [one of the previous
posts](/blog/2012-03-20-installing-ghc-on-opensuse-linux.html), but long story
short I removed GHC packages installed from the repo, downloaded pre-compiled
GHC binaries and then installed Leksah using cabal. Previously I said that
installation of EclipseFP can be tedious, but I should have reserved that word
for Leksah. First of all, it's not easy to find any sort of installation
instructions for Leksah. There is [a page on Haskell
wiki](http://www.haskell.org/haskellwiki/Leksah_Installation) and a [user's
manual](http://leksah.org/leksah_manual.pdf) but both are outdated, manual by
about 2 years, wiki even more. Luckily cabal is a smart tool and can manage the
installation process quite well by automatically downloading, compiling and
installing required dependencies. Since my GHC package repository was bare and
Leksah needs a lot of additional dependencies (and I really mean a lot), the
whole installation process took Very Long Time. Unfortunately not everything
went as smoothly as it should. I recall having some problems with gtksourceview,
but #haskell channel came to the rescue and after about 2 hours of struggling
with installation (and 3 hours of installing GHC...) I was ready to finally
start the IDE. Before we go into details about using Leksah I have to add that
upgrading Leksah to newer version was much easier and faster since the
dependencies were already present.

When Leksah starts for the first time it needs to collect information about
packages available in the system. This, again, takes Very Long Time, so you can
get yourself a cup of tea or possibly take a walk. Be prepared to rebuild this
database after installing any new packages or upgrading Leksah. The latter
doesn't happen very often, but we'll get to that later. After drinking your cup
of tea (possibly a few) or coming back from the walk - depending which option
you chose - you're finally ready to work with Leksah. Here's what you get:

[![](/images/blog/leksah1-1024x591.png "Editing Haskell in Leksah")](/images/blog/leksah1.png)

List of major features includes:

  * Syntax highlighting. No surprise here. Well, almost. You can't define your
    own colours for the editor and you're limited to a few predefined
    settings. These colour themes are nice but I don't see why the user can't
    customize them. I also noticed that for some of available settings spaces
    are not the same width as other characters, which breaks the layout of
    haddock comments - see the screenshot above. Aside from that the
    capabilities of Leksah in syntax highlighting are similar to EclipseFP, but
    the latter gives you customization options and doesn't have
    glitches. Anyway, I still think that Emacs wins as far as syntax
    highlighting is concerned.

  * Cabal file editor. While EclipseFP offers a nice, non-intrusive editor,
    Leksah will demolish your hand-crafted cabal file. It will insert hundreds
    of unused sections and completely rearrange an layout you've
    made. Definitely a point for EclipseFP.

  * Automatic compilation and error detection. There is an error pane that shows
    current compilation errors. Nevertheless, another point goes to EclipseFP
    since it is able to mark the errors directly in the source code and it can
    also display HLint warnings. **UPDATE:** Leksah can also underline errors
    directly in the code. My mistake here. Nevertheless EclipseFP still has the
    advantage of HLint integration.

  * Debugging. I still haven't learned how to use Haskell's debugger so I really
    can't tell how this works.

  * Package browser. It allows you to browse through available packages or
    packages in your project, but it's so clumsy that I don't consider it useful
    in any way. EclipseFP wins once again.

  * Autocompletion. This is the only thing that seems to work better in Leksah
    than in EclipseFP. It is more reliable, uses syntax highlighting to display
    completion suggestions and displays information about package from which a
    function is imported. A point for Leksah.

Generally, Leksah is extremely clumsy and not really convenient to work
with. Interface is cluttered, full of bugs and glitches: text labels aren't
displayed properly, it's hard to manage panes layout (not impossible, but hard),
some panes don't show up although they should, some commands from menus and
context menus don't work. The graphical interface is really more of problem in
Leksah than actual aid for the programmer. Just to give you the idea of how poor
an IDE Leksah is: only in the recent version 0.12 (released in March 2012) was
the pane displaying the files in the project added. It is a very simple tree
widget with no icons of any kind, no drag-and-drop features and no context menus
that would allow you to manipulate layout of the project in any way. I couldn't
also find a way to increase the size of the editor in any reasonable way.
There's an option to disable the toolbar and editor tabs, but that's all.  No
way to work in the full screen (( Don't get mislead by the screenshot I
provided.  There's no window decoration on it, but Leksah is really working in a
window, not fullscreen. )), no way to get rid of two status bars.

My attempts to develop code in Leksah were really painful and honestly speaking
I can't find a good reason why should anyone choose it as a Haskell IDE. Leksah
is buggy, inconvenient, annoying and lacks features. I don't expect this to
change quickly, 'cause the development process is very slow. The previous stable
version 0.10 was released in April 2011, the current one in March 2012. That's
eleven months of waiting only to get some minor features that definitely don't
solve Leksah's problems as a whole. Considering what was said above my advice is
simple: if you want a graphical IDE, use EclipseFP.

The only thing that's left in this series of posts is a general summary and a
small addendum about Emacs. Be on the lookout for part 4.

[^1]: zypper is openSUSE's RPM package manager

