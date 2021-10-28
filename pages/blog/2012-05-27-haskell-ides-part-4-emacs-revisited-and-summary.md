---
title: Haskell IDEs, Part 4&#58; Emacs revisited and Summary
date: 2012-05-27
---

Haskell IDEs, Part 4: Emacs revisited and Summary
=================================================

In the previous parts of this post I reviewed three different development
environments for Haskell: [Emacs](/blog/2012-05-13-haskell-ide-emacs.html),
[EclipseFP](/blog/2012-05-18-haskell-ides-part-2-eclipsefp.html) and
[Leksah](/blog/2012-05-25-haskell-ide-leksah.html).  This is the last part in
which I plan to write about Emacs once again and then summarize all the reviews.

My main problem with Emacs was inconvenience when working with multiple files,
mostly because I found switching between buffers (i.e. opened files)
annoying. After posting the review there were comments that recommended
extensions that could solve these problems.  Here's the list of extensions that,
after some testing, I decided to use:

  * [IDO mode](http://emacswiki.org/emacs/InteractivelyDoThings). IDO stands for
    Interactively Do Things and it greatly improves file opening and switching
    between buffers. When opening a file it shows the list of files and
    directories in a current directory, allows to navigate the directory tree in
    an easy manner, provides intuitive filtering capabilities and allows to
    select a file easily by selecting its name using arrow keys. Similar
    behaviour is provided when switching between opened buffers. A nice
    introductory tutorial to IDO can be found
    [here](http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/).

  * [NumberedWindows](http://www.emacswiki.org/emacs/NumberedWindows) improves
    switching between multiple windows by pressing the Meta key and a number of
    a window ((Don't get confused by Emacs' terminology. A window in Emacs is an
    area that displays the context of the buffer within a frame. You can split
    editing area horizontally or vertically and each of these areas is called a
    window. Term _frame_ is equivalent to what is normally referred to as a
    window in modern GUIs )). This is much easier to type then C-x o.

  * [CycleBuffer](http://www.emacswiki.org/emacs/CycleBuffer) allows switching
    to next/previous buffer using F9/F10 keys. That's an alternative to
    IDO-enhanced C-x b command.

[![](/images/blog/emacs1-1024x640.png "emacs")](/images/blog/emacs1.png)

These three extensions are a huge win. They greatly improve overall Emacs
experience. There's plenty of other window- and buffer-enhancing extensions that
I haven't mentioned - you can take a look at them on Emacs Wiki
[here](http://emacswiki.org/emacs/CategoryWindows) and
[here](http://emacswiki.org/emacs/SwitchingBuffers).

Right now I'm missing two things in Emacs: a bit tighter integration with
GHC/GHCi and a possibility to browse through the source tree. First problem
should theoretically not be a problem, since haskell-mode and ghc-mod both offer
good integration features but, as I mentioned in my review, some of these
features didn't work as the should or at all. I hope this could be resolved with
a small effort. As for the source tree browsing, there are of course
extensions. One of them is [ECB](http://ecb.sourceforge.net/), but I haven't
tried it yet.

Summary
=======

This series was meant to give you a rough idea of what three Haskell IDEs -
Emacs, EclipseFP and Leksah - have to offer.  As you've seen I was enthusiastic
about EclipseFP and Emacs, but very critical about Leksah. I would however
suggest not to rely solely on my opinion and test each of these environments on
your own. I've seen people on #haskell praising Leksah, so perhaps that project
is not a lost cause (as my post could have suggested). I also made one omission
- vim. There are Haskell modes for vim as well, but I've never been proficient
in it so when I was faced with improving my vim skills or learning Emacs from
scratch, I chose the latter (I think the key factor was the fact that I was
learning Scheme at the same time). As a general thought, I have to say that
there is no Haskell IDE that would boost my efficiency as a programmer the way
Eclipse boosted my Java programming efficiency. I don't know if such IDE will
ever be created, but it would be nice to have at least any kind of refactoring
capabilities. At the moment none of the IDEs has such and the programmer has to
manually perform repetitious tasks like renaming variables. I'm definitely
waiting for this to improve.

