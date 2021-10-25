---
title: Configuring Emacs is a nightmare
date: 2012-11-05
---

Configuring Emacs is a nightmare
================================

When I started using Emacs a few months ago I went through some effort to
configure it to my liking. There were a couple of issues I wasn't able to
resolve back then. I decided that it's time to get them fixed. That's how I
wasted two days doing something that should take no longer than an hour.

About 90% of my time went to configuring Emacs running under
[Tmux](http://tmux.sourceforge.net/). While Emacs runs fine under terminal
emulator (I'm using Konsole for that) it turned out that running it under Tmux
causes numerous problems: some key combinations don't work and even worse the
colours get screwed up. I am using Tmux always when working in console and at
first I wanted to blame Tmux for all the problems. Soon I realized that all
other applications work correct and only Emacs is causing problems. Problems
with key combinations were solved using
[xterm-extras](http://www.dur.ac.uk/p.j.heslin/Software/Emacs/) add-on. I am not
exactly sure what is causing issues with colours but this is related to `TERM`
environment variable. Within Tmux this is set to `screen`, while my terminal
emulator sets it to `xterm`. If I understand correctly Tmux takes care of
translating between these two different modes, but it looks that Emacs is still
affected by this difference. Of course I tried changing values of `TERM` in all
possible ways, but things only got worse. I tried improving Emacs' colours by
using [color-theme](http://emacswiki.org/emacs/ColorTheme) but under my terminal
emulator this breaks completely. I enabled 256 colours (by default there's only
8!), but still I mostly got garbage when applying different themes. Oh well, I
can live with 8 colours just fine. This was day one.

On the next day, when things were more or less working fine, I decided to
upgrade Emacs to version 24.2. I was tempted by Emacs' built in package
management system and a promise of better compatibility with terminal
emulators. This was a mistake. I spent another day trying to figure out why
configuration that worked fine under 23.2 breaks under 24.2.

I acknowledge the fact that if I had arcane knowledge of Elisp all my issues
would be much easier to solve. Without that knowledge I have to resort to
googling and voodoo programming. Well, I am just an ordinary user who just wants
to use a text editor. I need a text-based editor so I can do almost all of my
work using console. Emacs meets that requirement, which makes me more productive
but I seriously doubt that this increase in productivity justifies time needed
to configure editor to my needs. I guess I would be proud of myself had I
managed to solve all the problems. I didn't and there are still unresolved
issues:

  * How can I manage keyboard shortcuts in a sane way? I install some expansion
    only to realize it is not working because keyboard shortcuts collide with
    the ones from different expansion. I can remap the keys, but then again I
    run into risk of colliding with other shortcuts.

  * Can I have a block mode in Emacs similar to one offered by Kate (under KDE)?
    I found that [rect-mark](http://emacswiki.org/emacs/rect-mark.el) allows me
    to mark a rectangle area and cut it, but pasting doesn't work the way it
    should.

  * Emacs' undo is clumsy. I managed to improve it by using
    [undo-tree](http://www.emacswiki.org/emacs/UndoTree), but still it has rough
    edges: every delete is seen as a single operation. If I hold backspace and
    delete 30 characters Emacs sees that as 30 separate operations! Undoing that
    is a real pain.

Solving these issues would make Emacs a very decent editor to work with Haskell
(though not as good as Eclipse is for Java). Perhaps one day I'll find patience
and energy to resolve the remaining problems. Until then I steel myself for
using Emacs the way it is.

