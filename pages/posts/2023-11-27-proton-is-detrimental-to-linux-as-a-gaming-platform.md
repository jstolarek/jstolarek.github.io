---
title: Proton is detrimental to Linux as a gaming platform
date: 2023-11-27
description: In the early 2010s Linux was on a good path towards being
    recognized as a first-class gaming platform.  Sadly, the appearance of
    Proton killed native game ports and reversed the trend of supporting Linux
    by game developers.
---

Proton is detrimental to Linux as a gaming platform
===================================================

The old days
------------

I have this vivid memory stuck in my head.  It is late 2010, right at the start
of a cold Polish winter.  It is dark and windy outside.  I am sitting at my
then-girlfriend's room with a laptop and playing [Revenge of the
Titans](https://www.puppygames.net/revenge-of-the-titans/), a fast-paced tower
defence indie game with a good sense of humour.  Most importantly, I am playing
on Linux and the game runs natively.

I bought Revenge of the Titans as part of Humble Indie Bundle 2, a gaming
initiative aiming at rising popularity of indie games, which back in 2010 were
considered more of a curiosity rather than serious gaming experiences.  Aside
from the already mentioned Revenge of the Titans, Humble Indie Bundle 2
consisted of five games - Machinarium, Osmos, Cortex Command, and, possibly the
most famous of them all, Braid - that could be purchased by paying as much (or
as little) as one wanted.  Perhaps most surprising of all, the games were
available for all desktop platforms (Windows, Linux, macOS) and were DRM-free.
This was groundbreaking and for me, as a Linux user, being recongnized as a
first-class customer was a big deal.

What did gaming on Linux look like in 2010?  Well, it wasn't great.  It was rare
to see a major title ported to Linux, though it did happen.  Id Software was at
the forefront of providing quality Linux ports for their games.  In fact, one of
the reasons I switched from using Windows to Linux around 2004 was that on my
dated PC Quake III performed better on Linux than it did on Windows.  I really
want to stress this: **frame rates were higher on Linux!** In 2010 indie games
had not gained popularity yet, but a handful of titles that existed often had
Linux ports.  Finally, there was Wine, which allowed launching some of the
Windows games.

The fact that Humble Indie Bundle provided funding for the developers to port
their games to Linux and macOS[^1] felt like an important step towards adopting
Linux as a first-class gaming platform.  The future of Linux gaming seemed
bright.

How Proton eliminated native Linux builds
-----------------------------------------

Fast-forward 13 years to 2023.  What does Linux gaming look like today?  At
first glance it might seem things got better.  Last year Valve, owner of the
world's most popular gaming platform Steam, released Steam Deck, a Linux-based
portable PC.  For years [Valve has pushed for the ability to run Windows games
on Linux](https://old.reddit.com/r/linux_gaming/comments/ri2nb4/valve_seems_to_do_its_best_to_release_games_on/hov609f/).
To this end in 2018 Valve released
[Proton](https://github.com/ValveSoftware/Proton), a fork of Wine with a custom
set of patches and a bundle of extra libraries such as
[dxvk](https://github.com/doitsujin/dxvk).  Proton ships with Steam Client and
can be used on Linux to run Windows-only games.  There is a set of games that
have been certified to run with Proton, but it is also possible to run games
that received no such certification - you're just not guaranteed they will
actually run.

This might sound good at first, but for me this push for Proton resulted in
degradation of gaming experience on Linux.  First and foremost, **Proton is
closely tied to Steam, and that is a platform I just refuse to use due to its
inherent DRM**.  I like to own things I pay for, and that is not the case with
games purchased on Steam.  I would lie if I said I have never bought a game on
Steam, but I certainly can't remember when was the last time I did.

Secondly, **the existence of Proton became an excuse for the developers not to
publish native Linux ports**.  See
[here](https://www.rockpapershotgun.com/steam-decks-proton-a-total-war-saga) and
[here](https://www.gamingonlinux.com/2022/02/feral-interactive-have-no-plans-to-update-their-linux-ports-for-steam-deck/)
for examples, and [this interview with Ethan
Lee](https://nuclearmonster.com/2021/07/ethan-flibitijibibo-lee-may-retire-from-programming-due-to-valves-proton/)
for some more insights from an experienced Linux porter.  I can't recall when
was the last time I played a non-indie game natively.  I think it was Alien:
Isolation[^2].  Even indie games, which in the past used to fight for every
customer they can, usually don't have a Linux port these days or deliver that
port a long time after the game's original release.  One good example is Into
the Breach, a highly-acclaimed indie title which was released in February 2018
and had a Linux port over two years later in April 2020.

It seems to me that Valve has gained quite a big following in certain parts of
the Linux community, and that many people consider the current state of affairs
to be a good one.  One of the most commonly seen arguments is: ***"If a game
runs fine on Proton then there is no point in making a native port"*.  The
problem with this argument is that the *if* premise of this statement is
false.** A lot of games do not run well, or at all, with Proton.  Moreover, even
if a game runs fine on the release, there is absolutely no guarantee that it
will stay this way.  In the past there have been many situations when changes
introduced in a game patch or Proton broke a game.

Proton supposedly can be run without a Steam client, which to some extent would
invalidate my earlier criticism of being tied to one particular storefront.
However, despite multiple attempts, I have never succeeded in doing that.  I
tried running Proton standalone, just like I run Wine, as well as using various
frontends such as [Lutris](https://lutris.net/) and
[Heroic](https://github.com/Heroic-Games-Launcher/HeroicGamesLauncher).  Every
single time the only result was me wasting several hours of my time.  Note that
I am somewhat knowledgeable and have experience with things such as compiling
Wine with custom patches applied to run specific games.  I am willing to put the
effort into getting games to run on Linux, but so far I had no success with
Proton and had only wasted my time.

Whining about Wine
------------------

Aside from Proton there is also Wine.  As typical for an open source project, it
is being distributed through distribution repositories, making it trivial to
install and run.  However, gaming experience with Wine is not all that great.

Firstly, Wine suffers from the same problem as Proton, where an update can
completely break a previously working game.  My favourite example is Starcraft
II, a major game that has been on the market since 2010 and not long after its
release was marked as [Platinum in Wine AppDB compatibility
list](https://appdb.winehq.org/objectManager.php?bShowAll=true&bIsQueue=false&bIsRejected=false&sClass=version&sTitle=&sReturnTo=&iId=20882).
Since then it has been broken multiple times either by new game patches or
changes to Wine.  Just take a look at that compatibility list linked above -
some people rate the game as running perfectly, while others mark it as garbage.
I personally was hit by a patch that broke Starcraft II earlier this year and
was only saved by a determined redditor who found an obscure workaround to the
problem.

Windows compatibility that Wine aims for is a moving target.  As such there is
little stability in Wine, and whether things work for you or not is often down
to pure luck.  My personal success rate in the last two years is maybe around
30%.  The most recent game I tried to run was [System Shock
remake](2023-10-01-system-shock-remake.html).  Surely enough, many people report
it as running perfectly out of the box under vanilla Wine, but I had no such
luck and gave up after numerous attempts that took me several hours (around a
third of the time needed to actually finish the game).  This is all despite a
hardware configuration that had no problems running the game under Windows at
60FPS on highest detail settings.  The best I could get under Wine was running
System Shock in slow motion at around 5 frames per second[^3].

Anyway, while Wine is great for preserving old games, it isn't really a good
gaming platform for currently released games.

Better tomorrow was yesterday
-----------------------------

In this post I only scratched the surface of the problem, but I wanted to get my
opinion out there: **in the long term the existence of Proton hurts Linux as a
gaming platform.**

Of course one might ask whether the lack of native ports is really caused by
Valve?  Perhaps Linux is too small a market and porting games to it is not
financially viable?  When writing this post I read several interviews with indie
developers who release their games on Linux and they all consistently claim that
a Linux release, *when done right*, is profitable and not a problem to maintain
(see [here](https://www.forbes.com/sites/jasonevangelho/2019/08/07/porting-games-to-linux-is-a-waste-of-time-this-game-developer-says-youre-doing-it-wrong/?sh=7c313d602c16) and
[here](https://www.tumblr.com/system76/654884924769370112/developing-games-on-linux-an-interview-with)).

One interesting point I came across while doing research for this post was that
having poorly made Linux ports can be even worse than not having them at all
(for example, [this reddit
post](https://old.reddit.com/r/linux_gaming/comments/t2t7ba/a_problem_with_linux_native_ports_and_how_it_may/)).
This is certainly a valid point and I am not arguing for having just any kind of
native ports.  I want to see good ones, such as ports released by Id software in
the early 2000s.

All of this is particularly sad, given that creating a native Linux port was
never as easy as it is today.  Many game engines support Linux out-of-the-box
and the primary thing a developer needs to do is not shoot themselves in the
foot by using Windows-only middleware.  I personally had great experiences with
[Haxe](https://haxe.org/) language and [heaps.io](https://heaps.io/) engine
[during a game jam](https://github.com/jstolarek/ldjam-46), when me and my
friends managed to create builds not only for Windows, Linux, and Mac, but also
a native ARM build for Raspberry Pi.  Granted, heaps.io is not a mainstream game
engine, but it has several great success stories, most famous of which is [Dead
Cells](https://deadcells.com/).  Most importantly, Unity does support Linux.

I do not have much hope that the trend of not supporting gaming on Linux will
reverse anytime soon.  For now I am forced to play on Windows a lot more
frequently than a decade ago, and it feels like a huge step back for Linux.

[^1]: I need to mention Limbo, which was part of Humble Indie Bundle V.  Limbo
      did not have a native port and ran via Wine, which was a source of
      disappointment for many Linux users.

[^2]: Alien: Isolation is one of those rare cases where I bought a game on
      Steam.  Sadly, the [DRM-free release on
      GOG](https://www.gog.com/en/game/alien_isolation) only contains a Windows
      version and no Linux version.

[^3]: By the way, System Shock remake is another example where the developer
      doesn't care about providing a native Linux port, despite it being funded
      as the first stretch goal in the 2016 Kickstarter campaign.  But of course
      that campaign before Proton, and now Nightdive Studios doesn't even bother
      to keep their promise.
