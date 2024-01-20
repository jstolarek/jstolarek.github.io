---
title: Installing X-Reset and DFO mods in my PlayStation
date: 2023-12-18
description: I installed two additional mods in my PlayStation&#58; X-Reset and
  DFO.
---

Installing X-Reset and DFO mods in my PlayStation
=================================================

<center>
**DISCLAIMER:** This post is not intended to serve as installation tutorial.
</center>

I recently wrote about [installing a GCLoader in a GameCube I
own](2023-11-30-installing-a-gcloader-in-my-gamecube.html), but on the same day
I also installed two additional mods in my PlayStation: an [X-Reset
mod](https://www.consolesunleashed.com/product/sony-playstation-x-reset-mod-kit/)
and a [DFO
mod](https://www.consolesunleashed.com/product/sony-playstation-dual-frequency-oscillator-mod-kit/),
both purchased from [Consoles Unleashed](https://www.consolesunleashed.com/).

<div class="thumbnail">
<figure>
[![](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/x-reset-and-dfo_thumbnail.jpg)](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/x-reset-and-dfo.jpg)
<figcaption>X-Reset on the left and DFO on the right.</figcaption>
</figure>
</div>

X-Reset
=======

I already [installed XStation in my PSX](2023-03-35-installing-xstation.html),
which is an optical drive emulator (ODE) that allows launching games from ISO
images stored on an SD card.  While it's super convenient to have all games on a
single memory card, there was a minor usability issue related to using XStation.
Resetting the console to return to XStation's game selection menu required
physically pushing button on the console.  Not a big deal, but somewhat annoying
when you're sitting on a couch and want to quickly test a bunch of games.
X-Reset fixes that by adding button combinations to the controller that allow to
reset both the game (back to the title screen) and the console (back to
XStation's game selection menu).  Installation of X-Reset requires soldering a
small QSB (quick solder board) to the bottom of the motherboard.

<div class="thumbnail">
<figure>
[![](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/x-reset-preperation_thumbnail.jpg)](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/x-reset-preperation.jpg)
<figcaption>Fixing the QSB in place with Kapton tape before soldering.</figcaption>
</figure>
</div>

<div class="thumbnail">
<figure>
[![](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/x-reset-installed_thumbnail.jpg)](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/x-reset-installed.jpg)
<figcaption>X-Reset soldered to the motherboard.  Note a poor solder joint in
the middle.</figcaption>
</figure>
</div>

DFO
===

I have a PAL-region console, where the games were timed to run at 50Hz.
Obviously, playing at 60Hz results in smoother experience and a PAL PlayStation
with XStation installed can actually switch to displaying 60Hz signal when
running NTSC games[^1].  However, when a PAL console runs an NTSC game the GPU
timings are slightly incorrect, resulting in a refresh rate that's off by around
1 percent.  This is rather unnoticeable, but certain games will not work because
of that difference.

This can be fixed with a DFO mod.  DFO stands for Dual Frequency Oscillator.  As
the name suggests, it allows to automatically[^2] supply different GPU timings
depending on the game region, thus ensuring that games run at correct speed both
in 50Hz and 60Hz.

Installing the mod is a bit tricky.  As you can see from the photos below the
mod requires soldering four wires to the motherboard.  That's the simple part.
The difficult part is desoldering a resistor in order to disconnect the GPU from
the original frequency oscillator.  That resistor is located under the RF shield
that covers top of the motherboard.  The RF shield needs to be desoldered and
this was a difficult task.  I was unable to fully desolder it and resorted to
lifting it as much as possible to remove the resistor.  It worked, but I can't
say I felt confident doing that.

<div class="thumbnail">
<figure>
[![](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/rf-shield_thumbnail.jpg)](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/rf-shield.jpg)
<figcaption>That metal plate is the RF shield.  It is fixed to the board with
large blobs of solder that are difficult to remove.</figcaption>
</figure>
</div>

<div class="thumbnail">
<figure>
[![](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/removed-resistor_thumbnail.jpg)](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/removed-resistor.jpg)
<figcaption>I managed to lift the RF shield just enough to remove the resistor.</figcaption>
</figure>
</div>

<div class="thumbnail">
<figure>
[![](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/dfo-installed_thumbnail.jpg)](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/dfo-installed.jpg)
<figcaption>Soldering the DFO board is the easy part.</figcaption>
</figure>
</div>

Summary
=======

Both mods required fairly simple soldering.  I've certainly done more
complicated mods, but this time something went wrong and I really messed up the
solder joints.  They hold, but they don't turned out the way they should.
Either my soldering iron is giving out or I messed up when using rosin, which I
hadn't used previously.

<div class="thumbnail">
<figure>
[![](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/bad-soldering_thumbnail.jpg)](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/bad-soldering.jpg)
<figcaption>Poor quality solder joints.  I fixed them after taking these photos
but they are still not as good as they should be.</figcaption>
</figure>
</div>

<div class="thumbnail">
<figure>
[![](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/all-mods_thumbnail.jpg)](/images/posts/2023-12-18-installing-x-reset-and-dfo-in-my-psx/all-mods.jpg)
<figcaption>Bottom of my PlayStation's motherboard.  The large QSB on the left
is from previously installed XStation.</figcaption>
</figure>
</div>


[^1]: I am committing a crime here by equating PAL with 50Hz refresh rate and
      NTSC with 60Hz refresh rate.  There is a common misconception that PAL and
      NTSC standards prescribe a particular refresh rate, but that is not the
      case.  Both PAL and NTSC are colour coding standards and nothing else.
      They do not define refresh rates.

[^2]: I am very curious how that actually works.  How does a simple mod
      connected with only four cables recognize the game region?
