---
title: Installing a GCLoader in my GameCube
date: 2023-11-30
description: I finally managed to buy a GCLoader ODE for my GameCube.  Here's a
  quick report from the installation process.
---

Installing a GCLoader in my GameCube
====================================

I got a GameCube towards the end of my stay in the UK, in late 2021.  Right from
the start I wanted to mod it with a
[GCLoader](https://gc-loader.com/product/gc-loader-pnp-hw2/), which is an ODE
(Optical Drive Emulator) that replaces the original DVD drive, but it was
permanently out of stock.  This was due to chip shortage caused by the pandemic,
followed by the FPGA chip used in the GCLoader going completely out of
production.  As a stop-gap solution I had to install a [Xeno
modchip](https://www.consolesunleashed.com/product/nintendo-gamecube-region-free-mod-kit/)
on the main board of the DVD drive:

<div class="thumbnail">
<figure>
[![](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/modchip_thumbnail.jpg)](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/modchip.jpg)
<figcaption>Xeno modchip secured with Kapton tape.</figcaption>
</figure>
</div>

A quick note on the photo above.  As you can see the modchip is soldered
directly onto the DVD drive board, exactly as it was designed to be installed.
This required some fine soldering, with some solder points being less than 0,3mm
away from each other.  I was advised to install the modchip not by soldering it
onto the board, but by soldering wires onto the contact points on the board and
then connecting these wires to the modchip.  I ignored this advice, which I now
regret a bit.  Having the modchip on wires would make it trivial to remove it,
and thus allow to easily restore the drive to its original state.  De-soldering
the modchip from the board itself is a lot more complicated and I am not going
to risk it.

Installing the modchip allowed me to launch [Swiss
homebrew](https://github.com/emukidid/swiss-gc) from a burned DVD, which then
automatically booted into the latest version of Swiss installed on a microSD
card inside an [SD2SP2 adapter](https://github.com/citrus3000psi/SD2SP2).  From
a functionality perspective this setup provided everything I needed.

Recently the GCLoader was redesigned to use the currently available FPGA chip
and the ODE once again became available for purchase.  Production batches are
small and they sell out very quickly.  Luckily, they are announced in advance
and I managed to grab one unit in early November.

<div class="thumbnail">
<figure>
[![](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/gcloader_thumbnail.jpg)](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/gcloader.jpg)
<figcaption>The GCLoader, revision 2.</figcaption>
</figure>
</div>

In the light of what I said above about the modchip and SD2SP2 setup it is right
to ask whether switching to a GCLoader makes any sense.  After all, it doesn't
provide any new functionality and it takes away the ability to run games from
original discs.  Not to mention the cost of the ODE itself.  These are all fair
points.  However, GameCubes in recent years have seen frequent drive failures
due to decaying capacitors and I wanted to preempt such a failure on my console.
I should probably just recap the drive, but this is something I still don't feel
confident with and want to avoid recapping if possible.

Installing the GCLoader is simple since it doesn't require any soldering.  All
it takes are a couple of screwdrivers, with the only caveat being the need for a
[Gamebit](https://en.wikipedia.org/wiki/List_of_screw_drives#Line_Head_and_Line_Recess)
screwdriver.

<div class="thumbnail">
<figure>
[![](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/console-disassembled_thumbnail.jpg)](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/console-disassembled.jpg)
<figcaption>Disassembly is very straightforward.  That's the motherboard with a heatsink.</figcaption>
</figure>
</div>

<div class="thumbnail">
<figure>
[![](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/parts_thumbnail.jpg)](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/parts.jpg)
<figcaption>And these are all the other parts, including a GameBoy Player and a network adapter.</figcaption>
</figure>
</div>

<div class="thumbnail">
<figure>
[![](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/installation-in-progress_thumbnail.jpg)](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/installation-in-progress.jpg)
<figcaption>GCLoader attached to the drive mounting plate.</figcaption>
</figure>
</div>

The only tricky bit is attaching the GCLoader to a metal plate on which the DVD
drive normally sits (last photo).  The screws need to be a bit loose to allow
the GCLoader to move freely.  This is because the holes in the plate are larger
than the GCLoader screws and so it is possible to move it around by about 1mm in
each direction.  If it is fixed in a position that does not align with the port
on the main board there is a risk that resulting forces will eventually break
the solder points on either the GCLoader or the motherboard.  By leaving the
screws slightly loose the GCLoader can easily move to the right position.

And here is the console right before final assembly:

<div class="thumbnail">
<figure>
[![](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/final-reassembly_thumbnail.jpg)](/images/posts/2023-11-30-installing-a-gcloader-in-my-gamecube/final-reassembly.jpg)
<figcaption>Final reassembly, just before installing top of the console case.</figcaption>
</figure>
</div>

As you can see I installed a 3D printed SD mount on top of the GCLoader.  Being
optimistic about the GCLoader availability, I got the SD mount shortly after I
bought the console.  The SD mount waited for over two years in my drawer and I
am glad I can finally make use of it.  I can't remember where I bought the SD
mount, but I remember it was quite expensive.  However, the plastic is of such a
high quality that I think this expense was fully justified.

After assembling the console I downloaded the latest Swiss and placed it on an
SD card as a `boot.iso` file.  What really surprised me was that Swiss
recognized the outdated firmware of the GCLoader and informed me about the
available update.  There is no way the console can download the update since it
has no Internet connectivity[^1], but the fact that a sufficiently new version
of Swiss recognizes an outdated firmware is really neat.


[^1]: As you have seen in the above photos, I actually have a GameCube network
      adapter.  I bought it recently during a trip to London.  However, it is
      more of a curiosity though, since there were only a couple of games that
      made use of online features.
