---
title: Recalbox RGB Dual&#58; some impressions after six months of usage
date: 2022-12-26
---

Recalbox RGB Dual: some impressions after six months of usage
=============================================================

I am not a fan of Kickstarter and tend to stay away from it.  That being said,
once in a while I make an exception.  Typically my exceptions are books on retro
gaming, but towards the end of last year I decided to back campaign for
[Recalbox RGB
Dual](https://www.kickstarter.com/projects/recalbox/recalbox-rgb-dual), a
RaspberryPi HAT (Hardware Attached on Top) that provides SCART and VGA output
ports allowing to connect the Pi to a CRT TV or a VGA monitor:

<center>
[![rpi_heatsink](/images/posts/2022-12-26-rgb-dual/rgb_dual_1_thumbnail.jpg#thumbnail)](/images/posts/2022-12-26-rgb-dual/rgb_dual_1.jpg)
[![rpi_heatsink](/images/posts/2022-12-26-rgb-dual/rgb_dual_2_thumbnail.jpg#thumbnail)](/images/posts/2022-12-26-rgb-dual/rgb_dual_2.jpg)
</center>

RGB Dual allows setting up RaspberryPi as a dedicated retro emulation device and
running games on a proper CRT screen, ensuring that these old games look exactly
the way they were intended to.  In the last couple of years I transitioned
towards playing on original hardware and I emulate less than I used to, but
nevertheless there's still some retro hardware that I need to emulate:

  1. Playstation 1, a.k.a. PSX.  I haven't pulled the trigger on this one yet.
     Primary reasons are that mods are expensive and difficult (i.e. require
     precision soldering I'm not confident with), while emulation is accurate
     and, unlike Sega Saturn from the same generation, not computationally
     expensive.  Raspberry Pi has enough power to run PSX emulation at full
     speed.  Or at least it should.  As you'll see below this isn't necessarily
     the case.

  2. NeoGeo MVS, CPS-1/2/3, and other arcade hardware.  In principle, collecting
     original arcade hardware isn't easy due to its sizes and prices.  Towards
     the end of 2019 I was close to getting a consolized MVS, i.e. a NeoGeo MVS
     arcade board adapted to be used at home connected to a for a reasonable
     price.  This is a relatively cheap and accessible way of owning original
     NeoGeo arcade.  Sadly, I didn't decide on the purchase and during pandemic
     the prices skyrocketed.  Now consolized MVS costs 4-5 times more than it
     used to in 2019.

  3. Sega MegaDrive (and its CD variant).  Not too many MegaDrive games I really
     like so this one isn't a priority for me.

  4. NES.  I've been omitting this one due to no RGB output on the console,
     though this has recently changed thanks to Krikkz's [RGB
     Blaster](https://krikzz.com/our-products/cartridges/rgb-blaster.html).

There are a bunch of other 8-bit and 16-bit systems that I'd like to explore one
day without having to buy the actual hardware, so RGB Dual definitely looked
like a good investment.  I backed the campaign and patiently waited for the
delivery.  After a tolerable delay of 2 months I got my board in June.  The
first obstacle was getting RGB Dual to fit on top of Aluminium Armor heatsink.
This is a large heatsink that allows to efficiently cool the Pi silently with no
fans:

<center>
[![rpi_heatsink](/images/posts/2022-12-26-rgb-dual/rpi4_heatsink_1_thumbnail.jpg#thumbnail)](/images/posts/2022-12-26-rgb-dual/rpi4_heatsink_1.jpg)
[![rpi_heatsink](/images/posts/2022-12-26-rgb-dual/rpi4_heatsink_2_thumbnail.jpg#thumbnail)](/images/posts/2022-12-26-rgb-dual/rpi4_heatsink_2.jpg)
</center>

While I love these heatsinks and have them on all of my Pis I must admit their
height is a bit of a problem when connecting HATs.  After some initial fitting I
trimmed the bottom pins on RGB Dual board with pliers and secured them with
Kapton tape to make sure they don't touch the heatsink.  I also gently trimmed
plastic SCART port mountings:

<center>
[![rpi_heatsink](/images/posts/2022-12-26-rgb-dual/trimmed_pins_thumbnail.jpg#thumbnail)](/images/posts/2022-12-26-rgb-dual/trimmed_pins.jpg)
</center>

With these adjustments the GPIO connector still doesn't go all the way down but
it fits well enough for RGB Dual to operate safely:

<center>
[![Raspberry Pi with RGB Dual mounted on top](/images/posts/2022-12-26-rgb-dual/rgb_dual_mounted_1_thumbnail.jpg#thumbnail)](/images/posts/2022-12-26-rgb-dual/rgb_dual_mounted_1.jpg)
[![Raspberry Pi with RGB Dual mounted on top](/images/posts/2022-12-26-rgb-dual/rgb_dual_mounted_2_thumbnail.jpg#thumbnail)](/images/posts/2022-12-26-rgb-dual/rgb_dual_mounted_2.jpg)
[![Raspberry Pi with RGB Dual mounted on top](/images/posts/2022-12-26-rgb-dual/rgb_dual_mounted_3_thumbnail.jpg#thumbnail)](/images/posts/2022-12-26-rgb-dual/rgb_dual_mounted_3.jpg)
</center>

I could push it deeper but this comes at a cost of slightly bending the board
upwards, which I want to avoid not to weaken the solder joints on GPIO
connector.

Having fitted the board I began software setup.  RGB Dual works only with
[Recalbox](https://www.recalbox.com/), a Linux distribution dedicated to
emulation that runs [RetroArch](https://www.libretro.com/) under the hood.  I
flashed a memory card, put it into the Pi, connected to my TV via SCART
port... and got a black screen.  As a first debugging step I tried connecting
the Pi to HDMI monitor and, surely enough, everything Just Worked.  I was
convinced that my board was dead on arrival.  After spending 2 hours on
debugging it turned out that I need a special build of Recalbox distribution
that supports RGB Dual.  Too bad this wasn't mentioned in any obvious place -
this would have saved me some time.  Note also that this is no longer the case
as of the moment of this writing.  Mainline Recalbox distribution now supports
RGB Dual without any problems.

With everything ready I could now properly test how Recalbox works with the
board.  Long story short, it does what it advertises but not without issues.
While I had no problems with arcade and NES emulation, I've run into problems
with PSX emulation.  As mentioned earlier, Recalbox is based on RetroArch.  By
default RetroArch offers multiple cores for emulating Playstation, such as
Beetle PSX (a fork of [Mednafen](https://mednafen.github.io/)), [PCSX
ReARMed](https://github.com/notaz/pcsx_rearmed), or Swanstation (a fork of
[Duckstation](https://www.duckstation.org/)).  However, for the purpose of RGB
Dual developers of Recalbox limited available PSX cores to just a single one:
Swanstation.  The rationale behind this is that having just a single core allows
to offer better support and this definitely sounds like reasonable approach on a
new product.  Let me add here that Duckstation, while relatively new, offers
very high compatibility and in the recent years seems to have become a go-to
Playstation emulator.  (I'm saying this based on reading discussions online, not
from my experience.  I personally never used it, always relying on Beetle PSX.)
All that being said, PSX emulation quality offered by Swanstation in Recalbox is
below my expectations.  First and foremost there is no support for 480i
resolution.  As a result games that heavily rely on switching resolutions
between 240p and 480i, e.g. Chrono Cross, look bad when using the latter since
every other line is missing.  Note that this is not a hardware limitation of RGB
Dual: it supports 480i resolution in the main Recalbox menu without problems.
Developers are promising 480i support in future versions of Recalbox but at the
moment this support is missing.

I have also run into serious performance issues with Swanstation.  I am
currently playing Final Fantasy IX and while the game runs well in locations
with 2D backgrounds, it stutters horribly on the 3D World Map with framerates
going down from 60 frames per second to around 45.  Framerate drops also
sometimes happen during battles, either when there are many (4-5) enemies or
when backgrounds are more complex than usual (e.g. contain animated waterfall).
Lastly, most FMVs stutter.  The only way of improving the situation that I found
is disabling VSync.  Doing this allows to boost FPS count on the world map to
around 55 but comes at a cost of occasional image flickering.  This is very
disappointing.  Raspberry Pi 4 certainly has enough power to run PSX emulation
at full speed.  I've run Beetle PSX and PCSX ReARMed cores on
[Lakka](https://lakka.tv/) and never experienced any performance problems.  I am
guessing this higher emulation overhead comes from Swanstation's alleged higher
accuracy, but again I have never experienced any accuracy issues under other
cores while Swanstation freezes every few hours of gameplay.  Luckily, Recalbox
developers are promising more PSX cores compatible with RGB Dual in future
updates and I hope these new cores will solve the performance problems.  I am
eagerly waiting for new releases of Recalbox.  For now I just have to make do
with Swanstation.  Too bad PSX emulation is my primary use case for RGB Dual and
as of this writing it leaves a lot to be desired.
