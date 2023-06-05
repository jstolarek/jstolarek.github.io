---
title: Metroid II&#58; Return of Samus (and its remakes)
date: 2023-02-14
---

Metroid II: Return of Samus (and its remakes)
=============================================

Two weeks ago I finished playing Metroid II on GameBoy.  I bought the original
cartridge for a bargain price in Cex sometime in mid 2021.  I started playing
the game back then but gave up about 1,5 hour into the game, primarily due to
(perceived) high difficulty.  [Having modded another GameBoy Color
recently](2023-01-23-gameboy-color-nes-themed-modkit-from-funnyplaying.html) I
spontaneously picked up the cart and resumed playing, just to make sure that
everything works correctly.  Before I knew it I was past the difficulty spike
and quickly progressing through the game, finishing it in under 6 hours of total
game time.

Metroid II is unlike other Metroid games.  (Except for its remakes, but we'll
get to that.)  Other entries in the series require the player to explore a
single large map and do a lot of backtracking to revisit previously explored
areas.  Metroid II is quite different.  As usual the player begins by having
access to only a small part of the world map.  But this time access to further
areas of the planet opens automatically when player kills all metroids in the
currently explored area.  It's a rinse and repeat pattern: kill all metroids in
a given area, descend deeper into the caves of SR388 to a newly-open area, kill
all metroids there, and so on.  As a result at any given time player is only
exploring a relatively small chunk of the map that acts as a self-contained
level.  There is no reason to backtrack to previously explored areas, though
there is nothing stopping the player from doing so if they wish.  Like I said,
this is different from other Metroid games, but note that when the game was
released in 1991 there was only one other Metroid game.  The genre-defining
Super Metroid was only to come in 1994.  Also, such a design made a lot of sense
given that: a) the game was designed for a handheld console with the intention
of being played in small chunks (and I can confirm you're never farther than 2
minutes away from a save station); and b) the game didn't have a built-in map
and so the world had to be designed in a way that's possible to navigate without
a map.


Hardware issues with my NES-themed GameBoy Color
------------------------------------------------

As mentioned, my newly-modded GameBoy Color with larger screen was my motivation
to resume playing.  Unfortunately, I ran into hardware issues.

GameBoy Color runs on two AA batteries.  At the moment there are no easy,
drop-in rechargeable battery mods so I just use rechargeable batteries.  To my
surprise a pair of such batteries only gave me about 15-20 minutes of gameplay.
After that time the console began rebooting during play, sometimes going into a
loop of constant reboots.  I was of course expecting a higher power draw due to
larger, backlit IPS panel, but such a short battery life was an unpleasant
surprise.  I switched to using non-rechargeable Duracell batteries.  These gave
me maybe 90 minutes of play time before same problem started occurring.  That
was definitely unexpected.  Soon after my GBC refused to turn on regardless of
batteries used.

This kind of problem is actually quite common in GameBoys and is caused by the
power switch.  These tend get dirty and oxidize (read: rust) over time.  The
solution is to disassemble the switch be removing the metal covering it and then
clean the inside with IPA.  I already did that in the past with one of my
GameBoy Advances.  While this all sounds quite simple, desoldering the power
switch cover is actually quite difficult, and unfortunately I messed up.
During desoldering the bottom plastic part of the switch broke in half, with one
half coming off the PCB.  I have some replacement power switches for the
GameBoy so I thought at this point I might as well replace the (now literally)
broken power switch with a new one.  This required desoldering the remaining
half of the switch, which I also messed up by lifting one of the pads from the
PCB.  I managed to salvage this and solder the new switch in such a way that it
works, which I consider to be a small miracle.

But doing all these fixes required a free afternoon and I just wanted to
continue with the game!  It was a good reason to grab my GameBoy pocket:

<center>
<figure>
[![](/images/posts/2023-02-14-metroid-ii-return-of-samus-and-its-remakes/gbp_gameplay_thumbnail.jpg#thumbnail)](/images/posts/2023-02-14-metroid-ii-return-of-samus-and-its-remakes/gbp_gameplay.jpg)
<figcaption>Preparing to fight Metroid Queen on GameBoy Pocket.</figcaption>
</figure>
</center>

AM2R
----

In 2016 Metroid II received an unofficial, fan-made remake titled AM2R, which
stands for Another Metroid 2 Remake[^1].  It was made by a single person, Milton
Guasti, using GameMaker and re-using assets from Super Metroid and Metroid: Zero
Mission.  AM2R's re-envisioning of Metroid II is spot on, resulting in one of
the best Metroid games ever.  Unfortunately, Nintendo didn't like this and as
soon as they caught wind of AM2R they sent DMCA takedowns to sites hosting the
game.  Luckily, the story doesn't end here.  Things don't disappear on the
Internet that easily and the game still can be downloaded, e.g. from
archive.org.  What's more, [AM2R's "executable" (GameMaker byte code) was
disassembled](https://gitlab.com/yellowafterlife/AM2Rrc) and is being maintained
and developed by a [community of
fans](https://github.com/AM2R-Community-Developers).


Metroid: Samus Returns
----------------------

In 2017 Nintendo released its official Metroid II remake for the Nintendo 3DS,
titled Metroid: Samus Returns.  Development was handled by MercurySteam, a
Spanish developer, who also got to develop Castlevania games for PS3 and XBox
360.  For me this game was literally a platform seller - it was the reason I
bought 3DS.  Sadly, it was also a huge disappointment, mostly due to gameplay
changes that took the game into direction that's completely different from other
entries in the series.  Metroid was always a game about exploration, but
MercurySteam pushed Samus Returns towards a combat-focused game.  They
introduced a new "melee counter" combat mechanic.  Enemies will now charge at
Samus and the player is required to counter such attacks with a well-timed
counter that stuns the enemy, allowing to execute a follow-up counter-attack.
This is very difficult to pull off as the timings are very tight and I struggled
to pull them off even near the end of the game[^2].  The result is that instead
of actively engaging enemies, you typically stand still to let enemies attack
hoping that you can pull off the melee counter correctly.  To add insult to
injury, the game forces you to play with the analog stick.  I hate it when
metroidvanias require playing on the analog stick (yes, I'm thinking about Ori),
but here it is particularly bad due to 3DS' analog being a stick only by name.
The result is that you're now required to aim very precisely with analog that's
not up for the task.  The end result is a Metroid game that's hampered by its
combat mechanics, which was never an issue for any prior Metroid game.

I could spend a few more paragraphs bashing Samus Returns, but it actually has
one design aspect that needs praising: boss design.  I really like how
MercurySteam designs bosses, both in Samus Returns and recent Metroid Dread.
Bosses in these two games clearly telegraph what attacks they are going to
execute 1-3 seconds before actually executing them yet they remain challenging,
often requiring several attempts to defeat.  Mining Robot is definitely one of
the best bosses in the Metroid series.


Summary
-------

After getting over the initial difficulty spike, I really enjoyed Metroid II.
Also, plot-wise this is an important entry in the series, directly affecting
events in Metroid Fusion and Metroid Dread.  That being said I would hesitate to
recommend Metroid II to anyone except for die-hard series' fans.  I do recommend
AM2R though.  Despite being a fan-made project, and unlike Nintendo's Samus
Returns, AM2R perfectly captures the spirit of Metroid II and offers great
gameplay.

Finally, I really recommend watching GMTK's [How AM2R and Samus Returns remade
Metroid 2](https://www.youtube.com/watch?v=8WkEoYvlUF0) video, which offers a
much deeper comparison of Metroid II, AM2R and Metroid: Samus Returns than the
one I have given above.


[^1]: That "Another" bit is just for kicks, I guess?  There was no prior Metroid
      II remake.

[^2]: Recent Metroid Dread, also from MercurySteam, somewhat fixes that mechanic
      by adding more leniency to the counter timings.
