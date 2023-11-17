---
title: Free IPS Patcher
date: 2023-03-02
---

Free IPS Patcher
================

I recently started playing Final Fantasy IV on SNES.  Before playing the game I
decided to patch the ROM with [Namingway Edition
patch](https://www.romhacking.net/hacks/2337/) and once again found myself
without a tool for applying IPS patches.  Sure, there are [online
patchers](https://bbbradsmith.github.io/ipstool/) but I hate resorting to online
services for doing such simple tasks.  Luckily [IPS file format is actually very
simple](https://zerosoft.zophar.net/ips.php) so I decided to write a patching
tool.  Behold the [Free IPS
Patcher](https://bitbucket.org/jstolarek/free-ips-patcher).  Use it from the
command line like so:

```
ips ROM_FILE PATCH_FILE OUTPUT_FILE
```

It's all coded in less than 200 lines of C, with majority of the code being
error handling.  I was tempted to write the program in Haskell but I wanted it
to be easily accessible and, let's face it, most people don't have Haskell
toolchain on their machines.
