---
title: Installing OCaml under openSUSE 11.4, or&#58; "the compilation of conf-ncurses failed"
date: 2016-05-31
---

Installing OCaml under openSUSE 11.4, or: "the compilation of conf-ncurses failed"
==================================================================================

Recently I decided to learn the basics of OCaml and I spent yesterday installing
the compiler and some basic tools. On my machine at work I have a Debian 7
installation, while on my home laptop I have openSUSE 11.4. Both systems are
quite dated and ship with OCaml 3.x compiler, which is five years
old. Obviously, I wanted to have the latest version of the language. I could
have compiled OCaml from sources - and in fact I have done that in the past to
compile the latest version of Coq - but luckily there is a tool called OPAM
(OCaml Package manager). OPAM can be used to easily download and install desired
version of OCaml compiler. As the name implies, OPAM can be also used for
managing packages installed for a particular compiler version.

The installation process went very smooth on my Debian machine, but on openSUSE
I have run into problems. After getting the latest compiler I wanted to install
`ocamlfind`, a tool required by a project I wanted to play with. To my
disappointment installation ended with an error:

```
[ERROR] The compilation of conf-ncurses failed at "pkg-config ncurses".

This package relies on external (system) dependencies that may be missing.
`opam depext conf-ncurses.1' may help you find the correct installation for
your system.
```

I verified that I indeed have installed development files for the `ncurses`
library as well as the `pkg-config` tool. Running the suggested `opam` command
also didn't find any missing dependencies, and the log files from the
installation turned out to be completely empty, so I was left clueless. Googling
revealed that [I am not the first to encounter this
problem](https://github.com/ocaml/opam-repository/issues/5880), but offered no
solution. I did some more reading on `pkg-config` and learned that: a) it is a
tool that provides meta-information about installed libraries, and b) in order
to recognize that a library is installed it requires extra configuration files
(aka `*.pc` files) provided by the library. Running `pkg-config --list-all`
revealed that `ncurses` is not recognized as installed on my system, which
suggested that the relevant `*.pc` files are missing. Some more googling
revealed that `ncurses` library can be configured and then compiled with
`--enable-pc-files` switch, which should build the files needed by
`pkg-config`. I got the sources for the `ncurses` version installed on my system
(5.7) only to learn that this build option is unsupported. This explains why the
files are missing on my system. I got the sources for the latest version of
`ncurses` (6.0), configured them with `--enable-pc-files` and compiled, only to
learn that the `*.pc` files were not built. After several minutes of debugging I
realized that for some unexplained reasons the `configure`\-generated script
which should build the `*.pc` files (located at `misc/gen-pkgconfig`) did not
receive `+x` (executable) permission. After adding this permission manually I
ran the script and got five `*.pc` files for the `ncurses` 6.0 library. Then I
had to edit the files to match the version of `ncurses` of my system - relevant
information can be obtained by running `ncurses5-config --version`. The only
remaining thing was to place the five `*.pc` files in a place where `pkg-config`
can find them. On openSUSE this was `/usr/local/pkgconfig`, but this can differ
between various Linux flavours.

After all these magical incantations the installation of `ocamlfind` went
through fine and I can enjoy a working OCaml installation on both of my
machines. Now I'm waiting for the "Real-world OCaml" book ordered from Amazon
(orders shipped from UK Amazon to Poland tend to take around two weeks to
arrive).

