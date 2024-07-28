---
title: Fremen Zone&#58; Creating a Website Using Only HTML and CSS
date: 2024-06-02
---

Fremen Zone: Creating a Website Using Only HTML and CSS
=======================================================

Last July I wrote [a post about all the websites that I have run in my
life](2023-07-12-all-the-things-ive-ever-done-wrong-with-my-websites.html).  An
important part of that list was Fremen Zone, a fansite dedicated to Frank
Herbert's "Dune".  It was my first website ever, which I created back in high
school and have restored last year from backups on my hard drive.  I ended the
post with an idea of creating a new version of Fremen Zone:

> Restoring the Fremen Zone back to life sparked a new desire in me.  While I
> said I have no intention of updating it, I feel like re-doing the whole site
> from scratch.  (...)  And so I am slowly gathering material for a new Dune
> fanpage.  All the lessons above have taught me to avoid complexity and stay as
> close to the basics as possible.  And so my plan is to create a new website
> based on pure HTML and CSS.

Since late June 2023 I have been hard at work to create a new version of Fremen
Zone.  The website went live on Febuary 1st 2024 and is available at
<https://www.fremenzone.pl>.  In this post I want to talk about the technical
aspects of the new Fremen Zone page, in particular putting emphasis on
simplicity and writing a dedicated static page generator.

Keeping things simple
---------------------

The primary principle driving the technical design of the new page was to keep
technical solutions as simple as possible.  I didn't want to rely on any
existing frameworks, content management systems, or anything of that sort.
Instead, I wanted to create a website that uses only HTML5 and CSS, with no
JavaScript.  The goal was to make a page that is static, small (=quick to load),
fast with no bloat, and also easy to deploy or move to a different hosting.

The first step was to plan new website layout.  The old Fremen Zone used frames,
which nobody really relies on today.  For the new version I decided to go with a
navigation menu placed at the top of the page.  This layout felt clean,
minimalistic, and seemed easy to adapt to mobile devices.

Note that I haven't done any frontend programming since my original work on
Fremen Zone nearly quarter a century ago, so not only have I forgotten
everything I learned about HTML and CSS back then, but also both standards have
changed over time.  And so I was learning HTML5 and CSS from scratch as I worked
on the new page design.

An important change made to HTML since its early days is the introduction of
tags that carry important semantic meaning.  And so the navigation bar is
enclosed in `<nav></nav>` tags, sub-page headers use `<header></header>` tag,
and so on.

Writing a custom website generator
----------------------------------

I spent approximately two weeks prototyping the page.  Once the initial design
was in place I began preliminary work on the contents.  After creating several
subpages it quickly became apparent that pages contain lots of repetition, in
particular the `<head>` and `<navbar>` sections.  Surely, the were minor
differences - each page had a different `<title>` and each subpage's navbar
highlighted a different active section - but the overall structure of each page
was identical.  It was clear at this point that this repetition must be
eliminated.  And so the next thing to do was writing a custom static website
generator.

I decided to put each logical section of the page into a separate template file.
For example, template for a page header contains:

```html
<header>
  <img src="RELPATH/img/SECTION.png" class="logo" alt="">
</header>
```

This allowed me to reduce each page to just its proper contents enclosed inside
a `<div></div>` block.  Everything else - `<head>`, `<header>`, `<footer>`,
etc. - is stored in template files.  This way all the repetition is gone, and
changing, say, page's `<head>` section requires modifying only the respective
template file.

I then wrote a bash script to assemble full HTML pages from templates and the
proper source files.  At the heart if the script are the following lines:

```bash
echo "<!DOCTYPE html>"        >  $HTMLFILE
echo ""                       >> $HTMLFILE
echo "<html lang=\"pl\">"     >> $HTMLFILE
cat $TEMPLATEDIR/head.html    >> $HTMLFILE
echo ""                       >> $HTMLFILE
echo "<body>"                 >> $HTMLFILE
echo ""                       >> $HTMLFILE
cat $TEMPLATEDIR/scripts.html >> $HTMLFILE
echo ""                       >> $HTMLFILE
cat $TEMPLATEDIR/navbar.html  >> $HTMLFILE
echo ""                       >> $HTMLFILE
cat $TEMPLATEDIR/header.html  >> $HTMLFILE
echo ""                       >> $HTMLFILE
echo "<main>"                 >> $HTMLFILE
echo ""                       >> $HTMLFILE
cat $SRCDIR/$FILEPATH         >> $HTMLFILE
echo ""                       >> $HTMLFILE
echo "</main>"                >> $HTMLFILE
echo ""                       >> $HTMLFILE
cat $TEMPLATEDIR/footer.html  >> $HTMLFILE
echo ""                       >> $HTMLFILE
echo "</body>"                >> $HTMLFILE
echo "</html>"                >> $HTMLFILE
```

There are some bash variables in here, but their names should be
self-descriptive.

You might be wondering about the `RELPATH` and `SECTION` words used in the
template file above.  These are reserved words that are replaced with
page-specific values using `sed`.  This allows not only to have page-specific
titles and section logos, but also calculate relative paths:

```bash
# Insert relative paths
if [ "$FILEDIR" = "." ]; then
    RELPATH="."
else
    SLASHES="${FILEDIR//[^\/]}"
    DIRDEPTH="${#SLASHES}"
    RELPATH=".."
    for i in $(seq 1 $DIRDEPTH); do
        RELPATH=$RELPATH"/.."
    done
fi
sed --in-place "s,RELPATH,$RELPATH,g" "$HTMLFILE"
```

or insert last modification date from `git`:

```bash
# Add last modification date
LASTMODIFIED=`git log -1 --pretty="format:%cs" $SRCDIR/$FILEPATH`
sed --in-place "s,LASTMODIFIED,$LASTMODIFIED,g" "$HTMLFILE"
```


------------

  * zamiana skryptu na makefile
  * responsywność na mobile - do sekcji pierwszej
  * preload czcionek
  * hostowanie w dwóch repo
  * nie udało się uniknąć skryptów
