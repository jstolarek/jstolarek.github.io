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
fast and responsive, without bloat, and also easy to deploy or move to a
different hosting.

The first step was to plan new website layout.  The old Fremen Zone used frames,
which nobody really relies on today.  For the new version I decided to go with a
navigation menu placed at the top of the page.  This layout felt clean,
minimalistic, and seemed easy to adapt to mobile devices.

<div class="thumbnail">
<figure>
[![](/images/posts/2024-03-23-fremen-zone-creating-a-website-using-only-html-and-css/navbar_thumbnail.png)](/images/posts/2024-03-23-fremen-zone-creating-a-website-using-only-html-and-css/navbar.png)
<figcaption>Navigation bar highlights the current section.  Below the navbar is a section logo.</figcaption>
</figure>
</div>

I haven't done any frontend programming since my original work on Fremen Zone
nearly quarter a century ago, so not only have I forgotten everything I learned
about HTML and CSS back then, but also both standards have changed over time.
And so I was learning HTML5 and CSS from scratch as I worked on the new page
design.

An important change made to HTML since its early days is the introduction of
tags that carry important semantic meaning.  And so the navigation bar is
enclosed in `<nav></nav>` tags, sub-page headers use `<header></header>` tag,
and so on -- whenever semantic tags were available I made sure to use them.

Writing a custom website generator
----------------------------------

I spent approximately two weeks prototyping the page.  Once the initial design
was in place I began preliminary work on the contents.  After creating several
subpages it quickly became apparent that pages contain lots of repetition, in
particular in the `<head>` and `<navbar>` tags.  Surely, there were minor
differences - each page had a different `<title>` and each subpage's navbar
highlighted a different current section - but the overall structure of each page
was identical.  It was clear at this point that this repetition must be
eliminated.  And so the next step was to write a custom static website
generator.

I decided to put each logical fragment of the page into a separate template
file.  For example, template for the page's HTML header contains:

```html
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta name="author" content="Jan Stolarek">
  <link rel="preload" as="font" type="font/woff2" href="RELPATH/fonts/fa-solid-900.woff2" crossorigin>
  <link rel="preload" as="style" type="text/css" href="RELPATH/styl.css">
  <link rel="preload" as="image" type="image/png" href="RELPATH/img/SECTION.png">
  <link rel="stylesheet" type="text/css" href="RELPATH/styl.css">
  <link rel="icon" type="image/x-icon" href="RELPATH/favicon.png">
  <link rel="license" href="RELPATH/LICENCJA.md">
  <title>PAGETITLE</title>
</head>
```

Putting all those repeating fragments into their own template files allowed me
to reduce each page to just its proper contents enclosed inside a `<div></div>`
block.  Everything else - `<head>`, `<navbar>`, `<header>`, `<footer>`, etc. -
is stored in template files.  This way all the repetition in HTML sources is
gone and changing, say, page's `<head>` section requires modifying only the
respective template file.

I then wrote a bash script to assemble full HTML pages from templates and the
proper source files.  At the heart of the script are the following lines:

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

You might be wondering about the `RELPATH`, `SECTION`, and `PAGETITLE` words
used in the example template file above.  These are reserved words that are
replaced with page-specific values using `sed`.  This allows not only to have
page-specific titles and section logos, but also calculate relative paths:

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

I like to keep all my projects organized, and so files that comprise the website
are placed into dedicated directories.  For example, all images are placed in
`img` directory, while all kinds of resources (e.g. web fonts, a unique
non-generated 404 page, favicon) are in `res` folder:

```
fremenzone
├── css
├── docs
├── img
├── res
├── src
└── templates
```

In order for all these files to form a coherent page that can be displayed in a
browser, they must be assembled into one directory - `docs`.  This was initially
done by the same bash script that generated the pages from templates.  The
script would first copy all images, resources and CSS files to `docs` and then
it would proceed to generate *all* HTML pages, regardless of whether they were
changed or not.

This approach was very easy to program and worked well at first.  However, as I
added more subpages, the approach of rebuilding every HTML page became a bit too
slow.  Obviously, it wasn't very slow, but slow enough that I had to
deliberately pause before pressing F5 in the browser after making changes to the
sources.  It was time to write a proper `Makefile` for the page.

With a `Makefile` in place, only the modified files were regenerated.  No more
rebuilding of all of the HTML sources or copying of the whole image directory.
Switching the build process to `make` made rebuilding the modified sources
orders of magnitude faster.  The caveat is that rebuilding *all* of the website,
for example after calling `make clean`, is actualy several times slower than
with the bash script.  However, that is a corner case that happens very rarely.
What matters is that the most common use case has been optimized.

Polishing the details
---------------------

At various moments during Fremen Zone development I've spent quite significant
amounts of time on getting the various details right.

First and foremost I wanted to ensure that I write correct HTML.  To this end I
wrote a bash script that checks all of the generated pages using
[html-tidy](https://www.html-tidy.org/) and created a dedicated `make` target to
be able to call it easily.  The next step was to set up a GitHub action that
will call the script on every commit, just in case I miss something during local
development.  Additionally, just to be extra sure, I've set up a [github
action](https://github.com/Cyb3r-Jak3/html5validator-action) that check
generated pages using
[html5validator](https://github.com/svenkreiss/html5validator).

One of the things I absolutely wanted to have on my page was an RSS feed.
Again, I didn't want to rely on any generators - this might not have even been
possible, given that Fremen Zone does not take a format of a blog - which forced
me to dive into [RSS specification](https://cyber.harvard.edu/rss/rss.html).
Luckily, it's not that complex but it took some effort to make things work as
intended.  Not everything was clear from the specification, forcing me to peek
into various generated feeds, such as the one generated by Hakyll for this blog.
I also mixed in elements of
[Atom](https://validator.w3.org/feed/docs/atom.html), so the feed isn't purely
in the RSS format.

Once an RSS feed was ready, I wanted to have an RSS icon in the navigation bar.
This was surprisingly tricky to get right.  After quite a substantial amount of
research and experimentation, I figured out that the only reliable method that
works on multiple different browsers is to use a dedicated glyph from an
embedded web font.  I went with an older version (5.15.4) of
[FontAwesome](https://github.com/FortAwesome/Font-Awesome/) web fonts in two
different formats, [woff](https://caniuse.com/woff) and
[woff2](https://caniuse.com/woff2), the former one for those poor souls that use
unbeliveably outdated web browsers.  Using a web font also solved the problem of
displaying an icon for "hamburger menu" on mobile devices.

At some point I spent quite a significant amount of time on figuring out how to
optimize Fremen Zone for faster loading.  I admit that I am a beginner when it
comes to the topic and I had to rely on pieces of advice found online - often
conflicting, forcing me to dive deeper into HTML5 specification and browser
documentation.  One simple thing was to use a CSS minifier, a program that
strips whitespaces and comments from a CSS file, making it more than 50\%
smaller.  I also experimented with HTML minifiers, but couldn't find one that
would actually work without breaking the page and violating the HTML5 standard.

The next step in optimizing the loading times was related to embedded web fonts.
Firstly, it is possible to strip the redundant glyphs from a font file.  I only
needed two glyphs from each of the two font files (woff, woff2), which allowed
me to reduce file sizes from 40-50kB, to less than a kilobyte.  Secondly, it is
possible to pre-load the web fonts, i.e. instruct the browser in the HTML header
to start loading the font files before the rest of the page is parsed and the
glyphs from font are actually requested.  It is also possible to pre-load the
CSS and other resources, which I did.  I'm not exactly sure whether this brings
huge benefits - the measurements I made using the browser's developer tools were
inconclusive.  However, there seems to be no costs of attempting a preload, so I
decided to keep it.  For a brief moment I also experimented with [lazy image
loading](https://developer.mozilla.org/en-US/docs/Web/Performance/Lazy_loading),
but realized that's one of the features I absolutely hate on the web and
dropped the whole idea.

------------

  * kompatybilność z różnymi przeglądarkami
  * responsywność na mobile - do sekcji pierwszej
  * hostowanie w dwóch repo
  * nie udało się uniknąć skryptów
