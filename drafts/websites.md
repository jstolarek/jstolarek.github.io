All the things I've ever done wrong with my web sites
=====================================================

For the past 23 years I maintained my online presence by hosting a home page, a
blog, or a fansite of some sort.  Fabien Sanglard [recently wrote a blog
post](https://fabiensanglard.net/html/index.html) about technical aspects of his
own blog and this made me reflect on all my web sites and things I did wrong
when creating and hosting them.  Here's a chronological list.

Fremen Zone
-----------

This will be a trip down memory lane.  Fremen Zone was my fansite dedicated to
Frank Herbert's "Dune" novel and related works, i.e. games and movies.  I
started it when I was in high school back in 1999.  By that time I already knew
how to program, but creating web sites was a new thing for me, and I wanted both
to share my fascination with the Dune universe as well as learn how to create
web pages.  This was also the time when lots of online portals offered free
hosting.  In the west this was typically GeoCities, but here in Poland I used
Onet.  By signing up one could only get not only an email address but also a few
megabytes of disk space to host a web site.  I know that by today's standards 2
or 5 megabytes is laughably low, but back then it was more than enough.  Later
in its lifetime Fremen Zone changed hosting and included some multimedia
downloads, growing the page size to total of around 75 megabytes.

Fremen Zone was initially intended to be dedicated to Dune II strategy game.  A
very rough first prototype looked like this (this is the highest resolution
screenshot I have):

  PHOTO: fremenzone proto 1

It was quickly discarded and replaced with another prototype, this time with
scope extended to contain the books.  As you can see the working name of the
website was also different:

  PHOTO: fremenzone proto 2

Both prototypes where created using Microsoft FrontPage, a WYSIWYG HTML editor.
Luckily, I quickly realised that, firstly, I am not learning much by using this
kind of tool and, secondly, that the genereated HTML is bloated and unlikely to
work on web browsers other than Internet Explorer, such as Netscape Navigator.
As a result I switched to writing HTML and CSS by hand.  The results were very
good and I think they still hold up well even today:

  PHOTO: fremenzone now

Fremen Zone went online in June 2000.  The proces of first creating and then
maintaining it was a lot different from what we do today.  First and foremost, I
didn't have Internet access at home.  This meant I had to prepare everything in
advance and only upload the updated files once in a while using my dad's PC at
his work.  It was also the only time when I could browse the Internet to find
any information for the website, although this wasn't a big problem since
practically of the content was created by me based on the books.  The only
significant exception were reviews of the last three books in the Dune cycle,
which I have not yet read when I started creating Fremen Zone.  Luckily, Dune
was quite a popular books amongst my peers and a friend of mine who read all the
books agreed to provide missing reviews.  But with no Internet access this was a
logistically complicated process, since he lived in a different part of city.
We spent a long time discussing the books, the page, and writing reviews over
the phone, which resulted in phone bills going up significantly.  Again, not a
trivial problem in late 90s Poland.  I also mentioned that Fremen Zone contained
some downloads.  These included, among others, Dune and Dune II games, which
back then were considered abandonware.

Fremen Zone was last updated in 2004.  At some point there wasn't much more to
write about and the page simply existed without any supervision.  It eventually
withered away and disappeared when its free hosting was closed down.  I have no
recolection of when that happened.

In retrospective I am very happy about how Fremen Zone turned out.  Although the
page is no longer hosted I still have a copy on my hard drive that "just works"
and could be hosted without any modifications.  Perhaps the only complaint that
could be made is that doesn't look good on mobile devices, but honestly speaking
I couldn't care less about that.  Beside that pure HTML and basic CSS are
totally future proof.  When it comes to Fremen Zone I can proudly say I made no
mistakes.  As you will see below this is not the case for my other websites.

Lessons learned:

  * Filling the website with content is lots of fun.
  * Hand-written HTML and CSS are great, though full-scale redesigns are
    tedious.


Evolutionary biology blog at blogger.com
----------------------------------------

Towards the end of my university studies I picked interest in evolutionary
biology.  I read lots of books on the subject and I have to credit works of
Charles Darwin, Richard Dawkins, and E. O. Wilson as being the most important
influence in forming my approach to research and science in general.  (For the
record, I studied computer science, not biology.)  I decided to turn that
fascination of biology into a blog.  It contained mostly reviews of popular
science books about biology, myrmecology, cognitive science, and ocassionally
modern physics and cosmology.

I decided to host my blog using one of many blogging services available in the
noughties.  After some consideration I went with Google's Blogger, with first
post being published in 2008.  Blogger is designed as a user-friendly CMS with
some (limited) options to customise layout.  Post editing was done using an
online WYSIWYG editor, with everything being immediatelly saved, at least in
theory.  Over time I grew to rather dislike that editor.  It wasn't a horribly
bad experience but editing inside a text field embedded in a browser window is
far from being the best possible experience.  There was always the option to
switch it to pure HTML mode, but as you might guess two modes weren't perfectly
compatible as code generated from the editor wasn't exactly what one would write
by hand, and converesely, code written by hand sometimes wasn't parsed correctly
in WYSIWYG mode.

In the end I think the biggest problem with Blogger was taking ownership of
created content.  There is the ever-looming threat of service joining the
[Google Graveyard](https://killedbygoogle.com/).  Although Blogger allows to
export all posts to an XML file, it seems to be only able to export text, with
all images being lost in the export process.

Lessons leared:

  * Do not use a service that doesn't allow to export all created content.
    Always make sure to own everything that you created.  Ideally, don't depend
    on an external service at all.
  * Online WYSIWYG editors are somewhat inconvenient.


Professional homepage, version 1: plain HTML
--------------------------------------------

In late 2008 I started my work at Politechnika Łódzka[^1] (PŁ for short).  Back
then PŁ didn't have a unified online learning platform and so each employee used
their personal web page to provide learning materials for the students.  I had
to create such a page for myself and since I needed it ASAP I chose the easiest
solution: hand-written HTML with practically no CSS, other than white
background.  The page wasn't fancy but it did the job of delivering basic
information.

Lessons learned:

  * Again, hand-written HTML and CSS are great.


Professional homepage, version 2: DokuWiki
------------------------------------------

That first version of my professional homepage was intended to be a temporary
solution but in the end lasted for about 3 years until 2011.  What made me move
on was reading of ["Pragmatic Thinking and
Learning"](https://pragprog.com/titles/ahptl/pragmatic-thinking-and-learning/),
which convinced me that it is a good idea to have a personal wiki to organize my
research.  One of my colleagues pointed me to DokuWiki, which he described as a
lightweight wiki being solely based on markdown-formatted text files and thus
requiring no database.  This convinced me.  No database requirement made it easy
to set up, and versatility of configuration made it possible to tweak everything
to my liking.  From now on DokuWiki acted both as my official homepage,
containing information for students and my research portfolio, as well as my
personal wiki, which was hidden behind a login.  I could now organize all my
research notes into pages and the ease of doing that meant I was now making a
lot more notes than I used to.  I also really liked plugin support.  One of the
plugins allowed to me have both Polish and English version of each publicly
visible subpage.  Another provided LaTeX support, allowing to render formulas
related to wavelet transforms.

  PHOTO

While at first I was quite impressed with DokuWiki, over time I realized that it
doesn't quite live up to the promise of its storage being entirely text based.
While the current content of all the pages is indeed stored in text files, under
the hood DokuWiki maintains history of all the edits (after all, it is designed
to host a wiki) as well as an elaborate system of all sorts of caches and
indices.  This means that editing the text files directly instead of using the
HTML editor can potentially get you into trouble with internal data consistency.
And while above I stated how DokuWiki made it easy to make notes, over time my
workflow evolved towards being text-based terminal only and at this point using
a separate browser-embedded editor to take notes became a problem.  Moreover,
all the caches and historical records are binary files and therefore storing
sources of the website in a version control system was also a bit of a problem.

Lessons learned:

  * Having a personal wiki is a fantastic idea.  However, it must be possible to
    easily integrate the process of taking notes with the rest of my workflow.
    Therefore, HTML editors suck.


Yet Another Lambda Blog
-----------------------

When I got into functional programming back in 2012 I thought it would be a
great idea to also run a blog on the topic.  Aside from the fact that I enjoy
sharing my knowledge with others, a blog would also serve a similar purpose as
the personal wiki.  It would be a place where I can record my knowledge for
future purposes[^2].  I have already learned that relying on an external blog
hosting service is not a good idea so I decided to go with self-hosting and set
up a blog CMS along with my homepage on my university's server.  Today I can
safely say that while the assumptions were good, the execution was not: I went
with Wordpress.

Wordpress seemed like a good choice due to amount of offered features, both
built-in and available via additional plugins.  A variety of themes meant I had
no problem adjusting the looks of the blog to my liking.  Similarly to Blogger
Wordpress offered an online WYSIWYG, which at this point I was still willing to
tolerate.  What proved to be the biggest downside was the maintenance burden.
Firstly, since the instance was self-hosted, I was responsible for keeping my
installation up-to-date.  Most updates went fine, but some did not and I found
myself spending hours trying to recover from a failed update.  An unexpected
obstacle came from my hosting, which turned out to be using an extremely old
release of [PLD Linux](https://www.pld-linux.org/) with a woefully out-of-date
PHP version that has not received any security updates for about a decade.  At
some point this became a problem since Wordpress bumped their minimal required
PHP version and I was no longer able to update my installation.  Now, this is a
big deal.  Due to its popularity, Wordpress installations, especially those
outdated ones without security fixes, are a good target for hacker attacks.
Moreover, spambots are a major issue, with spam making up over 99,99% of all
comments.  In order to manage this spam you either turn off comments completely
or use a plugin for filtering spam (I used Akismet).  Lastly, there's the issue
of backups.  Wordpress has plugins that allow you to backup all of your
installation, but moving contents backed up this way to a different installation
is not a straightforward task.  Or at least was not straightforward last time I
tried it - parhaps things have improved?


Cyberbrain[^3]: gollum-based personal wiki
------------------------------------------

At some point I have found myself no longer using my personal wiki.  It's not
like I stopped using it suddenly.  It just happened gradually over time.  When I
realized that it really surprised me, since a personal wiki is a Good Thing and
many times have I been grateful for taking the time to write down various things
that I've learned.  After thinking about it for a while I realized the cause: I
really hated the online interface.  It was slow and completely disconnected from
my workflow: I spent 90% of my work time in the terminal, using Emacs in text
mode as my editor.  Making notes in the wiki meant switching to the browser and
using a completely different way of editing text than the one I use normally.

The solution to this problem came in the form of
[gollum](https://github.com/gollum/gollum/).  Gollum is a markdown based wiki
engine that can run as a local service on a machine.  And this time "markdown
based" really means what it means - no weird caching or indexing like in
DokuWiki.  Gollum requires that the wiki is stored in a git repository and as
such all history is recorded outside of the wiki engine intself.  In fact, most
of you have most likely already seen gollum in action as it is the engine that
was originally created to power GitHub wikis[^4].  Multiple markdown flavours
are supported, but the DokuWiki one isn't so I had to migrate all pages on my
personal wiki.

Once migration was done I had a perfect wiki.  All my notes are now stored in
Markdown files (mostly GitHub Markdown with minor tweaks and enhancements)
organized in directories.  Since it's Markdown it is easy to edit in Emacs,
which allows me to seamlessly integrate taking notes with programming.  In fact,
most of the time I view my notes in Emacs.  I switch to a web browser view of
the wiki only when I need to retrieve information which location in the wiki I
am unsure of and I need to click through several pages in order to find it.
This doesn't happen too often since I try to keep my wiki organized.  At the
moment all my notes total at 1.5MB - and that's just text-based Markdown files,
excluding any multimedia or git history - so it is crucial that they are well
structured.


Professional homepage with blog, version 3: Hakyll + GitHub pages
-----------------------------------------------------------------

After switching to gollum I was still using DokuWiki to run my professional
homepage.  That page, as well as Wordpress-based Yet Another Lambda Blog, came
to an end in 2021.  After spending 13 years at my University I decided to change
jobs, which meant losing access to my hosting.  I had to migrate everything
somewhere else.

All my experiences so far lead me to one conclusion: the simpler the better.  I
didn't want anything that uses a database.  I didn't want a comment system.  I
wanted to store everything in a simple text-based format that is easy to migrate
and store in a git repository.  This left me with two choices: either plain
HTML+CSS or a Markdown-based solution.  I wasn't yet ready to return to using
HTML.  After considering my options I decided to go with
[Hakyll](https://jaspervdj.be/hakyll/), a static HTML generator inspired by
Jekyll, but written in Haskell.

I wanted to combine my professional homepage with a blog.  This meant migrating
already existing DokuWiki pages, which was simple since I already knew how to
handle this from earlier migration to gollum, but it also meant migrating from
Wordpress.  This was a bigger problem, but luckily I found a tool that did 90%
of the job of converting existing blog posts to Markdown.  There were still
cleanup and tweaks here and there, in particular when it came to images and
footnotes.  The process was tedious but overall not that long.

There was a bunch of other stuff that had to be set up in Hakyll, from RSS feed
to creating templates and metadata for blog pages, and various rules defining
how and where to put various files (in particular images).  Luckily, Hakyll
documentation and online examples were helpful, and after about a week of work
the migration was complete.  You are most likely viewing the result right now,
but in case a future migration happens here's a screenshot:

  PHOTO: current

Overall, once setup Hakyll is easy to use and does the job.  It still has its
downsides though.  Firstly, it is a rather heavyweight technology in a sense
that it requires full Haskell toolchain to be installed in the system, plus a
bunch of library dependencies.  I am a Haskell programmer so I tend to have
Haskell compiler at hand, but this is not a small requirement.  And being
Markdown-based leads to limitations that you're going to hit sooner or later.
For me this happened when I decided I want to have captioned images that can be
enlarged with a click.  To achieve the desired effect I had to mix HTML and
Markdown.  For example:

```
<div class="thumbnail">
<figure>
[![](/images/posts/2023-06-03-firmament/crash_thumbnail.jpg)](/images/posts/2023-06-03-firmament/crash.jpg)
<figcaption>A sadly common sight.</figcaption>
</figure>
</div>
```

What next?
----------

TODO: mention new version of FZ, desire to use pure HTML+CSS, perhaps my own
generator.


[^1]: Official English translations of "Politechnika Łódzka" changed over time.
      When I started my work it was Technical University of Lodz but sometime
      aroun 2013, I think, it changed to Lodz University of Technology.

[^2]: I can't overstate how good a thing this is.  I have revisited some of my
      blog entries and found them extremely helpful and was grateful to myself
      that I took the time to write them.

[^3]: The term "cyberbrain" comes from Ghost in the Shell and referes to, well,
      cybernetic brain replacements used in the manga and its anime adaptation.

[^4]: I am not sure whether this is still the case. Based on bits and pieces of
      information I was able to find on the Internet I think gollum and the
      engine that currently powers GitHub wikis are technically separate
      codebases, but gollum strives to remain fully compatible with GitHub.
