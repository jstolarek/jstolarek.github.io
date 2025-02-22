---
title: Goodbye, Google&#58; Switching from Google Search to Kagi
date: 2024-09-27
---

Goodbye, Google: Switching from Google Search to Kagi
=====================================================

Last year, I wrote about [my experience of installing LineageOS on my
smartphone](2023-10-09-lineageos-on-samsung-s7-an-experience-report.html).  The
goal was to have an Android phone that is free of Google spyware.  Since then, I
have been gradually moving away from using services provided by Google,
primarily the search engine and email services.  In this post, I want to write
about [Kagi](https://kagi.com/), a search engine that I've been using since last
October.

Whatever product is being offered on the market, it is crucial that its
customers are satisfied and willing to pay for it.  The problem with Google
Search is that its users, i.e. people using Google to find information on the
Internet, are not its customers.  It is the advertisers that pay Google for
their service of positioning the ads.  And so, search results provided by Google
must first and foremost benefit the advertisers, with the user experience being
a secondary matter.  This is nothing new.  The problem has been anticipated by
Google creators Sergey Brin and Lawrence Page in their 1998 paper[^1]:

> The goals of the advertising business model do not always correspond to
> providing quality search to users. (...) we expect that advertising funded
> search engines will be inherently biased towards the advertisers and away from
> the needs of the consumers.

In the past years, this has been the subject of a wide discussion sparked by the
gradual degradation of Google's search results quality.

For a couple of years now, I have not been using Google as my primary search
engine.  I switched to DuckDuckGo instead.  For the most time, DuckDuckGo worked
well for me, but every now and then it couldn't find what I was looking for, and
I had to use Google as a fallback.

Better search results
---------------------

About a year ago, I heard about Kagi, a new search engine that people have been
raving about.  Long story short, I decided to give it a try.  Since then, Kagi
has improved my web searching experience by offering better results than its
competitors.  No more falling back to Google or DuckDuckGo.

<div class="thumbnail">
<figure>
[![](/images/posts/2024-09-27-goodbye-google-switching-from-google-search-to-kagi/kagi_main.png)](/images/posts/2024-09-27-goodbye-google-switching-from-google-search-to-kagi/kagi_main.png)
</figure>
</div>

The first couple of weeks of using Kagi felt... strange.  The search results were
sometimes so accurate that they felt scary.  Here's an example.  In a [recent
post about my fan website Fremen
Zone](2024-08-21-fremen-zone-creating-a-website-using-only-html-and-css.html), I
mentioned setting up GitHub actions to validate HTML.  I used
[html5validator-action](https://github.com/Cyb3r-Jak3/html5validator-action) for
this.  I initially ran into a setup problem, where every run of the action
resulted in "There is no git repository detected" error.  And so I used Kagi to
search for "Github actions Error: There is no git respository detected" (yes,
there's a typo in the search phrase).  Here is the search result I got from
Kagi:

<div class="thumbnail">
<figure>
[![](/images/posts/2024-09-27-goodbye-google-switching-from-google-search-to-kagi/kagi_win.png)](/images/posts/2024-09-27-goodbye-google-switching-from-google-search-to-kagi/kagi_win.png)
</figure>
</div>

The first result is literally a bug report for the project I was using with the
exact same problem I was facing.  It doesn't get any more accurate than that.
By contrast, here are results for the same search phrase from Google:

<div class="thumbnail">
<figure>
[![](/images/posts/2024-09-27-goodbye-google-switching-from-google-search-to-kagi/google_fail.png)](/images/posts/2024-09-27-goodbye-google-switching-from-google-search-to-kagi/google_fail.png)
</figure>
</div>

And from DuckDuckGo:

<div class="thumbnail">
<figure>
[![](/images/posts/2024-09-27-goodbye-google-switching-from-google-search-to-kagi/ddg_fail.png)](/images/posts/2024-09-27-goodbye-google-switching-from-google-search-to-kagi/ddg_fail.png)
</figure>
</div>

Neither of these results was helpful in any way[^2].  This is just one example,
but this happened over and over again.

Is paying for a search engine worth it?
---------------------------------------

Let's get this one out of the way: Kagi is a paid service.  It gives up entirely
on the ad-based business model, requiring the users to pay with real money and
not their private data.  The question is, whether it is worth to pay for a
service that everyone else offers for free?

After using Kagi - and paying for it - for a year, to me the answer is a "yes".
Making Kagi a paid search engine finally aligns the needs of the users with the
needs of the customers, because now these are the same people.  I no longer see
search results because the advertisers want me to see them.  The end result is a
service that is worth paying for.  There are some caveats, which I will get to
in a moment.

Kagi offers several subscription plans.  I am not going to comment on the
affordability, since that is going to be different for everyone depending on
income.  I will warn about the free trial though.  Kagi allows you to try a
hundred searches for free.  This is how I started at first, but quickly noticed
that, since the number of searches was limited, I would tend to use DuckDuckGo
by default, and only use Kagi if DDG failed.  That doesn't really give the true
taste of Kagi's usability.  It wasn't until I decided to try the paid plan for a
single month that I really saw the benefits of Kagi.  I also have some
scepticism about Kagi's Starter plan that, at the moment of this writing, offers
300 searches a month.  Kagi quotes statistics from Google and DuckDuckGo, which
claim an average of 100 and 30 (sic!) monthly searches per user, respectively.
I find these numbers unbelievably low.  Kagi offers search statistics, and I can
see there was not a single month when I would use less than 300 searches.  I
typically perform 340 searches a month, though there have been months when that
number went up to nearly 600.  And that is only when using Kagi on the desktop.
My wife, who uses Kagi on desktop and mobile, has an average of 200 searches
more per month[^3].

Features
--------

Searching is obviously the most important thing in a search engine, but Kagi has
a few more features.  Let's have a brief look at some of them.

Firstly, Kagi offers lenses, which can be though of as a predefined set of
filters.  For example, there's an academic lens that will search for results on
web pages related to science and academia.  There are a bunch of others, for
example usenet archives, fediverse forums, programming or recipes.

One kind of lens that deserves a special mention is the [Small
Web](https://blog.kagi.com/small-web).  This is Kagi's initiative to promote
personal websites and blogs, as they can be a great source of original opinions
and knowledge.

Kagi also works on adding LLM capabilities to the search experience.  This is
one feature I am very sceptical about - I don't share the hype around AI - and
the Kagi team definitely is well aware of the concerns surrounding AI and
ensures that user privacy is maintained, and no data generated by Kagi users is
used for training.  As part of the standard payment plans, Kagi offers the Fast
Answer, which can be triggered by appending a question mark at the end of the
query.  This will send the question to a LLM and provide an AI-generated answer,
together with references to sources from which the information was taken.  This
feature works just like all the other large language models of today:
unreliably.  Sometimes it gives good answers, and sometimes it fails, often in
subtle and non-obvious ways.  For this reason, I approach this feature with
caution.  Kagi also offers an AI
[Assistant](https://blog.kagi.com/announcing-assistant) as part of the most
expensive Ultimate plan, but I have not tested it[^4].

There is also the Summarizer, embedded into the Kagi browser plugin (more on the
plugin in the next section), which can summarize the currently viewed web page.
I have only ever used the Summarizer to see if it works, and it seems to do the
job.

Kagi offers a lot of customization options.  It is possible to customize the
layout of the search result page, either by choosing from predefined settings or
by providing a custom CSS for the results page.  This, I have to say, is pretty
cool.  Beyond that, one can create custom lenses and select which pre-defined
ones should be available.  It is also possible to customize which widgets should
be available on the search results page.

Importantly, Kagi does not have search history, which I consider to be a good
privacy feature.  To [quote the founder of
Kagi](https://news.ycombinator.com/item?id=37561264):

> Just to make it clear, Kagi does not link searches to an account already, to
> begin with. Refer to our privacy policy. We simply do not need that data for
> anything and it would be just a liability for us.

That being said, Kagi does offer the option to customize search results by
selecting which pages to consider more important and which ones to ignore.
There's even a [ranking of search stats and website
popularity](https://kagi.com/stats?stat=leaderboard).

And finally, if any of the above features does not work as expected, there's
[Kagi Feedback](https://kagifeedback.org/) that allows to report bugs and
suggest features.  What I find impressive is that the founder of Kagi himself
often responds to bug reports.

Cons to be aware of
-------------------

As already stated, I am very pleased with Kagi and I think it offers better
search results than other search engines I have tried so far.  However, not all
search result from Kagi are good.  For example, Kagi will sometimes point to
those weird generic pages that have paragraphs of text seemingly on the subject,
but not really having any real information.  (Not all of those pages are
AI-generated, so perhaps that's why they are difficult to filter out?)
Sometimes Kagi will include scam sites in the results, and sometimes it will
just not show you the obviously correct results.  A great example of this is
searching for "youtube downloader".  `yt-dlp` is the result that you most likely
want, but instead you get a bunch of SEO spam.  But even with that happening
occasionally, I still think the quality of Kagi search results is above its
competitors.  I have not yet been in a situation where DDG or Google would find
a search result that Kagi couldn't find.

One big problem with Kagi is using it as the default search engine.  This is
tricky for two reasons.  Firstly, Kagi is a paid service that requires one to be
logged in, which presents an obvious problem when browsing in private mode.
Secondly, certain browsers and OSes either only allow a certain set of
predefined search engines or require a lot of effort to add a new search engine.
Kagi's [team struggles to get around those
limitations](https://news.ycombinator.com/item?id=41164462), but from the
perspective of a user, the whole experience can be frustrating.  I use Firefox
on desktop and in order to have Kagi work in private mode I had to install an
extension.  However, on mobile, I was unable to use Kagi at all.  In mobile
Firefox it should be possible to add a new search engine with a custom search
URL, and Kagi provides a session link with a login token to make this work.  For
some reason, this does not work on my phone, although the same exact method
works without problems on my wife's phone.  And then many mobile browsers, for
example Vivaldi, won't even allow you to add a custom search engine.  As a
result, I still have to use DuckDuckGo on my Android.

Another minor issue with Kagi is that it is clearly tailored for Americans.  For
example, a comma is not recognized as a decimal separator in certain contexts.
If I search Kagi for "2.5USD in PLN" it activates a widget that gives me an
immediate response, but if I replace the dot with a comma and search for "2,5USD
in PLN" the widget does not work.  This is minor and things are improving - it
used to be the case that a comma was not recognized at all as a decimal
separator, so asking Kagi for things like "2,5 * 2" would return a range "{2,5}"
instead of "5".

Any map related searches are also far from being perfect, to say the least.
Kagi at least recognizes that and provides easy links to Google Maps and Apple
Maps from their map search.  Long story short, every time I need to find local
information on the map, I revert to Google.

Lastly, image search returns essentially the same results as all the other
search engines, so there is really no difference here.

Summary
-------

For me, Kagi is one of the biggest discoveries of the past few years, and **I
cannot afford not to use Kagi.** It might not be perfect, but it has certainly
improved my web searching experience.  Importantly, it has also given me hope
for a better web, one where the user's needs are at the centre.  I am curious to
see where the project goes, and in particular [how it aims to challenge Google's
search monopoly](https://blog.kagi.com/dawn-new-era-search).

[^1]: Quote comes from "The Anatomy of a Large-Scale Hypertextual Web Search
      Engine" paper, from Appendix A titled "Advertising and Mixed Motives".
      Text of the paper available
      [here](http://infolab.stanford.edu/~backrub/google.html).

[^2]: While writing this post, I also decided to test Bing.  Not only the
      results are garbage, but the results page is absolutely cluttered with
      widgets and whatnot.

[^3]: My wife and I did some investigation of this, since the number of extra
      searches felt surprisingly high.  Turns out that the number is
      artificially bumped, because switching to a tab that wasn't active for a
      while in a mobile browser will cause the page in that tab to be reloaded.
      If you switch to a tab that contains Kagi search result, this reload will
      trigger a new search of the previously searched term.

[^4]: Speaking of Kagi Assistant, I also think Kagi might have created [one of
      the worst commercials ever](https://www.youtube.com/watch?v=Lznc7p43nos),
      which is a real shame.
