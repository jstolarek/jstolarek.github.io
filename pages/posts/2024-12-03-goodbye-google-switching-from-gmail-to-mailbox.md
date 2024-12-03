---
title: Goodbye, Google&#58; Switching from GMail to Mailbox
date: 2024-12-03
---

Goodbye, Google: Switching from GMail to Mailbox
================================================

In recent years, I have been making attempts to minimize using services provided
by Google, the primary concern being privacy.  In my [earlier
post](2024-09-27-goodbye-google-switching-from-google-search-to-kagi.html), I
wrote about using [Kagi](https://www.kagi.com) as my search engine of choice.
Today, I want to talk about [Mailbox.org](https://mailbox.org/), a paid email
provider that I migrated to from GMail.

My expectations and why previous email providers didn't meet them
-----------------------------------------------------------------

I heavily rely on email in my daily life.  I always had at least two accounts: a
private one and a professional one at my current workplace.  Most of the time, I
had more than one mailbox for work.  In order to manage multiple email accounts
conveniently, I use a desktop email client.  Thus, a basic requirement for me is
that an email provider allows reading and sending emails via a client.  This
sounds like a really, really trivial requirement, but you will see in a moment
that for some email providers this is far from obvious.

For the most part of the last 25 years, I have used a free email account from
[Onet](https://www.onet.pl), a Polish news portal and email provider - think
Yahoo.  It being free came at a price of receiving advertising emails directly
to the inbox.  Luckily, it is trivial to filter out such mails with a custom
filter in the email client and never see those adds.  As such, I was happy with
services provided by Onet.  However, at some point they started to silently put
restrictions on emails send via desktop clients.  In particular, sending emails
that only contained links or attachments, or were simply too short, resulted in
them being silently dropped by the SMTP server.  Imagine a situation where you
are making arrangements with someone and reply simply with "Yes", and that email
gets silently dropped and never makes it to the other person.  Or you just want
to send someone a link, but that person never receives it.  I have been in
multiple such situations without realizing it.  Dropping emails for no good
reason would be bad enough in itself.  But doing so silently in such a way that
the sender does not even know that the message was not delivered is simply too
much.  When I realized what is going on, and verified with the provider that
this is an intended behaviour that's part of their "security policy" (sic!), I
knew I have to start looking for a new email service.

Unfortunately, at this point in time, I was heavily occupied with events in my
private life and did not have the time to properly research paid alternatives.
And so I went with an easily available solution: GMail.  Well, that was a
mistake.  Firstly, GMail complicates usage of clients that don't support OAuth
(mine doesn't).  Secondly, GMail places sent emails in the inbox.  If you send
an email via a client, your client will then download that email into your
inbox, showing it as a new email.  This is a major annoyance.  Again, it can be
sorted out with some extra work and custom filters, but this is not how email
should work.

I lived with GMail annoyances for a couple of years, until last November I
finally decided to bite the bullet and move to a new email provider.

Mailbox.org: Pros and Cons
--------------------------

After some research, and recommendations from friends, I decided on
[Mailbox.org](https://mailbox.org/), which offers a good set of features at a
competitive price.  As mentioned earlier, one of my motivations to move away
from Google are privacy concerns.  Mailbox.org operates from Germany, which
means it must follow the EU's strict data protection laws.  To me, this was an
important argument in favour of using Mailbox.org.

Mailbox.org offers lots of neat little features that one does not typically get
from free email providers such as GMail.  These include multiple email aliases,
disposable addresses, encryption support, detailed SPAM protection settings, and
mailbox backups, among others.  Aside from email, Mailbox.org also provides a
suite of online applications.  These include:

  * An address book and calendar.
  * A TODO list that allows to manage tasks, track their progress, set
    deadlines, attach files, etc.  I couldn't figure out a way to integrate it
    with a calendar, but I admit I haven't tried very hard.  Perhaps there's a
    way of doing it.
  * Cloud storage with 5GB of space (in the Standard plan).
  * XMPP chat
  * Video conferencing based on [OpenTalk](https://opentalk.eu).
  * Etherpad, a lightweight alternative to Google Docs.
  * Creating polls and event scheduling based on
    [Framadate](https://framadate.org).

It is good to see alternatives to GSuite finally being offered, though I admit I
have not tested most of these tools, since I rarely collaborate with others via
such tools and tend to work on the desktop.  I only really tried using Etherpad
and, frankly speaking, the experience wasn't great.  Firstly, Etherpad seems to
forget which edits were made by whom, i.e. rather than associating edits with a
Mailbox.org account it seems to be using cookies that have short expiration
dates.  Secondly, it frequently resets to default display settings, which is
quite annoying - presumably cookies, again.  But worst of all, it isn't
reliable.  I had a situation where a document I shared with someone became
completely broken and unresponsive.  After contacting the support, I was told
that they had an infrastructure problem and the only way to solve the problem is
to create a new document and copy the contents of the broken document there.  It
was a one-time incident, but something like this should not ever happen if the
tool is to be considered reliable.

Beyond that, Mailbox.org offers [good technical
documentation](https://kb.mailbox.org/en/private/) - and I really mean that.
This documentation does not shy away from providing full technical details so
that a knowledgeable user gets all the information they need.

And lastly, the service works with an email client without any problems.  This
should be obvious, but as demonstrated by my experiences, it isn't always the
case.

There are a couple of cons, though, aside from the already mentioned Etherpad
incident.  Possibly the biggest usability issue are strict SPAM filters.  By
"strict" I mean following all sorts of standards, that many big providers on the
internet (such as Microsoft) don't follow.  As a result, some incoming emails
don't get delivered or arrive delayed.  In particular, I have found
[greylisting](https://en.wikipedia.org/wiki/Greylisting_(email)) to cause
trouble.  Thankfully, SPAM filter behaviour can be modified in the settings, but
I don't like how the default settings are problematic, even if, strictly
speaking, the fault does not lie with Mailbox.org.

Mailbox.org offers online technical support - which in itself is good and yet
another thing one does not get with free email services - but I have found that
support to be highly incompetent.  I contacted support on three different
occasions, and each experience was bad.  I encountered consultants that were
arrogant or lacked knowledge, trying to convince me that the behaviour I am
experiencing is a problem caused by my browser and sending me through many
pointless debugging steps, only to admit in the end that this is actually an
intended behaviour of the service.

Speaking of "intended behaviours of the service", one of them is repeatedly
logging out of the web client.  Not that I use the web client frequently, but
having to log in every other day was annoying when I first started using the
service and frequently experimented with various settings.  I initially thought
that this was a bug, since the settings allow to disable automatic logout, but I
was eventually told that this is all intentional - despite what the settings
seem to imply.

There are a few more minor annoyances in the web client, such as the inability
to click any links in emails moved to the SPAM folder.  So if you want to click
an unsubscribe link you first need to move an email to the inbox folder, click
the link, and move it back to spam.  But again, this isn't really a huge problem
for me.

One last issue is a really minor one, but when I initially got the new email
address I kept mistaking the `.org` domain with `.com` and on several occasions
nearly provided an incorrect email address.

Summary
-------

After many years of struggling with free email providers, I finally have a
mailbox that works like it should.  I regret not making this move earlier, and I
regret even more ever getting involved with GMail.  This was a mistake that I
should not have done, given how hard it is to entirely phase out an old email
address.
