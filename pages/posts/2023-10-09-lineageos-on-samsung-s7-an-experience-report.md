---
title: LineageOS on Samsung S7&#58; An Experience Report
date: 2023-10-09
---

LineageOS on Samsung S7: An Experience Report
=============================================

I don't have much love for smartphones.  While I admit they can be useful at
times, I absolutely detest their ecosystems &#8210; both Android and Apple
&#8210; that primarily exist not to fulfil the needs of the users but those of
the big tech companies.  This is a regrettable state of things and any efforts
to change this have not seen widespread success.  That being said, a year ago I
decided to run an experiment.  Android is after all a free, open-source
software.  I thought there should be a way of installing a vanilla version of
Android that lacks Google spyware that manufacturers bundle with the phones.
After exploring the topic I decided to install
[LineageOS](https://lineageos.org/), an Android-based free and open source
operating system for smartphones, on my Samsung S7.

Several weeks ago the said Samsung phone died due to internal memory failure.  I
think this is a good moment to share an experience report from a year of using
LineageOS.


Android ecosystem
-----------------

Vanilla Android is an open source software.  If so, then how come it is such a
threat to user's privacy?  Well, that's because of [Google Play
Services](https://developers.google.com/android/guides/overview)[^1], a set of
libraries that phone manufacturers install on retail distributions of Android.
Crucially, many applications require Google Play Services in order to run
correctly.  Hence comes the first choice one needs to make when deciding to use
LineageOS.  It is possible to use LineageOS without Google Play Services, but
some applications will refuse to work or will experience issues.  See
[Plexus](https://plexus.techlore.tech/) project for a community-maintained list
of applications.  Alternatively, it is possible to install Google Play Services
on LineageOS and have all applications run without problems, but to me this
defeats the purpose of using LineageOS.

There is, however, a third option &#8210; an open-source implementation of
Google Play Services known as [microG](https://microg.org/).  I did some
superficial reading on microG and concluded that I don't fully understand how it
works.  In particular, I couldn't figure out how the presence of microG affects
my privacy.  As a result I decided to use LineageOS without microG, which is
exactly how LineageOS is distributed by default.


Obtaining LineageOS
-------------------

Let's talk about getting LineageOS onto your phone.  Developers maintain [a list
of supported devices](https://wiki.lineageos.org/devices/), for which official
builds are available.  The list isn't short, but don't be surprised if your
phone is not there.  Mine used to be supported at some point in the past, but
that support was dropped due to lack of active maintainer.  This presented me
with the first serious problem: how do I obtain an image of LineageOS if it
is not available on the official website?

Technically, there are two solutions.  One is to use unofficial ROMs
(i.e. pre-built images) available at the [XDA
Forums](https://forum.xda-developers.com/).  These images often come with extra
software, most commonly the already mentioned microG or GSF.  They are also
often built on a version of Android that is higher than the one officially
available for the phone.  However, there is a big issue of trust here.  It
generally seems that the uploaded builds are considered trusted, in a sense that
people just trust that the uploaded images are safe.  Except there is no way of
really verifying that the images do not contain any malware or spyware or
whatnot.  My instinct was not to use those builds.

I was left with the second solution: building the ROM image myself.  Since my
phone used to be officially supported there were build instructions available
for it, except they turned out to be highly inaccurate.  Long story short, it
took me 3 weeks to solve the problems I was running into while building, which
is more than most users are willing to spend on installing a new system to their
phone.  Even worse, after installing my newly-built image it turned out that the
Bluetooth doesn't work.  A case of missing firmware perhaps?

In the end I managed to find an archive of old official LineageOS images located
on a Russian FTP server that also hosted various leaked corporate data and a
North Korean Linux distribution known as RedStar OS.  Admittedly, this source
was more dubious than the XDA Forums, but the archive of images looked
legitimate and so I put my security concerns aside.  This image turned out to
work without any issues, including the previously broken Bluetooth.


Flashing OS image onto the phone
--------------------------------

Once I obtained the installation image I had to flash it onto my phone.  In the
past this typically required some sort of jailbreak.  Nowadays it seems that
more and more manufacturers permit installation of custom images without a need
for jailbreaking the phone[^2].  This was indeed the case for my Samsung S7.

Image installation is a two stage process.  Firstly, I had to install a custom
recovery image onto the phone.  My understanding of a recovery image is that
this is a low-level bootloader that allows installing a custom OS image as well
as performing low-level management of the phone.  In my case the recovery image
contained [TWRP](https://twrp.me/faq/whatistwrp.html).  Once the recovery image
was installed I could then flash the LineageOS image.  All of that was done by
connecting the phone to a PC via a USB cable and required having two tools on my
Linux - `adb` and `fastboot`.

I have to say that booting LineageOS for the first time was stressful.  I was
afraid that I might brick my phone, but none of that happened.  All worked as
expected and the new system booted without problems.


Getting applications
--------------------

Once the system was up and running I proceeded with installing applications on
it.  One method of doing it is installing applications distributed in .apk files
via USB.  This is not a convenient way of getting new software onto the phone,
but it is required to at least install [F-Droid](https://f-droid.org/).  F-Droid
is an application store, except that it contains open source applications[^3].
Using F-Droid I installed [Aurora
Store](https://github.com/whyorean/AuroraStore), an open-source Google Play
Store client that allows you to install applications from the official store.
At this point one can just install needed applications in the usual way.


The user experience
-------------------

Let's talk about the most important thing - how was the user experience?  In
short, it was good but somewhat worse than the original OS provided by Samsung.
Here is a list of problems I ran into.

Firstly, the official Samsung OS was based on Android 8, whereas LineageOS 14 I
installed onto the phone was based on Android 7.  That in itself made for a
noticeable downgrade.  It took me quite a while to get used to the look and feel
of applications shipped with LineageOS.  In particular, I didn't like how some
of the applications, e.g. SMS app, did not even allow me to change the colour.
Note also that we are talking about very old Android and LineageOS versions.  I
suspect that both the looks of applications as well as customization options are
very different in the more recent versions.

Another downgrade that was obvious from the very beginning was the keyboard.  It
not only, similarly to other system apps, lacked customization options but it
also provided noticeably worse hints.  I had to find a replacement.  This was a
difficult choice to make.  Sure, hints in Google's GBoard are great, but using
it comes at the expense of privacy.  Indeed, keyboard software is at the core of
a system's privacy and security since anything you type (emails, passwords)
comes through that application.  After extended research I decided to go with
[AnySoftKeyboard](https://anysoftkeyboard.github.io/).  I don't know if this was
the best choice, but it definitely was an acceptable choice.  But again I have
to stress: the quality of hints was noticeably worse than with GBoard.  At least
for the Polish language, which I primarily use.

I mentioned the Aurora Store as a client to Google Play Store.  One nice thing
it can do is allow application downloads via an anonymous account.  Obviously,
anonymous login will not allow you to access paid apps assigned to your Google
account, if you happen to have such.  What was an issue is that the app would
break every now and then.  This is inherent in these kinds of open-source
replacements for proprietary clients: every time the API changes the client app
breaks.  On certain occasions I also found that anonymous downloads did not
work, but logging into my Google account would solve the problem[^4].  One very
nice feature that Aurora Store has, but Google's official software lacks, is the
ability to filter apps based on in-app advertisements and reliance on GSF.  This
is a great feature and I miss it now that I'm back on a Google-based Android.

As mentioned earlier, applications that require GSF can experience problems.
This will range from certain functionalities being broken, e.g. Discord not
showing notifications, to not running at all.  I personally was not seriously
affected by this, but I am not a heavy smartphone user.  In particular I do not
use social media and their associated applications, and from what I read these
can exhibit problems.  The only application that I had to give up was InPost
Mobile, which is an app by a delivery company that operates a dense network of
parcel lockers throughout Poland.  This wasn't a big problem though since I can
still use these parcel lockers via SMS messages.

I also experienced two serious usability issues.  The first one was with the
loudspeaker.  Apparently, LineageOS couldn't do echo cancellation so whenever I
switched to a loudspeaker during a call, the caller would hear their own voice.
This rendered the loudspeaker practically unusable.  The second issue was photo
quality, which had decreased dramatically with photos coming out blurry and with
a lot of grain.  I experimented with several photo taking applications,
including ones that have really good reviews on the web, but none of them
improved the situation, and many made even worse photos.  **As a result my phone
became almost useless as a camera and I ended up borrowing my wife's phone
whenever I had to take photos.  This is probably the single biggest downside I
experienced from switching to LineageOS.**

Another minor issue I experienced was when attempting to contact Discord support
with a bug report.  As soon as they heard I am on an Android without GSF they
dismissed me by saying they *"only support Android devices that have the Google
Play Store pre-installed"*.  I recognize this is not a technical issue with
LineageOS itself, but it might be worth being aware of this kind of attitude
from the developers.


Summary
-------

The above list of encountered issues might look discouraging, but for me the
pros of using LineageOS outweigh the cons.  Most importantly, having a system
with no Google services that constantly spy on me was a big win.  Of course,
proprietary apps still have their own telemetry and it doesn't look like there's
anything that can be done about it in the Android ecosystem, other than a
radical move of not using such apps.  I also realize that for some people
stepping away from using Google services might be a radical move in itself.

At no time of using LineageOS had I thought of switching to something else.
This is in stark contrast with my current smartphone.  After my S7 died I had to
get a new phone ASAP, and being temporarily low on budget I went with the Xiaomi
Redmi Note 10 Pro.  Oh my, was that a mistake.  The phone comes with
advertisements built into system applications, including aggressive
advertisements that take over the whole screen.  Theoretically these ads can be
disabled, but sometimes that doesn't work and ads display anyway.  There's also
lots of annoying bugs and, despite good technical specs of the phone, the
overall experience isn't great.  Luckily, LineageOS is officially supported on
the phone so I see myself installing LineageOS again at some point in the
relatively near future.

[^1]: Google Play Services are also sometimes referred to as Google Software
      Framework &#8210; shortened as GSF &#8210; and I will use both terms
      interchangeably.

[^2]: Keep in mind that this usually voids the warranty.

[^3]: In the retro-modding community you'd just call them homebrew.  Not sure
      whether the term applies here.

[^4]: This means logging into my account only in Aurora Store, not on the phone
      itself.  Since LineageOS comes without GSF there is even no way to log
      into Google account on the phone.