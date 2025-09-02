---
title: Haskell file reading pitfall
date: 2012-04-14
---

Haskell file reading pitfall
============================

I decided that I should start a small project in Haskell in order to practice
the language. I hope that I'll be able to make the first public release
somewhere in a reasonable future and boast about it on the blog, but meanwhile
here's a report from the trenches. Two days ago, while I was developing the
code, I fell into a Haskell newbie pitfall. I spent nearly three hours trying to
find a solution to a relatively simple problem. Now that I've found it it's time
to share it.

The problem
===========

My application reads the contents of a text file, processes it and writes the
processed contents to the same file it was initially read from. I created two
simple functions to act as a back-end for my simple text file database:

```haskell
loadTextFileDataBase :: String -> IO [Task]
loadTextFileDataBase databaseFile =
    withFile databaseFile ReadMode (\handle -> do
        contents <- hGetContents handle
        return (read contents))

storeTextFileDataBase :: String -> [Task] -> IO ()
storeTextFileDataBase databaseFile tasks =
    writeFile databaseFile (show tasks)
```

That's pretty simple I/O in Haskell based on
[LYAH](http://learnyouahaskell.com/input-and-output#files-and-streams).  The
`withFile` function opens the file in ReadMode, passes the handle to that file
to a lambda and closes the file after the lambda ends. Inside my lambda the file
is read lazily as a `String`, which means that the file is read only when the
data from it is requested. The `String` is parsed to become a `[Task]` type
(`read` function) and the result of parsing is wrapped into an IO action using
the `return` function. Writing to a file is a simple one-liner that hopefully
requires no detailed explanation. Reading and writing of my own data type `Task`
is possible because it derives `Read` and `Show` typeclasses. It's a very nice
way of data serialization in Haskell. I'm not sure if it's very efficient,
though it's easy to use. I compiled the code and tried to run it only to get:

```
Prelude.read: no parse
```

Houston, we have a problem! I had a feeling that this has something to do with
the lazy reading of the file...

A solution
==========

I started debugging right away. [GHCi has a
debugger](http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci-debugger.html),
but I didn't bother to learn it yet, so my task was a bit more complicated than
it could've been. Debugging in Haskell using trial-and-error method isn't that
easy. For example, you cannot display text on the console just like that - you
have to bother with I/O actions, type signatures and so on, but it can be done.

Anyway, I started to break my program into the simplest possible components to
make sure that they work. I commented out the part of the program responsible
for writing to a file, so now my program was supposed only to read data from a
file, parse and display it. The error persisted. I replaced the `return`
function with `putStrLn` to display my list of Tasks on the console so I could
be sure that the data is read from the file. Then I used GHCi to make sure that
this string can be parsed correctly. In GHCi everything worked.

I was getting more and more desperate. I've already spent over two hours on
that. I was looking at the documentation of
[System.IO](http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO.html)
module where I accidentally saw
[readIO](http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO.html#v:readIO)
function. Description of that functions says: _"The readIO function is similar
to read except that it signals parse failure to the IO monad instead of
terminating the program."_ I wasn't exactly sure what it means, but the type
signature of this function was `Read a => String -> IO a` and, based on that
signature, I saw that I can replace `return (read contents)` with `readIO
contents`:

```haskell
loadTextFileDataBase :: String -> IO [Task]
loadTextFileDataBase databaseFile =
    withFile databaseFile ReadMode (\handle -> do
        contents <- hGetContents handle
        readIO contents)
```

The types were OK, so this expression should have some reasonable output. And it
worked! The `no parse` error was gone.

An explanation
==============

Now it's time for the most important part: understanding what actually happened
and why the solution works. As always, the great community at #haskell IRC
channel provided invaluable support. It turns out that mu suspicion about lazy
reading of the file was entirely correct. The file was opened, no reading was
done since `hGetContents` is lazy, and it was closed. At the time when contents
was requested, the file was already closed. This caused the error. The
definition of `readIO` looks like this:

```haskell
readIO :: Read a => String -> IO a
readIO s =  case (do { (x,t) <- reads s;
                  ("","") <- lex t;
                  return x } ) of
                   [x] -> return x
                   []  -> ioError (userError "Prelude.readIO: no parse")
                   _   -> ioError (userError "Prelude.readIO: ambiguous parse")
```

This basically forces the evaluation of condition in case-of expression, which
effectively leads to reading the file contents[^1].  During the discussion I was
also suggested to read and parse the file using this function:

```haskell
loadTextFileDataBase = fmap read . readFile
```

This uses [the Functor
typeclass](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass)
and I don't fully understand yet why this works the way it works. Besides, I
wouldn't be able to use this function in my program because I read from and
write to the same file. This means that I need more explicit control of
`fileOpen` and `hClose`.

Conclusions
===========

There are a few lessons learned. First of all, be very careful with
`hGetContents`.  I think that it would be wise to avoid it completely if
possible.  Second, Haskell type's system is very powerful. Recall that I found
the solution thanks to the type signature that fit in my code. Third, learn the
debugger.  Fourth, learn the more advanced features of Haskell, like functors,
monads and so on.  They allow to make the program a whole lot shorter. Five,
learn the API.  I was lucky to stumble upon the `readIO` function, but I could
as well spend two more hours on debugging.

[^1]: **Added 19/04/2012:** I think this sentence needs more explanation.
Case-of expression is lazy. The reason why it gets evaluated is the fact that we
are matching the condition against three different patterns: `[x]`,`[]` and
wildcard `_`. The program needs to know which pattern should be matched and
therefore evaluates the value of the condition, thus forcing the reading of the
file.
