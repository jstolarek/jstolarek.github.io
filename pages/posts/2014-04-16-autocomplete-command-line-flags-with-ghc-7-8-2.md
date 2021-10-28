---
title: Autocomplete command-line flags with GHC 7.8.2
date: 2014-04-16
---

Autocomplete command-line flags with GHC 7.8.2
==============================================

[GHC 7.8.2](https://www.haskell.org/ghc/download_ghc_7_8_2) has been released
just a few days ago ((Please ignore 7.8.1 release. It shipped with a [bug that
caused rejection of some valid
programs](https://ghc.haskell.org/trac/ghc/ticket/8978).)). This is the first
official release of GHC that contains my contributions. Most of them are
improvements in the code generator and are thus practically invisible to most
users. But I also implemented one very nice feature that will be useful to an
average Haskeller. GHC now has `--show-options` flag that lists all command-line
flags. This feature can be used to auto-complete command-line flags in shells
that support this feature. To enable auto-completion in Bash add this code
snippet to your `~/.bashrc` file:

```bash
# Autocomplete GHC commands
_ghc()
{
    local envs=`ghc --show-options`
    # get the word currently being completed
    local cur=${COMP_WORDS[$COMP_CWORD]}

    # the resulting completions should be put into this array
    COMPREPLY=( $( compgen -W "$envs" -- $cur ) )
}
complete -F _ghc -o default ghc
```

From my experience the first completion is a bit slow but once the flags are
cached things work fast.

