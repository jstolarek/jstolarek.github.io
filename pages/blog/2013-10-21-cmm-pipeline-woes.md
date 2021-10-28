---
title: Cmm pipeline woes
date: 2013-10-21
---

Cmm pipeline woes
=================

For the last 4 months I have been working on improving compilation of
intermediate Cmm language used by GHC. Recently I am noticing some recurring
problems in design of the Cmm pipeline.

Cmm
===

I already mentioned [in my earlier post](/blog/2013-10-06-let-no-escape.html)
that Cmm is a low-level language, something between C and assembly. Cmm is
produced from another intermediate language,
[STG](/blog/2013-06-13-getting-friendly-with-stg.html).  A single Cmm procedure
is represented as a directed graph. Each node in a graph is a Cmm Block of low
level instructions. Exactly one Cmm Block in a graph is an entry point - a block
from which the execution starts when procedure represented by a graph is
called. Most Cmm Blocks in a graph have at least one successor, that is node(s)
to which control flows from a given Cmm Block. A Cmm Block may not have a
successor if it is a call to another procedure, i.e. it passes flow of control
to another Cmm graph. Each Cmm Block consists of a linear list of Cmm Nodes. A
Cmm Node represents a single Cmm instruction like store to a register,
conditional jump or call to a function.

Pipeline
========

Cmm representation produced by the STG -> Cmm pass is incomplete. For example
operations on the stack are represented in an abstract way. It is also far from
being optimal as it may contain lots of empty blocks or control flow paths that
will never be taken. That's why we have Cmm pipeline. It takes Cmm
representation produced by the code generator ((That's how we often refer to STG
-> Cmm pass )) and applies a series of transformations to it. Some of them are
mandatory (like stack layout), while others perform optimisations and are
completely optional. Here's a rough overview of the pipeline:

  1. **Control Flow Optimisations.** Optimises structure of a graph by
     concatenating blocks and omitting empty blocks.

  2. **Common Block Elimination (optional).** Eliminates duplicate blocks.

  3. **Minimal Proc-point set.** Determines a minimal set of proc-points ((I
     want to avoid going into details of what are proc-points and why do we need
     them. For the purpose of this post it is sufficient that you consider
     proc-point as a property that might be assigned to a block based on its
     predecessors)).

  4. **Stack Layout.** Turns abstract stack representation into explicit stack
     pointer references. Requires proc-point information computed in step 3.
     Creates stack maps.

  5. **Sinking pass (optional).** Moves variable declarations closer to their
     usage sites. Inlines some literals and registers in a way similar to
     constant propagation.

  6. **CAF analysis.** Does analysis of constant-applicative forms (top-level
     declarations that don't have any parameters). CAF information is returned
     by the Cmm pipeline together with optimized Cmm graph.

  7. **Proc-point analysis and proc-point splitting (optional).** Here the
     pipeline splits into two alternative flows. They are identical except for
     the fact that one branch begins by performing proc-point splitting. This
     means that blocks that were determined to be proc-points are now turned
     into separate procedures. Requires proc-point information computed in step
     3.

  8. **Info Tables.** Populates info tables of each Cmm function with stack
     usage information. Uses stack maps created by the stack layout (step 4).

  9. **Control Flow Optimisations (optional).** Repeat control flow
     optimisations (step 1), but this time this is optional.

  10. **Unreachable Blocks Removal.** Eliminates blocks that don't have a
      predecessor in the Cmm graph.

As an example consider this Cmm produced by the code generator:

```
c4wk:
    goto c4wi;
c4wi:
    goto c4wl;
c4wl:
    goto c4wm;
c4wm:
    if ((old + 0) - < SpLim) goto c4x9; else goto c4xa;
c4x9:
    R2 = _s2Rv::P64;
    R1 = _s2Ru::P64;
    call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;
```

After going through the Cmm pipeline the empty blocks are eliminated and
abstract stack representation (`(old + 0)` and `<highSp>`) is turned into
explicit stack usage:

```
c4wi:
    if (Sp - 88 < SpLim) goto c4x9; else goto c4xa;
c4x9:
    R2 = _s2Rv::P64;
    R1 = _s2Ru::P64;
    call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;
```

Woes
====

During my work on the Cmm pipeline I encountered following bugs and design
issues:

  - In some cases the Stack Layout phase (step 4) can invalidate the minimal set
    of proc-points by removing a block that was earlier determined to be a
    proc-point. However, minimal proc-point information is later used by
    proc-point analysis. GHC would panic if a proc-point block was removed. This
    was bug [#8205](http://ghc.haskell.org/trac/ghc/ticket/8205). I solved it by
    adding additional check in the proc-point analysis that ensures that all
    proc-points actually exist in a graph. This was a simple, one line solution,
    but it doesn't feel right. It accepts the fact that our data is in an
    inconsistent state and places the responsibility of dealing with that on
    algorithms relying on that state. In other words, any algorithm that relies
    on minimal proc-point set after the stack layout must check whether given
    proc-points exist in a graph.

  - I observed that in some circumstances control flow optimisation pass may
    lead to unexpected duplication of blocks (see
    [#8456](http://ghc.haskell.org/trac/ghc/ticket/8456)). After some
    investigation it turned out that this pass began by computing set of
    predecessors for each block and then modified the graph based on this
    information. The problem was that the list of predecessors was not being
    updated as the graph was modified. This problem is the same as the previous
    one: we compute a fact about our data structure and based on that fact we
    start modifying the data but we don't update the fact as we go.

  - Control flow optimisations pass may produce unreachable blocks. They remain
    in the data structure representing the graph, but they are not reachable
    from any block in the graph. The common block elimination pass will remove
    the unreachable blocks but before it does the data is in an inconsistent
    state.

  - Same thing happens later: stack layout pass may create unreachable blocks
    and relies on later passes to remove them.

I listed only the problems I am aware of but I believe there are more and they
will manifest themselves one day. I spent last couple of days thinking how to
solve these issues. Of course fixing each of them separately is a relatively
simple task, but I'm trying to come up with some general design that would
prevent us from introducing inconsistencies in our data structures.

