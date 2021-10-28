---
title: Let no escape!
date: 2013-10-06
---

Let no escape!
==============

I observed that the number of people familiar with Core and using it for
debugging purposes is relatively big to the number of people familiar with other
two intermediate representations used by GHC further in the pipeline: STG and
Cmm.  Some seem to treat these two a bit like black magic :-) I have to admit
that I used to treat them like this before I came to MSR to work on the GHC
backend. Now I am a bit more familiar with code generator and I'd like to share
some of my knowledge. Today I will present an optimization called
"let-no-escape" (LNE for short). I believe it is almost unknown amongst
Haskellers and GHC hackers. Main reasons for that are: a) it is performed in the
code generator (STG-to-Cmm pass) and, as I said, people tend to stay away from
that part of the compiler; b) it was not described in any paper ("We didn't
consider it to be worth a paper" - Simon Marlow), nor it is described on GHC
wiki - the only chance to learn about was to find it in the source code. My plan
is to explain what and why LNE does.

Motivation
==========

Consider this imperative code in C:

```
if (y > 0) {
  x = 3;
} else {
  x = 4;
}
...x...// use x
```

Earlier I mentioned the Cmm intermediate language used by GHC. It is a low-level
imperative language, something between C and assembly. If we were to rewrite
above C program into Cmm we would get something like this:

```
     if (y > 0) goto L1; else goto L2;
L1 : x = 3
     goto L3;
L2 : x = 4;
     goto L3;
L3 : ....x... // use x
```

In both C and Cmm code we would expect the generated assembly to check value of
`y`, perform a conditional jump to one of the branches, set value of `x` to `3`
or `4` and finally perform unconditional jump from the branch to code that
follows the `if` construct (i.e. `L3` label in Cmm version).

All of this seems obvious and looks very simple, but it gets more complicated
when we start thinking in a functional setting. How do we implement this logic
in Haskell? Well, we could do something like this:

```haskell
let j x = ...x... -- use x
in case (y > 0) of
      True  -> j 3
      False -> j 4
```

We introduced a binding `j`, which corresponds to code after the conditional
(again, `L3` in the Cmm example). `case` expression corresponds to if
statement. In the branches we just make a call to `j` and pass desired value of
`x` as a parameter. There's a problem here though. If we simply compile this
code then `j` will be compiled as a function. It will be a closure with two
entry points (slow and fast) and possibly stack check and heap check at the very
beginning. So with C we had an unconditional jump, whereas here we get a
function call with all its overhead. This is very bad and we want something
better. One easy way to avoid call to `j` would be inlining it in every case
alternative. But this duplicates code so this is no good either. We really want
to produce code that is as fast as produced by C and without unnecessary
duplication. Let-no-escape optimization allows us to achieve that.

Let-no-escape
=============

When we compile core to STG we distinguish between two types of let bindings:
normal and let-no-escape ones. A binding is a let-no-escape binding if it is
non-updatable (so it must have some parameters), it is called in a tail position
with exactly the right number of arguments and it is "guaranteed to be entered
before the stack retreats - i.e. it is not enclosed in a heap-allocated closure
or passed as an argument to something" ((Quote from source file
`compiler/stgSyn/CoreToStg.lhs`)). Normal bindings - i.e. bindings for which at
least one of these conditions does not hold - are compiled as a closure and
follow the calling convention i.e. they expect their parameters to be passed in
some particular registers or on the stack. Let-no-escape bindings are compiled
in a special way: they are not closures and don't follow the calling
convention. Instead they are compiled as a normal block of Cmm code that expects
its entry parameters to be located in some particular local variables (which
will later be mapped to hardware registers or spilled to the stack). To "call" a
LNE binding we place parameters in the correct local variables and jump to the
block of Cmm code.

Let's look once again at our example program:

```haskell
let j x = ...x... -- use x
in case (y > 0) of
      True  -> j 3
      False -> j 4
```

The binding for `j` is a let-no-escape one: it is non-updatable, in a tail
position, all call sites pass exactly the required number of arguments and it is
not passed as a parameter. This means that we can compile `j` as a let-no-escape
binding - it will not be a closure and will expect its parameter in a local
variable. Call sites for `j` - i.e. `True` and `False` alternatives of a case
expression - need to place the value of argument `x` in a place where `j`
expects it. This brings us to Cmm code that I presented at the top of this post:

```
     if (y > 0) goto L1; else goto L2;
L1 : x = 3
     goto L3;
L2 : x = 4;
     goto L3;
L3 : ....x... // use x
```

The conditional `if` implements case expression that selects one of
alternatives. `L1` is the `True` branch, while `L2` is the `False`
alternative. We compiled `j` in such a way that it expects its parameter to be
passed in a local variable `x`, so every call site needs to initialize `x` with
the value it wants to pass to `j` as an argument. After initializing the
argument we perform unconditional jump to code that implements the binding
(instead of making a call). In this example `L3` implements the `j`
let-no-escape binding. It is not a callable closure, but a normal block of Cmm
code which expects its argument to be passed in a local variable `x`. In this
way we compiled a functional program to efficient low-level code that is
comparable with C.

Summary
=======

This was a conceptual explanation of what are let-no-escape bindings and why we
use them. A separate question is "how is this optimisation implemented by
GHC?". Relevant code is located in a couple of places, mostly in `codeGen/`
directory (STG -> Cmm pass). If you want to explore implementation details a
good starting point might be `getCallMethod` function and `CallMethod` data type
in `StgCmmClosure` module and `cgIdApp` in `StgCmmExpr` module.

