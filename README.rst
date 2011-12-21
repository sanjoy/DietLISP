DietLISP
--------

DietLISP is an interpreter for an untyped Lisp variant written in
Haskell.  This is a toy project that I worked on during my winter
break in the Doon valley (I can't compile anything larger on my Dell
Mini 1018).

The Language
============

DietLISP is a minimalist, lazy, purely functional lisp.  It does not
have a static type system, and supports macros.

I have tried to model the reduction rules on denotational semantics
instead of resorting to some ad-hoc reduction machinery.  The
reduction rules are specified in Semantics.hs.  As of now, the
interpreter does not try very hard to report helpful error messages.
