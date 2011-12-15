DietLISP
--------

DietLISP is an interpreter for an untyped Lisp variant written in
Haskell.  This is a toy project that I worked on during my winter
break in the Doon valley (I can't compile anything larger on my Dell
Mini 1018).  One of my goals was a simple and elegant design that is
easy to extend, and this shaped many of my decisions.  Choosing
Haskell as the programming language was one of them.  Granted, writing
a Lisp interpreter in Haskell is a bit like cheating, but this is not
an exercise in programming language implementation but an exercise in
programming language semantics.  For the same reason, instead of
resorting to some ad-hoc reduction machinery, I have tried to model
the reduction rules on denotational semantics -- another thing Haskell
makes easy.  Also, as will be apparent from the code, this interpreter
makes no attempt to report helpful error messages.  You'll need to run
this interpreter in a terminal that supports unicode (since I display
⊥ on error).

Features
========

DietLISP has the following features (I have been informal in the
distinction between the DietLISP language and this particular
implementation):

 * Expressions are evaluated eagerly.
 * Expressions are pure and have no side-effects.
 * There is no static type-system.
 * Macros are supported.
 * Non-strict (f ⊥ is not necessarily ⊥).

There are some code samples in the samples/ directory.
