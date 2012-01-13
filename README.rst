DietLISP
--------

DietLISP is an interpreter for an untyped Lisp variant written in
Haskell.  This is a toy project that I started working on during my
winter break in the Doon valley.

The Language
============

DietLISP is a minimalist, lazy, purely functional lisp.  It does not
have a static type system, and supports macros.

I have tried to model the reduction rules on denotational semantics
instead of resorting to some ad-hoc reduction machinery.  The
reduction rules are specified in Semantics.hs.  As of now, the
interpreter does not try very hard to report helpful error messages.

It is possible to write something resembling monads with an
appropriate use of macros and higher order functions.  You can see an
example in ``samples/Monads.dlisp``.

IO
~~

IO in DietLISP is implemented the way I used to think Haskell's IO
works.  Turns out I was wrong, but I still implemented my simplistic
model since it works and is something I clearly understand.

The idea is this: the outside world is modelled using a ``World``
object, which is passed around by the DietLISP program itself (so
there is very little magic).  The only way to obtain this object is to
use the special operator ``begin``, which evaluates the expression
that follows it with the current ``World`` as an argument.

There are two IO functions, ``read`` and ``write``.  ``read`` takes
the ``World`` and returns an (integer) input with the changed
``World`` (in which the input has been read) in a list.  ``write``
takes the ``World``, an object to print and returns a new ``World``
(in which the object has been printed).

The expression passed to ``begin`` must eventually evaluate to a
``World``.  In essence ``World`` models the universe around the
expressions being evaluated and a side-effecting expression is a
mapping from an old universe to a new one.

There is a small example in ``samples/dntIO.dlisp``.  I think the code
can simplified further by a judicious use of macros and higher-order
functions.

Nameless macros
~~~~~~~~~~~~~~~

DietLISP has this concept of a nameless macro, which is basically to
``defmacro`` what ``lambda`` is to defun.  This pushes the semantics
of macros a little closer to those of functions.  I'm not sure whether
this will add an interesting, orthogonal expressiveness; and I will
remove it if I find it makes writing a compiler much harder.

Future Goals
============

This is a pet project of mine, and hence there are no clearly set
goals.  However, I wish to work on the following as and when I get
time:

Stronger static guarantees
~~~~~~~~~~~~~~~~~~~~~~~~~~

**Stricter AST**

The current representation of the AST is not as rigid as I want it to
be.  After parsing, even, say, a list corresponding to an ``if``
expression can have five elements, when there should only be four.
DietLISP should figure out *what* an expression is and, say, put it in
a ``IfE`` instead of an generic ``ListE`` during the parsing stage.
Currently these kinds of errors are caught and reported during
execution time, which I think should not be the case.


**Type system**

DietLISP currently does not have a statically typed system and reports
type errors at runtime.  I'd like to implement System F's type system
(with Rank N types and what not) sometime in the future.  If I'm able
to decipher dependent types someday, then that too should be something
interesting to implement.

I think implementing a type system will be easier once I have a
stricter AST.

**A compiler**

The operational behaviour of the interpreter is not very efficient.
For instance, it does not run in a bounded control context.  I've been
saving CPS and other such nice things for the time I decide to
implement a compiler, probably to C or LLVM.  I think this will be a
good next step after implementing a type system.


Texts
=====

As it is probably evident by now, this is more of a academic project.
I found the following texts very helpful:

 - Structure and Interpretation of Computer Programs (Hal Abelson,
   Jerry Sussman, Julie Sussman)
 - Essential of Programming Languages (Daniel P. Friedman, Mitchell
   Wand, Christopher T. Haynes)
 - Design Concepts in Programming Languages (Franklyn Turbak, David
   Gifford, Mark A. Sheldon)

I'd at recommend at least the first two texts to any serious software
engineer.
