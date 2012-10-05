DietLISP
========

DietLISP is an interpreter for an untyped Lisp variant written in
Haskell.  This is a toy project that I started working on during my
winter break in the Doon valley.

The Language
------------

DietLISP is a minimalist, lazy, purely functional lisp.  It does not
have a static type system.  The distinguishing feature of DietLISP is
that all executable things in it are *semi-operatives*.

Semi-Operatives
~~~~~~~~~~~~~~~

The notion of a semi-operative is similar to that of an *operative* in
the Kernel programming language.  Semi-operatives combine macros and
functions into one unified concept but are more restrictive than
*fexprs*.

I'd like to describe a semi-operative as *a hook into the denotational
semantics of the interpreter*.  You see, in a regular lisp, functions
don't directly influence the interpreter and macros are hooks into the
*parser*.  By going a little deeper, we get a macro-function hybrid.

An operative, like a macro, gets the bits of AST it has been invoked
with, and (unlike a macro) the environment where it was invoked.  It
has access to functions that evaluate an AST in a specific environment
(``eval`` and ``eval*``), and functions to extend the environment
(``add-binding``).  The result of the operative invocation is the
value (rather, the *domain value*) the interpreter gets by executing
the operative.  Thus, it is useful to think of operatives as hooks
that arbitrarily map program phrases into domain values.

As an example of what this makes possible, look at
``samples/SimpleOperatives.dlisp``::

  ;; The #builtin# directives return an operative defined inside the
     interpreter in Haskell.  global-bind binds them to the global
     lexical scope.
  (global-bind let (#builtin# #let#))
  (global-bind eval (#builtin# #eval#))

  (global-bind wormhole (operative () env (eval env x)))

  (let (x 42) (wormhole)) ;; Prints 42, since wormhole evaluates x in
    the environment inside let x (42)

``eval`` evaluates the first ast in the environment passed as the
second argument.  ``eval*`` evaluates the first ast in the context of
the current environment and then evaluates this *result* in the
context of the environment passed as the second argument (the first
argument not evaluating to an ast is an error).  This allows you to
write functions like this::

   (operative (number) env
     (let (evaluated-number (eval* env number))
       (+ evaluated-number 5)))

``eval*`` first evaluates ``number`` in the current lexical scope
(which gives us the AST passed to the operative during evaluation) and
then evaluates *that* ast using ``env`` (which gives us some result to
which we then add 5).

Hopes and fears
~~~~~~~~~~~~~~~

So, will this work?  I don't know.  I have a hunch semi-operatives
might lead to a cleaner and more orthogonal language; I'll keep
updating this README as I explore and learn more.

Texts
-----

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
