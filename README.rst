DietLISP
========

DietLISP is an interpreter for an untyped Lisp variant written in
Haskell.  This is a toy project that I started working on during my
winter break in the Doon valley.

The Language
------------

DietLISP is a minimalist, lazy, purely functional lisp.  It does not
have a static type system.  The distinguishing feature of DietLISP is
that all executable forms in it are *semi-operatives*.

Semi-Operatives
~~~~~~~~~~~~~~~

The notion of a *semi-operative* is similar to that of an *operative*
in the Kernel programming language.  Semi-operatives combine macros
and functions into one unified concept but are more restrictive than
*fexprs*.

I like to describe a semi-operative as *a hook into the denotational
semantics of the interpreter*.  You see, in a regular lisp, functions
don't directly influence the interpreter and macros are hooks into the
*parser*.  Semi-operatives go a little deeper and we get a
macro-function hybrid.

A semi-operative, like a macro, gets the bits of AST it has been
invoked with, and (unlike a macro) the environment where it was
invoked.  It has access to functions that evaluate an AST in a
specific environment (``eval`` and ``eval*``), and functions to extend
the environment (``add-binding``).  The result of the semi-operative
invocation is the value (rather, the *domain value*) the interpreter
gets by executing the semi-operative.  I think it is useful to think
of semi-operatives as hooks that arbitrarily map program phrases into
domain values.

As an example of what this makes possible, look at
``samples/SimpleOperatives.dlisp``::

  ;; The #builtin# directives return an semi-operative defined inside
     the interpreter in Haskell.  global-bind binds them to the global
     lexical scope.
  (global-bind let (#builtin# #let#))
  (global-bind eval (#builtin# #eval#))

  (global-bind wormhole (operative () env (eval env x)))

  (let (x 42) (wormhole)) ;; Prints 42, since wormhole evaluates x in
    the environment inside let x (42)

``eval (ast, env)`` evaluates ``ast`` in the environment ``env``.
``eval* (ast, env)`` evaluates ``ast`` in the context of the current
environment and then evaluates this *result* in the context of ``env``
(``ast`` not evaluating to an AST is an error).  This allows you to
write functions this way::

   (operative (number) env
     (let (evaluated-number (eval* env number))
       (+ evaluated-number 5)))

In the above case, ``eval*`` first evaluates ``number`` in the current
lexical scope (which gives us the AST passed to the semi-operative
during evaluation) and then evaluates *that* ast in ``env`` (which
gives us some result to which we then add 5).

Conditional evaluation makes writing macros easy.  Here is a ``when``
macro::

  (global-bind when
    (operative (condition action) env
      (if (eval* env condition)
        (eval* env action)
        0)))

which evaluates to ``action`` if ``condition`` evaluates to
``true`` else evaluates to ``0``.

Two primitives useful when writing macros are ``unwrap-ast`` and
``wrap-to-ast``, which convert an AST to a domain value and
vice-versa; respectively.  A list can't be directly evaluated but
needs to be *wrapped* into an AST before evaluation.  Similarly, an
AST is impervious to direct introspection till it has been *unwrapped*
into a regular domain value.


Hopes and fears
~~~~~~~~~~~~~~~

So, will this all work out?  I don't know.  I have a hunch that
semi-operatives might lead to a cleaner and more orthogonal language;
I'll keep updating this README as I explore and learn more.

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
