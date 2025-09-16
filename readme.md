# Orchard

Hi!
In this repository, I tinker with different programming language ideas.
I explore languages with different language complexity vs. efficiency vs. ergonomics tradeoffs.

The most interesting bits:

- **Ground**:
  A low-level stack-based byte code.  
  [`ground`](ground) contains the specification.
  I wrote an interpreter in Zig, but that's private for now.
- **Grass** (Ground Assembly):
  A simple assembly dialect for writing Ground programs.
  Unlike the name suggests, Grass is actually encoding agnostic.  
  The `grass` directory contains a self-hosted Grass compiler that is not yet finished.
- **Pear** (Parenthesized Expressions And Really that's it):
  A language that is essentially just the untyped lambda calculus with lisp syntax.
  Doesn't really work yet, only in my imagination.
- **Plum** (Programming Language Used by Marcel):
  A high-level functional programming language with immutable data structures, pure functions, and garbage collection.  
  The `plum` directory contains some Plum code.
  The `martinaise/plum` directory contains a compiler for Plum written in [Martinaise](https://github.com/MarcelGarus/martinaise), my previous programming language.
