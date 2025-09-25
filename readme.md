# Orchard

Hi!
In this repository, I tinker with different programming language ideas.
I explore languages with different language complexity vs. efficiency vs. ergonomics tradeoffs.

The most interesting bits:

- **Ground**:
  A low-level stack-based byte code.  
  [`ground/readme.md`](ground/readme.md) contains the specification.  
  I wrote a Ground interpreter in Zig, but that's private for now.
- **Grass** (Ground Assembly):
  An assembly language for crafting binary files.  
  [`grass/readme.md`](grass/readme.md) contains more info.  
  [`grass/grass.grass`](grass/grass.grass) contains a self-hosted Grass compiler that is not yet finished.
- **Pear** (Parenthesized Expressions And Really that's it):
  A language that is essentially just the untyped lambda calculus with Lisp syntax.
  Doesn't really work yet, only in my imagination.  
  [`pear/readme.md`](pear/readme.md) contains more info.  
  [`martinaise/pear.mar`](pear/pear.mar) contains a Pear interpreter written in [Martinaise](https://github.com/MarcelGarus/martinaise), another programming language I created.  
  [`pear/core.pear`](pear/core.pear) contains the Pear standard library.  
  [`pear/pear.pear`](pear/pear.pear) contains a self-hosted Pear interpreter that is not yet finished.
- **Mango** (TODO): A low-level programming language with C-like syntax.
  Doesn't really work yet.  
  [`mango/readme.md`](mango/readme.md) contains more info.  
  [`mango/core`](mango/core) contains the Mango standard library.  
  [`mango/mango.mango`](mango/mango.mango) contains a self-hosted Mango compiler that is not yet finished.
- **Plum** (Programming Language Used by Marcel):
  A high-level functional programming language with immutable data structures, pure functions, and garbage collection.  
  [`plum/readme.md`](plum/readme.md) contains more info.  
  [`martinaise/plum`](martinaise/plum) contains a Plum compiler written in [Martinaise](https://github.com/MarcelGarus/martinaise), another programming language I created.  
  [`plum/core`](plum/core) contains the Plum standard library.
