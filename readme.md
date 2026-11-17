# Orchard

Hi!
In this repository, I tinker with different programming language ideas.
I explore languages with different language complexity vs. efficiency vs. ergonomics tradeoffs.

The most interesting bits:

- **VM**:
  A virtual machine for a stack-based byte code that operates on a heap of immutable objects.  
  [vm/](vm/) contains the VM, written in Zig.  
  [vm/src/vm_interpreter.zig](vm/src/vm_interpreter.zig) contains the core interpreter.  
  [vm/src/vm_x86_64.zig](vm/src/vm_x86_64.zig) contains a JIT compiler.  
- **Olive** (Optimizable Lisp Implementing Various Expressions):
  A Lisp where the focus is to be easy to optimize.
  [vm/src/bootstrap.objects](vm/src/bootstrap.objects) contains an Olive compiler written in raw objects.
  [vm/src/bootstrap.olive](vm/src/bootstrap.olive) contains an optimizing Olive compiler and a Yam notebook compiler.
- **Yam**:
  A typed language organized into editable notebooks.
  [vm/src/notebook.yam](vm/src/notebook.yam) contains the standard library, compiler, and live notebook editor.
  [vm/BOOTSTRAP.md](vm/BOOTSTRAP.md) describes the bootstrap pipeline.
- **Ground**:
  A low-level stack-based byte code.  
  [ground/readme.md](ground/readme.md) contains the specification.  
  I wrote a Ground interpreter in Zig, but that's private for now.
- **Grass** (Ground Assembly):
  An assembly language for crafting binary files.  
  [grass/readme.md](grass/readme.md) contains more info.  
  [grass/grass.grass](grass/grass.grass) contains a self-hosted Grass compiler that is not yet finished.
- **Plum** (Programming Language Used by Marcel):
  A high-level functional programming language with immutable data structures, pure functions, and garbage collection.  
  [plum/readme.md](plum/readme.md) contains more info.  
  [martinaise/plum](martinaise/plum) contains a Plum compiler written in [Martinaise](https://github.com/MarcelGarus/martinaise), another programming language I created.  
  [plum/core](plum/core) contains the Plum standard library.
