# Orchard

Hi!
In this repository, I tinker with different programming language ideas.
I explore languages with different language complexity vs. efficiency vs. ergonomics tradeoffs.

The most interesting bits:

- **VM**:
  A virtual machine for a stack-based byte code that operates on a heap of immutable objects.  
  [`vm/`](vm/) contains the VM, written in Zig.  
  [`vm/vm_interpreter.zig`](vm/vm_interpreter.zig) contains the core interpreter.  
  [`vm/vm_x86_64.zig`](vm/vm_x86_64.zig) contains a JIT compiler.  
- **Olive**:
  A low-level programming language that allows you to create objects and byte code in the Orchard VM.  
  [`vm/compiler_olive.zig`](vm/compiler_olive.zig) contains an Olive compiler written in Zig.  
  [`olive/`](olive/) contains Olive code.
- **Pear**:
  A functional programming language that is basically just the untyped lambda calculus with Lisp syntax.  
  [`vm/compiler_pear.zig`](vm/compiler_pear.zig) contains a Pear compiler written in Zig.  
  [`pear/`](pear/) contains Pear code.
- **Ground**:
  A low-level stack-based byte code.  
  [`ground/readme.md`](ground/readme.md) contains the specification.  
  I wrote a Ground interpreter in Zig, but that's private for now.
- **Grass** (Ground Assembly):
  An assembly language for crafting binary files.  
  [`grass/readme.md`](grass/readme.md) contains more info.  
  [`grass/grass.grass`](grass/grass.grass) contains a self-hosted Grass compiler that is not yet finished.
- **Plum** (Programming Language Used by Marcel):
  A high-level functional programming language with immutable data structures, pure functions, and garbage collection.  
  [`plum/readme.md`](plum/readme.md) contains more info.  
  [`martinaise/plum`](martinaise/plum) contains a Plum compiler written in [Martinaise](https://github.com/MarcelGarus/martinaise), another programming language I created.  
  [`plum/core`](plum/core) contains the Plum standard library.
