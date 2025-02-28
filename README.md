# Plum

Plum is a small, functional, cozy programming language.

**Small**:
You can understand the entire language in an hour.
Plum has a structural type system.
There are few concepts to learn – everything boils down to data and functions.

**Functional**:
Functions are pure, they can't have side effects.
Data structures are immutable and acyclic, allowing garbage collection via reference counting.
Code is eagerly evaluated.

**Cozy**:
The syntax is concise.
Plum compiles to Ground, a byte code that is designed to be easy to embed in other languages.
Type reflection at compile time makes some code really concise.

> [!IMPORTANT]
> Plum is still in _very_ early development.
> Some parts are not implemented yet.
> Some inputs may crash the compiler.
> The onboarding is horrible (see Getting Started).

## Intro

Plum is indentation-based.

Types are uppercase, type variables lowercase.
Use `&` to create structs and `|` to create enums.

```plum
Position =
  & x: Int
    y: Int

Maybe t =
  | some: t
    none

origin = & x: 0 y: 0
foo = | some: 5
```

Function names are always lowercase or symbols.
The signatures of functions have type annotations and support overloading based on the argument types.

```plum
get list: (List t) index: Int -> t = ...
get map: (Map k v) key: k -> v = ...
```

By default, calls use Lisp-like syntax.
Alternatively, you can use indentation to group arguments, or a dot syntax to chain function calls.

```plum
+ (* 1 2) 4

# equivalent:
+
  * 1 2
  4

# equivalent:
1 .* 2 .+ 4
```

```plum
a_map .get "Hi" .format.uppercase.chars .get 0
```

You can switch on enums using the `%` syntax.

```plum
input
. parse_int 10
% some: value -> value
  none -> crash "you would normally use unwrap for this"
. sqrt
. * 1000
```

Arguments of the type `Type` have to be compile-time known and can be used in other parts of the signature.
You can reflect on types (at compile time) using the built-in `type_info` function.

```plum
parse json: Json target: Type -> target =
  type_info target
  % int ->
      json
      % int: int -> int
        _ -> crash "int please"
    struct fields ->
      json
      % map map -> ...
        _ -> crash ...

a_string.parse_json .parse Position
```

## Getting Started

1. Install [Zig](https://ziglang.org).
2. Compile [Soil](https://github.com/MarcelGarus/soil-zig), a byte code VM, using Zig.
   Put `soil` in your path.
3. Compile [Martinaise](https://github.com/MarcelGarus/martinaise), my previous programming language, using Soil.
   I recommend starting the Martinaise bootstrapping process with compiler/6/martinaise.soil rather than compiling from scratch.
   Some earlier compiler stages generate assembly directly, requiring an x86_64 system and several assemblers (NASM and FASM).
   Good luck!
4. Compile the Plum compiler using the Martinaise compiler.
5. You can now compile Plum code to Ground byte code.
6. Write a VM for Ground byte code.
   That should take around 100 to 200 lines of code.
   The spec is in this repo.
7. You can now run Plum programs!

> I have plans for radically simplifying the installation by bootstrapping from the Ground up:
>
> - by writing a simple assembler in byte code by hand
> - by writing a Forth in that textual assembly
> - by writing a Lisp in that Forth
> - by writing a Plum compiler in that Lisp
>
> Once that is done, the installation will only require a Ground VM (either write one yourself or use an existing one).
> Everything above that doesn't have any external dependencies.

## Some todos

- self-hosting compiler
- incremental compiler
- bootstrapped compiler
