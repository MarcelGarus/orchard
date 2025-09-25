# Grass

Grass is an assembly language for crafting binary files.

When writing Ground files by hand, these are the major challenges:

- you need to remember magic numbers, like the opcodes
- you can't add comments or whitespace to give the code structure
- you have to compute jump targets by hand

Grass tries to solve these issues.
In it's simplest form, you can write bytes as hexadecimal numbers, with comments and whitespace between them:

```grass
# This is a comment.
d0 01 00 00 00 00 00 00 00  # push_word 1
d0 02 00 00 00 00 00 00 00  # push_word 2
a0                          # add
```

You can use `@` to define a label and `^` to refer to the label.
The label definition will result in no output.
The label reference will be replaced with a little-endian encoded 64-bit number that specifies the absolute offset of the label in the resulting binary.

```grass
01 02 03 @foo 04 05
^foo  # equivalent to writing: 03 00 00 00 00 00 00 00
^bar  # equivalent to writing: 17 00 00 00 00 00 00 00
06 07 @bar
```

You can use the syntax `: <name> <body> ;` to define a new name.
Every time you write the name in your code, it is as if you had written the body.
Essentially, names are just shorthands for byte sequences.
The bytes `00` to `ff` are names that the compiler has pre-defined.

```grass
# Define three words.
: push_word_1 d0 01 00 00 00 00 00 00 00 ;
: push_word_2 d0 02 00 00 00 00 00 00 00 ;
: add a0 ;

# Use the words.
push_word_1
push_word_2
add
```

A few notes on using names:

- Name definitions can reference names that are defined later on.
- Name definitions can't indirectly depend on themselves (aka be recursive).
  There is no reason to ever need recursive names (assuming you never want to generate an infinite byte sequence).
- Name definitions can't contain label definitions, as the name may be used zero or multiple times.
- Name definitions can contain label references.
