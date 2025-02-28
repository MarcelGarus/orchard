# Ground

Ground is a byte code format for a stack-based virtual machine (VM).

While a Ground program executes, there are three separate parts of state:

- **Byte Code:**
  Byte code consists of instructions.
  By default, they are executed sequentially, but there are also jump and call instructions.
  Going through instructions requires an instruction pointer and a call stack.

- **Data Stack:**
  The byte code operates on a stack.
  All instructions inspect the stack, for example by popping and pushing values.

- **Heap Memory:**
  Some instructions (malloc, free, load, store) operate on heap memory.

## Limitations

The byte code cannot store pointers to instructions or the stack – this gives Ground implementations the freedom to compile the byte code to machine code on startup.

Programs may not depend on the endianness of numbers.

## Communication with the Outside

Ground programs can't directly trigger behavior in the host.

Depending on the program, you are expected to initialize the stack with some arguments.
After running, the stack contains the result.

For example, a Ground program may expect a pointer to a string on the stack and return the number of words in that string.

## Encoding of Ground Programs

Ground programs consist of instructions.
A program is an encoding of the instructions, one after another.
The instructions are not aligned to a common size, but tightly packed together.

Next is a list of all instructions along with their encoding in hexadecimal notation.
Two letters in the encoding correspond to a single byte, using `xx` and `yy` to mark variables.
The encoded uses little endian encoding for 64-bit integers; VMs are free to choose any 8-byte encoding on the stack.
64-bit integers are always signed from -2^63 to 2^63 - 1, inclusive.

- `00`: **nop**: Does nothing.
- `a0`: **add_8**:
  Pops two 64-bit integers, adds them, and pushes the result as a 64-bit integer.
- `a1`: **sub_8**:
  Pops two 64-bit integers, subtracts the second (further up on the stack) from the first (further down on the stack) them, and pushes the result as a 64-bit integer.
- `a2`: **mul_8**:
  Pops two 64-bit integers, multiplies them, and pushes the result as a 64-bit integer.
- `a3`: **div_8**:
  Pops two 64-bit integers, divides the first (further down on the stack) by the second (further up on the stack), and pushes the result as a 64-bit integer.
  Division always rounds towards zero (`@divTrunc` in Zig).
- `a4`: **mod_8**:
  Pops two 64-bit integers, mods the first (further down on the stack) by the second (further up on the stack), and pushes the result as a 64-bit integer.
  This is true modulo, not the remainder (so, not `%` in C).
  As a consequence, the result is always positive.
- `a5`: **compare_zero_8**:
  Pops a 64-bit integer and compares it to zero.
  Then pushes a byte indicating the result:
  | value | byte |
  | ----- | ---- |
  | == 0 | 0 |
  | > 0 | 1 |
  | < 0 | 2 |
- `b0`: **and_8**:
  Pops two 64-bit integers, bitwise-ands them, and pushes the result as a 64-bit integer.
- `b1`: **or_8**:
  Pops two 64-bit integers, bitwise-ors them, and pushes the result as a 64-bit integer.
- `b2`: **xor_8**:
  Pops two 64-bit integers, bitwise-xors them, and pushes the result as a 64-bit integer.
- `c0 xx`: **push_padding**:
  Push x bytes of padding to the stack.
- `c1 xx`: **push_1**:
  Push the byte x to the stack.
- `c2 xx xx xx xx xx xx xx xx`: **push_8**:
  Pushes x on the stack.
  The stack must be aligned to 8 bytes.
- `c3 xx xx xx xx xx xx xx xx`: **push_1_from_stack**:
  Pushes a byte from the offset x from the stack (0 = top, etc.)
- `c4 xx xx xx xx xx xx xx xx`: **push_8_from_stack**:
  Pushes 8 bytes from the offset x from the stack (0 = top, etc.)
- `c5 xx`: **pop**:
  Pops x bytes from the stack.
- `c6 xx xx xx xx xx xx xx xx yy`: **pop_below_top**:
  Pops y bytes below the top x bytes.
- `d0`: **malloc**:
  First pops an alignment (1 byte), then a size (8 bytes).
  Allocates memory of the requested alignment and size.
  If that worked, pushes the address (8 bytes).
  If that didn't work, pushes 0 (8 bytes).
- `d1`: **free**:
  First pops an alignment (1) byte, then a size (8 bytes), then an address (8 bytes).
  Frees the memory at that address, which has to have the given size and alignment.
- `d2`: **store_1**:
  First pops a value (1 byte), then an address (8 bytes), and stores the value to the memory at that address.
- `d3`: **store_8**:
  First pops a value (8 bytes), then an address (8 bytes), and stores the value to the memory at that address.
- `d4`: **load_1**:
  Pops an address (8 bytes), loads a byte from that address, and pushes that byte to the stack.
- `d5`: **load_8**:
  Pops an address (8 bytes), loads 8 bytes from that address, and pushes them to the stack.
- `e0`: **crash**:
  First pops a length (8 bytes), then an address (8 bytes).
  Crashes the VM with the message at that address with that length.
  After crashing, the state of the VM is corrupted – no instructions should be executed anymore and allocated memory may not have been freed.
- `f0 xx xx xx xx xx xx xx xx`: **jump**:
  Jump to the instruction at offset x in the encoding.
- `f1 xx ...` **jump_table**:
  The x byte encodes how many jump targets there are.
  After the first 2 bytes, there are 8 bytes per jump target.
  Each one represents an offset in the encoding.
  The _jump_table_ instruction pops a single byte and jumps to the instruction at the index.
- `f2 xx xx xx xx xx xx xx xx`: **call**:
  Calls the function at offset x in the encoding.
  The VM is supposed to maintain a call stack so that the corresponding _return_ instruction returns to the instruction after this one.
- `f3 xx xx xx xx xx xx xx xx`: **push_indirect**:
  Pushes an 8-byte "indirect call token" that represents the offset x in the encoding.
- `f4`: **call_indirect**:
  Like call, but the target is not immediately encoded in the instruction, but popped from the stack.
  The target has to be an "indirect call token" previously created using _push_indirect_.
- `f5`: **return**:
  Returns from a function call.
