// This module JIT-compiles instructions to x86 machine code. We map the VM semantics like this:
//
// Heap: The VM's heap is a memory region that we allocate (the Zig Heap struct does that). The heap
//   can't grow dynamically; if you use it too much, you get an out-of-memory error instead.
// Data stack: The VM's data stack is a memory region that we allocate. The data stack can't grow
//   dynamically; code can overflow it.
// Call stack:  The VM's call stack is a memory region that we allocate. The call stack can't grow
//   dynamically; code can overflow it.
//
// r8 = heap cursor
// r9 = call stack cursor
// rsp = data stack cursor

const std = @import("std");
const ArrayList = std.ArrayListUnmanaged;
const Map = std.AutoArrayHashMap;
const Ally = std.mem.Allocator;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const Instruction = @import("instruction.zig").Instruction;
const ObjMap = @import("obj_map.zig").ObjMap;
const Val = @import("value.zig");

const Vm = @This();

ally: Ally,
// TODO: add guard pages next to the heap and stacks
heap: *Heap,
data_stack: Stack,
call_stack: Stack,
jit_cache: ObjMap([]const u8), // maps (pointer to instructions obj) to (x86_64 machine code)
run_jitted: *const RunFun,
// TODO: std.linux.munmap() the cache

// The jitted machine code doesn't follow the Sys-V calling convention.

const RunFun = fn(*RunState) callconv(.c) void;
const RunState = packed struct {
  machine_code: *const anyopaque,
  heap_cursor: *anyopaque,
  call_stack_cursor: *anyopaque,
  data_stack_cursor: *anyopaque,
};

// The stack grows downward, just like the native x86 stack. This allows us to use push and pop
// instructions directly to modify the (data) stack.
const Stack = struct {
  memory: []Word,
  cursor: usize,

  pub fn init(ally: Ally, num_words: usize) !Stack {
    return .{ .memory = try ally.alloc(Word, num_words), .cursor = num_words };
  }
  pub fn push(self: *Stack, word: Word) !void {
      if (self.cursor == 0) @panic("stack overflow");
      self.cursor -= 1;
      self.memory[self.cursor] = word;
  }
  pub fn pop(self: *Stack) Word {
      const word = self.memory[self.cursor];
      self.cursor += 1;
      return word;
  }
};

pub fn init(heap: *Heap, ally: Ally) !Vm {
    return .{
        .ally = ally,
        .heap = heap,
        .data_stack = try Stack.init(ally, 100),
        .call_stack = try Stack.init(ally, 100),
        .jit_cache = ObjMap([]const u8).empty,
        .run_jitted = @ptrCast(try make_run_fun(ally)),
    };
}

pub fn make_run_fun(ally: Ally) ![]const u8 {
  var code = MachineCode.init(ally);

  // Find out the instruction pointer. You'd normally do this by just hardcoding it into the machine code, but
  // the MachineCode builder only supports creating relocatable code. Thus, we generate and call a tiny
  // function inline that just copies the address from the x86 call stack and then returns.
  const a = try code.emit_placeholder("call {:32}", .{@as(i32, 0)});
  const b = try code.emit_placeholder("jmp {:32}", .{@as(i32, 0)});
  const c = code.len();
  try code.emit("mov rax, [rsp]", .{});
  try code.emit("ret", .{});
  const d = code.len();
  code.patch(a, "call {:32}", .{ @as(i32, @intCast(c - b)) });
  code.patch(b, "jmp {:32}", .{ @as(i32, @intCast(d - c)) });

  // Save relevant registers.
  try code.emit("mov r15, rsp", .{});

  // Move the state (rdi) from the struct into the correct registers.
  try code.emit("mov rbx, [rdi]", .{}); // machine code
  try code.emit("mov r8, [rdi + 8]", .{}); // heap
  try code.emit("mov rsp, [rdi + 16]", .{}); // data stack
  try code.emit("mov r9, [rdi + 24]", .{}); // call stack

  // "call" the jitted code (putting our next instruction on the VM's call stack)
  try code.emit("sub r9, 8", .{});
  const e = try code.emit_placeholder("add rax, {:8}", .{ @as(i8, 0) });
  try code.emit("mov [r9], rax", .{});
  try code.emit("jmp rbx", .{});
  const f = code.len();
  code.patch(e, "add rax, {:8}", .{ @as(i8, @intCast(f - b)) });

  try code.emit("hlt", .{});

  // Move the state from the registers into the struct.
  try code.emit("mov [rdi + 8], r8", .{}); // heap
  try code.emit("mov [rdi + 16], rsp", .{}); // data stack

  // Restore relevant registers.
  try code.emit("mov rsp, r15", .{});

  try code.emit("ret", .{});

  return try code.finish();
}

pub fn call(vm: *Vm, lambda: Val.Lambda, args: []const Val.Value) !Val.Value {
    const instructions = lambda.obj.child(1);
    const closure = lambda.obj.child(2);
    for (args) |arg| try vm.data_stack.push(arg.obj.address);
    try vm.data_stack.push(closure.address);
    try vm.run(try vm.compile(instructions));
    return .{ .obj = .{ .address = vm.data_stack.pop() } };
}
pub fn compile(vm: *Vm, instructions: Obj) ![]const u8 {
  if (vm.jit_cache.get(instructions)) |machine_code| return machine_code;
  const jitted = try compile_instructions(vm.ally, try Instruction.parse_instructions(vm.ally, instructions));
  try vm.jit_cache.put(vm.ally, instructions, jitted);
  return jitted;
}
// Turns the instructions into x86 machine code bytes.
pub fn compile_instructions(ally: Ally, instructions: []const Instruction) ![]const u8 {
    var code = MachineCode.init(ally);
    _ = try compile_all(&code, instructions);
    try code.emit("mov rax, [r9]", .{});
    try code.emit("add r9, 8", .{});
    try code.emit("jmp rax", .{});
    return code.finish();
}
const MachineCode = struct {
    ally: std.mem.Allocator,
    bytes: ArrayList(u8),

    pub fn init(ally: std.mem.Allocator) MachineCode {
        return .{ .ally = ally, .bytes = ArrayList(u8).empty };
    }
    fn emit_byte(self: *MachineCode, byte: u8) !void {
        try self.bytes.append(self.ally, byte);
    }
    pub fn len(self: MachineCode) usize {
        return self.bytes.items.len;
    }
    pub fn emit(self: *MachineCode, comptime x86_instr: []const u8, args: anytype) !void {
        std.debug.print("{s:<30} {any:<30} ", .{ x86_instr, args });
        const constants = .{
            // TODO: remove some unused instructions from here
            .@"hlt" = "f4",
            .@"push qword {:i32}" = "68 {0:i32le}",
            .@"push rax" = "50",
            .@"push [rax]" = "ff 30",
            .@"push rbx" = "53",
            .@"push rcx" = "51",
            .@"pop rax" = "58",
            .@"pop rbx" = "5b",
            .@"pop rcx" = "59",
            .@"mov rsp, [rdi + 16]" = "48 8b 67 10",
            .@"mov rsp, r15" = "4c 89 fc",
            .@"mov [rdi + 8], r8" = "4c 89 47 08",
            .@"mov [rdi + 16], rsp" = "48 89 67 10",
            .@"mov rax, {:64}" = "48 b8 {0:u64le}",
            .@"mov rax, rsp" = "48 89 e0",
            .@"mov rax, [rsp]" = "48 8b 04 24",
            .@"mov rax, r8" = "4c 89 c0",
            .@"mov rax, [r9]" = "49 8b 01",
            .@"mov rax, r10" = "4c 89 d0",
            .@"mov rax, r12" = "4c 89 e0",
            .@"mov rax, [rax]" = "48 8b 00",
            .@"mov rax, [r10]" = "49 8b 02",
            .@"mov rax, [r10 - 8]" = "49 8b 42 f8",
            .@"mov rax, [r12]" = "49 8b 04 24",
            .@"mov rbx, 0xffffffffffff" = "48 bb ff ff ff ff ff ff 00 00",
            .@"mov rbx, {:64}" = "48 bb {0:u64le}",
            .@"mov rbx, rsp" = "48 89 e3",
            .@"mov rbx, [rdi]" = "48 8b 1f",
            .@"mov rbx, [rax]" = "48 8b 18",
            .@"mov rcx, r8" = "4c 89 c1",
            .@"mov rcx, [rbx]" = "48 8b 0b",
            .@"mov rcx, [r10]" = "49 8b 0a",
            .@"mov r8, [rdi + 8]" = "4c 8b 47 08",
            .@"mov [r8], rax" = "49 89 00",
            .@"mov [r8], rbx" = "49 89 18",
            .@"mov [r8], rcx" = "49 89 08",
            .@"mov r9, [rdi + 24]" = "4c 8b 4f 18",
            .@"mov [r9], rax" = "49 89 01",
            .@"mov [r10], rax" = "49 89 02",
            .@"mov [r10], rcx" = "49 89 0a",
            .@"mov [r10], {:32}" = "49 c7 02 {0:u32le}",
            .@"mov [r10 - 8], rax" = "49 89 42 f8",
            .@"mov [r10 - 16], rax" = "49 89 42 f0",
            .@"mov [r12], rax" = "49 89 04 24",
            .@"mov [r12], {:32}" = "49 c7 04 24 {0:u32le}",
            .@"mov [r12 - 16], rax" = "49 89 44 24 f0",
            .@"mov r15, rsp" = "49 89 e7",
            .@"add rax, 8" = "48 83 c0 08",
            .@"add rax, {:8}" = "48 83 c0 {0:i8}",
            .@"add rax, rbx" = "48 01 d8",
            .@"add rax, [r10 - 16]" = "49 03 42 f0",
            .@"add rsp, rax" = "48 01 c4",
            .@"add rsp, rbx" = "48 01 dc",
            .@"add r8, 8" = "49 83 c0 08",
            .@"add r9, 8" = "49 83 c1 08",
            .@"add r10, 8" = "49 83 c2 08",
            .@"add r12, 8" = "49 83 c4 08",
            .@"sub rax, 8" = "49 83 e8 08",
            .@"sub rax, {:32}" = "48 2d {0:u32le}", // TODO: remove?
            .@"sub rax, rbx" = "48 29 d8",
            .@"sub rax, [r10 - 16]" = "49 2b 42 f0",
            .@"sub rbx, 8" = "48 83 eb 08",
            .@"sub r9, 8" = "49 83 e9 08",
            .@"sub r10, rax" = "49 29 c2",
            .@"sub r10, rbx" = "49 29 da",
            .@"sub r10, 8" = "49 83 ea 08",
            .@"sub r10d, {:32}" = "41 81 ea {0:u32le}",
            .@"shl rbx, 3" = "48 c1 e3 03",
            .@"and rax, rbx" = "48 21 d8",
            .@"xor rax, rax" = "48 31 c0",
            .@"cmp rax, 0" = "48 83 f8 00",
            .@"cmp rcx, 0" = "48 83 f9 00",
            .@"cmovnz rax, rbx" = "48 0f 45 c3",
            .@"jz {:32}" = "0f 84 {0:i32le}",
            .@"jmp {:32}" = "e9 {0:i32le}",
            .@"jmp rax" = "ff e0",
            .@"jmp rbx" = "ff e3",
            .@"call {:32}" = "e8 {0:i32le}",
            .@"call rax" = "ff d0",
            .ret = "c3",
        };
        const len_before = self.len();
        inline for (@typeInfo(@TypeOf(constants)).@"struct".fields) |field| {
            if (comptime std.mem.eql(u8, x86_instr, field.name)) {
                const bytes = @field(constants, field.name);
                comptime var iter = std.mem.splitScalar(u8, bytes, ' ');
                inline while (comptime iter.next()) |item| {
                    if (comptime std.mem.startsWith(u8, item, "{")) {
                        const content = item[1..(item.len - 1)];
                        comptime var it = std.mem.splitScalar(u8, content, ':');
                        const index_str = comptime it.next() orelse @compileError("expected index part");
                        const how_to_emit = comptime it.next() orelse @compileError("expected how to emit");
                        const index = comptime std.fmt.parseInt(usize, index_str, 10) catch unreachable;
                        const value = args[index];
                        if (comptime std.mem.eql(u8, how_to_emit, "i8")) {
                          if (@TypeOf(value) != i8) @compileError("expected i8, got " ++ @typeName(@TypeOf(value)));
                          try self.emit_byte(@bitCast(value));
                        } else if (comptime std.mem.eql(u8, how_to_emit, "u32le")) {
                          if (@TypeOf(value) != u32) @compileError("expected u32, got " ++ @typeName(@TypeOf(value)));
                          const target_space = try self.bytes.addManyAsArray(self.ally, 4);
                          std.mem.writeInt(u32, target_space, value, .little);
                        } else if (comptime std.mem.eql(u8, how_to_emit, "i32le")) {
                          if (@TypeOf(value) != i32) @compileError("expected i32, got " ++ @typeName(@TypeOf(value)));
                          const target_space = try self.bytes.addManyAsArray(self.ally, 4);
                          std.mem.writeInt(i32, target_space, value, .little);
                        } else if (comptime std.mem.eql(u8, how_to_emit, "u64le")) {
                          if (@TypeOf(value) != u64) @compileError("expected u64, got " ++ @typeName(@TypeOf(value)));
                          const target_space = try self.bytes.addManyAsArray(self.ally, 8);
                          std.mem.writeInt(u64, target_space, value, .little);
                        } else @compileError("unknown formatting" ++ how_to_emit);
                    } else {
                        const byte = comptime std.fmt.parseInt(u8, item, 16) catch unreachable;
                        try self.emit_byte(byte);
                    }
                }
                for (self.bytes.items[len_before..]) |b| std.debug.print(" {x:02}", .{b});
                std.debug.print("\n", .{});
                return;
            }
        }
        @panic("Unknown instruction: " ++ x86_instr);
    }
    pub fn emit_placeholder(self: *MachineCode, comptime x86_instr: []const u8, args: anytype) !usize {
        const length = self.len();
        try self.emit(x86_instr, args);
        return length;
    }
    pub fn patch(self: *MachineCode, at: usize, comptime x86_instr: []const u8, args: anytype) void {
        const cursor = self.bytes.items.len;
        self.bytes.items.len = at;
        self.emit(x86_instr, args) catch unreachable;
        self.bytes.items.len = cursor;
    }
    pub fn finish(self: *MachineCode) ![]const u8 {
      const bytes = self.bytes.items;

      std.debug.print("bytes:", .{});
      for (bytes) |byte| std.debug.print(" {x:02}", .{byte});
      std.debug.print("\n", .{});

      // We want to create an executable memory region that contains the machine code. To do that, we
      // need to talk to the operating system in a low level way, so we don't go through Zig allocators.
      const permission_read_write = std.os.linux.PROT.READ | std.os.linux.PROT.WRITE;
      const permission_read_exec = std.os.linux.PROT.READ | std.os.linux.PROT.EXEC;
      const address = std.os.linux.mmap(
          null, bytes.len, permission_read_write, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0,
      );
      if (address == 0) return error.OutOfMemory;
      const ptr: [*]u8 =@ptrFromInt(address);
      std.debug.print("machine code lives at {*}\n", .{ptr});
      @memcpy(ptr, bytes);
      std.debug.assert(std.os.linux.mprotect(ptr, bytes.len, permission_read_exec) == 0);
      return ptr[0..bytes.len];
    }
};
fn compile_all(code: *MachineCode, instructions: []const Instruction) !void {
    for (instructions) |instruction| try compile_single(code, instruction);
}
fn compile_single(code: *MachineCode, instruction: Instruction) error{OutOfMemory}!void {
    // Before an instruction, we expected to following invariants to hold:
    //
    // - r8 contains the heap cursor (the address that the next heap-allocated object would get)
    // - r9 points to the call stack, which grows downward
    // - rsp points to the data stack, which grows downward
    //
    // Instructions may modify the rax, rbx, rcx, rdx registers as they please.
    switch (instruction) {
        // TODO: choose smaller instruction sequences when possible
        .word => |word| {
            const signed = @as(i64, @bitCast(word));
            if (word == 0) {
                try code.emit("xor rax, rax", .{});
                try code.emit("push rax", .{});
            } else if (signed >= -0x80000000 and signed < 0x7fffffff) {
                try code.emit("push qword {:i32}", .{@as(i32, @intCast(signed))});
            } else {
                try code.emit("mov rax, {:64}", .{@as(u64, @intCast(word))});
                try code.emit("push rax", .{});
            }
        },
        .address => |object| {
            const address = object.address;
            const signed = @as(i64, @bitCast(address));
            if (signed >= -0x80000000 and signed < 0x7fffffff) {
                try code.emit("push qword {:i32}", .{@as(i32, @intCast(signed))});
            } else {
                try code.emit("mov rax, {:64}", .{@as(u64, @intCast(address))});
                try code.emit("push rax", .{});
            }
        },
        .stack => |offset| {
            const real_offset = offset * 8 + 8;
            const signed = @as(i64, @bitCast(real_offset));
            try code.emit("mov rax, rsp", .{});
            if (real_offset >= -0x80 and real_offset < 0x7f) {
              try code.emit("add rax, {:8}", .{@as(i8, @intCast(signed))});
            } else {
              try code.emit("mov rbx, {:64}", .{@as(u64, @intCast(real_offset))});
              try code.emit("add rax, rbx", .{});
            }
            try code.emit("push [rax]", .{});
        },
        .pop => |amount| {
            try code.emit("mov rax, {:64}", .{@as(u64, @intCast(amount * 8))});
            try code.emit("add rsp, rax", .{});
        },
        .popover => |amount| {
            try code.emit("pop rax", .{});
            try code.emit("mov rbx, {:64}", .{@as(u64, @intCast(amount * 8))});
            try code.emit("add rsp, rbx", .{});
            try code.emit("push rax", .{});
        },
        .add => {
            try code.emit("pop rbx", .{});
            try code.emit("pop rax", .{});
            try code.emit("add rax, rbx", .{});
            try code.emit("push rax", .{});
        },
        .subtract => {
            try code.emit("pop rbx", .{});
            try code.emit("pop rax", .{});
            try code.emit("sub rax, rbx", .{});
            try code.emit("push rax", .{});
        },
        .multiply => {
            try code.emit("pop rbx", .{});
            try code.emit("pop rax", .{});
            try code.emit("imul rax, rbx", .{});
            try code.emit("push rax", .{});
        },
        .divide => {
            try code.emit("pop rbx", .{});
            try code.emit("pop rax", .{});
            try code.emit("div rax, rbx", .{});
            try code.emit("push rax", .{});
        },
        .modulo => {
            try code.emit("pop rbx", .{});
            try code.emit("pop rax", .{});
            try code.emit("mod rax, rbx", .{});
            try code.emit("push rax", .{});
        },
        .shl => {
            try code.emit("pop rbx", .{});
            try code.emit("pop rax", .{});
            try code.emit("shl rax, rbx", .{});
            try code.emit("push rax", .{});
        },
        .shr => {
            try code.emit("pop rbx", .{});
            try code.emit("pop rax", .{});
            try code.emit("shr rax, rbx", .{});
            try code.emit("push rax", .{});
        },
        .compare => @panic("JIT compare"), // equal: 0, greater: 1, less: 2
        .@"if" => |if_| {
            try code.emit("pop rax", .{});
            try code.emit("cmp rax, 0", .{});
            const jump_to_else_placeholder = try code.emit_placeholder("jz {:32}", .{ @as(i32, 0) });
            const after_jump_to_else = code.len();
            try compile_all(code, if_.then);
            const jump_to_after_if_placeholder = try code.emit_placeholder("jmp {:32}", .{ @as(i32, 0) });
            const after_jump_to_after_if = code.len();
            const else_ = code.len();
            try compile_all(code, if_.else_);
            const after_if = code.len();
            // Patch the placeholders (these are relative jumps).
            code.patch(jump_to_else_placeholder, "jz {:32}", .{ @as(i32, @intCast(else_ - after_jump_to_else)) });
            code.patch(jump_to_after_if_placeholder, "jmp {:32}", .{ @as(i32, @intCast(after_if - after_jump_to_after_if)) });
        },
        .new => |new| {
            try code.emit("mov rax, r8", .{});
            // rax = start of object; r8 = start of object; rsp = top of stack
            try code.emit("mov rbx, {:64}", .{
              @as(u64, @bitCast(Heap.Header {
                .num_words = @intCast(new.num_words),
                .is_inner = if (new.has_pointers) 1 else 0,
              })),
            });
            try code.emit("mov [r8], rbx", .{});
            try code.emit("add r8, 8", .{});
            // The header word has now been allocated on the heap.
            try code.emit("mov rbx, {:64}", .{ @as(u64, @intCast(new.num_words * 8)) });
            try code.emit("add rsp, rbx", .{ });
            try code.emit("mov rbx, rsp", .{});
            for (0..new.num_words) |_| {
                try code.emit("sub rbx, 8", .{});
                try code.emit("mov rcx, [rbx]", .{});
                try code.emit("mov [r8], rcx", .{});
                try code.emit("add r8, 8", .{});
            }
            try code.emit("push rax", .{});
        },
        .flatptro => @panic("JIT flatptro"),
        .flatlito => @panic("JIT flatlito"),
        .points => @panic("JIT points"),
        .size => {
          try code.emit("pop rax", .{});
          try code.emit("mov rax, [rax]", .{});
          try code.emit("mov rbx, 0xffffffffffff", .{});
          try code.emit("and rax, rbx", .{});
          try code.emit("push rax", .{});
        },
        .load => {
          try code.emit("pop rbx", .{});
          try code.emit("shl rbx, 3", .{});
          try code.emit("pop rax", .{});
          try code.emit("add rax, 8", .{});
          try code.emit("add rax, rbx", .{});
          try code.emit("push [rax]", .{});
        },
        .heapsize => {
          try code.emit("push r8", .{});
        },
        .gc => @panic("JIT gc"),
        .eval => {
          try code.emit("hlt", .{});
          // try code.emit("mov rax, {:64}", .{ @as(u64, @bitCast(@intFromPtr(&hook_eval))) });
          // try code.emit("call rax", .{});
        },
        .crash => {
          try code.emit("mov rax, {:64}", .{ @as(u64, @bitCast(@intFromPtr(&hook_crash))) });
          try code.emit("call rax", .{});
        },
    }
}
pub fn run(vm: *Vm, jitted: []const u8) !void {
    std.debug.print("data stack:", .{});
    for (vm.data_stack.memory[vm.data_stack.cursor..]) |byte| std.debug.print(" {x:02}", .{byte});
    std.debug.print("\n", .{});
    std.debug.print("running jitted instructions\n", .{});

    var state = RunState{
        .machine_code = @ptrCast(jitted.ptr),
        .heap_cursor = @ptrFromInt(@intFromPtr(vm.heap.memory.ptr) + (8 * vm.heap.used)),
        .data_stack_cursor = @ptrFromInt(@intFromPtr(vm.data_stack.memory.ptr) + (8 * vm.data_stack.cursor)),
        .call_stack_cursor = @ptrFromInt(@intFromPtr(vm.call_stack.memory.ptr) + (8 * vm.call_stack.cursor)),
    };
    std.debug.print("state = {any}\n", .{state});
    @breakpoint();
    vm.run_jitted(&state);
    std.debug.print("state = {any}\n", .{state});
    vm.heap.used = (@intFromPtr(state.heap_cursor) - @intFromPtr(vm.heap.memory.ptr)) / 8;
    vm.data_stack.cursor = (@intFromPtr(state.data_stack_cursor) - @intFromPtr(vm.data_stack.memory.ptr)) / 8;
    vm.call_stack.cursor = (@intFromPtr(state.call_stack_cursor) - @intFromPtr(vm.call_stack.memory.ptr)) / 8;

    std.debug.print("ran jitted instructions\n", .{});
    std.debug.print("data stack:", .{});
    for (vm.data_stack.memory[vm.data_stack.cursor..]) |word| std.debug.print(" {x:02}", .{word});
    std.debug.print("\n", .{});
}

pub export fn hook_crash() callconv(.c) void {
  std.debug.print("oh no! crashing\n", .{});
  std.process.exit(1);
}
fn hook_eval() callconv(.c) void {
  std.debug.print("oh no! evaling\n", .{});
  std.process.exit(1);
}

pub fn deduplicate(vm: *Vm, checkpoint: Heap.Checkpoint, obj: Obj) !Obj {
    var map = try vm.heap.deduplicate(vm.ally, checkpoint);
    vm.jit_cache.remove_everything_after(checkpoint.address);
    const mapped = map.get(obj) orelse obj;
    map.deinit();
    return mapped;
}

pub fn garbage_collect(vm: *Vm, checkpoint: Heap.Checkpoint, keep: Obj) !Obj {
    const mapped = vm.heap.garbage_collect(vm.ally, checkpoint, keep);
    vm.jit_cache.remove_everything_after(checkpoint.address);
    return mapped;
}
