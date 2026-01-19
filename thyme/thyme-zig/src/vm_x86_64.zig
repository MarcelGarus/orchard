// This module JIT-compiles instructions to x86 machine code. We map the VM semantics like this:
//
// Heap: The VM's heap is a memory region that we allocate (the Zig Heap struct does that). The heap
//   can't grow dynamically; if you use it too much, you get an out-of-memory error instead.
// Data stack: The VM's data stack is a memory region that we allocate. The data stack can't grow
//   dynamically; code can overflow it.
// Call stack:  The VM's call stack is a memory region that we allocate. The call stack can't grow
//   dynamically; code can overflow it.
//
// We don't really use the x86 stack.
//
// r8 = heap cursor
// r9 = heap end
// r10 = data stack cursor
// r11 = data stack end
// r12 = call stack cursor
// r13 = call stack end

const std = @import("std");
const ArrayList = std.ArrayListUnmanaged;
const Map = std.AutoArrayHashMap;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
const Instruction = @import("instruction.zig").Instruction;
const Vm = @import("vm.zig");

pub const Ally = struct {
  ally: std.mem.Allocator,

  pub fn init(ally: std.mem.Allocator) !Ally {
    return .{ .ally = ally };
  }
};

pub const Jitted = struct {
    address: [*]u8, // Pointer to non-writable, executable memory with machine code.
    len: usize, // Length of the memory.

    pub fn deinit() !void {
        std.linux.munmap();
    }
};

pub fn compile(ally: Ally, instructions: []const Instruction) !Jitted {
    // Turn the instructions into x86 machine code bytes.
    var emitter = Emitter.init(ally.ally);
    _ = try compile_all(&emitter, instructions);
    try emitter.emit("ret", .{});
    const bytes = emitter.bytes.items;

    std.debug.print("bytes:", .{});
    for (bytes) |byte| std.debug.print(" {x:02}", .{byte});
    std.debug.print("\n", .{});

    // We want to create an executable memory region that contains the machine
    // code. To do that, we need to talk to the operating system in a low level
    // way, so we don't go through Zig allocators.

    // Step 1: Allocate a page-aligned memory region that is big enough for our
    //         generated machine code. Some operating systems don't like memory
    //         that is marked as executable and writable, so we only make it
    //         readable and writable for now.
    const address: [*]u8 = address: {
        const address = std.os.linux.mmap(
            null,
            bytes.len,
            std.os.linux.PROT.READ | std.os.linux.PROT.WRITE,
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
            -1,
            0,
        );
        if (address == 0) return error.OutOfMemory;
        break :address @ptrFromInt(address);
    };
    std.debug.print("machine code lives at {*}\n", .{address});

    // Step 2: Copy the machine code to that memory region. This requires
    //         adjusting the targets of internal jump instructions.
    // TODO: adjust jumps when relocating
    @memcpy(address, bytes);

    // Step 3: Make the memory executable. After this, writing to the memory
    //         will cause a fault.
    std.debug.assert(std.os.linux.mprotect(
        address,
        bytes.len,
        std.os.linux.PROT.READ | std.os.linux.PROT.EXEC,
    ) == 0);

    return .{ .address = address, .len = bytes.len };
}

pub fn run(vm: *Vm, jitted: Jitted) !void {
    std.debug.print("data stack:", .{});
    for (vm.data_stack.memory[0..vm.data_stack.used]) |byte| std.debug.print(" {x:02}", .{byte});
    std.debug.print("\n", .{});
    std.debug.print("running jitted instructions\n", .{});

    const State = packed struct {
        heap_cursor: [*]Word,
        heap_end: [*]Word,
        data_stack_cursor: [*]Word,
        data_stack_end: [*]Word,
        call_stack_cursor: [*]Word,
        call_stack_end: [*]Word,
    };
    var state = State{
        .heap_cursor = @ptrFromInt(@intFromPtr(vm.heap.memory.ptr) + (8 * vm.heap.used)),
        .heap_end = @ptrFromInt(@intFromPtr(vm.heap.memory.ptr) + (8 * vm.heap.memory.len)),
        .data_stack_cursor = @ptrFromInt(@intFromPtr(vm.data_stack.memory.ptr) + (8 * vm.data_stack.used)),
        .data_stack_end = @ptrFromInt(@intFromPtr(vm.data_stack.memory.ptr) + (8 * vm.data_stack.memory.len)),
        .call_stack_cursor = @ptrFromInt(@intFromPtr(vm.call_stack.memory.ptr) + (8 * vm.call_stack.used)),
        .call_stack_end = @ptrFromInt(@intFromPtr(vm.call_stack.memory.ptr) + (8 * vm.call_stack.memory.len)),
    };
    std.debug.print("state = {any}\n", .{state});
    @breakpoint();
    asm volatile (
        \\ movq (%%rbx), %%r8      # heap cursor
        \\ movq +8(%%rbx), %%r9    # heap end
        \\ movq +16(%%rbx), %%r10  # data stack cursor
        \\ movq +24(%%rbx), %%r11  # data stack end
        \\ movq +32(%%rbx), %%r12  # call stack cursor
        \\ movq +40(%%rbx), %%r13  # call stack end
        \\ push %%rbx
        \\ call *%%rax
        \\ pop %%rbx
        \\ movq %%r8, (%%rbx)      # heap cursor
        \\ movq %%r9, +8(%%rbx)    # heap end
        \\ movq %%r10, +16(%%rbx)  # data stack cursor
        \\ movq %%r11, +24(%%rbx)  # data stack end
        \\ movq %%r12, +32(%%rbx)  # call stack cursor
        \\ movq %%r13, +40(%%rbx)  # call stack end
        :
        : [machine_code] "{rax}" (jitted.address),
          [vm_state] "{rbx}" (&state),
        : .{
          .memory = true,
          .rax = true,
          .rbx = true,
          .rcx = true,
          .r8 = true,
          .r9 = true,
          .r10 = true,
          .r11 = true,
          .r12 = true,
          .r13 = true,
          .r14 = true,
          .r15 = true,
        });
    std.debug.print("state = {any}\n", .{state});
    vm.heap.used = (@intFromPtr(state.heap_cursor) - @intFromPtr(vm.heap.memory.ptr)) / 8;
    vm.data_stack.used = (@intFromPtr(state.data_stack_cursor) - @intFromPtr(vm.data_stack.memory.ptr)) / 8;
    vm.call_stack.used = (@intFromPtr(state.call_stack_cursor) - @intFromPtr(vm.call_stack.memory.ptr)) / 8;

    std.debug.print("ran jitted instructions\n", .{});
    std.debug.print("data stack:", .{});
    for (vm.data_stack.memory[0..vm.data_stack.used]) |word| std.debug.print(" {x:02}", .{word});
    std.debug.print("\n", .{});
}

fn compile_all(emitter: *Emitter, instructions: []const Instruction) !void {
    for (instructions) |instruction|
        try compile_single(emitter, instruction);
}
fn compile_single(emitter: *Emitter, instruction: Instruction) error{OutOfMemory}!void {
    switch (instruction) {
        .word => |word| {
            try emitter.emit("mov rax, {:64}", .{@as(u64, @intCast(word))});
            try emitter.emit("mov [r10], rax", .{});
            try emitter.emit("add r10, 8", .{});
        },
        .address => |object| {
            try emitter.emit("mov rax, {:64}", .{@as(u64, @intCast(object.address))});
            try emitter.emit("mov [r10], rax", .{});
            try emitter.emit("add r10, 8", .{});
        },
        .stack => |offset| {
            try emitter.emit("mov rax, r10", .{});
            try emitter.emit("mov rbx, {:64}", .{@as(u64, @intCast(offset * 8 + 8))});
            try emitter.emit("sub rax, rbx", .{});
            try emitter.emit("mov rax, [rax]", .{});
            try emitter.emit("mov [r10], rax", .{});
            try emitter.emit("add r10, 8", .{});
        },
        .pop => |amount| {
            try emitter.emit("mov rax, {:64}", .{@as(u64, @intCast(amount * 8))});
            try emitter.emit("sub r10, rax", .{});
        },
        .popover => |amount| {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("mov rbx, {:64}", .{@as(u64, @intCast(amount * 8))});
            try emitter.emit("sub r10, rbx", .{});
            try emitter.emit("mov [r10 - 8], rax", .{});
        },
        .add => {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("add rax, [r10 - 16]", .{});
            try emitter.emit("mov [r10 - 16], rax", .{});
            try emitter.emit("sub r10, 8", .{});
        },
        .subtract => {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("sub rax, [r10 - 16]", .{});
            try emitter.emit("mov [r10 - 16], rax", .{});
            try emitter.emit("sub r10, 8", .{});
        },
        .multiply => {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("imul rax, [r10 - 16]", .{});
            try emitter.emit("mov [r10 - 16], rax", .{});
            try emitter.emit("sub r10, 8", .{});
        },
        .divide => {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("div rax, [r10 - 16]", .{});
            try emitter.emit("mov [r10 - 16], rax", .{});
            try emitter.emit("sub r10, 8", .{});
        },
        .modulo => {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("div rax, [r10 - 16]", .{});
            try emitter.emit("mov [r10 - 16], rax", .{});
            try emitter.emit("sub r10, 8", .{});
        },
        .shl => {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("add rax, [r10 - 16]", .{});
            try emitter.emit("mov [r10 - 16], rax", .{});
            try emitter.emit("sub r10, 8", .{});
        },
        .shr => {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("add rax, [r10 - 16]", .{});
            try emitter.emit("mov [r10 - 16], rax", .{});
            try emitter.emit("sub r10, 8", .{});
        },
        .compare => @panic("JIT compare"), // equal: 0, greater: 1, less: 2
        .@"if" => |if_| {
            const then = try compile(.{ .ally = emitter.ally }, if_.then);
            const else_ = try compile(.{ .ally = emitter.ally }, if_.else_);
            try emitter.emit("mov rax, {:64}", .{@as(u64, @bitCast(@intFromPtr(else_.address)))});
            try emitter.emit("mov rbx, {:64}", .{@as(u64, @bitCast(@intFromPtr(then.address)))});
            try emitter.emit("sub r10, 8", .{});
            try emitter.emit("mov rcx, [r10]", .{});
            try emitter.emit("cmp rcx, 0", .{});
            try emitter.emit("cmovnz rax, rbx", .{});
            try emitter.emit("call rax", .{});
        },
        .new => |new| {
            try emitter.emit("mov rcx, r8", .{});
            try emitter.emit("mov rax, {:64}", .{
              @as(u64, @bitCast(Heap.Header {
                .num_words = @intCast(new.num_words),
                .is_inner = if (new.has_pointers) 1 else 0,
              })),
            });
            try emitter.emit("mov [r8], rax", .{});
            try emitter.emit("add r8, 8", .{});
            try emitter.emit("mov rax, {:64}", .{ @as(u64, @intCast(new.num_words * 8)) });
            try emitter.emit("sub r10, rax", .{ });
            try emitter.emit("mov rax, r10", .{});
            for (0..new.num_words) |_| {
                try emitter.emit("mov rbx, [rax]", .{});
                try emitter.emit("mov [r8], rbx", .{});
                try emitter.emit("add rax, 8", .{});
                try emitter.emit("add r8, 8", .{});
            }
            try emitter.emit("mov [r10], rcx", .{});
            try emitter.emit("add r10, 8", .{});
        },
        .flatptro => @panic("JIT flatptro"),
        .flatlito => @panic("JIT flatlito"),
        .points => @panic("JIT points"),
        .size => {
          try emitter.emit("mov rax, [r10 - 8]", .{});
          try emitter.emit("mov rbx, 0xffffffffffff", .{});
          try emitter.emit("and rax, rbx", .{});
        },
        .load => {
          try emitter.emit("mov rax, [r10 - 8]", .{});
          try emitter.emit("add rax, [r10 - 16]", .{});
          try emitter.emit("add rax, 8", .{});
          try emitter.emit("mov rax, [rax]", .{});
          try emitter.emit("mov [r10 - 16], rax", .{});
          try emitter.emit("sub r10, 8", .{});
        },
        .heapsize => @panic("JIT heapsize"),
        .gc => @panic("JIT gc"),
        .eval => {
          try emitter.emit("mov rax, {:64}", .{ @as(u64, @bitCast(@intFromPtr(&hook_eval))) });
          try emitter.emit("call rax", .{});
        },
        .crash => {
          try emitter.emit("mov rax, {:64}", .{ @as(u64, @bitCast(@intFromPtr(&hook_crash))) });
          try emitter.emit("call rax", .{});
        },
    }
}
fn hook_crash() callconv(.c) void {
  std.debug.print("oh no! crashing\n", .{});
  std.process.exit(1);
}
fn hook_eval() callconv(.c) void {
  std.debug.print("oh no! evaling\n", .{});
  std.process.exit(1);
}

const Emitter = struct {
    ally: std.mem.Allocator,
    bytes: ArrayList(u8),

    pub fn init(ally: std.mem.Allocator) Emitter {
        return .{ .ally = ally, .bytes = ArrayList(u8).empty };
    }

    fn emit_byte(self: *Emitter, byte: u8) !void {
        try self.bytes.append(self.ally, byte);
    }

    pub fn emit(self: *Emitter, comptime x86_instr: []const u8, args: anytype) !void {
        std.debug.print("{s:<30} {any}\n", .{ x86_instr, args });
        const constants = .{
          .@"mov rax, {:64}" = "48 b8 {0:64le}",
            .@"mov rax, r10" = "4c 89 d0",
            .@"mov rax, r12" = "4c 89 e0",
            .@"mov rax, [rax]" = "48 8b 00",
            .@"mov rax, [r10]" = "49 8b 02",
            .@"mov rax, [r10 - 8]" = "49 8b 42 f8",
            .@"mov rax, [r12]" = "49 8b 04 24",
            .@"mov rbx, 0xffffffffffff" = "48 bb ff ff ff ff ff ff 00 00",
            .@"mov rbx, {:64}" = "48 bb {0:64le}",
            .@"mov rbx, [rax]" = "48 8b 18",
            .@"mov rcx, r8" = "4c 89 c1",
            .@"mov rcx, [r10]" = "49 8b 0a",
            .@"mov [r8], rax" = "49 89 00",
            .@"mov [r8], rbx" = "49 89 18",
            .@"mov [r10], rax" = "49 89 02",
            .@"mov [r10], rcx" = "49 89 0a",
            .@"mov [r10], {:32}" = "49 c7 02 {0:32le}",
            .@"mov [r10 - 8], rax" = "49 89 42 f8",
            .@"mov [r10 - 16], rax" = "49 89 42 f0",
            .@"mov [r12], rax" = "49 89 04 24",
            .@"mov [r12], {:32}" = "49 c7 04 24 {0:32le}",
            .@"mov [r12 - 16], rax" = "49 89 44 24 f0",
            .@"add rax, 8" = "48 83 c0 08",
            .@"add rax, [r10 - 16]" = "49 03 42 f0",
            .@"add r8, 8" = "49 83 c0 08",
            .@"add r10, 8" = "49 83 c2 08",
            .@"add r12, 8" = "49 83 c4 08",
            .@"sub rax, 8" = "49 83 e8 08",
            .@"sub rax, {:32}" = "48 2d {0:32le}", // TODO: remove?
            .@"sub rax, rbx" = "48 29 d8",
            .@"sub rax, [r10 - 16]" = "49 2b 42 f0",
            .@"sub r10, rax" = "49 29 c2",
            .@"sub r10, rbx" = "49 29 da",
            .@"sub r10, 8" = "49 83 ea 08",
            .@"sub r10d, {:32}" = "41 81 ea {0:32le}",
            .@"and rax, rbx" = "48 21 d8",
            .@"cmp rcx, 0" = "48 83 f9 00",
            .@"cmovnz rax, rbx" = "48 0f 45 c3",
            .@"call rax" = "ff d0",
            .ret = "c3",
        };
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
                        if (comptime std.mem.eql(u8, how_to_emit, "32le")) {
                          if (@TypeOf(value) != u32) @compileError("expected u32, got " ++ @typeName(@TypeOf(value)));
                          try self.emit_byte(@truncate(value));
                          try self.emit_byte(@truncate(value >> 8));
                          try self.emit_byte(@truncate(value >> 16));
                          try self.emit_byte(@truncate(value >> 24));
                        } else if (comptime std.mem.eql(u8, how_to_emit, "64le")) {
                          if (@TypeOf(value) != u64) @compileError("expected u64, got " ++ @typeName(@TypeOf(value)));
                          try self.emit_byte(@truncate(value));
                          try self.emit_byte(@truncate(value >> 8));
                          try self.emit_byte(@truncate(value >> 16));
                          try self.emit_byte(@truncate(value >> 24));
                          try self.emit_byte(@truncate(value >> 32));
                          try self.emit_byte(@truncate(value >> 40));
                          try self.emit_byte(@truncate(value >> 48));
                          try self.emit_byte(@truncate(value >> 56));
                        } else @compileError("unknown formatting" ++ how_to_emit);
                    } else {
                        const byte = comptime std.fmt.parseInt(u8, item, 16) catch unreachable;
                        try self.emit_byte(byte);
                    }
                }
                return;
            }
        }
        @panic("Unknown instruction: " ++ x86_instr);
    }
};
