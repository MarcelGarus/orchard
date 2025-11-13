// This module JIT-compiles instructions to x86 machine code. We map the VM
// semantics like this:
//
// Heap:        The VM's heap is a memory region that we allocate (the Zig Heap
//              struct does that). The heap can't grow dynamically; if you use
//              it too much, you get an out-of-memory error instead.
// Data stack:  The VM's data stack is a memory region that we allocate. The
//              data stack can't grow dynamically; code can overflow it.
// Call stack:  The VM's call stack is a memory region that we allocate. The
//              call stack can't grow dynamically; code can overflow it.
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

const Ally = struct {};

const Jitted = struct {
    address: [*]u8, // Pointer to non-writable, executable memory with machine code.
    len: usize, // Length of the memory.

    pub fn deinit() !void {
        std.linux.munmap();
    }
};

pub fn compile(ally: Ally, instructions: []const Instruction) !Jitted {
    // Turn the instructions into x86 machine code bytes.
    var emitter = Emitter.init(ally);
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

pub fn run(jitted: Jitted, vm: *Vm) !void {
    std.debug.print("data stack:", .{});
    for (vm.data_stack.memory) |byte| std.debug.print(" {x:02}", .{byte});
    std.debug.print(" (cursor at {})\n", .{vm.data_stack.used});
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
    for (vm.data_stack.memory) |byte| std.debug.print(" {x:02}", .{byte});
    std.debug.print(" (cursor at {})\n", .{vm.data_stack.used});
}

fn compile_all(emitter: *Emitter, instructions: []const Instruction) !void {
    for (instructions) |instruction|
        try compile_single(emitter, instruction);
}
fn compile_single(emitter: *Emitter, instruction: Instruction) error{OutOfMemory}!void {
    switch (instruction) {
        .push_word => |word| {
            try emitter.emit("mov [r10], {:32}", .{@as(u32, @intCast(word))});
            try emitter.emit("add r10, 8", .{});
        },
        .push_address => |object| {
            try emitter.emit("mov [r10], {:32}", .{@as(u32, @intCast(object.address))});
            try emitter.emit("add r10, 8", .{});
        },
        .push_from_stack => |offset| {
            try emitter.emit("mov rax, r10", .{});
            try emitter.emit("sub rax, {:32}", .{@as(u32, @intCast(offset * 8)) + 8});
            try emitter.emit("mov rax, [rax]", .{});
            try emitter.emit("mov [r10], rax", .{});
            try emitter.emit("add r10, 8", .{});
        },
        .pop => |amount| {
            try emitter.emit("sub r10, {:32}", .{@as(u32, @intCast(amount * 8))});
        },
        .pop_below_top => |amount| {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("sub r10, {:32}", .{@as(u32, @intCast(amount * 8))});
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
        .shift_left => {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("add rax, [r10 - 16]", .{});
            try emitter.emit("mov [r10 - 16], rax", .{});
            try emitter.emit("sub r10, 8", .{});
        },
        .shift_right => {
            try emitter.emit("mov rax, [r10 - 8]", .{});
            try emitter.emit("add rax, [r10 - 16]", .{});
            try emitter.emit("mov [r10 - 16], rax", .{});
            try emitter.emit("sub r10, 8", .{});
        },
        .compare => @panic("JIT compare"), // equal: 0, greater: 1, less: 2
        .if_not_zero => @panic("JIT if_not_zero"),
        .new => @panic("JIT new"),
        .tag => @panic("JIT tag"),
        .num_pointers => @panic("JIT num_pointers"),
        .num_literals => @panic("JIT num_literals"),
        .load => @panic("JIT load"),
        .eval => @panic("JIT eval"),
        .crash => @panic("JIT crash"),
        .heap_checkpoint => @panic("JIT heap_checkpoint"),
        .collect_garbage => @panic("JIT collect_garbage"),
    }
}

const Emitter = struct {
    ally: Ally,
    bytes: ArrayList(u8),

    pub fn init(ally: Ally) Emitter {
        return .{ .ally = ally, .bytes = ArrayList(u8).empty };
    }

    fn emit_byte(self: *Emitter, byte: u8) !void {
        try self.bytes.append(self.ally, byte);
    }

    pub fn emit(self: *Emitter, comptime x86_instr: []const u8, args: anytype) !void {
        std.debug.print("{s:<30} {any}\n", .{ x86_instr, args });
        const constants = .{
            .@"mov rax, r10" = "4c 89 d0",
            .@"mov rax, r12" = "4c 89 e0",
            .@"mov rax, [rax]" = "48 8b 00",
            .@"mov rax, [r10]" = "49 8b 02",
            .@"mov rax, [r10 - 8]" = "49 8b 42 f8",
            .@"mov rax, [r12]" = "49 8b 04 24",
            .@"mov [r10], rax" = "49 89 02",
            .@"mov [r10], {:32}" = "49 c7 02 {0:le}",
            .@"mov [r10 - 8], rax" = "49 89 42 f8",
            .@"mov [r10 - 16], rax" = "49 89 42 f0",
            .@"mov [r12], rax" = "49 89 04 24",
            .@"mov [r12], {:32}" = "49 c7 04 24 {0:le}",
            .@"mov [r12 - 16], rax" = "49 89 44 24 f0",
            .@"add r10, 8" = "49 83 c2 08",
            .@"add r12, 8" = "49 83 c4 08",
            .@"add rax, [r10 - 16]" = "49 03 42 f0",
            .@"sub rax, 8" = "49 83 e8 08",
            .@"sub r10, 8" = "49 83 ea 08",
            .@"sub rax, {:32}" = "48 2d {0:le}",
            .@"sub r10, {:32}" = "49 81 ea {0:le}",
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
                        if (comptime std.mem.eql(u8, how_to_emit, "le")) {
                            switch (@TypeOf(value)) {
                                u32 => {
                                    try self.emit_byte(@truncate(value));
                                    try self.emit_byte(@truncate(value >> 8));
                                    try self.emit_byte(@truncate(value >> 16));
                                    try self.emit_byte(@truncate(value >> 24));
                                },
                                else => @compileError("le only supports u32"),
                            }
                        } else @compileError("only supports le encoding");
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
