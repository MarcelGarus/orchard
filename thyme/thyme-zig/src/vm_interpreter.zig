const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Map = std.AutoArrayHashMap;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
const Instruction = @import("instruction.zig").Instruction;
const Object = @import("object.zig");

const Self = @This();

heap: *Heap,
parsed: Map(Address, []const Instruction),

pub fn init(heap: *Heap, ally: Ally) Self {
    return .{
        .heap = heap,
        .parsed = Map(Address, []const Instruction).init(ally),
    };
}

pub fn call(vm: *Self, ally: Ally, fun: Object, args: []const Object) !Object {
    var data_stack = ArrayList(Word).empty;

    for (args) |arg| try data_stack.append(ally, @intCast(arg.address));

    var resources = Resources{ .num_instructions = 0 };
    try vm.run(ally, fun.instructions(), &data_stack, &resources);

    std.debug.print("{} instructions ran\n", .{resources.num_instructions});
    return .{
        .heap = vm.heap,
        .address = data_stack.pop() orelse unreachable,
    };
}

const Resources = struct { num_instructions: usize };

pub fn run(
    vm: *Self,
    ally: Ally,
    instructions: Object,
    data_stack: *ArrayList(Word),
    resources: *Resources,
) !void {
    const parsed = parsed: {
        if (vm.parsed.get(instructions.address)) |parsed|
            break :parsed parsed;
        const parsed = try Instruction.parse_all(ally, instructions);
        try vm.parsed.put(instructions.address, parsed);
        break :parsed parsed;
    };
    try vm.run_parsed(ally, parsed, data_stack, resources);
}

fn run_parsed(
    vm: *Self,
    ally: Ally,
    instructions: []const Instruction,
    data_stack: *ArrayList(Word),
    resources: *Resources,
) !void {
    for (instructions) |instruction| {
        resources.num_instructions += 1;

        // std.debug.print("{any}\n", .{data_stack.items});
        // std.debug.print("Running {f}", .{instruction});

        switch (instruction) {
            .push_word => |word| try data_stack.append(ally, word),
            .push_address => |object| try data_stack.append(ally, object.address),
            .push_from_stack => |offset| try data_stack.append(
                ally,
                data_stack.items[data_stack.items.len - 1 - offset],
            ),
            .pop => |amount| data_stack.items.len -= amount,
            .pop_below_top => |amount| {
                const top = data_stack.pop() orelse return error.BadInstruction;
                data_stack.items.len -= amount;
                try data_stack.append(ally, top);
            },
            .add => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                try data_stack.append(ally, @bitCast(a +% b));
            },
            .subtract => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                try data_stack.append(ally, @bitCast(a -% b));
            },
            .multiply => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                try data_stack.append(ally, @bitCast(a *% b));
            },
            .divide => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                try data_stack.append(ally, @bitCast(@divTrunc(a, b)));
            },
            .modulo => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                try data_stack.append(ally, @bitCast(@mod(a, b)));
            },
            .compare => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.BadInstruction);
                const result: Word = if (a == b) 0 else if (a > b) 1 else 2;
                try data_stack.append(ally, result);
            },
            .if_not_zero => |if_| {
                const condition = data_stack.pop() orelse return error.bad_if;
                const body_to_run = if (condition != 0) if_.then else if_.else_;
                try vm.run_parsed(ally, body_to_run, data_stack, resources);
            },
            .new => |new| {
                const pointers = try ally.alloc(Word, new.num_pointers);
                const literals = try ally.alloc(Word, new.num_literals);
                for (0..new.num_literals) |i|
                    literals[new.num_literals - 1 - i] = data_stack.pop() orelse return error.BadInstruction;
                for (0..new.num_pointers) |i|
                    pointers[new.num_pointers - 1 - i] = data_stack.pop() orelse return error.BadInstruction;
                const address = try vm.heap.new(.{
                    .tag = new.tag,
                    .pointers = pointers,
                    .literals = literals,
                });
                try data_stack.append(ally, address);
            },
            .tag => {
                const address = data_stack.pop() orelse return error.BadInstruction;
                const tag = vm.heap.get(address).tag;
                try data_stack.append(ally, @intCast(tag));
            },
            .num_pointers => {
                const address = data_stack.pop() orelse return error.BadInstruction;
                const num_pointers = vm.heap.get(address).pointers.len;
                try data_stack.append(ally, @intCast(num_pointers));
            },
            .num_literals => {
                const address = data_stack.pop() orelse return error.BadInstruction;
                const num_literals = vm.heap.get(address).literals.len;
                try data_stack.append(ally, @intCast(num_literals));
            },
            .load => {
                const offset: usize = @intCast(data_stack.pop() orelse return error.BadInstruction);
                const base = data_stack.pop() orelse return error.BadInstruction;
                const word = vm.heap.load(base, offset);
                try data_stack.append(ally, word);
            },
            .eval => {
                const to_eval = Object{
                    .heap = vm.heap,
                    .address = data_stack.pop() orelse return error.BadInstruction,
                };
                try vm.run_parsed(
                    ally,
                    try Instruction.parse_all(ally, to_eval),
                    data_stack,
                    resources,
                );
            },
            .crash => {
                const message = Object{
                    .heap = vm.heap,
                    .address = data_stack.pop() orelse return error.BadInstruction,
                };
                std.debug.print("Crashed:\n{f}\n", .{message});
                @panic("crashed");
            },
            else => @panic("todo"),
        }
    }
}
