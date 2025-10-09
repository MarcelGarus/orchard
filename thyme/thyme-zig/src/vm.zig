const std = @import("std");
const ArrayList = std.ArrayList;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
const Instruction = @import("instruction.zig").Instruction;
const Object = @import("object.zig");

heap: *Heap,
data_stack: ArrayList(Word),
call_stack: ArrayList(Address),

const Vm = @This();

pub fn init(heap: *Heap) Vm {
    return .{
        .heap = heap,
        .data_stack = ArrayList(Word).empty,
        .call_stack = ArrayList(Address).empty,
    };
}

pub fn eval(heap: *Heap, env: anytype, code: []const u8) !Object {
    const fun = try Object.new_fun_from_code(heap, env, code);
    return try call(heap, fun, .{});
}

pub fn call(heap: *Heap, fun: Object, args: anytype) !Object {
    const ally = heap.ally;
    var data_stack = ArrayList(Word).empty;
    var call_stack = ArrayList(Object).empty;
    var ip = fun.instructions();

    if (fun.num_params() != args.len)
        @panic("called function with wrong number of params");
    inline for (args) |arg| {
        if (@TypeOf(arg) == Object) @compileError("args should be objects");
        try data_stack.append(ally, @intCast(arg.address.address));
    }

    while (true) {
        const parsed = Instruction.parse_first(ip) catch |e| {
            std.debug.print("Couldn't parse instruction:\n{f}", .{ip});
            return e;
        } orelse {
            ip = call_stack.pop() orelse {
                // The root function returned.
                return .{
                    .heap = heap,
                    .address = .{ .address = data_stack.pop() orelse unreachable },
                };
            };
            continue;
        };
        const instruction = parsed.instruction;
        ip = parsed.rest;

        // for (0..call_stack.items.len) |_| std.debug.print("  ", .{});
        // std.debug.print("{any}\n", .{data_stack.items});
        // for (0..call_stack.items.len) |_| std.debug.print("  ", .{});
        // std.debug.print("Running {f}", .{instruction});

        switch (instruction) {
            .push_word => |word| try data_stack.append(ally, word),
            .push_address => |object| try data_stack.append(
                ally,
                object.address.address,
            ),
            .push_from_stack => |offset| try data_stack.append(
                ally,
                data_stack.items[data_stack.items.len - 1 - offset],
            ),
            .pop => |amount| data_stack.items.len -= amount,
            .pop_below_top => |amount| {
                const top = data_stack.pop() orelse return error.bad_instruction;
                data_stack.items.len -= amount;
                try data_stack.append(ally, top);
            },
            .add => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.bad_instruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.bad_instruction);
                try data_stack.append(ally, @bitCast(a +% b));
            },
            .subtract => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.bad_instruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.bad_instruction);
                try data_stack.append(ally, @bitCast(a -% b));
            },
            .multiply => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.bad_instruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.bad_instruction);
                try data_stack.append(ally, @bitCast(a *% b));
            },
            .divide => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.bad_instruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.bad_instruction);
                try data_stack.append(ally, @bitCast(@divTrunc(a, b)));
            },
            .modulo => {
                const b: i64 = @bitCast(data_stack.pop() orelse return error.bad_instruction);
                const a: i64 = @bitCast(data_stack.pop() orelse return error.bad_instruction);
                try data_stack.append(ally, @bitCast(@mod(a, b)));
            },
            .if_not_zero => |if_| {
                const condition = data_stack.pop() orelse return error.bad_if;
                try call_stack.append(ally, ip);
                ip = if (condition != 0) if_.then else if_.else_;
            },
            .new => |new| {
                const pointers = try ally.alloc(Address, new.num_pointers);
                const literals = try ally.alloc(Word, new.num_literals);
                for (0..new.num_literals) |i|
                    literals[new.num_literals - 1 - i] = data_stack.pop() orelse return error.bad_instruction;
                for (0..new.num_pointers) |i|
                    pointers[new.num_pointers - 1 - i] = .{
                        .address = data_stack.pop() orelse return error.bad_instruction,
                    };
                const address = try heap.new(.{
                    .tag = new.tag,
                    .pointers = pointers,
                    .literals = literals,
                });
                try data_stack.append(ally, address.address);
            },
            .tag => {
                const address = Address{
                    .address = data_stack.pop() orelse return error.bad_instruction,
                };
                const tag = heap.get(address).tag;
                try data_stack.append(ally, @intCast(tag));
            },
            .num_pointers => {
                const address = Address{
                    .address = data_stack.pop() orelse return error.bad_instruction,
                };
                const num_pointers = heap.get(address).pointers.len;
                try data_stack.append(ally, @intCast(num_pointers));
            },
            .num_literals => {
                const address = Address{
                    .address = data_stack.pop() orelse return error.bad_instruction,
                };
                const num_literals = heap.get(address).literals.len;
                try data_stack.append(ally, @intCast(num_literals));
            },
            .load => {
                const offset: usize = @intCast(data_stack.pop() orelse return error.bad_instruction);
                const base = Address{
                    .address = @intCast(data_stack.pop() orelse return error.bad_instruction),
                };
                const word = heap.load(base, offset);
                try data_stack.append(ally, word);
            },
            .eval => {
                const evaled = Object{
                    .heap = heap,
                    .address = .{
                        .address = data_stack.pop() orelse return error.bad_instruction,
                    },
                };
                try call_stack.append(ally, ip);
                ip = evaled;
            },
            .crash => {
                @panic("crashed");
            },
            else => @panic("todo"),
        }
    }
}
