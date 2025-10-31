const std = @import("std");
const ArrayList = std.ArrayList;
const Map = std.AutoArrayHashMap;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Address = Heap.Address;
const Instruction = @import("instruction.zig").Instruction;
const Object = @import("object.zig");
const Vm = @import("vm.zig");

pub const Ally = struct {
    ally: std.mem.Allocator,

    pub fn init(ally: std.mem.Allocator) !Ally {
        return .{ .ally = ally };
    }
};

pub const Jitted = []const Instruction;

pub fn compile(ally: Ally, instructions: []const Instruction) !Jitted {
    // TODO: copy instructions to safety, they should be allocated after this call
    _ = ally;
    return instructions;
}

pub fn run(vm: *Vm, instructions: Jitted) !void {
    for (instructions) |instruction| {
        // std.debug.print("{any}\n", .{data_stack.items});
        // std.debug.print("Running {f}", .{instruction});

        switch (instruction) {
            .push_word => |word| try vm.data_stack.push(word),
            .push_address => |object| try vm.data_stack.push(object.address),
            .push_from_stack => |offset| try vm.data_stack.push(vm.data_stack.get(offset)),
            .pop => |amount| vm.data_stack.pop_n(amount),
            .pop_below_top => |amount| {
                const top = vm.data_stack.pop();
                vm.data_stack.pop_n(amount);
                try vm.data_stack.push(top);
            },
            .add => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                try vm.data_stack.push(@bitCast(a +% b));
            },
            .subtract => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                try vm.data_stack.push(@bitCast(a -% b));
            },
            .multiply => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                try vm.data_stack.push(@bitCast(a *% b));
            },
            .divide => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                try vm.data_stack.push(@bitCast(@divTrunc(a, b)));
            },
            .modulo => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                try vm.data_stack.push(@bitCast(@mod(a, b)));
            },
            .compare => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                const result: Word = if (a == b) 0 else if (a > b) 1 else 2;
                try vm.data_stack.push(result);
            },
            .if_not_zero => |if_| {
                const condition = vm.data_stack.pop();
                const body_to_run = if (condition != 0) if_.then else if_.else_;
                try run(vm, body_to_run);
            },
            .new => |new| {
                const stack = vm.data_stack.memory[0..vm.data_stack.used];
                const literals = stack[stack.len - new.num_literals ..];
                const pointers = stack[stack.len - new.num_pointers - new.num_literals .. stack.len - new.num_literals];
                const address = try vm.heap.new(.{
                    .tag = new.tag,
                    .pointers = pointers,
                    .literals = literals,
                });
                vm.data_stack.pop_n(new.num_pointers + new.num_literals);
                try vm.data_stack.push(address);
            },
            .tag => {
                const address = vm.data_stack.pop();
                const tag = vm.heap.get(address).tag;
                try vm.data_stack.push(@intCast(tag));
            },
            .num_pointers => {
                const address = vm.data_stack.pop();
                const num_pointers = vm.heap.get(address).pointers.len;
                try vm.data_stack.push(@intCast(num_pointers));
            },
            .num_literals => {
                const address = vm.data_stack.pop();
                const num_literals = vm.heap.get(address).literals.len;
                try vm.data_stack.push(@intCast(num_literals));
            },
            .load => {
                const offset: usize = @intCast(vm.data_stack.pop());
                const base = vm.data_stack.pop();
                const word = vm.heap.load(base, offset);
                try vm.data_stack.push(word);
            },
            .heap_checkpoint => {
                const checkpoint = vm.heap.checkpoint();
                try vm.data_stack.push(@intCast(checkpoint.used));
            },
            .collect_garbage => {
                const keep = vm.data_stack.pop();
                const checkpoint = vm.data_stack.pop();
                vm.heap.dump_stats();
                std.debug.print("garbage collecting...\n", .{});
                var mapping = try vm.heap.garbage_collect(
                    vm.ally,
                    .{ .used = checkpoint },
                    keep,
                );
                vm.heap.dump_stats();
                const mapped_keep = mapping.get(keep) orelse unreachable;
                mapping.deinit();
                try vm.data_stack.push(mapped_keep);
            },
            .eval => {
                try vm.run(.{
                    .heap = vm.heap,
                    .address = vm.data_stack.pop(),
                });
            },
            .crash => {
                const message = Object{
                    .heap = vm.heap,
                    .address = vm.data_stack.pop(),
                };
                std.debug.print("Crashed:\n{f}\n", .{message});
                @panic("crashed");
            },
            else => @panic("todo"),
        }
    }
}
