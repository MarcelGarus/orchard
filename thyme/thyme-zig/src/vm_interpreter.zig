const std = @import("std");
const ArrayList = std.ArrayList;
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

pub const Jitted = []const Instruction;

pub fn compile(ally: Ally, instructions: []const Instruction) !Jitted {
    // TODO: copy instructions to safety, they should be deallocated after this call
    _ = ally;
    return instructions;
}

pub fn run(vm: *Vm, instructions: Jitted) !void {
    for (instructions) |instruction| {
        //std.debug.print("{any}\n", .{vm.data_stack.used});
        //std.debug.print("Running {f}", .{instruction});

        switch (instruction) {
            .word => |word| try vm.data_stack.push(word),
            .address => |object| try vm.data_stack.push(object.address),
            .stack => |offset| try vm.data_stack.push(vm.data_stack.get(offset)),
            .pop => |amount| vm.data_stack.pop_n(amount),
            .popover => |amount| {
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
            .shl => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                // std.debug.print("{} << {} is {}\n", .{ a, b, a >> @intCast(b) });
                try vm.data_stack.push(@bitCast(a << @intCast(b)));
            },
            .shr => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                // std.debug.print("{} >> {} is {}\n", .{ a, b, a >> @intCast(b) });
                try vm.data_stack.push(@bitCast(a >> @intCast(b)));
            },
            .compare => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                const result: Word = if (a == b) 0 else if (a > b) 1 else 2;
                try vm.data_stack.push(result);
            },
            .@"if" => |if_| {
                const condition = vm.data_stack.pop();
                const body_to_run = if (condition != 0) if_.then else if_.else_;
                try run(vm, body_to_run);
            },
            .new => |new| {
                const stack = vm.data_stack.memory[0..vm.data_stack.used];
                const words = stack[stack.len - new.num_words ..];
                const address = try vm.heap.new(.{ .has_pointers = new.has_pointers, .words = words });
                vm.data_stack.pop_n(new.num_words);
                try vm.data_stack.push(address);
            },
            .flatptro => {
                var obj = vm.data_stack.pop();
                var b = try vm.heap.object_builder();
                while (true) {
                    const words = vm.heap.get(obj).words;
                    switch (words.len) {
                        0 => break,
                        2 => {
                            try b.emit_pointer(words[0]);
                            obj = words[1];
                        },
                        else => unreachable,
                    }
                }
                const flattened = b.finish();
                try vm.data_stack.push(flattened);
            },
            .flatlito => {
                var obj = vm.data_stack.pop();
                var b = try vm.heap.object_builder();
                while (true) {
                    const words = vm.heap.get(obj).words;
                    switch (words.len) {
                        0 => break,
                        2 => {
                            try b.emit_literal(vm.heap.load(words[0], 0));
                            obj = words[1];
                        },
                        else => unreachable,
                    }
                }
                const flattened = b.finish();
                try vm.data_stack.push(flattened);
            },
            .points => {
                const address = vm.data_stack.pop();
                try vm.data_stack.push(if (vm.heap.get(address).has_pointers) 1 else 0);
            },
            .size => {
                const address = vm.data_stack.pop();
                const num_words = vm.heap.get(address).words.len;
                try vm.data_stack.push(@intCast(num_words));
            },
            .load => {
                const offset: usize = @intCast(vm.data_stack.pop());
                const base = vm.data_stack.pop();
                const word = vm.heap.load(base, offset);
                try vm.data_stack.push(word);
            },
            .heapsize => {
                const checkpoint = vm.heap.checkpoint();
                try vm.data_stack.push(@intCast(checkpoint.used));
            },
            .gc => {
                const keep = vm.data_stack.pop();
                const checkpoint = vm.data_stack.pop();
                vm.heap.dump_stats();
                std.debug.print("garbage collecting...\n", .{});
                const mapped_keep = try vm.heap.garbage_collect(
                    vm.ally,
                    .{ .used = checkpoint },
                    keep,
                );
                vm.heap.dump_stats();
                try vm.data_stack.push(mapped_keep);
            },
            .eval => try vm.run(vm.data_stack.pop()),
            .crash => {
                const message = vm.data_stack.pop();
                std.debug.print("Crashed:\n", .{});
                {
                    var buffer: [64]u8 = undefined;
                    const bw = std.debug.lockStderrWriter(&buffer);
                    defer std.debug.unlockStderrWriter();
                    vm.heap.format(message, bw) catch unreachable;
                    bw.print("\n", .{}) catch unreachable;
                }
                std.process.exit(1);
            },
        }
    }
}
