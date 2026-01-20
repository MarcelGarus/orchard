const std = @import("std");
const ArrayList = std.ArrayList;
const Map = std.AutoArrayHashMap;
const Ally = std.mem.Allocator;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const Instruction = @import("instruction.zig").Instruction;
const Val = @import("value.zig");
const ObjMap = @import("obj_map.zig").ObjMap;

const Vm = @This();

ally: Ally,
heap: *Heap,
data_stack: Stack,
call_stack: Stack,
parsed_cache: ObjMap([]const Instruction),

const Stack = struct {
    memory: []Word,
    used: usize,

    pub fn init(ally: Ally, size: usize) !Stack {
        return .{ .memory = try ally.alloc(Word, size), .used = 0 };
    }
    pub fn push(self: *Stack, word: Word) !void {
        if (self.memory.len == self.used) @panic("stack overflow");
        self.memory[self.used] = word;
        self.used += 1;
    }
    pub fn pop(self: *Stack) Word {
        self.used -= 1;
        return self.memory[self.used];
    }
    pub fn pop_n(self: *Stack, n: usize) void {
        self.used -= n;
    }
    pub fn get(self: *Stack, offset: usize) Word {
        return self.memory[self.used - 1 - offset];
    }
    pub fn is_empty(self: Stack) bool {
        return self.used == 0;
    }
};

pub fn init(heap: *Heap, ally: Ally) !Vm {
    return .{
        .ally = ally,
        .heap = heap,
        .data_stack = try Stack.init(ally, 1000000),
        .call_stack = try Stack.init(ally, 1000000),
        .parsed_cache = ObjMap([]const Instruction).empty,
    };
}

pub fn call(vm: *Vm, lambda: Val.Lambda, args: []const Val.Value) !Val.Value {
    const instructions = lambda.obj.child(1);
    const closure = lambda.obj.child(2);
    for (args) |arg| try vm.data_stack.push(arg.obj.address);
    try vm.data_stack.push(closure.address);
    try vm.run(try vm.parse(instructions));
    return .{ .obj = .{ .address = vm.data_stack.pop() } };
}
pub fn parse(vm: *Vm, instructions_obj: Obj) ![]const Instruction {
    if (vm.parsed_cache.get(instructions_obj)) |parsed| return parsed;
    const parsed = try Instruction.parse_instructions(vm.ally, instructions_obj);
    try vm.parsed_cache.put(vm.ally, instructions_obj, parsed);
    return parsed;
}
pub fn run(vm: *Vm, instructions: []const Instruction) !void {
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
                const obj = if (new.has_pointers)
                  try vm.heap.new_inner(@ptrCast(words))
                else
                  try vm.heap.new_leaf(@ptrCast(words));
                vm.data_stack.pop_n(new.num_words);
                try vm.data_stack.push(obj.address);
            },
            .flatptro => {
                var obj = Obj{ .address = vm.data_stack.pop() };
                var b = try vm.heap.build_inner();
                while (true) {
                    switch (obj.size()) {
                        0 => break,
                        2 => {
                            try b.emit(obj.child(0));
                            obj = obj.child(1);
                        },
                        else => unreachable,
                    }
                }
                const flattened = b.finish();
                try vm.data_stack.push(flattened.address);
            },
            .flatlito => {
                var obj = Obj{ .address = vm.data_stack.pop() };
                var b = try vm.heap.build_leaf();
                while (true) {
                    switch (obj.size()) {
                        0 => break,
                        2 => {
                            try b.emit(obj.child(0).word(0));
                            obj = obj.child(1);
                        },
                        else => unreachable,
                    }
                }
                const flattened = b.finish();
                try vm.data_stack.push(flattened.address);
            },
            .points => {
                const obj = Obj{ .address = vm.data_stack.pop() };
                try vm.data_stack.push(if (obj.is_inner()) 1 else 0);
            },
            .size => {
                const obj = Obj{ .address = vm.data_stack.pop() };
                const num_words = obj.size();
                try vm.data_stack.push(num_words);
            },
            .load => {
                const offset: usize = vm.data_stack.pop();
                const base = Obj{ .address = vm.data_stack.pop() };
                const word: usize = if (base.is_inner()) base.child(offset).address else base.word(offset);
                try vm.data_stack.push(word);
            },
            .heapsize => {
                const checkpoint = vm.heap.checkpoint();
                try vm.data_stack.push(checkpoint.address);
            },
            .gc => {
                const keep = vm.data_stack.pop();
                const checkpoint = vm.data_stack.pop();
                vm.heap.dump_stats();
                std.debug.print("garbage collecting...\n", .{});
                const mapped_keep = try vm.heap.garbage_collect(
                    vm.ally,
                    .{ .address = checkpoint },
                    Obj{ .address = keep },
                );
                vm.heap.dump_stats();
                try vm.data_stack.push(mapped_keep.address);
            },
            .eval => try vm.run(try vm.parse(Obj{ .address = vm.data_stack.pop() })),
            .crash => {
                const message = vm.data_stack.pop();
                std.debug.print("Crashed:\n", .{});
                {
                    var buffer: [64]u8 = undefined;
                    const bw = std.debug.lockStderrWriter(&buffer);
                    defer std.debug.unlockStderrWriter();
                    vm.heap.format(Obj{ .address = message }, bw) catch unreachable;
                    bw.print("\n", .{}) catch unreachable;
                }
                std.process.exit(1);
            },
        }
    }
}

pub fn deduplicate(vm: *Vm, checkpoint: Heap.Checkpoint, obj: Obj) !Obj {
    var map = try vm.heap.deduplicate(vm.ally, checkpoint);
    vm.parsed_cache.remove_everything_after(checkpoint.address);
    const mapped = map.get(obj) orelse obj;
    map.deinit();
    return mapped;
}

pub fn garbage_collect(vm: *Vm, checkpoint: Heap.Checkpoint, keep: Obj) !Obj {
    const mapped = vm.heap.garbage_collect(vm.ally, checkpoint, keep);
    vm.parsed_cache.remove_everything_after(checkpoint.address);
    return mapped;
}
