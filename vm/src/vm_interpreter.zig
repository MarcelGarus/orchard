const std = @import("std");
const ArrayList = std.ArrayList;
const Map = std.AutoArrayHashMap;
const Ally = std.mem.Allocator;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const Val = @import("pear_value.zig");
const ObjMap = @import("obj_map.zig").ObjMap;
const Ir = @import("ir.zig");
const get_symbol = Heap.get_symbol;

const Vm = @This();

ally: Ally,
heap: *Heap,
data_stack: Stack,
call_stack: Stack,
compiled_cache: ObjMap(CompiledFun),
instruction_count: usize = 0,

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

pub const CompiledFun = struct {
    instructions: []const Instruction,

    pub fn format(fun: CompiledFun, writer: *std.io.Writer) !void {
        try writer.print("compiled fun", .{});
        for (fun.instructions, 0..) |instr, i| {
            try writer.print("\n{d:4} {f}", .{ i, instr });
        }
    }
};
pub const Instruction = union(enum) {
    word: Word, // Pushes the word to the stack.
    address: Obj, // Pushes the address to the stack.
    stack: usize, // Pushes a word from the stack to the stack. Offset 0 = top element, 1 = below top, etc.
    pop: usize, // Pops the given number of words from the stack.
    popover: usize, // Keeps the top element on the stack, but pops the given number of words below that.
    add, // a b -> (a+b)
    subtract, // a b -> (a-b)
    multiply, // a b -> (a*b)
    divide, // a b -> (a/b)
    modulo, // a b -> (a%b)
    shift_left, // a b -> (a<<b). Stands for shift left.
    shift_right, // a b -> (a>>b). Stands for shift right.
    and_, // a b -> (a & b). Bitwise and.
    or_, // a b -> (a | b). Bitwise or.
    xor, // a b -> (a ^ b). Bitwise xor.
    compare, // Pops two words. Stack before: a b. If a == b, pushes 1. If a > b, pushes 1. If a < b, pushes 2.
    jump_if: usize, // Pops a word. If not 0, jumps to the instruction at the index.
    jump: usize,
    new_leaf: usize, // Creates a new heap object with the given number of words.
    new_inner: usize, // Creates a new heap object with the given number of words.
    flatten_to_inner, // Pops an address. An object of the form [a [b [c []]]] becomes [a b c].
    flatten_to_leaf, // Pops an address. An object of the form [[a] [[b] [[c] []]]] becomes [a b c].
    points, // Pops an address. Returns 1 or 0, depending on whether the object contains pointers.
    size, // Pops an address. Returns the size of the object.
    load, // addr offset -> ... Loads a word at the offset from the object.
    heap_size, // Pushes the size of the heap onto the stack. Can be used as a checkpoint for gc.
    gc, // heapsize obj -> obj. Collects garbage starting at the heapsize, only keeping dependencies of obj.
    call, // Pops an address, which should point to a function object. Calls it.
    crash, // Pops a message. Crashes with the message.

    pub fn format(instr: Instruction, writer: *std.io.Writer) !void {
        switch (instr) {
            .word => |word| try writer.print("word {}", .{word}),
            .address => |object| try writer.print("address {x}", .{object.address}),
            .stack => |offset| try writer.print("stack {}", .{offset}),
            .pop => |amount| try writer.print("pop {}", .{amount}),
            .popover => |amount| try writer.print("popover {}", .{amount}),
            .and_ => try writer.print("and", .{}),
            .or_ => try writer.print("or", .{}),
            .jump_if => |target| try writer.print("jump_if {}", .{target}),
            .jump => |target| try writer.print("jump {}", .{target}),
            .new_leaf => |size| try writer.print("new_leaf {}", .{size}),
            .new_inner => |size| try writer.print("new_inner {}", .{size}),
            inline else => try writer.print("{s}", .{@tagName(instr)}),
        }
    }
};

pub fn init(heap: *Heap, ally: Ally) !Vm {
    return .{
        .ally = ally,
        .heap = heap,
        .data_stack = try Stack.init(ally, 1000000),
        .call_stack = try Stack.init(ally, 1000000),
        .compiled_cache = ObjMap(CompiledFun).empty,
    };
}

pub fn run(vm: *Vm, fun: Ir.Fun, args: []const Word) error{ BadIr, UnknownVariable, OutOfMemory, Todo }!Word {
    const compiled = try vm.compile(fun);
    for (args) |arg| try vm.data_stack.push(arg);
    try vm.run_fun(compiled);
    return vm.data_stack.pop();
}
pub fn compile(vm: *Vm, fun: Ir.Fun) !CompiledFun {
    if (vm.compiled_cache.get(fun.obj)) |compiled| return compiled;
    const compiled = try really_compile(vm.ally, fun);
    try vm.compiled_cache.put(vm.ally, fun.obj, compiled);
    return compiled;
}

pub fn really_compile(ally: Ally, fun: Ir.Fun) !CompiledFun {
    std.debug.print("{f}", .{fun});
    var stack = std.ArrayList([]const u8).empty;
    defer stack.deinit(ally);
    for (fun.args()) |arg| try stack.append(ally, get_symbol(arg));
    var instrs = std.ArrayList(Instruction).empty;
    errdefer instrs.deinit(ally);
    try compile_expr(ally, fun, fun.body(), &stack, &instrs);
    if (fun.args().len > 0) try instrs.append(ally, .{ .popover = fun.args().len });
    const compiled = CompiledFun{ .instructions = try instrs.toOwnedSlice(ally) };
    std.debug.print("{f}\n", .{compiled});
    return compiled;
}
fn find_in_stack(stack: *const std.ArrayList([]const u8), name: []const u8) ?usize {
    var i: usize = stack.items.len;
    while (i > 0) {
        i -= 1;
        if (std.mem.eql(u8, stack.items[i], name)) return stack.items.len - 1 - i;
    }
    return null;
}
fn compile_expr(ally: std.mem.Allocator, root: Ir.Fun, expr: Ir.Expr, stack: *std.ArrayList([]const u8), instrs: *std.ArrayList(Instruction)) !void {
    switch (try expr.kind()) {
        .word => |w| try instrs.append(ally, .{ .word = w }),
        .object => |obj| try instrs.append(ally, .{ .address = obj }),
        .name => |n| {
            if (find_in_stack(stack, n)) |offset|
                try instrs.append(ally, .{ .stack = offset })
            else
                return error.UnknownVariable;
        },
        .let => |let| {
            try compile_expr(ally, root, let.def, stack, instrs);
            try stack.append(ally, let.name);
            try compile_expr(ally, root, let.expr, stack, instrs);
            _ = stack.pop();
            try instrs.append(ally, .{ .popover = 1 });
        },
        .add, .subtract, .multiply, .divide, .modulo, .shift_left, .shift_right, .and_, .or_, .xor, .compare => |args| {
            try compile_expr(ally, root, args.left, stack, instrs);
            try stack.append(ally, "");
            try compile_expr(ally, root, args.right, stack, instrs);
            _ = stack.pop();
            try instrs.append(ally, switch (try expr.kind()) {
                .add => .add,
                .subtract => .subtract,
                .multiply => .multiply,
                .divide => .divide,
                .modulo => .modulo,
                .shift_left => .shift_left,
                .shift_right => .shift_right,
                .and_ => .and_,
                .or_ => .or_,
                .xor => .xor,
                .compare => .compare,
                else => unreachable,
            });
        },
        .if_ => |if_| {
            try compile_expr(ally, root, if_.condition, stack, instrs);
            const after_condition = instrs.items.len;
            try instrs.append(ally, .crash); // placeholder
            try compile_expr(ally, root, if_.else_, stack, instrs);
            const after_else = instrs.items.len;
            try instrs.append(ally, .crash); // placeholder
            const before_then = instrs.items.len;
            try compile_expr(ally, root, if_.then, stack, instrs);
            const after_if = instrs.items.len;
            instrs.items[after_condition] = .{ .jump_if = before_then };
            instrs.items[after_else] = .{ .jump = after_if };
        },
        .new_leaf => |args| {
            for (args) |arg| {
                try compile_expr(ally, root, arg, stack, instrs);
                try stack.append(ally, "");
            }
            for (args) |_| _ = stack.pop();
            try instrs.append(ally, .{ .new_leaf = args.len });
        },
        .new_inner => |args| {
            for (args) |arg| {
                try compile_expr(ally, root, arg, stack, instrs);
                try stack.append(ally, "");
            }
            for (args) |_| _ = stack.pop();
            try instrs.append(ally, .{ .new_inner = args.len });
        },
        .flatten_to_leaf, .flatten_to_inner, .points, .size, .crash => |inner| {
            try compile_expr(ally, root, inner, stack, instrs);
            try instrs.append(ally, switch (try expr.kind()) {
                .flatten_to_leaf => .flatten_to_leaf,
                .flatten_to_inner => .flatten_to_inner,
                .points => .points,
                .size => .size,
                .crash => .crash,
                else => unreachable,
            });
        },
        .load => |load| {
            try compile_expr(ally, root, load.object, stack, instrs);
            try stack.append(ally, "");
            try compile_expr(ally, root, load.index, stack, instrs);
            try instrs.append(ally, .load);
            _ = stack.pop();
        },
        .gc => |inner| {
            try instrs.append(ally, .heap_size);
            try stack.append(ally, "");
            try compile_expr(ally, root, inner, stack, instrs);
            try instrs.append(ally, .gc);
            _ = stack.pop();
        },
        .call => |call| {
            for (call.args) |arg| {
                try compile_expr(ally, root, arg, stack, instrs);
                try stack.append(ally, "");
            }
            try compile_expr(ally, root, call.fun, stack, instrs);
            try instrs.append(ally, .call);
            for (call.args) |_| _ = stack.pop();
        },
        .rec => |args| {
            for (args) |arg| {
                try compile_expr(ally, root, arg, stack, instrs);
                try stack.append(ally, "");
            }
            try instrs.append(ally, .{ .address = root.obj });
            try instrs.append(ally, .call);
            for (args) |_| _ = stack.pop();
        },
    }
}

pub fn run_fun(vm: *Vm, fun: CompiledFun) !void {
    var ip: usize = 0;
    while (ip < fun.instructions.len) {
        vm.instruction_count += 1;
        const instruction = fun.instructions[ip];
        ip += 1;
        // std.debug.print("Running {f} ", .{instruction});
        // std.debug.print("stack:  ", .{});
        // for (vm.data_stack.memory[0..vm.data_stack.used]) |word| std.debug.print(" {}", .{word});
        // std.debug.print("\n", .{});
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
                std.debug.print("Adding {d} and {d}\n", .{ a, b });
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
            .shift_left => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                // std.debug.print("{} << {} is {}\n", .{ a, b, a >> @intCast(b) });
                try vm.data_stack.push(@bitCast(a << @intCast(b)));
            },
            .shift_right => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                // std.debug.print("{} >> {} is {}\n", .{ a, b, a >> @intCast(b) });
                try vm.data_stack.push(@bitCast(a >> @intCast(b)));
            },
            .compare => {
                const b: i64 = @bitCast(vm.data_stack.pop());
                const a: i64 = @bitCast(vm.data_stack.pop());
                const result: Word = if (a == b) 1 else if (a > b) 2 else 4;
                try vm.data_stack.push(result);
            },
            .and_ => {
                const b = vm.data_stack.pop();
                const a = vm.data_stack.pop();
                try vm.data_stack.push(a & b);
            },
            .or_ => {
                const b = vm.data_stack.pop();
                const a = vm.data_stack.pop();
                try vm.data_stack.push(a | b);
            },
            .xor => {
                const b = vm.data_stack.pop();
                const a = vm.data_stack.pop();
                try vm.data_stack.push(a ^ b);
            },
            .jump_if => |target| {
                const condition = vm.data_stack.pop();
                if (condition != 0) ip = target;
            },
            .jump => |target| ip = target,
            .new_leaf => |size| {
                const stack = vm.data_stack.memory[0..vm.data_stack.used];
                const words = stack[stack.len - size ..];
                const obj = try vm.heap.new_leaf(@ptrCast(words));
                vm.data_stack.pop_n(size);
                try vm.data_stack.push(obj.address);
            },
            .new_inner => |size| {
                const stack = vm.data_stack.memory[0..vm.data_stack.used];
                const words = stack[stack.len - size ..];
                const obj = try vm.heap.new_inner(@ptrCast(words));
                vm.data_stack.pop_n(size);
                try vm.data_stack.push(obj.address);
            },
            .flatten_to_inner => {
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
            .flatten_to_leaf => {
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
            .heap_size => {
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
            .call => try vm.run_fun(try vm.compile(Ir.Fun{ .obj = Obj{ .address = vm.data_stack.pop() } })),
            .crash => {
                const message = vm.data_stack.pop();
                vm.heap.dump_obj(Obj{ .address = message });
                std.process.exit(1);
            },
        }
    }
}

pub fn deduplicate(vm: *Vm, checkpoint: Heap.Checkpoint, obj: Obj) !Obj {
    var map = try vm.heap.deduplicate(vm.ally, checkpoint);
    vm.compiled_cache.remove_everything_after(checkpoint.address);
    const mapped = map.get(obj) orelse obj;
    map.deinit();
    return mapped;
}

pub fn garbage_collect(vm: *Vm, checkpoint: Heap.Checkpoint, keep: Obj) !Obj {
    const mapped = vm.heap.garbage_collect(vm.ally, checkpoint, keep);
    vm.compiled_cache.remove_everything_after(checkpoint.address);
    return mapped;
}
