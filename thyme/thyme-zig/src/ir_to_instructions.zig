const std = @import("std");
const ArrayList = std.ArrayList;
const Ally = std.mem.Allocator;

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Instruction = @import("instruction.zig").Instruction;
const Ir = @import("ir.zig");
const Id = Ir.Id;
const Object = @import("object.zig");

pub fn compile(heap: *Heap, ir: Ir) !Object {
    var stack = ArrayList(Id).empty;
    return try compile_body(heap, ir.body, ir, &stack);
}

fn compile_body(heap: *Heap, body: Ir.Body, ir: Ir, stack: *ArrayList(Id)) !Object {
    var builder = Builder.init(heap.ally, stack);
    for (body.ids) |id| {
        try compile_node(heap, id, ir, &builder);
    }
    try builder.push_from_stack(body.returns);
    try builder.pop_below_top(body.ids.len);

    return try Instruction.new_instructions(heap, builder.instructions.items);
}

fn compile_node(heap: *Heap, id: Id, ir: Ir, builder: *Builder) error{OutOfMemory}!void {
    switch (ir.get(id)) {
        .param => unreachable,
        .word => |word| try builder.push_word(word, id),
        .object => |object| try builder.push_address(object, id),
        .new => |new| {
            for (new.pointers) |ref| try builder.push_from_stack(ref);
            for (new.literals) |ref| try builder.push_from_stack(ref);
            try builder.new(new.tag, new.pointers.len, new.literals.len, id);
        },
        .tag => |obj| {
            try builder.push_from_stack(obj);
            try builder.tag(id);
        },
        .load => |load| {
            try builder.push_from_stack(load.base);
            try builder.push_from_stack(load.offset);
            try builder.load(id);
        },
        .add => |args| {
            try builder.push_from_stack(args.left);
            try builder.push_from_stack(args.right);
            try builder.add(id);
        },
        .subtract => |args| {
            try builder.push_from_stack(args.left);
            try builder.push_from_stack(args.right);
            try builder.subtract(id);
        },
        .multiply => |args| {
            try builder.push_from_stack(args.left);
            try builder.push_from_stack(args.right);
            try builder.multiply(id);
        },
        .divide => |args| {
            try builder.push_from_stack(args.left);
            try builder.push_from_stack(args.right);
            try builder.divide(id);
        },
        .modulo => |args| {
            try builder.push_from_stack(args.left);
            try builder.push_from_stack(args.right);
            try builder.modulo(id);
        },
        .compare => |args| {
            try builder.push_from_stack(args.left);
            try builder.push_from_stack(args.right);
            // try builder.compare(id);
            @panic("do we really need compare?");
        },
        .call => |call| {
            for (call.args) |arg| try builder.push_from_stack(arg);
            try builder.push_from_stack(call.fun);
            try builder.push_word(1, id);
            try builder.load(id);
            try builder.eval(call.args.len, id);
        },
        .if_not_zero => |if_| {
            try builder.push_from_stack(if_.condition);
            const condition = builder.stack.pop() orelse @panic("stack empty");
            const then = try compile_body(heap, if_.then, ir, builder.stack);
            _ = builder.stack.pop();
            const else_ = try compile_body(heap, if_.else_, ir, builder.stack);
            _ = builder.stack.pop();
            try builder.stack.append(builder.ally, condition);
            try builder.if_not_zero(then, else_, id);
        },
        .crash => |message| {
            try builder.push_from_stack(message);
            try builder.crash(id);
        },
    }
}

const Builder = struct {
    ally: Ally,
    instructions: ArrayList(Instruction),
    stack: *ArrayList(Id),

    pub fn init(ally: Ally, initial_stack: *ArrayList(Id)) Builder {
        return .{
            .ally = ally,
            .instructions = .empty,
            .stack = initial_stack,
        };
    }

    pub fn push_word(builder: *Builder, word: Word, id: Id) !void {
        try builder.instructions.append(builder.ally, .{ .push_word = word });
        try builder.stack.append(builder.ally, id);
    }
    pub fn push_address(builder: *Builder, object: Object, id: Id) !void {
        try builder.instructions.append(builder.ally, .{ .push_address = object });
        try builder.stack.append(builder.ally, id);
    }
    pub fn push_from_stack(builder: *Builder, id: Id) !void {
        const offset = for (0.., builder.stack.items) |i, stack_id| {
            if (stack_id.index == id.index)
                break builder.stack.items.len - 1 - i;
        } else {
            std.debug.print("stack: {any} id: {any}\n", .{ builder.stack, id });
            @panic("id not on stack");
        };
        try builder.instructions.append(builder.ally, .{ .push_from_stack = offset });
        try builder.stack.append(builder.ally, id);
    }
    pub fn pop(builder: *Builder, amount: usize) !void {
        try builder.instructions.append(builder.ally, .{ .pop = amount });
        builder.stack.items.len -= amount;
    }
    pub fn pop_below_top(builder: *Builder, amount: usize) !void {
        try builder.instructions.append(builder.ally, .{ .pop_below_top = amount });
        const top = builder.stack.pop() orelse @panic("stack empty");
        builder.stack.items.len -= amount;
        try builder.stack.append(builder.ally, top);
    }
    pub fn add(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .add);
        _ = builder.stack.pop();
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn subtract(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .subtract);
        _ = builder.stack.pop();
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn multiply(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .multiply);
        _ = builder.stack.pop();
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn divide(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .divide);
        _ = builder.stack.pop();
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn modulo(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .modulo);
        _ = builder.stack.pop();
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn shift_left(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .shift_left);
        _ = builder.stack.pop();
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn shift_right(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .shift_right);
        _ = builder.stack.pop();
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn if_not_zero(builder: *Builder, then: Object, else_: Object, id: Id) !void {
        try builder.instructions.append(builder.ally, .{ .if_not_zero = .{
            .then = then,
            .else_ = else_,
        } });
        _ = builder.stack.pop(); // condition
        try builder.stack.append(builder.ally, id);
    }
    pub fn new(builder: *Builder, tag_: u8, num_pointers_: usize, num_literals_: usize, id: Id) !void {
        try builder.instructions.append(builder.ally, .{ .new = .{
            .tag = tag_,
            .num_pointers = num_pointers_,
            .num_literals = num_literals_,
        } });
        builder.stack.items.len -= num_pointers_ + num_literals_;
        try builder.stack.append(builder.ally, id);
    }
    pub fn tag(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .tag);
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn num_pointers(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .num_pointers);
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn num_literals(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .num_literals);
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn load(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .load);
        _ = builder.stack.pop();
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
    pub fn eval(builder: *Builder, num_consumed: usize, id: Id) !void {
        try builder.instructions.append(builder.ally, .eval);
        _ = builder.stack.pop(); // list of instructions
        builder.stack.items.len -= num_consumed;
        try builder.stack.append(builder.ally, id);
    }
    pub fn crash(builder: *Builder, id: Id) !void {
        try builder.instructions.append(builder.ally, .crash);
        _ = builder.stack.pop();
        try builder.stack.append(builder.ally, id);
    }
};
