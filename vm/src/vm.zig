// A virtual machine can interpret heap objects with a particular structure as
// executable expressions (see bootstrap.objects for a specification). There are
// multiple implementations with different tradeoffs:
//
// - vm_tree_walking.zig: Every time a function is executed, this VM traverses
//   the expression tree and interprets the nodes. Slow, but catches all
//   undefined behavior.
// - vm_byte_code.zig: When a function is executed, compiles it into byte code
//   (cached) and runs it. This is faster than the tree-walking interpreter and
//   exploits undefined behavior for optimizations.
// - vm_x86_64.zig: Work in progress! A JIT compiler. Even faster than the
//   byte code VM, but only works on x86_64 CPUs.
//
// Every implementation provides the following fields/functions:
// - init(heap: *Heap, ally: Ally) !Impl
// - heap: *Heap
// - run(impl: *Impl, fun: Fun, args: []const Obj, fuel: *usize) !Result
// - garbage_collect(impl: *Impl, checkpoint: Checkpoint, keep: Obj) !Obj
// - deduplicate(impl: *Impl, checkpoint: Checkpoint, obj: Obj) !Obj

const std = @import("std");
const Ally = std.mem.Allocator;
const Writer = std.Io.Writer;
const builtin = @import("builtin");

const Heap = @import("heap.zig");
const Word = Heap.Word;
const Obj = Heap.Obj;
const get_symbol = Heap.get_symbol;

pub const TreeWalkingInterpreter = @import("vm_tree_walking.zig");
pub const ByteCodeInterpreter = @import("vm_byte_code.zig");
pub const Default = switch (builtin.cpu.arch) {
    // .x86_64 => Vm(@import("vm_x86_64.zig")), // a JIT compiler
    else => ByteCodeInterpreter,
};

pub const max_fuel = std.math.maxInt(usize);
pub const Result = union(enum) {
    returned: Obj,
    crashed: Obj,
    out_of_fuel,
    out_of_memory,
};

pub const Fun = struct {
    obj: Obj,

    pub fn args(fun: Fun) []const Obj {
        return fun.obj.child(0).children();
    }
    pub fn body(fun: Fun) Expr {
        return Expr{ .obj = fun.obj.child(1) };
    }

    pub fn format(fun: Fun, writer: *Writer) !void {
        try writer.print("fun\n", .{});
        try writer.print("{f}", .{fun.obj});
        // for (fun.args()) |arg| try writer.print(" {s}", .{get_symbol(arg)});
        // try writer.print("\n", .{});
        // (Expr{ .obj = fun.obj.child(1) }).format_indented(writer, 1) catch return error.WriteFailed;
    }
};

const Name = []const u8;

pub const Expr = struct {
    obj: Obj,

    // See bootstrap.objects for an explanation of the expressions.
    pub const Kind = union(enum) {
        word: Word,
        object: Obj,
        name: Name,
        let: Let,
        also: Also,
        add: LeftRight,
        subtract: LeftRight,
        multiply: LeftRight,
        divide: LeftRight,
        modulo: LeftRight,
        shift_left: LeftRight,
        shift_right: LeftRight,
        and_: LeftRight,
        or_: LeftRight,
        xor: LeftRight,
        compare: LeftRight,
        f_add: LeftRight,
        f_subtract: LeftRight,
        f_multiply: LeftRight,
        f_divide: LeftRight,
        f_compare: LeftRight,
        int_to_float: Expr,
        float_to_int: Expr,
        f_is_finite: Expr,
        if_: If,
        new_leaf: []const Expr,
        new_inner: []const Expr,
        flatten_to_leaf: Expr,
        flatten_to_inner: Expr,
        points: Expr,
        size: Expr,
        load: Load,
        call: Call,
        call_indirect: CallIndirect,
        rec: []const Expr,
        collect_garbage: Expr,
        sandbox: Sandbox,
        use_fuel: UseFuel,
        crash: Expr,
        unreachable_,

        pub const Let = struct { name: Name, def: Expr, body: Expr };
        pub const LeftRight = struct { left: Expr, right: Expr };
        pub const If = struct { condition: Expr, then: Expr, else_: Expr };
        pub const Load = struct { object: Expr, index: Expr };
        pub const Also = struct { ignored: Expr, value: Expr };
        pub const Call = struct { fun: Expr, args: []const Expr };
        pub const CallIndirect = struct { fun: Expr, args: Expr };
        pub const Sandbox = struct { fuel: Expr, body: Expr };
        pub const UseFuel = struct { amount: Expr, child: Expr };
    };

    pub fn kind(self: Expr) error{UndefinedBehavior}!Kind {
        // Validate the surface shape before reading any field.
        if (!self.obj.is_inner()) return error.UndefinedBehavior;
        if (self.obj.size() == 0) return error.UndefinedBehavior;
        const tag_obj = self.obj.child(0);
        if (!tag_obj.is_leaf()) return error.UndefinedBehavior;
        const tag = get_symbol(tag_obj);

        inline for (@typeInfo(Kind).@"union".fields) |field| {
            const expected_tag = comptime kebab_case: {
                var buf: [field.name.len]u8 = undefined;
                var len: usize = 0;
                for (field.name, 0..) |c, i| {
                    if (c == '_' and i == field.name.len - 1) continue;
                    buf[len] = if (c == '_') '-' else c;
                    len += 1;
                }
                break :kebab_case buf[0..len] ++ "";
            };
            if (std.mem.eql(u8, tag, expected_tag)) {
                // Each variant has a fixed arity. Validate size before reading
                // any child slot so we never trigger Heap.child OOB panics.
                const arity: usize = comptime switch (field.type) {
                    void => 0,
                    Word, Obj, Name, Expr, []const Expr => 1,
                    Kind.Also, Kind.LeftRight, Kind.Load, Kind.Call, Kind.CallIndirect, Kind.Sandbox, Kind.UseFuel => 2,
                    Kind.Let, Kind.If => 3,
                    else => @compileError("Unsupported payload type: " ++ @typeName(field.type)),
                };
                if (self.obj.size() != arity + 1) return error.UndefinedBehavior;

                const payload: field.type = switch (field.type) {
                    void => {},
                    Word => blk: {
                        const c = self.obj.child(1);
                        if (!c.is_leaf() or c.size() != 1) return error.UndefinedBehavior;
                        break :blk c.word(0);
                    },
                    Obj => self.obj.child(1),
                    Name => blk: {
                        const c = self.obj.child(1);
                        if (!c.is_leaf()) return error.UndefinedBehavior;
                        break :blk get_symbol(c);
                    },
                    Expr => .{ .obj = self.obj.child(1) },
                    Kind.Let => blk: {
                        const name_obj = self.obj.child(1);
                        if (!name_obj.is_leaf()) return error.UndefinedBehavior;
                        break :blk .{ .name = get_symbol(name_obj), .def = .{ .obj = self.obj.child(2) }, .body = .{ .obj = self.obj.child(3) } };
                    },
                    Kind.Also => .{ .ignored = .{ .obj = self.obj.child(1) }, .value = .{ .obj = self.obj.child(2) } },
                    Kind.LeftRight => .{ .left = .{ .obj = self.obj.child(1) }, .right = .{ .obj = self.obj.child(2) } },
                    Kind.If => .{ .condition = .{ .obj = self.obj.child(1) }, .then = .{ .obj = self.obj.child(2) }, .else_ = .{ .obj = self.obj.child(3) } },
                    Kind.Load => .{ .object = .{ .obj = self.obj.child(1) }, .index = .{ .obj = self.obj.child(2) } },
                    Kind.Call => blk: {
                        const args_obj = self.obj.child(2);
                        if (!args_obj.is_inner()) return error.UndefinedBehavior;
                        break :blk .{ .fun = .{ .obj = self.obj.child(1) }, .args = @ptrCast(args_obj.children()) };
                    },
                    Kind.CallIndirect => .{ .fun = .{ .obj = self.obj.child(1) }, .args = .{ .obj = self.obj.child(2) } },
                    Kind.Sandbox => .{ .fuel = .{ .obj = self.obj.child(1) }, .body = .{ .obj = self.obj.child(2) } },
                    Kind.UseFuel => .{ .amount = .{ .obj = self.obj.child(1) }, .child = .{ .obj = self.obj.child(2) } },
                    []const Expr => blk: {
                        const c = self.obj.child(1);
                        if (!c.is_inner()) return error.UndefinedBehavior;
                        break :blk @ptrCast(c.children());
                    },
                    else => unreachable,
                };
                return @unionInit(Kind, field.name, payload);
            }
        }

        return error.UndefinedBehavior;
    }

    pub fn format(expr: Expr, writer: *Writer) !void {
        expr.format_indented(writer, 0) catch return error.WriteFailed;
    }
    pub fn format_indented(expr: Expr, writer: *Writer, indentation: usize) !void {
        for (0..indentation) |_| try writer.print("  ", .{});
        const k = expr.kind() catch {
            try writer.print("<malformed-expr {f}>\n", .{expr.obj});
            return;
        };
        switch (k) {
            .word => |word| try writer.print("word {}\n", .{word}),
            .object => |obj| try writer.print("object {x}\n", .{obj.address}),
            .name => |name| try writer.print("name {s}\n", .{name}),
            .let => |let| {
                try writer.print("let {s}\n", .{let.name});
                try let.def.format_indented(writer, indentation + 1);
                try let.body.format_indented(writer, indentation + 1);
            },
            .add => |args| {
                try writer.print("add\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .subtract => |args| {
                try writer.print("subtract\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .multiply => |args| {
                try writer.print("multiply\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .divide => |args| {
                try writer.print("divide\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .modulo => |args| {
                try writer.print("modulo\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .shift_left => |args| {
                try writer.print("shift left\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .shift_right => |args| {
                try writer.print("shift right\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .and_ => |args| {
                try writer.print("and\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .or_ => |args| {
                try writer.print("or\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .xor => |args| {
                try writer.print("xor\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .compare => |args| {
                try writer.print("compare\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .f_add => |args| {
                try writer.print("f-add\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .f_subtract => |args| {
                try writer.print("f-subtract\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .f_multiply => |args| {
                try writer.print("f-multiply\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .f_divide => |args| {
                try writer.print("f-divide\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .f_compare => |args| {
                try writer.print("f-compare\n", .{});
                try args.left.format_indented(writer, indentation + 1);
                try args.right.format_indented(writer, indentation + 1);
            },
            .int_to_float => |arg| {
                try writer.print("int-to-float\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .float_to_int => |arg| {
                try writer.print("float-to-int\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .f_is_finite => |arg| {
                try writer.print("f-is-finite\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .if_ => |if_| {
                try writer.print("if\n", .{});
                try if_.condition.format_indented(writer, indentation + 1);
                try if_.then.format_indented(writer, indentation + 1);
                try if_.else_.format_indented(writer, indentation + 1);
            },
            .new_leaf => |children| {
                try writer.print("new leaf\n", .{});
                for (children) |child|
                    try child.format_indented(writer, indentation + 1);
            },
            .new_inner => |children| {
                try writer.print("new inner\n", .{});
                for (children) |child|
                    try child.format_indented(writer, indentation + 1);
            },
            .flatten_to_leaf => |arg| {
                try writer.print("flatten to leaf\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .flatten_to_inner => |arg| {
                try writer.print("flatten to inner\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .points => |arg| {
                try writer.print("points\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .size => |arg| {
                try writer.print("size\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .load => |load| {
                try writer.print("load\n", .{});
                try load.object.format_indented(writer, indentation + 1);
                try load.index.format_indented(writer, indentation + 1);
            },
            .collect_garbage => |arg| {
                try writer.print("gc\n", .{});
                try arg.format_indented(writer, indentation + 1);
            },
            .also => |also| {
                try writer.print("also\n", .{});
                try also.ignored.format_indented(writer, indentation + 1);
                try also.value.format_indented(writer, indentation + 1);
            },
            .call => |call| {
                try writer.print("call\n", .{});
                try call.fun.format_indented(writer, indentation + 1);
                for (call.args) |arg|
                    try arg.format_indented(writer, indentation + 1);
            },
            .call_indirect => |ci| {
                try writer.print("call-indirect\n", .{});
                try ci.fun.format_indented(writer, indentation + 1);
                try ci.args.format_indented(writer, indentation + 1);
            },
            .rec => |args| {
                try writer.print("rec\n", .{});
                for (args) |arg|
                    try arg.format_indented(writer, indentation + 1);
            },
            .sandbox => |sandbox| {
                try writer.print("sandbox\n", .{});
                try sandbox.fuel.format_indented(writer, indentation + 1);
                try sandbox.body.format_indented(writer, indentation + 1);
            },
            .use_fuel => |use_fuel| {
                try writer.print("use-fuel\n", .{});
                try use_fuel.amount.format_indented(writer, indentation + 1);
                try use_fuel.child.format_indented(writer, indentation + 1);
            },
            .crash => |error_| {
                try writer.print("crash\n", .{});
                try error_.format_indented(writer, indentation + 1);
            },
            .unreachable_ => try writer.print("unreachable\n", .{}),
        }
    }
};
