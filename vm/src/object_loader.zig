const std = @import("std");
const Ally = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringMap = std.StringHashMapUnmanaged;

const Heap = @import("heap.zig");
const Obj = Heap.Obj;

const Str = []const u8;

pub fn load(ally: Ally, heap: *Heap, input: Str) !Obj {
    var cursor: usize = 0;
    var defs = StringMap(Obj).empty;
    parse_defs(input, &cursor, heap, ally, &defs) catch |e| {
        std.debug.print("Error at {}: {any}.\n", .{ cursor, e });
        return e;
    };
    return defs.get("export").?;
}
fn parse_defs(input: Str, cursor: *usize, heap: *Heap, ally: Ally, defs: *StringMap(Obj)) !void {
    while (true) {
        trim_whitespace(input, cursor);
        if (cursor.* == input.len) break;
        const name = parse_name(input, cursor) orelse return error.ExpectedName;
        const expr = try parse_obj(input, cursor, heap, ally, defs.*) orelse
            return error.ExpectedExpression;
        try defs.put(ally, name, expr);
    }
}
fn trim_whitespace(input: Str, cursor: *usize) void {
    while (cursor.* < input.len) : (cursor.* += 1) {
        if (input[cursor.*] == ' ') continue;
        if (input[cursor.*] == '\n') continue;
        break;
    }
}
fn parse_name(input: Str, cursor: *usize) ?Str {
    const start = cursor.*;
    while (cursor.* < input.len) : (cursor.* += 1) {
        const c = input[cursor.*];
        if (c == ' ' or c == '\n' or c == '(' or c == ')') break;
    }
    const end = cursor.*;
    if (start == end) return null;
    return input[start..end];
}
fn parse_obj(input: Str, cursor: *usize, heap: *Heap, ally: Ally, defs: StringMap(Obj)) error{
    OutOfMemory,
    UnendingString,
    ExpectedEscapedChar,
    UnknownEscapeChar,
    ExpectedClosingParen,
    NameNotInScope,
}!?Obj {
    trim_whitespace(input, cursor);
    if (cursor.* == input.len) return null;
    switch (input[cursor.*]) {
        '0'...'9' => {
            var num: i64 = 0;
            while (cursor.* < input.len) : (cursor.* += 1) {
                const digit = std.mem.indexOfScalar(u8, "0123456789", input[cursor.*]) orelse break;
                num = num * 10 + @as(i64, @intCast(digit));
            }
            return try heap.new_leaf(&.{@bitCast(num)});
        },
        '\"' => {
            cursor.* += 1;
            var chars = ArrayList(u8).empty;
            while (true) : (cursor.* += 1) {
                if (cursor.* == input.len) return error.UnendingString;
                switch (input[cursor.*]) {
                    '\"' => {
                        cursor.* += 1;
                        break;
                    },
                    '\\' => {
                        cursor.* += 1;
                        if (cursor.* == input.len) return error.ExpectedEscapedChar;
                        try chars.append(ally, switch (input[cursor.*]) {
                            '\\' => '\\',
                            '"' => '"',
                            'n' => '\n',
                            else => return error.UnknownEscapeChar,
                        });
                    },
                    else => |c| try chars.append(ally, c),
                }
            }
            return try Heap.new_symbol(heap, chars.items);
        },
        '(' => {
            cursor.* += 1;
            var children = ArrayList(Obj).empty;
            while (try parse_obj(input, cursor, heap, ally, defs)) |expr|
                try children.append(ally, expr);
            trim_whitespace(input, cursor);
            if (cursor.* == input.len) return error.ExpectedClosingParen;
            if (input[cursor.*] != ')') return error.ExpectedClosingParen;
            cursor.* += 1;
            return try heap.new_inner(children.items);
        },
        else => {
            if (parse_name(input, cursor)) |name| return defs.get(name) orelse {
                return error.NameNotInScope;
            };
            return null;
        },
    }
}
