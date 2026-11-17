//! Host-owned notebook document and its fixed Yam ABI.
const std = @import("std");
const Heap = @import("heap.zig");
const Obj = Heap.Obj;
const Allocator = std.mem.Allocator;

pub const Chapter = struct { title: []const u8, children: []const Cell };
pub const Cell = union(enum) { text: []const u8, code: []const u8, chapter: Chapter, eval: []const u8 };
pub const Notebook = struct { root: Chapter };
const max_depth = 512;
const Error = Allocator.Error || error{ InvalidNotebook, InvalidTitle, InvalidNotebookObject, NotebookTooDeep };

const Line = struct {
    text: []const u8,
    indentation: usize,
    newline: bool,
    fn blank(self: Line) bool {
        return self.indentation == self.text.len;
    }
};
const Parser = struct {
    allocator: Allocator,
    lines: []const Line,
    at: usize = 0,
    fn skipBlank(self: *Parser) void {
        while (self.at < self.lines.len and self.lines[self.at].blank()) self.at += 1;
    }
    fn chapter(self: *Parser, depth: usize) Error!Chapter {
        if (depth > max_depth) return error.NotebookTooDeep;
        const line = self.lines[self.at];
        const header = line.text[line.indentation..];
        const title = if (std.mem.eql(u8, header, "chapter")) "" else if (std.mem.startsWith(u8, header, "chapter ")) header[8..] else return error.InvalidNotebook;
        try validateTitle(title);
        self.at += 1;
        self.skipBlank();
        var children: std.ArrayList(Cell) = .empty;
        const child_indent = if (self.at < self.lines.len) self.lines[self.at].indentation else 0;
        while (self.at < self.lines.len) {
            const child_line = self.lines[self.at];
            if (child_line.indentation <= line.indentation) break;
            if (child_line.indentation != child_indent) return error.InvalidNotebook;
            const child_header = child_line.text[child_indent..];
            if (std.mem.eql(u8, child_header, "chapter") or std.mem.startsWith(u8, child_header, "chapter ")) {
                try children.append(self.allocator, .{ .chapter = try self.chapter(depth + 1) });
            } else {
                const is_text = std.mem.eql(u8, std.mem.trimEnd(u8, child_header, " \t"), "text");
                const is_eval = std.mem.eql(u8, std.mem.trimEnd(u8, child_header, " \t"), "eval");
                if (!is_text and !is_eval and !std.mem.eql(u8, std.mem.trimEnd(u8, child_header, " \t"), "code")) return error.InvalidNotebook;
                self.at += 1;
                const start = self.at;
                var indentation: usize = std.math.maxInt(usize);
                while (self.at < self.lines.len) : (self.at += 1) {
                    const body_line = self.lines[self.at];
                    if (!body_line.blank()) {
                        if (body_line.indentation <= child_indent) break;
                        indentation = @min(indentation, body_line.indentation);
                    }
                }
                var body: std.ArrayList(u8) = .empty;
                for (self.lines[start..self.at]) |body_line| {
                    try body.appendSlice(self.allocator, body_line.text[@min(indentation, body_line.indentation)..]);
                    if (body_line.newline) try body.append(self.allocator, '\n');
                }
                if (body.items.len > 0 and body.items[body.items.len - 1] == '\n') body.items.len -= 1;
                if (is_text) for (body.items) |*byte| {
                    if (byte.* == '\n') byte.* = ' ';
                };
                try children.append(self.allocator, if (is_text) .{ .text = body.items } else if (is_eval) .{ .eval = body.items } else .{ .code = body.items });
            }
            self.skipBlank();
        }
        return .{ .title = title, .children = children.items };
    }
};

/// Returned strings/slices belong to allocator, or borrow source. An arena fits
/// the document lifetime and also reclaims partial allocations on parse errors.
pub fn parse(allocator: Allocator, source: []const u8) Error!Notebook {
    if (std.mem.indexOfScalar(u8, source, 0) != null) return error.InvalidNotebook;
    var lines: std.ArrayList(Line) = .empty;
    defer lines.deinit(allocator);
    var at: usize = 0;
    while (at < source.len) {
        const start = at;
        while (at < source.len and source[at] != '\n') at += 1;
        const newline = at < source.len;
        var end = at;
        if (newline) at += 1;
        if (end > start and source[end - 1] == '\r') end -= 1;
        const content = source[start..end];
        var indentation: usize = 0;
        while (indentation < content.len and content[indentation] == ' ') indentation += 1;
        if (indentation < content.len and content[indentation] == '\t') return error.InvalidNotebook;
        try lines.append(allocator, .{ .text = content, .indentation = indentation, .newline = newline });
    }
    var parser = Parser{ .allocator = allocator, .lines = lines.items };
    parser.skipBlank();
    if (parser.at == lines.items.len or lines.items[parser.at].indentation != 0) return error.InvalidNotebook;
    const root = try parser.chapter(0);
    parser.skipBlank();
    if (parser.at != lines.items.len) return error.InvalidNotebook;
    return .{ .root = root };
}

fn validateTitle(title: []const u8) Error!void {
    if (std.mem.indexOfAny(u8, title, "\r\n\x00") != null) return error.InvalidTitle;
}
fn indent(out: *std.ArrayList(u8), allocator: Allocator, depth: usize) Error!void {
    try out.appendNTimes(allocator, ' ', depth * 2);
}
fn writeTextLine(out: *std.ArrayList(u8), allocator: Allocator, text: []const u8, depth: usize) Error!void {
    const width = @max(1, 80 -| (depth * 2));
    var remaining = text;
    while (remaining.len > width) {
        // Replace one space with a line break: parsing rejoins it as a space.
        // Leave unbroken words intact rather than changing their contents.
        const limit = @min(remaining.len, width + 1);
        const at = std.mem.lastIndexOfScalar(u8, remaining[0..limit], ' ') orelse
            if (std.mem.indexOfScalar(u8, remaining[limit..], ' ')) |next| limit + next else break;
        if (at == 0) break;
        try indent(out, allocator, depth);
        try out.appendSlice(allocator, remaining[0..at]);
        try out.append(allocator, '\n');
        remaining = remaining[at + 1 ..];
    }
    if (remaining.len != 0) try indent(out, allocator, depth);
    try out.appendSlice(allocator, remaining);
    try out.append(allocator, '\n');
}
fn serializeChapter(out: *std.ArrayList(u8), allocator: Allocator, chapter: Chapter, depth: usize) Error!void {
    if (depth > max_depth) return error.NotebookTooDeep;
    try validateTitle(chapter.title);
    try indent(out, allocator, depth);
    try out.appendSlice(allocator, "chapter ");
    try out.appendSlice(allocator, chapter.title);
    try out.append(allocator, '\n');
    for (chapter.children) |cell| switch (cell) {
        .chapter => |child| try serializeChapter(out, allocator, child, depth + 1),
        .text, .code, .eval => |content| {
            if (std.mem.indexOfScalar(u8, content, 0) != null) return error.InvalidNotebook;
            try indent(out, allocator, depth + 1);
            try out.appendSlice(allocator, if (cell == .text) "text\n" else if (cell == .eval) "eval\n" else "code\n");
            if (content.len != 0) {
                var lines = std.mem.splitScalar(u8, content, '\n');
                while (lines.next()) |line| {
                    if (cell == .text) {
                        try writeTextLine(out, allocator, line, depth + 2);
                    } else {
                        if (line.len != 0) try indent(out, allocator, depth + 2);
                        try out.appendSlice(allocator, line);
                        try out.append(allocator, '\n');
                    }
                }
            }
        },
    };
}
pub fn serialize(allocator: Allocator, notebook: Notebook) Error![]u8 {
    var output: std.ArrayList(u8) = .empty;
    errdefer output.deinit(allocator);
    try serializeChapter(&output, allocator, notebook.root, 0);
    return output.toOwnedSlice(allocator);
}

/// Chapter = [title, children]; Cell = [tag, payload], tags text/code/chapter/eval = 0/1/2/3.
pub fn encode(allocator: Allocator, heap: *Heap, chapter: Chapter) Error!Obj {
    return encodeChapter(allocator, heap, chapter, 0);
}
fn encodeChapter(allocator: Allocator, heap: *Heap, chapter: Chapter, depth: usize) Error!Obj {
    if (depth > max_depth) return error.NotebookTooDeep;
    try validateTitle(chapter.title);
    // Build children first: the collector relies on references pointing backward.
    var children: std.ArrayList(Obj) = .empty;
    defer children.deinit(allocator);
    for (chapter.children) |cell| {
        const tag: usize = switch (cell) {
            .text => 0,
            .code => 1,
            .chapter => 2,
            .eval => 3,
        };
        const payload = switch (cell) {
            .text, .code, .eval => |content| try heap.new_symbol(content),
            .chapter => |child| try encodeChapter(allocator, heap, child, depth + 1),
        };
        try children.append(allocator, try heap.new_inner(&.{ try heap.new_leaf(&.{tag}), payload }));
    }
    return heap.new_inner(&.{ try heap.new_symbol(chapter.title), try heap.new_inner(children.items) });
}
fn inner(obj: Obj, length: ?usize) Error!void {
    if (!obj.is_inner() or (length != null and obj.size() != length.?)) return error.InvalidNotebookObject;
}
fn string(allocator: Allocator, obj: Obj) Error![]const u8 {
    if (!obj.is_leaf()) return error.InvalidNotebookObject;
    const value = Heap.get_symbol(obj);
    if (std.mem.indexOfScalar(u8, value, 0) != null) return error.InvalidNotebookObject;
    return allocator.dupe(u8, value);
}
pub fn decode(allocator: Allocator, obj: Obj) Error!Notebook {
    return .{ .root = try decodeChapter(allocator, obj, 0) };
}
fn decodeChapter(allocator: Allocator, obj: Obj, depth: usize) Error!Chapter {
    if (depth > max_depth) return error.NotebookTooDeep;
    try inner(obj, 2);
    const title = try string(allocator, obj.child(0));
    try validateTitle(title);
    const list = obj.child(1);
    try inner(list, null);
    const children = try allocator.alloc(Cell, list.size());
    for (list.children(), children) |value, *child| {
        try inner(value, 2);
        const tag = value.child(0);
        if (!tag.is_leaf() or tag.size() != 1) return error.InvalidNotebookObject;
        child.* = switch (tag.word(0)) {
            0 => .{ .text = try string(allocator, value.child(1)) },
            1 => .{ .code = try string(allocator, value.child(1)) },
            2 => .{ .chapter = try decodeChapter(allocator, value.child(1), depth + 1) },
            3 => .{ .eval = try string(allocator, value.child(1)) },
            else => return error.InvalidNotebookObject,
        };
    }
    return .{ .title = title, .children = children };
}

pub fn save(dir: std.Io.Dir, io: std.Io, path: []const u8, allocator: Allocator, notebook: Notebook) !void {
    const source = try serialize(allocator, notebook);
    defer allocator.free(source);
    var file = try dir.createFileAtomic(io, path, .{ .replace = true });
    defer file.deinit(io);
    var buffer: [4096]u8 = undefined;
    var writer = file.file.writer(io, &buffer);
    try writer.interface.writeAll(source);
    try writer.interface.flush();
    try file.file.sync(io);
    try file.replace(io);
}
