const std = @import("std");
const Heap = @import("heap.zig");
const Obj = Heap.Obj;
const Word = Heap.Word;
const Ally = std.mem.Allocator;

// Keys are object addresses. Remove entries when those addresses become invalid.
pub fn ObjMap(T: type) type {
    return struct {
        entries: std.AutoArrayHashMapUnmanaged(Word, T),

        const Self = @This();

        pub const empty = Self{ .entries = .empty };

        pub fn deinit(self: *Self, ally: Ally) void {
            self.entries.deinit(ally);
        }

        pub fn put(self: *Self, ally: Ally, key: Obj, value: T) !void {
            _ = try self.entries.getOrPutValue(ally, key.address, value);
        }

        pub fn get(self: Self, key: Obj) ?T {
            return self.entries.get(key.address);
        }

        pub fn remove_everything_after(self: *Self, address: Word) void {
            var i: usize = 0;
            while (i < self.entries.count()) {
                if (self.entries.keys()[i] >= address) {
                    self.entries.swapRemoveAt(i);
                } else {
                    i += 1;
                }
            }
        }
    };
}
