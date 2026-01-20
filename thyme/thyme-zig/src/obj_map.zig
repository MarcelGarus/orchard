const std = @import("std");
const ArrayList = std.ArrayList;
const Heap = @import("heap.zig");
const Obj = Heap.Obj;
const Word = Heap.Word;
const Ally = std.mem.Allocator;

// A map that maps objects to something else.
pub fn ObjMap(T: type) type {
  return struct {
    entries: ArrayList(Entry),
    const Entry = struct { key: Obj, value: T };

    const Self = @This();

    pub const empty = Self{ .entries = ArrayList(Entry).empty };

    pub fn put(self: *Self, ally: Ally, key: Obj, value: T) !void {
      try self.entries.append(ally, .{ .key = key, .value = value });
    }

    pub fn get(self: Self, key: Obj) ?T {
      for (self.entries.items) |entry| if (entry.key == key) return entry.value;
      return null;
    }

    pub fn remove_everything_after(self: *Self, address: Word) void {
      var i: usize = 0;
      while (i < self.entries.items.len) {
          if (self.entries.items[i].key.address >= address) {
            _ = self.entries.swapRemove(i);
          } else {
              i += 1;
          }
      }
    }
  };
}
