# collections

`Vec` is built in; the other collections are imported one by one:

```zeru,ignore
import std.collections.hashmap::{HashMap};
import std.collections.hashset::{HashSet};
import std.collections.vecdeque::{VecDeque};
import std.collections.binary_heap::{BinaryHeap};
```

| Type | Holds | Main methods |
|---|---|---|
| `HashMap<K, V>` | a value for each key | `insert`, `get`, `contains`, `remove` |
| `HashSet<K>` | keys, each once | `insert`, `contains`, `remove` |
| `VecDeque<T>` | a queue open at both ends | `push_back`, `push_front`, `pop_front`, `pop_back`, `front`, `back`, `at` |
| `BinaryHeap<T>` | values, largest out first | `push`, `pop`, `peek` |

All of them also have `new()`, `len()`, `is_empty()` and `clear()`.

## HashMap and HashSet

Keys must be integers, bools, enums without data, or tuples and structs of
those (the `Hash` bound).
`get` returns a reference to the value, or `None`:

```zeru
import std.collections.hashmap::{HashMap};

var stock: HashMap<i32, i32> = HashMap.new();
stock.insert(101, 5);
stock.insert(202, 0);
stock.insert(101, 7);
println("{} {}", stock.len(), *stock.get(101).unwrap()); // 2 7
println("{}", stock.get(303).is_none()); // true
println("{}", stock.remove(202)); // 0
println("{}", stock.contains(202)); // false
```

`remove` gives back the value it takes out, as a `V?`, so a value that owns
memory leaves the map whole:

```zeru
import std.collections.hashmap::{HashMap};

var names: HashMap<i32, String> = HashMap.new();
names.insert(1, String.from("Ada"));
match names.remove(1) {
    Some(name) => println("bye {}", name), // bye Ada
    None => println("nobody"),
}
```

```zeru
import std.collections.hashset::{HashSet};

var seen: HashSet<i64> = HashSet.new();
println("{}", seen.insert(4)); // true
println("{}", seen.insert(4)); // false
println("{}", seen.len()); // 1
```

`insert` on a set returns whether the key was new.

`for (key, value) in map` walks a map, and `for key in set` a set, in the
order the keys went in until a removal moves the last one into its gap.
With `&var map` the values can change, though the keys cannot.
`get_or_insert(key, value)` gives the value under `key`, storing `value`
there first when the key is new, which makes counting one line:

```zeru
import std.collections.hashmap::{HashMap};

var counts: HashMap<i32, i32> = HashMap.new();
for n in [4, 2, 4, 4] {
    *counts.get_or_insert(n, 0) += 1;
}
for (n, times) in counts {
    print("{}x{} ", n, times);
}
println(""); // 4x3 2x1
```

A tuple of such values is a key too, as the position on a grid:

```zeru
import std.collections.hashmap::{HashMap};

var grid: HashMap<(i32, i32), u8> = HashMap.new();
grid.insert((2, 3), '#');
println("{}", *grid.get((2, 3)).unwrap() == '#'); // true
```

## VecDeque

A queue: add at one end, take from the other.

```zeru
import std.collections.vecdeque::{VecDeque};

var queue: VecDeque<str> = VecDeque.new();
queue.push_back("first");
queue.push_back("second");
queue.push_front("urgent");
println("{}", queue.pop_front().unwrap()); // urgent
println("{}", queue.len()); // 2
```

## BinaryHeap

Gives back the largest value first, whatever the order they went in.
Tuples order by their first part, so `(priority, value)` pairs come out by
priority; pushing the priority negated gives the smallest first, as a
shortest-path search wants:

```zeru
import std.collections.binary_heap::{BinaryHeap};

var heap: BinaryHeap<i32> = BinaryHeap.new();
heap.push(3);
heap.push(9);
heap.push(5);
println("{} {}", heap.pop().unwrap(), heap.pop().unwrap()); // 9 5

var queue: BinaryHeap<(i64, str)> = BinaryHeap.new();
queue.push((-7, "far"));
queue.push((-2, "near"));
var (distance, place) = queue.pop().unwrap();
println("{} {}", -distance, place); // 2 near
```
