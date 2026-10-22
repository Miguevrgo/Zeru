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

Keys must be integers, bools or enums without data (the `Hash` bound).
`get` returns a reference to the value, or `None`:

```zeru
import std.collections.hashmap::{HashMap};

var stock: HashMap<i32, i32> = HashMap.new();
stock.insert(101, 5);
stock.insert(202, 0);
stock.insert(101, 7);
println("{} {}", stock.len(), *stock.get(101).unwrap()); // 2 7
println("{}", stock.get(303).is_none()); // true
stock.remove(202);
println("{}", stock.contains(202)); // false
```

```zeru
import std.collections.hashset::{HashSet};

var seen: HashSet<i64> = HashSet.new();
println("{}", seen.insert(4)); // true
println("{}", seen.insert(4)); // false
println("{}", seen.len()); // 1
```

`insert` on a set returns whether the key was new.

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

Gives back the largest value first, whatever the order they went in:

```zeru
import std.collections.binary_heap::{BinaryHeap};

var heap: BinaryHeap<i32> = BinaryHeap.new();
heap.push(3);
heap.push(9);
heap.push(5);
println("{} {}", heap.pop().unwrap(), heap.pop().unwrap()); // 9 5
```
