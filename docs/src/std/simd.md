# simd

`std.simd` works on many numbers at once: a vector is an `Array` as wide as
the processor's vector registers, and each function does the same thing to
every lane in one instruction.

```zeru
import std.simd;

fn main() {
    const a: Array<i32, simd::LANES_32> = [3; simd::LANES_32];
    const b: Array<i32, simd::LANES_32> = [4; simd::LANES_32];
    const c = simd::add(a, b);
    println("{} {}", c[0], simd::sum(c) == 7 * simd::LANES_32 as i32); // 7 true
}
```

## How wide a vector is

`simd::BYTES` is the size of a vector: 16 bytes in the default build, which
runs on every x86-64 processor, and with `--native` 32 on a processor with
AVX2 or 64 with AVX-512. The lanes of a vector follow from the size of its
numbers:

| Constant | Lanes of | 16 bytes | 64 bytes |
|---|---|---|---|
| `simd::LANES_8` | `i8`, `u8` | 16 | 64 |
| `simd::LANES_16` | `i16`, `u16` | 8 | 32 |
| `simd::LANES_32` | `i32`, `u32`, `f32` | 4 | 16 |
| `simd::LANES_64` | `i64`, `u64`, `usize`, `f64` | 2 | 8 |

Written with these constants, the same code uses the widest vectors of
whatever it is built for. `[x; simd::LANES_16]` is a vector of one value.

## The functions

| Function | Gives |
|---|---|
| `load(values[i..])` | the vector of the first lanes of a slice |
| `store(&var out[i..], v)` | writes `v` over the first elements of a slice |
| `add`, `sub`, `mul` | the sums, differences and products, lane by lane |
| `min`, `max` | the smaller or larger of each pair of lanes |
| `and`, `or`, `xor` | bit operations, for integer lanes |
| `shl(v, n)`, `shr(v, n)` | each integer lane shifted by `n` |
| `eq`, `lt`, `gt` | a mask: integer lanes all ones where it holds, zero elsewhere |
| `select(mask, a, b)` | the lane of `a` where the mask is not zero, else that of `b` |
| `sum(v)` | the sum of the lanes |
| `madd(a, b)` | from two vectors of `i16`, the `i32` lanes `a[2i] * b[2i] + a[2i + 1] * b[2i + 1]` |

Integer arithmetic wraps around, as `+%` does: there is no overflow check
on a lane. `load` and `store` check that the slice holds a whole vector,
outside `--release-fast`.

A loop over a long slice takes a vector at a time, and the elements left
over one by one:

```zeru
import std.simd;

fn total(values: &[i32]) i32 {
    var sums: Array<i32, simd::LANES_32> = [0; simd::LANES_32];
    var at: usize = 0;
    while at + simd::LANES_32 <= values.len() {
        sums = simd::add(sums, simd::load(values[at..]));
        at += simd::LANES_32;
    }
    var sum = simd::sum(sums);
    while at < values.len() {
        sum += values[at];
        at += 1;
    }
    return sum;
}

fn main() {
    var values: Vec<i32> = Vec.new();
    for i in 0..1000 {
        values.push(i);
    }
    println("{}", total(&values)); // 499500
}
```

Each step of that loop waits for the one before it to finish its `add`. On
the hottest loops, two to four sums kept apart and added at the end let the
processor run them side by side.
