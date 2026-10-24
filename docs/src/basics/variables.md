# Variables and Types

## Variables

`var` declares a variable. Its type comes from the value, or is written
after a colon:

```zeru
var count = 3;
var price: f64 = 9.5;
count = count + 1;
println("{} {}", count, price); // 4 9.5
```

`const` declares a value that never changes. Assigning to it again is an
error. Constants can also live outside any function, where their type is
always written:

```zeru
const LIMIT: i32 = 10;

fn main() {
    const half = LIMIT / 2;
    println("{}", half); // 5
}
```

Declaring a variable again with the same name hides the old one from then
on, which is handy when a value changes type:

```zeru
var input = "42";
var input = input.parse_int().unwrap();
println("{}", input + 1); // 43
```

## Numbers

| Kind | Types |
|---|---|
| Signed integers | `i8`, `i16`, `i32`, `i64` |
| Unsigned integers | `u8`, `u16`, `u32`, `u64`, `usize` |
| Floating point | `f32`, `f64` |

`usize` is the type of lengths and indexes. A number written with nothing
around it to decide its type is an `i32`, or an `f64` if it has a point:

```zeru
var a = 7; // i32
var b = 2.5; // f64
var c: u8 = 255;
var d = 1_000_000; // underscores separate digits
var e = 0xff + 0b101 + 0o17; // hex, binary, octal
var f = 2.5e-3; // exponent
println("{} {}", d, e); // 1000000 275
```

Each number type knows its smallest and largest value, handy to start a
search for the lowest or highest:

```zeru
var lowest = i32.MAX;
for x in [7, -3, 12] {
    if x < lowest {
        lowest = x;
    }
}
println("{} {} {}", lowest, u8.MAX, i8.MIN); // -3 255 -128
```

For `f32` and `f64`, `MIN` is the most negative value.

### No silent conversions

Zeru never converts one number type into another on its own. Adding an
`i32` to an `i64`, or comparing a `u64` with an `i64`, is a compile error.
`as` converts explicitly:

```zeru
var small: i32 = 7;
var big: i64 = 1000;
println("{}", small as i64 + big); // 1007
println("{}", 3.9 as i32); // 3
println("{}", 300 as u8); // 44
```

A float becomes an integer by dropping its fraction. An integer converted to
a narrower one keeps its low bits, as `300 as u8` shows.

### Overflow

Arithmetic that does not fit its type stops the program:

```zeru,ignore
fn main() {
    var level: u8 = 255;
    level += 1;
}
```

```text
panic at main.zr:3:5: arithmetic overflow
```

When wrapping around is what you want, as in hashes and checksums, the
wrapping operators `+%`, `-%` and `*%` (and `+%=`, `-%=`, `*%=`) say so:

```zeru
var level: u8 = 255;
level +%= 1;
println("{}", level); // 0
```

Integer division rounds toward zero, and `%` takes the sign of the left
side. Dividing by zero stops the program.

```zeru
println("{} {}", 7 / 2, -7 / 2); // 3 -3
println("{} {}", 7 % 2, -7 % 2); // 1 -1
println("{}", 7.0 / 2.0); // 3.5
```

## Booleans

`bool` is `true` or `false`, combined with `&&`, `||` and `!`:

```zeru
var sunny = true;
var warm = false;
println("{}", sunny && !warm); // true
```

## Characters

A character in single quotes is the number of its ASCII code, an `i32`
unless a type is given. Text is made of bytes, so characters are mostly
compared with the bytes of a string:

```zeru
var letter: u8 = 'a';
println("{} {}", letter, 'A' + 1); // 97 66
println("{}", "zeru"[0] == 'z'); // true
```

Escapes: `\n` (new line), `\t` (tab), `\r`, `\0`, `\\`, `\'`, `\"` and
`\x41` for a byte in hex.

## Operators

| Operators | Meaning |
|---|---|
| `+ - * / %` | arithmetic |
| `+% -% *%` | wrapping arithmetic |
| `== != < <= > >=` | comparison |
| `&& \|\| !` | logic on `bool` |
| `& \| ^ << >> !` | bits on integers (`!` flips every bit) |
| `= += -= *= /= %= &= \|= ^= <<= >>=` | assignment |

`>>` on a signed integer keeps the sign; on an unsigned one it fills with
zeros.

Integers also have methods on their bits: `count_ones`, `leading_zeros` and
`trailing_zeros` count them, as a `usize`; `rotate_left(n)` and
`rotate_right(n)` move them round by `n` places; `swap_bytes` reverses the
order of the bytes.

```zeru
var flags: u8 = 0b1011_0000;
var word: u16 = 0x1234;
println("{} {} {}", flags.count_ones(), flags.leading_zeros(), flags.trailing_zeros()); // 3 0 4
println("{:08b} {:x}", flags.rotate_left(2), word.swap_bytes()); // 11000010 3412
```
