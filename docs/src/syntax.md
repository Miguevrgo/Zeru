# Syntax at a Glance

A one-page summary of the language. Each part links to the page that
explains it.

## Variables and types ([more](basics/variables.md))

```zeru,ignore
var count = 0;                    // i32 unless a type is given
var (q, r) = divide(7, 2);        // a tuple taken apart; (q: i32, _) works too
(a, b) = (b, a);                  // assigned at once
var ratio: f64 = 0.5;
const LIMIT: i32 = 10;            // never changes; also at top level
const TABLE: Array<u32, 256> = make_table();   // top level: computed while compiling
var input = "42";
var input = input.parse_int();    // a new variable hiding the old one

i8 i16 i32 i64 u8 u16 u32 u64 usize f32 f64 bool str
1_000  0xff  0b101  0o17  2.5e-3  'a'  "text"  `raw\text`
x as i64                          // the only way to change a number's type
i32.MAX  i64.MIN  f64.MAX         // each number type's limits
a +% b   a -% b   a *% b          // wrapping arithmetic
x.count_ones()  x.leading_zeros()  x.trailing_zeros()
x.rotate_left(3)  x.rotate_right(3)  x.swap_bytes()
```

## Functions ([more](basics/functions.md))

```zeru,ignore
fn add(a: i32, b: i32) i32 { return a + b; }
fn log(message: str) { println("{}", message); }
fn bump(var n: i32) i32 { n += 1; return n; }   // var: an assignable copy
fn pair() (i32, str) { return (1, "one"); }
fn largest<T: Ord>(a: T, b: T) T { ... }        // generic, with a bound
```

## Control flow ([more](basics/control-flow.md))

```zeru,ignore
if x > 0 { ... } else if x == 0 { ... } else { ... }
var sign = if x < 0 { -1 } else { 1 };   // one expression, or a block that returns
while running { ... break; ... continue; ... }
for i in 0..10 { ... }            // 0 to 9
for i in 1..=10 { ... }           // 1 to 10
for x in list { ... }             // each element
for i, x in list { ... }          // with its index
for x in &var list { x += 1; }    // change each element
for (a, b) in pairs { ... }       // each tuple taken apart
for (x, y) in zip(xs, ys) { ... } // two lists side by side
var name = match day {
    Day::Sat | Day::Sun => "weekend",
    default => "weekday",
};
var size = match n { 0 => "none", 1..=9 => "few", x if x < 0 => "negative", default => "many" };
```

## Lists ([more](basics/sequences.md))

```zeru,ignore
var a: Array<i32, 3> = [1, 2, 3];   var zeros: Array<u8, 64> = [0; 64];
var v = Vec[1, 2];                  var empty: Vec<str> = Vec.new();
var grid = Vec[Vec[0; width]; height];   // n known as it runs
v.push(3);  v.pop();  v.len();  v[0];  v.get(9);  v.sort();  v.reverse();  v.copy();
var part = v[1..];                  // a slice, &[i32]
var front = &var v[..2];  front.sort();   // a slice that writes, &var [i32]
var t = (1, true);  t.0;
```

## Text ([more](basics/text.md))

```zeru,ignore
var s: str = "hello";               s.len();  s[0];  s[1..3];
s.split();  s.split(",");  s.lines();  s.contains("a");  s.find("a");  s.trim();  s.starts_with("he");  s.parse_int();
var owned = String.from("hi");      owned.push_str("!");  owned.push('?');
println("{} and {}", 1, "two");     println("{}", v);  // [1, 2]
println("{:.2} {:>5} {:08b} {:x}", 2.0 / 3.0, "ab", 5, 255);  // 0.67    ab 00000101 ff
```

## Structs, enums, traits ([more](structure/structs.md))

```zeru,ignore
struct Point {
    x: i32,
    y: i32,

    fn new(x: i32, y: i32) Point { return Point { x: x, y: y }; }
    fn len2(self) i32 { return self.x * self.x + self.y * self.y; }
    fn move_by(var self, dx: i32) { self.x += dx; }
    fn drop(var self) { ... }       // runs as the value goes away
}
var p = Point.new(1, 2);  p.move_by(3);  p.x;
p == q;  p < q;                  // structs compare field by field

enum Shape { Circle(f64), Rect(f64, f64), Empty }
match shape {
    Shape::Circle(r) => ...,
    Shape::Rect(w, h) => ...,
    Shape::Empty => ...,
}

trait Named { fn name(self) str; }  // met by any type with name(self) str
fn greet<T: Named>(who: T) { ... }
struct Stack<T> { items: Vec<T>, ... }
```

## Modules ([more](structure/modules.md))

```zeru,ignore
import geometry;                     // geometry.zr, next to the main file
import units.money::{euros};         // units/money.zr, euros unqualified
import std.collections.hashmap::{HashMap};
geometry::area(..);
pub fn shared() { ... }              // usable from other files
```

## Memory ([more](memory/README.md))

```zeru,ignore
var b = a;              // copies plain data, moves a Vec, String or owner
var c = a.copy();       // an explicit copy of what a owns
var old = replace(&var p.name, String.new());   // take a value out, leave another
fn read(v: &Vec<i32>)       read(&v);         // borrow to read
fn change(v: &var Vec<i32>) change(&var v);   // borrow to change
*r = 5;                 // write through a reference
var p: *u8 = &bytes[0]; // raw pointer, unchecked
```

## Errors ([more](errors/README.md))

```zeru,ignore
fn find(..) usize? { return 3; ... return None; }
fn parse(..) i64! { return 3; ... return Err(EINVAL); }
fn save(..) ! { ... return Ok(); ... }        // success or an error code
x.is_some()  x.is_none()  x.is_ok()  x.is_err()  x.unwrap()  x.unwrap_err()
var n = parse(text)!;    // returns the error from this function
save(path);              // error: a result is never ignored
var i = find(list)?;     // returns None from this function
match r { Ok(v) => .., Err(code) => .. }
match o { Some(v) => .., None => .. }
assert(n > 0);  assert(n > 0, "n is {}", n);  exit(1);
```

```zeru,ignore
#test {                          // once, at the end of the file
    test("name") { assert(f() == 1); }
}
```
