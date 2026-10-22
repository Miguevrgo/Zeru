# Text

Text in Zeru is bytes, usually UTF-8. There are two types:

- `str` is a view of text that lives somewhere else: a string literal in
  the program, or part of a `String`. It cannot change.
- `String` is text the program owns and can grow.

## str

A string literal is a `str`:

```zeru
var greeting = "hello, world";
println("{} has {} bytes", greeting, greeting.len()); // hello, world has 12 bytes
```

Indexing gives a byte (`u8`), slicing gives another `str`, and `for` walks
the bytes:

```zeru
var word = "zeru";
println("{} {}", word[0] == 'z', word[1..3]); // true er
var vowels = 0;
for byte in word {
    if byte == 'e' || byte == 'u' {
        vowels += 1;
    }
}
println("{}", vowels); // 2
```

`len()` counts bytes, not letters: a letter outside ASCII, like `ñ`, takes
more than one byte.

The methods:

| Method | Gives |
|---|---|
| `len()` | the length in bytes |
| `trim()` | the text without spaces, tabs and line breaks around it |
| `split()` | a `Vec<str>` of the words between runs of spaces, tabs and line breaks |
| `split(sep)` | a `Vec<str>` of the parts between each `sep`, empty ones included |
| `lines()` | a `Vec<str>` of the lines, without their line breaks |
| `starts_with(t)`, `ends_with(t)` | whether the text begins or ends with `t` |
| `parse_int()`, `parse_float()` | the number the text spells, or an error (`i64!`, `f64!`) |

```zeru
var line = "  width = 80 ";
var parts = line.trim().split(" = ");
println("{} is {}", parts[0], parts[1]); // width is 80
var width = parts[1].parse_int().unwrap();
println("{}", width * 2); // 160
println("{}", "3.5".parse_float().unwrap() + 1.0); // 4.5
println("{}", "1x".parse_int().is_err()); // true
```

`split()` with no separator is the one to read numbers or words separated
by any amount of spaces. With a separator, each one counts, so two in a
row leave an empty part between them:

```zeru
println("{}", "84283   63343".split()); // ["84283", "63343"]
println("{}", "84283   63343".split(" ")); // ["84283", "", "", "63343"]
```

The parts `split` and `lines` give are views into the original text; no
text is copied.

`==` and `!=` compare text byte by byte:

```zeru
var answer = "yes";
println("{}", answer == "yes"); // true
```

### Raw strings

Between backticks, a string keeps everything as written: backslashes,
quotes and line breaks. It suits paths, patterns and blocks of text:

```zeru
var path = `C:\games\save.txt`;
var block = `first line
second "quoted" line`;
println("{}", path); // C:\games\save.txt
println("{}", block.lines().len()); // 2
```

## String

`String` is text that grows. Make one empty or from a `str`, then add text
or bytes to it:

```zeru
var s = String.from("hello");
s.push_str(", ");
s.push_str("world");
s.push('!');
println("{} ({} bytes)", s, s.len()); // hello, world! (13 bytes)
```

A `String` can be used wherever a `str` is expected, so every `str` method
works on it, and it can be compared, printed and passed to functions that
take `str`:

```zeru
fn shout(text: str) {
    println("{}!", text);
}

fn main() {
    var name = String.from("  ann ");
    shout(name.trim()); // ann!
    println("{}", name.starts_with("  a")); // true
}
```

Like a `Vec`, a `String` owns its memory: assigning it moves it, `copy()`
makes a second one, and it is freed when it goes out of scope. Under the
hood it is a `Vec<u8>`, so `s[i]` reads or writes a byte and `for` walks
the bytes.

Functions in the standard library that produce new text, like reading a
file, return a `String`.

A `str` taken from a `String` is a view into it, so while the view is in
use the compiler does not let the `String` change or go away:

```zeru,fails
fn main() {
    var s = String.from("abc");
    var view = s[0..2];
    s.push_str("def");
    println("{}", view);
}
```

```text
Error: Cannot change 's' while 'view' still refers into it
```

The `push_str` above may move the text to a bigger block of memory, which
would leave `view` pointing at freed memory, so the compiler refuses it.
