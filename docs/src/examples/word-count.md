# Word Count

This program counts the lines, words and bytes of a file, and finds its
longest line:

```sh
$ zeru run wc.zr -- poem.txt
3 lines, 9 words, 45 bytes
longest line: 19 bytes
$ zeru run wc.zr -- missing.txt
no such file
```

```zeru,check
import std.fs;
import std.os;

const USAGE: i32 = 1;

// What wc counts in a text.
struct Counts {
    lines: usize,
    words: usize,
    bytes: usize,
    longest: usize,
}

fn is_space(byte: u8) bool {
    return byte == ' ' || byte == '\t' || byte == '\r';
}

fn count(text: str) Counts {
    var counts = Counts { lines: 0, words: 0, bytes: text.len(), longest: 0 };
    for line in text.lines() {
        counts.lines += 1;
        if line.len() > counts.longest {
            counts.longest = line.len();
        }
        var in_word = false;
        for byte in line {
            if is_space(byte) {
                in_word = false;
            } else if !in_word {
                in_word = true;
                counts.words += 1;
            }
        }
    }
    return counts;
}

fn run() ! {
    var args = os::args();
    if args.len() != 2 {
        eprintln("usage: wc <file>");
        return Err(USAGE);
    }
    var text = fs::read_file(args[1])!;
    var counts = count(text);
    println("{} lines, {} words, {} bytes", counts.lines, counts.words, counts.bytes);
    println("longest line: {} bytes", counts.longest);
}

fn main() {
    var result = run();
    if result.is_err() {
        var code = result.unwrap_err();
        if code == ENOENT {
            eprintln("no such file");
        } else if code != USAGE {
            eprintln("cannot read the file: error {}", code);
        }
        exit(1);
    }
}
```

## How it works

**`main` and `run`.** The work happens in `run`, which returns `!`: it
either succeeds or fails with an error code. That lets it use `!` after
every step that can fail, like `fs::read_file(args[1])!`, and keep the
normal path straight. `main` makes one decision at the end: on an error,
it tells the user what happened and ends with exit code 1.

**Its own error code.** A missing argument is not an operating system
error, so the program has its own code, `USAGE`. It is a constant, so it
reads as a name everywhere, and `main` tells it apart from `ENOENT`, which
`fs::read_file` returns when there is no such file.

**Counting.** `count` walks the text once. `text.lines()` gives each line
as a `str`, a view into the text with no copying. A word starts at each
byte that is not a space right after one that is, which `in_word`
remembers.

**Memory.** `read_file` returns a `String` that owns the file's contents.
It is passed to `count`, which takes a `str`, without copying. The
`String` is freed when `run` returns, and nothing in the program says so.

## Try it

- Also count the bytes of the shortest line that is not empty.
- Accept several files and print a total.
- Count how many lines start with `#` using `starts_with`.
