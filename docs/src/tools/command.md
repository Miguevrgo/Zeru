# The zeru Command

| Command | Does |
|---|---|
| `zeru run file.zr [-- args]` | compiles and runs, passing what follows `--` to the program |
| `zeru build file.zr` | compiles into `build/` |
| `zeru test file.zr [filter]` | runs the tests of the file and the files it imports |
| `zeru check file.zr` | looks for errors without generating code |
| `zeru fmt files...` | formats files in place |
| `zeru clean` | removes `build/` |
| `zeru update` | installs the build of the latest commit of `main` |
| `zeru update --stable` | installs the latest release |

`run`, `build` and `test` take `--release-safe` or `--release-fast`, as the
[Build Modes](modes.md) page explains. `zeru help <command>` shows each
command's options.

## Errors

The compiler reports every error it finds: what is wrong, the line it is
on, the other lines that explain it, and how to fix it, with the fixed
code when it is one change:

```text
Error: 'list' is used after it was moved
   ╭─[ main.zr:8:19 ]
   │
 7 │     consume(list);
   │             ──┬─
   │               ╰─── 'list' is moved here
 8 │     println("{}", list.len());
   │                   ─────┬────
   │                        ╰────── used here after the move
   │
   │ Help: to keep 'list', move a copy of it instead
   │           consume(list.copy());
───╯
```

`zeru check` gives the same errors without building anything, which is
faster. With `--json` it prints each error as one JSON object per line,
for editors and other tools, with the fix as an edit they can apply.

## Warnings

A program with no errors may still get warnings, for code that builds but
is likely a mistake. A warning never stops a build:

| Warning | Example |
|---|---|
| a variable never read | `var count = 0;` and no read of `count` |
| a private function or constant never used | `fn helper() { }` that nothing calls |
| a returned value left unused | `v.pop();`; `var _ = v.pop();` drops it on purpose |
| code after `return`, `break`, `continue`, `exit` or `assert(false)` | `return; println("done");` |
| a comparison known before it runs | `x == x`, `n >= 0` with `n` unsigned |
| a value assigned to itself | `x = x;` |
| a function that calls itself on every path | `fn f() i32 { return f(); }` |
| longer code for what the language or std has | `v.len() == 0`, `flag == true`, `if c { true } else { false }`, `if a < b { a } else { b }` |

A variable, function or constant whose name starts with `_` gets no
warning for being unused.

## Tests

Tests go in one `#test` block at the end of a file. Each `test("name")`
holds one test, and the block may also hold functions, structs and imports
only the tests use:

```zeru,check
fn add(a: i32, b: i32) i32 {
    return a + b;
}

fn main() {
    println("{}", add(1, 2));
}

#test {
    test("add sums") {
        assert(add(2, 3) == 5);
    }

    test("text becomes a number") {
        var n = "42".parse_int()!;
        assert(n == twice(20));
    }

    fn twice(n: i64) i64 {
        return n * 2;
    }
}
```

A test fails when it panics, as a failed `assert` does, or when an error
leaves it through `!`. It sees everything in its file, `pub` or not.
`zeru run` and `zeru build` leave the block out.

`zeru test main.zr` runs the tests of `main.zr` and of every file it
imports, outside the standard library, each in a process of its own. It
lists the passes as they finish, then each failure with what it printed.
For the file above, whose second test is wrong:

```text
   Compiling v0.4.0 (main.zr)
    Finished unoptimized + debug info in 0.012s
[PASS] main.zr:L10 add sums
[FAIL] main.zr:L14 text becomes a number
       panic at main.zr:16:9: assertion failed: n == twice(20)
---------------------------
1 passed, 1 failed in 0.002s
```

`zeru test main.zr parse` runs only the tests whose name holds `parse`.
The command exits with an error when a test fails.

## Formatting

`zeru fmt` lays code out in the one Zeru style: four-space indents, lines
up to 100 columns, long calls and lists broken one item per line, comments
and blank lines kept where you put them. It has no settings.

```sh
zeru fmt main.zr src/*.zr   # format in place
zeru fmt --check main.zr    # only list files that are not formatted
zeru fmt - < main.zr        # format stdin to stdout
```

`--check` exits with an error when a file needs formatting, which suits a
CI step.

## Editors

The [zeru-editors](https://github.com/Miguevrgo/zeru-editors) repository
has highlighting for Neovim, Vim, Emacs and VS Code, and a language server
for Neovim and Emacs: errors as you type, documentation on hover, go to
definition, references, rename and completion.
