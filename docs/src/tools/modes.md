# Build Modes

`zeru build` and `zeru run` compile in one of three modes:

```sh
zeru run main.zr                 # debug
zeru run --release-safe main.zr  # optimized, checks kept
zeru run --release-fast main.zr  # optimized, checks left out
```

| | debug | release-safe | release-fast |
|---|---|---|---|
| Optimization | none | full | full, more aggressive |
| Debug info for debuggers | yes | no | no |
| Moves and references checked when compiling | yes | yes | yes |
| Index, overflow, division and `unwrap` checks | yes | yes | no |
| `assert` | yes | yes | no |
| Compiles | fastest | slower | slower |
| Runs | slowest | fast | fastest |

## debug

The default. Nothing is optimized, so the program compiles quickly and
steps through a debugger line by line, with variables where you expect
them. Use it while writing and testing.

## release-safe

The program is optimized as much as the safety checks allow. Every check
of the [Panics](../errors/panics.md) page stays, and so does every
`assert`, so a bug still stops the program with its file and line instead
of doing something unexpected. Use it for programs you ship.

## release-fast

Optimized, and with the runtime checks left out: indexes are not checked
against the length, arithmetic is not checked for overflow, `unwrap` does
not check for `None`, and `assert` disappears along with its condition. A
program without bugs gives the same results, sooner. A program with one of
those bugs does whatever the machine happens to do: read the wrong memory,
give a wrong number, or crash without saying where.

Use it for code that is well tested and where speed matters, like a
simulation or a number-crunching tool, after it runs cleanly in the other
modes.

The checks made while compiling, moves and references, are the same in
every mode: no build mode lets a program use freed memory through a
reference.

## What gets built

The executable goes to `build/`, named after the source file. `--emit-ir` also keeps the LLVM IR, the code just
before it becomes machine code, in `build/`. `zeru clean` removes the
folder.

Every executable is static: it uses no C library and no shared library,
and runs on any Linux x86-64 machine as it is.
