# The zeru Command

| Command | Does |
|---|---|
| `zeru run file.zr [-- args]` | compiles and runs, passing what follows `--` to the program |
| `zeru build file.zr` | compiles into `build/` |
| `zeru check file.zr` | looks for errors without generating code |
| `zeru fmt files...` | formats files in place |
| `zeru clean` | removes `build/` |
| `zeru update` | installs the build of the latest commit of `main` |
| `zeru update --stable` | installs the latest release |

`run` and `build` take `--release-safe` or `--release-fast`, as the
[Build Modes](modes.md) page explains. `zeru help <command>` shows each
command's options.

## Errors

The compiler reports every error it finds, each with the line it is on and
a hint where it can guess the fix:

```text
Error: Use of moved value 'list'. Value was previously moved and is no longer valid
   ╭─[ main.zr:4:19 ]
   │
 4 │     println("{}", list.len());
   │                   ─────┬────
   │                        ╰────── Use of moved value 'list'. Value was previously moved and is no longer valid
───╯
```

`zeru check` gives the same errors without building anything, which is
faster. With `--json` it prints each error as one JSON object per line,
for editors and other tools.

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
