# Usage

Now that you have installed Zeru its time to write your first program. Create a file with `.zr` extension and write some code, you may find examples/ directory helpful, here we will show a simple hello world program:

`filename: main.zr`

```rust
fn main() {
    println("Hello, World!");
}
```

Now you can compile and run it:

```sh
$ zeru build main.zr && ./build/main
Hello, World!
```

If you prefer to immediately run the compiled executable, use:

```sh
zeru run main.zr
```

Arguments for the program go after `--`, and the program reads them with
`os::args()` from `std.os`, its own path first:

```sh
zeru run main.zr -- input.txt 3
```

There are more flags available, which you can read using the help command `zeru --help`. The executable will be generated inside the `build` directory.

## Editor support

`zeru lsp` is a language server: it speaks LSP over stdin and stdout and
answers from the same lexer, parser, type checker and borrow checker that
`zeru build` runs, so the editor never disagrees with the compiler. It gives
diagnostics as you type, hover with types, go to definition, references,
rename, completion, signature help, document symbols and semantic
highlighting. Files the editor has open are read from the editor, the rest
from disk, and the standard library is found as `zeru build` finds it.

In Neovim 0.11 or later:

```lua
vim.filetype.add({ extension = { zr = 'zeru' } })
vim.lsp.config('zeru', {
  cmd = { 'zeru', 'lsp' },
  filetypes = { 'zeru' },
  root_markers = { '.git' },
})
vim.lsp.enable('zeru')
```
