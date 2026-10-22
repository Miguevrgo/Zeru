# Installation

## Quick install (Linux x86-64)

The installer downloads the latest release, the compiler and its standard
library, into `~/.zeru`:

```sh
curl -fsSL https://raw.githubusercontent.com/Miguevrgo/Zeru/main/install.sh | bash
export PATH="$HOME/.zeru/bin:$PATH"   # add this line to your ~/.bashrc or ~/.zshrc
```

Zeru runs on Linux x86-64. Programs are linked with your system's C
compiler (`cc`), which only acts as the linker.

`zeru update` then installs the build of `main`'s latest commit, and
`zeru update --stable` the latest release.

## Building from source

Zeru is under active development, so `main` is ahead of the latest release.
To build it you need:

### Prerequisites

* Rust (latest stable), the language the compiler is written in
* LLVM 21 or newer
* A C compiler (`cc`), only as the linker: programs Zeru builds are static
  and use no C library

### Building

With those installed, clone the repository and build it:

```sh
git clone git@github.com:Miguevrgo/Zeru.git
cd Zeru
cargo build --release
```

The compiler is then `./target/release/zeru`. It uses the `std/` folder in
the closest directory above its executable, so this one uses the
repository's standard library. Set `ZERU_STD_PATH` to use another one.

## Editor Support

Neovim gets a plugin with a language server: errors as you type and on save,
hover with documentation, go to definition, references, rename, completion and
highlighting. With [lazy.nvim](https://github.com/folke/lazy.nvim):

```lua
{ "Miguevrgo/zeru-editors", lazy = false, build = "cargo build --release --manifest-path server/Cargo.toml" }
```

Emacs gets highlighting, indentation and the same language server through eglot; Vim and
VS Code get highlighting. Setup for every editor:

[github.com/Miguevrgo/zeru-editors](https://github.com/Miguevrgo/zeru-editors)
