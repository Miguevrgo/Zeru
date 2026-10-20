# Installation

## Quick install (Linux x86-64)

The installer downloads the latest release, the compiler and its standard
library, into `~/.zeru`:

```sh
curl -fsSL https://raw.githubusercontent.com/Miguevrgo/Zeru/main/install.sh | bash
export PATH="$HOME/.zeru/bin:$PATH"   # add this line to your ~/.bashrc or ~/.zshrc
```

Programs are linked with your system's C compiler (`cc`), which only acts as
the linker.

`zeru update` then installs the build of `main`'s latest commit, and
`zeru update --stable` the latest release.

## Building from source

Zeru is under active development, so `main` is ahead of the latest release.
To build it you need:

### Prerequisites

* Rust (latest stable)
* LLVM 21 or newer
* A C compiler (`cc`), only as the linker: programs Zeru builds are static and use no libc

### Building

**After installing** rust and llvm you can clone the repo and compile it using cargo:

```zsh
git clone git@github.com:Miguevrgo/Zeru.git
cd Zeru
cargo build --release
```

After building, you can use the executable located in `./target/release/zeru` to compile and run your `.zr` programs.

> [!NOTE]
> The compiler uses the `std/` folder in the closest directory above its executable, so `./target/release/zeru` uses the repository's. Set the ZERU_STD_PATH env variable to use another one.

## Editor Support

Neovim gets a plugin with a language server: errors as you type and on save,
hover with documentation, go to definition, references, rename, completion and
highlighting. With [lazy.nvim](https://github.com/folke/lazy.nvim):

```lua
{ "Miguevrgo/zeru-editors", lazy = false, build = "cargo build --release --manifest-path server/Cargo.toml" }
```

Vim, VS Code and Emacs get syntax highlighting. Setup for every editor:

👉 **[https://github.com/Miguevrgo/zeru-editors](https://github.com/Miguevrgo/zeru-editors)**
