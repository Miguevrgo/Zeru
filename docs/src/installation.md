# Installation

The first step is to install Zeru. There are some releases provided under github directory (this releases include some precompiled version for the compiler), additionally, there is an install.sh script under the main project which takes the last stable version and performs a full install of the standard library along with the compiler binary. You'll need an internet connection for the download.

> [!TIP]
> Zeru is still under active development, which means the recommended way to try Zeru is currently through a code compilation on main, for this sake you'll need some dependencies:

## Prerequisites

* Rust (latest stable)
* LLVM 21 or newer
* A C compiler (`cc`), only as the linker: programs Zeru builds are static and use no libc

## Building

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

For syntax highlighting in **Vim/Neovim, VSCode or Emacs** checkout:

👉 **[https://github.com/Miguevrgo/zeru-editors](https://github.com/Miguevrgo/zeru-editors)**
