# Installation

Currently, Zeru is in active development. To try it out, you need to build the compiler from source.

### Prerequisites

* Rust (latest stable)
* LLVM 21 or newer

### Building

```shell
git clone git@github.com:Miguevrgo/Zeru.git
cd Zeru

cargo build --release
```

After building, you can use the executable located in `./target/release/zeru` to compile and run your `.zr` programs.

Alternatively, you can install the pre-built binaries (if available) using the provided installation script:

```shell
./install.sh
```

### Editor Support

#### Syntax Highlighting (Tree-sitter)

For syntax highlighting in Vim/Neovim, VSCode or Emacs checkout:

👉 **[https://github.com/Miguevrgo/zeru-editors](https://github.com/Miguevrgo/zeru-editors)**

```shell
git clone https://github.com/Miguevrgo/tree-sitter-zeru
cd tree-sitter-zeru
./install.sh neovim   # or: ./install.sh helix
```
