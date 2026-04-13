<div align="center">
  <img src="assets/Logo.svg" alt="Zeru Logo" >

# Zeru Programming Language

  **Simple. Fast. Safe.**

<p>
    <a href="https://github.com/Miguevrgo/zeru/actions">
      <img src="https://img.shields.io/github/actions/workflow/status/Miguevrgo/zeru/ci.yml?style=flat-square" alt="Build Status">
    </a>
    <a href="https://github.com/Miguevrgo/zeru/blob/main/LICENSE">
      <img src="https://img.shields.io/badge/license-MIT-blueviolet?style=flat-square" alt="License">
    </a>
    <img src="https://img.shields.io/badge/built_with-Rust-orange?style=flat-square" alt="Built with Rust">
  </p>

  <p>
    Zeru is a systems programming language designed to combine the
    <strong>performance of C/Zig/Rust</strong> with the <strong>safety guarantees of Rust</strong>,
    while keeping the <strong>simplicity and readability of Go</strong>.
  </p>
</div>

## 🔮 Why Zeru?

I am no expert, nor do I consider myself a better programmer than those who have created memorable languages like Zig, Rust, or C++. However, in my humble opinion, while I still love each of these languages, none of them feels quite "right" to me:

* **C++:** I am not reinventing the wheel by stating it is a bloated language. Backwards compatibility essentially makes it 5 languages in one, with legacy code forcing implementations that lead to undefined behaviors. Zeru aims to be kind of like "modern C++" (saving the distances).
* **Rust:** I love Rust, but when I try to recommend it to someone, they find it too difficult, and let's not even talk about `async` + explicit lifetimes. Compilation times are quite big and, as a personal opinion, while I love sharing code, I don't like dependency-bloated projects. I think they are harder to maintain and create security risks (malicious code injection).
* **Zig:** I can't really criticize Zig as I haven't tried it extensively. However, just reading the examples, I am not very attracted to the manual allocator handling in user code, and I am looking for something different.
* **Go:** The only thing I like about it is the simplicity. I am not saying it is bad, but I don't enjoy reading or using it. I think the step from Rust to Go involves losing too many safety guarantees and efficiency (due to the GC), which isn't worth it for me. Zeru adopts Go's philosophy of not having multiple ways of doing the same thing.

I would also like to note that **Zeru is not "The next language"** to reign over them all. We all know the "JS frameworks" memes. Zeru's ideal state is simply what **my** perfect language would look like and what I'd love to program in. If this philosophy attracts other people, they are kindly welcome to join me.

> **Disclaimer:** As of the current state, Zeru is not recommended for any application requiring strict guarantees. Hopefully, some future version (v1.0+) will be robust and ready for usage.

## ⚡ Features

* **🛡️ Memory Safe:** Strict type checking and ownership concepts to prevent common bugs.
* **🚀 High Performance:** Compiles to optimized native machine code via LLVM.
* **✨ Modern Syntax:** Clean, expressive, and explicit. No hidden control flow.
* **🔧 Zero Cost Abstractions:** Pay only for what you use.

## 📖 Documentation

For detailed information on the language design, installation, syntax, and features, please read the **[Zeru Book](https://github.com/Miguevrgo/Zeru/tree/main/docs)**. You can build it locally by navigating to the `docs/` directory and running:

```bash
mdbook serve --open
```

## 📦 Installation

Currently, Zeru is in active development. To try it out, you need to build the compiler from source.

### Prerequisites

* Rust (latest stable)
* LLVM 21 or newer

### Building

```rust
git clone git@github.com:Miguevrgo/Zeru.git
cd Zeru

cargo build --release
```

## 💻 Usage

Create a file with `.zr` extension and program some code, you may find useful to read the `examples`.

Now you cand compile the and run it:

```sh
zeru build file_name.zr
```

There are more flags available, which you can read using the help command `zeru --help`. The executable will be generated inside the build directory.

## 🤝 Contributing

Zeru is a personal project, and a challenge, meaning there are surely better ways to do some things, simpler, more efficient and/or readable. There are also missing features and bugs, and I would love to know about them.

To collaborate, you can fork the project, create a branch for the feature/improvement/bug fix, and open a Pull Request. Additionally, you can send me an email at <miguevrgo@gmail.com>.

## 🎨 Editor Support

### Syntax Highlighting (Tree-sitter)

For syntax highlighting in Vim/Neovim, VSCode or Emacs checkout: 

👉 **https://github.com/Miguevrgo/zeru-editors**

```shell
git clone https://github.com/Miguevrgo/tree-sitter-zeru
cd tree-sitter-zeru
./install.sh neovim   # or: ./install.sh helix
```

## 📄 License

Distributed under the MIT License. See LICENSE for more information.
