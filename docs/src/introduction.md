![The Zeru Logo](./assets/Logo.svg)

Zeru is a systems programming language designed to combine the **performance of C/Zig/Rust** with the **safety guarantees of Rust**, while keeping the **simplicity and readability of Go**.

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
