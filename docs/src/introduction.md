![The Zeru Logo](./assets/Logo.svg)

# The Zeru Programming Language

Zeru is a small systems programming language. It compiles to native code,
has no garbage collector and no runtime to speak of, and checks at compile
time that a program never reads memory that was freed or moved.

It is built around three ideas:

- **Simple.** The language is small enough to learn in a weekend. There is
  one way to do each task: one loop over a list, one way to report an
  error, one way to free memory (you do not; it happens when a value goes
  out of scope).
- **Fast.** Programs are static Linux executables optimized by LLVM. Values
  live on the stack unless they need to grow, and nothing runs behind your
  back.
- **Safe.** Moves and references are checked when the program compiles.
  Out-of-bounds indexes, overflowing arithmetic and other mistakes stop the
  program with the file and line where they happened, unless you build for
  maximum speed and ask for those checks to be left out.

A taste of it:

```zeru
import std.collections.hashmap::{HashMap};

struct Account {
    owner: str,
    balance: i64,

    fn deposit(var self, amount: i64) {
        self.balance += amount;
    }
}

fn main() {
    var accounts: Vec<Account> = Vec.new();
    accounts.push(Account { owner: "ann", balance: 10 });
    accounts.push(Account { owner: "bob", balance: 0 });
    accounts[1].deposit(25);

    var by_balance: HashMap<i64, usize> = HashMap.new();
    for i, account in accounts {
        by_balance.insert(account.balance, i);
    }
    var richest = *by_balance.get(25).unwrap();
    println("{} has 25", accounts[richest].owner); // bob has 25
}
```

## About this book

The book is a guide, not a full reference. It starts with installing the
compiler and writing a first program, then goes through the language one
topic at a time, the standard library, the build modes and a few complete
programs. The last chapter explains why the language looks the way it
does.

Every example in the book compiles and runs. Where an example prints
something, the output is written after the call as a comment:

```zeru
println("{} + {} = {}", 2, 3, 2 + 3); // 2 + 3 = 5
```

The standard library documents every function next to its code, with
examples. Editors with the Zeru plugin show that documentation when you hover
over a name.

> **Note:** Zeru is young. The language may still change between versions,
> and it is not ready for programs that need strong guarantees.
