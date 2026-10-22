# A First Program: Guess the Number

This program picks a secret number between 1 and 100 and asks the player
to guess it, saying after each try whether the guess was too small or too
big. It touches most of what a real program needs: reading input, turning
text into numbers, loops, conditions and handling bad input.

Here it is whole. The rest of the page goes through it piece by piece.

```zeru,check
import std.os;

fn main() {
    var secret = os::random_range(1, 100) as i64;
    println("Guess the number between 1 and 100.");
    var tries = 0;
    while true {
        print("Your guess: ");
        var line = os::stdin().read_line().unwrap();
        if line.is_none() {
            println("Bye!");
            return;
        }
        var guess = line.unwrap().trim().parse_int();
        if guess.is_err() {
            println("That is not a number.");
            continue;
        }
        tries += 1;
        var n = guess.unwrap();
        if n < secret {
            println("Too small.");
        } else if n > secret {
            println("Too big.");
        } else {
            println("You got it in {} tries!", tries);
            return;
        }
    }
}
```

Save it as `guess.zr` and play:

```sh
$ zeru run guess.zr
Guess the number between 1 and 100.
Your guess: 50
Too big.
Your guess: 25
Too small.
Your guess: 37
You got it in 3 tries!
```

## Importing a module

```zeru,ignore
import std.os;
```

The standard library is split into modules: `os` talks to the operating
system, `fs` to files, `net` to the network, and so on. `import std.os;`
makes the module available as `os`, so its functions are called as
`os::random_range(..)`. A few things, like `println`, `Vec` and `String`,
need no import at all.

## The secret number

```zeru,ignore
var secret = os::random_range(1, 100) as i64;
```

`var` declares a variable. `os::random_range(1, 100)` returns a random `u64`
from 1 to 100, both included. The guess will be an `i64`, a signed 64-bit
integer, and Zeru never converts between number types on its own, so
`as i64` converts the secret once here and the two can be compared later.

## The loop

```zeru,ignore
var tries = 0;
while true {
    ...
}
```

`while` repeats its block while the condition holds; `while true` repeats
until something leaves the loop. Here a `return` ends `main`, and so the
program.

## Reading a line

```zeru,ignore
var line = os::stdin().read_line().unwrap();
if line.is_none() {
    println("Bye!");
    return;
}
```

`os::stdin()` is the standard input and `read_line()` reads one line from
it. Two things can go differently than planned, and the type of the result
says so: reading may fail (the input was closed by an error), and there may
be no line at all (the player pressed Ctrl+D). The result is a
`(String?)!`:

- `!` means the call may fail with an error code. `unwrap()` takes the
  value out, and stops the program if there was an error instead.
- `?` means the value may be missing. `is_none()` asks whether it is, and
  then the program says goodbye.

The [Errors](../errors/README.md) chapter covers both in detail.

## Turning text into a number

```zeru,ignore
var guess = line.unwrap().trim().parse_int();
if guess.is_err() {
    println("That is not a number.");
    continue;
}
```

`trim()` removes the spaces and line break around the text, and
`parse_int()` reads it as a number. Text that is not a number is an error,
so `parse_int` returns an `i64!`. This time the program does not stop on
an error: it checks with `is_err()`, tells the player, and `continue` goes
back to the start of the loop.

## Comparing

```zeru,ignore
tries += 1;
var n = guess.unwrap();
if n < secret {
    println("Too small.");
} else if n > secret {
    println("Too big.");
} else {
    println("You got it in {} tries!", tries);
    return;
}
```

The guess is known to be a number now, so `unwrap()` is safe. The
`if`/`else if`/`else` chain picks one of three answers. Conditions take no
parentheses, and the braces are always required.

## What to try next

- Limit the game to 7 tries.
- Print the secret when the player gives up with Ctrl+D.
- Read the highest number from the command line with `os::args()`.
