# term

`std.term` has what a program needs to draw the whole terminal, as a
system monitor or a game does: raw mode, the size of the screen, the
cursor, clearing, and reading keys as they are pressed.

```zeru,ignore
import std.term;
import std.term::{Key};

fn main() {
    term::raw_enable().unwrap();
    term::hide_cursor();
    var (rows, columns) = term::size().unwrap();
    var row: u16 = rows / 2;
    while true {
        term::clear();
        term::move_to(row, columns / 2);
        print("@");
        term::flush();
        var pressed = term::key(0);
        if pressed.is_none() {
            continue;
        }
        match pressed.unwrap() {
            Key::Up if row > 0 => row -= 1,
            Key::Down if row + 1 < rows => row += 1,
            Key::Char(c) if c == 'q' => {
                break;
            }
            default => {}
        }
    }
    term::show_cursor();
    term::flush();
    term::raw_disable().unwrap();
}
```

## Raw mode

`raw_enable()` makes each key reach the program as it is pressed, without
being echoed, and `raw_disable()` puts the terminal back as it was. In raw
mode Ctrl-C does not stop the program: it comes as `Key::Ctrl('c')`. The
terminal no longer starts a new line on `"\n"` either, so a program writes
`"\r\n"`, or moves the cursor.

## Drawing

| Function | Does |
|---|---|
| `size()` | gives the rows and columns of the terminal, as `(u16, u16)` |
| `move_to(row, column)` | moves the cursor, counting from 0 at the top left |
| `clear()`, `clear_line()`, `clear_down()` | clears the screen, the cursor's row, or from the cursor down |
| `hide_cursor()`, `show_cursor()` | hides or shows the cursor |
| `flush()` | sends what was written so far |

These write through the same buffer as `print`, and `flush()` sends it
all, so a screen drawn and then flushed appears at once, without flicker.

## Keys

`key(ms)` gives the next key pressed, waiting up to `ms` milliseconds, or
for ever when `ms` is 0; `None` when none came. A key is a `Key`:
`Char(byte)` for a letter, digit or sign, `Ctrl(letter)`, `Enter`, `Tab`,
`Backspace`, `Escape`, the arrows `Up`, `Down`, `Left` and `Right`,
`Home`, `End`, `PageUp`, `PageDown` and `Delete`.

Text is read as ASCII: a letter outside it, as `ñ`, comes as one `Char`
for each of its bytes.
