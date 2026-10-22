# fs

```zeru,ignore
import std.fs;
```

## Whole files

The simplest way to use a file is all at once:

| Function | Does |
|---|---|
| `read_file(path) String!` | reads the whole file |
| `write_file(path, text) !` | creates or empties the file and writes `text` |
| `append_file(path, text) !` | adds `text` at the end, creating the file if needed |

```zeru
import std.fs;

fs::write_file("todo.txt", "buy milk\n").unwrap();
fs::append_file("todo.txt", "call ann\n").unwrap();
var text = fs::read_file("todo.txt").unwrap();
for line in text.lines() {
    println("- {}", line);
}
```

```text
- buy milk
- call ann
```

## File

For a big file, or to read it a line at a time, open a `File`. It is
closed when it goes out of scope.

| Function | Does |
|---|---|
| `File.open(path)` | opens a file to read |
| `File.create(path)` | creates or empties a file to write |
| `File.create_new(path)` | creates a file to write, `EEXIST` if it exists |
| `File.append(path)` | opens a file to write at its end, creating it if needed |
| `f.read_line() (String?)!` | the next line without its line break, `None` at the end |
| `f.read(&var buf, count) usize!` | up to `count` bytes added to the `String` `buf`, 0 at the end |
| `f.write(text) !` | writes all of `text` |
| `f.seek(offset) !` | moves to byte `offset` from the start |

```zeru
import std.fs;

fs::write_file("names.txt", "ann\nbob\ncid\n").unwrap();
var file = fs::File.open("names.txt").unwrap();
var count = 0;
while true {
    var line = file.read_line().unwrap();
    if line.is_none() {
        break;
    }
    count += 1;
}
println("{} names", count); // 3 names
```

## Paths and directories

| Function | Does |
|---|---|
| `exists(path)`, `is_file(path)`, `is_dir(path)` | ask what is at `path` |
| `metadata(path) Metadata!` | kind, size, permissions, owner and times |
| `symlink_metadata(path)` | the same, about a link itself |
| `remove_file(path)`, `rename(from, to)`, `copy_file(from, to)` | manage files |
| `create_dir(path)`, `remove_dir(path)` | make or remove an (empty) directory |
| `read_dir(path) Vec<Entry>!` | the `name` and `kind` of each entry of a directory |
| `set_permissions(path, mode)` | as `0o644` |
| `set_owner(path, uid, gid)`, `set_times(path, accessed, modified)` | `None` leaves one unchanged |
| `symlink(target, path)`, `hard_link(target, path)`, `read_link(path)` | links |
| `real_path(path)`, `current_dir()`, `set_current_dir(path)` | where things are |

```zeru
import std.fs;

fs::create_dir("notes").unwrap();
fs::write_file("notes/a.txt", "hello").unwrap();
println("{}", fs::metadata("notes/a.txt").unwrap().size); // 5
var entries = fs::read_dir("notes").unwrap();
println("{} {}", entries.len(), entries[0].name); // 1 a.txt
fs::remove_file("notes/a.txt").unwrap();
fs::remove_dir("notes").unwrap();
println("{}", fs::exists("notes")); // false
```

Every function that can fail returns the operating system's error code:
`ENOENT` when nothing is at the path, `EACCES` without permission, and so
on.
