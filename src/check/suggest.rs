//! What to suggest for a name that names nothing: the name another
//! language gives what Zeru calls otherwise, the closest name that exists,
//! or the std module to import for it.

use std::path::Path;

use crate::errors::{Sources, Span, ZeruError};

/// Type names other languages use, and the Zeru type for each.
const TYPE_ALIASES: &[(&str, &str)] = &[
    ("int", "i32"),
    ("uint", "u32"),
    ("long", "i64"),
    ("ulong", "u64"),
    ("short", "i16"),
    ("byte", "u8"),
    ("char", "u8"),
    ("float", "f64"),
    ("double", "f64"),
    ("boolean", "bool"),
    ("Bool", "bool"),
    ("string", "str"),
    ("Str", "str"),
    ("list", "Vec"),
    ("List", "Vec"),
    ("Map", "HashMap"),
    ("Dict", "HashMap"),
    ("Set", "HashSet"),
];

/// Values other languages write, and how Zeru writes them.
const VALUE_ALIASES: &[(&str, &str)] = &[
    ("null", "None"),
    ("nil", "None"),
    ("NULL", "None"),
    ("nullptr", "None"),
    ("True", "true"),
    ("False", "false"),
    ("this", "self"),
];

/// Method names other languages use, and the Zeru methods for each, the
/// first one a type has being the one meant.
const METHOD_ALIASES: &[(&str, &str)] = &[
    ("length", "len"),
    ("size", "len"),
    ("count", "len"),
    ("append", "push"),
    ("push_back", "push"),
    ("add", "push"),
    ("add", "insert"),
    ("clone", "copy"),
    ("dup", "copy"),
    ("empty", "is_empty"),
    ("isEmpty", "is_empty"),
    ("includes", "contains"),
    ("has", "contains"),
    ("contains_key", "contains"),
    ("erase", "remove"),
    ("delete", "remove"),
    ("pop_back", "pop"),
    ("strip", "trim"),
];

/// The Zeru name for `name` in `table`, among those `exists` accepts.
fn alias<'t>(
    table: &[(&str, &'t str)],
    name: &str,
    exists: impl Fn(&str) -> bool,
) -> Option<&'t str> {
    table
        .iter()
        .find(|&&(other, ours)| other == name && exists(ours))
        .map(|&(_, ours)| ours)
}

pub(crate) fn type_alias(name: &str) -> Option<&'static str> {
    alias(TYPE_ALIASES, name, |_| true)
}

pub(crate) fn value_alias(name: &str) -> Option<&'static str> {
    alias(VALUE_ALIASES, name, |_| true)
}

/// The method another language calls `name`, among `methods`.
pub(crate) fn method_alias(name: &str, methods: &[String]) -> Option<&'static str> {
    alias(METHOD_ALIASES, name, |ours| {
        methods.iter().any(|m| m == ours)
    })
}

/// Suggest in `error` the candidate closest to `name`, replacing what `at`
/// covers when it is known.
pub(crate) fn did_you_mean(
    error: &mut ZeruError,
    name: &str,
    candidates: &[String],
    at: Option<Span>,
) {
    if let Some(similar) = closest(name, candidates) {
        error.help(format!("did you mean '{similar}'?"));
        if let Some(at) = at {
            error.fix(at, similar);
        }
    }
}

/// The candidate closest to `name`, never `name` itself, within one edit
/// of a short name and two of a longer one; a single letter has none.
pub(crate) fn closest<'c>(name: &str, candidates: &'c [String]) -> Option<&'c str> {
    let most = match name.chars().count() {
        0 | 1 => 0,
        2..=4 => 1,
        _ => 2,
    };
    let lower = name.to_lowercase();
    candidates
        .iter()
        .filter(|c| c.as_str() != name)
        .map(|c| (c.as_str(), edit_distance(&lower, &c.to_lowercase())))
        .filter(|&(_, distance)| distance <= most)
        .min_by_key(|&(c, distance)| {
            let shared = c
                .chars()
                .zip(name.chars())
                .take_while(|(a, b)| a == b)
                .count();
            (distance, usize::MAX - shared)
        })
        .map(|(c, _)| c)
}

/// The edits that turn `a` into `b`: a letter added, removed or changed,
/// or two side by side swapped.
fn edit_distance(a: &str, b: &str) -> usize {
    let (a, b): (Vec<char>, Vec<char>) = (a.chars().collect(), b.chars().collect());
    let mut rows = vec![vec![0; b.len() + 1]; a.len() + 1];
    for (i, row) in rows.iter_mut().enumerate() {
        row[0] = i;
    }
    for (j, cell) in rows[0].iter_mut().enumerate() {
        *cell = j;
    }
    for i in 1..=a.len() {
        for j in 1..=b.len() {
            let change = usize::from(a[i - 1] != b[j - 1]);
            let mut best = (rows[i - 1][j] + 1)
                .min(rows[i][j - 1] + 1)
                .min(rows[i - 1][j - 1] + change);
            if i > 1 && j > 1 && a[i - 1] == b[j - 2] && a[i - 2] == b[j - 1] {
                best = best.min(rows[i - 2][j - 2] + 1);
            }
            rows[i][j] = best;
        }
    }
    rows[a.len()][b.len()]
}

/// Each std module, as `std.collections.hashmap`, with its source.
pub(crate) fn std_modules() -> Vec<(String, String)> {
    fn walk(dir: &Path, prefix: &str, found: &mut Vec<(String, String)>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
                continue;
            };
            let module = format!("{prefix}.{stem}");
            if path.is_dir() {
                walk(&path, &module, found);
            } else if path.extension().is_some_and(|e| e == "zr") && module != "std.builtin" {
                found.push((module, std::fs::read_to_string(&path).unwrap_or_default()));
            }
        }
    }
    let mut found = Vec::new();
    if let Some(std) = crate::driver::std_path() {
        walk(&std, "std", &mut found);
    }
    found.sort();
    found
}

/// The std module an import names `short`, as `std.math` for `math`, when
/// it declares a public `name`.
pub(crate) fn std_module_with(short: &str, name: &str) -> Option<String> {
    std_modules()
        .into_iter()
        .find(|(module, source)| {
            module.rsplit('.').next() == Some(short)
                && source.lines().any(|line| declares(line, name))
        })
        .map(|(module, _)| module)
}

/// The std module that declares a public `name` at its top level.
pub(crate) fn std_home(name: &str) -> Option<String> {
    std_modules()
        .into_iter()
        .find(|(_, source)| source.lines().any(|line| declares(line, name)))
        .map(|(module, _)| module)
}

/// Whether `line` declares a public `name` at the top level of a file.
pub(crate) fn declares(line: &str, name: &str) -> bool {
    declared(line) == Some(name)
}

/// The public names a file declares at its top level.
pub(crate) fn publics(source: &str) -> Vec<String> {
    source
        .lines()
        .filter_map(declared)
        .map(String::from)
        .collect()
}

/// The name `line` declares public at the top level of a file.
fn declared(line: &str) -> Option<&str> {
    let rest = line.strip_prefix("pub ")?;
    let rest = ["struct ", "enum ", "fn ", "const ", "trait "]
        .iter()
        .find_map(|kind| rest.strip_prefix(kind))?;
    let end = rest
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .unwrap_or(rest.len());
    Some(&rest[..end])
}

/// Where `name<...>` is written inside `within`, and what its angle
/// brackets hold.
pub(crate) fn generic_in(sources: &Sources, within: Span, name: &str) -> Option<(Span, String)> {
    let start = sources.word_in(within, name)?;
    let text = sources.text(Span::new(start.end, within.end));
    let inner = text.strip_prefix('<')?;
    let mut depth = 1;
    let close = inner.find(|c| {
        match c {
            '<' => depth += 1,
            '>' => depth -= 1,
            _ => {}
        }
        depth == 0
    })?;
    let span = Span::new(start.start, start.end + close + 2);
    Some((span, inner[..close].trim().to_string()))
}

/// Suggest in `error` importing `module`, or only `symbol` from it, at the
/// top of the file `at` is in.
pub(crate) fn suggest_import(
    sources: &Sources,
    error: &mut ZeruError,
    at: Span,
    module: &str,
    symbol: Option<&str>,
) {
    let (what, import) = match symbol {
        Some(symbol) => (
            format!("'{symbol}' is in {module}"),
            format!("{module}::{{{symbol}}}"),
        ),
        None => (format!("{module} is not imported"), module.to_string()),
    };
    error.help(format!("{what}; import it"));
    if let Some(start) = sources.file_start(at) {
        error.fix(Span::new(start, start), format!("import {import};\n"));
    }
}
