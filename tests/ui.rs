//! Snapshot tests. Each `tests/ui/<group>/<name>.zr`,
//! and each `main.zr` of a multi-file test, is built and run in every safety mode and
//! must match `<name>.out`. `BLESS=1 cargo test --test ui` rewrites the snapshots.
//! A first line `//@ checked` skips release-fast, which drops the runtime checks.
//! Every example must also build and exit 0 in every mode, and every example
//! in the book and in std's docs must print what it shows.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

const MODES: [&str; 3] = ["", "--release-safe", "--release-fast"];

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Every `.zr` file `depth` 1 below where the walk started, and deeper ones
/// whose name passes `entry`; other deeper files are modules they import.
fn sources(dir: &Path, depth: usize, entry: fn(&str) -> bool, found: &mut Vec<PathBuf>) {
    for path in std::fs::read_dir(dir).unwrap().map(|e| e.unwrap().path()) {
        let name = path.file_name().unwrap().to_string_lossy();
        if path.is_dir() {
            sources(&path, depth + 1, entry, found);
        } else if name.ends_with(".zr") && (depth == 1 || entry(&name)) {
            found.push(path);
        }
    }
}

fn clean(text: &str) -> String {
    let prefix = format!("{}/", root().display());
    let mut out = String::new();
    let mut chars = text.chars();
    while let Some(c) = chars.next() {
        if c == '\x1b' {
            for c in chars.by_ref() {
                if c.is_ascii_alphabetic() {
                    break;
                }
            }
        } else {
            out.push(c);
        }
    }
    out.replace(&prefix, "")
        .lines()
        .filter(|line| {
            let word = line.trim_start_matches(|c: char| !c.is_ascii_alphabetic());
            !word.starts_with("Compiling ") && !word.starts_with("Finished ")
        })
        .map(|line| stable(line.trim_end()))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Drop what changes from run to run or mode to mode: a thread id in a compiler
/// panic, and debug metadata in an LLVM verifier message.
fn stable(line: &str) -> String {
    let mut line = line.to_string();
    for (marker, keep, close) in [(", !dbg !", "", 0), ("thread 'main' (", "thread 'main'", 1)] {
        while let Some(at) = line.find(marker) {
            let end = at + marker.len();
            let rest = &line[end..];
            let digits = rest.len() - rest.trim_start_matches(|c: char| c.is_ascii_digit()).len();
            line.replace_range(at..end + digits + close, keep);
        }
    }
    line
}

fn section(out: &mut String, name: &str, text: &str) {
    if !text.is_empty() {
        out.push_str(&format!("-- {name}\n{text}\n"));
    }
}

/// Build `source` in `mode` inside `scratch`, run it, and describe what happened.
fn observe(source: &Path, mode: &str, scratch: &Path) -> String {
    std::fs::create_dir_all(scratch).unwrap();
    let mut build = Command::new(env!("CARGO_BIN_EXE_zeru"));
    build.arg("build").arg(source).current_dir(scratch);
    if !mode.is_empty() {
        build.arg(mode);
    }
    let built = build
        .env("ZERU_STD_PATH", root().join("std"))
        .output()
        .unwrap();
    let mut out = String::new();
    if !built.status.success() {
        out.push_str("status: compile error\n");
        section(
            &mut out,
            "stderr",
            &clean(&String::from_utf8_lossy(&built.stderr)),
        );
        return out;
    }

    let exe = scratch.join("build").join(source.file_stem().unwrap());
    let mut child = Command::new(exe)
        .current_dir(scratch)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let deadline = Instant::now() + Duration::from_secs(20);
    while child.try_wait().unwrap().is_none() {
        if Instant::now() > deadline {
            child.kill().unwrap();
            break;
        }
        std::thread::sleep(Duration::from_millis(5));
    }
    let ran = child.wait_with_output().unwrap();
    use std::os::unix::process::ExitStatusExt;
    let status = match (ran.status.code(), ran.status.signal()) {
        (Some(code), _) => format!("exit {code}"),
        (_, Some(signal)) => format!("signal {signal}"),
        _ => "unknown".to_string(),
    };
    out.push_str(&format!("status: {status}\n"));
    section(
        &mut out,
        "stdout",
        &clean(&String::from_utf8_lossy(&ran.stdout)),
    );
    section(
        &mut out,
        "stderr",
        &clean(&String::from_utf8_lossy(&ran.stderr)),
    );
    out
}

fn diff(expected: &str, actual: &str) -> String {
    let (old, new): (Vec<_>, Vec<_>) = (expected.lines().collect(), actual.lines().collect());
    let mut out = String::new();
    for i in 0..old.len().max(new.len()) {
        match (old.get(i), new.get(i)) {
            (Some(a), Some(b)) if a == b => out.push_str(&format!("  {a}\n")),
            (a, b) => {
                if let Some(a) = a {
                    out.push_str(&format!("- {a}\n"));
                }
                if let Some(b) = b {
                    out.push_str(&format!("+ {b}\n"));
                }
            }
        }
    }
    out
}

/// Run `check` over `jobs` on every core, collecting the failures it reports.
fn run_all<T: Sync>(jobs: &[T], check: impl Fn(&T, &Path) -> Option<String> + Sync) {
    let next = AtomicUsize::new(0);
    let failures = Mutex::new(Vec::new());
    static RUNS: AtomicUsize = AtomicUsize::new(0);
    let run = RUNS.fetch_add(1, Ordering::Relaxed);
    let scratch = std::env::temp_dir().join(format!("zeru-ui-{}-{run}", std::process::id()));
    std::thread::scope(|scope| {
        for worker in 0..std::thread::available_parallelism().map_or(4, |n| n.get()) {
            let (next, failures, check, scratch) = (&next, &failures, &check, &scratch);
            scope.spawn(move || {
                while let Some(job) = jobs.get(next.fetch_add(1, Ordering::Relaxed)) {
                    if let Some(failure) = check(job, &scratch.join(worker.to_string())) {
                        failures.lock().unwrap().push(failure);
                    }
                }
            });
        }
    });
    let _ = std::fs::remove_dir_all(&scratch);
    let failures = failures.into_inner().unwrap();
    assert!(
        failures.is_empty(),
        "\n{}\n{} failed",
        failures.join("\n"),
        failures.len()
    );
}

#[test]
fn ui() {
    let mut tests = Vec::new();
    sources(
        &root().join("tests/ui"),
        0,
        |name| name == "main.zr",
        &mut tests,
    );
    tests.sort();
    let bless = std::env::var_os("BLESS").is_some();

    run_all(&tests, |source, scratch| {
        let name = source.strip_prefix(root()).unwrap().display().to_string();
        let snapshot = source.with_extension("out");
        let checked = std::fs::read_to_string(source)
            .unwrap()
            .starts_with("//@ checked");
        let mut failures = String::new();
        for mode in MODES {
            if checked && mode == "--release-fast" {
                continue;
            }
            let actual = observe(source, mode, scratch);
            if bless && mode.is_empty() {
                std::fs::write(&snapshot, &actual).unwrap();
            }
            let expected = std::fs::read_to_string(&snapshot).unwrap_or_default();
            if actual != expected {
                let mode = if mode.is_empty() { "debug" } else { mode };
                failures.push_str(&format!("{name} ({mode})\n{}", diff(&expected, &actual)));
            }
        }
        (!failures.is_empty()).then_some(failures)
    });
}

/// Each example, run in every mode, must exit with 0; a directory's program
/// is its `main.zr`.
#[test]
fn examples() {
    let mut programs = Vec::new();
    sources(
        &root().join("examples"),
        1,
        |name| name == "main.zr",
        &mut programs,
    );
    programs.sort();

    run_all(&programs, |source, scratch| {
        let name = source.strip_prefix(root()).unwrap().display();
        MODES.iter().find_map(|mode| {
            let seen = observe(source, mode, scratch);
            (!seen.starts_with("status: exit 0\n")).then(|| format!("{name} {mode}\n{seen}"))
        })
    });
}

fn files(dir: &Path, found: &mut Vec<PathBuf>) {
    for path in std::fs::read_dir(dir).unwrap().map(|e| e.unwrap().path()) {
        if path.is_dir() {
            files(&path, found);
        } else {
            found.push(path);
        }
    }
}

struct Block {
    name: String,
    tag: String,
    code: String,
    text: Option<String>,
}

/// The ```zeru blocks of the book's pages and of std's doc comments.
fn blocks() -> Vec<Block> {
    let mut paths = Vec::new();
    files(&root().join("docs/src"), &mut paths);
    files(&root().join("std"), &mut paths);
    paths.sort();
    let mut blocks = Vec::new();
    for path in paths {
        let doc = match path.extension().and_then(|e| e.to_str()) {
            Some("md") => false,
            Some("zr") => true,
            _ => continue,
        };
        let text = std::fs::read_to_string(&path).unwrap();
        let lines: Vec<&str> = text
            .lines()
            .map(|line| match doc {
                false => line,
                true => line
                    .trim_start()
                    .strip_prefix("//")
                    .map_or("", |l| l.strip_prefix(' ').unwrap_or(l)),
            })
            .collect();
        let fence = |start: usize| {
            let end = start + lines[start..].iter().position(|l| *l == "```").unwrap();
            (lines[start..end].join("\n"), end + 1)
        };
        let mut i = 0;
        while i < lines.len() {
            let Some(tag) = lines[i].strip_prefix("```zeru") else {
                i += 1;
                continue;
            };
            let name = format!("{}:{}", path.strip_prefix(root()).unwrap().display(), i + 1);
            let (code, end) = fence(i + 1);
            i = end;
            let next = i + lines[i..].iter().take_while(|l| l.is_empty()).count();
            let text = (lines.get(next) == Some(&"```text")).then(|| {
                let (text, end) = fence(next + 1);
                i = end;
                text
            });
            blocks.push(Block {
                name,
                tag: tag.to_string(),
                code,
                text,
            });
        }
    }
    blocks
}

/// Whether `wanted` shows up in `seen`'s lines in order, each inside one line.
fn shows(seen: &str, wanted: &[&str]) -> bool {
    let mut lines = seen.lines();
    wanted
        .iter()
        .all(|want| lines.any(|line| line.contains(want)))
}

/// Every example in the book and in std's docs: a `zeru` block runs, wrapped
/// in `main` when it has none, and prints each `); // X` it shows, or the
/// ```text block after it; `zeru,check` only compiles; `zeru,fails` must not
/// exit with 0; `zeru,ignore` is skipped.
#[test]
fn docs() {
    let blocks: Vec<Block> = blocks()
        .into_iter()
        .filter(|b| b.tag != ",ignore")
        .collect();
    run_all(&blocks, |block, scratch| {
        let _ = std::fs::remove_dir_all(scratch);
        std::fs::create_dir_all(scratch).unwrap();
        let source = if block.code.contains("fn main(") {
            block.code.clone()
        } else {
            let (imports, body): (Vec<&str>, Vec<&str>) =
                block.code.lines().partition(|l| l.starts_with("import "));
            format!(
                "{}\nfn main() {{\n{}\n}}\n",
                imports.join("\n"),
                body.join("\n")
            )
        };
        std::fs::write(scratch.join("main.zr"), &source).unwrap();
        if block.tag == ",check" {
            let output = Command::new(env!("CARGO_BIN_EXE_zeru"))
                .args(["check", "main.zr"])
                .current_dir(scratch)
                .env("ZERU_STD_PATH", root().join("std"))
                .output()
                .unwrap();
            let stderr = clean(&String::from_utf8_lossy(&output.stderr));
            return (!output.status.success()).then(|| format!("{}\n{stderr}", block.name));
        }
        let seen = observe(Path::new("main.zr"), "", scratch);
        let exited = seen.starts_with("status: exit 0\n");
        let text: Option<Vec<&str>> = block
            .text
            .as_ref()
            .map(|text| text.lines().filter(|l| !l.is_empty()).collect());
        let passed = match (block.tag.as_str(), text) {
            (",fails", text) => !exited && text.is_none_or(|text| shows(&seen, &text)),
            (_, Some(text)) => shows(&seen, &text),
            _ => {
                let wanted: Vec<&str> = block
                    .code
                    .lines()
                    .filter_map(|l| l.split_once("); // ").or_else(|| l.split_once("), // ")))
                    .map(|(_, out)| out)
                    .collect();
                exited && shows(&seen, &wanted)
            }
        };
        (!passed).then(|| format!("{}\n{source}\n{seen}", block.name))
    });
}

#[test]
fn check_reports_json() {
    let source = root().join("tests/ui/references/change_while_referenced.zr");
    let output = Command::new(env!("CARGO_BIN_EXE_zeru"))
        .args(["check", "--json"])
        .arg(&source)
        .current_dir(source.parent().unwrap())
        .env("ZERU_STD_PATH", root().join("std"))
        .output()
        .unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(!output.status.success());
    assert!(stdout.starts_with("{\"file\":"), "{stdout}");
    assert!(
        stdout.contains("\"line\":") && stdout.contains("\"notes\":[{"),
        "{stdout}"
    );
    assert!(!root().join("tests/ui/references/build").exists());
}

#[test]
fn check_warns_without_failing() {
    let output = Command::new(env!("CARGO_BIN_EXE_zeru"))
        .args(["check", "--json"])
        .arg(root().join("tests/cli/warnings/lints.zr"))
        .env("ZERU_STD_PATH", root().join("std"))
        .output()
        .unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(output.status.success(), "{stdout}");
    let lines: Vec<&str> = stdout
        .lines()
        .map(|line| {
            assert!(line.contains("\"severity\":\"warning\""), "{line}");
            let at = line.find("\"line\":").unwrap() + 7;
            &line[at..at + line[at..].find(',').unwrap()]
        })
        .collect();
    assert_eq!(
        lines,
        [
            "1", "4", "8", "20", "25", "26", "27", "28", "30", "31", "32", "33", "35", "38", "44"
        ],
        "{stdout}"
    );
}

#[test]
fn test_runs_each_test_alone() {
    let scratch = std::env::temp_dir().join(format!("zeru-test-{}", std::process::id()));
    std::fs::create_dir_all(&scratch).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_zeru"))
        .arg("test")
        .arg(root().join("tests/cli/unit_tests/main.zr"))
        .current_dir(&scratch)
        .env("ZERU_STD_PATH", root().join("std"))
        .output()
        .unwrap();
    let _ = std::fs::remove_dir_all(&scratch);
    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(!output.status.success());
    let [passes, failures] = ["[PASS]", "[FAIL]"].map(|tag| {
        stdout
            .lines()
            .filter_map(|line| line.strip_prefix(tag))
            .map(|line| line.rsplit_once('/').map_or(line, |(_, file)| file))
            .collect::<Vec<_>>()
    });
    assert_eq!(
        passes,
        [
            "main.zr:L14 add sums",
            "main.zr:L27 helpers live in the block",
            "shapes.zr:L6 area multiplies"
        ],
        "{stdout}"
    );
    assert_eq!(
        failures,
        [
            "main.zr:L18 add fails on purpose",
            "main.zr:L23 an error fails the test"
        ],
        "{stdout}"
    );
    assert!(
        stdout.contains("       printed before failing\n"),
        "{stdout}"
    );
    assert!(stdout.contains("3 passed, 2 failed in "), "{stdout}");
}
