#!/usr/bin/env python3
"""Compile every Zeru source in the repo and run the self-checking examples.

Used by CI, and runnable by hand:

    python3 ci/compile_suite.py                  # debug
    python3 ci/compile_suite.py --release-safe   # -O2 with safety checks
    python3 ci/compile_suite.py --release-fast   # -O3, checks stripped

ci/reject holds programs that must NOT compile. Each names the complaint it
expects on its first line, so a diagnostic that stops being reported, or starts
saying something else, fails the suite instead of passing quietly.

Exits non-zero when anything fails, so a red suite is actually red.
"""

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
COMPILER = ROOT / "target" / "release" / "zeru"

GREEN, RED, YELLOW, RESET = "\033[32m", "\033[31m", "\033[33m", "\033[0m"


def run(args, cwd):
    return subprocess.run(
        args,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )


class Suite:
    def __init__(self, mode_flags, workdir):
        self.mode_flags = mode_flags
        self.workdir = workdir
        self.failures = []

    def check(self, label, ok, detail=""):
        print(f"{GREEN}ok{RESET}  {label}" if ok else f"{RED}FAIL{RESET}  {label}")
        if not ok:
            self.failures.append((label, detail))
        return ok

    def compile(self, source):
        """Compile `source` and confirm an executable actually came out."""
        result = run([COMPILER, "build", source] + self.mode_flags, cwd=self.workdir)
        if result.returncode != 0:
            return self.check(f"compile {source.name}", False, result.stdout)
        # A linker that failed while the compiler reported success would
        # otherwise slip through as a pass.
        exe = self.workdir / "build" / source.stem
        return self.check(f"compile {source.name}", exe.is_file(), "no executable produced")

    def reject(self, source):
        """Require `source` to be refused, with the message it says it expects."""
        expected = source.read_text().partition("\n")[0].removeprefix("// expect:").strip()
        result = run([COMPILER, "build", source] + self.mode_flags, cwd=self.workdir)

        if result.returncode == 0:
            self.check(f"reject  {source.name}", False, "compiled, but should not")
        else:
            self.check(
                f"reject  {source.name}",
                expected in result.stdout,
                f"expected {expected!r}, got:\n{result.stdout}",
            )

    def run_example(self, source):
        """Run what compile() built and require a zero exit: examples self-check via exit codes."""
        result = run([self.workdir / "build" / source.stem], cwd=self.workdir)
        self.check(
            f"run     {source.name}",
            result.returncode == 0,
            f"exit {result.returncode}\n{result.stdout}",
        )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--release-safe", action="store_true")
    group.add_argument("--release-fast", action="store_true")
    args = parser.parse_args()

    mode_flags = []
    if args.release_safe:
        mode_flags = ["--release-safe"]
    elif args.release_fast:
        mode_flags = ["--release-fast"]

    if not COMPILER.is_file():
        print(f"{YELLOW}Compiler not built, building it first{RESET}")
        subprocess.run(["cargo", "build", "--release"], cwd=ROOT, check=True)

    # The compiler looks for std under ~/.zeru unless told otherwise, and CI
    # must test the tree it checked out.
    os.environ["ZERU_STD_PATH"] = str(ROOT / "std")

    mode = mode_flags[0] if mode_flags else "--debug"
    print(f"Zeru compile suite ({mode})\n")

    workdir = Path(tempfile.mkdtemp(prefix="zeru-suite-"))
    try:
        suite = Suite(mode_flags, workdir)

        # Top-level examples, plus the entry point of each multi-file project.
        # A project's other files have no `main` and are reached by import.
        examples = sorted((ROOT / "examples").glob("*.zr")) + sorted(
            (ROOT / "examples").glob("*/*_test.zr")
        )
        for example in examples:
            if suite.compile(example):
                suite.run_example(example)

        # A rejection comes out of the front end, which the safety mode does not
        # reach, so checking it once is checking it everywhere.
        if not mode_flags:
            for source in sorted((ROOT / "ci" / "reject").glob("*.zr")):
                suite.reject(source)

        print(f"\n{'=' * 50}")
        if suite.failures:
            print(f"{RED}{len(suite.failures)} failed{RESET}\n")
            for label, detail in suite.failures:
                print(f"{RED}FAIL{RESET} {label}")
                print(detail.strip() + "\n")
            return 1

        print(f"{GREEN}all passed{RESET}")
        return 0
    finally:
        shutil.rmtree(workdir, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main())
