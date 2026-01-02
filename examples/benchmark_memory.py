#!/usr/bin/env python3
"""
Zeru Compiler Memory & Performance Benchmark

Measures memory usage and time for each compilation phase:
- Lexing
- Parsing  
- Semantic Analysis
- Code Generation
- LLVM Optimization + Linking

This script uses the generated benchmark code from generate_benchmark.py
to stress-test the compiler at various scales.

Usage:
    python benchmark_memory.py                    # Run default suite
    python benchmark_memory.py --file large.zr   # Benchmark specific file
    python benchmark_memory.py --scale 100 1000 10000 100000  # Test at specific LOC
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass, asdict
from datetime import datetime
from pathlib import Path
from typing import List, Optional, Dict, Any

SCRIPT_DIR = Path(__file__).parent.absolute()
PROJECT_ROOT = SCRIPT_DIR.parent

ZERU_BIN = os.environ.get("ZERU_BIN", str(PROJECT_ROOT / "target/release/zeru"))
BUILD_DIR = PROJECT_ROOT / "build"
BENCHMARK_SCRIPT = SCRIPT_DIR / "generate_benchmark.py"

DEFAULT_SCALES = [100, 500, 1000, 5000, 10000, 50000]

class Colors:
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    CYAN = '\033[96m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

@dataclass
class PhaseMetrics:
    """Metrics for a single compilation phase."""
    name: str
    time_ms: float
    memory_kb: Optional[int] = None

@dataclass  
class BenchmarkResult:
    """Complete benchmark result for a single run."""
    file_path: str
    lines_of_code: int
    total_time_ms: float
    peak_memory_kb: int
    phases: List[PhaseMetrics]
    success: bool
    error_message: Optional[str] = None

@dataclass
class BenchmarkSuite:
    """Collection of benchmark results."""
    timestamp: str
    system_info: Dict[str, str]
    zeru_version: str
    results: List[BenchmarkResult]

def get_system_info() -> Dict[str, str]:
    """Gather system information for reproducibility."""
    info = {}

    try:
        info["os"] = subprocess.check_output(
            ["uname", "-s"], text=True
        ).strip()
        info["kernel"] = subprocess.check_output(
            ["uname", "-r"], text=True
        ).strip()
    except:
        info["os"] = sys.platform

    try:
        cpu_info = subprocess.check_output(
            ["lscpu"], text=True
        )
        for line in cpu_info.split("\n"):
            if "Model name" in line:
                info["cpu"] = line.split(":")[1].strip()
                break
    except:
        info["cpu"] = "unknown"

    try:
        mem_info = subprocess.check_output(
            ["free", "-h"], text=True
        )
        for line in mem_info.split("\n"):
            if "Mem:" in line:
                info["memory"] = line.split()[1]
                break
    except:
        info["memory"] = "unknown"

    return info

def get_zeru_version() -> str:
    """Get Zeru compiler version."""
    try:
        result = subprocess.run(
            [ZERU_BIN, "--version"],
            capture_output=True,
            text=True,
            timeout=5
        )
        return result.stdout.strip() or result.stderr.strip() or "unknown"
    except:
        return "unknown"

def count_lines(file_path: Path) -> int:
    """Count non-empty, non-comment lines in a file."""
    count = 0
    with open(file_path) as f:
        for line in f:
            stripped = line.strip()
            if stripped and not stripped.startswith("//"):
                count += 1
    return count

def measure_peak_memory(command: List[str]) -> tuple:
    """
    Run a command and measure its peak memory usage and execution time.
    Returns (exit_code, time_ms, peak_memory_kb, stdout, stderr).
    """
    import resource

    start = time.perf_counter()
    try:
        result = subprocess.run(
            command,
            capture_output=True,
            text=True,
            timeout=300
        )
        elapsed_ms = (time.perf_counter() - start) * 1000

        peak_memory_kb = 0
        try:
            time_result = subprocess.run(
                ["/usr/bin/time", "-v"] + command,
                capture_output=True,
                text=True,
                timeout=300
            )
            for line in time_result.stderr.split("\n"):
                if "Maximum resident set size" in line:
                    peak_memory_kb = int(line.split(":")[1].strip())
                    break
        except FileNotFoundError:
            peak_memory_kb = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss

        return (
            result.returncode,
            elapsed_ms,
            peak_memory_kb,
            result.stdout,
            result.stderr
        )
    except subprocess.TimeoutExpired:
        return (-1, 300000, 0, "", "Timeout")
    except Exception as e:
        return (-1, 0, 0, "", str(e))

def run_benchmark(file_path: Path, runs: int = 3) -> BenchmarkResult:
    """
    Run the Zeru compiler on a file and collect metrics.
    Performs multiple runs and averages the results.
    """
    loc = count_lines(file_path)

    all_times = []
    all_memory = []
    last_error = None

    BUILD_DIR.mkdir(exist_ok=True)
    output_name = file_path.stem

    for run in range(runs):
        exit_code, time_ms, memory_kb, stdout, stderr = measure_peak_memory([
            ZERU_BIN, "build", str(file_path)
        ])

        if exit_code == 0:
            all_times.append(time_ms)
            all_memory.append(memory_kb)
        else:
            last_error = stderr or "Unknown error"

    if not all_times:
        return BenchmarkResult(
            file_path=str(file_path),
            lines_of_code=loc,
            total_time_ms=0,
            peak_memory_kb=0,
            phases=[],
            success=False,
            error_message=last_error
        )

    avg_time = sum(all_times) / len(all_times)
    max_memory = max(all_memory)

    phases = [
        PhaseMetrics(name="total_compilation", time_ms=avg_time, memory_kb=max_memory)
    ]
 
    return BenchmarkResult(
        file_path=str(file_path),
        lines_of_code=loc,
        total_time_ms=avg_time,
        peak_memory_kb=max_memory,
        phases=phases,
        success=True
    )

def generate_test_file(lines: int, seed: int = 42) -> Path:
    """Generate a benchmark file with the specified number of lines."""
    output_path = BUILD_DIR / f"bench_{lines}loc.zr"

    subprocess.run([
        sys.executable,
        str(BENCHMARK_SCRIPT),
        "--lines", str(lines),
        "--seed", str(seed),
        "--output", str(output_path)
    ], check=True)

    return output_path

def format_memory(kb: int) -> str:
    """Format memory in human-readable form."""
    if kb < 1024:
        return f"{kb} KB"
    elif kb < 1024 * 1024:
        return f"{kb / 1024:.1f} MB"
    else:
        return f"{kb / (1024 * 1024):.2f} GB"

def format_time(ms: float) -> str:
    """Format time in human-readable form."""
    if ms < 1000:
        return f"{ms:.1f} ms"
    else:
        return f"{ms / 1000:.2f} s"

def print_result(result: BenchmarkResult):
    """Print a single benchmark result."""
    status = f"{Colors.GREEN}✓{Colors.ENDC}" if result.success else f"{Colors.RED}✗{Colors.ENDC}"

    print(f"  {status} {result.file_path}")
    print(f"      LOC: {result.lines_of_code:,}")

    if result.success:
        print(f"      Time: {format_time(result.total_time_ms)}")
        print(f"      Memory: {format_memory(result.peak_memory_kb)}")

        if result.lines_of_code > 0 and result.total_time_ms > 0:
            loc_per_ms = result.lines_of_code / result.total_time_ms
            bytes_per_loc = (result.peak_memory_kb * 1024) / result.lines_of_code
            print(f"      Throughput: {loc_per_ms:.1f} LOC/ms")
            print(f"      Memory/LOC: {bytes_per_loc:.1f} bytes")
    else:
        print(f"      {Colors.RED}Error: {result.error_message}{Colors.ENDC}")
    print()

def print_summary(results: List[BenchmarkResult]):
    """Print summary statistics."""
    successful = [r for r in results if r.success]

    if not successful:
        print(f"{Colors.RED}No successful benchmark runs!{Colors.ENDC}")
        return

    print(f"\n{Colors.BOLD}{'='*70}{Colors.ENDC}")
    print(f"{Colors.BOLD}SUMMARY{Colors.ENDC}")
    print(f"{'='*70}")
    print()


    print(f"{'LOC':>10} {'Time':>12} {'Memory':>12} {'LOC/ms':>10} {'Bytes/LOC':>12}")
    print("-" * 60)

    for r in sorted(successful, key=lambda x: x.lines_of_code):
        loc_per_ms = r.lines_of_code / r.total_time_ms if r.total_time_ms > 0 else 0
        bytes_per_loc = (r.peak_memory_kb * 1024) / r.lines_of_code if r.lines_of_code > 0 else 0

        print(f"{r.lines_of_code:>10,} {format_time(r.total_time_ms):>12} "
              f"{format_memory(r.peak_memory_kb):>12} {loc_per_ms:>10.1f} {bytes_per_loc:>12.1f}")

    print()


    if len(successful) >= 2:
        sorted_results = sorted(successful, key=lambda x: x.lines_of_code)
        first, last = sorted_results[0], sorted_results[-1]

        loc_ratio = last.lines_of_code / first.lines_of_code
        time_ratio = last.total_time_ms / first.total_time_ms if first.total_time_ms > 0 else 0
        mem_ratio = last.peak_memory_kb / first.peak_memory_kb if first.peak_memory_kb > 0 else 0

        print(f"Scaling Analysis ({first.lines_of_code:,} -> {last.lines_of_code:,} LOC):")
        print(f"  LOC increased: {loc_ratio:.1f}x")
        print(f"  Time increased: {time_ratio:.1f}x")
        print(f"  Memory increased: {mem_ratio:.1f}x")


        import math
        if time_ratio > 0 and loc_ratio > 1:
            time_exponent = math.log(time_ratio) / math.log(loc_ratio)
            complexity = "O(n)" if time_exponent < 1.2 else \
                        "O(n log n)" if time_exponent < 1.5 else \
                        "O(n²)" if time_exponent < 2.2 else "O(n³+)"
            print(f"  Estimated time complexity: ~{complexity} (exponent: {time_exponent:.2f})")

def save_results(suite: BenchmarkSuite, output_path: Path):
    """Save results to JSON file."""
    data = {
        "timestamp": suite.timestamp,
        "system_info": suite.system_info,
        "zeru_version": suite.zeru_version,
        "results": [asdict(r) for r in suite.results]
    }

    with open(output_path, "w") as f:
        json.dump(data, f, indent=2)

    print(f"\nResults saved to: {output_path}")

def main():
    parser = argparse.ArgumentParser(
        description="Zeru Compiler Memory & Performance Benchmark"
    )
    parser.add_argument(
        "--file", "-f",
        type=Path,
        help="Benchmark a specific .zr file"
    )
    parser.add_argument(
        "--scale", "-s",
        type=int,
        nargs="+",
        default=None,
        help="Lines of code to test (e.g., --scale 100 1000 10000)"
    )
    parser.add_argument(
        "--runs", "-r",
        type=int,
        default=3,
        help="Number of runs per benchmark (default: 3)"
    )
    parser.add_argument(
        "--output", "-o",
        type=Path,
        default=None,
        help="Save results to JSON file"
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed for code generation"
    )

    args = parser.parse_args()


    if not Path(ZERU_BIN).exists():
        print("Building Zeru in release mode...")
        subprocess.run(["cargo", "build", "--release"], cwd=PROJECT_ROOT, check=True)

    BUILD_DIR.mkdir(exist_ok=True)


    print(f"\n{Colors.BOLD}{'='*70}{Colors.ENDC}")
    print(f"{Colors.BOLD}     ZERU COMPILER BENCHMARK{Colors.ENDC}")
    print(f"{'='*70}\n")

    system_info = get_system_info()
    zeru_version = get_zeru_version()

    print(f"System: {system_info.get('os', 'unknown')} {system_info.get('kernel', '')}")
    print(f"CPU: {system_info.get('cpu', 'unknown')}")
    print(f"Memory: {system_info.get('memory', 'unknown')}")
    print(f"Zeru: {zeru_version}")
    print(f"Runs per benchmark: {args.runs}")
    print()

    results: List[BenchmarkResult] = []

    if args.file:
        print(f"Benchmarking: {args.file}")
        result = run_benchmark(args.file, runs=args.runs)
        results.append(result)
        print_result(result)
    else:
        scales = args.scale or DEFAULT_SCALES
        print(f"Running scale tests: {scales} LOC\n")

        for loc in scales:
            print(f"{Colors.CYAN}Generating {loc:,} LOC benchmark...{Colors.ENDC}")
            try:
                test_file = generate_test_file(loc, seed=args.seed)
                actual_loc = count_lines(test_file)
                print(f"  Generated: {test_file} ({actual_loc:,} actual LOC)")
                print(f"  Running benchmark...")
                result = run_benchmark(test_file, runs=args.runs)
                results.append(result)
                print_result(result)
            except Exception as e:
                print(f"{Colors.RED}  Error: {e}{Colors.ENDC}\n")

    print_summary(results)

    if args.output:
        suite = BenchmarkSuite(
            timestamp=datetime.now().isoformat(),
            system_info=system_info,
            zeru_version=zeru_version,
            results=results
        )
        save_results(suite, args.output)

if __name__ == "__main__":
    main()
