#!/bin/bash
# Zeru Compilation Benchmark
# Measures compilation time vs Lines of Code (LOC)

set -e

ZERU_BIN="${ZERU_BIN:-./target/release/zeru}"
BUILD_DIR="./build"

GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}                    Zeru Compilation Benchmark                  ${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""

if [ ! -f "$ZERU_BIN" ]; then
    echo "Building Zeru in release mode..."
    cargo build --release
fi

mkdir -p "$BUILD_DIR"

ZR_FILES=$(find . -name "*.zr" 2>/dev/null | sort)

if [ -z "$ZR_FILES" ]; then
    echo "No .zr files found in current directory"
    exit 1
fi

echo -e "${YELLOW}File                          LOC      Time (ms)    LOC/ms${NC}"
echo "─────────────────────────────────────────────────────────────"

total_loc=0
total_time_ms=0
file_count=0

for file in $ZR_FILES; do
    filename=$(basename "$file" .zr)

    loc=$(grep -v '^\s*$' "$file" | grep -v '^\s*//' | wc -l)

    total_run_time=0
    runs=100

    for i in $(seq 1 $runs); do
        start_time=$(date +%s%N)
        $ZERU_BIN "$file" -o "$BUILD_DIR/$filename" 2>/dev/null || true
        end_time=$(date +%s%N)

        run_time=$(((end_time - start_time) / 1000000))
        total_run_time=$((total_run_time + run_time))
    done

    avg_time_ms=$((total_run_time / runs))

    if [ "$avg_time_ms" -gt 0 ]; then
        loc_per_ms=$(awk "BEGIN {printf \"%.2f\", $loc / $avg_time_ms}")
    else
        loc_per_ms="∞"
    fi

    printf "%-30s %4d     %8d      %s\n" "$filename.zr" "$loc" "$avg_time_ms" "$loc_per_ms"

    total_loc=$((total_loc + loc))
    total_time_ms=$((total_time_ms + avg_time_ms))
    file_count=$((file_count + 1))
done

echo "─────────────────────────────────────────────────────────────"

if [ "$total_time_ms" -gt 0 ]; then
    total_loc_per_ms=$(awk "BEGIN {printf \"%.2f\", $total_loc / $total_time_ms}")
else
    total_loc_per_ms="∞"
fi

avg_time=$((total_time_ms / file_count))

echo ""
echo -e "${GREEN}Summary:${NC}"
echo "  Total files:        $file_count"
echo "  Total LOC:          $total_loc"
echo "  Total time:         ${total_time_ms}ms"
echo "  Average time/file:  ${avg_time}ms"
echo "  Throughput:         ${total_loc_per_ms} LOC/ms"
echo ""

echo -e "${BLUE}System Info:${NC}"
echo "  CPU: $(lscpu | grep 'Model name' | cut -d ':' -f2 | xargs)"
echo "  Zeru version: $($ZERU_BIN --version 2>/dev/null || echo 'unknown')"
echo ""
