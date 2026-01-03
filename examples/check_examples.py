#!/usr/bin/env python3
import os
import subprocess
import glob

def check_examples():
    compiler_path = "../target/release/zeru"
    examples_dir = "./"

    if not os.path.exists(compiler_path):
        print("Compiler not found. Building it first")
        subprocess.run(["cargo", "build", "--release"], check=True)

    zr_files = sorted(glob.glob(os.path.join(examples_dir, "*.zr")))

    passed = []
    failed = []

    for file_path in zr_files:
        file_name = os.path.basename(file_path)
        print(f"Testing {file_name}...", end=" ", flush=True)

        try:
            result = subprocess.run(
                [compiler_path, "build", file_path],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )

            if result.returncode == 0:
                print("✅ PASS")
                passed.append(file_name)
            else:
                print("❌ FAILED")
                failed.append((file_name, result.stderr))
        except Exception as e:
            print(f"❌ ERROR: {e}")
            failed.append((file_name, str(e)))
    print("\n" + "=" * 40)
    print(f"Total: {len(zr_files)}")
    print(f"Passed: {len(passed)}")
    print(f"Failed: {len(failed)}")

    if failed:
        print("\nFailed files:")
        for name, error in failed:
            print(f"\n❌{name}:")
            print("\t\n".join(error.strip().splitlines()))

check_examples()
