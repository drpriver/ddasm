#!/usr/bin/env python3
"""
ABI Test Runner

Compiles the native C library and runs the ddasm test against it.
"""

import subprocess
import sys
import os
import platform

# Colors for terminal output
RED = '\033[0;31m'
GREEN = '\033[0;32m'
YELLOW = '\033[0;33m'
NC = '\033[0m'  # No Color

def main():
    # Get the directory of this script
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_root = os.path.dirname(os.path.dirname(script_dir))

    # Paths
    c_source = os.path.join(script_dir, "abi_test.c")
    test_source = os.path.join(script_dir, "test_abi.c")
    ddasm = os.path.join(project_root, "Bin", "ddasm")

    # Platform-specific library name
    if platform.system() == "Darwin":
        lib_name = "libabi_test.dylib"
        lib_flags = ["-dynamiclib"]
    else:
        lib_name = "libabi_test.so"
        lib_flags = ["-shared", "-fPIC"]

    lib_path = os.path.join(script_dir, lib_name)

    print("Running ABI native tests...")
    print("=" * 25)
    print()

    # Step 1: Compile the native library
    compile_cmd = ["gcc", "-O2"] + lib_flags + ["-o", lib_path, c_source, "-lm"]

    result = subprocess.run(compile_cmd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"{RED}FAIL{NC}: compile native library ({lib_name})")
        print(f"      {result.stderr}")
        return 1
    print(f"{GREEN}PASS{NC}: compile native library ({lib_name})")

    # Step 2: Run the ddasm test

    run_cmd = [ddasm, test_source, "-L", script_dir]

    result = subprocess.run(run_cmd, capture_output=True, text=True)

    if result.returncode != 0:
        print(f"{RED}FAIL{NC}: run test_abi.c (exit code {result.returncode})")
        if result.stdout:
            for line in result.stdout.strip().split('\n')[:5]:
                print(f"      {line}")
        if result.stderr:
            for line in result.stderr.strip().split('\n')[:5]:
                print(f"      {line}")
        return 1

    print(f"{GREEN}PASS{NC}: run test_abi.c")

    print()
    print("=" * 25)
    print(f"Results: {GREEN}2 passed{NC}, 0 failed, 0 skipped")
    return 0

if __name__ == "__main__":
    sys.exit(main())
