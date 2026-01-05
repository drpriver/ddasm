#!/usr/bin/env python3
"""
ABI Test Runner

Compiles the native C library and runs the ddasm test against it.
"""

import subprocess
import sys
import os
import platform

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

    print("ABI Test Runner")
    print("=" * 60)

    # Step 1: Compile the native library
    print(f"\n[1] Compiling native library: {lib_name}")
    compile_cmd = ["gcc", "-O2"] + lib_flags + ["-o", lib_path, c_source, "-lm"]
    print(f"    Command: {' '.join(compile_cmd)}")

    result = subprocess.run(compile_cmd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"    FAILED to compile native library:")
        print(result.stderr)
        return 1
    print("    OK")

    # Step 2: Run the ddasm test
    print(f"\n[2] Running ddasm test: test_abi.c")

    # Set up environment with library path
    env = os.environ.copy()
    if platform.system() == "Darwin":
        env["DYLD_LIBRARY_PATH"] = script_dir + ":" + env.get("DYLD_LIBRARY_PATH", "")
    else:
        env["LD_LIBRARY_PATH"] = script_dir + ":" + env.get("LD_LIBRARY_PATH", "")

    run_cmd = [ddasm, test_source, "-L", script_dir]
    print(f"    Command: {' '.join(run_cmd)}")
    print(f"    Library path: {script_dir}")
    print()
    print("-" * 60)

    result = subprocess.run(run_cmd, capture_output=False)

    print("-" * 60)

    if result.returncode != 0:
        print(f"\n[FAIL] Test returned exit code {result.returncode}")
        return 1

    print("\n[PASS] All ABI tests passed!")
    return 0

if __name__ == "__main__":
    sys.exit(main())
