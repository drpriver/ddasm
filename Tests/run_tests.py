#!/usr/bin/env python3
"""Simple test runner for C parser tests and DASM tests.
Runs c2dasm on each .c file and ddasm on each .dasm file in Tests/.
"""

import os
import subprocess
import sys
from pathlib import Path

# Colors for terminal output
RED = '\033[0;31m'
GREEN = '\033[0;32m'
YELLOW = '\033[0;33m'
NC = '\033[0m'  # No Color


def main():
    script_dir = Path(__file__).parent.resolve()
    project_dir = script_dir.parent
    c2dasm = project_dir / 'Bin' / 'c2dasm'
    ddasm = project_dir / 'Bin' / 'ddasm'

    # Check if c2dasm exists
    if not c2dasm.exists() or not os.access(c2dasm, os.X_OK):
        print(f"{RED}Error: c2dasm not found at {c2dasm}{NC}")
        print("Run 'make c2dasm' first")
        sys.exit(1)

    # Check if ddasm exists
    if not ddasm.exists() or not os.access(ddasm, os.X_OK):
        print(f"{RED}Error: ddasm not found at {ddasm}{NC}")
        print("Run 'make ddasm' first")
        sys.exit(1)

    pass_count = 0
    fail_count = 0
    skip_count = 0

    # Run C parser tests
    print("Running C parser tests...")
    print("=========================")
    print()

    test_files = sorted(script_dir.glob('*.c'))

    for test_file in test_files:
        filename = test_file.name

        # Check for SKIP marker in file
        with open(test_file, 'r') as f:
            first_line = f.readline()
            if '// SKIP' in first_line:
                print(f"{YELLOW}SKIP{NC}: {filename}")
                skip_count += 1
                continue

        # Run c2dasm and capture output
        result = subprocess.run(
            [str(c2dasm), str(test_file)],
            capture_output=True,
            text=True
        )

        if result.returncode == 0:
            print(f"{GREEN}PASS{NC}: {filename}")
            pass_count += 1
        else:
            print(f"{RED}FAIL{NC}: {filename}")
            # Show first 5 lines of error output
            error_output = result.stderr or result.stdout
            error_lines = error_output.strip().split('\n')[:5]
            for line in error_lines:
                print(f"      {line}")
            fail_count += 1

    # Run DASM tests
    print()
    print("Running DASM tests...")
    print("=========================")
    print()

    dasm_files = sorted(script_dir.glob('*.dasm'))

    for test_file in dasm_files:
        filename = test_file.name
        expected_file = test_file.with_suffix('.expected')

        # Check for SKIP marker in file
        with open(test_file, 'r') as f:
            first_line = f.readline()
            if '# SKIP' in first_line:
                print(f"{YELLOW}SKIP{NC}: {filename}")
                skip_count += 1
                continue

        # Run ddasm and capture output
        result = subprocess.run(
            [str(ddasm), str(test_file)],
            capture_output=True,
            text=True
        )

        actual_output = result.stdout.rstrip('\n')
        if result.stderr:
            actual_output = result.stderr.rstrip('\n')

        # If there's an expected file, compare output
        if expected_file.exists():
            with open(expected_file, 'r') as f:
                expected_output = f.read().rstrip('\n')

            if actual_output == expected_output:
                print(f"{GREEN}PASS{NC}: {filename}")
                pass_count += 1
            else:
                print(f"{RED}FAIL{NC}: {filename}")
                print(f"      Expected: {expected_output}")
                print(f"      Actual:   {actual_output}")
                fail_count += 1
        else:
            # No expected file - just check that it runs without error
            if result.returncode == 0:
                print(f"{GREEN}PASS{NC}: {filename} (no .expected file, checked execution only)")
                pass_count += 1
            else:
                print(f"{RED}FAIL{NC}: {filename}")
                error_lines = actual_output.split('\n')[:5]
                for line in error_lines:
                    print(f"      {line}")
                fail_count += 1

    print()
    print("=========================")
    print(f"Results: {GREEN}{pass_count} passed{NC}, {RED}{fail_count} failed{NC}, {YELLOW}{skip_count} skipped{NC}")

    if fail_count > 0:
        sys.exit(1)
    sys.exit(0)


if __name__ == '__main__':
    main()
