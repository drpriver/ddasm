#!/bin/bash
# Simple test runner for C parser tests
# Runs c2dasm on each .c file in Tests/ and reports pass/fail

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
C2DASM="$PROJECT_DIR/Bin/c2dasm"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Counters
PASS=0
FAIL=0
SKIP=0

# Check if c2dasm exists
if [ ! -x "$C2DASM" ]; then
    echo -e "${RED}Error: c2dasm not found at $C2DASM${NC}"
    echo "Run 'make c2dasm' first"
    exit 1
fi

echo "Running C parser tests..."
echo "========================="
echo ""

# Find all .c files in Tests directory
for test_file in "$SCRIPT_DIR"/*.c; do
    if [ ! -f "$test_file" ]; then
        continue
    fi

    filename=$(basename "$test_file")

    # Check for SKIP marker in file
    if head -1 "$test_file" | grep -q "// SKIP"; then
        echo -e "${YELLOW}SKIP${NC}: $filename"
        ((SKIP++))
        continue
    fi

    # Run c2dasm and capture output
    if "$C2DASM" "$test_file" > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}: $filename"
        ((PASS++))
    else
        echo -e "${RED}FAIL${NC}: $filename"
        # Show error output
        "$C2DASM" "$test_file" 2>&1 | head -5 | sed 's/^/      /'
        ((FAIL++))
    fi
done

echo ""
echo "========================="
echo -e "Results: ${GREEN}$PASS passed${NC}, ${RED}$FAIL failed${NC}, ${YELLOW}$SKIP skipped${NC}"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
