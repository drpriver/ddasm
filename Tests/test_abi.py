#!/usr/bin/env python3
"""
ABI Compatibility Test Suite

Generates C code to test sizeof, alignof, and offsetof for various types,
compiles with both system compiler (gcc) and ddasm, and compares results.
"""

import subprocess
import tempfile
import os
import sys

# Basic C types
BASIC_TYPES = [
    "char",
    "signed char",
    "unsigned char",
    "short",
    "unsigned short",
    "int",
    "unsigned int",
    "long",
    "unsigned long",
    "long long",
    "unsigned long long",
    "float",
    "double",
    "long double",
    "void*",
    "char*",
    "int*",
]

# All header types to test
# Format: (header, types, skip, category)
HEADER_TYPES = [
    # Standard C types
    ("stdint.h", [
        "int8_t", "int16_t", "int32_t", "int64_t",
        "uint8_t", "uint16_t", "uint32_t", "uint64_t",
        "intptr_t", "uintptr_t", "size_t", "ptrdiff_t",
        "intmax_t", "uintmax_t",
    ], False, "Standard C"),
    ("stddef.h", ["size_t", "ptrdiff_t", "wchar_t"], False, "Standard C"),
    ("time.h", ["time_t", "clock_t", "struct tm", "struct timespec"], False, "Standard C"),
    ("signal.h", ["sig_atomic_t"], False, "Standard C"),
    ("setjmp.h", ["jmp_buf"], False, "Standard C"),
    ("stdio.h", ["FILE", "fpos_t"], False, "Standard C"),
    ("wchar.h", ["wint_t", "mbstate_t"], False, "Standard C"),
    ("stdlib.h", ["div_t", "ldiv_t", "lldiv_t"], False, "Standard C"),
    # POSIX types
    ("sys/types.h", [
        "off_t", "ssize_t", "pid_t", "uid_t", "gid_t",
        "mode_t", "dev_t", "ino_t", "nlink_t",
    ], False, "POSIX"),
    ("pthread.h", [
        "pthread_t", "pthread_mutex_t", "pthread_cond_t", "pthread_attr_t",
    ], False, "POSIX"),
    # Network types
    ("sys/socket.h", ["socklen_t", "sa_family_t"], False, "Network"),
    ("netinet/in.h", [
        "in_port_t", "in_addr_t", "struct in_addr", "struct in6_addr",
        "struct sockaddr_in", "struct sockaddr_in6",
    ], False, "Network"),
    # File/stat types
    ("sys/stat.h", ["struct stat"], False, "File/Stat"),
    ("dirent.h", ["struct dirent"], False, "File/Stat"),
    # Signal extended types
    ("signal.h", ["sigset_t", "struct sigaction", "siginfo_t", "stack_t"], True, "Signal Extended"),  # Skip: needs _POSIX_C_SOURCE
    # Poll/Select types
    ("poll.h", ["struct pollfd", "nfds_t"], False, "Poll/Select"),
    ("sys/select.h", ["fd_set"], False, "Poll/Select"),
    ("sys/time.h", ["struct timeval"], False, "Poll/Select"),
    # Terminal types
    ("termios.h", ["struct termios", "speed_t", "tcflag_t", "cc_t"], False, "Terminal"),
    # IPC types
    ("sys/uio.h", ["struct iovec"], False, "IPC"),
    ("sys/socket.h", ["struct msghdr", "struct cmsghdr"], False, "IPC"),
    # User/Group types
    ("pwd.h", ["struct passwd"], False, "User/Group"),
    ("grp.h", ["struct group"], False, "User/Group"),
    # Resource types
    ("sys/resource.h", ["struct rusage", "struct rlimit", "rlim_t"], False, "Resource"),
    # Regex types
    ("regex.h", ["regex_t", "regmatch_t", "regoff_t"], False, "Regex"),
]

# Struct offset tests
# Format: (header, struct_type, members, skip)
STRUCT_OFFSET_TYPES = [
    # Standard C structs
    ("time.h", "struct tm", [
        "tm_sec", "tm_min", "tm_hour", "tm_mday", "tm_mon",
        "tm_year", "tm_wday", "tm_yday", "tm_isdst"
    ], False),
    ("time.h", "struct timespec", ["tv_sec", "tv_nsec"], False),
    # POSIX structs
    ("sys/stat.h", "struct stat", [
        "st_dev", "st_ino", "st_mode", "st_nlink", "st_uid", "st_gid",
        "st_rdev", "st_size", "st_blksize", "st_blocks",
    ], False),
    ("netinet/in.h", "struct sockaddr_in", ["sin_family", "sin_port", "sin_addr"], False),
    ("netinet/in.h", "struct sockaddr_in6", [
        "sin6_family", "sin6_port", "sin6_flowinfo", "sin6_addr", "sin6_scope_id",
    ], False),
    ("poll.h", "struct pollfd", ["fd", "events", "revents"], False),
    ("sys/time.h", "struct timeval", ["tv_sec", "tv_usec"], False),
    ("sys/uio.h", "struct iovec", ["iov_base", "iov_len"], False),
    ("sys/resource.h", "struct rlimit", ["rlim_cur", "rlim_max"], False),
    ("regex.h", "regmatch_t", ["rm_so", "rm_eo"], False),
    ("signal.h", "struct sigaction", ["sa_handler", "sa_mask", "sa_flags"], True),  # Skip: needs _POSIX_C_SOURCE
    ("signal.h", "stack_t", ["ss_sp", "ss_flags", "ss_size"], True),  # Skip: needs _POSIX_C_SOURCE
    ("termios.h", "struct termios", ["c_iflag", "c_oflag", "c_cflag", "c_lflag"], False),
    ("pwd.h", "struct passwd", ["pw_name", "pw_uid", "pw_gid", "pw_dir", "pw_shell"], False),
    ("grp.h", "struct group", ["gr_name", "gr_gid", "gr_mem"], False),
]


def generate_size_align_code(headers, types, feature_macro=None):
    """Generate C code to print sizeof and alignof for types."""
    code = '#pragma library("libc")\n'
    if feature_macro:
        code += f'#define {feature_macro}\n'
    code += '#include <stdio.h>\n#include <stddef.h>\n'
    for h in headers:
        code += f'#include <{h}>\n'
    code += '\nint main(void) {\n'

    for t in types:
        # Use _Alignof for C11 alignment
        safe_name = t.replace(' ', '_').replace('*', 'ptr')
        code += f'    printf("{safe_name} sizeof=%zu alignof=%zu\\n", sizeof({t}), _Alignof({t}));\n'

    code += '    return 0;\n}\n'
    return code


def generate_offsetof_code(header, struct_type, members, feature_macro=None):
    """Generate C code to print offsetof for struct members."""
    macro_def = f'#define {feature_macro}\n' if feature_macro else ''
    code = f'''#pragma library("libc")
{macro_def}#include <stdio.h>
#include <stddef.h>
#include <{header}>

int main(void) {{
'''
    safe_name = struct_type.replace(' ', '_')
    code += f'    printf("{safe_name} sizeof=%zu alignof=%zu\\n", sizeof({struct_type}), _Alignof({struct_type}));\n'

    for member in members:
        code += f'    printf("{safe_name}.{member} offset=%zu\\n", offsetof({struct_type}, {member}));\n'

    code += '    return 0;\n}\n'
    return code


def compile_and_run_gcc(code, timeout=5):
    """Compile with gcc and run, return output."""
    with tempfile.NamedTemporaryFile(mode='w', suffix='.c', delete=False) as f:
        f.write(code)
        src_path = f.name

    exe_path = src_path.replace('.c', '')

    try:
        # Compile with gcc
        result = subprocess.run(
            ['gcc', '-std=c11', '-o', exe_path, src_path],
            capture_output=True, text=True, timeout=timeout
        )
        if result.returncode != 0:
            return None, f"gcc compile error: {result.stderr}"

        # Run
        result = subprocess.run(
            [exe_path],
            capture_output=True, text=True, timeout=timeout
        )
        if result.returncode != 0:
            return None, f"gcc run error: {result.stderr}"

        return result.stdout.strip(), None
    except subprocess.TimeoutExpired:
        return None, "timeout"
    except Exception as e:
        return None, str(e)
    finally:
        if os.path.exists(src_path):
            os.unlink(src_path)
        if os.path.exists(exe_path):
            os.unlink(exe_path)


def compile_and_run_ddasm(code, c2dasm_path='./Bin/c2dasm', ddasm_path='./Bin/ddasm', timeout=10):
    """Compile with ddasm and run, return output."""
    with tempfile.NamedTemporaryFile(mode='w', suffix='.c', delete=False) as f:
        f.write(code)
        src_path = f.name

    dasm_path = src_path.replace('.c', '.dasm')

    try:
        # Compile with ddasm
        result = subprocess.run(
            [c2dasm_path, src_path],
            capture_output=True, text=True, timeout=timeout
        )
        if result.returncode != 0:
            return None, f"ddasm compile error: {result.stderr}"

        # Write dasm output
        with open(dasm_path, 'w') as f:
            f.write(result.stdout)

        # Assemble and run with ddasm
        result = subprocess.run(
            [ddasm_path, dasm_path],
            capture_output=True, text=True, timeout=timeout
        )

        return result.stdout.strip(), None
    except subprocess.TimeoutExpired:
        return None, "timeout"
    except Exception as e:
        return None, str(e)
    finally:
        if os.path.exists(src_path):
            os.unlink(src_path)
        if os.path.exists(dasm_path):
            os.unlink(dasm_path)


def parse_output(output):
    """Parse output lines into a dict of measurements."""
    results = {}
    for line in output.split('\n'):
        line = line.strip()
        if not line:
            continue
        parts = line.split()
        if len(parts) >= 2:
            name = parts[0]
            for part in parts[1:]:
                if '=' in part:
                    key, val = part.split('=', 1)
                    results[f"{name}.{key}"] = int(val)
    return results


def compare_results(gcc_results, ddasm_results):
    """Compare results and return list of differences."""
    diffs = []
    all_keys = set(gcc_results.keys()) | set(ddasm_results.keys())

    for key in sorted(all_keys):
        gcc_val = gcc_results.get(key)
        ddasm_val = ddasm_results.get(key)

        if gcc_val is None:
            diffs.append(f"  {key}: missing in gcc (ddasm={ddasm_val})")
        elif ddasm_val is None:
            diffs.append(f"  {key}: missing in ddasm (gcc={gcc_val})")
        elif gcc_val != ddasm_val:
            diffs.append(f"  {key}: gcc={gcc_val} ddasm={ddasm_val}")

    return diffs


def test_basic_types():
    """Test basic C types."""
    print("Testing basic C types...")
    code = generate_size_align_code([], BASIC_TYPES)

    gcc_out, gcc_err = compile_and_run_gcc(code)
    if gcc_err:
        print(f"  SKIP: gcc failed: {gcc_err}")
        return 0, 1

    ddasm_out, ddasm_err = compile_and_run_ddasm(code)
    if ddasm_err:
        print(f"  FAIL: ddasm failed: {ddasm_err}")
        return 0, 1

    gcc_results = parse_output(gcc_out)
    ddasm_results = parse_output(ddasm_out)
    diffs = compare_results(gcc_results, ddasm_results)

    if diffs:
        print("  FAIL: Differences found:")
        for d in diffs:
            print(d)
        return 0, 1
    else:
        print(f"  PASS: {len(gcc_results)} measurements match")
        return 1, 0


def test_header_types(header, types, name, feature_macro=None):
    """Test types from a specific header."""
    print(f"Testing {name}...")
    code = generate_size_align_code([header], types, feature_macro)

    gcc_out, gcc_err = compile_and_run_gcc(code)
    if gcc_err:
        print(f"  SKIP: gcc failed: {gcc_err}")
        return 0, 0, 1

    ddasm_out, ddasm_err = compile_and_run_ddasm(code)
    if ddasm_err:
        print(f"  FAIL: ddasm failed: {ddasm_err}")
        return 0, 1, 0

    gcc_results = parse_output(gcc_out)
    ddasm_results = parse_output(ddasm_out)
    diffs = compare_results(gcc_results, ddasm_results)

    if diffs:
        print("  FAIL: Differences found:")
        for d in diffs:
            print(d)
        return 0, 1, 0
    else:
        print(f"  PASS: {len(gcc_results)} measurements match")
        return 1, 0, 0


def test_struct_offsets(header, struct_type, members, feature_macro=None):
    """Test offsetof for struct members."""
    safe_name = struct_type.replace(' ', '_')
    print(f"Testing {safe_name} offsets...")
    code = generate_offsetof_code(header, struct_type, members, feature_macro)

    gcc_out, gcc_err = compile_and_run_gcc(code)
    if gcc_err:
        print(f"  SKIP: gcc failed: {gcc_err}")
        return 0, 0, 1

    ddasm_out, ddasm_err = compile_and_run_ddasm(code)
    if ddasm_err:
        print(f"  FAIL: ddasm failed: {ddasm_err}")
        return 0, 1, 0

    gcc_results = parse_output(gcc_out)
    ddasm_results = parse_output(ddasm_out)
    diffs = compare_results(gcc_results, ddasm_results)

    if diffs:
        print("  FAIL: Differences found:")
        for d in diffs:
            print(d)
        return 0, 1, 0
    else:
        print(f"  PASS: {len(gcc_results)} measurements match")
        return 1, 0, 0


def main():
    print("=" * 60)
    print("ABI Compatibility Test Suite")
    print("=" * 60)
    print()

    total_pass = 0
    total_fail = 0
    total_skip = 0

    # Test basic types
    p, f = test_basic_types()
    total_pass += p
    total_fail += f
    print()

    # Group header types by category
    categories = {}
    for header, types, skip, category in HEADER_TYPES:
        if category not in categories:
            categories[category] = []
        categories[category].append((header, types, skip))

    # Test header types by category
    for category, items in categories.items():
        print(f"=== {category} Types ===")
        for header, types, skip in items:
            if skip:
                print(f"Testing <{header}> types...")
                print(f"  SKIP: marked as skipped")
                total_skip += 1
            else:
                p, f, s = test_header_types(header, types, f"<{header}> types")
                total_pass += p
                total_fail += f
                total_skip += s
        print()

    # Test struct offsets
    print("=== Struct Member Offsets ===")
    for header, struct_type, members, skip in STRUCT_OFFSET_TYPES:
        if skip:
            safe_name = struct_type.replace(' ', '_')
            print(f"Testing {safe_name} offsets...")
            print(f"  SKIP: marked as skipped")
            total_skip += 1
        else:
            p, f, s = test_struct_offsets(header, struct_type, members)
            total_pass += p
            total_fail += f
            total_skip += s
    print()

    # Summary
    print("=" * 60)
    print(f"Results: {total_pass} passed, {total_fail} failed, {total_skip} skipped")
    print("=" * 60)

    return 0 if total_fail == 0 else 1


if __name__ == '__main__':
    sys.exit(main())
