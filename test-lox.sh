#!/bin/bash

# Run Crafting Interpreters reference tests against zlox.
# Usage: ./test-lox.sh [filter]
#
# Optional environment variables:
#   CRAFTING_INTERPRETERS - path to craftinginterpreters repo
#   ZLOX                  - path to zlox binary

set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
ZLOX="${ZLOX:-$ROOT/target/release/rlox}"
TEST_ROOT="${CRAFTING_INTERPRETERS:-/home/egr/code/craftinginterpreters}/test"
FILTER="${1:-}"

if [[ ! -x "$ZLOX" ]]; then
    echo "Error: zlox binary not found at '$ZLOX'. Run 'zig build' first." >&2
    exit 1
fi

if [[ ! -d "$TEST_ROOT" ]]; then
    echo "Error: test directory not found at '$TEST_ROOT'." >&2
    echo "Set CRAFTING_INTERPRETERS to the craftinginterpreters repo path." >&2
    exit 1
fi

export ROOT ZLOX TEST_ROOT FILTER

python3 - <<'PY'
import os
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(os.environ["ROOT"])
ZLOX = os.environ["ZLOX"]
TEST_ROOT = Path(os.environ["TEST_ROOT"])
FILTER = os.environ.get("FILTER", "")

SKIP_PREFIXES = ("test/scanning", "test/expressions")
EXPECT_RE = re.compile(r"// expect: ?(.*)")
RUNTIME_ERR_RE = re.compile(r"// expect runtime error: (.+)")
COMPILE_ERR_RE = re.compile(r"// Error")
NONTTEST_RE = re.compile(r"// nontest")
ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")


def strip_ansi(text: str) -> str:
    return ANSI_RE.sub("", text)


def classify(path: Path):
    rel = path.relative_to(TEST_ROOT.parent).as_posix()
    if "benchmark" in rel:
        return "skip"
    for prefix in SKIP_PREFIXES:
        if rel.startswith(prefix):
            return "skip"
    if FILTER and not rel.removeprefix("test/").startswith(FILTER):
        return "skip"

    text = path.read_text()
    if NONTTEST_RE.search(text):
        return "skip"

    expects = EXPECT_RE.findall(text)
    runtime = RUNTIME_ERR_RE.search(text)
    compile_err = COMPILE_ERR_RE.search(text)

    if expects:
        return ("output", expects)
    if runtime:
        return ("runtime", runtime.group(1))
    if compile_err:
        return ("compile", None)
    return "skip"


def run(path: Path):
    result = subprocess.run([ZLOX, "c", str(path)], capture_output=True, text=True)
    output = result.stdout.splitlines()
    if output and output[-1] == "":
        output = output[:-1]
    combined = strip_ansi(result.stdout + result.stderr)
    return result.returncode, output, combined.splitlines()


passed = failed = skipped = 0
failures = []

for path in sorted(TEST_ROOT.rglob("*.lox")):
    kind = classify(path)
    if kind == "skip":
        skipped += 1
        continue

    rel = path.relative_to(TEST_ROOT.parent).as_posix()
    code, output, combined = run(path)

    if kind[0] == "output":
        expects = kind[1]
        if output == expects:
            passed += 1
        else:
            failed += 1
            failures.append((rel, "output", expects, output))
    elif kind[0] == "runtime":
        message = kind[1]
        if code != 0 and any(message in line for line in combined):
            passed += 1
        else:
            failed += 1
            failures.append((rel, "runtime", message, combined[:6], code))
    elif kind[0] == "compile":
        if code != 0:
            passed += 1
        else:
            failed += 1
            failures.append((rel, "compile", "expected compile error", output, code))

print("=== zlox reference tests ===")
print(f"Passed:  {passed}")
print(f"Failed:  {failed}")
print(f"Skipped: {skipped}")
print()

if failures:
    print("Failures:")
    for rel, kind, *rest in failures:
        print(f"  {rel} [{kind}]")
        for item in rest:
            if isinstance(item, list):
                for line in item:
                    print(f"    {line}")
            else:
                print(f"    {item}")
    sys.exit(1)

print("All runnable reference tests passed.")
PY
