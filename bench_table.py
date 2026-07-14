#!/usr/bin/env python3
"""Parse ./bench_all.sh (poop) output into comparison tables.

Rows are benchmark scripts; columns are rlox, clox, and delta %.
One table is printed per poop measurement line (wall_time, peak_rss, ...).

Usage:
  ./bench_all.sh | ./bench_table.py
  ./bench_table.py results.txt
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass, field
from pathlib import Path


ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")
BENCH_HEADER_RE = re.compile(r"^-+ Benchmarking:\s*(.+?)\s*-+$")
BENCHMARK_RE = re.compile(r"^Benchmark\s+(\d+)\s+\((\d+)\s+runs\):\s*(.+)$")
# metric line: name, then mean with unit, optionally "± σ", then more columns
METRIC_RE = re.compile(
    r"^\s*"
    r"(?P<name>[a-z_]+)\s+"
    r"(?P<mean>\d+(?:\.\d+)?)\s*(?P<unit>[a-zA-Z%]+)?"
)


UNIT_SCALE = {
    "": 1.0,
    "ns": 1e-9,
    "us": 1e-6,
    "µs": 1e-6,
    "μs": 1e-6,
    "ms": 1e-3,
    "s": 1.0,
    "B": 1.0,
    "KB": 1024.0,
    "MB": 1024.0**2,
    "GB": 1024.0**3,
    "K": 1e3,
    "M": 1e6,
    "G": 1e9,
    "T": 1e12,
}


@dataclass
class Measurement:
    display: str
    value: float


@dataclass
class ScriptResult:
    path: str
    name: str
    rlox: dict[str, Measurement] = field(default_factory=dict)
    clox: dict[str, Measurement] = field(default_factory=dict)


def strip_ansi(text: str) -> str:
    return ANSI_RE.sub("", text)


def classify_command(command: str) -> str | None:
    # Match the binary path only (first token), not the .lox argument path.
    binary = Path(command.split()[0]).name if command.strip() else ""
    if binary == "rlox":
        return "rlox"
    if binary == "clox":
        return "clox"
    return None


def parse_mean(mean: str, unit: str | None) -> Measurement:
    unit = unit or ""
    scale = UNIT_SCALE.get(unit)
    if scale is None:
        # Unknown unit: keep numeric part only for relative comparisons.
        scale = 1.0
    value = float(mean) * scale
    display = f"{mean}{unit}" if unit else mean
    return Measurement(display=display, value=value)


def parse_poop_output(text: str) -> tuple[list[str], list[ScriptResult]]:
    metrics_order: list[str] = []
    results: list[ScriptResult] = []
    current: ScriptResult | None = None
    current_impl: str | None = None

    for raw_line in text.splitlines():
        line = strip_ansi(raw_line).rstrip()
        if not line.strip():
            continue

        header = BENCH_HEADER_RE.match(line)
        if header:
            path = header.group(1).strip()
            current = ScriptResult(path=path, name=Path(path).name)
            results.append(current)
            current_impl = None
            continue

        bench = BENCHMARK_RE.match(line)
        if bench:
            if current is None:
                # Allow plain poop output without our header.
                current = ScriptResult(path="unknown", name="unknown")
                results.append(current)
            current_impl = classify_command(bench.group(3))
            continue

        if line.lstrip().startswith("measurement"):
            continue

        metric = METRIC_RE.match(line)
        if not metric or current is None or current_impl is None:
            continue

        name = metric.group("name")
        if name == "measurement":
            continue
        if name not in metrics_order:
            metrics_order.append(name)

        measurement = parse_mean(metric.group("mean"), metric.group("unit"))
        if current_impl == "rlox":
            current.rlox[name] = measurement
        elif current_impl == "clox":
            current.clox[name] = measurement

    return metrics_order, results


def delta_percent(rlox: Measurement, clox: Measurement) -> str:
    if clox.value == 0:
        return "n/a"
    delta = (rlox.value - clox.value) / clox.value * 100.0
    sign = "+" if delta >= 0 else ""
    return f"{sign}{delta:.1f}%"


def format_table(metric: str, results: list[ScriptResult]) -> str:
    rows: list[tuple[str, str, str, str]] = []
    for result in results:
        z = result.rlox.get(metric)
        c = result.clox.get(metric)
        if z is None and c is None:
            continue
        z_disp = z.display if z else "-"
        c_disp = c.display if c else "-"
        if z is not None and c is not None:
            d_disp = delta_percent(z, c)
        else:
            d_disp = "-"
        rows.append((result.name, z_disp, c_disp, d_disp))

    if not rows:
        return f"{metric}\n(no data)\n"

    headers = ("script", "rlox", "clox", "delta %")
    widths = [len(h) for h in headers]
    for row in rows:
        for i, cell in enumerate(row):
            widths[i] = max(widths[i], len(cell))

    def fmt(row: tuple[str, ...]) -> str:
        return "  ".join(cell.ljust(widths[i]) for i, cell in enumerate(row))

    lines = [
        metric,
        fmt(headers),
        "  ".join("-" * w for w in widths),
    ]
    lines.extend(fmt(row) for row in rows)
    return "\n".join(lines) + "\n"


def main() -> int:
    if len(sys.argv) > 1:
        text = Path(sys.argv[1]).read_text(encoding="utf-8", errors="replace")
    else:
        text = sys.stdin.read()

    if not text.strip():
        print("No input. Pipe ./bench_all.sh output or pass a file path.", file=sys.stderr)
        return 1

    metrics, results = parse_poop_output(text)
    if not results:
        print("No benchmark blocks found in input.", file=sys.stderr)
        return 1
    if not metrics:
        print("No poop measurement rows found in input.", file=sys.stderr)
        return 1

    tables = [format_table(metric, results) for metric in metrics]
    sys.stdout.write("\n".join(tables))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
