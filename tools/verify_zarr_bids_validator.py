#!/usr/bin/env python3
"""Run the pinned official BIDS validator over every Zarr oracle export.

This is intentionally a hard gate.  It does not fall back to scalafim's BIDS
loader, and it does not report success when the official validator executable
is absent.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
from pathlib import Path


SCALARS = ("uint8", "int16", "int32", "float32", "float64")
PINNED_VALIDATOR_VERSION = "3.0.1"


def find_validator(explicit: Path | None) -> Path:
    if explicit is not None:
        candidate = explicit.expanduser().resolve()
        if not candidate.is_file():
            raise SystemExit(f"BIDS validator executable does not exist: {candidate}")
        if not os.access(candidate, os.X_OK):
            raise SystemExit(f"BIDS validator is not executable: {candidate}")
        return candidate

    for name in ("bids-validator-deno", "bids-validator"):
        found = shutil.which(name)
        if found is not None:
            return Path(found).resolve()

    raise SystemExit(
        "official BIDS validator is required; install "
        f"bids-validator-deno=={PINNED_VALIDATOR_VERSION} or pass --validator"
    )


def check_version(validator: Path, expected: str) -> str:
    completed = subprocess.run(
        [str(validator), "--version"],
        check=False,
        capture_output=True,
        text=True,
    )
    output = "\n".join(
        value.strip() for value in (completed.stdout, completed.stderr) if value.strip()
    )
    if completed.returncode != 0:
        raise AssertionError(
            f"official BIDS validator --version failed ({completed.returncode}):\n{output}"
        )
    if expected not in output:
        raise AssertionError(
            f"expected official BIDS validator {expected}, found: {output or '<empty>'}"
        )
    return output


def validate_dataset(validator: Path, dataset: Path) -> dict[str, object]:
    if not dataset.is_dir():
        raise AssertionError(f"missing BIDS export: {dataset}")
    completed = subprocess.run(
        [str(validator), "--json", "--no-color", str(dataset)],
        check=False,
        capture_output=True,
        text=True,
    )
    if completed.returncode != 0:
        rendered = completed.stdout.strip() or completed.stderr.strip()
        raise AssertionError(
            f"official BIDS validation failed for {dataset.name} "
            f"({completed.returncode}):\n{rendered}"
        )
    try:
        result = json.loads(completed.stdout)
    except json.JSONDecodeError as error:
        raise AssertionError(
            f"official BIDS validator returned non-JSON output for {dataset.name}:\n"
            f"{completed.stdout}\n{completed.stderr}"
        ) from error
    if not isinstance(result, dict):
        raise AssertionError(
            f"official BIDS validator returned a non-object result for {dataset.name}"
        )
    issues = result.get("issues")
    issue_values = issues.get("issues") if isinstance(issues, dict) else None
    if not isinstance(issue_values, list):
        raise AssertionError(
            f"official BIDS validator omitted its issue list for {dataset.name}"
        )
    errors = [
        issue
        for issue in issue_values
        if isinstance(issue, dict) and issue.get("severity") == "error"
    ]
    if errors:
        raise AssertionError(
            f"official BIDS validator reported errors for {dataset.name}: "
            f"{json.dumps(errors, separators=(',', ':'))}"
        )
    summary = result.get("summary")
    if not isinstance(summary, dict):
        raise AssertionError(
            f"official BIDS validator omitted its summary for {dataset.name}"
        )
    if summary.get("subjects") != ["01"]:
        raise AssertionError(f"validator did not identify sub-01 in {dataset.name}: {summary}")
    if summary.get("tasks") != ["rest"]:
        raise AssertionError(f"validator did not identify task-rest in {dataset.name}: {summary}")
    data_types = summary.get("dataTypes")
    if not isinstance(data_types, list) or "func" not in data_types:
        raise AssertionError(f"validator did not identify functional data in {dataset.name}: {summary}")
    return result


def summarize(result: dict[str, object]) -> dict[str, object]:
    issue_container = result["issues"]
    assert isinstance(issue_container, dict)
    issue_values = issue_container["issues"]
    assert isinstance(issue_values, list)
    counts = {"error": 0, "warning": 0}
    for issue in issue_values:
        if isinstance(issue, dict) and issue.get("severity") in counts:
            severity = issue["severity"]
            assert isinstance(severity, str)
            counts[severity] += 1
    summary = result.get("summary")
    schema_version = summary.get("schemaVersion") if isinstance(summary, dict) else None
    return {
        "errors": counts["error"],
        "warnings": counts["warning"],
        "schema_version": schema_version,
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("output_root", type=Path)
    parser.add_argument("--validator", type=Path)
    parser.add_argument("--receipt", type=Path)
    parser.add_argument(
        "--expected-version",
        default=PINNED_VALIDATOR_VERSION,
        help="exact validator version substring required from --version",
    )
    arguments = parser.parse_args()

    validator = find_validator(arguments.validator)
    version = check_version(validator, arguments.expected_version)
    datasets = {
        scalar: validate_dataset(validator, arguments.output_root / f"{scalar}-bids")
        for scalar in SCALARS
    }
    receipt = {
        "gate": "official-bids-validator",
        "validator": str(validator),
        "version": version,
        "expected_version": arguments.expected_version,
        "datasets": datasets,
    }
    if arguments.receipt is not None:
        arguments.receipt.parent.mkdir(parents=True, exist_ok=True)
        arguments.receipt.write_text(
            json.dumps(receipt, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
    print(json.dumps(
        {
            "gate": receipt["gate"],
            "version": receipt["version"],
            "datasets": {
                scalar: summarize(result)
                for scalar, result in datasets.items()
            },
        },
        separators=(",", ":"),
        sort_keys=True,
    ))


if __name__ == "__main__":
    main()
