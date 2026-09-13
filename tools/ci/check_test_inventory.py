#!/usr/bin/env python3
"""Check that bounded release batches cover the supported sbt test inventory."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import re
import shlex
import sys


REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_BUILD = REPO_ROOT / "build.sbt"
DEFAULT_RUNNER = REPO_ROOT / "tools" / "ci" / "full-repository-tests.sh"

# These modules need native OpenJFX plus a display. They remain in the compile
# court and are the only supported tests omitted from the headless release run.
DISPLAY_EXCLUSIONS = {
    "imageViewJavafxJVM/test": "requires OpenJFX natives and a display",
    "surfaceViewJavafxJVM/test": "requires OpenJFX natives and a display",
}


def alias_targets(build_text: str, alias: str) -> list[str]:
    pattern = re.compile(
        rf'addCommandAlias\(\s*"{re.escape(alias)}"\s*,\s*"([^"]*)"\s*\)'
    )
    match = pattern.search(build_text)
    if match is None:
        raise ValueError(f"build.sbt does not define {alias}")
    return [target.strip() for target in match.group(1).split(";") if target.strip()]


def release_batches(script_text: str) -> dict[str, list[str]]:
    logical = script_text.replace("\\\n", " ")
    batches: dict[str, list[str]] = {}
    for line in logical.splitlines():
        stripped = line.strip()
        if not stripped.startswith("run_batch "):
            continue
        words = shlex.split(stripped, comments=True)
        if len(words) < 3:
            raise ValueError(f"release batch has no targets: {stripped}")
        name = words[1]
        if name in batches:
            raise ValueError(f"duplicate release batch name: {name}")
        batches[name] = words[2:]
    if not batches:
        raise ValueError("release runner defines no run_batch invocations")
    return batches


def inspect_inventory(build_text: str, script_text: str) -> tuple[dict[str, object], list[str]]:
    supported_list = alias_targets(build_text, "scalafimTestAll")
    example_list = alias_targets(build_text, "examplesTest")
    batches = release_batches(script_text)
    actual_list = [target for targets in batches.values() for target in targets]

    supported = set(supported_list)
    examples = set(example_list)
    excluded = set(DISPLAY_EXCLUSIONS)
    extra_examples = examples - supported
    actual = set(actual_list)

    errors: list[str] = []
    duplicate_supported = sorted(
        target for target in supported if supported_list.count(target) > 1
    )
    duplicate_actual = sorted(
        target for target in actual if actual_list.count(target) > 1
    )
    missing = sorted((supported - excluded) - actual)
    unknown = sorted(actual - supported - extra_examples)
    missing_examples = sorted(extra_examples - actual)

    if duplicate_supported:
        errors.append("duplicate scalafimTestAll targets: " + ", ".join(duplicate_supported))
    if duplicate_actual:
        errors.append("duplicate release targets: " + ", ".join(duplicate_actual))
    if missing:
        errors.append("missing release targets: " + ", ".join(missing))
    if unknown:
        errors.append("unknown release targets: " + ", ".join(unknown))
    if missing_examples:
        errors.append("missing extra example tests: " + ", ".join(missing_examples))
    inventory: dict[str, object] = {
        "batches": batches,
        "display_exclusions": DISPLAY_EXCLUSIONS,
        "extra_example_tests": sorted(extra_examples),
        "supported_test_targets": sorted(supported),
    }
    return inventory, errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build", type=Path, default=DEFAULT_BUILD)
    parser.add_argument("--runner", type=Path, default=DEFAULT_RUNNER)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--check", action="store_true")
    mode.add_argument("--print-batches", action="store_true")
    args = parser.parse_args()

    try:
        inventory, errors = inspect_inventory(
            args.build.read_text(encoding="utf-8"),
            args.runner.read_text(encoding="utf-8"),
        )
    except (OSError, ValueError) as error:
        print(f"test inventory error: {error}", file=sys.stderr)
        return 1

    if args.print_batches:
        print(json.dumps(inventory, indent=2, sort_keys=True))
    if errors:
        for error in errors:
            print(f"test inventory error: {error}", file=sys.stderr)
        return 1
    if args.check:
        target_count = len(inventory["supported_test_targets"])
        batch_count = len(inventory["batches"])
        print(f"test inventory ok: {target_count} supported targets in {batch_count} batches")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
