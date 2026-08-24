#!/usr/bin/env python3
"""Synchronize the first-level guide with its compiled DMS scenario source."""

from __future__ import annotations

import argparse
from pathlib import Path
import re
import textwrap


REPO = Path(__file__).resolve().parents[2]
SOURCE = REPO / "modules" / "fit" / "shared" / "src" / "test" / "scala" / "scalafim" / "fmri" / "fit" / "scenarios" / "DelayedMatchToSampleDslScenarioSuite.scala"
GUIDE = REPO / "docs" / "first-level-analysis.md"
SECTIONS = (
  "first-level-model",
  "first-level-hypotheses",
)


def source_section(source: str, name: str) -> str:
  start = f"// docs:{name}:start"
  end = f"// docs:{name}:end"
  if source.count(start) != 1 or source.count(end) != 1:
    raise SystemExit(f"expected exactly one {name} marker pair in {SOURCE}")
  body = source.split(start, 1)[1].split(end, 1)[0]
  return textwrap.dedent(body).strip("\n")


def rendered_section(name: str, body: str) -> str:
  return (
    f"<!-- BEGIN extracted:{name} -->\n"
    f"```scala\n{body}\n```\n"
    f"<!-- END extracted:{name} -->"
  )


def replace_section(document: str, name: str, rendered: str) -> str:
  begin = f"<!-- BEGIN extracted:{name} -->"
  end = f"<!-- END extracted:{name} -->"
  if document.count(begin) != 1 or document.count(end) != 1:
    raise SystemExit(f"expected exactly one {name} extraction block in {GUIDE}")
  pattern = re.compile(re.escape(begin) + r".*?" + re.escape(end), re.DOTALL)
  return pattern.sub(rendered, document)


def expected_guide() -> tuple[str, list[str]]:
  source = SOURCE.read_text()
  document = GUIDE.read_text()
  bodies = [source_section(source, name) for name in SECTIONS]
  for name, body in zip(SECTIONS, bodies):
    document = replace_section(document, name, rendered_section(name, body))
  return document, bodies


def check() -> None:
  expected, bodies = expected_guide()
  actual = GUIDE.read_text()
  errors: list[str] = []
  if actual != expected:
    errors.append("extracted scenario blocks are stale")
  scala_blocks = re.findall(r"```scala\n(.*?)\n```", actual, re.DOTALL)
  if scala_blocks != bodies:
    errors.append("every Scala fence must be an extracted compiled scenario block")
  if errors:
    raise SystemExit(f"first-level guide check failed: {', '.join(errors)}")
  print(f"validated executable first-level guide: {GUIDE.relative_to(REPO)}")


def main() -> int:
  parser = argparse.ArgumentParser(description=__doc__)
  mode = parser.add_mutually_exclusive_group(required=True)
  mode.add_argument("--write", action="store_true")
  mode.add_argument("--check", action="store_true")
  args = parser.parse_args()

  expected, _ = expected_guide()
  if args.write:
    GUIDE.write_text(expected)
    print(f"updated executable first-level guide: {GUIDE.relative_to(REPO)}")
  else:
    check()
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
