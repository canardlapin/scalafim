#!/usr/bin/env python3
"""Keep first-level guides identical to their compiled Scala examples."""
from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
import re
import textwrap

REPO = Path(__file__).resolve().parents[2]


@dataclass(frozen=True)
class Guide:
  source: Path
  document: Path
  sections: tuple[str, ...]


GUIDES = {
  "analysis": Guide(
    REPO / "modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/DelayedMatchToSampleDslScenarioSuite.scala",
    REPO / "docs/first-level-analysis.md",
    ("first-level-model", "first-level-hypotheses")),
  "selected": Guide(
    REPO / "modules/fit/shared/src/test/scala/scalafim/examples/SelectedEstimatesExample.scala",
    REPO / "docs/selected-estimates.md",
    ("selected-imports", "selected-condition", "selected-fir", "selected-response", "selected-uncertainty",
     "selected-nuisance", "selected-run-coefficients", "selected-prepare", "selected-execute")),
}


def source_section(source: str, name: str, path: Path) -> str:
  start, end = f"// docs:{name}:start", f"// docs:{name}:end"
  if source.count(start) != 1 or source.count(end) != 1:
    raise SystemExit(f"expected exactly one {name} marker pair in {path}")
  return textwrap.dedent(source.split(start, 1)[1].split(end, 1)[0]).strip("\n")


def rendered_section(name: str, body: str) -> str:
  return (f"<!-- BEGIN extracted:{name} -->\n```scala\n{body}\n```\n"
          f"<!-- END extracted:{name} -->")


def replace_section(document: str, name: str, rendered: str, path: Path) -> str:
  begin, end = f"<!-- BEGIN extracted:{name} -->", f"<!-- END extracted:{name} -->"
  if document.count(begin) != 1 or document.count(end) != 1:
    raise SystemExit(f"expected exactly one {name} extraction block in {path}")
  pattern = re.compile(re.escape(begin) + r".*?" + re.escape(end), re.DOTALL)
  return pattern.sub(lambda _: rendered, document)


def expected_guide(guide: Guide) -> tuple[str, list[str]]:
  source = guide.source.read_text()
  document = guide.document.read_text()
  bodies = [source_section(source, name, guide.source) for name in guide.sections]
  for name, body in zip(guide.sections, bodies):
    document = replace_section(document, name, rendered_section(name, body), guide.document)
  return document, bodies


def check(guide: Guide) -> None:
  expected, bodies = expected_guide(guide)
  actual = guide.document.read_text()
  errors = []
  if actual != expected:
    errors.append("extracted example blocks are stale")
  if re.findall(r"```scala\n(.*?)\n```", actual, re.DOTALL) != bodies:
    errors.append("every Scala fence must be an extracted compiled example block")
  if errors:
    raise SystemExit(f"{guide.document.name} check failed: {', '.join(errors)}")
  print(f"validated executable first-level guide: {guide.document.relative_to(REPO)}")


def main() -> int:
  parser = argparse.ArgumentParser(description=__doc__)
  mode = parser.add_mutually_exclusive_group(required=True)
  mode.add_argument("--write", action="store_true")
  mode.add_argument("--check", action="store_true")
  parser.add_argument("--guide", choices=["all", *GUIDES], default="all")
  args = parser.parse_args()
  selected = GUIDES.values() if args.guide == "all" else [GUIDES[args.guide]]
  for guide in selected:
    if args.write:
      guide.document.write_text(expected_guide(guide)[0])
      print(f"updated executable first-level guide: {guide.document.relative_to(REPO)}")
    else:
      check(guide)
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
