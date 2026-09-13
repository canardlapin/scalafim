#!/usr/bin/env python3
"""Synchronize the first-level guide with its compiled DMS scenario source."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import re
import textwrap


REPO = Path(__file__).resolve().parents[2]
SOURCE = REPO / "modules" / "fit" / "shared" / "src" / "test" / "scala" / "scalafim" / "fmri" / "fit" / "scenarios" / "DelayedMatchToSampleDslScenarioSuite.scala"
GUIDE = REPO / "docs" / "first-level-analysis.md"
ACCEPTANCE_DATA = REPO / "docs" / "verification" / "first-level-identity-acceptance.json"
ACCEPTANCE_GUIDE = REPO / "docs" / "verification" / "first-level-identity-acceptance.md"
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


def acceptance_data() -> dict:
  value = json.loads(ACCEPTANCE_DATA.read_text())
  if not isinstance(value, dict):
    raise SystemExit(f"expected a JSON object in {ACCEPTANCE_DATA}")
  return value


def validate_acceptance_data(value: dict) -> list[str]:
  errors: list[str] = []
  if value.get("schema_version") != "scalafim-first-level-identity-acceptance/v1":
    errors.append("acceptance ledger schema is invalid")
  catalog = value.get("evidence_catalog")
  criteria = value.get("criteria")
  if not isinstance(catalog, dict) or not isinstance(criteria, list):
    return errors + ["acceptance ledger catalog or criteria are missing"]
  expected_counts = {"global": 9, "phase0": 6, "phase1": 8, "phase2": 7, "phase3": 7, "phase4": 7}
  observed_counts = {scope: 0 for scope in expected_counts}
  ids: set[str] = set()
  manifest = json.loads((REPO / "docs/scenarios/manifest.json").read_text())
  scenarios = {
    item.get("id"): item for item in manifest.get("scenarios", [])
    if isinstance(item, dict)
  }
  for criterion in criteria:
    if not isinstance(criterion, dict):
      errors.append("acceptance ledger contains a malformed criterion")
      continue
    identifier = criterion.get("id")
    scope = criterion.get("scope")
    if not isinstance(identifier, str) or identifier in ids:
      errors.append(f"acceptance criterion id is missing or duplicated: {identifier!r}")
    else:
      ids.add(identifier)
    if scope not in observed_counts:
      errors.append(f"acceptance criterion {identifier} has an unknown scope")
    else:
      observed_counts[scope] += 1
    if not isinstance(criterion.get("criterion"), str) or not criterion["criterion"].strip():
      errors.append(f"acceptance criterion {identifier} has no wording")
    evidence = criterion.get("evidence")
    if not isinstance(evidence, list) or not evidence:
      errors.append(f"acceptance criterion {identifier} has no evidence mapping")
    elif any(item not in catalog for item in evidence):
      errors.append(f"acceptance criterion {identifier} names unknown evidence")
    status = criterion.get("status")
    if status not in {"not_run", "pass", "fail", "blocked"}:
      errors.append(f"acceptance criterion {identifier} has invalid status {status!r}")
    candidate = criterion.get("source_candidate")
    if status in {"pass", "fail"} and not isinstance(candidate, str):
      errors.append(f"executed acceptance criterion {identifier} has no source candidate")
    if isinstance(candidate, str) and re.fullmatch(r"[0-9a-f]{40}", candidate) is None:
      errors.append(f"acceptance criterion {identifier} has an invalid source candidate")
  if observed_counts != expected_counts:
    errors.append(f"acceptance criterion counts are {observed_counts}, expected {expected_counts}")

  for evidence_id, evidence in catalog.items():
    if not isinstance(evidence, dict):
      errors.append(f"evidence {evidence_id} is malformed")
      continue
    if not isinstance(evidence.get("modules"), list) or not evidence["modules"]:
      errors.append(f"evidence {evidence_id} has no owning modules")
    commands = evidence.get("commands")
    if not isinstance(commands, list) or not commands or any(not isinstance(command, str) or not command for command in commands):
      errors.append(f"evidence {evidence_id} has no executable commands")
    for key in ("source_paths", "external_fixtures"):
      paths = evidence.get(key)
      if not isinstance(paths, list):
        errors.append(f"evidence {evidence_id} has invalid {key}")
        continue
      for relative in paths:
        if not isinstance(relative, str) or not (REPO / relative).exists():
          errors.append(f"evidence {evidence_id} has unresolved path {relative!r}")
    suites = evidence.get("suites")
    if not isinstance(suites, list):
      errors.append(f"evidence {evidence_id} has invalid suites")
      continue
    for suite in suites:
      relative = suite.get("path") if isinstance(suite, dict) else None
      path = REPO / relative if isinstance(relative, str) else None
      if path is None or not path.is_file():
        errors.append(f"evidence {evidence_id} has unresolved suite {relative!r}")
        continue
      source = path.read_text()
      for test in suite.get("tests", []):
        if f'test("{test}")' not in source and f'property("{test}")' not in source:
          errors.append(f"evidence {evidence_id} has unresolved test {test!r}")
      for scenario_id in suite.get("scenarios", []):
        scenario = scenarios.get(scenario_id)
        if scenario is None or scenario.get("suite_path") != relative:
          errors.append(f"evidence {evidence_id} has unresolved scenario {scenario_id!r}")
  return errors


def markdown_cell(value: str) -> str:
  return value.replace("|", "\\|").replace("\n", " ")


def render_acceptance_guide(value: dict) -> str:
  catalog = value["evidence_catalog"]
  criteria = value["criteria"]
  lines = [
    "# First-level scientific-identity acceptance ledger",
    "",
    "This ledger preserves the retained wording of epic",
    "`bd-01KZ3ZCDD2RMYQ3WVY606FNKY1` and phases 0–4. It maps each",
    "criterion to current source and executable evidence. `not_run` means the",
    "mapping is current but the command has not yet run against the final clean",
    "candidate; historical task closure is not counted as fresh evidence. The",
    "candidate-bound release report supplies final execution status.",
    "",
    "## Evidence catalog",
    "",
  ]
  for evidence_id in sorted(catalog, key=lambda item: int(item[1:])):
    evidence = catalog[evidence_id]
    lines.extend([
      f"### {evidence_id}: {evidence['title']}",
      "",
      "- Owning modules: " + ", ".join(f"`{value}`" for value in evidence["modules"]),
      "- Public API/source: " + ", ".join(f"`{value}`" for value in evidence["source_paths"]),
    ])
    if evidence["suites"]:
      rendered_suites = []
      for suite in evidence["suites"]:
        names = suite.get("tests", []) + suite.get("scenarios", [])
        rendered_suites.append(f"`{suite['path']}` ({'; '.join(f'`{name}`' for name in names)})")
      lines.append("- Suites and exact tests/scenarios: " + "; ".join(rendered_suites))
    else:
      lines.append("- Suites and exact tests/scenarios: repository-level structural check")
    fixtures = evidence["external_fixtures"]
    lines.append("- External fixtures/receipts: " + (
      ", ".join(f"`{value}`" for value in fixtures) if fixtures else "none claimed"
    ))
    lines.append("- Commands:")
    lines.extend(f"  - `{command}`" for command in evidence["commands"])
    lines.append("")

  headings = {
    "global": "Global acceptance criteria",
    "phase0": "Phase 0 acceptance criteria",
    "phase1": "Phase 1 acceptance criteria",
    "phase2": "Phase 2 acceptance criteria",
    "phase3": "Phase 3 acceptance criteria",
    "phase4": "Phase 4 acceptance criteria",
  }
  for scope, heading in headings.items():
    lines.extend([
      f"## {heading}",
      "",
      "| ID | Retained criterion | Evidence | Current status | Source candidate |",
      "| --- | --- | --- | --- | --- |",
    ])
    for criterion in criteria:
      if criterion["scope"] != scope:
        continue
      evidence = ", ".join(f"[{item}](#{item.lower()})" for item in criterion["evidence"])
      candidate = criterion["source_candidate"] or "—"
      lines.append(
        f"| {criterion['id']} | {markdown_cell(criterion['criterion'])} | {evidence} | "
        f"`{criterion['status']}` | `{candidate}` |"
      )
    lines.append("")
  return "\n".join(lines).rstrip() + "\n"


def check() -> None:
  expected, bodies = expected_guide()
  actual = GUIDE.read_text()
  errors: list[str] = []
  if actual != expected:
    errors.append("extracted scenario blocks are stale")
  scala_blocks = re.findall(r"```scala\n(.*?)\n```", actual, re.DOTALL)
  if scala_blocks != bodies:
    errors.append("every Scala fence must be an extracted compiled scenario block")
  ledger = acceptance_data()
  errors.extend(validate_acceptance_data(ledger))
  if not ACCEPTANCE_GUIDE.is_file() or ACCEPTANCE_GUIDE.read_text() != render_acceptance_guide(ledger):
    errors.append("first-level identity acceptance ledger is stale")
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
    ledger = acceptance_data()
    errors = validate_acceptance_data(ledger)
    if errors:
      raise SystemExit(f"first-level acceptance ledger failed: {', '.join(errors)}")
    ACCEPTANCE_GUIDE.write_text(render_acceptance_guide(ledger))
    print(f"updated executable first-level guide: {GUIDE.relative_to(REPO)}")
    print(f"updated first-level acceptance ledger: {ACCEPTANCE_GUIDE.relative_to(REPO)}")
  else:
    check()
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
