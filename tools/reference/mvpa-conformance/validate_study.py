#!/usr/bin/env python3
"""Validate the committed MVPA conformance study without external runtimes."""

from __future__ import annotations

import csv
import hashlib
import json
import math
from collections import Counter
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
AUDIT = ROOT / "docs/audits/mvpa-conformance"
REFERENCE = ROOT / "tools/reference/mvpa-conformance"
ALLOWED_DISPOSITIONS = {
    "exact_parity",
    "convention_equivalent",
    "conditional_prediction_parity",
    "numeric_parity_receipt_gap",
    "reproducible_downstream",
    "intentional_divergence",
    "absent_no_claim",
    "pending",
}
PERFORMANCE_FIELDS = {
    "scenario",
    "implementation",
    "stage",
    "shape",
    "median_ms_per_call",
    "p25_ms_per_call",
    "p75_ms_per_call",
    "relative_time_to_fastest",
    "allocation_bytes",
    "allocation_scope",
    "checksum",
    "claim_boundary",
}


def fail(message: str) -> None:
    raise SystemExit(message)


def load_json(path: Path):
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        fail(f"invalid JSON {path.relative_to(ROOT)}: {error}")


def load_csv(path: Path) -> list[dict[str, str]]:
    try:
        with path.open(newline="", encoding="utf-8") as stream:
            return list(csv.DictReader(stream))
    except OSError as error:
        fail(f"cannot read {path.relative_to(ROOT)}: {error}")


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def unique(rows: list[dict[str, str]], field: str, source: str) -> None:
    values = [row[field] for row in rows]
    duplicates = sorted(value for value, count in Counter(values).items() if count > 1)
    if duplicates:
        fail(f"duplicate {field} in {source}: {duplicates}")
    if any(not value for value in values):
        fail(f"blank {field} in {source}")


def validate_reference() -> None:
    result = load_json(REFERENCE / "reference-results-v1.json")
    lock = load_json(REFERENCE / "reference-lock.json")
    if lock.get("schema") != "scalafim-mvpa-conformance-reference-lock/v1":
        fail("unexpected reference-lock schema")
    expected_references = {"pymvpa", "rsatoolbox_matlab", "rsatoolbox_python"}
    if set(result.get("references", {})) != expected_references:
        fail("reference-results reference set drifted")
    if set(lock.get("references", {})) != expected_references:
        fail("reference-lock reference set drifted")
    if result["references"]["pymvpa"]["version"] != lock["references"]["pymvpa"]["reported_version"]:
        fail("PyMVPA version differs between lock and result")
    if result["references"]["rsatoolbox_python"]["version"] != lock["references"]["rsatoolbox_python"]["release"]:
        fail("rsatoolbox version differs between lock and result")
    if result["references"]["rsatoolbox_matlab"]["revision"] != lock["references"]["rsatoolbox_matlab"]["revision"]:
        fail("MATLAB RSA Toolbox revision differs between lock and result")
    agreements = result.get("agreements", [])
    if not agreements:
        fail("reference-results contains no agreements")
    for agreement in agreements:
        if agreement["max_abs_diff"] > agreement["tolerance"]:
            fail(f"reference agreement exceeds tolerance: {agreement['name']}")


def validate_coverage() -> list[dict[str, str]]:
    rows = load_csv(AUDIT / "coverage.csv")
    required = {
        "scenario_id",
        "reference",
        "capability",
        "scalafim_surface",
        "disposition",
        "convention_or_boundary",
        "evidence",
    }
    if not rows or set(rows[0]) != required:
        fail("coverage.csv schema drifted")
    unique(rows, "scenario_id", "coverage.csv")
    observed = {row["disposition"] for row in rows}
    unknown = observed - ALLOWED_DISPOSITIONS
    if unknown:
        fail(f"unknown coverage dispositions: {sorted(unknown)}")
    if ALLOWED_DISPOSITIONS - observed:
        fail(f"unexercised coverage dispositions: {sorted(ALLOWED_DISPOSITIONS - observed)}")
    for row in rows:
        if not row["reference"] or not row["capability"] or not row["convention_or_boundary"] or not row["evidence"]:
            fail(f"incomplete coverage row: {row['scenario_id']}")
        if row["disposition"] == "pending" and row["evidence"] != "coverage-ledger":
            fail(f"pending scenario claims executable evidence: {row['scenario_id']}")
    return rows


def validate_findings() -> list[dict[str, object]]:
    path = AUDIT / "findings.jsonl"
    findings = []
    for number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        try:
            finding = json.loads(line)
        except json.JSONDecodeError as error:
            fail(f"invalid findings.jsonl line {number}: {error}")
        required = {"id", "date", "category", "severity", "scenario", "status", "evidence", "detail", "implication", "mote"}
        if set(finding) != required:
            fail(f"findings.jsonl schema drift at line {number}")
        if not str(finding["mote"]).startswith("bd-"):
            fail(f"finding has no Mote issue: {finding['id']}")
        findings.append(finding)
    unique(findings, "id", "findings.jsonl")  # type: ignore[arg-type]
    return findings


def validate_assurance() -> list[dict[str, str]]:
    rows = load_csv(AUDIT / "assurance.csv")
    unique(rows, "assurance_id", "assurance.csv")
    for row in rows:
        if row["jvm"] != "pass" or row["scala_js"] != "pass":
            fail(f"assurance row is not cross-platform pass: {row['assurance_id']}")
        if not row["claim_boundary"]:
            fail(f"assurance row lacks a claim boundary: {row['assurance_id']}")
    return rows


def validate_ergonomics() -> list[dict[str, str]]:
    rows = load_csv(AUDIT / "ergonomics.csv")
    keys = [(row["scenario_id"], row["implementation"]) for row in rows]
    if len(keys) != len(set(keys)):
        fail("duplicate scenario/implementation in ergonomics.csv")
    for row in rows:
        for field in ("direct_raw_loc", "direct_executable_loc", "support_raw_loc", "support_executable_loc"):
            if int(row[field]) < 0:
                fail(f"negative LOC in ergonomics.csv: {keys}")
        if not row["pain_points"] or not row["elegance_notes"]:
            fail(f"uninterpreted ergonomics row: {row['scenario_id']} {row['implementation']}")
    return rows


def validate_performance() -> list[dict[str, str]]:
    receipt = load_json(AUDIT / "performance-receipt-20260826.json")
    if receipt.get("schema") != "scalafim-mvpa-performance-receipt/v1":
        fail("unexpected performance receipt schema")
    for relative, digest in receipt.get("harness_sha256", {}).items():
        path = ROOT / relative
        if not path.is_file() or sha256(path) != digest:
            fail(f"performance harness drift: {relative}")
    for agreement in receipt.get("checksum_agreements", []):
        if agreement["absolute_difference"] > agreement["tolerance"]:
            fail(f"performance checksum disagreement: {agreement['implementation']} {agreement['scenario']}")

    rows = load_csv(AUDIT / "performance.csv")
    if not rows or set(rows[0]) != PERFORMANCE_FIELDS:
        fail("performance.csv schema drifted")
    keys = [(row["scenario"], row["implementation"]) for row in rows]
    if len(keys) != len(set(keys)):
        fail("duplicate scenario/implementation in performance.csv")
    results = {(result["scenario"], result["implementation"]): result for result in receipt["results"]}
    if set(keys) != set(results):
        fail("performance receipt and ledger contain different implementation/scenario pairs")
    for row in rows:
        result = results[(row["scenario"], row["implementation"])]
        checks = {
            "median_ms_per_call": result["median_ns_per_call"] / 1e6,
            "p25_ms_per_call": result["p25_ns_per_call"] / 1e6,
            "p75_ms_per_call": result["p75_ns_per_call"] / 1e6,
            "checksum": result["checksum"],
        }
        for field, expected in checks.items():
            if not math.isclose(float(row[field]), float(expected), rel_tol=0.0, abs_tol=5e-10):
                fail(f"performance ledger differs from receipt: {row['scenario']} {row['implementation']} {field}")
        allocation = result["allocation_bytes"]
        if allocation is None:
            if row["allocation_bytes"]:
                fail(f"unexpected allocation value: {row['scenario']} {row['implementation']}")
        elif not math.isclose(float(row["allocation_bytes"]), float(allocation), rel_tol=0.0, abs_tol=5e-4):
            fail(f"allocation differs from receipt: {row['scenario']} {row['implementation']}")
    return rows


def validate_report(
    coverage: list[dict[str, str]],
    findings: list[dict[str, object]],
    assurance: list[dict[str, str]],
) -> None:
    path = AUDIT / "report.md"
    if not path.is_file():
        fail("missing reader-facing conformance report")
    report = path.read_text(encoding="utf-8")
    required_phrases = {
        "local evidence, not hosted proof",
        "original MATLAB RSA Toolbox",
        "Python rsatoolbox",
        "PyMVPA",
        "numeric parity, receipt failure",
        f"{len(coverage)} coverage rows",
        f"{len(assurance)} cross-platform assurance rows",
    }
    for disposition in ALLOWED_DISPOSITIONS:
        required_phrases.add(f"`{disposition}`")
    for phrase in sorted(required_phrases):
        if phrase not in report:
            fail(f"report omits required scope phrase: {phrase}")
    for finding in findings:
        if str(finding["id"]) not in report:
            fail(f"report omits finding: {finding['id']}")


def main() -> int:
    validate_reference()
    coverage = validate_coverage()
    findings = validate_findings()
    assurance = validate_assurance()
    ergonomics = validate_ergonomics()
    performance = validate_performance()
    validate_report(coverage, findings, assurance)
    print(
        "validated MVPA conformance study: "
        f"{len(coverage)} coverage rows, {len(findings)} findings, "
        f"{len(assurance)} cross-platform assurance rows, "
        f"{len(ergonomics)} ergonomics rows, and {len(performance)} performance rows"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
