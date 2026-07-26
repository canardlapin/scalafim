#!/usr/bin/env python3
"""Validate the frozen HalfFlow-CC G0 protocol and its live artifacts."""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
from pathlib import Path
from typing import Any


ROOT = Path(__file__).resolve().parents[2]
DEFAULT_RECEIPT = (
    ROOT
    / "docs"
    / "benchmarks"
    / "receipts"
    / "half-flow-cc-g0-control-2026-07-22.json"
)


def load_json(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"expected a JSON object: {path}")
    return value


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def resolve(path: str) -> Path:
    candidate = Path(path)
    return candidate if candidate.is_absolute() else ROOT / candidate


def require(condition: bool, message: str) -> None:
    if not condition:
        raise ValueError(message)


def unavailable(unit: str, reason: str = "protocol example") -> dict[str, Any]:
    return {"available": False, "unit": unit, "reason": reason}


def validate_json_schema(
    path: Path,
    expected_version: str,
    example: dict[str, Any],
) -> None:
    schema = load_json(path)
    require(schema.get("$schema") == "https://json-schema.org/draft/2020-12/schema", "unexpected JSON Schema draft")
    require(schema.get("properties", {}).get("schemaVersion", {}).get("const") == expected_version, "result schema version mismatch")
    try:
        from jsonschema import Draft202012Validator
    except ImportError:
        return
    Draft202012Validator.check_schema(schema)
    Draft202012Validator(schema).validate(example)


def validate_artifacts(receipt: dict[str, Any]) -> None:
    roles: dict[str, dict[str, Any]] = {}
    for artifact in receipt["frozenArtifacts"]:
        role = artifact["role"]
        require(role not in roles, f"duplicate artifact role: {role}")
        roles[role] = artifact
        path = resolve(artifact["path"])
        require(path.is_file(), f"missing frozen artifact: {path}")
        actual = sha256(path)
        require(actual == artifact["sha256"], f"artifact hash drift for {role}: {actual}")

    required = {
        "fixed-image",
        "moving-image",
        "fixed-mask",
        "moving-mask",
        "common-supplied-affine",
        "historical-ants-receipt",
    }
    require(set(roles) == required, f"artifact roles differ: {sorted(roles)}")
    require(roles["fixed-mask"]["use"] == "optimization-support", "fixed mask must be optimization support")
    require(roles["moving-mask"]["use"] == "evaluation-only", "moving mask must be evaluation-only")


def validate_sources(receipt: dict[str, Any], live_sources: bool) -> None:
    for relative, expected in receipt["sourceSha256"].items():
        require(isinstance(relative, str) and relative, "invalid frozen source path")
        require(
            isinstance(expected, str)
            and len(expected) == 64
            and all(character in "0123456789abcdef" for character in expected),
            f"invalid frozen source hash for {relative}",
        )
        if not live_sources:
            continue
        path = ROOT / relative
        require(path.is_file(), f"missing frozen source: {relative}")
        actual = sha256(path)
        require(actual == expected, f"source hash drift for {relative}: {actual}")


def validate_protocol_artifacts(receipt: dict[str, Any], live_sources: bool) -> None:
    for relative, expected in receipt["protocolArtifacts"].items():
        require(isinstance(relative, str) and relative, "invalid protocol artifact path")
        require(
            isinstance(expected, str)
            and len(expected) == 64
            and all(character in "0123456789abcdef" for character in expected),
            f"invalid protocol artifact hash for {relative}",
        )
        if not live_sources:
            continue
        path = ROOT / relative
        require(path.is_file(), f"missing protocol artifact: {relative}")
        actual = sha256(path)
        require(actual == expected, f"protocol artifact hash drift for {relative}: {actual}")


def validate_lanes(receipt: dict[str, Any]) -> None:
    lanes = receipt["lanes"]
    by_id = {lane["id"]: lane for lane in lanes}
    require(len(by_id) == len(lanes), "duplicate lane id")
    require(set(by_id) == {"A", "B", "C", "D", "E"}, "lanes must be exactly A-E")

    def same(left: str, right: str, fields: tuple[str, ...]) -> None:
        for field in fields:
            require(by_id[left][field] == by_id[right][field], f"lanes {left}/{right} differ in {field}")

    same("A", "B", ("metric", "step", "action", "geometry", "controllers"))
    require(not by_id["A"]["hardAccumulatedInverseGate"], "lane A must disable the old inverse gate")
    require(by_id["B"]["hardAccumulatedInverseGate"], "lane B must isolate the old inverse gate")
    same("A", "C", ("metric", "step", "geometry", "controllers", "hardAccumulatedInverseGate"))
    require(by_id["C"]["action"] == "one-direction-full-flow", "lane C must isolate one-direction action")
    same("A", "D", ("step", "action", "geometry", "controllers", "hardAccumulatedInverseGate"))
    require(by_id["D"]["metric"] != by_id["A"]["metric"], "lane D must isolate the surrogate metric")
    require("3x3" in by_id["E"]["step"], "lane E must retain the frozen 3x3 control")


def validate_fairness(receipt: dict[str, Any]) -> None:
    experiment = receipt["sharedExperiment"]
    optimization = set(experiment["optimizationInputs"])
    evaluation = set(experiment["evaluationOnlyInputs"])
    require("fixed-mask" in optimization, "fixed mask missing from optimization support")
    require("moving-mask" not in optimization, "moving evaluation mask leaked into optimization")
    require("moving-mask" in evaluation, "moving mask missing from evaluation-only inputs")
    require(optimization.isdisjoint(evaluation), "optimization and evaluation-only roles overlap")

    affine = experiment["suppliedAffine"]
    require(set(affine["suppliedTo"]) == {"A", "B", "C", "D", "E", "ANTS"}, "common affine consumers drifted")
    require(affine["currentConversionStatus"] == "unverified", "G0 must not claim unproved affine conversion")

    budget = experiment["acceptedStepBudget"]
    require(budget["total"] == 24, "accepted-step total drifted")
    require(sum(level["acceptedSteps"] for level in budget["levels"]) == budget["total"], "level budgets do not sum")
    require([level["shrink"] for level in budget["levels"]] == [4, 2, 1], "shared shrink schedule drifted")

    historical = receipt["historicalControl"]
    require(historical["role"] == "historical-regression-only", "E0 must remain historical only")
    require("not an A-E" in historical["comparability"], "E0 comparability warning missing")
    claims = " ".join(receipt["claims"]).lower()
    require("sub18 alone" in claims, "single-pair claim prohibition missing")


def validate_result_contract(receipt: dict[str, Any]) -> None:
    contract = receipt["resultSchema"]
    schema_path = resolve(contract["path"])
    require(schema_path.is_file(), f"missing result schema: {schema_path}")
    artifacts = {artifact["role"]: artifact for artifact in receipt["frozenArtifacts"]}

    def schema_artifact(role: str, evaluation_only: bool = False) -> dict[str, Any]:
        artifact = artifacts[role]
        value: dict[str, Any] = {
            "path": artifact["path"],
            "sha256": artifact["sha256"],
            "role": role,
        }
        if evaluation_only:
            value["evaluationOnly"] = True
        return value

    error_summary = {
        "p50Mm": unavailable("mm"),
        "p95Mm": unavailable("mm"),
        "p99Mm": unavailable("mm"),
        "maximumMm": unavailable("mm"),
        "maximumVox": unavailable("voxel"),
    }
    topology_arm = {
        "minimumJacobian": unavailable("determinant"),
        "nonPositive": 0,
        "quantiles": {},
    }
    example = {
        "schemaVersion": contract["schemaVersion"],
        "recordId": "protocol-validation-example",
        "capturedAtUtc": receipt["capturedAtUtc"],
        "case": {
            "id": receipt["sharedExperiment"]["caseId"],
            "cohort": "sub18-plumbing",
            "role": "plumbing",
            "labelsHeldOut": True,
        },
        "lane": {"id": "A", "algorithmId": "halfflow-cc-a", "historicalControl": False},
        "status": "diagnostic",
        "provenance": {
            "repositoryHead": receipt["repository"]["headAtFreeze"],
            "workingTreePolicy": receipt["repository"]["workingTreePolicy"],
            "sourceSha256": receipt["sourceSha256"],
            "runtimeClass": "schema-validation",
            "threads": 1,
            "randomSeed": None,
        },
        "inputs": {
            "fixedImage": schema_artifact("fixed-image"),
            "movingImage": schema_artifact("moving-image"),
            "optimizationSupport": [schema_artifact("fixed-mask")],
            "evaluationOnly": [schema_artifact("moving-mask", evaluation_only=True)],
        },
        "affine": {
            "id": receipt["sharedExperiment"]["suppliedAffine"]["id"],
            "sourceArtifact": schema_artifact("common-supplied-affine"),
            "suppliedTo": ["scalafim", "ants"],
            "conversionStatus": "unverified",
        },
        "configuration": {
            "acceptedStepBudget": 24,
            "levels": receipt["sharedExperiment"]["acceptedStepBudget"]["levels"],
            "metric": {},
            "step": {},
            "geometry": {},
            "flow": {},
            "controllers": {},
        },
        "objective": {
            "loss": {
                "before": unavailable("unitless"),
                "after": unavailable("unitless"),
            }
        },
        "anatomy": [],
        "support": {
            "activeFraction": unavailable("fraction"),
            "edgeActiveFraction": unavailable("fraction"),
            "minimumVariance": unavailable("intensity-squared"),
        },
        "topology": {"forward": topology_arm, "backward": topology_arm, "admitted": False},
        "inverse": {
            "exportStatus": "not-requested",
            "failureReason": None,
            "forwardThenBackward": error_summary,
            "backwardThenForward": error_summary,
        },
        "controller": {
            "accepted": 0,
            "attempted": 0,
            "retries": 0,
            "rejectionReasonCounts": {},
            "attemptsArtifact": schema_artifact("historical-ants-receipt"),
        },
        "performance": {
            "phasesSeconds": {},
            "endToEndSeconds": unavailable("seconds"),
            "peakRssBytes": unavailable("bytes"),
            "allocatedBytes": unavailable("bytes"),
        },
        "artifacts": [],
        "claims": ["schema validation example only"],
    }
    validate_json_schema(schema_path, contract["schemaVersion"], example)
    schema = load_json(schema_path)
    required = set(schema["required"])
    for group in contract["requiredDiagnosticGroups"]:
        require(group in required, f"result schema does not require diagnostic group: {group}")


def validate(receipt_path: Path, live_sources: bool = False) -> None:
    receipt = load_json(receipt_path)
    require(receipt.get("schema") == "scalafim-half-flow-cc-g0-freeze-v1", "unexpected G0 receipt schema")
    require(receipt.get("epic") == "bd-01KY514GPNG0P1W5AJ1TW96CEP", "epic id mismatch")
    require(receipt.get("gate") == "bd-01KY517059SYQ3429D6G4WSNH5", "gate id mismatch")
    validate_artifacts(receipt)
    validate_sources(receipt, live_sources)
    validate_protocol_artifacts(receipt, live_sources)
    validate_lanes(receipt)
    validate_fairness(receipt)
    validate_result_contract(receipt)


def expect_failure(action: Any, message_fragment: str) -> None:
    try:
        action()
    except ValueError as error:
        require(message_fragment in str(error), f"unexpected self-test failure: {error}")
        return
    raise ValueError(f"self-test unexpectedly passed: {message_fragment}")


def self_test(receipt_path: Path) -> None:
    receipt = load_json(receipt_path)

    leaked = copy.deepcopy(receipt)
    leaked["sharedExperiment"]["optimizationInputs"].append("moving-mask")
    expect_failure(
        lambda: validate_fairness(leaked),
        "moving evaluation mask leaked into optimization",
    )

    confounded = copy.deepcopy(receipt)
    confounded["lanes"][1]["metric"] = "different-metric"
    expect_failure(
        lambda: validate_lanes(confounded),
        "lanes A/B differ in metric",
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--receipt", type=Path, default=DEFAULT_RECEIPT)
    parser.add_argument(
        "--live-sources",
        action="store_true",
        help="also require the current checkout to match the historical frozen source hashes",
    )
    args = parser.parse_args()
    receipt = args.receipt.resolve()
    validate(receipt, live_sources=args.live_sources)
    self_test(receipt)
    mode = "live-source" if args.live_sources else "historical"
    print(f"HalfFlow-CC G0 protocol valid ({mode}): {receipt}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
