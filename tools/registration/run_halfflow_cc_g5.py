#!/usr/bin/env python3
"""Validate and stage the reproducible HalfFlow-CC G5 experiment.

This helper intentionally does not download licensed data or silently run a
partial ablation matrix.  It checks the evidence already captured in a G5
checkpoint and verifies the exact LPBA40 layout expected by the five-pair
holdout before an expensive benchmark is admitted.
"""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any


CHECKPOINT_SCHEMA = "scalafim-half-flow-cc-g5-checkpoint-v1"
MATRIX_SCHEMA = "scalafim-half-flow-cc-g5-matrix-checkpoint-v1"
IMPROVEMENT_SCHEMA = "scalafim-half-flow-cc-g5-improvement-probes-v1"
NEXT_STEPS_SCHEMA = "scalafim-half-flow-cc-g5-next-steps-v1"
CHECKPOINT_STATUS = "diagnostic-not-decision"
REQUIRED_LANES = ["A", "B", "C", "D", "E"]
LPBA_SUBJECTS = [f"S{index:02d}" for index in range(1, 7)]


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def require(condition: bool, message: str, errors: list[str]) -> None:
    if not condition:
        errors.append(message)


def validate_hashed_path(
    label: str,
    path: Path,
    expected: str,
    errors: list[str],
    *,
    optional: bool = False,
) -> None:
    if not path.is_file():
        if not optional:
            errors.append(f"{label}: missing file {path}")
        return
    actual = sha256(path)
    if actual != expected:
        errors.append(f"{label}: SHA-256 mismatch ({actual} != {expected})")


def validate_checkpoint(repo: Path, checkpoint: Path) -> int:
    payload: dict[str, Any] = json.loads(checkpoint.read_text(encoding="utf-8"))
    if payload.get("schema") == IMPROVEMENT_SCHEMA:
        return validate_improvement_probes(repo, checkpoint, payload)
    if payload.get("schema") == NEXT_STEPS_SCHEMA:
        return validate_next_steps(repo, checkpoint, payload)
    errors: list[str] = []

    schema = payload.get("schema")
    require(schema in {CHECKPOINT_SCHEMA, MATRIX_SCHEMA}, "unexpected checkpoint schema", errors)
    require(payload.get("status") == CHECKPOINT_STATUS, "checkpoint must remain non-decisional", errors)
    if schema == CHECKPOINT_SCHEMA:
        require(
            payload.get("remainingGate", {}).get("requiredLanes") == REQUIRED_LANES,
            "checkpoint must retain the complete A-E gate",
            errors,
        )
    else:
        require(list(payload.get("lanes", {}).keys()) == REQUIRED_LANES, "matrix must contain lanes A-E", errors)
    require(
        payload.get("remainingGate", {}).get("requiredLabeledDevelopmentPairs", 0) >= 5,
        "checkpoint must require at least five labeled development pairs",
        errors,
    )

    for relative, expected in payload.get("repository", {}).get("sourceSha256", {}).items():
        validate_hashed_path(f"source {relative}", repo / relative, expected, errors)

    for label, item in payload.get("inputs", {}).items():
        validate_hashed_path(label, Path(item["path"]), item["sha256"], errors)

    oracle = payload.get("affineAdmission", {}).get("antsApplyTransformsLinearImageOracle", {})
    if schema == CHECKPOINT_SCHEMA:
        positive = oracle.get("storedRasAsPullback", {})
        negative = oracle.get("inverseRasNegativeControl", {})
        require(positive.get("samples", 0) > 0, "affine oracle has no positive samples", errors)
        require(positive.get("maximumError", float("inf")) < 1e-6, "stored-RAS affine oracle failed", errors)
        require(
            negative.get("rmsError", 0.0) > 1000.0 * positive.get("rmsError", float("inf")),
            "inverse affine negative control is not discriminating",
            errors,
        )

    lane = payload.get("laneA", {}) if schema == CHECKPOINT_SCHEMA else payload.get("lanes", {}).get("A", {})
    controller = lane.get("controller", {})
    accepted = controller.get("accepted", lane.get("accepted"))
    attempted = controller.get("attempted", lane.get("attempted"))
    require(accepted == 24, "lane A did not complete its accepted-step budget", errors)
    require(attempted == 24, "lane A checkpoint is no longer the zero-rejection run", errors)
    topology = lane.get("authoritativeStateTopology", {})
    require(topology.get("nonPositive", lane.get("nonPositive")) == 0, "lane A authoritative state contains folds", errors)
    require(lane.get("export", {}).get("status") == "typed-failure", "lane A export status drifted", errors)
    if schema == CHECKPOINT_SCHEMA:
        require(
            lane.get("export", {}).get("candidateUse", "").startswith("evaluation-only"),
            "diagnostic candidate must not be described as admitted",
            errors,
        )

    comparator = payload.get("antsSameAffineComparator", {})
    supplied_sha = payload.get("inputs", {}).get("suppliedAffine", {}).get("sha256")
    if schema == CHECKPOINT_SCHEMA:
        require(comparator.get("affineEstimationDisabled") is True, "ANTs affine estimation was not disabled", errors)
        require(comparator.get("emittedAffineSha256") == supplied_sha, "ANTs did not preserve the supplied affine", errors)
        require("emulation" in comparator.get("runtimeClass", ""), "ANTs runtime class must disclose emulation", errors)

    for label, item in lane.get("artifacts", {}).items():
        if isinstance(item, dict) and "path" in item and "sha256" in item:
            validate_hashed_path(f"optional artifact {label}", Path(item["path"]), item["sha256"], errors, optional=True)
    if schema == MATRIX_SCHEMA:
        lanes = payload.get("lanes", {})
        require(lanes.get("B", {}).get("legacyInverseRejections", 0) > 0, "lane B did not isolate inverse rejection", errors)
        require(lanes.get("B", {}).get("legacyUnsafeAccepted") == 0, "lane B accepted an inverse-unsafe candidate", errors)
        require(lanes.get("D", {}).get("candidateInvalidations", 0) > 0, "lane D did not report candidate invalidation", errors)
        require(lanes.get("E", {}).get("status") == "typed-failure-before-nonlinear-optimization", "lane E status drifted", errors)
        for name, result in lanes.items():
            artifact = result.get("artifact", {})
            if "summary" in artifact and "summarySha256" in artifact:
                summary = Path(artifact["summary"])
                validate_hashed_path(f"lane {name} summary", summary, artifact["summarySha256"], errors, optional=True)
                if "attemptsSha256" in artifact:
                    validate_hashed_path(
                        f"lane {name} attempts",
                        summary.parent / "attempts.tsv",
                        artifact["attemptsSha256"],
                        errors,
                        optional=True,
                    )

    if errors:
        for error in errors:
            print(f"FAIL\t{error}")
        return 1
    print(f"PASS\tcheckpoint={checkpoint}")
    print("PASS\taffine-direction=stored-RAS-pullback")
    print("PASS\tlane-A=diagnostic-only; export=typed-failure")
    if schema == MATRIX_SCHEMA:
        print("PASS\tA-E plumbing matrix complete; remaining-gate=>=5 labeled pairs")
    else:
        print("PASS\tremaining-gate=A,B,C,D,E over >=5 labeled pairs")
    return 0


def validate_improvement_probes(repo: Path, checkpoint: Path, payload: dict[str, Any]) -> int:
    errors: list[str] = []
    require(payload.get("status") == CHECKPOINT_STATUS, "probe receipt must remain non-decisional", errors)
    require(
        payload.get("remainingGate", {}).get("requiredLabeledDevelopmentPairs", 0) >= 5,
        "probe receipt must retain the five-pair gate",
        errors,
    )
    require(
        "single-pair" in payload.get("case", {}).get("role", ""),
        "probe receipt must disclose its single-pair role",
        errors,
    )
    for relative, expected in payload.get("sources", {}).items():
        validate_hashed_path(f"source {relative}", repo / relative, expected, errors)

    probes = payload.get("probes", {})
    budget = probes.get("budget2x", {})
    require(budget.get("status") == "retained-as-multi-pair-candidate", "budget2x status drifted", errors)
    require(budget.get("accepted") == 48 and budget.get("attempted") == 48, "budget2x trace drifted", errors)
    require(budget.get("nonPositiveJacobians") == 0, "budget2x contains folds", errors)
    require(budget.get("exportStatus") == "typed-failure", "budget2x export status drifted", errors)
    artifact = budget.get("artifact", {})
    for label in ("summary", "attempts"):
        if artifact.get(label) and artifact.get(f"{label}Sha256"):
            validate_hashed_path(
                f"budget2x {label}",
                Path(artifact[label]),
                artifact[f"{label}Sha256"],
                errors,
                optional=True,
            )

    expected_rejections = {
        "softened": "rejected",
        "shrink8": "rejected",
        "fixedPoint150": "rejected",
        "oneSidedNewtonExport": "rejected-and-reverted",
        "boundedTwoSidedNewtonExport": "rejected-and-reverted",
        "globalTranslationStencil": "rejected-and-removed",
    }
    for name, expected in expected_rejections.items():
        require(probes.get(name, {}).get("status") == expected, f"{name} rejection status drifted", errors)
    require(
        payload.get("verification", {}).get("jvm", {}).get("passed", 0) >= 71,
        "JVM verification count drifted",
        errors,
    )
    require(
        payload.get("verification", {}).get("scalaJs", {}).get("passed", 0) >= 71,
        "Scala.js verification count drifted",
        errors,
    )

    if errors:
        for error in errors:
            print(f"FAIL\t{error}")
        return 1
    print(f"PASS\timprovement-probes={checkpoint}")
    print("PASS\tbudget2x=multi-pair-candidate; topology=fold-free; export=typed-failure")
    print("PASS\trejected=softened,shrink8,fixedPoint150,newton,global-translation")
    print("PASS\tremaining-gate=>=5 labeled pairs")
    return 0


def validate_next_steps(repo: Path, checkpoint: Path, payload: dict[str, Any]) -> int:
    errors: list[str] = []
    require(payload.get("status") == CHECKPOINT_STATUS, "next-steps receipt must remain non-decisional", errors)
    require(
        payload.get("remainingGate", {}).get("requiredLabeledDevelopmentPairs", 0) >= 5,
        "next-steps receipt must retain the five-pair gate",
        errors,
    )
    for relative, expected in payload.get("repository", {}).get("sourceSha256", {}).items():
        validate_hashed_path(f"source {relative}", repo / relative, expected, errors)

    retained = payload.get("retained", {})
    gate = retained.get("regriddedTopologyGate", {})
    require(gate.get("status") == "retained", "regridded topology gate status drifted", errors)
    require(gate.get("regressionOnJvmAndScalaJs") is True, "regrid regression lacks cross-platform evidence", errors)

    rejected = payload.get("rejected", {})
    for name in ("acceptedStepPlateau", "boundaryTaper", "localBlockCcProposal"):
        require(rejected.get(name, {}).get("status") == "rejected-and-removed", f"{name} status drifted", errors)
    adaptive = rejected.get("acceptedStepPlateau", {})
    require(adaptive.get("accepted") == adaptive.get("attempted") == 60, "adaptive trace drifted", errors)
    require(adaptive.get("exportP99Mm", 0.0) > adaptive.get("budget2xExportP99Mm", 1.0), "adaptive export did not regress", errors)
    taper = rejected.get("boundaryTaper", {})
    require(taper.get("attemptTraceMatchesBudget2x") is True, "taper comparison is not matched", errors)
    require(taper.get("exportP99Mm", 0.0) > taper.get("budget2xExportP99Mm", 1.0), "taper p99 did not regress", errors)
    require(taper.get("exportMaximumMm", 0.0) > taper.get("budget2xExportMaximumMm", 1.0), "taper maximum did not regress", errors)

    for probe in (adaptive, taper):
        artifact = probe.get("artifact", {})
        for label in ("summary", "attempts"):
            if artifact.get(label) and artifact.get(f"{label}Sha256"):
                validate_hashed_path(
                    f"{label} artifact",
                    Path(artifact[label]),
                    artifact[f"{label}Sha256"],
                    errors,
                    optional=True,
                )

    verification = payload.get("verification", {})
    require(verification.get("jvm", {}).get("passed") == 72, "JVM verification count drifted", errors)
    require(verification.get("scalaJs", {}).get("passed") == 72, "Scala.js verification count drifted", errors)
    require(verification.get("warnings") == 0, "verification warnings drifted", errors)

    if errors:
        for error in errors:
            print(f"FAIL\t{error}")
        return 1
    print(f"PASS\tnext-steps={checkpoint}")
    print("PASS\tretained=regridded-topology-gate; rejected=plateau,taper,local-block-cc")
    print("PASS\tJVM=72; Scala.js=72; remaining-gate=>=5 labeled pairs")
    return 0


def lpba_paths(root: Path) -> list[Path]:
    paths: list[Path] = []
    for subject in LPBA_SUBJECTS:
        directory = root / "delineation_space" / subject
        paths.extend(
            [
                directory / f"{subject}.delineation.skullstripped.hdr",
                directory / f"{subject}.delineation.skullstripped.img",
                directory / f"{subject}.delineation.structure.label.hdr",
                directory / f"{subject}.delineation.structure.label.img",
            ]
        )
    return paths


def preflight_lpba(root: Path) -> int:
    missing = [path for path in lpba_paths(root) if not path.is_file()]
    if missing:
        print(f"FAIL\tlpba40-root={root}")
        for path in missing:
            print(f"MISSING\t{path}")
        print("BLOCKED\tLPBA40 requires manual LONI license acceptance; this tool never downloads it")
        return 2
    print(f"PASS\tlpba40-root={root}")
    print("PASS\tdevelopment-pairs=S02-S06-to-S01")
    return 0


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    subcommands = result.add_subparsers(dest="command", required=True)
    validate = subcommands.add_parser("validate-checkpoint")
    validate.add_argument("checkpoint", type=Path)
    validate.add_argument("--repo", type=Path, default=Path.cwd())
    preflight = subcommands.add_parser("preflight-lpba")
    preflight.add_argument("root", type=Path)
    return result


def main() -> int:
    args = parser().parse_args()
    if args.command == "validate-checkpoint":
        return validate_checkpoint(args.repo.resolve(), args.checkpoint.resolve())
    if args.command == "preflight-lpba":
        return preflight_lpba(args.root.resolve())
    raise AssertionError(args.command)


if __name__ == "__main__":
    raise SystemExit(main())
