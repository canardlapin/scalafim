#!/usr/bin/env python3
"""Independent protocol controls; no production statistic or RNG implementation.

The seed-path reference below reproduces the published resample4s integer
derivation contract for cross-language checking. R owns simulator draws.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
from typing import Any

NAMESPACE = "scalafim/umvpa/inference-calibration/v1"
PHASES = ("fixture", "simulator", "pilot", "confirmation")
COUNTS = {"fixture": 1, "simulator": 10000, "pilot": 200}
CONFIRMATION_COUNTS = {"null": 10000, "alternative": 5000, "refusal": 10000}
DRAWS = {"fixture": None, "simulator": 0, "pilot": 199, "confirmation": 1999}
BOOTSTRAPS = 9999
MASK64 = (1 << 64) - 1
STAGES = {"noise": 101, "fit": 102, "resampling": 103, "censor": 104, "bootstrap": 105}
CRITERIA = {
    "alpha": .05, "type_i_cp90": [.035, .065], "coverage_cp90": [.935, .965],
    "fdp_upper95": .060, "standardized_bias_boot90": [-.05, .05],
    "reported_empirical_variance_boot90": [.90, 1.10], "standard_power_lower95": .80,
    "strong_minus_weak_boot95_lower": 0.0, "bootstrap_draws": BOOTSTRAPS,
    "mean_qa_residual_sd": .02, "covariance_qa_absolute": .03,
    "noiseless_combined_tolerance": 1e-12, "oracle_combined_tolerance": 1e-10,
}


class ProtocolError(ValueError):
    pass


def canonical(value: Any) -> bytes:
    return (json.dumps(value, sort_keys=True, separators=(",", ":"), allow_nan=False) + "\n").encode()


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def protocol_seed(phase: str, scenario: str, ordinal: int) -> tuple[str, int]:
    if phase not in PHASES:
        raise ProtocolError("phase must be fixture, simulator, pilot or confirmation")
    if not scenario or "\0" in scenario:
        raise ProtocolError("scenario must be nonempty and contain no NUL")
    if type(ordinal) is not int or ordinal < 0:
        raise ProtocolError("dataset ordinal must be a nonnegative integer")
    raw = (NAMESPACE + "\0" + phase + "\0" + scenario + "\0" + str(ordinal)).encode("utf-8")
    digest = hashlib.sha256(raw).digest()
    seed = int.from_bytes(digest[:8], "big") & ((1 << 63) - 1)
    return digest.hex(), seed or 1


def mix64(value: int) -> int:
    """Independent integer reference for resample4s seed-path/v1, not a RNG."""
    value &= MASK64
    value = ((value ^ (value >> 30)) * 0xBF58476D1CE4E5B9) & MASK64
    value = ((value ^ (value >> 27)) * 0x94D049BB133111EB) & MASK64
    return value ^ (value >> 31)


def child_seed(root: int, path: list[tuple[int, int]]) -> int:
    if not path or any(type(t) is not int or type(i) is not int or i < 0 for t, i in path):
        raise ProtocolError("seed paths require named domains and nonnegative ordinals")
    value = mix64((root & MASK64) ^ 0x736565642D706174)
    value = mix64(value + 0x632BE59BD9B4E019 + len(path))
    for tag, ordinal in path:
        value = mix64(value + 0x8CB92BA72F3D8DD7 + tag)
        value = mix64(value + 0x9E3779B185EBCA87 + ordinal)
    return value if value < (1 << 63) else value - (1 << 64)


def r_state(seed: int) -> list[int]:
    """Versioned external R CMRG initializer; full child seed remains decimal text.

    Six nonzero positive signed-32-bit words are inside both CMRG moduli.
    R performs all random draws. This expansion neither changes the protocol
    root seed nor substitutes a 32-bit truncation for its identity.
    """
    text = "external-R-CMRG-sha256-state/v1\0" + str(seed)
    raw = hashlib.sha256(text.encode()).digest()
    return [int.from_bytes(raw[4*i:4*i+4], "big") % 2147483646 + 1 for i in range(6)]


def seed_record(phase: str, scenario: str, ordinal: int) -> dict[str, Any]:
    digest, root = protocol_seed(phase, scenario, ordinal)
    children = {name: child_seed(root, [(tag, 0)]) for name, tag in STAGES.items()}
    return {
        "phase": phase, "scenario_id": scenario, "dataset_index": ordinal,
        "sha256_utf8": digest, "root_seed64": str(root),
        "child_seed_algorithm": "seed-path/v1",
        "child_seeds64": {name: str(value) for name, value in children.items()},
        "r_noise_state": r_state(children["noise"]),
        "r_noise_initializer": "external-R-CMRG-sha256-state/v1",
    }


def dataset_count(phase: str, cell: dict[str, Any]) -> int:
    if phase == "confirmation":
        try:
            return CONFIRMATION_COUNTS[cell["rate_class"]]
        except KeyError as error:
            raise ProtocolError("confirmation cell needs null/alternative/refusal rate_class") from error
    if phase not in COUNTS:
        raise ProtocolError("unknown phase")
    return COUNTS[phase]


def actual_draws(phase: str, cell: dict[str, Any]) -> int | None:
    mechanism = cell["reference_mechanism"]
    if mechanism in ("analytic-known-gaussian", "typed-refusal", "simulator-only"):
        return None if phase != "simulator" else 0
    if mechanism != "fixed-nonidentity-randomization":
        raise ProtocolError("unrecognized reference mechanism")
    if phase == "fixture":
        return cell.get("fixture_nonidentity_draws")
    return DRAWS[phase]


def file_locks(repository: Path, paths: list[str]) -> dict[str, str]:
    root = repository.resolve()
    result = {}
    for name in paths:
        path = (root / name).resolve()
        if not path.is_relative_to(root) or not path.is_file():
            raise ProtocolError("source lock must name an existing file inside the checkout: " + name)
        result[name] = sha256_bytes(path.read_bytes())
    return result


def validate_manifest(manifest: dict[str, Any], repository: Path, phase: str, operation: str = "execute") -> None:
    if manifest.get("namespace") != NAMESPACE or manifest.get("criteria") != CRITERIA:
        raise ProtocolError("namespace or frozen criteria changed")
    if manifest.get("confirmation_counts") != CONFIRMATION_COUNTS or manifest.get("draws") != DRAWS:
        raise ProtocolError("dataset or transform budgets changed")
    cells = manifest.get("cells", [])
    if not cells or len({c["id"] for c in cells}) != len(cells):
        raise ProtocolError("nonempty, unique named cells are required")
    for cell in cells:
        dataset_count(phase, cell)
        actual_draws(phase, cell)
        if cell.get("claim") == "C2" and cell.get("availability") != "unsupported":
            raise ProtocolError("complete selection-aware inference is not implemented")
    locks = manifest.get("source_locks", {})
    if phase == "confirmation" and operation != "plan":
        if manifest.get("status") != "frozen-confirmation-ready":
            raise ProtocolError("confirmation is quarantined until explicitly frozen")
        if not locks or file_locks(repository, list(locks)) != locks:
            raise ProtocolError("confirmation sources differ from the frozen lock")
        prerequisites = manifest.get("prerequisites", {})
        for gate in ("cross_language_seed_fixture", "simulator_qa", "pilot_feasibility", "independent_oracles"):
            if prerequisites.get(gate) != "passed":
                raise ProtocolError("confirmation prerequisite missing: " + gate)
        if not manifest.get("resource_approval"):
            raise ProtocolError("confirmation requires a source-bound resource approval")
        if manifest.get("inventory_complete") is not True:
            raise ProtocolError("the declared campaign inventory is not complete")
        if any(c.get("definition_status") != "frozen" for c in cells):
            raise ProtocolError("unresolved truth/HRF/nuisance parameters cannot enter confirmation")
    if operation == "execute" and phase in ("simulator", "pilot") and not manifest.get("resource_approval"):
        raise ProtocolError("simulator/pilot execution requires an explicit resource approval")
    if operation == "execute" and phase in ("simulator", "pilot"):
        if not locks or file_locks(repository, list(locks)) != locks:
            raise ProtocolError("simulator/pilot source lock is absent or changed")


def summarize(cell: dict[str, Any], phase: str, rows: list[dict[str, Any]]) -> dict[str, Any]:
    """Retain every planned dataset; identical retries cannot inflate counts."""
    expected = dataset_count(phase, cell)
    unique = {}
    retries = 0
    for row in rows:
        ordinal = row.get("dataset_index")
        if type(ordinal) is not int or not 0 <= ordinal < expected:
            raise ProtocolError("dataset index outside the fixed population")
        if row.get("root_seed64") != str(protocol_seed(phase, cell["id"], ordinal)[1]):
            raise ProtocolError("dataset seed does not match its immutable assignment")
        if ordinal in unique:
            if canonical(unique[ordinal]) != canonical(row):
                raise ProtocolError("conflicting retry")
            retries += 1
        else:
            unique[ordinal] = row
    missing = sorted(set(range(expected)) - set(unique))
    statuses = {"evaluated": 0, "refused": 0, "failed": 0, "incomplete": 0}
    contradiction_indices = []
    for ordinal, row in sorted(unique.items()):
        status = row.get("status")
        if status not in statuses:
            raise ProtocolError("unknown dataset status")
        statuses[status] += 1
        if status != "evaluated":
            if row.get("p_values") is not None or row.get("reject") is not None:
                raise ProtocolError("failed/refused/incomplete records cannot carry inferential outputs")
            if not row.get("reason"):
                raise ProtocolError("unavailable records need a retained reason")
            continue
        if row.get("completed_draws") != actual_draws(phase, cell):
            raise ProtocolError("actual reference count differs from the fixed count")
        if not row.get("family_complete", False):
            raise ProtocolError("evaluated family is incomplete")
        members = cell.get("members")
        if members is not None and row.get("member_ids") != members:
            raise ProtocolError("evaluated family membership/order changed")
        if members is not None and len(row.get("reject", [])) != len(members):
            raise ProtocolError("evaluated decision vector omits a family member")
        probabilities = row.get("p_values")
        if probabilities is not None and any(type(x) not in (int, float) or not math.isfinite(x) or not 0 < x <= 1 for x in probabilities):
            raise ProtocolError("nonfinite or invalid reference probabilities")
        declared = row.get("declared_population_null", [])
        actual = row.get("actual_conditional_null", declared)
        if len(declared) != len(actual):
            raise ProtocolError("truth classifications have different membership")
        if any(a and not b for a, b in zip(declared, actual)):
            contradiction_indices.append(ordinal)
    unsupported = cell.get("availability") == "unsupported"
    complete = not missing and len(unique) == expected
    operationally_complete = complete and not statuses["failed"] and not statuses["incomplete"]
    if unsupported:
        outcome = "refusal-contract-complete" if operationally_complete and statuses["refused"] == expected else "failed-refusal-contract"
    elif not operationally_complete or statuses["refused"]:
        outcome = "unavailable-incomplete-or-failed"
    elif contradiction_indices:
        outcome = "unavailable-population-conditional-estimand-contradiction"
    else:
        outcome = "ready-for-independent-rate-adjudication"
    return {
        "scenario_id": cell["id"], "phase": phase, "expected_datasets": expected,
        "unique_completed_datasets": len(unique), "identical_retries": retries,
        "statuses": statuses, "missing_indices": missing,
        "estimand_contradiction_indices": contradiction_indices,
        "original_population_labels_preserved": True, "primary_denominator": expected,
        "outcome": outcome,
        "scientific_release": "not-adjudicated",
    }


def proposal() -> dict[str, Any]:
    """Declare all mandatory families; unresolved population details stay visible."""
    cells = []
    roots = {
        "R0": [0, 0, 0, 0], "R1": [.5, 0, 0, 0], "R2": [.5, .3, 0, 0],
        "R3": [.5, .3, .2, 0], "R4": [.5, .3, .2, .12],
    }
    for n in (80, 160):
        for p, q in ((6, 4), (4, 6)):
            for family, correlations in roots.items():
                for nuisance in ("intercept", "three-column"):
                    for rate in (("null", "alternative") if family in ("R1", "R2", "R3") else (("null",) if family == "R0" else ("alternative",))):
                        cells.append({
                            "id": f"rank.{family}.n{n}.p{p}.q{q}.{nuisance}.{rate}",
                            "procedure": "rank", "claim": "C1", "rate_class": rate,
                            "reference_mechanism": "fixed-nonidentity-randomization",
                            "availability": "candidate", "definition_status": "frozen" if nuisance == "intercept" else "unresolved",
                            "parameters": {"n": n, "p": p, "q": q, "correlations": correlations, "nuisance": nuisance},
                            "gap": None if nuisance == "intercept" else "freeze nuisance-correlation meaning and exact population covariance before pilot",
                        })
    for family in roots:
        cells.append({
            "id": f"rank.{family}.repeat40x2.refusal", "procedure": "rank", "claim": "C1",
            "rate_class": "refusal", "reference_mechanism": "typed-refusal",
            "availability": "unsupported", "definition_status": "unresolved",
            "parameters": {"subjects": 40, "rows_per_subject": 2, "correlations": roots[family]},
            "gap": "no admitted blocked residual action; freeze the repeated covariance and nuisance specification",
        })
    for design, n, p in (("IID80", 80, 256), ("IID240", 240, 2048), ("TIME1", 576, 256), ("REPEAT24", 9216, 256), ("REPEAT80", 30720, 256)):
        for family in ("V0", "V1", "V2", "V3", "V4", "V5"):
            cells.append({
                "id": f"voxel.{family}.{design}.omnibus",
                "procedure": "voxel-omnibus", "claim": "C1",
                "rate_class": "refusal" if family == "V5" else ("null" if family in ("V0", "V1", "V3", "V4") else "alternative"),
                "reference_mechanism": "fixed-nonidentity-randomization",
                "availability": "pending-M4.08", "definition_status": "unresolved",
                "parameters": {"design": design, "n": n, "voxels": p, "components": 4, "truth_family": family},
                "gap": "freeze source-bound M4.08 factory, all variants and temporal HRF/TR/fourth task column where relevant",
            })
    for design, n in (("IID80", 80), ("IID240", 240)):
        for rho in (0.0, .15, .30, .50):
            cells.append({
                "id": f"component.association.{design}.rho{rho:.2f}",
                "procedure": "association", "claim": "C1", "rate_class": "null" if rho == 0 else "alternative",
                "reference_mechanism": "fixed-nonidentity-randomization", "availability": "candidate",
                "definition_status": "unresolved", "parameters": {"n": n, "rho": rho, "component_correlation": .50},
                "gap": "freeze population covariance/nuisance meanings and independent discovery projections",
            })
        for delta in (0.0, .01, .04, .09):
            cells.append({
                "id": f"component.incremental.{design}.deltaR2{delta:.2f}",
                "procedure": "incremental", "claim": "P1", "rate_class": "null" if delta == 0 else "alternative",
                "reference_mechanism": "analytic-known-gaussian", "availability": "candidate",
                "definition_status": "unresolved",
                "parameters": {"n": n, "population_delta_r_squared": delta, "component_correlation": .50},
                "gap": "population zero is not necessarily fixed-trained-head conditional theta<=0; preserve contradictions",
            })
    for claim, reason in (
        ("C2", "selection-aware pipeline is not implemented"),
        ("C1-temporal-row-shuffle", "time rows cannot be freely shuffled"),
        ("C1-unknown-exposure", "unknown or observed confirmation cannot be untouched"),
        ("C1-incomplete-family", "failed/omitted members prevent complete-family inference"),
        ("C1-mixed2r", "fixed Gaussian prediction has no marginal CDF/p or justified shared null action"),
        ("C1-voxel-component-slice", "global shuffle does not establish partial component-null invariance"),
        ("C1-FDR", "no qualified marginal p/FDR reference for this scope"),
    ):
        cells.append({
            "id": "unsupported." + claim, "procedure": "refusal", "claim": "C2" if claim == "C2" else "C1",
            "rate_class": "refusal", "reference_mechanism": "typed-refusal", "availability": "unsupported",
            "definition_status": "unresolved", "parameters": {}, "gap": reason,
        })
    return {
        "schema": 1, "status": "proposal-not-confirmation-ready", "namespace": NAMESPACE,
        "criteria": CRITERIA, "confirmation_counts": CONFIRMATION_COUNTS, "draws": DRAWS,
        "source_locks": {}, "resource_approval": None,
        "prerequisites": {name: "pending" for name in ("cross_language_seed_fixture", "simulator_qa", "pilot_feasibility", "independent_oracles")},
        "cells": cells, "inventory_complete": False,
        "catalog_boundary": "Proposal retains unresolved protocol definitions; full innovation/phi/censor/action Cartesian inventory must be frozen before confirmation.",
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    seed = sub.add_parser("seed")
    seed.add_argument("phase", choices=PHASES); seed.add_argument("scenario"); seed.add_argument("ordinal", type=int)
    vectors = sub.add_parser("fixture"); vectors.add_argument("--output", type=Path, required=True)
    template = sub.add_parser("proposal"); template.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    if args.command == "seed":
        print(json.dumps(seed_record(args.phase, args.scenario, args.ordinal), indent=2))
    elif args.command == "proposal":
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_bytes(canonical(proposal()))
    else:
        examples = [
            ("fixture", "seed-contract.ascii", 0), ("fixture", "seed-contract.ascii", 1),
            ("fixture", "unicode.κ", 0), ("simulator", "rank.R0.n80.p6.q4.intercept.null", 17),
            ("pilot", "voxel.V0.IID80.omnibus", 199),
            ("confirmation", "seed-contract.digest-only-not-a-study", 0),
        ]
        artifact = {"schema": 1, "scope": "root/child seed and deterministic analytic fixtures only; no study confirmation exposure",
                    "namespace": NAMESPACE, "seeds": [seed_record(*item) for item in examples],
                    "upstream_seed_path_reference": {"root": "90210", "path": [[1, 17], [2, 3]], "child": "-3497511708555549203"}}
        args.output.parent.mkdir(parents=True, exist_ok=True); args.output.write_bytes(canonical(artifact))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
