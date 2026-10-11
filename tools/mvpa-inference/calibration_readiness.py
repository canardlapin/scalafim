#!/usr/bin/env python3
"""Report written calibration requirements and refuse unbound confirmation.

No generator, build or statistical evaluation is launched. An evidence receipt
is a reviewable input, not a signature or proof of scientific qualification.
"""
from __future__ import annotations

import argparse
import json
import math
import re
import shutil
import subprocess
from pathlib import Path

from calibration_protocol import (
    CRITERIA, NAMESPACE, ProtocolError, canonical, dataset_count,
    file_locks, proposal, sha256_bytes, validate_metric_bindings,
)

PROTOCOL = "docs/plans/unified-mvpa-inference-calibration-protocol.md"
PACKET = "docs/verification/umvpa-inference-calibration-20261007"
RANK_POPULATION = "docs/plans/unified-mvpa-rank-population-v1.md"
RANK_METRICS = "docs/plans/unified-mvpa-rank-metric-bindings-v1.md"
# Current rank adapter closure. Record fields come from CalibrationProtocolSupport
# (record_schema 2) and RankConfirmationSuite; calibration_protocol validates them.
RANK_SOURCES = [
    "modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/pattern/RankConfirmation.scala",
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/inference/CalibrationProtocolSupport.scala",
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/inference/RankConfirmationSuite.scala",
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/analysis/CalibrationBindings.scala",
    "tools/mvpa-inference/rank_population.R", RANK_POPULATION, RANK_METRICS,
]
REQUIREMENTS = {
    "IID80": {"section": "4.1", "axes": {"innovations": ["Gaussian", "standardized-t5", "centered-log-normal"]},
              "gaps": ["log-normal scale parameter", "target joint covariance and discovery projection receipts"]},
    "IID240": {"section": "4.1", "axes": {"nuisance_columns": ["intercept", "two-fixed-columns"]},
               "gaps": ["which target columns each nuisance correlates .4 with", "fixed design values/full joint covariance", "qualified nuisance action"]},
    "TIME1": {"section": "4.1,6", "axes": {"phi": [0, .4, .8], "censoring": ["none", "predeclared-10-percent", "motion-linked-adverse"]},
              "gaps": ["frozen HRF parameters and TR", "fourth task column (only three specified)", "motion population and nuisance SD .30 covariance meaning", "initial AR state", "which adverse-censor combinations are mandatory", "temporal covariance/reference qualification"]},
    "REPEAT24": {"section": "4.1,6", "axes": {"subjects": [24], "sessions": [2], "runs_per_session": [2], "phi": [.4]},
                 "gaps": ["TIME1 task/motion gaps", "full cross-session/run slope covariance interpreting .4", "censor variants not explicitly enumerated", "whole-subject action qualification"]},
    "REPEAT80": {"section": "4.1,6", "axes": {"subjects": [80], "phi": [.4, .8], "censoring": ["predeclared-10-percent"]},
                 "gaps": ["TIME1 task/motion gaps", "full repeated random-effect covariance", "subject action/reference qualification"]},
    "voxel-families": {"section": "4.2", "axes": {"truth": ["V0", "V1", "V2", "V3", "V4", "V5"], "family": ["all-p*r-components", "p-omnibus", "each-component-p-slice"], "multiplicity": ["max-statistic", "BH-after-valid-marginal-p"]},
                       "gaps": ["which truth/design/innovation/family combinations form primary vs stress cells", "V5 rounding and nonfinite variants/identities", "partial component-null action", "marginal/FDR qualification", "source-bound omnibus population factory"]},
    "association": {"section": "4.2", "axes": {"rho": [0, .15, .30, .50], "component_offdiagonal_correlation": [.50]},
                    "gaps": ["full brain-target covariance and sign conventions", "nuisance covariance meanings", "independent discovery projection population"]},
    "incremental": {"section": "4.2", "axes": {"population_delta_r_squared": [0, .01, .04, .09], "component_offdiagonal_correlation": [.50], "reduced_head": ["training-refit", "coefficient-deletion-adverse"]},
                    "gaps": ["training population/sample size and head fitting choices", "conditional fixed-head theta null vs population deltaR2 null", "full response covariance/loss metric", "common mixed-family action and valid marginal CDF"]},
    "rank": {"section": "4.3,5,8.2", "axes": {"truth": ["R0", "R1", "R2", "R3", "R4"], "n": [80, 160], "dimensions": [[6, 4], [4, 6]], "nuisance": ["intercept", "three-column"], "repeated": ["40-independent-blocks-of-2"]},
             "gaps": ["repeated block covariance and compatible BLUS action", "repeated combinations with dimension/nuisance variants"],
             "resolved": {"three-column nuisance population": RANK_POPULATION, "raw-stage versus closed metric binding": RANK_METRICS}},
    "rank-adverse": {"section": "4.3,5", "axes": {"counterfactual": ["null-space-omission", "one-step", "plain-residual-permutation", "single-global-shuffle"]},
                     "gaps": ["explicit retained scenario IDs and source-bound implementations (not valid release methods)"]},
    "group-grid": {"section": "7", "axes": {"n": [8, 20, 80], "mean_standardized_effect": [0, .20, .50], "tau_squared": [0, .04, .20], "variance_profile": ["equal-.25", "spread-.04-to-1", "reverse", "estimated-df"], "estimated_df": [8, 25, 50], "nuisance": ["intercept", "balanced-binary", "quarter/reverse-leverage", "three-column-continuous"], "innovations": ["Gaussian", "symmetric-t5", "centered-skewed-log-normal"]},
                   "gaps": ["standardization scale defining .20/.50", "exact nuisance design/leverage values", "estimated-df base variance profile", "log-normal scale", "joint component/effect/variance covariance", "cross-product and primary/stress inventory review"]},
    "group-adverse": {"section": "7.1", "axes": {"case": ["GADV-DL-PM", "GADV-DF8", "GADV-NUISANCE", "GADV-FEASIBLE-WEIGHT", "GADV-FIRSTLEVEL", "GADV-MAXNULL"]},
                      "gaps": ["refresh native-owner grids/receipts and method scope", "paired effect/variance and inverse-moment df bootstrap executable adjudicator", "Monte Carlo adverse controls; deterministic GADV-DL-PM/GADV-DF8 numerical oracles exist but are not rate evidence"],
                      "deterministic_oracles": "tools/mvpa-inference/generate_group_oracle.R"},
    "refusal-exposure-completeness": {"section": "1,2,6,8.2", "axes": {"case": ["selection-aware-C2", "temporal-row-shuffle", "unknown/observed-exposure", "failed/omitted-member", "nonfinite-input", "rank-loss", "incompatible-block", "too-few-transforms", "mixed2r", "component-slice", "FDR-without-valid-p"]},
                                     "gaps": ["each refusal must have actual source-bound executable adverse inputs; a catalog string is not a 10000-replicate refusal result"]},
    "threshold-exact": {"section": "7.1,8.2,9", "axes": {"case": ["identity-and-plus-one", "ties", "attainable-alpha-boundaries", "finite-extrema", "19-and-nextUp19"]},
                        "gaps": ["bind retained public cutoff/adjusted-p independent fixtures to current sources"]},
}


def cell_digest(cells):
    return sha256_bytes(canonical(sorted(cells, key=lambda cell: cell["id"])))


def _artifact(repository, reference):
    if not isinstance(reference, dict) or set(reference) != {"path", "sha256"}:
        raise ProtocolError("evidence reference requires exact path and sha256")
    observed = file_locks(repository, [reference["path"]])
    if observed[reference["path"]] != reference["sha256"]:
        raise ProtocolError("evidence artifact changed: " + reference["path"])
    value = json.loads((repository / reference["path"]).read_text())
    if not isinstance(value, dict):
        raise ProtocolError("evidence artifact must be a JSON object")
    return value


def _positive(value):
    return type(value) in (int, float) and math.isfinite(value) and value > 0


def _provider_pins(repository):
    text = (repository / "build.sbt").read_text()
    return dict(re.findall(r'lazy val (\w+Revision)\s*=\s*"([a-f0-9]{40})"', text))


def _validate_loaded_providers(runtime, repository):
    loaded = runtime.get("resolved_provider_sources", {})
    if not isinstance(loaded, dict):
        return False
    pins = _provider_pins(repository)
    foundation = ("galeRevision", "multivarRevision", "resample4sRevision", "alderRevision")
    if any(name not in pins for name in foundation):
        return False
    for name in pins:
        value = loaded.get(name, {})
        if not isinstance(value, dict) or name not in pins or value.get("revision") != pins[name] or value.get("status") != "verified-loaded-source" or not isinstance(value.get("source_locks"), dict) or not value["source_locks"]:
            return False
        # Provider checkouts are outside ScalaFIM, so their explicitly recorded
        # absolute files have a separate hash boundary from repository sources.
        for path, expected in value["source_locks"].items():
            source = Path(path)
            if not source.is_absolute() or sha256_bytes(source.read_bytes()) != expected:
                return False
    return True


def admission_errors(manifest, repository):
    """Fail closed on incomplete authority, evidence, runtime or resource locks.

    This gate precedes confirmation and only requires seed, generator, oracle
    and feasibility evidence. It does not demand that the candidate already
    passed the scientific confirmation it is intended to evaluate.
    """
    errors = []
    cells = manifest.get("cells", [])
    ids = sorted(cell["id"] for cell in cells)
    digest = cell_digest(cells)
    locks = manifest.get("source_locks", {})
    if not locks or PROTOCOL not in locks:
        errors.append("protocol-and-source-lock-incomplete")
    required = [PROTOCOL, "build.sbt", "project/build.properties", "project/plugins.sbt",
                "tools/mvpa-inference/calibration_protocol.py", "tools/mvpa-inference/calibration_readiness.py",
                "tools/mvpa-inference/generate_known_truth.R", "tools/mvpa-inference/qualify_known_truth.R"]
    rank_cells = [cell for cell in cells if cell.get("procedure") == "rank" and cell.get("availability") != "unsupported"]
    if rank_cells:
        required += RANK_SOURCES
    if any(path not in locks for path in required):
        errors.append("method-or-generator-source-lock-incomplete")
    for cell in rank_cells:
        try:
            validate_metric_bindings(cell, required=True)
        except ProtocolError:
            errors.append("rank-metric-bindings-incomplete:" + str(cell.get("id")))
    admission = manifest.get("confirmation_admission", {})
    if not isinstance(admission, dict):
        return errors + ["confirmation-admission-must-be-an-object"]
    if admission.get("cell_digest") != digest:
        errors.append("scenario-lock-absent-or-changed")
    metric = admission.get("metric_binding")
    approval = manifest.get("resource_approval") or {}
    if not isinstance(approval, dict):
        approval = {}
    try:
        binding = _artifact(repository, metric)
        if binding.get("status") != "authorized-before-confirmation" or binding.get("namespace") != NAMESPACE or not binding.get("authority_reference") or binding.get("confirmation_exposure") is not False:
            errors.append("metric-binding-authority-unresolved")
        if binding.get("protocol_sha256") != locks.get(PROTOCOL) or binding.get("cell_digest") != digest:
            errors.append("metric-binding-protocol-or-scenario-changed")
        # Per-cell bindings are part of the cell digest; the authorization must
        # also bind the exact written binding specification it relied on.
        if rank_cells and (RANK_METRICS not in locks or binding.get("metric_specification_sha256") != locks.get(RANK_METRICS)):
            errors.append("rank-metric-specification-unbound")
    except (ProtocolError, OSError, ValueError, TypeError):
        errors.append("metric-binding-evidence-absent-or-changed")
    evidence = admission.get("prerequisite_evidence", {})
    if not isinstance(evidence, dict):
        evidence = {}
    for gate, phase in (("cross_language_seed_fixture", "fixture"), ("simulator_qa", "simulator"), ("pilot_feasibility", "pilot"), ("independent_oracles", "fixture")):
        try:
            receipt = _artifact(repository, evidence.get(gate))
            if receipt.get("status") != "passed" or receipt.get("phase") != phase or receipt.get("source_locks") != locks or receipt.get("cell_ids") != ids:
                errors.append(gate + "-scope-or-source-unqualified")
            if gate in ("simulator_qa", "pilot_feasibility") and receipt.get("datasets_per_cell") != dataset_count(phase, {}):
                errors.append(gate + "-fixed-count-unsatisfied")
            if gate == "pilot_feasibility" and (receipt.get("random_nonidentity_draws") != 199 or receipt.get("measured_wall_seconds") is None or receipt.get("measured_peak_rss_bytes") is None):
                errors.append("pilot-feasibility-unmeasured")
        except (ProtocolError, OSError, ValueError, TypeError):
            errors.append(gate + "-evidence-absent-or-changed")
    try:
        inventory = _artifact(repository, admission.get("inventory_review"))
        if inventory.get("status") != "reviewed-complete" or inventory.get("cell_digest") != digest or inventory.get("protocol_sha256") != locks.get(PROTOCOL) or not inventory.get("reviewer") or inventory.get("unresolved_requirements") != []:
            errors.append("inventory-review-incomplete")
    except (ProtocolError, OSError, ValueError, TypeError):
        errors.append("inventory-review-evidence-absent-or-changed")
    try:
        runtime = _artifact(repository, admission.get("runtime_lock"))
        executables = runtime.get("executables", {})
        if not isinstance(executables, dict):
            raise ProtocolError("runtime executables must be an object")
        if runtime.get("status") != "locked" or runtime.get("source_locks") != locks or not all(name in executables for name in ("java", "node", "Rscript", "python3", "sbt")):
            errors.append("runtime-lock-incomplete")
        if not _validate_loaded_providers(runtime, repository) or runtime.get("build_override_properties_recorded") is not True or not runtime.get("sbt_jvm_command"):
            errors.append("loaded-provider-or-sbt-runtime-unbound")
        effective = runtime.get("effective_resources", {})
        if not isinstance(effective, dict):
            effective = {}
        if effective.get("status") != "observed-server-config" or any(effective.get(name) != approval.get(name) for name in ("jvm_heap_bytes", "cpus", "worker_processes")):
            errors.append("effective-server-config-does-not-match-budget")
        command = runtime.get("sbt_jvm_command", [])
        heap_args = [str(argument) for argument in command if str(argument).startswith("-Xmx")]
        heap_match = re.fullmatch(r"-Xmx([0-9]+)([kKmMgG]?)", heap_args[0]) if len(heap_args) == 1 else None
        heap_bytes = int(heap_match[1]) * {"": 1, "k": 1024, "m": 1024**2, "g": 1024**3}[heap_match[2].lower()] if heap_match else None
        if not _positive(approval.get("jvm_heap_bytes")) or heap_bytes != approval.get("jvm_heap_bytes") or "-XX:ActiveProcessorCount=" + str(approval.get("cpus")) not in command:
            errors.append("effective-jvm-flags-do-not-match-budget")
        for value in executables.values():
            executable = Path(value["path"]).resolve()
            if not value.get("version") or sha256_bytes(executable.read_bytes()) != value["sha256"]:
                errors.append("runtime-executable-changed")
    except (ProtocolError, OSError, ValueError, TypeError, KeyError):
        errors.append("runtime-lock-evidence-absent-or-changed")
    metric_hash = metric.get("sha256") if isinstance(metric, dict) else None
    if approval.get("phase") != "confirmation" or sorted(approval.get("scope", [])) != ids or approval.get("source_locks") != locks or approval.get("cell_digest") != digest or approval.get("metric_binding_sha256") != metric_hash:
        errors.append("resource-approval-phase-scope-source-or-metric-unbound")
    if not approval.get("authority_reference") or approval.get("no_reduced_count_on_abort") is not True:
        errors.append("resource-authority-or-fixed-count-policy-absent")
    for name in ("worker_processes", "cpus", "max_rss_bytes", "max_wall_seconds"):
        value = approval.get(name)
        if not _positive(value):
            errors.append("resource-hard-limit-absent:" + name)
    if any(type(approval.get(name)) is not int for name in ("worker_processes", "cpus", "jvm_heap_bytes", "max_rss_bytes")):
        errors.append("resource-counts-and-bytes-must-be-integers")
    if _positive(approval.get("jvm_heap_bytes")) and _positive(approval.get("max_rss_bytes")) and approval["jvm_heap_bytes"] > approval["max_rss_bytes"]:
        errors.append("heap-budget-exceeds-process-rss-limit")
    try:
        measured = _artifact(repository, approval.get("measured_cost_receipt"))
        if measured.get("status") != "measured" or measured.get("scope") != "bounded-probes-not-full-confirmation" or measured.get("source_locks") != locks or measured.get("cell_ids") != ids or measured.get("draws") != 1999 or measured.get("includes_generation_io_bootstrap") is not True:
            errors.append("confirmation-cost-unmeasured-or-wrong-scope")
        probes = measured.get("probes", [])
        if sorted(probe.get("cell_id", "") for probe in probes) != ids:
            errors.append("measured-cost-cell-coverage-incomplete")
        by_id = {cell["id"]: cell for cell in cells}
        for probe in probes:
            if probe.get("cell_id") not in by_id or probe.get("parameters") != by_id[probe["cell_id"]].get("parameters") or type(probe.get("datasets")) is not int or probe["datasets"] <= 0 or probe.get("draws") != 1999 or probe.get("bootstrap_draws") != 9999:
                errors.append("measured-probe-dimensions-or-budget-unbound")
            if any(not _positive(probe.get(name)) for name in ("wall_seconds", "cpu_seconds", "peak_rss_bytes")):
                errors.append("measured-probe-numeric-resources-absent")
            if _positive(probe.get("peak_rss_bytes")) and _positive(approval.get("max_rss_bytes")) and probe["peak_rss_bytes"] > approval["max_rss_bytes"]:
                errors.append("measured-probe-exceeds-admitted-rss")
            observed = _artifact(repository, probe.get("process_receipt"))
            if observed.get("status") != "completed" or observed.get("resource_refusal") is not None or observed.get("source_locks") != locks or observed.get("cell_id") != probe.get("cell_id") or any(observed.get(name) != probe.get(name) for name in ("datasets", "draws", "wall_seconds", "cpu_seconds", "peak_rss_bytes")):
                errors.append("measured-probe-process-evidence-does-not-match")
    except (ProtocolError, OSError, ValueError, TypeError):
        errors.append("measured-cost-evidence-absent-or-changed")
    return errors


def source_observation(repository):
    roots = ["modules/" + name for name in ("mvpa", "mvpa-group", "mvpa-spatial", "group", "threshold")]
    paths = [PROTOCOL, "build.sbt", ".jvmopts", "project/build.properties", "project/plugins.sbt", "tools/build/sbt-warm", "docs/scenarios/fixtures/mvpa.inference-known-truth.v1.r.json"]
    for name in roots + ["tools/mvpa-inference"]:
        paths.extend(str(path.relative_to(repository)) for path in (repository / name).rglob("*") if path.is_file() and "target" not in path.parts and "__pycache__" not in path.parts and path.suffix in (".scala", ".py", ".R"))
    locks = file_locks(repository, sorted(set(paths)))
    def git(*args):
        return subprocess.check_output(["git", *args], cwd=repository, text=True).strip()
    build = (repository / "build.sbt").read_text()
    return {"git_head": git("rev-parse", "HEAD"), "git_tree_at_head": git("rev-parse", "HEAD^{tree}"),
            "source_locks": locks, "source_lock_digest": sha256_bytes(canonical(locks)),
            "method": "SHA256 of actual working-tree bytes; git HEAD/tree is provenance, not a claim that current edits were tested",
            "tracked_dirty_paths": git("diff", "--name-only").splitlines(),
            "provider_pins_declared": dict(re.findall(r'lazy val (\w+Revision)\s*=\s*"([a-f0-9]{40})"', build)),
            "provider_runtime_status": "declared pins only; actual staged resolver/overrides and loaded provider bytes require runtime receipt"}


def runtime_observation():
    result = {}
    for name, args in (("java", ["-version"]), ("node", ["--version"]), ("Rscript", ["--version"]), ("python3", ["--version"])):
        executable = shutil.which(name)
        if executable:
            completed = subprocess.run([executable, *args], capture_output=True, text=True, timeout=15)
            path = Path(executable).resolve()
            result[name] = {"path": str(path), "sha256": sha256_bytes(path.read_bytes()), "version": (completed.stdout + completed.stderr).strip(), "exit_code": completed.returncode}
        else:
            result[name] = {"status": "not-on-observer-PATH"}
    sbt = shutil.which("sbt")
    result["sbt"] = {"path": sbt, "sha256": sha256_bytes(Path(sbt).resolve().read_bytes()) if sbt else None, "version": None, "status": "launcher-only; no build started; actual server/launcher runtime unresolved"}
    return {"status": "observation-only-not-runtime-lock", "executables": result,
            "scope": "observer PATH only; JVM sbt launcher and R subprocess selection may differ; no CPU/RSS admission"}


def report(repository):
    catalog = proposal()
    rank = [cell for cell in catalog["cells"] if cell["procedure"] == "rank"]
    defined = [cell for cell in rank if cell["definition_status"] == "frozen"]
    summary_path = PACKET + "/pilot-summary.json"
    prior = json.loads((repository / summary_path).read_text())
    cells = []
    for cell in catalog["cells"]:
        is_defined = cell in defined
        cells.append(dict(cell, confirmation_datasets=dataset_count("confirmation", cell),
                          executable_generator_phases=["simulator", "fixture", "pilot"] if is_defined else [],
                          executable_confirmation=False))
    return {"schema": "scalafim/umvpa/calibration-readiness/v1", "namespace": NAMESPACE,
            "status": "confirmation-unavailable", "scientific_release": "unavailable", "confirmation_invoked": False,
            "written_requirements": REQUIREMENTS, "required_combination_policy": "Axes retain every explicit written value. Unspecified cross-products are unresolved, not invented mandatory cells or silently omitted.",
            "catalog_cells": cells, "catalog_complete": False,
            "rank_scope": {"entries": len(rank), "defined_entries": len(defined), "unresolved_or_unsupported_entries": len(rank)-len(defined),
                           "defined_confirmation_datasets": sum(dataset_count("confirmation", c) for c in defined),
                           "defined_confirmation_nonidentity_draws": sum(dataset_count("confirmation", c)*1999 for c in defined),
                           "admission": "none; parameter-defined is distinct from independently qualified or executable confirmation"},
            "source_observation": source_observation(repository), "runtime_observation": runtime_observation(),
            "historical_evidence": {"summary": summary_path, "summary_sha256": file_locks(repository, [summary_path])[summary_path],
                                    "source_commit": prior["source_commit"], "pilot_datasets": prior["total_datasets"],
                                    "source_reuse": "historical evidence unchanged; current edited bytes require fresh fixtures/source locks, never retag old runs"},
            "refusals": admission_errors(catalog, repository) + ["confirmation-generator-and-adapter-not-implemented", "full-written-inventory-unresolved"],
            "criteria_unchanged": CRITERIA}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repository", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("xb") as output:
        output.write(canonical(report(args.repository.resolve())))


if __name__ == "__main__":
    main()
