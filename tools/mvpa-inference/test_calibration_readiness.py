#!/usr/bin/env python3
"""Adversarial admission checks; no simulations, source qualification or builds."""
import copy
import tempfile
import unittest
from pathlib import Path

from calibration_protocol import (
    NAMESPACE, ProtocolError, canonical, file_locks, proposal,
    sha256_bytes, validate_manifest,
)
from calibration_readiness import PROTOCOL, RANK_METRICS, RANK_SOURCES, REQUIREMENTS, admission_errors, cell_digest


class CalibrationReadinessTests(unittest.TestCase):
    def _bound_fixture(self, root):
        """Synthetic metadata only, never real authority or scientific evidence."""
        manifest = proposal(); manifest["cells"] = [manifest["cells"][0]]
        cells = manifest["cells"]; ids = [cells[0]["id"]]; digest = cell_digest(cells)
        paths = [PROTOCOL, "build.sbt", "project/build.properties", "project/plugins.sbt",
                 "tools/mvpa-inference/calibration_protocol.py", "tools/mvpa-inference/calibration_readiness.py",
                 "tools/mvpa-inference/generate_known_truth.R", "tools/mvpa-inference/qualify_known_truth.R"] + RANK_SOURCES
        for name in paths:
            path = root / name; path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("SYNTHETIC TEST ONLY " + name)
        (root / "build.sbt").write_text("\n".join('lazy val ' + name + ' = "' + "1" * 40 + '"' for name in ("galeRevision", "multivarRevision", "resample4sRevision", "alderRevision")))
        locks = file_locks(root, paths); manifest["source_locks"] = locks
        def artifact(name, value):
            raw = canonical(value); (root / name).write_bytes(raw)
            return {"path": name, "sha256": sha256_bytes(raw)}
        metric = artifact("metric.json", {"status": "authorized-before-confirmation", "namespace": NAMESPACE,
                         "authority_reference": "SYNTHETIC TEST ONLY", "confirmation_exposure": False,
                         "protocol_sha256": locks[PROTOCOL], "cell_digest": digest,
                         "metric_specification_sha256": locks[RANK_METRICS]})
        prereqs = {}
        for gate, phase in (("cross_language_seed_fixture", "fixture"), ("simulator_qa", "simulator"), ("pilot_feasibility", "pilot"), ("independent_oracles", "fixture")):
            receipt = {"status": "passed", "phase": phase, "source_locks": locks, "cell_ids": ids}
            if phase in ("simulator", "pilot"):
                receipt["datasets_per_cell"] = 10000 if phase == "simulator" else 200
            if phase == "pilot":
                receipt.update(random_nonidentity_draws=199, measured_wall_seconds=1., measured_peak_rss_bytes=1024)
            prereqs[gate] = artifact(gate + ".json", receipt)
        executable = root / "synthetic-executable"; executable.write_text("SYNTHETIC TEST ONLY")
        executable_lock = {"path": str(executable), "sha256": sha256_bytes(executable.read_bytes()), "version": "test-only-v1"}
        runtime = {"status": "locked", "source_locks": locks,
                   "executables": {name: executable_lock for name in ("java", "node", "Rscript", "python3", "sbt")},
                   "resolved_provider_sources": {name: {"status": "verified-loaded-source", "revision": "1" * 40,
                     "source_locks": {str(executable): executable_lock["sha256"]}} for name in ("galeRevision", "multivarRevision", "resample4sRevision", "alderRevision")},
                   "build_override_properties_recorded": True,
                   "sbt_jvm_command": [str(executable), "-Xmx512m", "-XX:ActiveProcessorCount=2"],
                   "effective_resources": {"status": "observed-server-config", "jvm_heap_bytes": 512*1024**2, "cpus": 2, "worker_processes": 1}}
        process = {"status": "completed", "resource_refusal": None, "source_locks": locks,
                   "cell_id": ids[0], "datasets": 1, "draws": 1999, "wall_seconds": 1., "cpu_seconds": .5, "peak_rss_bytes": 1024}
        probe = {name: process[name] for name in ("cell_id", "datasets", "draws", "wall_seconds", "cpu_seconds", "peak_rss_bytes")}
        probe.update(parameters=cells[0]["parameters"], bootstrap_draws=9999, process_receipt=artifact("process.json", process))
        measured = {"status": "measured", "scope": "bounded-probes-not-full-confirmation", "source_locks": locks,
                    "cell_ids": ids, "draws": 1999, "includes_generation_io_bootstrap": True, "probes": [probe]}
        manifest["confirmation_admission"] = {"cell_digest": digest, "metric_binding": metric,
                    "prerequisite_evidence": prereqs, "runtime_lock": artifact("runtime.json", runtime),
                    "inventory_review": artifact("inventory.json", {"status": "reviewed-complete", "cell_digest": digest,
                       "protocol_sha256": locks[PROTOCOL], "reviewer": "SYNTHETIC TEST ONLY", "unresolved_requirements": []})}
        manifest["resource_approval"] = {"phase": "confirmation", "scope": ids, "source_locks": locks,
                   "cell_digest": digest, "metric_binding_sha256": metric["sha256"], "authority_reference": "SYNTHETIC TEST ONLY",
                   "no_reduced_count_on_abort": True, "worker_processes": 1, "cpus": 2,
                   "jvm_heap_bytes": 512*1024**2, "max_rss_bytes": 1024**3, "max_wall_seconds": 3600,
                   "measured_cost_receipt": artifact("measured.json", measured)}
        manifest["status"] = "frozen-confirmation-ready"; manifest["inventory_complete"] = True
        manifest["prerequisites"] = {name: "passed" for name in prereqs}
        return manifest, artifact, measured, runtime

    def test_coherently_bound_fixture_admits_metadata_then_rejects_empty_cost_and_runtime_drift(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); manifest, artifact, measured, runtime = self._bound_fixture(root)
            self.assertEqual(admission_errors(manifest, root), [])
            validate_manifest(manifest, root, "confirmation")
            measured["probes"][0]["wall_seconds"] = None
            manifest["resource_approval"]["measured_cost_receipt"] = artifact("measured.json", measured)
            self.assertIn("measured-probe-numeric-resources-absent", admission_errors(manifest, root))
            runtime["sbt_jvm_command"] = ["java", "-Xmx2g", "-XX:ActiveProcessorCount=8"]
            manifest["confirmation_admission"]["runtime_lock"] = artifact("runtime.json", runtime)
            self.assertIn("effective-jvm-flags-do-not-match-budget", admission_errors(manifest, root))

    def test_parameter_defined_rank_scope_has_the_written_dimensions(self):
        cells = [cell for cell in proposal()["cells"] if cell["procedure"] == "rank"]
        defined = [cell for cell in cells if cell["definition_status"] == "frozen"]
        # n x (P,Q) x nuisance shapes; each has R0(null), R1/R2/R3(null + alternative), R4(alt).
        # Three-column nuisance is frozen by the rank population specification.
        self.assertEqual(len(defined), 2 * 2 * 2 * (1 + 6 + 1))
        self.assertEqual(len(cells), len(defined) + 5)
        self.assertEqual(sum(10000 if cell["rate_class"] == "null" else 5000 for cell in defined), 480000)
        self.assertIn("initial AR state", REQUIREMENTS["TIME1"]["gaps"])
        self.assertIn("GADV-FIRSTLEVEL", REQUIREMENTS["group-adverse"]["axes"]["case"])

    def test_rank_cells_without_frozen_metric_bindings_cannot_be_admitted(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); manifest, _, _, _ = self._bound_fixture(root)
            del manifest["cells"][0]["metric_bindings"]
            errors = admission_errors(manifest, root)
            self.assertIn("rank-metric-bindings-incomplete:" + manifest["cells"][0]["id"], errors)

    def test_boolean_prerequisites_and_hypothetical_budget_cannot_admit_confirmation(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            path = root / PROTOCOL
            path.parent.mkdir(parents=True); path.write_text("protocol evidence")
            manifest = proposal()
            manifest["cells"] = [manifest["cells"][0]]
            manifest["source_locks"] = file_locks(root, [PROTOCOL])
            manifest["status"] = "frozen-confirmation-ready"
            manifest["inventory_complete"] = True
            manifest["prerequisites"] = {gate: "passed" for gate in manifest["prerequisites"]}
            manifest["resource_approval"] = {"cpus": 4, "max_wall_seconds": 100000}
            with self.assertRaisesRegex(ProtocolError, "admission incomplete"):
                validate_manifest(manifest, root, "confirmation")
            # Planning remains available and makes no admission claim.
            validate_manifest(manifest, root, "confirmation", operation="plan")

    def test_metric_artifact_cannot_change_without_invalidating_its_frozen_hash(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            manifest = proposal(); manifest["cells"] = [manifest["cells"][0]]
            digest = cell_digest(manifest["cells"])
            manifest["source_locks"] = {PROTOCOL: "a" * 64}
            binding = {"status": "authorized-before-confirmation", "namespace": NAMESPACE,
                       "authority_reference": "test-only-review", "confirmation_exposure": False,
                       "protocol_sha256": "a" * 64, "cell_digest": digest}
            raw = canonical(binding); (root / "binding.json").write_bytes(raw)
            manifest["confirmation_admission"] = {"cell_digest": digest, "metric_binding": {"path": "binding.json", "sha256": sha256_bytes(raw)}}
            errors = admission_errors(manifest, root)
            self.assertNotIn("metric-binding-evidence-absent-or-changed", errors)
            # The authorization names no locked metric specification.
            self.assertIn("rank-metric-specification-unbound", errors)
            binding["authority_reference"] = "a different reviewer"
            (root / "binding.json").write_bytes(canonical(binding))
            self.assertIn("metric-binding-evidence-absent-or-changed", admission_errors(manifest, root))

    def test_cell_order_is_not_an_identity_change_but_parameters_are(self):
        cells = proposal()["cells"][:3]
        self.assertEqual(cell_digest(cells), cell_digest(list(reversed(cells))))
        modified = copy.deepcopy(cells); modified[0]["parameters"]["n"] = 160
        self.assertNotEqual(cell_digest(cells), cell_digest(modified))

    def test_changed_method_lock_remains_unusable_for_pilots(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory); (root / "method.py").write_text("original")
            manifest = proposal()
            manifest["source_locks"] = file_locks(root, ["method.py"])
            manifest["resource_approval"] = {"phase": "pilot", "scope": []}
            (root / "method.py").write_text("changed")
            with self.assertRaisesRegex(ProtocolError, "source lock"):
                validate_manifest(manifest, root, "pilot")

    def test_unknown_operation_does_not_skip_execution_gates(self):
        with self.assertRaisesRegex(ProtocolError, "not recognized"):
            validate_manifest(proposal(), Path("."), "pilot", operation="preview-and-run")


if __name__ == "__main__":
    unittest.main()
