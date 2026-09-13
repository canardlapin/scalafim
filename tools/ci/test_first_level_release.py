from __future__ import annotations

import hashlib
import json
from pathlib import Path
import sys
import tempfile
import unittest


sys.path.insert(0, str(Path(__file__).resolve().parent))
from finalize_first_level_release import (  # noqa: E402
  CANONICAL_REPOSITORY,
  EVIDENCE_SCHEMA,
  FIRST_LEVEL_WORKFLOW,
  GATES,
  GATE_SCHEMA,
  LEGACY_EXTERNAL_EVIDENCE,
  REGENERATION_WORKFLOW,
  RUN_SCHEMA,
  benchmark_evidence,
  canonical_sha256,
  external_evidence,
  gate_results,
  required_regeneration_hashes,
)


CANDIDATE = "a" * 40
CI_JOBS = {
  "full-repository": "full repository JVM/Scala.js compile and test",
  "focused-first-level": "focused first-level JVM/Scala.js gates",
  "scientific-coverage": "scoped first-level scientific coverage",
}
REGEN_JOBS = {
  "r-receipts": "locked R receipt regeneration",
  "python-receipts": "locked Python and Nilearn receipt regeneration",
}


class ExternalEvidenceSuite(unittest.TestCase):
  def setUp(self) -> None:
    self.temporary = tempfile.TemporaryDirectory()
    self.root = Path(self.temporary.name)
    (self.root / "docs/scenarios/fixtures").mkdir(parents=True)
    (self.root / "modules/fit/fixtures").mkdir(parents=True)
    (self.root / "tools/r-parity").mkdir(parents=True)
    (self.root / "payloads").mkdir()
    (self.root / "raw").mkdir()
    (self.root / "artifacts").mkdir()

    self.lock_path = self.root / "tools/r-parity/reference-lock.json"
    self.lock_path.write_text('{"schema_version":"lock"}\n')
    self.fixture_path = self.root / "docs/scenarios/fixtures/example.r.json"
    self.fixture_path.write_text(json.dumps({
      "receipt": {"environment": {"lock_path": "tools/r-parity/reference-lock.json"}}
    }))
    self.scala_path = self.root / "modules/fit/fixtures/ExampleRFixture.scala"
    self.scala_path.write_text("object ExampleRFixture\n")
    (self.root / "docs/scenarios/manifest.json").write_text(json.dumps({
      "scenarios": [{
        "reference": {
          "exporter_path": "tools/generate.R",
          "fixture_path": "docs/scenarios/fixtures/example.r.json",
          "generated_scala_fixture_path": "modules/fit/fixtures/ExampleRFixture.scala",
        }
      }]
    }))

  def tearDown(self) -> None:
    self.temporary.cleanup()

  def workflow_pair(
      self,
      name: str,
      workflow: str,
      jobs: dict[str, str],
      run_id: int,
      *,
      status: str = "completed",
      conclusion: str | None = "success",
      artifacts: list[dict[str, object]] | None = None,
  ) -> tuple[dict[str, object], dict[str, object]]:
    source_url = f"https://github.com/{CANONICAL_REPOSITORY}/actions/runs/{run_id}"
    payload: dict[str, object] = {
      "repository": CANONICAL_REPOSITORY,
      "workflow_run": {
        "id": run_id,
        "run_attempt": 1,
        "head_sha": CANDIDATE,
        "status": status,
        "conclusion": conclusion,
        "path": workflow,
        "html_url": source_url,
      },
      "jobs": [
        {"name": display, "status": "completed", "conclusion": "success"}
        for display in jobs.values()
      ],
    }
    if artifacts is not None:
      payload["artifacts"] = artifacts
    record: dict[str, object] = {
      "kind": name,
      "repository": CANONICAL_REPOSITORY,
      "candidate_commit": CANDIDATE,
      "observed_at": "2026-09-13T14:00:00Z",
      "source_url": source_url,
      "workflow": workflow,
      "run_id": run_id,
      "run_attempt": 1,
      "head_sha": CANDIDATE,
      "status": status,
      "conclusion": conclusion,
      "required_jobs": jobs,
    }
    return record, payload

  def write_payload(self, name: str, payload: dict[str, object]) -> tuple[str, str]:
    path = self.root / "payloads" / f"{name}.json"
    path.write_text(json.dumps(payload, sort_keys=True))
    return path.relative_to(self.root).as_posix(), hashlib.sha256(path.read_bytes()).hexdigest()

  def write_capture(self, name: str, payload: dict[str, object]) -> dict[str, str]:
    path = self.root / "raw" / f"{name}.json"
    path.write_text(json.dumps(payload, sort_keys=True))
    return {
      "path": path.relative_to(self.root).as_posix(),
      "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
    }

  def make_bundle(
      self,
      *,
      ci_status: str = "completed",
      ci_conclusion: str | None = "success",
      ci_record_repository: str = CANONICAL_REPOSITORY,
      ci_record_commit: str = CANDIDATE,
      include_python_artifact: bool = True,
      include_coverage_context: bool = True,
      branch_via_ruleset: bool = False,
  ) -> Path:
    ci_record, ci_payload = self.workflow_pair(
      "remote_first_level_ci",
      FIRST_LEVEL_WORKFLOW,
      CI_JOBS,
      101,
      status=ci_status,
      conclusion=ci_conclusion,
    )
    ci_record["repository"] = ci_record_repository
    ci_record["candidate_commit"] = ci_record_commit
    ci_payload["captures"] = {
      "run": self.write_capture("ci-run", ci_payload["workflow_run"]),
      "jobs": self.write_capture("ci-jobs", {
        "total_count": len(ci_payload["jobs"]),
        "jobs": ci_payload["jobs"],
      }),
    }

    artifacts = [
      {"name": "locked-r-receipts", "expired": False},
    ]
    if include_python_artifact:
      artifacts.append({"name": "locked-python-receipts", "expired": False})
    regen_record, regen_payload = self.workflow_pair(
      "reference_regeneration",
      REGENERATION_WORKFLOW,
      REGEN_JOBS,
      102,
      artifacts=artifacts,
    )
    lock_hashes, generated_hashes = required_regeneration_hashes(self.root)
    regen_record.update({
      "required_artifacts": {
        "r-receipts": "locked-r-receipts",
        "python-receipts": "locked-python-receipts",
      },
      "reference_lock_hashes": lock_hashes,
      "generated_fixture_hashes": generated_hashes,
    })
    regen_payload["reference_lock_hashes"] = lock_hashes
    regen_payload["generated_fixture_hashes"] = generated_hashes
    artifact_evidence = {}
    artifact_captures = {}
    for lane, name, filename, capture_name in (
        ("r-receipts", "locked-r-receipts", "scenario-r-regeneration-evidence.json", "artifact_r"),
        ("python-receipts", "locked-python-receipts", "scenario-python-regeneration-evidence.json", "artifact_python"),
    ):
      manifest = {
        "schema_version": "scalafim-first-level-regeneration-artifact/v1",
        "repository": CANONICAL_REPOSITORY,
        "candidate_commit": CANDIDATE,
        "lane": lane,
        "evidence_lane": "release",
        "reference_lock_hashes": lock_hashes,
        "generated_fixture_hashes": generated_hashes,
        "reference_locks": {
          "tools/r-parity/reference-lock.json": json.loads(self.lock_path.read_text()),
        },
      }
      artifact_path = self.root / "artifacts" / filename
      artifact_path.write_text(json.dumps(manifest, sort_keys=True))
      capture = {
        "path": artifact_path.relative_to(self.root).as_posix(),
        "sha256": hashlib.sha256(artifact_path.read_bytes()).hexdigest(),
      }
      artifact_evidence[lane] = {"name": name, "manifest": manifest, **capture}
      artifact_captures[capture_name] = capture
    regen_payload["artifact_evidence"] = artifact_evidence
    regen_payload["captures"] = {
      "run": self.write_capture("regen-run", regen_payload["workflow_run"]),
      "jobs": self.write_capture("regen-jobs", {
        "total_count": len(regen_payload["jobs"]),
        "jobs": regen_payload["jobs"],
      }),
      "artifacts": self.write_capture("regen-artifacts", {
        "total_count": len(regen_payload["artifacts"]),
        "artifacts": regen_payload["artifacts"],
      }),
      **artifact_captures,
    }

    contexts = list(CI_JOBS.values())
    if not include_coverage_context:
      contexts.remove(CI_JOBS["scientific-coverage"])
    branch_ref = {
      "repository": {
        "full_name": CANONICAL_REPOSITORY,
        "default_branch": "main",
      },
      "branch": {"name": "main", "commit": {"sha": CANDIDATE}},
    }
    if branch_via_ruleset:
      classic = {
        "status": "absent",
        "http_status": 404,
        "endpoint": f"repos/{CANONICAL_REPOSITORY}/branches/main/protection",
        "error": "HTTP 404",
        "payload": None,
      }
      ruleset_payload = [{
        "id": 7,
        "target": "branch",
        "enforcement": "active",
        "conditions": {"ref_name": {"include": ["~DEFAULT_BRANCH"], "exclude": []}},
        "rules": [{
          "type": "required_status_checks",
          "parameters": {
            "required_status_checks": [{"context": value} for value in contexts]
          },
        }],
      }]
    else:
      classic = {
        "status": "ok",
        "http_status": 200,
        "endpoint": f"repos/{CANONICAL_REPOSITORY}/branches/main/protection",
        "error": None,
        "payload": {
          "required_status_checks": {
            "strict": True,
            "contexts": contexts,
            "checks": [],
          },
        },
      }
      ruleset_payload = []
    rulesets = {
      "status": "ok",
      "http_status": 200,
      "endpoint": f"repos/{CANONICAL_REPOSITORY}/rulesets?includes_parents=true",
      "error": None,
      "payload": ruleset_payload,
    }
    branch_payload: dict[str, object] = {
      "repository": CANONICAL_REPOSITORY,
      "branch": "main",
      "candidate_commit": CANDIDATE,
      "branch_ref": branch_ref,
      "policy_snapshot": {"classic": classic, "rulesets": rulesets},
      "resolved_required_status_checks": sorted(contexts),
      "collection_error": None,
      "captures": {
        "branch": self.write_capture("branch-reference", branch_ref),
        "protection": self.write_capture("branch-protection", classic),
        "rulesets": self.write_capture("repository-rulesets", rulesets),
      },
    }
    branch_record: dict[str, object] = {
      "kind": "branch_protection",
      "repository": CANONICAL_REPOSITORY,
      "candidate_commit": CANDIDATE,
      "observed_at": "2026-09-13T14:05:00Z",
      "source_url": f"https://api.github.com/repos/{CANONICAL_REPOSITORY}/branches/main/protection",
      "branch": "main",
      "expected_required_checks": sorted(CI_JOBS.values()),
    }

    records = []
    for name, record, payload in (
        ("ci", ci_record, ci_payload),
        ("regeneration", regen_record, regen_payload),
        ("branch", branch_record, branch_payload),
    ):
      payload_path, payload_hash = self.write_payload(name, payload)
      record["payload_path"] = payload_path
      record["payload_sha256"] = payload_hash
      if name == "branch":
        record["policy_snapshot_sha256"] = payload_hash
      records.append(record)

    bundle_path = self.root / "evidence.json"
    bundle_path.write_text(json.dumps({
      "schema_version": EVIDENCE_SCHEMA,
      "evidence": records,
    }, indent=2))
    return bundle_path

  def evaluate(self, bundle: Path) -> dict[str, dict[str, object]]:
    return external_evidence(
      CANDIDATE,
      bundle_path=bundle,
      candidate_branch="main",
      repo_root=self.root,
    )

  def test_missing_and_empty_urls_are_unverified(self) -> None:
    empty = {kind: "" for kind in LEGACY_EXTERNAL_EVIDENCE}
    evidence = external_evidence(CANDIDATE, legacy_urls=empty)
    self.assertTrue(all(value["status"] == "unverified" for value in evidence.values()))

  def test_malformed_url_is_unverified(self) -> None:
    urls = {kind: "" for kind in LEGACY_EXTERNAL_EVIDENCE}
    urls["remote_first_level_ci"] = "not-a-url"
    evidence = external_evidence(CANDIDATE, legacy_urls=urls)
    self.assertEqual(evidence["remote_first_level_ci"]["status"], "unverified")

  def test_url_only_is_provided_but_not_verified(self) -> None:
    urls = {kind: "https://github.com/example/evidence" for kind in LEGACY_EXTERNAL_EVIDENCE}
    evidence = external_evidence(CANDIDATE, legacy_urls=urls)
    self.assertTrue(all(value["status"] == "provided" for value in evidence.values()))

  def test_foreign_repository_is_unverified(self) -> None:
    evidence = self.evaluate(self.make_bundle(ci_record_repository="someone/fork"))
    self.assertEqual(evidence["remote_first_level_ci"]["status"], "unverified")

  def test_wrong_candidate_commit_is_unverified(self) -> None:
    evidence = self.evaluate(self.make_bundle(ci_record_commit="b" * 40))
    self.assertEqual(evidence["remote_first_level_ci"]["status"], "unverified")

  def test_nonterminal_and_failed_workflow_states(self) -> None:
    cases = (
      ("queued", None, "provided"),
      ("in_progress", None, "provided"),
      ("completed", "failure", "failed"),
      ("completed", "skipped", "failed"),
    )
    for status, conclusion, expected in cases:
      with self.subTest(status=status, conclusion=conclusion):
        evidence = self.evaluate(self.make_bundle(
          ci_status=status,
          ci_conclusion=conclusion,
        ))
        self.assertEqual(evidence["remote_first_level_ci"]["status"], expected)

  def test_absent_regeneration_artifact_is_unverified(self) -> None:
    evidence = self.evaluate(self.make_bundle(include_python_artifact=False))
    self.assertEqual(evidence["reference_regeneration"]["status"], "unverified")

  def test_tampered_payload_is_unverified(self) -> None:
    bundle = self.make_bundle()
    payload = self.root / "payloads/ci.json"
    payload.write_text(payload.read_text() + "\n")
    evidence = self.evaluate(bundle)
    self.assertEqual(evidence["remote_first_level_ci"]["status"], "unverified")

  def test_tampered_raw_capture_is_unverified(self) -> None:
    bundle = self.make_bundle()
    capture = self.root / "raw/ci-run.json"
    capture.write_text(capture.read_text() + "\n")
    evidence = self.evaluate(bundle)
    self.assertEqual(evidence["remote_first_level_ci"]["status"], "unverified")

  def test_missing_required_branch_check_is_failed(self) -> None:
    evidence = self.evaluate(self.make_bundle(include_coverage_context=False))
    self.assertEqual(evidence["branch_protection"]["status"], "failed")

  def test_valid_bundle_verifies_every_kind(self) -> None:
    evidence = self.evaluate(self.make_bundle())
    self.assertEqual(
      {kind: value["status"] for kind, value in evidence.items()},
      {
        "branch_protection": "verified",
        "reference_regeneration": "verified",
        "remote_first_level_ci": "verified",
      },
    )

  def test_active_ruleset_can_supply_branch_checks(self) -> None:
    evidence = self.evaluate(self.make_bundle(branch_via_ruleset=True))
    self.assertEqual(evidence["branch_protection"]["status"], "verified")


class LocalEvidenceSuite(unittest.TestCase):
  def setUp(self) -> None:
    self.temporary = tempfile.TemporaryDirectory()
    self.root = Path(self.temporary.name)
    self.gates = self.root / "gates"
    self.gates.mkdir()
    self.commit = "c" * 40
    self.inputs = {"gate_inventory_sha256": "d" * 64}
    self.manifest = {
      "schema_version": RUN_SCHEMA,
      "run_id": "run-1",
      "started_at": "2026-09-13T14:00:00Z",
      "source": {"commit": self.commit, "clean": True, "dirty_path_count": 0},
      "inputs": self.inputs,
      "inputs_sha256": canonical_sha256(self.inputs),
    }
    for name, command in GATES.items():
      log = self.gates / f"{name}.log"
      log.write_text(f"{name} passed\n")
      receipt = {
        "schema_version": GATE_SCHEMA,
        "name": name,
        "run_id": "run-1",
        "candidate_commit": self.commit,
        "inputs_sha256": self.manifest["inputs_sha256"],
        "command": command,
        "exit_code": 0,
        "status": "pass",
        "started_at": "2026-09-13T14:01:00Z",
        "ended_at": "2026-09-13T14:02:00Z",
        "log": {
          "path": str(log),
          "sha256": hashlib.sha256(log.read_bytes()).hexdigest(),
        },
      }
      (self.gates / f"{name}.json").write_text(json.dumps(receipt))

  def tearDown(self) -> None:
    self.temporary.cleanup()

  def evaluate(self, *, inputs: dict[str, object] | None = None):
    return gate_results(
      self.gates,
      self.manifest,
      current_commit=self.commit,
      current_clean=True,
      current_inputs=self.inputs if inputs is None else inputs,
      repo_root=self.root,
    )

  def receipt(self, name: str) -> dict[str, object]:
    return json.loads((self.gates / f"{name}.json").read_text())

  def write_receipt(self, name: str, receipt: dict[str, object]) -> None:
    (self.gates / f"{name}.json").write_text(json.dumps(receipt))

  def test_clean_matching_gate_evidence_verifies(self) -> None:
    gates, errors = self.evaluate()
    self.assertEqual(errors, [])
    self.assertTrue(all(gate["status"] == "pass" for gate in gates.values()))

  def test_stale_mixed_run_and_wrong_commit_are_rejected(self) -> None:
    receipt = self.receipt("documentation")
    receipt["run_id"] = "old-run"
    self.write_receipt("documentation", receipt)
    receipt = self.receipt("compile_all")
    receipt["candidate_commit"] = "e" * 40
    self.write_receipt("compile_all", receipt)
    gates, errors = self.evaluate()
    self.assertEqual(gates["documentation"]["status"], "invalid")
    self.assertEqual(gates["compile_all"]["status"], "invalid")
    self.assertTrue(any("run id" in error for error in errors))
    self.assertTrue(any("candidate commit" in error for error in errors))

  def test_missing_or_truncated_log_is_rejected(self) -> None:
    (self.gates / "documentation.log").unlink()
    (self.gates / "compile_all.log").write_text("truncated\n")
    gates, errors = self.evaluate()
    self.assertEqual(gates["documentation"]["status"], "invalid")
    self.assertEqual(gates["compile_all"]["status"], "invalid")
    self.assertTrue(any("log is missing" in error for error in errors))
    self.assertTrue(any("log hash" in error for error in errors))

  def test_changed_inputs_and_missing_gate_are_rejected(self) -> None:
    (self.gates / "test_all.json").unlink()
    gates, errors = self.evaluate(inputs={"gate_inventory_sha256": "f" * 64})
    self.assertEqual(gates["test_all"]["status"], "invalid")
    self.assertTrue(any("inputs are stale" in error for error in errors))
    self.assertTrue(any("receipt is missing" in error for error in errors))

  def test_skipped_and_failed_gates_remain_nonpassing(self) -> None:
    for name, exit_code, status in (("documentation", 125, "skipped"), ("compile_all", 1, "fail")):
      receipt = self.receipt(name)
      receipt["exit_code"] = exit_code
      receipt["status"] = status
      self.write_receipt(name, receipt)
    gates, errors = self.evaluate()
    self.assertEqual(errors, [])
    self.assertEqual(gates["documentation"]["status"], "skipped")
    self.assertEqual(gates["compile_all"]["status"], "fail")

  def test_benchmark_receipt_binds_candidate_sources_workload_and_raw_data(self) -> None:
    source_file = self.root / "benchmark.scala"
    raw_file = self.root / "raw.json"
    source_file.write_text("object Benchmark\n")
    raw_file.write_text("[]\n")
    (self.root / "project").mkdir()
    (self.root / "build.sbt").write_text(
      'ThisBuild / scalaVersion := "3.7.4"\n' +
      'lazy val galeRevision = "' + "a" * 40 + '"\n'
    )
    (self.root / "project/build.properties").write_text("sbt.version=1.11.7\n")
    receipt_path = self.root / "receipt.json"
    source_hashes = {"benchmark.scala": hashlib.sha256(source_file.read_bytes()).hexdigest()}
    workload = {
      "budget_profile": {"fit": {"timepoints": "360"}},
      "budget_schema_version": "budget/v1",
      "results": [{"benchmark": "fit", "jmh": {"forks": 1}, "params": {"timepoints": "360"}}],
    }
    receipt = {
      "schema_version": "scalafim-first-level-benchmark-receipt/v2",
      "benchmark_status": "pass",
      "release_eligible": True,
      "source": {
        "commit": self.commit,
        "dirty": False,
        "files": source_hashes,
        "files_sha256": canonical_sha256(source_hashes),
        "provider_revisions": {
          "galeRevision": "a" * 40,
          "sbtVersion": "1.11.7",
          "scalaVersion": "3.7.4",
        },
      },
      "budget_profile": {"fit": {"timepoints": "360"}},
      "budget_schema_version": "budget/v1",
      "fixture_hashes": {},
      "results": workload["results"],
      "workload": workload,
      "workload_sha256": canonical_sha256(workload),
      "raw_reports": [{"name": "raw.json", "sha256": hashlib.sha256(raw_file.read_bytes()).hexdigest()}],
    }
    receipt_path.write_text(json.dumps(receipt))
    evidence, problems = benchmark_evidence(receipt_path, self.commit, self.root)
    self.assertEqual(problems, [])
    self.assertEqual(evidence["status"], "verified")
    _, wrong_candidate = benchmark_evidence(receipt_path, "f" * 40, self.root)
    self.assertTrue(any("different candidate" in problem for problem in wrong_candidate))


if __name__ == "__main__":
  unittest.main()
