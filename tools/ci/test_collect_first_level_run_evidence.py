from __future__ import annotations

import json
from pathlib import Path
import sys
import tempfile
import unittest


sys.path.insert(0, str(Path(__file__).resolve().parent))
from collect_first_level_run_evidence import (  # noqa: E402
  CI_JOBS,
  REGENERATION_JOBS,
  EvidenceError,
  parse_workflow_capture,
  regeneration_artifact_manifest,
  upsert_bundle,
  validate_regeneration_artifact,
)
from finalize_first_level_release import (  # noqa: E402
  CANONICAL_REPOSITORY,
  EVIDENCE_SCHEMA,
  FIRST_LEVEL_WORKFLOW,
  REGENERATION_WORKFLOW,
  validate_workflow_record,
)


CANDIDATE = "a" * 40


class CollectorParserSuite(unittest.TestCase):
  def workflow(
      self,
      *,
      workflow: str = FIRST_LEVEL_WORKFLOW,
      repository: str = CANONICAL_REPOSITORY,
      candidate: str = CANDIDATE,
      attempt: int = 2,
      status: str = "completed",
      conclusion: str | None = "success",
      jobs: dict[str, str] = CI_JOBS,
  ) -> tuple[dict[str, object], dict[str, object]]:
    run = {
      "id": 101,
      "run_attempt": attempt,
      "head_sha": candidate,
      "status": status,
      "conclusion": conclusion,
      "path": workflow,
      "html_url": "https://github.com/canardlapin/scalafim/actions/runs/101",
      "repository": {"full_name": repository},
    }
    job_values = [
      {
        "name": display,
        "run_attempt": attempt,
        "status": "completed",
        "conclusion": "success",
      }
      for display in jobs.values()
    ]
    return run, {"total_count": len(job_values), "jobs": job_values}

  def parse(self, run, jobs, *, workflow=FIRST_LEVEL_WORKFLOW, required=CI_JOBS):
    return parse_workflow_capture(
      run,
      jobs,
      repository=CANONICAL_REPOSITORY,
      candidate=CANDIDATE,
      run_id=101,
      workflow=workflow,
      required_jobs=required,
    )

  def test_accepts_exact_candidate_workflow_and_attempt(self) -> None:
    run, jobs = self.workflow()
    parsed_run, parsed_jobs = self.parse(run, jobs)
    self.assertEqual(parsed_run["run_attempt"], 2)
    self.assertEqual(len(parsed_jobs), 3)

  def test_rejects_wrong_candidate_workflow_and_repository(self) -> None:
    cases = (
      {"candidate": "b" * 40},
      {"workflow": REGENERATION_WORKFLOW},
      {"repository": "someone/fork"},
    )
    for values in cases:
      with self.subTest(values=values):
        run, jobs = self.workflow(**values)
        with self.assertRaises(EvidenceError):
          self.parse(run, jobs)

  def test_rejects_jobs_from_an_old_rerun_attempt(self) -> None:
    run, jobs = self.workflow(attempt=3)
    jobs["jobs"][0]["run_attempt"] = 2
    with self.assertRaisesRegex(EvidenceError, "rerun attempt"):
      self.parse(run, jobs)

  def test_rejects_missing_required_job(self) -> None:
    run, jobs = self.workflow()
    jobs["jobs"].pop()
    jobs["total_count"] = len(jobs["jobs"])
    with self.assertRaisesRegex(EvidenceError, "omits required jobs"):
      self.parse(run, jobs)

  def test_validator_classifies_pending_failed_and_skipped_jobs(self) -> None:
    cases = (
      ("in_progress", None, None, "provided"),
      ("completed", "failure", None, "failed"),
      ("completed", "success", "skipped", "failed"),
    )
    for run_status, conclusion, job_conclusion, expected in cases:
      with self.subTest(expected=expected):
        run, jobs_response = self.workflow(status=run_status, conclusion=conclusion)
        if job_conclusion is not None:
          jobs_response["jobs"][0]["conclusion"] = job_conclusion
        record = {
          "workflow": FIRST_LEVEL_WORKFLOW,
          "run_id": 101,
          "run_attempt": 2,
          "head_sha": CANDIDATE,
          "status": run_status,
          "conclusion": conclusion,
          "source_url": run["html_url"],
          "required_jobs": CI_JOBS,
        }
        payload = {
          "repository": CANONICAL_REPOSITORY,
          "workflow_run": run,
          "jobs": jobs_response["jobs"],
        }
        status, _, _ = validate_workflow_record(
          record,
          payload,
          workflow=FIRST_LEVEL_WORKFLOW,
          required_job_ids=frozenset(CI_JOBS),
          repository=CANONICAL_REPOSITORY,
          candidate_commit=CANDIDATE,
        )
        self.assertEqual(status, expected)


class RegenerationArtifactSuite(unittest.TestCase):
  def setUp(self) -> None:
    self.temporary = tempfile.TemporaryDirectory()
    self.root = Path(self.temporary.name)
    (self.root / "docs/scenarios/fixtures").mkdir(parents=True)
    (self.root / "modules/fit/fixtures").mkdir(parents=True)
    (self.root / "tools/r-parity").mkdir(parents=True)
    lock = self.root / "tools/r-parity/reference-lock.json"
    lock.write_text(json.dumps({"r": {"version": "4.5.1", "packages": {"fmriAR": {"revision": "1"}}}}))
    fixture = self.root / "docs/scenarios/fixtures/example.r.json"
    fixture.write_text(json.dumps({
      "receipt": {"environment": {"lock_path": "tools/r-parity/reference-lock.json"}}
    }))
    scala = self.root / "modules/fit/fixtures/Example.scala"
    scala.write_text("object Example\n")
    (self.root / "docs/scenarios/manifest.json").write_text(json.dumps({
      "scenarios": [{
        "reference": {
          "exporter_path": "tools/generate.R",
          "fixture_path": "docs/scenarios/fixtures/example.r.json",
          "generated_scala_fixture_path": "modules/fit/fixtures/Example.scala",
        }
      }]
    }))

  def tearDown(self) -> None:
    self.temporary.cleanup()

  def manifest(self) -> dict[str, object]:
    return regeneration_artifact_manifest(
      "r-receipts",
      CANDIDATE,
      evidence_lane="release",
      repo_root=self.root,
    )

  def validate(self, manifest: dict[str, object]) -> None:
    validate_regeneration_artifact(
      manifest,
      lane="r-receipts",
      repository=CANONICAL_REPOSITORY,
      candidate=CANDIDATE,
      repo_root=self.root,
    )

  def test_accepts_matching_locked_release_artifact(self) -> None:
    self.validate(self.manifest())

  def test_rejects_nightly_wrong_candidate_and_tampered_fixture_hash(self) -> None:
    for key, value in (
        ("evidence_lane", "nightly"),
        ("candidate_commit", "b" * 40),
        ("generated_fixture_hashes", {}),
    ):
      with self.subTest(key=key):
        manifest = self.manifest()
        manifest[key] = value
        with self.assertRaises(EvidenceError):
          self.validate(manifest)

  def test_rejects_artifact_after_candidate_fixture_changes(self) -> None:
    manifest = self.manifest()
    (self.root / "docs/scenarios/fixtures/example.r.json").write_text("{}")
    with self.assertRaises(EvidenceError):
      self.validate(manifest)

  def test_bundle_update_preserves_other_evidence_and_replaces_kind(self) -> None:
    path = self.root / "evidence.json"
    path.write_text(json.dumps({
      "schema_version": EVIDENCE_SCHEMA,
      "evidence": [{"kind": "branch_protection", "value": 1}, {"kind": "remote_first_level_ci", "value": 1}],
    }))
    upsert_bundle(path, [{"kind": "remote_first_level_ci", "value": 2}])
    entries = {entry["kind"]: entry for entry in json.loads(path.read_text())["evidence"]}
    self.assertEqual(entries["branch_protection"]["value"], 1)
    self.assertEqual(entries["remote_first_level_ci"]["value"], 2)


if __name__ == "__main__":
  unittest.main()
