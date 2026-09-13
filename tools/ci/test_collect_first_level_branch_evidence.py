from __future__ import annotations

import json
from pathlib import Path
import sys
import tempfile
import unittest


sys.path.insert(0, str(Path(__file__).resolve().parent))
from collect_first_level_branch_evidence import (  # noqa: E402
  EvidenceError,
  expected_checks_from_bundle,
  http_status,
  policy_contexts,
)
from collect_first_level_run_evidence import CI_JOBS  # noqa: E402
from finalize_first_level_release import CANONICAL_REPOSITORY, EVIDENCE_SCHEMA  # noqa: E402


CANDIDATE = "a" * 40
EXPECTED = set(CI_JOBS.values())


def classic(contexts: set[str] = EXPECTED) -> dict[str, object]:
  return {
    "status": "ok",
    "payload": {
      "required_status_checks": {
        "strict": True,
        "contexts": sorted(contexts),
        "checks": [],
      }
    },
  }


def rulesets(values: list[dict[str, object]] | None = None) -> dict[str, object]:
  return {"status": "ok", "payload": values or []}


def required_ruleset(contexts: set[str], *, include: list[str] | None = None) -> dict[str, object]:
  return {
    "id": 7,
    "target": "branch",
    "enforcement": "active",
    "conditions": {
      "ref_name": {
        "include": include or ["~DEFAULT_BRANCH"],
        "exclude": [],
      }
    },
    "rules": [{
      "type": "required_status_checks",
      "parameters": {
        "required_status_checks": [{"context": value} for value in sorted(contexts)]
      },
    }],
  }


class BranchPolicyParserSuite(unittest.TestCase):
  def evaluate(self, classic_value, ruleset_value):
    return policy_contexts(
      classic_value,
      ruleset_value,
      branch="main",
      default_branch="main",
    )

  def test_classic_protection_supplies_expected_checks(self) -> None:
    contexts, error = self.evaluate(classic(), rulesets())
    self.assertIsNone(error)
    self.assertEqual(contexts, EXPECTED)

  def test_missing_required_check_remains_observable(self) -> None:
    contexts, error = self.evaluate(classic(EXPECTED - {CI_JOBS["scientific-coverage"]}), rulesets())
    self.assertIsNone(error)
    self.assertFalse(EXPECTED.issubset(contexts or set()))

  def test_absent_policy_is_unverified(self) -> None:
    contexts, error = self.evaluate(
      {"status": "absent", "http_status": 404, "error": "HTTP 404", "payload": None},
      rulesets(),
    )
    self.assertIsNone(contexts)
    self.assertIn("no readable protection", error or "")

  def test_permission_failure_is_unverified(self) -> None:
    contexts, error = self.evaluate(
      {"status": "error", "http_status": 403, "error": "HTTP 403", "payload": None},
      rulesets(),
    )
    self.assertIsNone(contexts)
    self.assertIn("not readable", error or "")

  def test_active_default_branch_ruleset_supplies_checks(self) -> None:
    contexts, error = self.evaluate(
      {"status": "absent", "http_status": 404, "error": "HTTP 404", "payload": None},
      rulesets([required_ruleset(EXPECTED)]),
    )
    self.assertIsNone(error)
    self.assertEqual(contexts, EXPECTED)

  def test_nonmatching_ruleset_does_not_count_as_policy(self) -> None:
    contexts, error = self.evaluate(
      {"status": "absent", "http_status": 404, "error": "HTTP 404", "payload": None},
      rulesets([required_ruleset(EXPECTED, include=["refs/heads/release/*"])]),
    )
    self.assertIsNone(contexts)
    self.assertIn("no readable protection", error or "")

  def test_http_status_parser(self) -> None:
    self.assertEqual(http_status("gh: Resource not accessible (HTTP 403)"), 403)
    self.assertIsNone(http_status("connection reset"))


class ExpectedCheckSuite(unittest.TestCase):
  def setUp(self) -> None:
    self.temporary = tempfile.TemporaryDirectory()
    self.root = Path(self.temporary.name)
    self.bundle = self.root / "evidence.json"

  def tearDown(self) -> None:
    self.temporary.cleanup()

  def write(self, record: dict[str, object]) -> None:
    self.bundle.write_text(json.dumps({
      "schema_version": EVIDENCE_SCHEMA,
      "evidence": [record],
    }))

  def test_uses_display_names_from_candidate_ci_record(self) -> None:
    self.write({
      "kind": "remote_first_level_ci",
      "candidate_commit": CANDIDATE,
      "required_jobs": CI_JOBS,
    })
    self.assertEqual(expected_checks_from_bundle(self.bundle, CANDIDATE), sorted(EXPECTED))

  def test_rejects_stale_or_incomplete_ci_record(self) -> None:
    for candidate, jobs in (("b" * 40, CI_JOBS), (CANDIDATE, {"full-repository": "full"})):
      with self.subTest(candidate=candidate, jobs=jobs):
        self.write({
          "kind": "remote_first_level_ci",
          "candidate_commit": candidate,
          "required_jobs": jobs,
        })
        with self.assertRaises(EvidenceError):
          expected_checks_from_bundle(self.bundle, CANDIDATE)


if __name__ == "__main__":
  unittest.main()
