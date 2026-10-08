#!/usr/bin/env python3
"""Verify reference-lock routing without losing checks for other receipt groups."""
import argparse
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import check_receipts


class ReceiptLockRoutingSuite(unittest.TestCase):
  def test_regeneration_selects_one_lock_but_checks_every_receipt(self):
    jobs = (
      check_receipts.ReceiptJob("raw.R", "raw.py"),
      check_receipts.ReceiptJob("corrected.R", "corrected.py", "corrected-lock.json"),
    )
    lock = {"schema_version": check_receipts.LOCK_SCHEMA, "r": {}}
    with (
      patch.object(check_receipts, "parse_args", return_value=argparse.Namespace(regenerate="r", lock_path="corrected-lock.json")),
      patch.object(check_receipts, "receipt_jobs", return_value=jobs),
      patch.object(check_receipts, "load_json", return_value=lock),
      patch.object(check_receipts, "regenerate_r") as regenerate,
      patch.object(check_receipts, "check_job") as check,
    ):
      self.assertEqual(check_receipts.main(), 0)
      regenerate.assert_called_once_with((jobs[1],), lock)
      self.assertEqual([call.args[0] for call in check.call_args_list], list(jobs))

  def test_unknown_lock_cannot_report_successful_empty_regeneration(self):
    jobs = (check_receipts.ReceiptJob("raw.R", "raw.py"),)
    with (
      patch.object(check_receipts, "parse_args", return_value=argparse.Namespace(regenerate="r", lock_path="missing-lock.json")),
      patch.object(check_receipts, "receipt_jobs", return_value=jobs),
      patch.object(check_receipts, "load_json", return_value={"schema_version": check_receipts.LOCK_SCHEMA}),
    ):
      with self.assertRaisesRegex(SystemExit, "no receipt generators"):
        check_receipts.main()

  def test_scoped_sources_are_verified_against_their_exact_revision(self):
    with tempfile.TemporaryDirectory() as directory:
      root = Path(directory)
      lock = {"packages": {"fmriAR": {"environment_variable": "FMRIAR_AR_R", "revision": "expected"}}}
      with (
        patch.dict(check_receipts.os.environ, {"FMRIAR_AR_R": str(root)}),
        patch.object(check_receipts, "git_revision", return_value="wrong-revision"),
      ):
        with self.assertRaisesRegex(SystemExit, "expected expected"):
          check_receipts.locked_sources(lock)


if __name__ == "__main__":
  unittest.main()
