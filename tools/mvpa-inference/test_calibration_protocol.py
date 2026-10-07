#!/usr/bin/env python3
"""Small protocol safety checks, not a simulator QA or calibration campaign."""
import copy
import tempfile
import unittest
from pathlib import Path

from calibration_protocol import (
    ProtocolError, child_seed, dataset_count, protocol_seed, proposal,
    seed_record, summarize, validate_manifest,
)


class CalibrationProtocolTests(unittest.TestCase):
    def test_published_child_derivation_reference(self):
        self.assertEqual(child_seed(90210, [(1, 17), (2, 3)]), -3497511708555549203)

    def test_namespace_is_raw_utf8_and_not_a_framed_digest(self):
        import hashlib
        digest, number = protocol_seed("fixture", "unicode.κ", 0)
        text = ("scalafim/umvpa/inference-calibration/v1\0fixture\0unicode.κ\0" + "0").encode()
        self.assertEqual(digest, hashlib.sha256(text).hexdigest())
        framed = len(text).to_bytes(4, "little") + text
        self.assertNotEqual(digest, hashlib.sha256(framed).hexdigest())
        self.assertGreater(number, 0)

    def test_assignments_do_not_depend_on_order_or_retry(self):
        forward = [seed_record("pilot", "case", i) for i in range(6)]
        reverse = [seed_record("pilot", "case", i) for i in reversed(range(6))]
        self.assertEqual(forward, list(reversed(reverse)))
        self.assertNotEqual(forward[0]["root_seed64"], str(protocol_seed("confirmation", "case", 0)[1]))
        self.assertEqual(len(set(forward[0]["child_seeds64"].values())), 5)

    def test_confirmation_cannot_run_from_a_proposal(self):
        with tempfile.TemporaryDirectory() as temp:
            with self.assertRaisesRegex(ProtocolError, "quarantined"):
                validate_manifest(proposal(), Path(temp), "confirmation")

    def test_resource_planning_does_not_pretend_to_execute_or_freeze(self):
        with tempfile.TemporaryDirectory() as temp:
            for phase in ("simulator", "pilot", "confirmation"):
                validate_manifest(proposal(), Path(temp), phase, operation="plan")
            with self.assertRaisesRegex(ProtocolError, "resource approval"):
                validate_manifest(proposal(), Path(temp), "simulator", operation="execute")

    def test_fixed_counts_are_not_optional_campaign_sizes(self):
        for kind, expected in [("null", 10000), ("alternative", 5000), ("refusal", 10000)]:
            self.assertEqual(dataset_count("confirmation", {"rate_class": kind}), expected)
        changed = copy.deepcopy(proposal()); changed["confirmation_counts"]["null"] = 200
        with self.assertRaisesRegex(ProtocolError, "budgets"):
            validate_manifest(changed, Path("."), "fixture")

    def test_failed_records_and_missing_datasets_stay_in_the_receipt(self):
        cell = {"id": "cell", "rate_class": "null", "availability": "candidate",
                "reference_mechanism": "fixed-nonidentity-randomization"}
        row = {"dataset_index": 0, "root_seed64": str(protocol_seed("pilot", "cell", 0)[1]),
               "status": "failed", "reason": "numeric failure", "p_values": None, "reject": None}
        result = summarize(cell, "pilot", [row])
        self.assertEqual(result["primary_denominator"], 200)
        self.assertEqual(result["statuses"]["failed"], 1)
        self.assertEqual(len(result["missing_indices"]), 199)
        self.assertEqual(result["scientific_release"], "not-adjudicated")

    def test_retry_conflicts_and_inferential_failed_outputs_refuse(self):
        cell = {"id": "cell", "rate_class": "null", "reference_mechanism": "typed-refusal"}
        row = {"dataset_index": 0, "root_seed64": str(protocol_seed("fixture", "cell", 0)[1]),
               "status": "refused", "reason": "unsupported", "p_values": None, "reject": None}
        self.assertEqual(summarize(cell, "fixture", [row, row])["identical_retries"], 1)
        altered = dict(row, reason="different")
        with self.assertRaisesRegex(ProtocolError, "conflicting"):
            summarize(cell, "fixture", [row, altered])
        with self.assertRaisesRegex(ProtocolError, "cannot carry"):
            summarize(cell, "fixture", [dict(row, p_values=[.05])])

    def test_population_conditional_null_contradictions_are_not_reclassified(self):
        cell = {"id": "cell", "rate_class": "null", "availability": "candidate",
                "reference_mechanism": "analytic-known-gaussian"}
        row = {"dataset_index": 0, "root_seed64": str(protocol_seed("fixture", "cell", 0)[1]),
               "status": "evaluated", "family_complete": True, "completed_draws": None,
               "p_values": None, "reject": [True], "declared_population_null": [True],
               "actual_conditional_null": [False]}
        result = summarize(cell, "fixture", [row])
        self.assertEqual(result["estimand_contradiction_indices"], [0])
        self.assertEqual(result["outcome"], "unavailable-population-conditional-estimand-contradiction")
        self.assertTrue(row["declared_population_null"][0])


if __name__ == "__main__":
    unittest.main()
