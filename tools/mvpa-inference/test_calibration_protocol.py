#!/usr/bin/env python3
"""Small protocol safety checks, not a simulator QA or calibration campaign."""
import copy
import tempfile
import unittest
from pathlib import Path

from calibration_protocol import (
    ProtocolError, child_seed, dataset_count, protocol_seed, proposal,
    seed_record, summarize, validate_manifest, validate_rank_record, validate_metric_bindings,
)


class CalibrationProtocolTests(unittest.TestCase):
    def rank_case(self):
        members = ["rank-1", "rank-2", "rank-3", "rank-4"]
        cell = {"id": "rank.cell", "procedure": "rank", "rate_class": "null", "members": members,
                "reference_mechanism": "fixed-nonidentity-randomization", "fixture_nonidentity_draws": 199,
                "metric_bindings": [
                    {"id": "raw-h3", "metric": "type-i", "p_value_scope": "raw-stage", "members": ["rank-3"], "aggregation": "single"},
                    {"id": "closed-h3", "metric": "type-i", "p_value_scope": "closed-sequential", "members": ["rank-3"], "aggregation": "single"},
                    {"id": "closed-family", "metric": "fwer", "p_value_scope": "closed-sequential", "members": members[2:], "aggregation": "any"}]}
        row = {"record_schema": 2, "phase": "fixture", "scenario_id": cell["id"], "dataset_index": 0,
               "root_seed64": str(protocol_seed("fixture", cell["id"], 0)[1]), "status": "evaluated",
               "completed_draws": 199, "family_complete": True, "member_ids": members,
               "p_value_scope": "closed-sequential", "raw_exceedances": [19, 3, 9, 1],
               "raw_p_values": [.1, .02, .05, .01], "closed_p_values": [.1]*4, "p_values": [.1]*4,
               "raw_reject": [False, True, True, True], "closed_reject": [False]*4, "reject": [False]*4,
               "declared_population_null": [False, False, True, True],
               "actual_conditional_null": [False, False, True, True]}
        return cell, row

    def test_raw_stage_and_closed_decisions_remain_distinct(self):
        cell, row = self.rank_case()
        result = summarize(cell, "fixture", [row])
        self.assertEqual([r["successes"] for r in result["metric_counts"]], [1, 0, 0])
        self.assertEqual(result["scientific_release"], "not-adjudicated")

    def test_rank_record_rejects_incomplete_or_inconsistent_observations(self):
        _, row = self.rank_case()
        for change in ({"raw_p_values": None}, {"raw_exceedances": [19, 3, 8, 1]},
                       {"raw_exceedances": [True, 3, 9, 1]}, {"closed_p_values": [.1, .1, .05, .1]},
                       {"raw_reject": [False]*4}, {"closed_reject": [0]*4},
                       {"p_values": [.01]*4}, {"raw_p_values": [float("nan")]*4},
                       {"member_ids": ["rank-2", "rank-1", "rank-3", "rank-4"]},
                       {"actual_conditional_null": [False, False, True]}):
            with self.subTest(change=change), self.assertRaises(ProtocolError):
                validate_rank_record(dict(row, **change))

    def test_new_records_bind_identity_and_cannot_hide_failed_metrics(self):
        cell, row = self.rank_case()
        for change in ({"scenario_id": "another"}, {"phase": "pilot"},
                       {"status": "failed", "reason": "crashed", "p_values": None, "reject": None}):
            with self.subTest(change=change), self.assertRaises(ProtocolError):
                summarize(cell, "fixture", [dict(row, **change)])

    def test_historical_closed_records_cannot_be_used_as_raw_or_confirmation(self):
        cell, row = self.rank_case()
        row.pop("record_schema")
        for field in ("raw_p_values", "raw_reject", "raw_exceedances", "closed_p_values", "closed_reject"):
            row.pop(field)
        with self.assertRaisesRegex(ProtocolError, "schema 2"):
            summarize(cell, "fixture", [row])
        cell.pop("metric_bindings")
        self.assertEqual(summarize(cell, "fixture", [row])["metric_counts"], [])
        row["root_seed64"] = str(protocol_seed("confirmation", cell["id"], 0)[1])
        with self.assertRaisesRegex(ProtocolError, "historical"):
            summarize(cell, "confirmation", [row])

    def test_binding_cannot_substitute_raw_power_or_wrong_truth(self):
        cell, row = self.rank_case()
        for change in ({"metric": "standard-power"}, {"metric": "fwer", "aggregation": "any"},
                       {"members": ["rank-5"]}, {"members": ["rank-3", "rank-3"]}):
            altered = copy.deepcopy(cell)
            altered["metric_bindings"] = [dict(cell["metric_bindings"][0], **change)]
            with self.subTest(change=change), self.assertRaises(ProtocolError):
                validate_metric_bindings(altered)
        cell["metric_bindings"][0]["members"] = ["rank-1"]
        with self.assertRaisesRegex(ProtocolError, "population truth"):
            summarize(cell, "fixture", [row])

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
