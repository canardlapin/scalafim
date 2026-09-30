"""Regression checks for schedule integrity, denominators and admission gates."""
import unittest

import summarize_voxelwise_rrg_calibration as s


def row(scenario, index, covered=True):
    d = 100000 + index
    runs = 2 if scenario in ("multiple_runs", "censor_continuous") else 1
    rho = [.1, .5, -.2]
    sigma = [[.36 if i == j else .18 for j in range(3)] for i in range(3)]
    return {"scenario": scenario, "arm": "plugin_refit_rank1", "dataset": d,
            "dataset_index": index, "input_seed": 10007 + 7919 * d,
            "bootstrap_seed": 660001 + 101 * d, "response_sha256": "a" * 64,
            "rank": 1, "replicates": 999, "completed_replicates": 999,
            "status": "ok", "error": None, "seconds": 0.01,
            "physical_run_ids": list(range(runs)), "original_whitening": "estimated",
            "replicate_whitening": "refit", "generator": "joint_gaussian_ar1",
            "original_rho_per_run_voxel": [rho] * runs, "true_rho_per_run_voxel": [rho] * runs,
            "generating_rho": [rho] * runs, "fitted_sigma_per_run": [sigma] * runs,
            "generating_sigma_per_run": [sigma] * runs, "covariance_diagnostic_error": None,
            "true_sigma": sigma, "replicate_rho_mean": [[.09, .48, -.19]] * runs,
            "replicate_rho_sd": [[.1, .1, .1]] * runs, "stationarity_bound_hits": [[0, 0, 0]] * runs,
            "estimate": [index * .001] * 12, "truth": [0.0] * 12,
            "lower": [-1.0 if covered else 1.0] * 12, "upper": [2.0] * 12,
            "variance": [1.0] * 12}


def regular_rows(n, covered):
    return [row(scenario, i, i < covered) for scenario in s.REGULAR for i in range(n)]


class CalibrationSummaryTests(unittest.TestCase):
    def test_schedule_counts_and_disjoint_seed_ranges(self):
        self.assertEqual(len(s.expected_keys("development", 400)), 3400)
        self.assertEqual(len(s.expected_keys("fresh", 2000)), 8000)
        self.assertEqual(len(s.expected_keys("stress", 200)), 600)
        self.assertFalse(s.expected_keys("fresh", 2000) & s.expected_keys("stress", 200))

    def test_shared_seeds_are_not_reported_as_independent_scenario_inputs(self):
        result = s.validate(regular_rows(2, 2), "fresh", 2)
        self.assertEqual(result["scenario_dataset_count"], 8)
        self.assertEqual(result["distinct_input_seed_count"], 2)
        self.assertNotIn("independent_input_count", result)

    def test_missing_and_duplicate_attempts_cannot_pass(self):
        rows = regular_rows(2, 2)
        with self.assertRaisesRegex(ValueError, "schedule mismatch"):
            s.validate(rows[:-1], "fresh", 2)
        with self.assertRaisesRegex(ValueError, "duplicate"):
            s.validate(rows + [rows[0]], "fresh", 2)
        result = s.summarize(rows[:-1], "fresh", 2, allow_incomplete=True)
        self.assertEqual(result["qualification"]["verdict"], "incomplete_qualification")

    def test_seed_and_nonfinite_output_refuse(self):
        rows = regular_rows(1, 1)
        rows[0]["bootstrap_seed"] += 1
        with self.assertRaisesRegex(ValueError, "seed"):
            s.validate(rows, "fresh", 1)

    def test_noise_axes_and_generator_interventions_are_checked(self):
        rows = regular_rows(1, 1)
        rows[2]["replicate_rho_mean"] = [[.1, .2, .3]]
        with self.assertRaisesRegex(ValueError, "wrong-axis"):
            s.validate(rows, "fresh", 1)
        rows = regular_rows(1, 1)
        rows[0]["generating_rho"] = [[.1, .2, .3]]
        with self.assertRaisesRegex(ValueError, "intervention"):
            s.validate(rows, "fresh", 1)
        rows = regular_rows(1, 1)
        rows[0]["variance"][0] = float("nan")
        with self.assertRaisesRegex(ValueError, "variance"):
            s.validate(rows, "fresh", 1)

    def test_failed_attempt_stays_in_all_attempt_denominator(self):
        rows = regular_rows(2, 2)
        rows[1]["status"] = "failed"
        rows[1]["completed_replicates"] = 27
        rows[1]["error"] = "replicate 28 failed"
        result = s.summarize(rows, "fresh", 2)
        group = next(g for g in result["groups"] if g["scenario"] == "baseline")
        self.assertEqual(group["metrics"][0]["coverage"], 1.0)
        self.assertEqual(group["metrics"][0]["success_and_coverage_per_attempt"], .5)
        self.assertEqual(group["failure_records"][0]["completed_replicates"], 27)

    def test_exact_binomial_boundary_formula(self):
        n = 500
        zero, all_ = s.clopper_pearson(0, n), s.clopper_pearson(n, n)
        self.assertEqual(zero[0], 0.0)
        self.assertEqual(all_[1], 1.0)
        self.assertAlmostEqual(zero[1], 1 - (s.ALPHA / 2) ** (1 / n), places=13)
        self.assertAlmostEqual(all_[0], (s.ALPHA / 2) ** (1 / n), places=13)

    def test_no_early_acceptance_and_final_band_admission(self):
        early = s.gate(regular_rows(500, 475), 500, True)
        self.assertEqual(early["verdict"], "continue_to_next_checkpoint")
        final = s.gate(regular_rows(2000, 1900), 2000, True)
        self.assertEqual(final["verdict"], "accepted_declared_approximate_band")
        self.assertEqual(len(final["coordinates"]), 48)
        self.assertTrue(all(c["inside_band"] for c in final["coordinates"]))

    def test_one_bad_coordinate_rejects_the_candidate(self):
        rows = regular_rows(500, 475)
        for r in rows:
            if r["scenario"] == "strong_ar":
                r["lower"][4] = -1.0 if r["dataset_index"] < 410 else 1.0
        result = s.gate(rows, 500, True)
        self.assertEqual(result["verdict"], "prespecified_calibration_rejection")
        rejected = [c for c in result["coordinates"] if c["outside_band"]]
        self.assertEqual([(c["scenario"], c["coordinate"]) for c in rejected],
                         [("strong_ar", "coefficient_1_voxel_1")])

    def test_failure_and_timeout_are_not_statistical_rejection(self):
        rows = regular_rows(500, 475)
        rows[0]["status"] = "failed"
        self.assertEqual(s.gate(rows, 500, True)["verdict"], "computational_qualification_failure")
        self.assertEqual(s.gate(rows, 500, False)["verdict"], "incomplete_qualification")

    def test_final_overlap_is_inconclusive(self):
        result = s.gate(regular_rows(2000, 1860), 2000, True)
        self.assertEqual(result["verdict"], "inconclusive")


if __name__ == "__main__":
    unittest.main()
