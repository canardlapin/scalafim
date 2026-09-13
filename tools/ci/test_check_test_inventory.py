from pathlib import Path
import sys
import unittest


sys.path.insert(0, str(Path(__file__).resolve().parent))
from check_test_inventory import inspect_inventory  # noqa: E402


def build(test_all: str, examples: str = "") -> str:
    return (
        f'addCommandAlias("scalafimTestAll", ";{test_all}")\n'
        f'addCommandAlias("examplesTest", ";{examples}")\n'
    )


class TestInventorySuite(unittest.TestCase):
    def test_missing_estimates_target_fails(self) -> None:
        _, errors = inspect_inventory(
            build("coreJVM/test;estimatesJVM/test"),
            "run_batch core coreJVM/test\n",
        )
        self.assertIn("missing release targets: estimatesJVM/test", errors)

    def test_duplicate_target_fails(self) -> None:
        _, errors = inspect_inventory(
            build("coreJVM/test"),
            "run_batch one coreJVM/test\nrun_batch two coreJVM/test\n",
        )
        self.assertIn("duplicate release targets: coreJVM/test", errors)

    def test_unknown_target_fails(self) -> None:
        _, errors = inspect_inventory(
            build("coreJVM/test"),
            "run_batch core coreJVM/test typoJVM/test\n",
        )
        self.assertIn("unknown release targets: typoJVM/test", errors)

    def test_documented_display_exclusions_are_allowed(self) -> None:
        _, errors = inspect_inventory(
            build(
                "coreJVM/test;imageViewJavafxJVM/test;surfaceViewJavafxJVM/test"
            ),
            "run_batch core coreJVM/test\n",
        )
        self.assertEqual(errors, [])

    def test_extra_examples_are_reported_and_required(self) -> None:
        inventory, errors = inspect_inventory(
            build("coreJVM/test", "coreJVM/test;demoJVM/test"),
            "run_batch core coreJVM/test\nrun_batch examples demoJVM/test\n",
        )
        self.assertEqual(errors, [])
        self.assertEqual(inventory["extra_example_tests"], ["demoJVM/test"])

        _, missing_errors = inspect_inventory(
            build("coreJVM/test", "coreJVM/test;demoJVM/test"),
            "run_batch core coreJVM/test\n",
        )
        self.assertIn("missing extra example tests: demoJVM/test", missing_errors)


if __name__ == "__main__":
    unittest.main()
