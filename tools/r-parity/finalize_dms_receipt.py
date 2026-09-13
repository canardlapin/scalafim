#!/usr/bin/env python3
"""Finalize or check the external delayed-match-to-sample receipt."""

from receipt_tools import REPO_ROOT, check_requested, finalize_receipt


def main() -> int:
  return finalize_receipt(
    "docs/scenarios/fixtures/fit.dms-multiphase-dsl.corrected-spmg.v1.r.json",
    "scalafim-r-dms-fixture/v1",
    "modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/CorrectedSpmgDmsRFixture.scala",
    "independent_end_to_end",
    check=check_requested(__doc__ or ""),
    lock_path=REPO_ROOT / "tools/r-parity/mixed-tr-reference-lock.json",
  )


if __name__ == "__main__":
  raise SystemExit(main())
