#!/usr/bin/env python3
"""Finalize or check the external realistic-nuisance receipt."""

from receipt_tools import check_requested, finalize_receipt


def main() -> int:
  return finalize_receipt(
    "docs/scenarios/fixtures/fit.realistic-nuisance.v1.r.json",
    "scalafim-r-realistic-nuisance-fixture/v1",
    "modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/RealisticNuisanceRFixture.scala",
    "independent_end_to_end",
    check=check_requested(__doc__ or ""),
  )


if __name__ == "__main__":
  raise SystemExit(main())
