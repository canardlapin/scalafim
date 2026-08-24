#!/usr/bin/env python3
"""Finalize or check the independent base-R WLS receipt."""

from receipt_tools import check_requested, finalize_receipt


def main() -> int:
  return finalize_receipt(
    "docs/scenarios/fixtures/fit.structural-s18-wls.v1.r.json",
    "scalafim-r-wls-fixture/v1",
    "modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/WlsRFixture.scala",
    "fit_inference",
    check=check_requested(__doc__ or ""),
  )


if __name__ == "__main__":
  raise SystemExit(main())
