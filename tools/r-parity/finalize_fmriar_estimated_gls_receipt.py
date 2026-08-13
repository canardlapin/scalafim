#!/usr/bin/env python3
"""Finalize or check the external fmriAR estimated-GLS receipt."""

from receipt_tools import check_requested, finalize_receipt


def main() -> int:
  return finalize_receipt(
    "docs/scenarios/fixtures/fit.fmriar-estimated-gls.v1.r.json",
    "scalafim-r-fmriar-estimated-gls-fixture/v1",
    "modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/FmriArEstimatedGlsFixture.scala",
    "fit_inference",
    check=check_requested(__doc__ or ""),
  )


if __name__ == "__main__":
  raise SystemExit(main())
