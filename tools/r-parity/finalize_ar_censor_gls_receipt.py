#!/usr/bin/env python3
"""Finalize or check the external AR/censor GLS receipt."""

from receipt_tools import check_requested, finalize_receipt


def main() -> int:
  return finalize_receipt(
    "docs/scenarios/fixtures/fit.ar-censor-boundary-gls.v1.r.json",
    "scalafim-r-ar-censor-gls-fixture/v1",
    "modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/ArCensorGlsRFixture.scala",
    "fit_inference",
    check=check_requested(__doc__ or ""),
  )


if __name__ == "__main__":
  raise SystemExit(main())
