#!/usr/bin/env python3
"""Finalize or check the external fmriAR algorithm receipt."""

from receipt_tools import check_requested, finalize_receipt


def main() -> int:
  return finalize_receipt(
    "docs/scenarios/fixtures/ar.fmriar-parity.v1.r.json",
    "scalafim-r-fmriar-parity-fixture/v1",
    "modules/ar/shared/src/test/scala/scalafim/fmri/ar/fixtures/FmriArRFixture.scala",
    "ar_estimation_and_whitening",
    check=check_requested(__doc__ or ""),
  )


if __name__ == "__main__":
  raise SystemExit(main())
