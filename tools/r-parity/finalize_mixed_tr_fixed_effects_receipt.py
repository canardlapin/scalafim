#!/usr/bin/env python3
"""Finalize or check the external mixed-TR fixed-effects receipt."""

from receipt_tools import check_requested, finalize_receipt


def main() -> int:
  return finalize_receipt(
    "docs/scenarios/fixtures/fit.mixed-tr-fixed-effects.v1.r.json",
    "scalafim-r-fixed-effects-fixture/v1",
    "modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/MixedTrFixedEffectsRFixture.scala",
    "independent_end_to_end",
    check=check_requested(__doc__ or ""),
  )


if __name__ == "__main__":
  raise SystemExit(main())
