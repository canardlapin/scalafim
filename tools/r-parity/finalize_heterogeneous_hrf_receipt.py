#!/usr/bin/env python3
"""Finalize or check the external heterogeneous-HRF receipt."""

from receipt_tools import check_requested, finalize_receipt


def main() -> int:
  return finalize_receipt(
    "docs/scenarios/fixtures/design.heterogeneous-hrf.v1.r.json",
    "scalafim-r-kernel-fixture/v1",
    "modules/design/shared/src/test/scala/scalafim/fmri/design/fixtures/HeterogeneousHrfRFixture.scala",
    "hrf_evaluation",
    check=check_requested(__doc__ or ""),
  )


if __name__ == "__main__":
  raise SystemExit(main())
