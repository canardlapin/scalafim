#!/usr/bin/env python3
"""Finalize or check the external structural 2x2 design receipt."""

from receipt_tools import check_requested, finalize_receipt


def main() -> int:
  return finalize_receipt(
    "docs/scenarios/fixtures/design.structural-2x2.v1.r.json",
    "scalafim-r-design-fixture/v1",
    "modules/design/shared/src/test/scala/scalafim/fmri/design/fixtures/Structural2x2RFixture.scala",
    "design_construction",
    check=check_requested(__doc__ or ""),
  )


if __name__ == "__main__":
  raise SystemExit(main())
