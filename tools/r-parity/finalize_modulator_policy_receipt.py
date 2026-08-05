#!/usr/bin/env python3
"""Finalize or check the external modulator-policy receipt."""

from receipt_tools import check_requested, finalize_receipt


def main() -> int:
  return finalize_receipt(
    "docs/scenarios/fixtures/design.modulator-policies.v1.r.json",
    "scalafim-r-policy-fixture/v1",
    "modules/design/shared/src/test/scala/scalafim/fmri/design/fixtures/ModulatorPolicyRFixture.scala",
    "design_construction",
    check=check_requested(__doc__ or ""),
  )


if __name__ == "__main__":
  raise SystemExit(main())
