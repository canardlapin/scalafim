#!/usr/bin/env python3
"""Finalize or check the locked design-corrected AR receipt."""
from receipt_tools import REPO_ROOT, check_requested, finalize_receipt

if __name__ == "__main__":
  raise SystemExit(finalize_receipt(
    "docs/scenarios/fixtures/ar.fmriar-bias.v1.r.json",
    "scalafim-r-fmriar-bias-fixture/v1",
    "modules/ar/shared/src/test/scala/scalafim/fmri/ar/fixtures/FmriArBiasRFixture.scala",
    "ar_estimation",
    check=check_requested(__doc__ or ""),
    lock_path=REPO_ROOT / "tools/r-parity/ar-reference-lock.json",
  ))
