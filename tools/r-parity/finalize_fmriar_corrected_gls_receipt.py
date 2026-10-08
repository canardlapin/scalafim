#!/usr/bin/env python3
"""Finalize or check the locked design-corrected AR receipt."""
from receipt_tools import REPO_ROOT, check_requested, finalize_receipt

if __name__ == "__main__":
  raise SystemExit(finalize_receipt(
    "docs/scenarios/fixtures/fit.fmriar-corrected-gls.v1.r.json",
    "scalafim-r-fmriar-corrected-gls-fixture/v1",
    "modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/FmriArCorrectedGlsFixture.scala",
    "fit_inference",
    check=check_requested(__doc__ or ""),
    lock_path=REPO_ROOT / "tools/r-parity/ar-reference-lock.json",
  ))
