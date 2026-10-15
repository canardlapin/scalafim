#!/usr/bin/env python3
"""Finalize/check the independently versioned fmriAR 0.4.1 receipt."""
import json
from pathlib import Path
from receipt_tools import REPO_ROOT, check_requested, finalize_receipt, file_sha256

if __name__ == "__main__":
    checking = check_requested(__doc__)
    result = finalize_receipt(
        "docs/scenarios/fixtures/ar.fmriar-041.v1.r.json",
        "scalafim-r-fmriar-041-fixture/v1",
        "modules/ar/shared/src/test/scala/scalafim/fmri/ar/fixtures/FmriAr041RFixture.scala",
        "ar_standard_bic_voxel_acf_stationary_whitening",
        check=checking,
        lock_path=REPO_ROOT / "tools/r-parity/ar-041-reference-lock.json",
    )
    path = REPO_ROOT / "docs/scenarios/fixtures/ar.fmriar-041.v1.r.json"
    payload = json.loads(path.read_text())
    expected = file_sha256(REPO_ROOT / "modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/FmriAr041GlsFixture.scala")
    if checking:
        if payload["receipt"]["hashes"].get("generated_fit_scala_sha256") != expected:
            raise SystemExit("stale fmriAR 0.4.1 GLS consumer fixture")
    else:
        payload["receipt"]["hashes"]["generated_fit_scala_sha256"] = expected
        path.write_text(json.dumps(payload,sort_keys=True,indent=2)+"\n")
    raise SystemExit(result)
