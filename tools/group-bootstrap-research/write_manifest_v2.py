#!/usr/bin/env python3
"""Write tools/group-bootstrap-research/manifest-v2.json (+ .sha256).

Manifest v2 extends the unchanged cell manifest (cells.json, its parent) with the
owner-decided selection and decision rules, the SHA-256 of every harness source
and of the R reference script, and the sealed heavy log (hash only; its content is
never read into any report). Canonical form: sorted keys, no whitespace, ASCII,
one trailing newline. Run from the repository root after the final source edit (add --freeze, after the gates pass, to mark it "frozen for pilot"; --freeze-confirmation marks "frozen for confirmation");
ManifestFileSuite fails if a harness source changes without this file being rewritten.
"""
import hashlib
import json
import pathlib
import sys

ROOT = pathlib.Path.cwd()
TOOLS = ROOT / "tools/group-bootstrap-research"
SOURCE_DIRS = [
    "modules/group/shared/src/test/scala/scalafim/group/research/bootstrap",
    "modules/group/jvm/src/test/scala/scalafim/group/research/bootstrap",
]
R_SCRIPT = "tools/group-bootstrap-research/generate_reference_fixtures.R"
SELF = "tools/group-bootstrap-research/write_manifest_v2.py"
HEAVY_LOG = pathlib.Path(
    "/private/tmp/scalafim-execution-20260929/logs/bootstrap-harness-jvm-heavy.log"
)


def sha(path: pathlib.Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


FROZEN = "frozen for pilot"
PENDING = "pending re-review; not frozen"
FROZEN_CONFIRMATION = "frozen for confirmation"
# --freeze marks "frozen for pilot"; --freeze-confirmation marks "frozen for confirmation" (the owner's later step).
if "--freeze" in sys.argv[1:] and "--freeze-confirmation" in sys.argv[1:]:
    sys.exit("pass at most one of --freeze and --freeze-confirmation")
STATUS = (
    FROZEN_CONFIRMATION
    if "--freeze-confirmation" in sys.argv[1:]
    else FROZEN if "--freeze" in sys.argv[1:] else PENDING
)


def main() -> None:
    sources = {}
    for d in SOURCE_DIRS:
        for p in sorted((ROOT / d).glob("*.scala")):
            sources[str(p.relative_to(ROOT))] = sha(p)
    sources[R_SCRIPT] = sha(ROOT / R_SCRIPT)
    sources[SELF] = sha(ROOT / SELF)
    cells_sha = sha(TOOLS / "cells.json")
    manifest = {
        "schema": "scalafim-group-bootstrap-manifest/v2",
        "mote": "bd-01M21BNZR9ZBRAYY9JD5WCQ8KX",
        "status": STATUS,
        "parent": {
            "path": "tools/group-bootstrap-research/cells.json",
            "sha256": cells_sha,
        },
        "selection_rule": {
            "version": "selection-rule/v1",
            "decided": "owner 2026-09-30",
            "score": "max over B-plug, B-fixV, B-EB of (null rejections + study failures) / R; study failures = outer PM/solve failures + Unresolved p bounds",
            "accounting": "CountAsRejection",
            "pool": "CoreMinusFixed (90 core cells minus the 6 fixed confirmation cells)",
            "candidates": ["B-plug", "B-fixV", "B-EB"],
            "ties": "LowestIdString",
            "count": 6,
            "clarification": "each study counts once in the score, whether Reject, Unresolved or Failed; select refuses unless every candidate of every pool cell has exactly R = 2000 pilot studies (hence equal R across candidates)",
        },
        "decision_rule": {
            "version": "decision-rule/v2",
            "decided": "owner 2026-09-30 (Bound = Adopt restricted to S)",
            "precedence": [
                "Adopt: all 12 cells pass null and failure, Gain in >= 1 power cell, Non-loss in all 4",
                "Bound(union of qualifying S), S in {n >= 20, nu >= 40}: every confirmation cell in S passes null and failure, Gain in >= 1 power cell in S, Non-loss in every power cell in S; Fails outside S allowed; S without a power cell cannot qualify",
                "Decline: any Fail or any Definite loss",
                "Unresolved: otherwise",
            ],
            "clarifications": [
                "Fail means a null Fail (k >= 1456); a failure count above 59 with k < 1456 does not Pass, so that cell is Unresolved, not Decline",
                "a Definite loss in a power cell outside S does not block Bound(S), because Bound precedes Decline",
            ],
            "refusal": "outcome refuses unless R = 20000 for every cell, exactly 12 distinct core confirmation cells including the 6 fixed, and exactly the 4 declared power cells",
            "null_pass_max_k": 1149,
            "null_fail_min_k": 1456,
            "failure_pass_max_f": 59,
            "tail_delta": "6.25e-6",
            "margin": "0.065",
            "gain_margin": "0",
            "non_loss_margin": "0.02",
        },
        "controls": {
            "C-knownv-frozen-tau": "u* drawn from the restricted tau0-hat^2; T* is the Wald statistic with tau^2 frozen at the observed unrestricted tau-hat^2 (owner-confirmed 2026-09-30)"
        },
        "sources": sources,
        "heavy_log": {
            "path": str(HEAVY_LOG),
            "sha256": sha(HEAVY_LOG),
            "status": "sealed: contains descriptive rates on three declared cells from a since-deleted test; not evidence; not to be read or used for cell selection",
        },
    }
    text = (
        json.dumps(manifest, sort_keys=True, separators=(",", ":"), ensure_ascii=True)
        + "\n"
    )
    (TOOLS / "manifest-v2.json").write_text(text)
    digest = hashlib.sha256(text.encode()).hexdigest()
    (TOOLS / "manifest-v2.json.sha256").write_text(f"{digest}  manifest-v2.json\n")
    print(f"manifest-v2.json sha256 {digest} ({len(sources)} sources, status: {STATUS})")


if __name__ == "__main__":
    main()
