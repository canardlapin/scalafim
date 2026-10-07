#!/usr/bin/env python3
"""Verify archived bounded-preparation evidence without third-party packages."""
import gzip
import hashlib
import json
import re
from pathlib import Path

PACKET = Path(__file__).resolve().parent
ROOT = PACKET.parents[2]


def main():
    manifest = json.loads((PACKET / "manifest.json").read_text())
    for path, expected in manifest["sha256"].items():
        actual = hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
        assert actual == expected, f"hash mismatch: {path}"
    assert manifest["qualification"] == "not-admitted"
    validation = json.loads((PACKET / "validation.json").read_text())
    for filename in ("regression.log.gz", "workflow.log.gz"):
        log = gzip.decompress((PACKET / filename).read_bytes()).decode()
        gates = re.findall(r"\[sbt-warm\] (.*): exit (\d+) in ([\d.]+)s", log)
        assert gates and all(int(code) == 0 for _, code, _ in gates), filename
        assert "[error]" not in log, filename
        assert all(int(failed) == int(errors) == 0 for failed, errors in
                   re.findall(r"Failed (\d+), Errors (\d+), Passed \d+", log)), filename
    assert all(gate["exit"] == 0 for gate in validation["gates"])
    commands = {gate["command"] for gate in validation["gates"]}
    assert {"designJVM/test", "designJS/test", "fitJVM/test", "fitJS/test", "scalafimCompileAll"} <= commands
    stress = json.loads((PACKET / "stress-receipts.json").read_text())
    for platform in ("JVM", "JS"):
        receipt = stress[platform]
        assert receipt["rows"] == 600 and receipt["trials"] == 1200 and receipt["rank"] == 10
        assert receipt["blocks"] == 38 and receipt["maxBlockValues"] == 192000
        assert receipt["retainedSourceValues"] == 0
        assert receipt["retainedValues"] == 9116727 and receipt["bandwidth"] == 127
        assert receipt["bankBytes"] == 204786040
        assert receipt["qualification"] == "not-admitted" and receipt["prepareSeconds"] > 0
    diagnostic = json.loads(gzip.decompress((PACKET / "stress-diagnostic.json.gz").read_bytes()))
    for key in ("failure", "preparationError", "outputPreparationError", "executionError"):
        assert key not in diagnostic, (key, diagnostic.get(key))
    assert diagnostic["qualification"] == "not-admitted"
    assert diagnostic["trials"] == 1200 and diagnostic["rows"] == 600
    assert diagnostic["attemptedDelivered"] == 4
    assert diagnostic["retainedSourceDesignValues"] == 0
    assert diagnostic["trialPreparationReceipt"]["maxLoweredBlockValues"] == 192000
    assert sum(diagnostic["statuses"].values()) == diagnostic["attemptedDelivered"]
    assert diagnostic["float32OutputBytes"] == diagnostic["emitted"] * 1200 * 4
    assert diagnostic["readValues"] == 4 * 600
    assert "trial-preparation/v1" in diagnostic["provenance"]
    assert "certificate" in diagnostic["originalFamilyAdmission"].lower()
    print(f"Verified {len(manifest['sha256'])} hashes, both-platform stress preparation and four public attempts; not admitted.")


if __name__ == "__main__":
    main()
