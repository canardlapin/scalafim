#!/usr/bin/env python3
"""Run focused JVM/JS gates and a finite, reversible sample-budget mutation.

Run from the repository root with no concurrent build or source mutation:
  COURSIER_REPOSITORIES=https://repo.maven.apache.org/maven2 python3 docs/verification/neural-input-budget-20261005/run.py
"""
import datetime
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tarfile
import tempfile
import time

ROOT = Path(__file__).resolve().parents[3]
EVIDENCE = Path(__file__).resolve().parent
SOURCE = ROOT / "modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/regressor/NeuralInput.scala"
SUITE = ROOT / "modules/hrf/shared/src/test/scala/scalafim/fmri/hrf/regressor/NeuralInputSuite.scala"
SELECTOR = SUITE.with_name("NeuralInputBudgetMutationSuite.scala")
FILES = [SOURCE, SUITE, ROOT / "modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/regressor/Regressor.scala"]
GUARD = 'count <- ConvolutionDiscretization.sampleCount(end - from, resolution, "neural input grid")\n        .left.map(NeuralInputError.InvalidGrid.apply)'
MUTANT = 'count <- Right(math.floor((end - from) / resolution).toInt + 1)'


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    baseline = SOURCE.read_text()
    assert baseline.count(GUARD) == 1
    assert not SELECTOR.exists()
    logs = Path(tempfile.mkdtemp(prefix="scalafim-neural-input-budget-"))
    runs = []
    env = dict(os.environ)
    env.setdefault("COURSIER_REPOSITORIES", "https://repo.maven.apache.org/maven2")

    def run(name, task, expected_code):
        command = ["python3", "tools/build/sbt-warm", task]
        started = time.monotonic()
        with (logs / (name + ".log")).open("w") as stream:
            result = subprocess.run(command, cwd=ROOT, env=env, stdout=stream, stderr=subprocess.STDOUT)
        output = (logs / (name + ".log")).read_text()
        summaries = re.findall(r"Failed:? (\d+), Errors:? (\d+), Passed:? (\d+)", output)
        item = dict(name=name, command=command, exit_code=result.returncode,
                    seconds=time.monotonic() - started, summaries=summaries)
        runs.append(item)
        print(json.dumps(item), flush=True)
        assert result.returncode == expected_code, output[-8000:]
        assert summaries, output[-8000:]
        if expected_code:
            assert any(summary == ("1", "0", "0") for summary in summaries), summaries
        else:
            assert any(summary == ("0", "0", "13") for summary in summaries), summaries

    for platform in ("JVM", "JS"):
        run("before-" + platform.lower(), f"hrf{platform}/testOnly scalafim.fmri.hrf.regressor.NeuralInputSuite", 0)
    try:
        SOURCE.write_text(baseline.replace(GUARD, MUTANT))
        SELECTOR.write_text((EVIDENCE / SELECTOR.name).read_text())
        for platform in ("JVM", "JS"):
            run("mutation-" + platform.lower(), f"hrf{platform}/testOnly scalafim.fmri.hrf.regressor.NeuralInputBudgetMutationSuite", 1)
    finally:
        SOURCE.write_text(baseline)
        SELECTOR.unlink(missing_ok=True)
    for platform in ("JVM", "JS"):
        run("restored-" + platform.lower(), f"hrf{platform}/testOnly scalafim.fmri.hrf.regressor.NeuralInputSuite", 0)
    assert SOURCE.read_text() == baseline
    assert not SELECTOR.exists()
    archive = EVIDENCE / "logs-and-sources.tar.gz"
    with tarfile.open(archive, "w:gz") as tar:
        for log in sorted(logs.iterdir()):
            tar.add(log, arcname=log.name)
        for path in FILES:
            tar.add(path, arcname=str(path.relative_to(ROOT)))
        tar.add(EVIDENCE / "run.py", arcname="run.py")
        tar.add(EVIDENCE / SELECTOR.name, arcname=SELECTOR.name)
    receipt = dict(
        mote="bd-01M479NBKJ8S6216HXD787MANK",
        verified_at_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
        base_commit=subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
        maximum_grid_samples=1000001,
        behavior="Typed finite grid and event-end validation before conversion/allocation; floating bin clipping before integer conversion and block iteration. Preserves inclusive floor bins and direct event amplitudes for either summate value.",
        source_sha256={str(path.relative_to(ROOT)): sha(path) for path in FILES},
        runs=runs,
        mutation=dict(change="Replaced only the bounded count helper with the former floor(width/resolution).toInt + 1 count.",
                      selector="One existing just-over-budget regression; maximum mutant allocation is two 1,000,002-element arrays (about 16 MB total).",
                      restored=True, temporary_selector_removed=True),
        archive_sha256=sha(archive),
    )
    (EVIDENCE / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print("Evidence:", EVIDENCE / "receipt.json", flush=True)


if __name__ == "__main__":
    main()
