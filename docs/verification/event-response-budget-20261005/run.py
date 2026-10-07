#!/usr/bin/env python3
"""Focused portable gates and bounded, reversible admission mutations.

Run from the repository root while no other build/source mutation is active:
  COURSIER_REPOSITORIES=https://repo.maven.apache.org/maven2 python3 docs/verification/event-response-budget-20261005/run.py
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
SOURCE = ROOT / "modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/EventResponseNormalization.scala"
SUITE = ROOT / "modules/hrf/shared/src/test/scala/scalafim/fmri/hrf/EventResponseNormalizationSuite.scala"
DESIGN_SUITE = ROOT / "modules/design/shared/src/test/scala/scalafim/fmri/design/event/EventResponseConvolutionSuite.scala"
POLICY = ROOT / "modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/NormalizationReferenceGrid.scala"
SELECTOR = SUITE.with_name("EventResponseBudgetMutationSuite.scala")
FILES = [SOURCE, SUITE, DESIGN_SUITE, POLICY]
MUTATIONS = [
    ("work", "if !work.isFinite || work > MaximumReferenceEvaluations.toDouble then", "if false then", [
        "composed trapezoid work is refused before evaluating the kernel",
        "nested blocked, lagged and bound kernels contribute their response work",
        "exact piecewise response work includes all segments before evaluation",
    ]),
    ("endpoint", "else if !(intervals * referenceStep.value).isFinite then", "else if false then", [
        "reference endpoint overflow is a typed refusal before evaluation",
    ]),
    ("scale", "if kernel.nbasis > MaximumBasisScales then", "if false then", [
        "basis scale allocation is bounded for preserved and unit-peak responses",
    ]),
]


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def selector(names):
    literals = ",\n      ".join(json.dumps(name) for name in names)
    return '''package scalafim.fmri.hrf

class EventResponseBudgetMutationSuite extends EventResponseNormalizationSuite:
  override def munitTests(): Seq[munit.Test] =
    val names = Set(
      ''' + literals + '''
    )
    val selected = super.munitTests().filter(test => names.contains(test.name))
    assert(selected.length == names.size)
    selected
'''


def main():
    original = SOURCE.read_text()
    assert not SELECTOR.exists()
    snapshots = {str(path.relative_to(ROOT)): sha(path) for path in FILES}
    logs = Path(tempfile.mkdtemp(prefix="scalafim-event-response-budget-"))
    runs = []
    env = dict(os.environ)
    env.setdefault("COURSIER_REPOSITORIES", "https://repo.maven.apache.org/maven2")

    def run(name, tasks, failures=None):
        command = ["python3", "tools/build/sbt-warm", *tasks]
        started = time.monotonic()
        log = logs / (name + ".log")
        with log.open("w") as stream:
            result = subprocess.run(command, cwd=ROOT, env=env, stdout=stream, stderr=subprocess.STDOUT)
        output = log.read_text()
        summaries = re.findall(r"Failed:? (\d+), Errors:? (\d+), Passed:? (\d+)", output)
        item = dict(name=name, command=command, exit_code=result.returncode,
                    seconds=time.monotonic() - started, summaries=summaries,
                    compiler_warnings=bool(re.search(r"\[warn\] --|\[warn\] .*\.scala:", output)))
        runs.append(item)
        print(json.dumps(item), flush=True)
        assert result.returncode == (1 if failures is not None else 0), output[-8000:]
        assert summaries, output[-8000:]
        assert not item["compiler_warnings"], output[-8000:]
        if failures is not None:
            assert summaries == [(str(failures), "0", "0")], summaries
        else:
            assert all(failed == "0" and errors == "0" and int(passed) > 0 for failed, errors, passed in summaries), summaries

    focused = [f"hrf{platform}/testOnly scalafim.fmri.hrf.EventResponseNormalizationSuite" for platform in ("JVM", "JS")]
    design = [f"design{platform}/testOnly scalafim.fmri.design.event.EventResponseConvolutionSuite" for platform in ("JVM", "JS")]
    run("before", focused + design)
    try:
        for name, guard, replacement, names in MUTATIONS:
            assert original.count(guard) == 1, guard
            SOURCE.write_text(original.replace(guard, replacement))
            text = selector(names)
            SELECTOR.write_text(text)
            (logs / (name + "-selector.scala")).write_text(text)
            (logs / (name + "-source.scala")).write_text(SOURCE.read_text())
            for platform in ("JVM", "JS"):
                run("mutation-" + name + "-" + platform.lower(),
                    [f"hrf{platform}/testOnly scalafim.fmri.hrf.EventResponseBudgetMutationSuite"], len(names))
            SOURCE.write_text(original)
            SELECTOR.unlink()
    finally:
        SOURCE.write_text(original)
        SELECTOR.unlink(missing_ok=True)
    run("restored", focused + design)
    assert SOURCE.read_text() == original
    assert not SELECTOR.exists()
    assert snapshots == {str(path.relative_to(ROOT)): sha(path) for path in FILES}
    archive = EVIDENCE / "logs-and-sources.tar.gz"
    with tarfile.open(archive, "w:gz") as tar:
        for log in sorted(logs.iterdir()):
            tar.add(log, arcname=log.name)
        for path in FILES:
            tar.add(path, arcname=str(path.relative_to(ROOT)))
        tar.add(EVIDENCE / "run.py", arcname="run.py")
    receipt = dict(
        issue="bd-01M47BDT7PT73Z82RK6EETWDZ7",
        actor="scalafim-event-response-20261005",
        verified_at_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
        base_commit=subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
        limits=dict(reference_samples=1000000, basis_scales=1000000, scalar_evaluations=10000000),
        semantics=["Retains ceil reference grid and per-basis peaks; UnitPeak uses its reference step and ignores unused caller precision.",
                   "Impulse cost uses descriptor evaluation work; finite pulses use actual exact/piecewise/fallback and nested work estimate.",
                   "Finite final reference time, work and basis-scale admission precede arrays or kernel calls.",
                   "PreservePulseScale applies only basis-scale shape cap, performs no normalization work/precision admission.",
                   "Custom kernel internals and Gauss rule construction cost remain outside the declared scalar-evaluation budget."],
        source_sha256=snapshots, runs=runs,
        mutation_safety="Work and endpoint mutants stop at sentinel callbacks; scale mutant allocates only a 1,000,001-element retained vector before a type assertion fails. No huge reference loop is run.",
        restoration=dict(restored=True, temporary_selector_removed=True),
        archive_sha256=sha(archive),
    )
    (EVIDENCE / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print("Evidence:", EVIDENCE / "receipt.json", flush=True)


if __name__ == "__main__":
    main()
