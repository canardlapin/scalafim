#!/usr/bin/env python3
"""Plant realistic bugs in the bootstrap research harness one at a time, run the
research suites on the JVM through the shared sbt runner, record which tests
catch each bug, and restore the source (verified by SHA-256).

Usage (from the repository root):
  python3 tools/group-bootstrap-research/mutate.py [MUTATION_ID ...]
"""
import hashlib
import json
import pathlib
import re
import subprocess
import sys

ROOT = pathlib.Path.cwd()
RUNNER = "/private/tmp/scalafim-execution-20260929/run-sbt.py"
LOGS = pathlib.Path("/private/tmp/scalafim-execution-20260929/logs")
PKG = "modules/group/shared/src/test/scala/scalafim/group/research/bootstrap/"
TASK = "groupJVM/testOnly scalafim.group.research.bootstrap.*"

MUTATIONS = [
    (
        "M1",
        "uncentred statistic: candidates draw from the unrestricted fit",
        PKG + "Bootstrap.scala",
        "    case UnrestrictedUncentred | UnrestrictedRecentred => false\n    case _ => true",
        "    case UnrestrictedUncentred | UnrestrictedRecentred => false\n    case _ => false",
    ),
    (
        "M2",
        "wrong df: restricted PM targets n - p instead of n - p0",
        PKG + "PmKernel.scala",
        "restricted.fit(shifted, v, design.restrictedDf, policy)",
        "restricted.fit(shifted, v, design.residualDf, policy)",
    ),
    (
        "M3",
        "missing u*: the heterogeneity innovation is dropped from every scheme",
        PKG + "Bootstrap.scala",
        "val tauU = if scheme == Scheme.OmitU then 0.0 else math.sqrt(tau2World)",
        "val tauU = if scheme == Scheme.OmitU then 0.0 else 0.0 * math.sqrt(tau2World)",
    ),
    (
        "M4",
        "v double-count in B-fixV: v* = v chi2/nu on top of e* ~ N(0, v)",
        PKG + "Bootstrap.scala",
        "case Scheme.FixV | Scheme.KnownVFrozenTau => v",
        "case Scheme.KnownVFrozenTau => v",
    ),
    (
        "M5",
        "plus-one rule dropped: p = k/B",
        PKG + "Bootstrap.scala",
        "def pLower: Double = (1.0 + exceed) / (draws + 1.0)",
        "def pLower: Double = exceed.toDouble / draws",
    ),
    (
        "M6",
        "B-EB posterior df ignores nu_i: sigma*^2 = d0 s~^2 / chi2",
        PKG + "Bootstrap.scala",
        "else (d0 + nu) * posteriorScale(v, nu) / chi",
        "else d0 * posteriorScale(v, nu) / chi",
    ),
    (
        "M7",
        "mKH floor dropped: unmodified Knapp-Hartung scale q",
        PKG + "PmKernel.scala",
        "def mkhScale: Double = math.max(1.0, full.qStatistic / design.residualDf)",
        "def mkhScale: Double = full.qStatistic / design.residualDf",
    ),
    (
        "M8",
        "sign flip drops failed orbit points instead of |T| := 0",
        PKG + "SignFlip.scala",
        "        case _ =>\n          failures += 1\n          0.0\n      g += 1",
        "        case _ =>\n          failures += 1\n          Double.NegativeInfinity\n      g += 1",
    ),
    (
        "X5",
        "family H sigma^2 drawn as scaled chi^2 instead of scaled-inverse chi^2",
        PKG + "ModelJ.scala",
        "10.0 * 0.52 / lane.nextChiSquare(10.0)",
        "0.52 * lane.nextChiSquare(10.0) / 10.0",
    ),
    (
        "X6",
        "lognormal u/e not at unit variance",
        PKG + "ModelJ.scala",
        "/ math.sqrt((e - 1.0) * e)",
        "/ ((e - 1.0) * e)",
    ),
    (
        "X7",
        "t3 u/e not at unit variance",
        PKG + "ModelJ.scala",
        "z / math.sqrt(lane.nextChiSquare(3.0) / 3.0) / math.sqrt(3.0)",
        "z / math.sqrt(lane.nextChiSquare(3.0) / 3.0)",
    ),
    (
        "X8",
        "AR naive variance uses RSS/T instead of RSS/(T - rank)",
        PKG + "ModelJ.scala",
        "rss / series.nominalDf)",
        "rss / series.length)",
    ),
]


def sha(path: pathlib.Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def run(mid: str, title: str, rel: str, old: str, new: str) -> dict:
    path = ROOT / rel
    original = path.read_bytes()
    before = sha(path)
    text = original.decode()
    if text.count(old) != 1:
        raise SystemExit(f"{mid}: anchor must occur exactly once in {rel}")
    path.write_text(text.replace(old, new))
    log = f"bootstrap-harness-mutation-{mid}.log"
    attempt = 1
    while (LOGS / log).exists():  # never reuse a log name (the runner refuses existing logs)
        attempt += 1
        log = f"bootstrap-harness-mutation-{mid}-r{attempt}.log"
    try:
        code = subprocess.call(
            ["python3", RUNNER, str(ROOT), log, TASK], stdout=subprocess.DEVNULL
        )
    finally:
        path.write_bytes(original)
    after = sha(path)
    if after != before:
        raise SystemExit(f"{mid}: restore failed ({after} != {before})")
    body = (LOGS / log).read_text(errors="replace")
    ansi = re.compile(r"\x1b\[[0-9;]*m")
    failed = []
    for line in body.splitlines():
        clean = ansi.sub("", line)
        m = re.search(
            r"==> X scalafim\.group\.research\.bootstrap\.(\w+)\.(.*?)\s+\d+\.\d+s",
            clean,
        )
        if m:
            failed.append(f"{m.group(1)}: {m.group(2)}")
    compiled = "Compilation failed" not in body
    summary = next(
        (
            ansi.sub("", l)
            for l in body.splitlines()
            if "Passed: Total" in l or "Failed: Total" in l
        ),
        "",
    )
    return {
        "id": mid,
        "bug": title,
        "file": rel,
        "exit": code,
        "compiled": compiled,
        "summary": summary.strip(),
        "caught_by": failed,
        "log": str(LOGS / log),
        "restored_sha256": after,
    }


def main() -> None:
    wanted = set(sys.argv[1:])
    results = [run(*m) for m in MUTATIONS if not wanted or m[0] in wanted]
    print(json.dumps(results, indent=2))


if __name__ == "__main__":
    main()
