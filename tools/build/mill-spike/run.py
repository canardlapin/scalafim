#!/usr/bin/env python3
"""Exploratory HRF build comparison; all generated builds live outside the repo.

Run with JDK 21, sbt and Node on PATH:
  python3 tools/build/mill-spike/run.py --directory /private/tmp/scalafim-build-spike

The first run downloads Mill 1.1.10 and primes dependency/compiler caches.
Timings exclude that bootstrap. Each workload covers both JVM and Scala.js.
Only disposable copies of ScalaFIM sources are edited or cleaned.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import statistics
import subprocess
import time
import xml.etree.ElementTree as ET


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[2]
MILL_VERSION = "1.1.10"
MILL_SHA256 = "63538d1cb27c29dd36821d832a964580e3bf046d956f6e6b3cd55d1f0124a561"
EDIT_PATH = "hrf/shared/src/main/scala/scalafim/fmri/hrf/TemporalDerivativeConvention.scala"
EDIT_TEXT = "invalid SPM kernel sampling grid: $detail"


def main():
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--directory", type=Path, required=True)
  parser.add_argument("--repetitions", type=int, default=3)
  args = parser.parse_args()
  if args.repetitions < 1:
    parser.error("--repetitions must be positive")
  destination = args.directory.resolve()
  if destination == REPO or REPO in destination.parents:
    parser.error("use a disposable directory outside the repository")
  destination.mkdir(parents=True, exist_ok=True)
  env = dict(os.environ)
  env["COURSIER_REPOSITORIES"] = "https://repo.maven.apache.org/maven2"
  env["TERM"] = "dumb"
  env["NO_COLOR"] = "1"
  env.pop("SBT_OPTS", None)
  env.pop("JAVA_OPTS", None)
  rows = []
  sources = {
    str(p.relative_to(REPO)): hashlib.sha256(p.read_bytes()).hexdigest()
    for p in sorted((REPO / "modules/hrf").rglob("*.scala"))
    if "target" not in p.parts
  }
  result = {
    "source_commit": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=REPO, text=True).strip(),
    "source_sha256": sources,
    "java": subprocess.run(["java", "-version"], capture_output=True, text=True, check=True).stderr.strip(),
    "node": subprocess.check_output(["node", "--version"], text=True).strip(),
    "host": platform.platform(),
    "repetitions": args.repetitions,
    "scope": "HRF only, JVM + Scala.js; dependencies already cached; sequential tools",
    "build_template_sha256": {name: hashlib.sha256((HERE / name).read_bytes()).hexdigest()
                               for name in ["build.mill", "build.sbt"]},
    "test_counts": {},
    "rows": rows,
  }

  def save():
    (destination / "timings.json").write_text(json.dumps(result, indent=2) + "\n")

  def run(name, directory, command, label, measured=False):
    log = destination / f"{name}-{label}.log"
    start = time.perf_counter()
    with log.open("w") as stream:
      completed = subprocess.run(command, cwd=directory, env=env, stdin=subprocess.DEVNULL,
                                 stdout=stream, stderr=subprocess.STDOUT)
    elapsed = time.perf_counter() - start
    print(f"{name} {label}: {elapsed:.3f}s (exit {completed.returncode})", flush=True)
    if measured:
      rows.append({"tool": name, "workload": label.rsplit("-", 1)[0], "seconds": elapsed,
                   "exit_code": completed.returncode, "command": command, "log": log.name})
      save()
    if completed.returncode:
      raise RuntimeError(f"Failed: {command}; inspect {log}")
    return elapsed

  for name in ["sbt-1.11.7", "sbt-1.13.0", "mill-1.1.10"]:
    directory = destination / name
    directory.mkdir(exist_ok=True)
    # Do not overwrite source inputs under an already-running server.
    if (directory / "hrf").exists():
      raise RuntimeError(f"Use a fresh directory; source copy exists: {directory}")
    shutil.copytree(REPO / "modules/hrf", directory / "hrf", ignore=shutil.ignore_patterns("target"))
    is_mill = name.startswith("mill")
    if is_mill:
      shutil.copyfile(HERE / "build.mill", directory / "build.mill")
      launcher = directory / "mill"
      subprocess.run(["curl", "-fLsS", "--retry", "3", "-o", str(launcher),
                      f"https://repo.maven.apache.org/maven2/com/lihaoyi/mill-dist/{MILL_VERSION}/mill-dist-{MILL_VERSION}-mill.sh"], check=True)
      if hashlib.sha256(launcher.read_bytes()).hexdigest() != MILL_SHA256:
        raise RuntimeError("Mill bootstrap checksum mismatch")
      launcher.chmod(0o755)
      prefix = [str(launcher), "-j", "1"]
      compile_command = prefix + ["{hrfJVM,hrfJS}.compile"]
      test_command = prefix + ["{hrfJVM,hrfJS}.test"]
      shutdown = [str(launcher), "shutdown"]
      cold_command = [str(launcher), "--no-daemon", "-j", "1", "{hrfJVM,hrfJS}.compile"]
    else:
      (directory / "project").mkdir(exist_ok=True)
      (directory / "project/build.properties").write_text(f"sbt.version={name[4:]}\n")
      (directory / "project/plugins.sbt").write_text(
        'addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")\n'
        'addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")\n')
      shutil.copyfile(HERE / "build.sbt", directory / "build.sbt")
      (directory / ".jvmopts").write_text(
        "-Xmx3g\n-XX:ActiveProcessorCount=4\n-Dsbt.supershell=false\n-Dsbt.color=false\n")
      compile_command = ["sbt", "--client", ";hrfJVM/compile;hrfJS/compile"]
      test_command = ["sbt", "--client", ";hrfJVM/test;hrfJS/test"]
      shutdown = ["sbt", "--client", "shutdown"]
      cold_command = ["sbt", "-batch", ";hrfJVM/compile;hrfJS/compile"]
    edit = directory / EDIT_PATH
    original = edit.read_text()
    if original.count(EDIT_TEXT) != 1:
      raise RuntimeError("The declared incremental edit no longer matches the source")
    try:
      run(name, directory, test_command, "bootstrap-tests")
      counts = {}
      for target in ["jvm", "js"]:
        reports = ([directory / "out" / ("hrfJVM" if target == "jvm" else "hrfJS") /
                    "test/testForked.dest/test-report.xml"] if is_mill else
                   list((directory / f"hrf/{target}/target/test-reports").glob("*.xml")))
        if not reports:
          raise RuntimeError(f"Missing {name} {target} test reports")
        trees = [ET.parse(p).getroot() for p in reports]
        counts[target] = sum(len(t.findall(".//testcase")) for t in trees)
        if any(t.findall(".//failure") or t.findall(".//error") for t in trees):
          raise RuntimeError(f"Failing {name} {target} test reports")
      if not all(counts.values()) or (result["test_counts"] and
          counts != next(iter(result["test_counts"].values()))):
        raise RuntimeError(f"Non-matching test counts for {name}: {counts}")
      result["test_counts"][name] = counts
      save()
      run(name, directory, compile_command, "warmup")
      for i in range(args.repetitions):
        run(name, directory, compile_command, f"warm-noop-{i}", True)
      for i in range(args.repetitions):
        edit.write_text(original.replace(EDIT_TEXT, EDIT_TEXT + f" [build-spike-{i}]"))
        run(name, directory, compile_command, f"incremental-edit-{i}", True)
      edit.write_text(original)
      run(name, directory, compile_command, "restore-source")
      run(name, directory, test_command, "test-warmup")
      for i in range(args.repetitions):
        run(name, directory, test_command, f"warm-tests-{i}", True)
      for i in range(args.repetitions):
        clean_command = (prefix + ["clean", "{hrfJVM,hrfJS}.compile"] if is_mill else
                         ["sbt", "--client", ";hrfJVM/clean;hrfJS/clean"])
        run(name, directory, clean_command, f"clean-{i}")
        run(name, directory, compile_command, f"clean-compile-{i}", True)
      run(name, directory, shutdown, "shutdown")
      for i in range(args.repetitions):
        run(name, directory, cold_command, f"cold-noop-{i}", True)
    finally:
      edit.write_text(original)
      if is_mill or (directory / "project/target/active.json").exists():
        subprocess.run(shutdown, cwd=directory, env=env, stdin=subprocess.DEVNULL,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=60)
  result["medians"] = {
    name: {workload: statistics.median(r["seconds"] for r in rows
                                     if r["tool"] == name and r["workload"] == workload)
           for workload in sorted({r["workload"] for r in rows})}
    for name in sorted({r["tool"] for r in rows})
  }
  save()
  print(json.dumps(result["medians"], indent=2))


if __name__ == "__main__":
  main()
