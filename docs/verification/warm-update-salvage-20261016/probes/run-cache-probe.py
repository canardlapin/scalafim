#!/usr/bin/env python3
"""Compile the two original cache implementations against a minimal error ADT.

Requires JDK and locally cached Scala 3.7.4 jars; does not modify source checkouts
or download dependencies. Synthetic bytes exercise staging, not NIfTI decoding.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile

MAIN = "ac2ec8b706fce08596a943d79f71a140aad05948"
RETAINED = "7c3ff0a0edc92be0e2fe35c431d5b9e458c92ab4"
SOURCE = "modules/dataset/jvm/src/main/scala/scalafim/dataset/io/"
JARS = [
    "org/scala-lang/scala3-compiler_3/3.7.4/scala3-compiler_3-3.7.4.jar",
    "org/scala-lang/scala3-interfaces/3.7.4/scala3-interfaces-3.7.4.jar",
    "org/scala-lang/scala3-library_3/3.7.4/scala3-library_3-3.7.4.jar",
    "org/scala-lang/tasty-core_3/3.7.4/tasty-core_3-3.7.4.jar",
    "org/scala-lang/scala-library/2.13.16/scala-library-2.13.16.jar",
    "org/scala-lang/modules/scala-asm/9.8.0-scala-1/scala-asm-9.8.0-scala-1.jar",
    "org/scala-sbt/compiler-interface/1.10.7/compiler-interface-1.10.7.jar",
    "org/scala-sbt/util-interface/1.10.7/util-interface-1.10.7.jar",
    "org/jline/jline-reader/3.29.0/jline-reader-3.29.0.jar",
    "org/jline/jline-terminal/3.29.0/jline-terminal-3.29.0.jar",
    "org/jline/jline-terminal-jni/3.29.0/jline-terminal-jni-3.29.0.jar",
]


def main():
    here = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", type=Path, default=here.parents[3])
    parser.add_argument("--maven-cache", type=Path, default=Path.home() /
                        "Library/Caches/Coursier/v1/https/repo.maven.apache.org/maven2")
    parser.add_argument("--output", type=Path, default=here)
    args = parser.parse_args()
    jars = [args.maven_cache / name for name in JARS]
    missing = [str(path) for path in jars if not path.is_file()]
    if missing:
        parser.error("Missing cached compiler jars: " + ", ".join(missing))
    cp = os.pathsep.join(map(str, jars))
    args.output.mkdir(parents=True, exist_ok=True)
    results = {}
    with tempfile.TemporaryDirectory(prefix="scalafim-cache-probe-build-") as temp:
        root = Path(temp)
        for label, revision, name in [
            ("current", MAIN, "NiftiResponseBlockSource.scala"),
            ("retained", RETAINED, "NiftiStagingCache.scala"),
        ]:
            source = subprocess.check_output(
                ["git", "-C", str(args.repo), "show", f"{revision}:{SOURCE}{name}"], text=True)
            if label == "current":
                body = source[source.index("final class NiftiStagingCache"):
                              source.index("\nfinal class NiftiResponseBlockSource")]
                imports = "\n".join(line for line in source.splitlines()
                                    if line.startswith(("import java.", "import scala.")))
                source = "package scalafim.dataset.io\nimport scalafim.dataset.DatasetError\n" + imports + "\n" + body
            work = root / label
            work.mkdir()
            original = work / "NiftiStagingCache.scala"
            original.write_text(source)
            classes = work / "classes"
            classes.mkdir()
            result = {"revision": revision, "path": SOURCE + name}
            compile_run = subprocess.run([
                "java", "-cp", cp, "dotty.tools.dotc.Main", "-classpath", cp,
                "-d", str(classes), str(here / "DatasetError.scala"),
                str(original), str(here / "CacheProbe.scala"),
            ], text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            (args.output / f"{label}-compile.log").write_text(compile_run.stdout)
            result["compileExit"] = compile_run.returncode
            if compile_run.returncode == 0:
                run = subprocess.run(["java", "-cp", str(classes) + os.pathsep + cp,
                                      "salvage.CacheProbe"], text=True,
                                     stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                (args.output / f"{label}-run.log").write_text(run.stdout)
                result["runExit"] = run.returncode
                if run.returncode == 0:
                    result["observed"] = json.loads(run.stdout)
            results[label] = result
    (args.output / "cache-results.json").write_text(json.dumps(results, indent=2) + "\n")
    print(json.dumps(results, indent=2))
    assert all(result.get("runExit") == 0 for result in results.values())
    assert results["current"]["observed"]["returnedFreshContent"] is False
    assert results["current"]["observed"]["repairedCorruptedCache"] is False
    assert results["retained"]["observed"]["returnedFreshContent"] is True
    assert results["retained"]["observed"]["repairedCorruptedCache"] is True


if __name__ == "__main__":
    main()
