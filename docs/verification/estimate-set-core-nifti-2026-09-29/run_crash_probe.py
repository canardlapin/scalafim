"""Run separate JVMs and retain exact crash-boundary/readback receipts.

Run estimatesIoJVM/test first so sbt exports the compiled Test/fullClasspath.
Each stage starts a fresh JVM directly; a crash exits only its own process.
"""

from pathlib import Path
import hashlib
import json
import subprocess
import sys


if len(sys.argv) != 2:
    raise SystemExit("usage: run_crash_probe.py NEW_OUTPUT_DIRECTORY")

output = Path(sys.argv[1]).resolve()
if output.exists():
    raise SystemExit(f"refusing to reuse existing output: {output}")
output.mkdir(parents=True)
execution = Path("/private/tmp/scalafim-execution-20260929")
worktree = execution / "io"
provider = execution / "image4s-provider"
classpath_file = worktree / "modules/estimates-io/jvm/target/streams/test/fullClasspath/_global/streams/export"
classpath = classpath_file.read_text().strip()
if str(provider) not in classpath:
    raise SystemExit("compiled classpath does not contain the local image4s provider")
classpath_sha256 = hashlib.sha256(classpath.encode()).hexdigest()
receipt = []


def run(stage: str, action: str, root: Path, expected: int, read_expectation="") -> None:
    log_name = f"io-crash-{output.name}-{stage}-{action}.log"
    command = ["java", "-cp", classpath, "scalafim.estimates.io.EstimateCrashProbe",
               action, str(root)]
    if read_expectation:
        command.append(read_expectation)
    print(f"START {stage} {action}", flush=True)
    log = execution / "logs" / log_name
    with log.open("w") as stream:
        completed = subprocess.run(command, text=True, stdout=stream, stderr=subprocess.STDOUT)
    log_text = log.read_text()
    passed = completed.returncode == expected
    if action == "seed":
        passed = passed and "SEED_PASS" in log_text
    if action == "read":
        passed = passed and "READ_PASS" in log_text
    entry = {
        "stage": stage,
        "action": action,
        "expected_exit": expected,
        "actual_exit": completed.returncode,
        "passed": passed,
        "classpath_sha256": classpath_sha256,
        "raw_log": str(log),
    }
    receipt.append(entry)
    (output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print(f"END {stage} {action} exit={completed.returncode} passed={passed}", flush=True)
    if not passed:
        print(log_text[-3000:], flush=True)
        raise SystemExit(1)


for stage in ("crash-before-seal", "crash-after-seal", "crash-after-collection", "crash-after-cas"):
    root = output / stage
    run(stage, "seed", root, 0)
    run(stage, stage, root, 87)
    run(stage, "read", root, 0, "new" if stage == "crash-after-cas" else "base")
