"""Generate independent 72 MiB NIfTI content and run the reader in a 64 MiB JVM.

The gzip files are streamed to disk. The direct JVM uses the test classpath
exported by `estimatesIoJVM/Test/compile`; no sbt process shares its heap.
"""

from pathlib import Path
import gzip
import hashlib
import json
import struct
import subprocess
import sys
import time


if len(sys.argv) != 2:
    raise SystemExit("usage: run_gzip_budget_probe.py NEW_OUTPUT_DIRECTORY")
output = Path(sys.argv[1]).resolve()
if output.exists():
    raise SystemExit(f"refusing to reuse existing output: {output}")
output.mkdir(parents=True)
worktree = Path(__file__).resolve().parents[3]
classpath_file = worktree / "modules/estimates-io/jvm/target/streams/test/fullClasspath/_global/streams/export"
classpath = classpath_file.read_text().strip()
probe_class = worktree / "modules/estimates-io/jvm/target/scala-3.7.4/test-classes/scalafim/estimates/io/EstimateGzipBudgetProbe$.class"
if not probe_class.is_file():
    raise SystemExit("compile estimatesIoJVM/Test/compile before running the direct probe")


def header(dtype: int, bits: int) -> bytes:
    result = bytearray(352)
    struct.pack_into("<i", result, 0, 348)
    struct.pack_into("<8h", result, 40, 3, 256, 256, 128, 1, 1, 1, 1)
    struct.pack_into("<h", result, 70, dtype)
    struct.pack_into("<h", result, 72, bits)
    struct.pack_into("<8f", result, 76, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
    struct.pack_into("<f", result, 108, 352.0)
    struct.pack_into("<f", result, 112, 1.0)
    result[123] = 2
    struct.pack_into("<h", result, 254, 1)
    struct.pack_into("<4f", result, 280, 1.0, 0.0, 0.0, 0.0)
    struct.pack_into("<4f", result, 296, 0.0, 1.0, 0.0, 0.0)
    struct.pack_into("<4f", result, 312, 0.0, 0.0, 1.0, 0.0)
    result[344:348] = b"n+1\0"
    return bytes(result)


count = 256 * 256 * 128
chunk = 1024 * 1024
for name, dtype, bits, fill in (
    ("values.nii.gz", 64, 64, b"\0"),
    ("validity.nii.gz", 2, 8, b"\1"),
):
    path = output / name
    with path.open("wb") as raw:
        with gzip.GzipFile(fileobj=raw, mode="wb", filename="", mtime=0) as encoded:
            encoded.write(header(dtype, bits))
            if dtype == 2:
                encoded.write(b"\0")
                remaining = count - 1
            else:
                remaining = count * 8
            while remaining:
                length = min(chunk, remaining)
                encoded.write(fill * length)
                remaining -= length


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for block in iter(lambda: source.read(chunk), b""):
            digest.update(block)
    return digest.hexdigest()


command = ["java", "-Xmx64m", "-XX:ActiveProcessorCount=2", "-cp", classpath,
           "scalafim.estimates.io.EstimateGzipBudgetProbe", str(output)]
log = output / "direct-jvm.log"
started = time.monotonic()
with log.open("w") as stream:
    completed = subprocess.run(command, stdout=stream, stderr=subprocess.STDOUT, timeout=180)
elapsed = time.monotonic() - started
log_text = log.read_text()
receipt = {
    "exit_code": completed.returncode,
    "passed": completed.returncode == 0 and "PROBE_PASS" in log_text,
    "elapsed_seconds": elapsed,
    "jvm_max_heap_bytes": 64 * 1024 * 1024,
    "uncompressed_staging_budget_bytes": 2 * 352 + count * 9,
    "compressed_files": {name: {"bytes": (output / name).stat().st_size, "sha256": sha256(output / name)}
                         for name in ("values.nii.gz", "validity.nii.gz")},
    "source_sha256": sha256(worktree / "modules/estimates-io/jvm/src/test/scala/scalafim/estimates/io/EstimateGzipBudgetProbe.scala"),
    "compiled_class_sha256": sha256(probe_class),
    "classpath_string_sha256": hashlib.sha256(classpath.encode()).hexdigest(),
    "result_line": next((line for line in log_text.splitlines() if line.startswith("PROBE_PASS")), ""),
    "raw_log": str(log),
}
(output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
print(json.dumps(receipt, indent=2))
if not receipt["passed"]:
    print(log_text[-4000:])
    raise SystemExit(1)
