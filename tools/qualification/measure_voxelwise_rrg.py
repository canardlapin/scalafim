#!/usr/bin/env python3
"""Measure fresh JVM processes on macOS using /usr/bin/time -l.

Supply the resolved `fitJVM / Test / fullClasspath` (one colon-separated line).
The Scala driver performs ten fixed small warm-up fits before the timed fit.
Peak RSS includes process startup, warm-up, input generation, and extraction.
No benchmark result here claims statistical equivalence to unrestricted GLS.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("classpath", type=Path)
    parser.add_argument("output_dir", type=Path)
    parser.add_argument("--java", default="/opt/homebrew/opt/openjdk/bin/java")
    parser.add_argument("--voxels", type=int, required=True)
    parser.add_argument("--replicates", type=int, default=0)
    parser.add_argument("--repeats", type=int, default=3)
    parser.add_argument("--timeout", type=int, default=300)
    args = parser.parse_args()
    if sys.platform != "darwin":
        parser.error("this peak-RSS measurement uses macOS /usr/bin/time -l")
    if args.output_dir.exists():
        parser.error("refusing to overwrite a benchmark directory")
    cp = args.classpath.read_text().strip()
    if not all(Path(p).exists() for p in cp.split(":")):
        parser.error("classpath contains missing entries")
    args.output_dir.mkdir(parents=True)
    records = []
    for repeat in range(args.repeats):
        output = args.output_dir / f"fit-{repeat}.json"
        resource = args.output_dir / f"resources-{repeat}.txt"
        log = args.output_dir / f"process-{repeat}.log"
        command = ["/usr/bin/time", "-l", "-o", str(resource), args.java,
                   "-Xmx3g", "-XX:ActiveProcessorCount=4", "-cp", cp,
                   "scalafim.fmri.fit.VoxelwiseReducedRankQualification", "performance",
                   str(args.voxels), str(args.replicates), str(output)]
        started = time.monotonic()
        timed_out = False
        with log.open("wb") as stream:
            process = subprocess.Popen(command, stdout=stream, stderr=subprocess.STDOUT, start_new_session=True)
            try:
                exit_code = process.wait(timeout=args.timeout)
            except subprocess.TimeoutExpired:
                timed_out = True
                # This group belongs exclusively to the process launched above.
                os.killpg(process.pid, signal.SIGTERM)
                try:
                    exit_code = process.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    exit_code = process.wait()
        text = resource.read_text() if resource.exists() else ""
        rss = re.search(r"(\d+)\s+maximum resident set size", text)
        record = {"repeat": repeat, "exit_code": exit_code, "timed_out": timed_out,
                  "whole_process_wall_seconds": time.monotonic() - started,
                  "peak_process_rss_bytes": int(rss[1]) if rss else None,
                  "process_log_sha256": hashlib.sha256(log.read_bytes()).hexdigest(),
                  "fit": json.loads(output.read_text()) if output.exists() else None}
        records.append(record)
        receipt = {"schema": "voxelwise-rrg-resource-v1", "voxels": args.voxels,
                   "replicates": args.replicates, "requested_repeats": args.repeats,
                   "heap_cap_bytes": 3 * 1024**3, "active_processor_count": 4,
                   "timeout_per_process_seconds": args.timeout,
                   "measurement_scope": "fit timer covers preparation including bootstrap; RSS includes warmup/input/extraction; sum of heap-pool peaks is not simultaneous peak heap",
                   "records": records}
        (args.output_dir / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
        print(json.dumps(record), flush=True)
        # A resource failure establishes a boundary; do not repeat an OOM/timeout.
        if exit_code != 0 or timed_out:
            break


if __name__ == "__main__":
    main()
