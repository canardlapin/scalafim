#!/usr/bin/env python3
"""One bounded S0 baseline job. Invalid host evidence is retained, never admitted."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import signal
import subprocess
import sys
import time
from datetime import datetime, timezone


JAVA_NODE = ("java", "node")
CPU_THRESHOLD_PERCENT = 10.0


def now():
    return datetime.now(timezone.utc).isoformat()


def command_output(command):
    return subprocess.check_output(command, text=True, stderr=subprocess.STDOUT).strip()


def parse_cpu_time(value):
    """Parse the cumulative BSD `ps time` field into seconds."""
    days = 0
    clock = value
    if "-" in clock:
        day, clock = clock.split("-", 1)
        days = int(day)
    fields = clock.split(":")
    if len(fields) == 2:
        minutes, seconds = int(fields[0]), float(fields[1])
        hours = 0
    elif len(fields) == 3:
        hours, minutes, seconds = int(fields[0]), int(fields[1]), float(fields[2])
    else:
        raise ValueError("unrecognized ps CPU time: " + value)
    return float(((days * 24 + hours) * 60 + minutes) * 60) + seconds


def process_snapshot():
    """Read identity and cumulative CPU time for macOS Java/Node processes.

    `pcpu` is deliberately not used: it is process-lifetime/decayed state rather
    than CPU consumed in this sampling interval.
    """
    command = ["ps", "-Ao", "pid=,ppid=,pgid=,lstart=,time=,comm="]
    raw = command_output(command)
    processes = []
    # `lstart` has five whitespace-separated fields. Parsing the fixed prefix
    # and then splitting the suffix preserves a command path that may contain
    # spaces while retaining a stable PID-start identity.
    pattern = re.compile(
        r"^\s*(\d+)\s+(\d+)\s+(\d+)\s+"
        r"([A-Z][a-z]{2}\s+[A-Z][a-z]{2}\s+\d+\s+\d\d:\d\d:\d\d\s+\d{4})\s+"
        r"(\d+(?:-\d+)?(?::\d+){1,2}(?:\.\d+)?)\s+(.+)$"
    )
    for line in raw.splitlines():
        match = pattern.match(line)
        if match is None:
            raise RuntimeError("unparseable ps process row: " + line)
        pid, parent, group, started, cpu_time, executable = match.groups()
        if Path(executable).name in JAVA_NODE:
            processes.append({
                "pid": int(pid),
                "parent_pid": int(parent),
                "process_group": int(group),
                "started": started,
                "cpu_time": cpu_time,
                "cpu_seconds": parse_cpu_time(cpu_time),
                "executable": executable,
            })
    return {"command": command, "raw": raw, "java_node": processes}


def host_snapshot(previous=None, owned_group=None):
    process_data = process_snapshot()
    captured_monotonic = time.monotonic()
    previous_by_pid = {}
    if previous is not None:
        previous_by_pid = {entry["pid"]: entry for entry in previous["processes"]["java_node"]}
    interval_seconds = None if previous is None else captured_monotonic - previous["captured_monotonic"]
    competing = []
    for entry in process_data["java_node"]:
        if entry["process_group"] == owned_group:
            continue
        before = previous_by_pid.get(entry["pid"])
        same_process = before is not None and before["started"] == entry["started"]
        observed = dict(entry)
        if not same_process or interval_seconds is None or interval_seconds <= 0.0:
            # A new non-owned JVM/Node can have consumed arbitrary CPU between
            # samples. Retain it and invalidate rather than guessing zero.
            observed["newly_observed"] = True
            observed["interval_seconds"] = interval_seconds
            observed["delta_cpu_seconds"] = None
            observed["cpu_percent_interval"] = None
            competing.append(observed)
        else:
            delta = max(0.0, entry["cpu_seconds"] - before["cpu_seconds"])
            observed["newly_observed"] = False
            observed["interval_seconds"] = interval_seconds
            observed["delta_cpu_seconds"] = delta
            observed["cpu_percent_interval"] = 100.0 * delta / interval_seconds
            if observed["cpu_percent_interval"] > CPU_THRESHOLD_PERCENT:
                competing.append(observed)
    thermal = command_output(["pmset", "-g", "therm"])
    throttled = any(int(value) < 100 for value in re.findall(r"(?:CPU_Speed_Limit|Scheduler_Limit)\s*=\s*(\d+)", thermal))
    throttled = throttled or any(int(value) > 0 for value in re.findall(r"Thermal_Level\s*=\s*(\d+)", thermal))
    return {
        "utc": now(), "captured_monotonic": captured_monotonic,
        "load": list(os.getloadavg()), "processes": process_data,
        "competing": competing, "thermal": thermal, "throttled": throttled,
        "power": command_output(["pmset", "-g", "batt"]),
    }


def keep_supervisor_anchor(signum, frame):
    # Deliberately a Python handler rather than SIG_IGN: exec resets custom
    # handlers in the benchmark child, while this supervisor keeps the PGID
    # live through the parent's TERM grace period.
    return None


def stop_owned_anchor(process, grace_seconds=10):
    """Terminate an owned group while its live supervisor proves ownership."""
    if process is None or process.poll() is not None:
        return False
    if os.getpgid(process.pid) != process.pid:
        raise RuntimeError("supervisor no longer owns its expected process group")
    os.killpg(process.pid, signal.SIGTERM)
    try:
        process.wait(timeout=grace_seconds)
    except subprocess.TimeoutExpired:
        # The no-op supervisor handler keeps this group anchored during the
        # grace period, so a KILL here cannot target a reused process group.
        if process.poll() is not None or os.getpgid(process.pid) != process.pid:
            raise RuntimeError("supervisor lost before owned-group SIGKILL")
        os.killpg(process.pid, signal.SIGKILL)
        process.wait()
        return True
    raise RuntimeError("supervisor exited during TERM grace; owned group was not killed")


def proved_owned_descendants(processes, owned_group, observed_owned):
    """Return live children whose PID-start identity was observed in the live PGID."""
    live = {(entry["pid"], entry["started"]): entry for entry in processes["java_node"]}
    return [entry for identity, entry in observed_owned.items()
            if identity in live and entry["process_group"] == owned_group]


def stop_proved_descendants(processes, owned_group, observed_owned):
    """Best-effort cleanup only after anchor loss; never signal a reused group."""
    stopped = []
    for entry in proved_owned_descendants(processes, owned_group, observed_owned):
        try:
            os.kill(entry["pid"], signal.SIGTERM)
            stopped.append({"pid": entry["pid"], "started": entry["started"]})
        except ProcessLookupError:
            pass
    return stopped


def write_exit_receipt(path, payload):
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(json.dumps(payload) + "\n")
    os.replace(temporary, path)


def supervise(exit_file, command):
    if not command:
        raise ValueError("supervisor requires a command")
    # Installed before fork so an interrupted parent still has a live anchor;
    # Python resets this custom handler on exec in the JMH child.
    signal.signal(signal.SIGTERM, keep_supervisor_anchor)
    child = subprocess.Popen(command)
    exit_code = child.wait()
    write_exit_receipt(exit_file, {"utc": now(), "jmh_exit_code": exit_code, "child_pid": child.pid})
    # This process remains the session/process-group anchor until the parent
    # has inspected the JMH exit receipt and terminates this owned group.
    while True:
        signal.pause()


def read_exit_receipt(path):
    try:
        payload = json.loads(path.read_text())
        if not isinstance(payload.get("jmh_exit_code"), int):
            raise ValueError("missing integer jmh_exit_code")
        return payload
    except (OSError, ValueError, json.JSONDecodeError) as error:
        raise RuntimeError("invalid supervisor exit receipt: " + str(error))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path)
    parser.add_argument("--run-name")
    parser.add_argument("--preflight-only", action="store_true")
    parser.add_argument("--max-load", type=float, choices=(2.0, 5.0), default=2.0)
    parser.add_argument("--supervise", action="store_true")
    parser.add_argument("--supervisor-exit-file", type=Path)
    parser.add_argument("supervised_command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if args.supervise:
        command = args.supervised_command[1:] if args.supervised_command[:1] == ["--"] else args.supervised_command
        if args.supervisor_exit_file is None:
            parser.error("--supervisor-exit-file is required with --supervise")
        supervise(args.supervisor_exit_file, command)
        return 0
    if args.root is None or args.run_name is None:
        parser.error("--root and --run-name are required")
    if args.max_load == 5.0 and platform.node().split(".")[0].lower() != "buc-gw01":
        parser.error("the prospectively approved load-5 exception applies only to buc-gw01")

    root = args.root.resolve()
    if Path(args.run_name).name != args.run_name or args.run_name in (".", ".."):
        parser.error("run-name must be a single directory name")
    output = root / "runs" / args.run_name
    output.mkdir(parents=True, exist_ok=False)
    receipt = {"schema": "scalafim.hrf-s0.v1", "started_utc": now(), "admitted": False,
               "stage": "S0 pilot only; not S4 non-inferiority", "reasons": [],
               "host": platform.node(), "max_start_end_load": args.max_load,
               "invocation": sys.argv}
    process = None
    observed_owned = {}
    try:
        # Establish one measured CPU interval before admitting a start snapshot.
        initial = host_snapshot()
        time.sleep(1.0)
        start = host_snapshot(initial)
        receipt["host_preflight"] = initial
        receipt["host_start"] = start
        if start["load"][0] > args.max_load:
            receipt["reasons"].append("start_load_above_limit")
        if start["competing"]:
            receipt["reasons"].append("competing_java_or_node")
        if start["throttled"]:
            receipt["reasons"].append("thermal_throttling_at_start")
        if args.preflight_only or receipt["reasons"]:
            receipt["status"] = "preflight_only" if not receipt["reasons"] else "host_refused"
            return 2
        manifest_path = root / "package" / "manifest.json"
        manifest = json.loads(manifest_path.read_text())
        for relative, expected in manifest["files_sha256"].items():
            actual = hashlib.sha256((root / "package" / relative).read_bytes()).hexdigest()
            if actual != expected:
                raise RuntimeError("package hash mismatch: " + relative)
        receipt["manifest_sha256"] = hashlib.sha256(manifest_path.read_bytes()).hexdigest()
        java = root / "toolchain/jdk-21.0.12.1+1/Contents/Home/bin/java"
        receipt["java_version"] = command_output([str(java), "-version"])
        classpath = os.pathsep.join(str(root / "package" / p) for p in manifest["classpath"])
        command = [str(java), "-Xms1g", "-Xmx1g", "-XX:ActiveProcessorCount=2", "-cp", classpath,
                   "org.openjdk.jmh.Main",
                   "scalafim.fmri.hrf.(RegressorConvolutionBenchmark|DenseDriveBenchmark|BasisResponseBenchmark).*",
                   "-f", "5", "-wi", "3", "-w", "1s", "-i", "5", "-r", "1s", "-t", "1",
                   "-prof", "gc", "-rf", "json", "-rff", str(output / "jmh.json"),
                   "-jvm", str(java), "-jvmArgs", "-Xms1g -Xmx1g -XX:ActiveProcessorCount=2"]
        receipt["command"] = command
        exit_file = output / "supervisor-exit.json"
        supervisor_command = [sys.executable, str(Path(__file__).resolve()), "--supervise",
                              "--supervisor-exit-file", str(exit_file), "--"] + command
        deadline = time.monotonic() + 7200
        with (output / "jmh.log").open("w") as log, (output / "host.jsonl").open("w") as samples:
            process = subprocess.Popen(supervisor_command, stdout=log, stderr=subprocess.STDOUT,
                                       cwd=root, start_new_session=True)
            receipt["owned_pid_and_process_group"] = process.pid
            last_sample = start
            next_sample = time.monotonic()
            while True:
                if exit_file.is_file():
                    receipt["supervisor_exit"] = read_exit_receipt(exit_file)
                    receipt["exit_code"] = receipt["supervisor_exit"]["jmh_exit_code"]
                    stop_owned_anchor(process)
                    break
                if process.poll() is not None:
                    receipt["reasons"].append("supervisor_died_before_exit_receipt")
                    final_processes = process_snapshot()
                    receipt["proved_descendants_terminated"] = stop_proved_descendants(
                        final_processes, process.pid, observed_owned)
                    break
                sample = host_snapshot(last_sample, process.pid)
                for entry in sample["processes"]["java_node"]:
                    if entry["process_group"] == process.pid and entry["pid"] != process.pid:
                        observed_owned[(entry["pid"], entry["started"])] = entry
                samples.write(json.dumps(sample) + "\n")
                samples.flush()
                if sample["competing"]:
                    receipt["reasons"].append("competing_java_or_node")
                if sample["throttled"]:
                    receipt["reasons"].append("thermal_throttling_during_run")
                if sample["thermal"] != start["thermal"] or sample["power"] != start["power"]:
                    receipt["reasons"].append("thermal_or_power_status_changed")
                if time.monotonic() > deadline:
                    receipt["reasons"].append("two_hour_timeout")
                if receipt["reasons"]:
                    stop_owned_anchor(process)
                    break
                last_sample = sample
                next_sample += 1.0
                time.sleep(max(0.0, next_sample - time.monotonic()))
        if process is not None and process.poll() is None:
            stop_owned_anchor(process)
        end = host_snapshot(last_sample)
        receipt["host_end"] = end
        if end["load"][0] > args.max_load:
            receipt["reasons"].append("end_load_above_limit")
        if end["competing"]:
            receipt["reasons"].append("competing_java_or_node_at_end")
        if end["throttled"]:
            receipt["reasons"].append("thermal_throttling_at_end")
        if end["thermal"] != start["thermal"] or end["power"] != start["power"]:
            receipt["reasons"].append("thermal_or_power_status_changed_at_end")
        if receipt.get("exit_code") != 0:
            receipt["reasons"].append("jmh_failed_or_missing_exit_receipt")
        if not (output / "jmh.json").is_file():
            receipt["reasons"].append("missing_jmh_results")
        receipt["status"] = "outputs_require_review" if not receipt["reasons"] else "invalid_run"
        # Host validity alone never proves complete, finite benchmark output.
        return 0 if not receipt["reasons"] else 2
    except (Exception, KeyboardInterrupt) as error:
        if process is not None and process.poll() is None:
            stop_owned_anchor(process)
        receipt["status"] = "execution_error"
        receipt["reasons"].append(type(error).__name__ + ": " + str(error))
        return 2
    finally:
        receipt["finished_utc"] = now()
        (output / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
        print(json.dumps({"receipt": str(output / "receipt.json"), "status": receipt.get("status"),
                          "reasons": receipt["reasons"]}))


if __name__ == "__main__":
    raise SystemExit(main())
