# PHRF-CMP S6 reconciliation: GLMsingle bridge

Mote: `bd-01M3VSW06P4G26MA7DEC9BZ4WH`. Date: 2026-10-10. Worktree branch
`work/phrf-cmp-reconcile`, base origin/main `8e85ef61`. Host: macOS 15.1.1
arm64 (Apple M2 Pro), Temurin JDK from the sbt-warm base, Python 3.12.13.

This record reconciles the bounded S6 acceptance (runner design v2.1,
section 6, S6 row) with the exact source and runtime, and with complete local
results from today. **Result: S6 stays open.** One delivered regression pin
fails deterministically on this host (see "Blocking finding"). Every other S6
obligation is evidenced.

## Exact source and runtime

| Item | Value at `8e85ef61` |
| --- | --- |
| `tools/phrf-comparison/glmsingle/from_generator.py` | `c5a8c0dcdbf9320978b0e794f8b4ff04524c6c0ef9e3731eb9fdcf9506eb89ac` (frozen; equals `GlmSinglePins.FromGeneratorSha256`) |
| `tools/phrf-comparison/glmsingle/run_glmsingle.py` | `499e81d9e632e10dd87c97d9ad300c1361c41c8e103dbd5e638183996961916f` (v0b-frozen) |
| `GlmSingleBridge.scala` | `a83c872b78d38a94ef92e5ff157a22935812a7240ab613e6ed76b917203af89c`, last changed by `0ec1a0fc` (Linux custody/lifecycle repair), matching `docs/verification/main-landing-20261006/linux-bridge/receipt.json` |
| Upstream | GLMsingle `1ab54a65edd3ea41a6133d4b4ecb78a9c7296684` (equals `GlmSinglePins.GlmSingleCommit`) |
| Runtime | Recreated from the hash lock (see `v0b-glmsingle-gate.md`). The freeze is identical to `environment.freeze.txt` and to the 2026-10-05 consolidation freeze; copy in `runtime/glmsingle-freeze-20261010.txt`. Tests were pointed at it through `PHRF_GLMSINGLE_PYTHON`. No repository `.venv` link was created, and the stale shared path no longer exists. |
| Test fixture | `modules/phrf-comparison/jvm/src/test/resources/fixtures/T-TX-fast__d0000.npz` `3b5cb36b...cd0f`, unchanged since recovery |

## Acceptance mapping (S6 row obligations)

| Obligation | Evidence | Status |
| --- | --- | --- |
| Converter receipt: known amplitude recovers E-trial | `converter/test_from_generator.py`: **13 passed** today against real pinned GLMsingle (`logs/s6-converter-pytest.log.gz`). The GLMsingle lock has no pytest, so this ran in a second venv built from the same hash lock (identical freeze) under uv Python 3.12.10, with only `pytest==7.4.4` added. This includes the pre-registered library-HRF bridge check (declared tolerances) and the T-TX-slow gate. The T-TX-slow gate was chosen post hoc, and the receipt discloses that. | evidenced |
| Baseline +100 on scored and pool voxels; v0b units | Same suite: `test_plus100_exact_scored_and_pool`, `test_unit_conversion_roundtrip_direct_float64`, `test_roundtrip_against_float64_recomputation` | evidenced |
| Reorder test (E-trial in input order) | `GlmSingleBridgeSuite`: pure permutation and end-to-end stub permutation tests pass | evidenced |
| Failures: two trials in a volume, kill, timeout | `run_glmsingle.py:80` raises on a shared volume. The bridge maps the child failure (`ChildFailed(3, ...)`) in the forced-failure test. Timeout, kill, survivor and watchdog tests pass. | evidenced |
| `cpu_s` reaches the guard | `GlmSingleAttempt.guardCpuSeconds` (rusage) is wired into `ArmResult` in `exec/PilotArms.scala:229-247`. The CPU-on-NonFatal and F3 burner tests pass. | evidenced (the guard itself is S7) |
| STORED output read by `Npz` | Strict STORED/f8 checks in the converter suite. The S1 closeout parses the frozen adapter golden on JVM and JS. | evidenced |
| E-trial equals the converter output bit for bit; output bytes equal the receipt pin | `GlmSingleBridgeSuite` "real GLMsingle on T-TX-fast ..." and "F3/F4 ..." | **FAIL today** (below) |

## Module gates run today

| Gate | Result |
| --- | --- |
| `phrfComparisonJVM/test` with `PHRF_REQUIRE_INTEROP=1` and recreated GLMsingle/custody/generator interpreters | **Total 476: 473 passed, 2 failed, 1 skipped** (opt-in `RhoBiasHeavySuite`), 779 s (`logs/s6-phrfcomparison-jvm.log.gz`). The 2 failures are both in `GlmSingleBridgeSuite` (33/35 passed). Custody interop ran mandatorily: InteropSuite 5, SealSuite 11 and SealedStoreSuite 12 passed. |
| `phrfComparisonJS/test` | **243 passed**, 0 failed (`logs/s0-fit-and-phrfcomparison-js.log.gz`; the bridge is JVM-only) |
| Watchdog | parent-death test: child group gone **0.024 s** after the parent's SIGKILL (bound 2 s) |
| Leak scan | 17 modified files, 1.86 MB scanned, hits = 1 (the planted control only) |
| Mount | `hfs, local, nodev, nosuid, nobrowse`, mode `rwx------`; `mdutil -s` still reports `unknown indexing state` (unprivileged; residual F8(3)) |

## Blocking finding: output bytes differ from the pinned fixture hash

Both failing tests assert that the frozen converter's output for the
committed fixture has SHA-256
`575a74822329ea91f32acc00d2e5ca979d8424bd5bdf2c34a1ee532384aa72e8`. Today's
output is `398d45a249590ef9d41eca2e659eb2fcc8c3a5d9aca8d3eb749b9f826d98e27f`.

- The result is deterministic. It is the same through the bridge, in a
  direct CLI run, with `PYTHONHASHSEED` 0 or 12345, with or without the
  thread pins, and under Python 3.12.13 (Homebrew) and 3.12.10 (uv), with an
  identical lock freeze.
- The source, lock freeze, upstream commit and fixture bytes are all
  identical to the 2026-10-05 consolidation run. That run passed both tests
  (`docs/verification/consolidation-20261005/final-consumer-gates-v3.log.gz`).
- The 2026-10-06 main-landing run **skipped** both tests (no interpreter;
  `main-landing-20261006/linux-bridge/scalafim-bridge-comparison-jvm-final.log.gz`).
  So between 10-05 and today the pin went unexercised.
- The pilot-size probe (below) reproduces the S6 receipt's realized pool sizes
  exactly (1953, 2096). Its output hashes (`6ed84b25...`, `245882d4...`)
  differ from the receipt's (`00ee77cd...`, `7e0e21b2...`). This is
  consistent with a low-order numerical difference rather than a logic
  change.
- numpy and scipy here link the system Accelerate framework. No macOS or
  Accelerate update since 10-05 appears in the install history. The root
  cause is **not established**.

Neither the pin nor any tolerance was changed. Before S6 can close, someone
must explain the difference and re-qualify. Options: obtain the 10-05 output
bytes, or a host where the pin reproduces, and compare the arrays; or adopt a
reviewed host-qualified pin. Either is outside this reconciliation.

## Bridge CPU re-measured (S10 workload evidence)

A temporary, uncommitted probe suite (deleted after the run; the worktree was
clean afterwards) ran the real bridge on the S6 receipt's pilot-size harness
datasets. They were regenerated today byte-identically: `T-TX-fast__d0000`
`b2374755...0082` and `T-TX-jit__d0000` `43eebc35...925e`, 40 scored /
4000 pool, harness root `7a3c91d50b44e2f1`. Each ran twice, sequentially,
with `expectNPool=4000` and `expectNVoxels=40` (`logs/s6-cpu-probe.log.gz`).

| Dataset | rep | guard (rusage) s | polled s | sidecar `cpu_s` | wall s | realized pool | survivors |
| --- | --- | --- | --- | --- | --- | --- | --- |
| T-TX-fast | 1 | 26.79 | 13.47 | 11.05 | 35.38 | 1953 | 0 |
| T-TX-fast | 2 | 26.84 | 13.51 | 11.19 | 34.29 | 1953 | 0 |
| T-TX-jit | 1 | 27.00 | 13.65 | 11.48 | 31.52 | 2096 | 0 |
| T-TX-jit | 2 | 26.52 | 13.29 | 11.11 | 29.59 | 2096 | 0 |

Reading: the section 5.2 guard figure is about **26.5-27.0 core-s per
pilot-size GLMsingle dataset**. The timing-endpoint figure (sidecar) is about
**11.1-11.5 s**. The excess of roughly 15 s inside the bridge is consistent
with the S6 receipt's unexplained "~5 s excess" on the tiny fixture, scaled
up. It is still unexplained. The S6 receipt's in-process CLI figure was
9.0-9.3 s. Other agents' sbt servers were running on the host, so these are
loaded-host figures. Use the guard figure for the S10 budget.

## Retained limitations (unchanged, not claimed away)

- The parent-death watchdog does **not** cover a descendant that calls
  `setsid` (forbidden by the runner contract). It also does not cover a
  **SIGKILL aimed at the watchdog alone**. A SIGKILLed JVM's RAM device is
  released only by the next start's `sweepStale`.
- `ram://` and `/dev/shm` pages can reach swap. JVM heap dumps and core files
  are not controlled. Spotlight indexing of the scratch cannot be confirmed
  off without root. The leak scan cannot see data that never reached a file.
- `GlmSingleBridgeSuite` must not run concurrently with another instance.
- Linux: the 10-06 container run executed the lifecycle suite on Linux JDK17
  (35 tests OK). That is **not** Linux production or campaign custody, which
  remains unqualified.
- GLMsingle E-trial correlation in fast TX cells is capped near 0.65-0.8 by
  model limits. This is not a bridge defect.
- No type-D equivalence and no pilot admission are claimed.

## Verdict

Every S6 obligation except the frozen output-bytes pin is evidenced. That pin
fails deterministically today, with the cause unidentified, so **S6 remains
open**.
