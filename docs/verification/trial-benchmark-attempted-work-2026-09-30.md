# Trial benchmark attempted-work aggregation (2026-09-30)

Mote: `bd-01M3R33S7SSGCCFRR7241JK82W`, follow-up to `bd-01M3QW9AESPAP2D6Y4B788BEYG`.
Base: integration `2e230d4e`.

## Defect

`TrialBandedWorkSnapshot` gained a tenth field,
`attempted: TrialBandedAttemptedWorkSnapshot`, with the bounded
attempt/failure counters. `TrialBandedBenchmark.zeroWork` and `addWork` still
constructed the snapshot with nine arguments, so `fitBenchJVM` no longer
compiled. `fitBenchJVM` is not part of `scalafimCompileAll`, so the
integrated compile gate did not detect this.

## Change

Only `benchmarks/fit-jvm/src/main/scala/scalafim/fmri/fit/profile/TrialBandedBenchmark.scala`:

- `zeroWork` carries an all-zero `TrialBandedAttemptedWorkSnapshot`.
- `addWork` sums `attempted` field-wise via `addAttempted`, covering all sixteen
  counters. Each worker owns a fresh `TrialBandedWork`, so the per-worker
  snapshots are disjoint. Summing them does not double count. Shared node-bank
  setup work is reported by `TrialBandedSetupReceipt` and is not part of the
  worker snapshots.
- The `trial-banded-checkpoint/v1` line also prints the aggregated attempted
  counters, so they are reported rather than silently summed.

No benchmark was run and no throughput claim is made.

## Evidence

- Command: `python3 ROOT/run-sbt.py <worktree> bench-snapshot-compile-r1.log fitBenchJVM/compile`
  (sbt `-J-Xmx3g -J-XX:ActiveProcessorCount=4`).
- Exit 0, 50.4 s, 0 `[warn]` lines; log SHA-256
  `c83a09b62d63c149704908b702b58056b6936ec8609eaebe6e1dda4f83585990`.
- Source SHA-256 `182ffa27ea3c405edaea0b025857cb17ff51bc10fa8c69c99eb92fb913d9af3f`.
- Not covered: `Jmh/compile` (the generated JMH harness) and any benchmark run.

## Open recommendation

Add `fitBenchJVM/compile` to a qualification gate, so that counter-schema
changes cannot silently break the benchmark again. That is a `build.sbt` alias
change and is outside this ticket's owned paths.
