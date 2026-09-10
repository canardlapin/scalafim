# ProfileHrf condition milestone (PHRF-21)

Date: 2026-09-10. Cohort and host: [profile-hrf-cohorts.md](profile-hrf-cohorts.md).
Evidence: `ConditionMilestoneSuite` in `first-level-laws` (both platforms),
`ConditionProfileBenchmark` (JMH), and the runnable example
`workflowExamplesJVM/runMain scalafim.examples.workflows.runProfileHrfConditionWorkflow`.

## Accuracy against the dense exact-kernel oracle (JVM)

Gaussian, K = 57 (m = 19 at 1e-3), 15x15 bank scanned hierarchically, 2 Newton steps:

| SNR | Admitted | p95 peak latency | p95 FWHM | p95 rel. amplitude | Gates |
| --- | ---: | ---: | ---: | ---: | --- |
| 1.0 | 100 % | 0.0004 s | 0.0012 s | 2.2e-4 | met |
| 0.5 | 100 % | 0.0006 s | 0.0017 s | 4.4e-4 | met |
| 0.25 | 87.5 % | 0.0020 s | 0.0040 s | 1.2e-3 | not gated; reported |

LWU, K = 69 (m = 23 at 1e-3), 13x9x7 bank, 3 Newton steps (recorded per-family budget):

| SNR | Admitted | p95 peak latency | p95 FWHM | p95 rel. amplitude | Gates |
| --- | ---: | ---: | ---: | ---: | --- |
| 1.0 | 95 % | 0.000 s | 0.010 s | 3.2e-4 | met |
| 0.5 | 83 % | 0.010 s | 0.010 s | 4.2e-4 | admission unmet (rho-weak voxels); accuracy met |

Summaries are compared on a 0.01 s grid, so LWU latency and width differences
are quantised to one cell. The oracle-deficit diagnostic (exact-kernel
residual at the decoder's answer versus the oracle's) shows the decoder beats
the oracle in 177/200 (Gaussian, SNR 1.0) and 93/95 (LWU, SNR 1.0) admitted
voxels: the residual differences are mostly oracle error, and the gate values
above are conservative.

Work per voxel: Gaussian 72 node scores, 2 jets, 1 exact evaluation; LWU 163
node scores, 3 jets, 1 exact evaluation. All within the recorded budgets.

## Throughput (compact route, single thread)

JVM, reference host, `-Dscalafim.phrf.voxels=100000`, five measured runs after
one warm-up (compact route: projection `[U; qF]' W y` plus post-solve per voxel):

| Item | Value |
| --- | --- |
| V = 100,000, T = 600, K = 57, m = 19 | median 9.0 s, p95 9.5 s |
| of which projection (last run) | 1.5 s |
| Whitening through `ar` (once, whole cohort) | 0.25 s |
| Accepted | 99.2 % |
| Per voxel | 71.9 node scores, 2.00 jets, 1.00 exact evaluations, 9 fallbacks in total |
| V = 10,000 | median 0.91 s (extrapolates to 9.1 s) |
| C0 absolute goal (<= 30 s single-thread) | met, 9.0 s |

Memory: the suite holds the whole 100,000-voxel cohort in memory twice (raw
and whitened, 480 MB each), so its heap delta of about 925 MiB is input
storage, not engine memory; the engine state (basis, node bank, worker
workspace) is a few MiB. The 256 MiB engine budget is checked by the
block-streaming path, whose per-block memory is bounded by construction
(`BlockExecutor`), and process RSS is reported by the JMH benchmark.

JMH (`fitBenchJVM/Jmh/run`, short pass: 1 fork, 2 warm-up and 3 measured
iterations of 1 s, 256-voxel blocks, tolerance 1e-3; error bars are wide at
this length and the numbers are indicative):

| Phase per 256-voxel block | ms/op | per voxel |
| --- | ---: | ---: |
| Projection `[U; qF]' W y` | 3.9 | 0.015 ms |
| Exhaustive scan of all 225 nodes (the hierarchical scan does 72) | 9.1 | 0.036 ms |
| One continuous full jet (includes projection) | 14.3 | 0.056 ms |
| Complete fit: projection, hierarchical scan, decode, readout | 19.3 | 0.075 ms |

Reproduce with:

```sh
sbt "fitBenchJVM/Jmh/run -f 1 -wi 3 -i 5 -p voxels=256,2048 -p tolerance=1e-3,1e-4 .*ConditionProfileBenchmark.*"
```

Scala.js on Node, same suite, V = 10,000 (accuracy results bit-identical to
the JVM):

| Item | Value |
| --- | --- |
| V = 10,000, T = 600, K = 57 | median 3.2 s, p95 3.3 s (JVM: 0.91 s) |
| of which projection (last run) | 0.62 s |
| Extrapolated to V = 100,000 | 32 s single-thread (3.5x the JVM) |
| Per voxel | 71.9 node scores, 2.00 jets, 1.00 exact evaluations |

JS reports its own throughput; the absolute C0 goal is a JVM figure.

## Verdict

The condition-only milestone is qualified for the Gaussian family over its
declared domain at SNR >= 0.5 and for LWU at SNR 1.0, with the LWU admission
gate at SNR 0.5 recorded as unmet. Both families, the compact and Gram
routes, the executor, sinks, pooling and typed outputs are verified on JVM
and JS; the absolute C0 goal is met at 9.0 s single-thread on the reference
host. This is condition-only evidence: trial calibration and performance
remain open for PHRF-14/15/16.

## Unmet and open

- LWU admission at SNR 0.5 (83 %) is below the 95 % gate. The unadmitted
  voxels fail the conditional-sd rule on the undershoot coordinate; this is
  the provisional admission rule (D3) working on a weakly identified
  parameter and is left for calibration in PHRF-14, not relaxed here.
- Absolute memory: the receipt's engine estimate covers the JVM heap delta
  around the pass; process RSS is reported by JMH, not by the suite.
- Parallel execution and the retention-route throughput are measured by the
  JMH benchmark and the executor tests, not yet by this suite.
