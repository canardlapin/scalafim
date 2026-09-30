# Selected precision pooling review — 2026-09-29

## Source and estimator

This work evaluates `FirstLevelFixedEffectsEstimates` and
`FixedEffectsEstimates.estimate` at the selected-output boundary. Each run's
variance remains the stable QR residual-coordinate estimate. The pooled
coefficient vector is the full multivariate solution of the summed run
precisions and precision-weighted coefficients; selection is applied after
that solve. Marginal uncertainty uses the same factor against requested
readout weights. Unrequested covariance output stays absent.

The current baseline must be measured on this source and provider revision;
historical 451/1494 MiB figures are not a baseline for this candidate.
Before any pooling edit, source SHA-256 values are:

- `FixedEffectsEstimates.scala`: `95177f1004742019a0a06e9c91de9106d4a604916a67d51994f0a272692bacd3`
- `FirstLevelFixedEffectsEstimates.scala`: `951483a49d45d038811ff52812564a60cf1c7a4161ebcdb1d93c71d0698a977f`
- `CoefficientMatrixStorage.scala`: `8873341868ab393aa5e2071f78b03dd6fabf1cee62df20529a3f8661c70afe4e`

The measured baseline starts at commit `ca0f3602480ff2d739a74b46f3d19161bb828560`,
with the pinned Gale revision `18d24dbb5056122032b0278f8bad557a9bb1cf23`.
The diagnostic correction `d352c483` changes no pooling source. The host is
Darwin 23.3.0 arm64, Mac15,11, 36 GiB physical RAM, 14 logical CPUs; the
prescribed sbt runner caps its JVM at 3 GiB and four active processors.

## Workloads and admission rule

The component benchmark `SelectedPrecisionPoolingBenchmark` measures 8192
voxels with either two or 64 jointly pooled outputs. It separately measures
precision accumulation and the full selected pooling step. The end-to-end
benchmark `SelectedPrecisionEndToEndBenchmark` uses 160 rows over two runs,
mixed TR (2.0 s and 0.8 s), 8192 voxels, block size 512, with two or 64
selected outputs. Resident and mapped binary readers expose the identical
synthetic matrix through the public `DatasetSeriesReader` interface. The
mapped reader accesses requested rows/voxels from a file mapping; neither
reader changes the fit plan or estimator.

The admission criterion, set from the unchanged-source baseline before
candidate measurement, is at least a 3% whole-fit allocation decrease in the
64-output workload and at least a 10% component allocation decrease, without
more than a 2% whole-fit time regression across either backing and width.
The change must preserve the numerical and identity tests, and must not
increase the invocation-level peak RSS materially. The baseline component
uses about 5% of the 64-output whole-fit allocation, so a 3% whole-fit
decrease is demanding but possible. Five JMH samples per case give a useful
comparison, not a universal performance guarantee.

## Baseline

The first end-to-end smoke (`diagnostics-smoke-01.log`) failed before measuring:
its hand-built event design had legacy structural identity and was correctly
rejected by fixed-effects preparation. The benchmark fixture now supplies an
explicit validated compiled design schema. This failure is retained as a
rejected fixture, not a numerical or timing result. Corrected smoke checks and
matched measurements are below. Corrected `diagnostics-smoke-02.log` exited 0.
Its two-output checksum was `-0.45135330724036893` for both resident and
mapped-file readers; its 64-output checksum was `-0.4565974697703291` for
both. This establishes matched observable work for the timing specimens, not
a scientific oracle for the pooled estimator.

The unchanged-source JMH receipts are
`/private/tmp/scalafim-execution-20260929/baseline-full-jmh.json` and
`/private/tmp/scalafim-execution-20260929/baseline-component-jmh.json`.
Both exited 0; the commands and complete output are in
`logs/diagnostics-baseline-{full,component}.log` under the execution root.
Each case had one fork, two 300 ms warmups, five 300 ms measurements, one
thread, and the JMH GC profiler. The whole fit includes reading, preparation,
QR, selected pooling, and output construction.

| Workload | Mean ms/op | Observed sample range ms/op | Allocated MB/op |
| --- | ---: | ---: | ---: |
| Whole fit, 2 outputs, resident | 3594.6 | 3553.6–3692.0 | 14633.6 |
| Whole fit, 2 outputs, mapped file | 3537.1 | 3518.0–3562.9 | 14633.1 |
| Whole fit, 64 outputs, resident | 6112.8 | 6106.1–6123.7 | 15776.2 |
| Whole fit, 64 outputs, mapped file | 7177.0 | 7136.1–7246.9 | 15777.9 |
| Precision accumulation, 2 outputs | 0.427 | 0.394–0.489 | 2.294 |
| Precision accumulation, 64 outputs | 91.0 | 84.9–102.9 | 807.3 |
| Selected pooling, 2 outputs | 1.303 | 1.181–1.397 | 5.769 |
| Selected pooling, 64 outputs | 623.8 | 359.0–698.9 | 1095.3 |

The first 64-output selected-pooling sample is much faster than the remaining
four, so its mean is unstable; the isolated precision-accumulation allocation
is the clearer attribution evidence. No whole-fit result alone identifies a
pooling hotspot. `/usr/bin/time -l` recorded 3.38 GB maximum RSS and 282.08 s
user plus 11.65 s system CPU for the entire whole-fit baseline invocation.
Those process figures include sbt/JMH startup and all four cases, and cannot
be assigned to a single benchmark cell. Its elapsed time also includes time
waiting for the shared sbt lock, so JMH reports the comparable wall time.

## Candidate and verification

The candidate changes only `CoefficientMatrixStorage.sumAt`. When every
precision field is the closed `Scaled` representation, it multiplies each
base entry by that voxel's scale while accumulating into the one output
builder. It visits runs and matrix entries in the original order, avoiding
the two temporary scaled matrices per voxel. Mixed or caller-supplied fields
use the original path unchanged. The full multivariate solve, QR residual
variance calculation, optional uncertainty logic, and output selection are
unchanged.
The candidate `CoefficientMatrixStorage.scala` SHA-256 is
`474889e42ae59061441f48deb4e69876d7f6403d8fb1a5ccb764a8062bf13fae`.
Both before and after runs used unchanged benchmark files: component SHA-256
`40e459b0e5c9fc3e81e31f0104e81ebda9edf3ce50d8b282e8a2b0c12f838975`
and whole-fit SHA-256
`ea61ab100fc4ed218c0dacd8dfd4befc461ccc6f81edb687000f12d700d1fd92`.

`diagnostics-pooling-tests-r1.log` exited 0: 20 selected estimator tests on
JVM and the same 20 on JS, plus warning-clean benchmark compilation. These
include an independent rational correlated-precision oracle, mixed-TR R
contrasts and FIR bins, rank and malformed-field refusal, row/voxel reorder,
optional uncertainty suppression, and arbitrary/structured provider cases.
`diagnostics-final-tests.log` ran the complete fit suites successfully: 340
JVM tests and 328 JS tests passed with no failures. Its overall process exit
was 1 solely because the later checksum smoke invocation omitted its required
arguments. The corrected four-case `diagnostics-candidate-smoke-r3.log`
exited 0. The 2-output checksum was `-0.45135330724036893` for both
resident and mapped-file readers, and the 64-output checksum was
`-0.4565974697703291` for both. Both exactly match the baseline smoke
outputs. These checks establish that the measured runs produce the same
selected observable, while the independent suites establish numerical parity.

The candidate component JMH receipt
`/private/tmp/scalafim-execution-20260929/candidate-component-jmh.json`
exited 0 with the identical specimen and settings:

| Workload | Baseline ms/op | Candidate ms/op | Baseline MB/op | Candidate MB/op |
| --- | ---: | ---: | ---: | ---: |
| Precision accumulation, 2 outputs | 0.427 | 0.209 | 2.294 | 1.049 |
| Precision accumulation, 64 outputs | 91.0 | 44.9 | 807.3 | 269.6 |
| Selected pooling, 2 outputs | 1.303 | 1.086 | 5.769 | 4.524 |
| Selected pooling, 64 outputs | 623.8 | 309.0 | 1095.3 | 558.3 |

The candidate selected-pooling 64-output samples were 306.7–311.4 ms/op.
The paired whole-fit candidate receipt
`/private/tmp/scalafim-execution-20260929/candidate-full-jmh.json` exited 0.

| Whole-fit workload | Baseline ms/op | Candidate ms/op | Time change | Baseline MB/op | Candidate MB/op | Allocation change |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 2 outputs, resident | 3594.6 | 3525.0 | −1.94% | 14633.6 | 14631.6 | −0.014% |
| 2 outputs, mapped file | 3537.1 | 3565.0 | +0.79% | 14633.1 | 14631.4 | −0.012% |
| 64 outputs, resident | 6112.8 | 6109.3 | −0.06% | 15776.2 | 15221.6 | −3.52% |
| 64 outputs, mapped file | 7177.0 | 7199.0 | +0.31% | 15777.9 | 15221.1 | −3.53% |

The 64-output allocation reductions exceed the 3% admission threshold for
both backings. No case exceeded the 2% time regression limit. The 64-output
candidate observed timing ranges were 6073.0–6136.4 ms/op resident and
7162.6–7241.6 ms/op mapped; these overlap the baseline ranges. Thus the
whole-fit timing evidence supports no material slowdown, not a claim of a
whole-fit speedup. The two-output allocation changes are negligible, as
expected because a two-column temporary matrix is small.

The candidate whole-fit `/usr/bin/time -l` invocation recorded 2.82 GB
maximum RSS and 253.48 s user plus 12.82 s system CPU, compared with the
baseline invocation's 3.38 GB and 282.08 s user plus 11.65 s system. This
direction is consistent with lower allocation, but startup, compilation,
JMH forks, and shared-lock wait make invocation CPU/RSS coarse evidence;
they do not measure per-case CPU or peak memory. JMH is the per-case wall and
allocation evidence.

Raw JSON SHA-256 receipts (in baseline-full, baseline-component,
candidate-full, candidate-component order) are
`d3908b621dcfca51578ea8196c973cc9d30710a1f6031377aac7ac0c4ba303cc`,
`a5d826b753ffdfa6a0766ed1e70c7369540553d4e60f492241443354423d20db`,
`70e88b9c625613f3c5f835a7dc21b804b21b21beb516f3afb3056f88eaa8c4f0`,
and `4c3b7667156f6ec76f05d860014ba8433cbe14523a8dc1cdf0224139fe4e85da`.
