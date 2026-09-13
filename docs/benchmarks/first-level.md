# First-level performance court

The first-level performance court protects distinct costs rather than one
opaque end-to-end timing. It measures HRF basis operations, response
functionals, convolution and integration paths, and fit execution with and
without reusable preparation. Correctness tests and parity tolerances remain
unchanged when a performance result moves.

Run the full local court with:

```sh
bash tools/ci/first-level-benchmark.sh
```

The script compiles the benchmark sources with the strict first-level compiler
policy, runs single-threaded JMH forks with the allocation profiler, and writes
five raw reports plus a machine-readable admission receipt under
`target/first-level-benchmarks/` by default. Set `SCALAFIM_BENCHMARK_OUT` to use
another output directory.

## What is measured

| Area | Benchmarks | Scientific or architectural question |
| --- | --- | --- |
| Basis response | point, exact window, trapezoid window | What does a typed response functional cost, and does exact integration retain its intended advantage? |
| Basis reconstruction | typed reconstruction, direct contraction | What is the explicit cost of reconstructing through the public typed basis surface? |
| Regressor convolution | production convolution, FFT, direct loop | Does the selected production path remain preferable at the admitted first-level shape? |
| Epoch integration | exact, trapezoid | Does analytic integration retain its intended time and allocation advantage? |
| AR estimation | fixed global, fixed runwise, automatic global | What do multiresponse AR(4) estimation, pooling, and order selection allocate and cost? |
| Compiled design | ownership snapshot, rank preview, fingerprint | What does the one-time immutable scientific-identity boundary cost? |
| OLS | plan-and-fit, prepared multiresponse, prepared single response, prepared chunking | How much work is planning, how much is response fitting, and what does chunk assembly cost? |
| WLS | plan-and-fit, prepared fit | Is factorization reuse visible for weighted fits? |
| fixed GLS | plan-and-fit, prepared fit | Is covariance preparation separated from repeated response fitting? |

The admitted fit shape has 360 time points, 32 predictors, and 128 responses.
AR estimation uses the same 360-by-128 response shape, two runs, explicit
censor resets, and a maximum order of four.
The convolution shape has 1,200 scans, a three-element basis, and 0.1-second
precision. Epoch integration uses a 12-second window at 0.05-second precision.
These are stable comparison points, not claims that one shape represents every
study.

## Admission policy

[`first-level-budgets.json`](../../tools/benchmark/first-level-budgets.json)
sets explicit time and allocation ceilings for every required row. The receipt
builder rejects missing or duplicate rows, unit changes, source-hash drift, or
a breached ceiling. It additionally requires:

- production convolution to beat both retained FFT and direct-loop baselines;
- exact epoch integration to beat trapezoid integration;
- prepared OLS, WLS, and fixed-GLS multiresponse fits to allocate less and run
  faster than their plan-and-fit counterparts.

Typed reconstruction versus direct contraction and chunked versus unchunked
multiresponse execution are recorded as advisory ratios. Automatic versus
fixed-order AR estimation and runwise versus global pooling are also recorded
without imposing a scientifically arbitrary ordering. These comparisons make
overhead visible without encoding a false expectation that one valid policy
must beat another in every run.

Budgets are deliberately broad enough to tolerate ordinary shared-runner noise
while still catching order-of-magnitude regressions. Change them only with a
new raw JMH receipt, an explanation of the changed workload or implementation,
and unchanged scientific correctness gates. Never loosen a numerical tolerance
to pass this court.

## Receipt and release eligibility

The checked-in
[`first-level-current.json`](receipts/first-level-current.json) records the
source commit, dirty state, hashes of every benchmark and court source, exact
JMH parameters, toolchain, scores, allocation rates, budgets, and comparison
ratios. Validation requires no JMH rerun:

```sh
python3 -S tools/benchmark/finalize_first_level_receipt.py \
  --check docs/benchmarks/receipts/first-level-current.json
```

The version-two receipt also binds the provider revisions, workload-definition
hash, and hashes of all five raw JMH reports. A benchmark may pass every
numerical budget while remaining ineligible for a release. In particular, the
finalizer records a dirty worktree as a blocking caveat. The dedicated CI
workflow publishes raw reports and its receipt as an artifact; an actual
release court must use the exact clean candidate checkout and retain those raw
reports alongside the final machine-readable release report.

Performance results are machine- and JDK-specific. Compare like environments,
use the recorded ratios to interpret shared-runner noise, and investigate a
failure before changing the baseline.

## Current measured baseline

The checked-in receipt was measured on arm64 macOS with OpenJDK 25.0.1 and JMH
1.37: one fork, two 500-millisecond warmup iterations, and four
500-millisecond measurement iterations. It passes the admission policy and was
release eligible for its clean source commit. It remains a comparison baseline,
not release evidence for a later candidate.

| Work | Time | Allocation |
| --- | ---: | ---: |
| typed basis reconstruction | 0.407 us/op | 928 B/op |
| direct coordinate contraction | 0.322 us/op | 272 B/op |
| exact basis window functional | 1.078 us/op | 992 B/op |
| trapezoid basis window functional | 51.031 us/op | 44,065 B/op |
| production convolution | 1,175.589 us/op | 4,684,141 B/op |
| retained FFT convolution | 6,405.560 us/op | 7,836,248 B/op |
| retained direct-loop convolution | 1,865.266 us/op | 9,920,498 B/op |
| exact epoch integration | 212.663 us/op | 55,835 B/op |
| trapezoid epoch integration | 10,248.797 us/op | 5,864,166 B/op |
| automatic global AR(4) estimation | 0.426 ms/op | 47,282 B/op |
| fixed global AR(4) estimation | 0.533 ms/op | 29,550 B/op |
| fixed runwise AR(4) estimation | 0.611 ms/op | 29,813 B/op |
| compiled-design planning | 1.452 ms/op | 1,738,031 B/op |
| OLS plan and fit | 2.004 ms/op | 664,499 B/op |
| prepared OLS multiresponse fit | 1.745 ms/op | 444,264 B/op |
| prepared OLS chunk assembly | 2.124 ms/op | 590,082 B/op |
| prepared OLS single response | 0.034 ms/op | 12,208 B/op |
| WLS plan and fit | 2.485 ms/op | 1,275,662 B/op |
| prepared WLS fit | 1.778 ms/op | 444,264 B/op |
| fixed-GLS plan and fit | 4.253 ms/op | 1,887,929 B/op |
| prepared fixed-GLS fit | 2.188 ms/op | 1,133,287 B/op |

These rounded values are for reading; the JSON receipt retains full precision,
parameters, source hashes, and comparison ratios.

The initial compiled-design measurement allocated 2,869,588 B/op. Building the
unchanged canonical fingerprint in one pre-sized buffer reduced that to
1,738,031 B/op on the clean candidate, below the original 2,000,000 B/op
ceiling without changing the budget or identity encoding.
