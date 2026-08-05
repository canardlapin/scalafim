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
four raw reports plus a machine-readable admission receipt under
`target/first-level-benchmarks/` by default. Set `SCALAFIM_BENCHMARK_OUT` to use
another output directory.

## What is measured

| Area | Benchmarks | Scientific or architectural question |
| --- | --- | --- |
| Basis response | point, exact window, trapezoid window | What does a typed response functional cost, and does exact integration retain its intended advantage? |
| Basis reconstruction | typed reconstruction, direct contraction | What is the explicit cost of reconstructing through the public typed basis surface? |
| Regressor convolution | production convolution, FFT, direct loop | Does the selected production path remain preferable at the admitted first-level shape? |
| Epoch integration | exact, trapezoid | Does analytic integration retain its intended time and allocation advantage? |
| OLS | plan-and-fit, prepared multiresponse, prepared single response, prepared chunking | How much work is planning, how much is response fitting, and what does chunk assembly cost? |
| WLS | plan-and-fit, prepared fit | Is factorization reuse visible for weighted fits? |
| fixed GLS | plan-and-fit, prepared fit | Is covariance preparation separated from repeated response fitting? |

The admitted fit shape has 360 time points, 32 predictors, and 128 responses.
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
multiresponse execution are recorded as advisory ratios. They make overhead
visible without encoding a false expectation that the higher-level path must
beat the lower-level primitive in every run.

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
python -S tools/benchmark/finalize_first_level_receipt.py \
  --check docs/benchmarks/receipts/first-level-current.json
```

A benchmark may pass every numerical budget while remaining ineligible for a
release. In particular, the finalizer records a dirty worktree as a blocking
caveat. The dedicated CI workflow publishes raw reports and its receipt as an
artifact; an actual release court must use a clean checkout and retain those
raw reports alongside the final machine-readable release report.

Performance results are machine- and JDK-specific. Compare like environments,
use the recorded ratios to interpret shared-runner noise, and investigate a
failure before changing the baseline.

## Current measured baseline

The checked-in receipt was measured on arm64 macOS with OpenJDK 25.0.1 and JMH
1.37: one fork, two 500-millisecond warmup iterations, and four
500-millisecond measurement iterations. It passes the admission policy but is
not release eligible because the source checkout was dirty.

| Work | Time | Allocation |
| --- | ---: | ---: |
| typed basis reconstruction | 0.645 us/op | 912 B/op |
| direct coordinate contraction | 0.552 us/op | 272 B/op |
| exact basis window functional | 1.271 us/op | 1,000 B/op |
| trapezoid basis window functional | 86.867 us/op | 44,065 B/op |
| production convolution | 565.777 us/op | 3,527,285 B/op |
| retained FFT convolution | 8,628.893 us/op | 6,679,493 B/op |
| retained direct-loop convolution | 2,570.162 us/op | 8,763,708 B/op |
| exact epoch integration | 298.790 us/op | 55,877 B/op |
| trapezoid epoch integration | 14,346.294 us/op | 5,864,222 B/op |
| OLS plan and fit | 7.326 ms/op | 664,616 B/op |
| prepared OLS multiresponse fit | 6.824 ms/op | 444,415 B/op |
| prepared OLS chunk assembly | 5.529 ms/op | 562,388 B/op |
| prepared OLS single response | 0.043 ms/op | 12,289 B/op |
| WLS plan and fit | 7.768 ms/op | 1,268,852 B/op |
| prepared WLS fit | 7.122 ms/op | 444,418 B/op |
| fixed-GLS plan and fit | 15.345 ms/op | 1,892,971 B/op |
| prepared fixed-GLS fit | 7.720 ms/op | 1,135,362 B/op |

These rounded values are for reading; the JSON receipt retains full precision,
parameters, source hashes, and comparison ratios.
