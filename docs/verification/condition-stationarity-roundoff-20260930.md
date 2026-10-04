# Gaussian evaluator precision and PHRF condition fitting

This change replaces the Gaussian family's uniform-grid multiplicative recurrence
with the direct per-lag exponential formula already used for irregular grids.
The shared decoder, stationarity tolerance (`1e-9`), work budgets, weak-identification
limits, and scientific admission thresholds are unchanged.

## Why the change is needed

The prior frozen development study reported candidate-policy admissions of
190/200 and 185/200 on JVM (SNR 1 and 0.5), versus 190/200 and 192/200 on JS.
Across those two cells, 27 voxel statuses differed between platforms; the largest
coordinate difference was 5.16e-8. All refusals were budget exits.
Saved JVM traces for SNR 0.5 voxels 9 and 31 contained stationary full-Newton
candidates whose computed energies increased by 7 and 1 ULP, respectively.
The existing decoder correctly refused those increases.

A 90-digit mpmath replay first refitted the amplitudes using the stored binary64
designs. Both candidate residual differences remained positive. Replacing scalar
energy subtraction with a stable residual difference would therefore not repair
these cases, and no generic ULP allowance was added.

The next replay held the compiled basis, compact design factor, projected response,
and coordinates fixed, and separated kernel evaluation from all subsequent sums
and amplitude fitting. High-precision projection of the stored recurrent samples
still gave positive differences. Analytic Gaussian samples, and direct binary64
Gaussian samples with high-precision downstream arithmetic, restored descent:

| Platform | Voxel | Stored kernel, MP downstream | Analytic kernel, MP downstream | Direct binary64 kernel, MP downstream |
|---|---:|---:|---:|---:|
| JVM |9|+1.67037e-12|-1.38210e-13|-1.37993e-13|
| JVM |31|+5.51493e-14|-3.65701e-14|-4.18539e-14|
| JS |9|+1.67037e-12|-1.38210e-13|-1.37993e-13|
| JS |31|+5.51494e-14|-3.65700e-14|-4.18539e-14|

The recurrent samples had maximum absolute error 1.62e-14 in these pairs.
This is enough to reverse a terminal step's tiny objective improvement.
The experiment concerns the fixed compiled basis, not a certificate over every
HRF shape or an original-family scientific qualification.

## Implementation and regression evidence

Each nonnegative lag now evaluates `exp(-0.5 * (t-tau)^2 * exp(-2*logSd))`
directly. Negative lags remain causal zeros. All analytic derivative formulas
are unchanged. The unused uniform-grid detector and recurrence constants are
removed. This also removes dependence on neighbouring sample positions and
preserves representable values below 1e-300, consistently with the previous
irregular-grid path.

Two new regression tests failed on unchanged production code while the eight
existing Gaussian tests passed: grid-context invariance and a 90-digit value/jet
oracle. The oracle uses exact binary64 inputs and independent `mp.diff`
derivatives at four nearby development-case shapes. Its absolute-scaled
4e-15 tolerance does not assert relative accuracy in the far tail.
The generator reproduces the fixture bytes exactly.

## Qualification status

The implementation is local commit `77f8d06cd0c069ab7761358bc6a4e4fe8120b399`.
The source freeze is `b1c4fd191cb5b1911871addeacf346413e621e13bb667f2239e4976eb5f41311`
on integration base `a6dbd3b188bd321c41053877121ccf64cf93259b`. The qualification
harness, platform entry points, decoder, and scientific protocol are byte-identical
to the prior frozen study. The integration base has a newer Gale pin than that
study, so the cohort comparison is not an isolated evaluator A/B experiment; the
fixed-basis multiprecision replay above supplies the causal evaluator evidence.

Verification:

- `scalafimCompileAll`: passed, no compiler warnings.
- Full HRF suites: 251 tests per platform; HRF laws: 82 per platform; decoder: 22 per platform.
- Final Gaussian suite, including the subsequently added tiny-tail regression: 10 tests per platform.
- Compact condition runtime and profile-fit suites: 9 tests per platform.
- Condition C0 controls and the extraction regression: 9 tests per platform.
- Independent oracle fixture regeneration: byte-for-byte match.
- Separate six-response DEV reference-search panel: passed on JVM and JS. Independent recomputation confirms maximum coarse/dense HRF latency error 3.052e-6 s, FWHM error 6.194e-6 s, and relative amplitude error 1.120e-6; the frozen limits are .002/.005/1e-4. All requested refinement levels completed. This is a modest search-stability panel, not proof of a global optimum.

The first broad test process was stopped at a later JS link under sustained pressure on its
2 GiB heap (exit 143); only its completed test targets count above. The outstanding
JS targets passed in a fresh 3 GiB process. This is a build-resource limitation,
not an engine-memory measurement.

The initial full JS development run reached its 1800-second timeout with no
cohort records (exit 124); it is retained as an incomplete attempt, not a result.
The identical frozen executable completed the 2700-second-limit retry with exit 0
after 1911.309407 seconds on the wrapper monotonic clock. The JVM run completed
with exit 0. UTC timestamps and monotonic duration are both retained; this was
not a performance measurement.

| Platform | SNR | Admitted / 200 | HRF latency error p95 (s) | FWHM error p95 (s) | Relative amplitude error p95 | Gate |
|---|---:|---:|---:|---:|---:|---|
| JVM | 1 | 191 | 0.000194714 | 0.00067179 | 0.00018114 | Pass |
| JVM | 0.5 | 195 | 0.000460752 | 0.00126473 | 0.000333524 | Pass |
| JS | 1 | 192 | 0.000194714 | 0.00067179 | 0.000163296 | Pass |
| JS | 0.5 | 195 | 0.000460752 | 0.00126473 | 0.000333524 | Pass |

All four candidate cells pass the unchanged minimum 190/200 and accuracy gates.
Independent Python audits checked all 1,600 terminal records across both policies
and platforms, including stationary acceptance, covariance, coherence, work caps,
reported p95 values, and gate truth. Every cell has zero terminal-audit failures
and zero unresolved references. Both reference-search panels also pass.

The candidate policy yields 19 different voxel statuses between platforms;
see `development-summary.json`. This is bounded qualification on each platform,
not bitwise parity or universal convergence. Both smaller-budget policies have
the same admitted IDs as their larger-budget counterpart on this cohort; that
is descriptive evidence and does not promote the smaller budget.

Next: integrate/review the exact candidate and complete the prior-use audit and
reviewed freeze before the preregistered fresh study. No fresh stream was used
in this work. PHRF-21 remains open. The separate original-family interval and
performance gates remain open as well.

Independent final review supports this development-only closure; see
`review-final-dev.md`. The prior failed study remains preserved. Direct evaluation
uses one exponential per nonnegative lag; no speedup or performance qualification
is claimed.

## Reproduction and retained evidence

The sibling `condition-stationarity-roundoff-20260930/` directory contains the
prior failed logs, the staged extraction, multiprecision results, source and
artifact manifests, and true process exit metadata. One trailing blank line was
removed from `GaussianFamily.scala` after compilation; the exact executed patch
is retained in `execution-tracked.patch.gz`, and both file hashes are in
`packaging-only-edit.json`. `archive-manifest.json`
records both compressed-file hashes and uncompressed input hashes.

With Python and mpmath 1.3.0, the independent staged calculation is:

```sh
gzip -dc docs/verification/condition-stationarity-roundoff-20260930/stages-both.log.gz > /tmp/phrf-stages.log
python3 docs/verification/condition-stationarity-roundoff-20260930/check_evaluator_stages.py /tmp/phrf-stages.log
python3 tools/hrf/generate_gaussian_precision_oracle.py
```

`check_fixed_design.py` accepts the decompressed `inputs-both-r2.log.gz`.
`analyze_oracle.py` independently recomputes the six-response search-stability
panel metrics. `analyze_study.py` accepts a decompressed standalone cohort log and checks every
terminal record, status count, covariance, stationary acceptance, work cap, and
reported p95. The source hashes identify the experiment; generated classpath and
JavaScript artifact hashes identify what actually ran.
