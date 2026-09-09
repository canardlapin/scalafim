# ScalaFIM group-model repair qualification — 2026-09-08

The audited identity, contrast-construction, unit-scaling and weighted-failure defects are repaired. Weighted allocation and CPU costs are lower on the measured dense workloads. PM and modified Knapp–Hartung (mKH) are independently verified as explicit strategies, but the calibration results **do not admit an automatic small-sample default**.

These are local source changes over native HEAD `72a35a46df661e6e8b0e8dd6bd7dbb59002ec9e4`. The numerical baseline is the audited group implementation from `fd992a0c85001eb50ba2e78d29c497693ca494dc`; its group sources remained unchanged through the audit-retention commit. The original [audit report](group-audit-2026-09-08.md) and receipt remain intact.

## Repairs and regression evidence

- **First-level bridge:** align both estimates and SEs by voxel index to the requested VoxelAxis; reject duplicate/substituted indices, contrast-name substitutions, negative/zero/nonfinite SEs and SE² overflow/underflow. Geometry-free axes require canonical indices; a voxel result cannot silently become a parcel result.
- **Named contrasts/designs:** subtraction occurs before keyed reduction, so A−A becomes a rejected zero contrast, never −A. `fromStrings` returns typed errors for empty, all-zero and nonfinite weights and normalized-name collisions. Two-sample labels validate before construction and cannot collide with the intercept term.
- **Numerics:** normalize covariate units, center on an existing constant and check rank with Gale pivoted QR. Fit on a shared orthonormal basis and transform coefficients/covariance back to the original parameterization. Weighted solves normalize precision and use Gale weighted QR if weight imbalance makes the normal equations unsuitable. No private solver or inverse implementation was introduced.
- **Availability:** reject globally invalid designs before sample fitting. Partial weighted failures carry sample indices and typed reasons through `GroupFit`, term and contrast results; valid samples remain available. All-failed weighted maps return `Left(AllSamplesFailed(...))`. Direct `GroupGlm.wls` now returns `Either`, replacing the former successful-NaN contract.
- **Allocation:** reuse primitive Gram/RHS/covariance scratch; generate weights while processing each sample; retain only that sample's first pass for random effects. No full reciprocal/reweighted matrices or full retained first-pass output maps.

Final ordinary test command:

```sh
sbt 'all groupJVM/test groupJS/test fmriWorkflowJVM/test fmriWorkflowJS/test'
```

**Pass: 82 group tests on JVM, 82 on JavaScript, 55 workflow tests on JVM, and 44 on JavaScript (263 executions).** The 16 historical public audit probes also passed on both platforms before their temporary copies were removed from the candidate. Final group and workflow source sets match the native checkout byte-for-byte: 61 Scala files. Tests ran in the isolated audited provider build, using Gale `83cac90a678d1b8a31c590e0c1b8fc8bf3427161` and the retained audit dependency inputs. This is not `testAll`, hosted CI, packaged application, or UI proof.

The new [GroupRepairSuite](../../modules/group/shared/src/test/scala/scalafim/fmri/group/GroupRepairSuite.scala) retains independent executed R `lm` / `metafor` fixtures, participant/sample reorderings, unit changes from 1e−12 to 1e12, covariate-origin and column-order changes, intercept-containing contrasts, a 1e16 precision ratio with analytic covariance, genuine rank deficiency, invalid uncertainty, and identified partial/all-sample failures. PM/DL × z/mKH comparisons check coefficients, SEs, named-contrast covariance/statistics/p-values and heterogeneity. R's PM root tolerance is explicitly tightened to 1e−12; the Scala stopping rule is `abs(Q-df) <= 1e-9 * df` with checked convergence and a zero boundary.

## Inference qualification

`TauEstimator.PauleMandel` and `MetaInference.ModifiedKnappHartung` are explicit additions. mKH scales coefficient and contrast covariance by `max(1, Q_RE/(n-p))` and uses `t(n-p)`, matching metafor's `test="adhoc"`. The new `GroupModel.mixedEffects` requires estimator and inference arguments. The older `randomEffects` constructor retains documented DL/z compatibility. Consumers pattern-matching `RandomEffects` must account for its added inference field. See the [metafor method contract](https://wviechtb.github.io/metafor/reference/rma.uni.html) and the [original Hartung–Knapp paper](https://pubmed.ncbi.nlm.nih.gov/11406840/) for method context; formula parity is separate from calibration.

Fresh calibration used 20,000 independent Gaussian null data sets in each of 54 configurations: n=8/20/80; p=1/3; known sampling variances or independent chi-square variance estimates with df=8/40; true τ²=0/.2/1. Variances span .04 to 1. The p=3 design includes an imbalanced group indicator and a continuous covariate. Four methods use the same generated data: **1,080,000 data sets and 4,320,000 sample fits, with zero failed fits.** Re-running after buffer refinement produced identical rejection/failure counts for all 216 method/configuration results.

Selected pointwise rejection rates at nominal α=.05; intervals are 95% Wilson Monte Carlo intervals:

| Subjects / terms | First-level variances | True τ² | Method | Rejection | Monte Carlo interval |
|---|---|---:|---|---:|---:|
| 8 / 1 | known | 0.2 | ols | 4.680% | 4.396–4.982% |
| 8 / 1 | known | 0.2 | meta:re(DL,z) | 11.475% | 11.041–11.924% |
| 8 / 1 | known | 0.2 | meta:re(DL,mKH) | 6.595% | 6.259–6.947% |
| 8 / 1 | known | 0.2 | meta:re(PM,mKH) | 6.895% | 6.552–7.254% |
| 20 / 1 | known | 0.2 | ols | 4.785% | 4.498–5.090% |
| 20 / 1 | known | 0.2 | meta:re(DL,z) | 7.690% | 7.329–8.067% |
| 20 / 1 | known | 0.2 | meta:re(DL,mKH) | 5.690% | 5.377–6.020% |
| 20 / 1 | known | 0.2 | meta:re(PM,mKH) | 6.040% | 5.718–6.379% |
| 80 / 1 | known | 0.2 | ols | 4.880% | 4.590–5.187% |
| 80 / 1 | known | 0.2 | meta:re(DL,z) | 5.585% | 5.275–5.912% |
| 80 / 1 | known | 0.2 | meta:re(DL,mKH) | 5.130% | 4.833–5.444% |
| 80 / 1 | known | 0.2 | meta:re(PM,mKH) | 5.420% | 5.115–5.742% |
| 80 / 3 | estimated, df=8 | 0 | ols | 4.200% | 3.931–4.487% |
| 80 / 3 | estimated, df=8 | 0 | meta:re(DL,z) | 8.555% | 8.175–8.951% |
| 80 / 3 | estimated, df=8 | 0 | meta:re(DL,mKH) | 7.915% | 7.549–8.297% |
| 80 / 3 | estimated, df=8 | 0 | meta:re(PM,mKH) | 8.100% | 7.730–8.486% |

mKH reduces some DL/z inflation but remains liberal in relevant cases; it can also be markedly conservative at the zero-heterogeneity boundary. PM/mKH is therefore an explicit method option, **not a certified default**. These results do not establish power, non-Gaussian robustness, repeated-subject validity or spatial FWER/FDR calibration. Native admission follow-up: `bd-01M20TKX7VYFT3BHCM7QAMA6NG`. This is a documented decline of general default admission, not a deferred implementation labeled complete.

## Dense execution performance

Apple M3 Max, 14 logical CPUs, 36 GiB RAM, macOS 14.3, Homebrew OpenJDK 25.0.1; JVM heap 3 GiB and six active processors. Each version/configuration ran in **three fresh JVM forks**, with **ten warmup fits and five measured fits per fork**. All 36 processes completed successfully; 180 measured fits. Identical seeded Gaussian effects and continuously varying variances replace the audit's repeated voxel patterns. Every coefficient and SE was checked outside timing; aggregate coefficient/SE checksums agree within 1e−11 relative across versions.

All rows use 100 subjects. Fit wall/CPU/allocation entries are medians of three fork medians. RSS is the largest whole-process peak across the three forks, including inputs, warmup, outputs and JVM overhead. Arrows are baseline → repaired. MB uses decimal units.

| Terms / samples | Method | Wall s | Calling-thread CPU s | Allocation MB / fit | Peak RSS MB |
|---|---|---:|---:|---:|---:|
| 4 / 200,000 | OLS | 0.383 → 0.389 | 0.213 → 0.210 | 20.8 → 20.8 | 699 → 716 |
| 4 / 200,000 | FE | 1.408 → 1.014 | 0.826 → 0.675 | 432.0 → 268.8 | 1506 → 1168 |
| 4 / 200,000 | DL | 2.734 → 1.858 | 1.649 → 1.166 | 859.2 → 478.4 | 1736 → 1276 |
| 12 / 20,000 | OLS | 0.073 → 0.071 | 0.054 → 0.051 | 5.9 → 6.0 | 202 → 214 |
| 12 / 20,000 | FE | 0.930 → 0.603 | 0.441 → 0.350 | 162.6 → 108.8 | 654 → 583 |
| 12 / 20,000 | DL | 1.213 → 1.040 | 0.907 → 0.579 | 324.6 → 196.1 | 678 → 569 |

At 4 terms / 200,000 samples, FE allocation falls **37.8%** and calling-thread CPU **18.3%**; DL allocation falls **44.3%** and CPU **29.3%**. At 12 terms / 20,000 samples, FE/DL allocation falls **33.1%/39.6%**, with CPU reductions **20.5%/36.2%**. OLS costs are broadly unchanged. Compare CPU, allocation and RSS separately: RSS is not retained output size, and allocation is not a peak-memory bound.

The one-minute host load ranged from 52.6 to 126.6; wall time remains shared-host evidence, not an SLA. Runs were sequential with no overlapping fitting/test job from this session. The final benchmark is distinct from an earlier exploratory buffer version; its incomplete exploratory run is excluded. After benchmarking, only a low-level row-count diagnostic's argument labels, covariance documentation and regression coverage changed. `GroupEngine.fit`, which the benchmark exercises, does not call that low-level diagnostic; successful-fit arithmetic is unchanged. The receipt preserves the benchmark kernel source and runtime hashes separately from the final verified sources.

## Bounds and adoption

- Dense inputs and packed covariance outputs remain resident. Manifest-backed spatial blocking belongs to native `bd-01KX6G9BFJRERNFRWR17WT0YGN`; these measurements exclude loading, disk IO and app rendering.
- Voxel indices do not prove co-registration. `TContrastResult` does not carry the affine/registration provenance needed to verify physical alignment. Callers must establish it and align design rows to subject IDs.
- This bridge does not carry first-level residual df or cross-response covariance. Separate responses are fit separately; extra runs, conditions or FIR coefficients must not become independent subjects.
- Partial-result consumers must respect failure indices. Finite-family FDR handling is implemented, but statistical guarantees under outcome-dependent sample exclusion are not established here.
- The group inference qualification ticket `bd-01M20NE80H1DXKQCFQZT15FTTP` is completed as a tested qualification and explicit default-admission decline. The new admission ticket remains open. Estimator expansion continues under `bd-01KX2CQZGWBQ3KKX7BWE28X3T2`.
- PLS Neuro adoption is tracked in its own store as `bd-01M20V5F314V68XNY9SHDY4RR0`. It must adopt an exact provider revision/candidate and verify the group UI, availability, provenance and method-specific inference; no UI or release completion is claimed by this repair.

## Reproduction and receipt

[Qualification tools](../../tools/group-repair/README.md) describe ordinary tests, the independent R generator, extended calibration and the benchmark main. The compressed [receipt](group-repair-2026-09-08.json.gz) contains source hashes, the exact build/compile/runtime commands, final and historical-probe test logs, R inputs/references/session information, every calibration count, all 36 completed benchmark processes and resource output, and per-file runtime manifests.
