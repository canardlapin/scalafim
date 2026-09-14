# Group numerical baseline recovery — 2026-09-14

Issue: `bd-01M20TKX7VYFT3BHCM7QAMA6NG`

Review baseline: `925da6da3ce88394145c012b2a8c68e2e5cca66d`

Current Gale revision: `099832ff15c8a4a8fcf3398c7b779fb4bbc12434`

## Scope

This tranche recovers and requalifies the core group-model numerical baseline
from candidate `ab7ad147a23280e29abfe62a7acea27c5559d299`. The recovery is intentionally
narrow: it includes the typed design, weighting, fitting, result, contrast, and
failure contracts plus direct numerical tests. It excludes the candidate's old
first-level bridge, sign-flip inference, contrast diagnostics, and bulk generated
reports and receipts. The first-level bridge now belongs to `fit-estimates` and
was tested there as a consumer instead of being restored under `group`.

The recovered contracts are:

- QR-prepared group designs with coefficient results transformed back to the
  caller's original units, origin, and column order;
- per-sample OLS and inverse-variance fitting with typed, identified failures;
- explicit DerSimonian-Laird and Paule-Mandel heterogeneity policies and an
  explicit modified Knapp-Hartung inference policy;
- propagation of sample-level failures through fits and contrasts;
- construction-time validation of designs, weights, and nonzero contrasts; and
- `Either`-reported WLS failures rather than unchecked numerical exceptions.

No recovered method becomes a default merely by being present in this tranche.

## Independent numerical evidence

`GroupNumericalBaselineSuite` adds five bounded checks:

1. an independently evaluated intercept-only DerSimonian-Laird and
   Paule-Mandel reference problem;
2. coefficient and contrast invariance under changes of units, predictor
   origin, and design-column order;
3. exact sample identities for partial failures and an all-failed fit;
4. finite behavior under a `1e16` inverse-variance imbalance; and
5. rejection of invalid designs, weights, and zero contrasts, including a
   same-term contrast constructed through the public helper.

The precision-imbalance case exposed an important discrepancy in the historical
candidate receipt. Its claimed `1e-12` coefficient tolerance fails with
`8.000000003107811` instead of `8.0` on both the current Gale revision and an
exact checkout of the historical Gale revision
`83cac90`. The adopted `2e-7` coefficient bound is derived from the approximately
`1e8` condition number of `sqrt(W) X`: first-order double-precision error is on
the order of `eps * cond * |beta|`, approximately `1.8e-7`. Well-conditioned
coefficients and covariance remain subject to much tighter checks. This is a
qualified limitation, not a reproduced historical pass.

Fresh tests on the working candidate after adding the metafor corpus:

- `groupJVM/test`: 69 passed;
- `groupJS/test`: 68 passed (the JVM suite has one runtime-boundary-only test);
- `fitEstimatesJVM/test`: 11 passed;
- `fitEstimatesJS/test`: 8 passed;
- `fmriWorkflowJVM/test`: 23 passed;
- `fmriWorkflowJS/test`: 19 passed; and
- `sbt -Dsbt.supershell=false scalafimCompileAll`: passed, both platforms,
  warning-clean.

## Fresh committed-diff review

A second review used `925da6d..f0c9f5c` as the exact boundary, started from the
changed public types, and traced every repository construction and match site.
It found no blocking loss of a type guarantee and no unchecked cast,
exhaustivity suppression, or erased error introduced by the recovery.

Two API consequences are explicit:

- `GroupContrast.difference` now returns `Either[GroupError, GroupContrast]`.
  This is a source-level change, but it prevents normalized same-term inputs
  such as `"a"` and `" a "` from constructing an all-zero contrast. All current
  call sites consume the typed result.
- Weighted partial failures retain rectangular numerical maps by writing `NaN`
  at unavailable samples, but the sentinel is paired with a unique,
  bounds-checked `GroupSampleFailure` vector and is propagated to term and
  contrast results. It is not an unlabelled success value.

The legacy `randomEffects` constructor continues to name the historical DL/z
policy. New code can use `mixedEffects`, which requires both tau estimator and
inference policy explicitly. The review does not reinterpret compatibility DL/z
as a calibrated default.

## Current metafor oracle

`tools/r-parity/generate_group_metafor_fixture.R` generates a checked JSON
receipt and shared Scala fixture using R 4.5.1 and metafor 5.0.1. Three designs
cover an intercept-only heterogeneous case, an offset moderator, and a
three-term meta-regression. Each is evaluated under FE/z, DL/z, PM/z, DL/mKH,
and PM/mKH. The Scala suite compares every coefficient and covariance entry,
including off-diagonals, plus standard errors, statistics, p-values, tau squared,
fixed-Q, and residual degrees of freedom.

The first oracle run retained three failures rather than weakening the whole
comparison. One FE p-value differed by `1.73919273e-8`, within the portable
distribution implementation's documented `1e-7` approximation bound. The two
PM failures were caused by metafor's default root tolerance: it returned
coefficient `0.37712550958999003` with reweighted Q
`6.9997279634404688`, while ScalaFIM returned `0.3771140571511673` at the
tighter root. Pinning the independent metafor call to
`control=list(tol=1e-10)` gives coefficient `0.37711405715119023` and Q
`6.9999999999994884`. No Scala estimator tolerance was changed.

The regenerated corpus passes all 15 cases on both JVM and Scala.js. The
generator's `--check` mode also reproduces the checked-in JSON and Scala bytes.

## Current resource receipt

`GroupBaselineBenchmark` was run in separate sbt processes with 100 subjects,
4 design terms, 50,000 samples, 5 warmups, and 3 measured iterations. The table
reports medians from the three measured iterations.

| Policy | Wall seconds | Thread CPU seconds | Allocated bytes |
| --- | ---: | ---: | ---: |
| OLS | 0.036432459 | 0.036433 | 5,223,880 |
| Fixed effects | 0.109100333 | 0.108889 | 67,243,328 |
| DerSimonian-Laird | 0.213652167 | 0.213244 | 118,043,136 |
| Paule-Mandel + mKH | 0.765122084 | 0.763618 | 399,906,152 |

Finite-output checksums were, respectively:

- OLS: coefficients `44901.00478841063`, standard errors
  `26150.966362847816`;
- fixed effects: coefficients `44936.01770144746`, standard errors
  `14659.880767320166`;
- DerSimonian-Laird: coefficients `44899.77709618844`, standard errors
  `27844.717547127686`; and
- Paule-Mandel + mKH: coefficients `44901.988385096796`, standard errors
  `26115.002575797`.

These measurements qualify the current candidate's resource shape and finite
outputs. They do not establish an improvement over a pre-recovery comparator.

### Exact pre-recovery comparator

The same harness was subsequently run for five measured iterations on
`f0c9f5c` and on an archive of exact parent `925da6d`. The comparator adaptation
only removes unavailable PM/mKH syntax; the workload and measured body are
unchanged. Negative percentages mean the recovered candidate used less of the
resource.

| Policy | Candidate wall | Parent wall | Wall change | CPU change | Allocation change |
| --- | ---: | ---: | ---: | ---: | ---: |
| OLS | 0.051932167 | 0.051990500 | -0.11% | -0.16% | +0.41% |
| Fixed effects | 0.155703416 | 0.202976750 | -23.29% | -23.31% | -37.74% |
| DerSimonian-Laird | 0.298618584 | 0.415503792 | -28.13% | -28.79% | -45.05% |

Checksums agree to floating-point roundoff. OLS therefore has no material
improvement claim; the fixed-effects and DL weighted paths do show bounded
time/allocation reductions for this workload. All raw runs, hashes, environment,
and caveats are preserved in
`docs/benchmarks/receipts/group-baseline-comparator-2026-09-14.json`.

## Limitations and completion plan

This tranche does not admit a new statistical default or make a universal
correctness claim. It has no new held-out group-inference calibration. The
current metafor corpus establishes formula and numerical parity for the named
policies; it does not overturn the retained adverse calibration results for
small samples or estimated first-level variances.

Before closing `bd-01M20TKX7VYFT3BHCM7QAMA6NG`, the remaining steps are:

1. commit the review, oracle corpus, and exact comparator receipt;
2. qualify the group suite, first-level bridge, and workflow consumers from that clean,
   identified commit; and
3. record the resulting evidence in Mote before closing the issue or admitting
   any method as a default.
