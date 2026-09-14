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

Fresh tests on the working candidate:

- `groupJVM/test`: 54 passed;
- `groupJS/test`: 53 passed (the JVM suite has one runtime-boundary-only test);
- `fitEstimatesJVM/test`: 11 passed;
- `fitEstimatesJS/test`: 8 passed;
- `fmriWorkflowJVM/test`: 23 passed;
- `fmriWorkflowJS/test`: 19 passed; and
- `sbt -Dsbt.supershell=false scalafimCompileAll`: passed, both platforms,
  warning-clean.

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

## Limitations and completion plan

This stopping point does not admit a new statistical default or make a universal
correctness claim. It has no held-out group-inference calibration, and the old
candidate's broad R/metafor grid and performance-improvement percentages were
not adopted as current evidence. Consumer tests also ran in a worktree that
contains unrelated in-progress changes; the commit for this tranche therefore
isolates only the group and threshold files listed in its diff.

To finish `bd-01M20TKX7VYFT3BHCM7QAMA6NG`:

1. independently review the narrowed committed diff and its typed API changes;
2. regenerate a bounded R/metafor oracle fixture against the current estimator
   policies and verify it on JVM and Scala.js;
3. if an allocation or throughput improvement will be claimed, measure the
   committed candidate against an exact pre-recovery comparator under the same
   harness and environment;
4. qualify the first-level bridge and workflow consumers from one clean,
   identified commit; and
5. record the resulting evidence in Mote before closing the issue or admitting
   any method as a default.
