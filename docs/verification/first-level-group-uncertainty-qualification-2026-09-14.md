# First-level group uncertainty qualification (2026-09-14)

Issue: `bd-01M210WJ4BWVCXEMTARHDR2AC7`

## Qualified contract

Variance and standard-error products may carry a
`MarginalUncertaintyDescriptor` bound to the same effect, observation,
estimand, and pooling axes. Its `MarginalVarianceOrigin` is one of:

- known variance, with an identified method;
- estimated variance with positive scalar residual or effective degrees of
  freedom;
- estimated variance with a positive sample-dependent df product on the same
  axes;
- unknown provenance, with a retained reason.

Approximate df must be identified as effective df. Reference-distribution df,
zero/negative values, an unaligned df product, or an absent semantic descriptor
cannot enter a weighted durable group read. The reader counts a df product in
the same bounded block budget as effects and uncertainty.

Every variance-carrying `GroupData` now carries a
`GroupUncertaintyReceipt`. Entries follow the subject, contrast, and sample
axes exactly and retain variance origin, resolved df, estimator, serial-noise
policy, nuisance policy, run-combination policy, product identity, and pooling
scope. Raw variance construction produces an explicit unknown receipt; it does
not invent df. Effects-only data remains receipt-free.

`GroupEstimateAdmission` now returns verified geometry evidence rather than a
bare success value. Its declared world frame must match every admitted unit.
The bounded reader retains that evidence in the resulting group data and still
performs no resampling.

## Native first-level bridges

`FitEstimateProducer` publishes OLS SE semantics with its nominal residual df.
A shared fit spanning multiple acquisitions is identified as `JointRuns`, and
the run-combination statement remains the producer's shared-fit policy.

Native `TContrastResult` values produced by contrast evaluation now retain the
fit engine, coefficient scope, response-preparation provenance, and complete AR
diagnostics. `FitGroupAdapter` maps those fields into the group uncertainty
receipt, preserves nominal residual df, and reorders values by voxel identity.
It refuses contrast-name disagreement, missing voxel identities, non-finite
effects, and non-positive or non-finite SEs. Manually constructed legacy
results have explicit unknown fit-policy fields, and the eager bridge always
marks cross-subject registration unknown because matching voxel indices alone
are not registration evidence.

## Evidence

Portable tests cover:

- known, unknown, scalar-df, and approximate sample-dependent-df origins;
- exact subject, contrast, and reordered sample axes;
- missing descriptors, invalid df, unavailable data, repeated participants,
  geometry refusal, and world-frame disagreement;
- retained joint-run GLS estimator, segmented AR policy, full-covariance
  run-combination statement, and pooling scope;
- real OLS contrast-to-group execution, exact SE squaring, nominal df, and
  explicit eager geometry limits;
- runwise GLS contrast propagation with voxelwise AR diagnostics;
- estimate metadata write/read and pinned group readback.

Worktree JVM gates:

- `RunwiseGlsSuite`: 4/4 passed.
- `estimatesJVM/test`: 11/11 passed.
- `estimatesIoJVM/test`: 9/9 passed.
- `groupJVM/test`: 75/75 passed.
- `fitEstimatesJVM/test`: 14/14 passed.

Scala.js and clean committed-candidate gates are recorded below once run.

## Admission boundary

This slice qualifies identity and provenance retention. It does not prove that
reported first-level variances are calibrated, derive effective df from nominal
residual df, admit an across-run AR pooling policy, establish registration from
shape or voxel indices, or make a new group inferential method eligible. Those
questions retain their independent numerical and scientific admission gates.
