# Subject confirmation to group inputs — M5.02

Ticket `bd-01M2BNHVYX5971WGE1C78K5041`, isolated worktree
`scalafim-umvpa-family-group-20261007`, 2026-10-07.
Final affected-module gates passed **662 JVM + 661 JS** tests, with zero errors
or compiler warnings and one explicit opt-in campaign skip on each platform.
Intermediate focused gates remain historical; they do not replace the final
joint-model/origin/df-role source qualification. This packet makes no calibrated
group-procedure claim.

## Adopted capabilities and scope

The `mvpa-group` consumer depends on `mvpa` and `group`, keeping both providers
independent. It delegates every group fit to the adopted `GroupModel` and
`GroupEngine`. The closed group-baseline and uncertainty prerequisites retain
their original numerical/metafor and provenance qualification; their adverse
estimated-variance calibration results remain applicable. No historical branch
was cherry-picked and no group solver/distribution kernel was added.

The first-level adapter consumes the actual `VoxelLoadingConfirmation` result.
It requires an explicit, source-bound `LoadingSeparableGaussian` declaration:
joint errors are Gaussian with homogeneous feature covariance and
`Cov(vec-row(E)) = K_rows ⊗ Sigma_features`. Marginal Gaussian row laws alone
do not supply this assumption. GeneralJoint and Unknown model statuses are
typed refusals; a nonblank method label cannot substitute for the declaration.
The producer now retains its already-computed normalized conditional component
covariance **G**. A caller must provide a separately source-bound residual
feature covariance **Σ**; diagonal entries must match the actual estimated
residual scales squared. Mean, G, scales, df, design, evidence sources and row/
unit identities participate in its content identity. Σ is copied and validated
with Gale's eigenvalues-only PSD capability. Feature independence is never
inferred from marginal voxel errors.

The coefficient covariance is **Σ ⊗ G**, in feature-major/component-minor
order. It then follows the existing M5.01 task/spatial transport. The resulting
covariance is always **Estimated**, even when residual df `n-rank` is known.
This is an estimated-covariance plug-in arithmetic capability, not an assertion
that estimated variances support an exact known-Gaussian group reference.
External residual-covariance receipts do not authenticate physical origin.
The joint-model declaration identity is included in the residual covariance
and uncertainty source identities; a witness for another fitted source refuses.

## Immutable uncertainty origin

The narrow core API additions are mandatory `SubjectCovarianceOrigin` and
`SubjectCoordinateDfRole` metadata on `SubjectCoefficientEstimate.bind`.
Known, Estimated, Approximate and Unknown
are distinct from `SubjectCoordinateDf`; origin participates in the source v2
identity and survives every coordinate change. All current callers were
migrated. There is no compatibility constructor that silently picks an origin.

Df role is likewise immutable and hashed: Residual, Effective, Reference and
Unspecified remain separate. The loading adapter always supplies Residual.
Approximate df requires an explicit Effective role. Group Estimated declarations
must retain the exact source role; identical numeric df/method cannot relabel
residual df as effective, and Reference/Unspecified roles cannot enter an
estimated-variance group model. They remain available as arithmetic provenance.

`SubjectGroupSubject.declared` binds the actual coordinate identity,
covariance value source and uncertainty receipt, and checks the immutable
origin before accepting a native variance declaration. A loading-derived
Estimated covariance cannot become Known by extracting its coordinates and
redeclaring the same source with a new method label. Known covariance methods
must match the established method. Estimated/Approximate inputs retain actual
scalar residual/effective df, method and approximation labels; incompatible df
or Unknown uncertainty is refused.

The earlier M5.01 verification archive remains an immutable historical packet.
This packet records the new origin API and its source-bound integration gates.

## Eager/materialized bridge

The canonical `SubjectGroupInput` owns the full subject covariance matrices and
their source metadata alongside native `GroupData` and
`GroupUncertaintyReceipt`. Native contrasts carry the original ordered task
keys, while abstract measurement samples carry the exact feature keys.
Geometry evidence is retained in the input identity and is never inferred
from equal shapes or an affine. Physical beta/covariance units remain separate
from anatomical coordinate units such as mm.

Relabelling identical actual brain evidence as two independent participants is
refused from metadata before covariance access or numeric allocation. Distinct
independent brain identities may still share target evidence and row schemas;
this guard does not authenticate intentionally renamed foreign payloads.

`materialize` copies all numeric cells. `fromMaterialized` validates every
mean/covariance cell against its bound source before reconstructing the native
input; it retains full cross-feature and cross-component covariance, df and
uncertainty classification. Subject ordering, wrong task/space endpoints,
tampered values, missing uncertainty and exhausted budgets refuse directly.
The owned-cell bound excludes borrowed inputs, objects and provider scratch.

Native group fits are explicitly marginal. The returned model/fit retains its
canonical input, so the full covariance remains available rather than being
silently replaced by diagonal variance products. An explicit
`KnownVarianceGaussianFixedEffects` request requires Known covariance sources.
Estimated/Approximate sources can enter only an explicitly justified native
mixed-effects numerical calculation, such as PM/mKH; its approximation label
and retained provenance do not establish calibrated inference.

Joint component/spatial inference is a typed unavailable capability. Durable
export is also unavailable: current `EstimateDomain` requires actual RAS/mm
voxel geometry and pair-axis covariance is per sample. No fake voxel grid is
created for generic measurement/operator coordinates. Extending durable full
cross-feature artifacts or joint group inference belongs to its owning
provider, not a private helper here.

## API sketch

After freezing M5.01 shared coordinates and the exact discovery-selected
measurement alignment, run the admitted voxel-loading producer and supply its
actual residual feature covariance:

```scala
for
  jointLaw <- LoadingSeparableGaussian.declare(loading,
    LoadingJointGaussianModel.HomogeneousSeparable, actualJointModelReceipt)
  residual <- LoadingResidualCovariance.bind(loading, jointLaw, residualFeatureCovariance,
    residualCovarianceValueSource, residualCovarianceReceipt)
  subject <- SubjectGroupSubject.fromLoading(subjectColumn, nativeFeatures,
    confirmationDesign, loading, residual, shared, frozenSpatialAlignment,
    SubjectComparisonKind.SharedComponentCoefficients, frozenStability,
    physicalValueUnits, firstLevelFitProvenance, PoolingScope.JointRuns)
yield subject
```

Combine the ordered subjects under the exact common measurement axis:

```scala
for
  domain <- SubjectGroupDomain.samples(commonFeatures, geometryEvidence)
  eager <- SubjectGroupBridge.eager(shared, domain, orderedSubjectKeys, subjects)
  materialized <- eager.materialize()
  restored <- SubjectGroupBridge.fromMaterialized(materialized)
  model <- restored.marginalModel(groupDesign, declaredDesignSubjects,
    SubjectGroupCalculation.ApproximateMixedEffects(TauEstimator.PauleMandel,
      MetaInference.ModifiedKnappHartung, explicitApproximationJustification))
  calculation <- model.fit()
yield calculation
```

This does not promote an Estimated covariance to Known, invent effective df,
drop unavailable subjects, or imply M5 scientific-release qualification.

## Independent numerical evidence

`oracle.R` uses independent base-R dense least squares and covariance algebra.
Its Walsh fixture has nuisance-adjusted target scores and correlated residual
features. It computes all source/common mean and covariance cells, including
off-diagonal component and feature terms after an oblique task change.

For three explicitly known-variance subject inputs, component variances are
`(1,4,1)` and `(4,1,1)`, with effects `(1,3,5)` and `(2,4,6)`. Native fixed
effects must return means `(3,14/3)` and SE `(2/3,2/3)`. A second anatomical
measurement has effects scaled by 10 and variances by 9, giving means
`(30,140/3)` and SE `(2,2)`. These are analytic numerical fixtures; they are
not new group calibration studies.

`expected.tsv` retains every independent R expectation and `R-session.txt`
the measured R runtime. Reproduction:

```text
LC_ALL=C LANG=C /usr/local/bin/Rscript \
  docs/verification/umvpa-subject-group-20261007/oracle.R \
  docs/verification/umvpa-subject-group-20261007
```

`nonseparable-counterexample.R` separately qualifies the joint-law refusal.
Four independent Gaussian rows have both feature marginal row covariances I4,
but feature cross-covariance varies by row as `(.25,-.25,-.25,.25)`. The joint
covariance is SPD with minimum eigenvalue .75. For intercept plus
`t=(-2,-1,1,2)`, true slope cross-covariance is **.015**; expected pooled
residual cross-covariance is **-.075**, so a pooled `Sigma ⊗ G` surrogate gives
**-.0075**, the wrong sign. GeneralJoint is refused rather than running that
surrogate. This is an independent analytic counterexample, not new calibration.

Independent final source review accepted the exact joint-law/origin/df-role
source without remaining blockers; see `independent-review.md`.

## Final execution

Parent-coordinated exact-source owning-module gates:

| Module | JVM passed | JS passed | Declared skips |
| --- | ---: | ---: | --- |
| `mvpa` | 454 | 454 | One opt-in campaign on each platform |
| `mvpaSpatial` | 24 | 24 | None |
| `mvpaGroup` | 8 | 8 | None |
| `group` | 75 | 74 | None |
| `estimates` | 11 | 11 | None |
| `threshold` | 90 | 90 | None |
| **Total** | **662** | **661** | **Two opt-in skips** |

All commands exited 0 without compiler warnings or test errors. The group
bridge tests exercise the actual adopted voxel-loading producer and group
engine; the full core suites also gate the mandatory origin/df-role migration.
Independent base-R mean/covariance fixtures and the SPD nonseparable refusal
counterexample are retained with the source manifest. No optional campaign
result is counted as a pass or release qualification.

The [integration receipt](integration-gates.json) records every exact command,
module count and 17 source hashes. Its SHA-256 is
`664edf5ea71b6997ef436c5f161401953d3d0c02e28e42cdb49e954d6ea6b68b`.
Portable raw logs are [full JVM](logs/full-jvm.log) and
[full JS](logs/full-js.log), hashed in both receipts. This module manifest
also binds the independent R fixtures, final source review and API migration.
The original receipt is preserved as `integration-gates-pre-timeout.json`.
One later test-only opt-in timeout amendment changed `RankConfirmationSuite`;
fresh full MVPA JVM/JS runs passed 454 tests plus the declared skip each, with
zero errors/warnings. Their [additive raw log](../umvpa-inference-calibration-20261007/logs/post-timeout-mvpa.log)
and exact amended source hash are bound by `post_gate_amendment`. The other
five module sources and their original successful logs remain unchanged.
Remaining scientific consumers are M5.03 summaries and the
M5.04/M4.09 qualification programme; no universal estimated-SE default or joint
group inference is claimed by this adapter.
