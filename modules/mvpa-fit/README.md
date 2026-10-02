# scalafim-fmri-mvpa-fit

Shared JVM/Scala.js composition between first-level fMRI trial readouts and the
portable MVPA operator boundary.

`IdentifiedReadoutRelations.withRuns` binds run partitions, effect and neural
axes to actual `RunReadoutRelation` providers. For each run, it composes the
prepared `TrialReadout` with the response operator directly as a typed relation
without materializing trial-by-feature beta patterns. Metadata compatibility is
checked before acquisition. Explicit scoped providers guard forward and adjoint
applications, expire before closing, and preserve acquisition/preparation
support. A one-shot declaration cannot authorize repeated relational evaluation.

Use the shared `RelationRdm` and `RelationConsumers` for signed geometry and
RSA. Pairing and metric admission are explicit; temporal nuisance belongs to
the prepared readout, and RSA model controls remain separate comparison inputs.
The independent `BetaFreeRsaAcceptanceSuite` checks a three-run literal response
fixture through this native route on JVM and Scala.js.

`OneShotDataset`, `OneShotMvpaTask` and `OneShotMvpaEngine` are temporary boundaries
for generic adapters scheduled for removal in M3.13. Canonical consumers now
use identified run evidence and typed global artifacts.

Predictive operator ridge and soft LDA now run through `AlderOperatorRidge`
and `AlderSoftLda` in [mvpa-dataset](../mvpa-dataset/README.md). They retain the
single-fit `OperatorRidge.fit` and `SoftLda.fit` numerical kernels. Native
validation supplies identified train/test rows, hard or simplex membership,
fit audits and convergence/operator receipts. Soft LDA keeps component policy
and trial nuisance explicit and fits nuisance scope using training rows.

The earlier predictive CV analyses and universal predictive payloads were
removed in M1.12. Relational operator analyses and their universal payloads
were removed in M2.09; remaining generic one-shot boundaries expire in M3.13.

## Canonical global artifacts

`CanonicalRunEvidence.fromObservations` binds each run's actual time and neural
axes, identified observations, temporal geometry schedule and explicit scoped
provider. `CanonicalRunSet.make` binds their exact order to a run partition
axis. `ObservationProduct` owns provider acquisition and expiration.

`CanonicalGlobal.fit` returns a `CanonicalArtifact[N, C]`: its neural domain
remains nominally `N`, while fitted component coordinates are existential.
`fitNonnegative` and `fitManova` return the corresponding constrained and
spectrum artifacts. They retain the upstream functional frame, regularization,
operator program, diagnostics, training runs, temporal receipts and actual
moment content identity. These are descriptive training fits.

`assess`, `assessNonnegative`, `assessManova` and `assessSigned` return separate
leave-one-run-out assessments. Every geometry schedule is resolved before the
first response access. Each training fit is frozen before its held-out response
is read. Contrast methods retain `Z'Z`, `Z'X`, contrast variance and temporal
residual moments; they do not substitute a relational noise query for the
first-level residual SSCP. MANOVA evaluates the held-out spectrum inside the
frozen training frame. Signed assessments preserve negative cross-run quotients
and the explicit run-wise sign-flip exchangeability declaration.

`NonnegativeCanonicalModelSpec` fixes residual regularization and solver policy
before folds. Its coordinate cone is part of the estimand: permutations preserve
the result, while arbitrary rotations need not. The upstream fit retains its
stationary-point and feasibility diagnostics; no global optimality is added.

The earlier canonical datasets, feature-set scanners and parallel ROI summaries
were removed in M3.01. The existing independent R fixture suites now exercise
these native artifacts, including scale/basis laws, held-out perturbations,
geometry scopes, signed reversal and degeneracy.

These canonical adapters explicitly materialize responses and neural-square
moments. Capacity and local numeric-storage admission precede provider reads.
The backend workspace remains an explicit unknown unless the caller supplies a
provider declaration; `WholeNumeric` requires known source and backend costs.
The local storage receipt is not process peak-memory or whole-brain feasibility
evidence. General eigensolvers and constrained optimization remain in pinned
Multivar/Gale.

Current migration evidence: [M3.01 native global/canonical verification](../../docs/verification/umvpa-global-canonical-20261001.md).
