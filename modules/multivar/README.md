# scalafim-multivar

Typed duality diagrams and multivariate analysis for ScalaFIM. The semantic
core is the directed cycle `C --R--> C* --X--> O --A--> O* --X*--> C`:
nominal spaces, primal/dual orientation, form roles, evidence, centering,
singularity policy, and provenance are part of the analysis object rather than
loose matrix arguments.

The constitutional invariants are in
[`docs/plans/multivar-duality-constitution.md`](../../docs/plans/multivar-duality-constitution.md).
The implemented single-layer architecture, evidence-transition rules, and
result-equivalence vocabulary are in
[`docs/plans/multivar-operator-core.md`](../../docs/plans/multivar-operator-core.md).
All statistical methods execute on the typed operator/program substrate. There
is no legacy diagram, metric, map/projection, GPCA, paired-GMD, or CPCA engine
behind the semantic API.

This module owns the portable algebra below MVPA and neuroimaging adapters:

- nominal semantic spaces plus distinct primal and dual coordinates;
- directed, composable linear maps and role-specific forms with value-bound
  numerical certificates;
- ordered feature/sample index sets;
- disjoint block partitions and matrix-free direct sums;
- matrix-view contracts over dense, sparse, and lazy operator-backed inputs;
- immutable semantic duality diagrams carrying row measure, row/column forms,
  centering evidence, singular policies, certificate effects, and provenance;
- generalized PCA as one generalized Rayleigh--Ritz program whose fitted
  `FunctionalFrame` derives scores and axes, with clustered-spectrum and
  normalization diagnostics;
- explicit exact/partial row maps, incidence and aggregation maps, couplings,
  signed row links, common-entity hub alignment, and relationship support;
- nominal multiset association, disagreement, and hard/soft constraint
  objectives compiled through direct-sum operators and a separate block design;
- one typed `PairedOperatorProblem` for PLSC, regularized CCA, and reduced-rank
  regression: cross and marginal statistics arise through `secondOrder`, PLSC
  and CCA differ by normalization geometry, and fits expose two
  `FunctionalFrame`s plus the common `OperatorProgramFit` result contract;
- CPCA as one `CpcaOperatorProblem` over a typed table, row relationship,
  feature covariance, and row/feature constraint operators; each nonzero block
  exposes one feature `FunctionalFrame`, derived row scores, and an
  `OperatorProgramFit`, while
  unresolved numeric constraint specs remain available for ROI planning;
- row-whitening/projector geometry for design-conditioned effect operators,
  kept separate from row bilinear geometry and connected explicitly through
  the induced `D = W' W` metric when design-conditioned GPCA is wanted;
- shared error ADTs for estimation, preprocessing, and operator layers;
- pure `MultivarPlan` / `FitArtifactShape` descriptions for sample-by-feature
  ROI execution, including diagram-backed GPCA.
- pure whole-input `PairedMultivarPlan` descriptions for paired latent
  analyses; ROI-by-ROI paired execution is deliberately deferred to a later
  adapter/executor boundary.

`multivar-ir` serializes the semantic graph—space identities, orientation,
forms, certificates, scale/gauge, centering, singular policy, alignments,
objectives, unsafe assumptions, and payload hashes—for cross-language
conformance.

## API boundary

GPCA code constructs a `SemanticDualityDiagram` and calls `SemanticGpca.fit`,
or constructs `GpcaProblem` directly from frozen table and geometry operators.
Dynamic ROI planning freezes its runtime spaces and `MetricSpec` lifecycle
inputs immediately into the same `Op` graph. `MetricSpec` is a validated
construction specification; it is not a second semantic metric representation.
Unsafe evidence assumptions remain explicit through
`Unsafe.assumeSymmetric/assumePsd/assumeSpd` and retain their reason in
provenance.

Typed paired code should construct `PairedOperatorProblem.fromTables`, supplying
the two self row geometries and the directed cross-row relationship explicitly.
The `Plsc`, `Cca`, and `ReducedRankRegression` matrix entry points are lifecycle
conveniences: after preprocessing they construct that typed problem and return
typed fitted frame or coefficient transforms derived from its operator fit.
The RRR coefficient is a directed `OpCoefficient`; prediction is exposed by a
`FittedCoefficientTransform` rather than a separate decoder hierarchy.

CPCA code constructs `CpcaOperatorProblem` and fits a validated
`CpcaBlockRequest`. Planned ROI execution constructs the same typed problem
directly and carries `PreparedCpcaOperatorFit`; no raw CPCA problem or resolved
map constraint layer exists.

`multivar` depends only on `linalg`. Keep dataset, image, MVPA adapter, JVM
solver backend, and scheduler-specific code in higher modules.

## Neuroimaging boundary

Neuroimaging modules should translate their own objects into pure multivar
plans instead of pulling dataset/image/runtime types into this module.

- `mvpa` pattern sources map naturally to `SampleByFeatureInput` plus a
  `RoiPlanSet`.
- `dataset` and ROI adapters should carry only serializable source references
  such as `MultivarSourceRef.DatasetSelection(...)` at this layer.
- CPCA formula interfaces, sample/feature metadata encoders, and model-matrix
  builders live in design, MVPA, dataset, or user-facing adapter modules.
  `multivar` accepts numeric `CpcaConstraint` values and resolves them
  only after the concrete row/ROI spaces and metrics are known.
- `LocalMultivarExecutor` is the reference interpreter for ROI/block execution.
  A later JVM adapter can partition by ROI and broadcast small fitted maps using
  the same `MultivarPlan` and `FitArtifactShape` values, without changing the
  shared algebra.

## Release evidence

The shared test suite covers the current core invariants on both JVM and JS:

- typed ids, dimensions, index sets, and complete disjoint block partitions;
- dense, sparse, and affine `MatrixView` algebra without implicit sparse
  densification, including lazy transposed views for duality symmetry;
- preprocessing, typed operator composition, fitted-transform, and
  coefficient-orientation boundaries;
- SVD/PCA plus operator-program PLSC/regularized CCA/reduced-rank regression,
  including typed partial row relationships, generalized cross-SVD residuals,
  row-permutation laws, directed coefficient orientation, and unchanged R
  parity fixtures;
- row/column geometry and GPCA, including dense, diagonal, sparse-preserving,
  rank-deficient PSD preparation, and R-reference-backed paths;
- CPCA identity/zero/basis constraint specs, typed projector orientation,
  independent `X* A X` and projected-block oracles, ROI-local operator-plan
  execution, sparse materialization rejection, generic program/result
  semantics, diagonal-metric whitening, four-block orthogonality,
  reconstruction, partition inertia, and metric-orthonormal factors;
- semantic-diagram construction, algebraic dual invariants, metric
  self-adjointness, centered/support-restricted preparation, clustered-spectrum
  evidence, and backend/policy diagnostics;
- centering projection laws, certificate invalidation, and explicit support,
  quotient, regularization, or rejection policies for singular geometry;
- exact, partial, coupling, signed-link, and hub-factorized alignment laws,
  including unmatched support and global PSD construction;
- direct-sum association/agreement/constraint compilation with block design
  kept separate from row correspondence;
- typed multiblock partitions, lifted block frames, and direct-sum operators;
- row-side whitening/projector/effect-operator algebra matching the
  multivarious fixed-effect projector form, including equivalence between
  whitening-then-PCA and GPCA under the induced row metric;
- kernel and Nyström artifacts, including out-of-sample projection;
- pure ROI/sample-by-feature `MultivarPlan` execution that stays independent of
  MVPA, dataset, image IO, concrete schedulers, and JVM-only numeric libraries.
- pure whole-input `PairedMultivarPlan` validation for paired latent analyses,
  kept separate from the ROI-local `MultivarPlan` executor path.
