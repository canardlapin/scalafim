# Multivar duality constitution

Status: implemented and release-gate verified (2026-07-17).

This document is the architectural constitution for `multivar` and
`multivar-ir`. It records which distinctions are structural, which numerical
representations may vary, how legacy APIs migrate, and where future algorithms
must attach. The governing principle is that the duality diagram is executable
semantics, not explanatory decoration.

## Semantic spine

For nominal row and column spaces `O` and `C`, the diagram is

```text
             R
       C ----------> C*
       ^              |
       | X*           | X
       |              v
       O* <---------- O
             A
```

with

```text
X  : C* -> O       data table
R  : C  -> C*      column form
A  : O  -> O*      row form
X* : O* -> C       algebraic dual of X
```

The induced operators are `K_C = X* A X R` and `K_O = X R X* A`.
This orientation determines the Scala types, runtime descriptors, serialized
IR, multiplication rules, and law tests. Equal dimensions never establish
space identity, and a primal vector is never interchangeable with a covector.

The public semantic graph is rooted at `SemanticDualityDiagram`. The older
`DualityDiagram` and raw `MvMetric` entry points remain compatibility layers;
they are not the destination for new API design.

## Constitutional invariants

1. A portable guarantee exists as a Scala distinction where practicable, a
   runtime descriptor or certificate, and a language-neutral IR field.
2. Statistical algorithms consume semantic objects. Raw arrays cross a public
   boundary only through a visibly named `Unsafe` constructor with a reason.
3. Space identity is nominal and dimensions are runtime-validated. Row
   correspondence requires evidence even when row counts agree.
4. `Primal[S]` and `Dual[S]` are different coordinates. `Lin[From, To]`
   composition and `star` preserve orientation.
5. Numerical representation does not determine semantic role. Metric,
   cometric, penalty, kernel, covariance, precision, and row link are nominally
   distinct even when backed by identical arrays.
6. Row measure, row geometry, column transformation, and column geometry are
   separate values. A relationship between them is made by a named derivation.
7. Centering is an evidenced projection, not a Boolean flag.
8. Singular geometry requires an explicit policy and never silently invokes a
   pseudoinverse.
9. A diagram is immutable. Transformations return a new diagram, provenance,
   audit record, and certificate-validity effects.
10. Algorithms request operator and solver capabilities; storage class is not
    an algorithmic identity and missing capabilities never justify accidental
    materialization.
11. Objective identity is nominal. Association, disagreement penalties, and
    hard equality constraints may share kernels but are different problems.
12. Result comparison follows the identifiable object: signs, frames,
    clustered eigenspaces, and subspaces have different equivalence laws.

## Four implementation layers

| Layer | Owns | Does not own |
| --- | --- | --- |
| Typed linear algebra | nominal spaces, primal/dual coordinates, directed maps, form roles, measures, certificates, operator composition | backend-specific statistical policy |
| Duality diagrams | immutable diagrams, centering, singular policies, support restriction, GPCA semantics and laws | row matching inferred from position |
| Row relationships | exact/partial maps, incidence and aggregation, couplings, signed links, hub alignment, support and marginals | block-design coefficients or statistical objective choice |
| Multiset spectral problems | direct sums, block design, row/objective forms, constraints, association fits and diagnostics | IO, dataset adapters, Python/R runtimes |

`linalg` owns portable numerical operators, factorization contracts, and
reference solvers. `multivar` owns statistical meaning. `multivar-ir` owns the
wire contract. Higher modules own neuroimaging adaptation and execution.

## Safe and unsafe boundaries

Safe constructors validate endpoint identities, orientation, dimensions,
roles, numerical claims, centering evidence, and relationship semantics.
Certificate-bearing forms bind the certificate to the immutable operator value
identity that was tested.

`Unsafe` is an explicit compatibility and trust boundary:

- `Unsafe.genPcaFromArrays` admits anonymous row/column spaces and requires a
  non-empty reason;
- `Unsafe.pairedDiagramFromArrays` admits positional row identity and requires
  a non-empty reason;
- `Unsafe.assumeSymmetric`, `assumePsd`, and `assumeSpd` retain the assumption
  and reason in provenance.

The raw `GenPca.fit(MatrixView, ...)` overload is deprecated and delegates to
`Unsafe.genPcaFromArrays`. Existing sparse, affine, backend, and
`StoragePolicy` behavior is preserved. Paired legacy estimators route their
positional assumption through `Unsafe.pairedDiagramFromArrays`.

Unsafe is not an error-suppression switch: invalid dimensions and numerical
failures still return typed errors. It only names evidence the caller chose not
to establish.

## Forms, evidence, and certificate lifecycle

`Form` separates role, structure, positivity, scale semantics, operator, and
evidence. Certified facts include their residual norm, absolute and relative
tolerances, numerical method, precision, backend, regularization, and immutable
value identity. Assumed facts have different evidence types and provenance.

Repairs create new values. Support restriction and ridge regularization do not
mutate a form or reuse a certificate for a changed operator. Diagram
transformations record certificate effects explicitly:

- changing centering invalidates prepared-table evidence;
- changing row geometry invalidates row-geometry and metric-orthogonal
  centering evidence;
- changing column geometry invalidates column-geometry evidence;
- unchanged, value-bound certificates may be preserved only when the operation
  proves that preservation.

Metric scale is serialized as absolute, normalized, or shape/gauge semantics.
The IR can preserve a gauge identity now; joint robust shape estimators and
scale-sensitive gauge reconciliation remain deferred.

## Centering and singular geometry

`RowMeasure` is a normalized functional independent of the row form.
`CenterByMeasure`, `CenterOrthogonally`, `NoCentering`, and
`AlreadyCenteredBy` are distinct plans. Their certificates test idempotence,
annihilation of the constant direction, annihilation by the measure, and—when
claimed—metric self-adjointness.

The available singular policies are:

- `RejectSingularGeometry`;
- `RestrictToSupport(threshold)`;
- `WorkInQuotientSpace(threshold)`;
- `RegularizeWith(amount)`.

Support restriction returns a nominal effective space, embedding, reduced SPD
form, discarded nullity, rank certificate, and threshold. No pseudoinverse is
selected implicitly.

## Generalized PCA result semantics

GPCA is one dual singular system, not two independently normalized
eigendecompositions. `SemanticGenPca.fit` returns both sides and checks the
transport and metric-orthonormality laws with explicit Frobenius residuals and
absolute/relative tolerances.

The semantic result names are:

| Meaning | Semantic accessor | Legacy accessor |
| --- | --- | --- |
| standard row axes `U`, `U* A U = I` | `standardRowScores.values` | `ou` |
| principal row scores `X R V = U Sigma` | `principalRowScores.values` | `projection.scores` |
| column axes `V`, `V* R V = I` | `columnAxes.values` | `ov` |
| metric loadings `R V` | `columnMetricLoadings.values` | `v` |
| row metric loadings `A U` | `rowMetricLoadings.values` | `u` |
| row-dual principal scores `A U Sigma` | `rowDualPrincipalScores.values` | `metricScores` |
| singular/generalized eigenvalues | `spectrum` | `d` and `d^2` |

Repeated or numerically clustered eigenvalues identify a subspace, not
individual columns. `GenPcaSpectrum.clusters` reports the identifiable
partition. Law and covariance tests compare appropriate subspaces/projectors,
not arbitrary column signs.

Weighted reconstruction uses
`tr((X - Y)* A (X - Y) R)` and is tested against the discarded generalized
eigenvalue mass and alternative rank-constrained approximations. Implementations
may choose a row problem, column problem, or weighted SVD according to available
capabilities while reporting the same semantic result.

## Row relationships and partial alignment

Row position never establishes identity. The nominal relationship families are
`ExactBijection`, `PartialInjection`, `IncidenceMap`, `AggregationMap`,
`NonnegativeCoupling`, `ProbabilisticCoupling`, `SignedRowLink`, and explicit
same-entity evidence. Support, unmatched mass, multiplicity, normalization,
marginals, and observed/external/estimated origin are runtime semantics.

The canonical coherent construction factors through a nominal entity space
`E`:

```text
P_s : O_s -> E
L_st = P_s* A_E P_t : O_t -> O_s*
```

`HubAlignment` and `EntityAlignedStudy` derive adjoint-consistent links and a
PSD global direct-sum row form when `A_E` is PSD. Missing cells, missing views,
unmatched rows, uncertain matches, excluded relationships, and structural zeros
remain distinct conditions. Partiality belongs to relationship support, never
to padded zero observations.

`BlockDesign` answers which views interact and with what coefficient. A row
relationship answers which observations interact. The two cannot coerce into
one another.

## Direct sums, objectives, and constraints

`DirectSumStudy` constructs block-diagonal `X_oplus` and `R_oplus` without
requiring dense storage. A row-side block operator compiles independent,
same-row, hub-aligned, or pairwise relationships. `LinearMap.blockMatrix` and
`LinearMap.scale` keep this compilation matrix-free when the constituent
operators are matrix-free.

The following remain nominally distinct:

- `MaximizeAssociation`;
- `PenalizeDisagreement`;
- `ConstrainEqualScores`;
- `HardScoreConstraint` and `BoundedScoreConstraint`.

Every fitted objective records its exact formula, normalization, transformed
solver formulation, and relationship between solved and reported objectives.
Symmetric indefinite association forms are objective forms, never mislabeled
row metrics. Optional marginal-, cycle-, hub-, and global-PSD certificates state
exactly which coherence was established.

## Language-neutral IR

`multivar-ir` version 0.1 carries the same object graph across Scala, Python,
and R boundaries: nominal space IDs, primal/dual endpoints, operator roles and
representations, form structure/positivity/scale/gauge, measures, centering,
singular policies, relationships and support/marginals, constraints/objectives,
certificates, unsafe assumptions, and provenance.

The decoder rejects unknown fields, endpoint mismatches, unsupported
singularity, role coercions, uncertified positivity, and payload tampering with
stable cross-binding categories. Numeric payloads have canonical SHA-256
identities whether inline or external. See
`modules/multivar-ir/SCHEMA.md` and its conformance corpus.

## Migration guide

| Legacy usage | Preferred usage |
| --- | --- |
| `GenPca.fit(x, rowMetric, colMetric, ...)` | construct `SemanticDualityDiagram`, call `SemanticGenPca.fit`; use `Unsafe.genPcaFromArrays(..., reason)` only at a compatibility boundary |
| separately passed table and metrics | one immutable semantic diagram with measure, centering, policies, provenance, and forms |
| infer same rows from equal counts | construct `SameEntityEvidence`, a typed row map/link, or a `HubAlignment` |
| treat any SPD array as a weight | choose a named form constructor and provide/derive its certificate |
| `centered = true` | `AlreadyCenteredBy(functional, certificate)` |
| silent pseudoinverse | select a `SingularGeometryPolicy` |
| compare component columns directly | inspect spectral clusters and compare signs, frames, or subspaces as scientifically identifiable |
| use a connection matrix for both design and matching | keep `BlockDesign` separate from row relationships |
| serialize backend arrays | encode `multivar-ir` semantics and content-addressed payloads |

## Numerical and storage contract

- Shared kernels remain portable across JVM and Scala.js.
- Sparse, diagonal, low-rank, block, affine, and matrix-free operators are
  represented by capabilities; public algorithms do not demand dense matrices.
- `StoragePolicy.PreserveSparse` rejects operations that would densify.
- There is no fallback that materializes an operator merely because an
  optimized capability is missing.
- Residual assertions name the norm and use explicit absolute plus relative
  tolerances. R parity fixtures remain independent numerical anchors where R
  behavior is part of the contract.

## Deliberately deferred extensions

These fit the constitution but are not claimed by the current implementation:

- learned alignment estimators, uncertainty models, and fold-safe train/test
  application;
- robust joint shape estimation, fresh gauge identities, and explicit gauge
  reconciliation for scale-sensitive objectives;
- Krein/indefinite-geometry decompositions with result semantics distinct from
  metric GPCA;
- Python and R bindings that consume the 0.1 IR and reproduce its rejection
  categories;
- distributed execution and neuroimaging-specific adapters;
- additional multiset objectives such as trace ratio, ratio trace, GCCA, and
  co-inertia, each as a nominal problem that may compile to the direct-sum core
  only when mathematically exact;
- scalable iterative multiset solvers beyond the portable dense feature-space
  reference implementation.

These are extension points, not omissions to paper over with flags or
free-form method strings.

## Conformance gate

The release gate requires both JVM and Scala.js tests for `linalg`, `multivar`,
and `multivar-ir`, then repository-wide `compileAll` and `testAll`. It also
requires dependency scans confirming that shared multivar code imports no
Breeze, dataset, image, scheduler, or binding runtime; diff whitespace checks;
JSON Schema validation; and law/conformance evidence with named norms and
explicit absolute/relative tolerances.
