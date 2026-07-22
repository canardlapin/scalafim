# Fitted projection contract

Status: implemented executable specification; JVM/Scala.js parity and negative
contract suites are release gates.

This document fixes the mathematical meaning and lifecycle rules of projection
before the public capability APIs are added. The R package `multivarious` is a
parity source, not the type design: compatibility behavior is preserved under
named conventions, while geometry-general behavior remains explicit.

## Coordinates and fitted state

Let a fitted model own:

- an ordered feature identity space `F = (f_1, ..., f_p)`;
- a fitted preprocessing state `P_F`, including center and scale state;
- a working-coordinate frame `W: F -> K`, represented by a `p x k` matrix;
- an optional feature metric `Q` and row measure `D`;
- an optional synthesis map `B: K -> F`, represented by a `k x p` matrix;
- a model identity and revision.

Projection never refits preprocessing. Given an ordered restriction `S` of
distinct identities from `F`, `P_F[S]` applies exactly the stored state for
those identities, even when the supplied columns arrive in a different order.
Unknown, duplicate, or ambiguously identified features are rejected. Bare
positional projection is permitted only when the fitted model itself has an
explicit positional feature space.

All result values carry provenance sufficient to recover the fitted model,
input row identities, ordered feature restriction, component restriction,
preprocessing revision, projection convention, and any regularization or
solver certificate. Operations preserve input row order. Operations involving
training scores require exact row-identity alignment or an explicit checked
permutation; they never silently zip rows by position.

## Named analysis capabilities

The following operations are distinct capabilities and distinct result types.
They are not modes selected by a Boolean flag.

### Full score projection

For `X_raw: R x F`, let `X = P_F(X_raw)`. Then

```text
score(X_raw) = X W                         [n x p][p x k] -> [n x k]
```

The result is a row-score coordinate in `K`. It preserves row identities and
uses the complete fitted feature space.

### Partial feature contribution

For a restriction `S` and `X_S = P_F[S](X_raw[S])`:

```text
contribution_S(X_raw[S]) = X_S W_S         [n x |S|][|S| x k] -> [n x k]
```

This is an additive contribution, not an estimate of the complete latent
score. For a disjoint ordered partition `(S_1, ..., S_b)` of `F`:

```text
score(X_raw) = sum_j contribution_S_j(X_raw[S_j])
```

This law is the basis of fitted multiblock projection. A block projection is a
named block contribution with block identity and block-local preprocessing
provenance.

### Partial least-squares score recovery

Partial recovery estimates complete latent coordinates from observed features:

```text
C_S = X_S W_S
G_S = W_S^T Q_S W_S + lambda I
recover_S(X_raw[S]) = C_S solve(G_S)        [n x k][k x k] -> [n x k]
```

`Q_S` is the principal restriction of the fitted feature metric. Euclidean R
compatibility uses `Q_S = I`. Implementations solve the system and do not form
an inverse. `lambda` is a non-negative domain value. With `lambda = 0`, the
restricted Gram operator must have a certified usable rank; otherwise recovery
is rejected. A positive ridge may regularize a rank-deficient restriction, but
the result records the ridge and solver certificate.

Contribution and recovery coincide only under special frame geometry. Their
different semantics must remain visible in method names, result types, IR, and
provenance.

### Supplementary-variable coefficients

Supplementary variables share the fitted training rows, not the fitted feature
space. They produce variable-by-component coefficients (`q x k`), never row
scores. Two conventions are required.

`MultivariousCovarianceScaled` preserves the current R `project_vars` formula.
For centered supplementary variables `Y_c`, fitted training scores `S`, and
stored singular-value scales `d`:

```text
L_R = Y_c^T S diag(d^-2) / (n - 1)          [q x n][n x k] -> [q x k]
```

The supplementary variables are centered by their own column means. This is a
compatibility coefficient: with singular values in `d`, it is neither ordinary
least-squares regression nor a standardized correlation. The convention is
therefore named and is not the geometry-general default. `n <= 1` or
`d_j^2 <= tolerance` is rejected with a typed diagnostic.

`MetricLeastSquares` estimates coefficients in the fitted score geometry:

```text
L_D = Y_c^T D S solve(S^T D S + lambda I)   [q x n][n x k][k x k] -> [q x k]
```

The row measure, centering policy, ridge, and rank certificate are explicit.
Exact row identities are mandatory because `Y_c` is compared with training
scores.

## Named synthesis capabilities

Analysis and synthesis are separate. A frame `W` does not by itself imply that
`W^T` is a decoder. A fit exposes synthesis only when it owns an explicit
decoder `B`, derives a pseudoinverse under a named metric and tolerance, or has
a certificate that the relevant transpose is valid.

### Synthesis and reconstruction

For latent coordinates `Z`:

```text
synthesizeWorking(Z) = Z B                 [n x k][k x p] -> [n x p]
reconstruct(Z) = P_F^-1(Z B)               original feature coordinates
```

Component and target-feature restrictions select rows and columns of the same
fitted decoder. Partial reconstruction first uses an explicitly selected
contribution or recovery capability, then synthesizes requested target
features, and finally applies the target features' fitted inverse preprocessing.
The result records both scoring and synthesis conventions.

### Paired transfer

For paired domains `A` and `B`, transfer is composition rather than coefficient
reuse:

```text
transfer_A_to_B(X_A) = P_B^-1(score_A(X_A) B_B)
```

The source analysis convention and target decoder are independently named.
Source/target domains must differ. Component and target-feature restrictions
are checked against their own identities. Transfer preserves source row
identities and carries both fitted-domain revisions.

## Failure and boundary policy

- Shape errors, identity mismatches, duplicate restrictions, unavailable
  inverse preprocessing, and absent decoders are typed failures.
- Centering and scaling always come from fitted state, except the explicitly
  documented own-centering step for supplementary variables.
- Zero fitted scales are rejected when preprocessing is constructed; a selected
  null singular value is rejected by supplementary compatibility projection.
- Rank deficiency is never hidden by a pseudoinverse default. The implemented
  choices are an exact solve or a caller-selected ridge-regularized solve. A
  future pseudoinverse path would require its own named tolerance policy.
- Dense generalized metrics must agree with an independent dense oracle.
  Matrix-free metrics expose the same algebra and a solver certificate.
- Multiblock inputs retain block and feature identities. Independently fitted
  block preprocessing is reused; no global re-centering is introduced during
  projection.

## Parity fixture

`tools/r-parity/generate_multivar_projection_fixtures.R` is a base-R oracle
anchored to `multivarious` commit
`d44b7d0104a8647aefb61c3217c069c247a27b3d`. It emits the shared Scala fixture
used unchanged by JVM and Scala.js. The fixture covers full projection, partial
contribution, Euclidean and generalized-metric recovery, supplementary-variable
compatibility and metric least squares, block additivity, synthesis,
reconstruction, and paired transfer. The Scala contract suite recomputes the
formulas independently and checks metamorphic laws in addition to golden values.

## Implementation map and intentional limits

- `FittedFrameTransform` owns complete projection and produces
  `PartialScoreResult` through distinct contribution and least-squares entry
  points. Feature restrictions are identity-based and retain the fitted schema.
- `SupplementaryProjector` aligns exact training-row identities and returns a
  `SupplementaryFrame`; null components require an explicit reject, drop, or
  regularize policy.
- `FittedBidirectionalTransform` owns an explicit synthesis `Op`. Construction
  is explicit-decoder, orthonormal-transpose after a Gram check, or Euclidean
  least squares. Reconstruction supports component and target-feature
  restriction in working or original coordinates.
- `PairedTransfer` is intentionally limited to the PLSC and CCA estimands whose
  shared component coordinates have a defined transfer interpretation. It does
  not reinterpret reduced-rank regression coefficients as a decoder.
- `FittedMultiblockProjection` provides named block scores and weighted block
  contributions. It reuses the fitted block preprocessor and local frame, and
  records both the local and global frame identities.
- `ProgramProjectionIr` and `ProgramSynthesisCapabilityIr` durably encode the
  selected action, analysis/decoder identities, restrictions, metric or ridge,
  coordinate convention, result kind, equivalence, and provenance. The strict
  v0.2 codec rejects unknown action fields and incompatible operator ports.

There is no implicit synthesis from a frame, positional fallback for a named
schema, silent pseudoinverse for a singular partial Gram matrix, arbitrary
paired-estimand transfer, or whole-data refitting during projection. A custom
`FittedPreprocessor` that cannot honor inverse transformation makes original-
coordinate reconstruction fail through the typed boundary; working-coordinate
synthesis remains available.
