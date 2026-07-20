# Paired Latent Backbone

`multivar` should represent PLSC, regularized CCA, and reduced-rank regression
as one paired latent algebra rather than three unrelated estimator surfaces.
The common object is a row-aligned pair of observed spaces factored through a
shared latent space, with method-specific operator construction and
method-specific interpretation.

The goal is to make the mathematical family explicit:

> paired observed spaces + sample alignment + metrics/regularization + operator
> builder => typed maps into a latent space, and sometimes a directed map
> between observed spaces.

This keeps the public API inspectable, typed, cross-platform, and compatible
with the existing `MatrixView`, `DualityDiagram`, `MatrixMap`, and
`CrossProjection` backbone.

## Goals

- Put paired-table latent methods on a shared algebraic foundation.
- Preserve the current PLSC and CCA public behavior while factoring out the
  common map/projection construction.
- Add reduced-rank regression as a directed extension of the same paired
  backbone, not as a separate regression subsystem.
- Keep shared code portable across JVM and Scala.js.
- Keep linear algebra solver capabilities in `linalg`; `multivar` should adapt
  solver errors at the boundary, not define private decomposition engines.
- Make regularization typed and explicit instead of passing raw doubles through
  the API.

## Non-Goals

- Do not add formula parsing, dataset metadata, image/ROI adapters, or
  scheduler-specific execution to `multivar`.
- Do not force paired methods into the current single-input `MultivarPlan`.
- Do not expose Breeze, native libraries, or JVM-only solver types in shared
  APIs.
- Do not transliterate R list/S3 estimator surfaces.

## Core Types

### PairedDualityDiagram

Add a validated paired-data construction beside `DualityDiagram`:

```scala
final case class PairedDualityDiagram private (
    x: DualityDiagram,
    y: DualityDiagram,
    sampleSpace: MvSpace
)
```

The smart constructor should enforce:

- `x.rows == y.rows`
- both tables have positive observed dimensions
- row metrics are either both absent or compatible with the same sample space
- observed spaces are distinct and have `SpaceRole.Observed`
- the shared sample space has `SpaceRole.Samples`

This gives PLSC, CCA, and RRR one place to validate paired sample geometry,
future sample weights, and row metrics.

### PairedLatentMethod

Represent method choice as an ADT:

```scala
enum PairedLatentMethod:
  case Plsc
  case Cca(regularization: CcaRegularization)
  case ReducedRankRegression(
      direction: RegressionDirection,
      regularization: RegressionRegularization
  )
```

`RegressionDirection` should be explicit even if the first implementation only
supports `XToY`.

### Typed Regularization

Regularization should be structural:

```scala
final case class CcaRegularization(x: scalafim.linalg.Ridge, y: scalafim.linalg.Ridge)

enum RegressionRegularization:
  case Ols
  case Ridge(value: scalafim.linalg.Ridge)
```

Keep compatibility helpers that accept `Double`, but normalize them into these
types immediately.

### PairedLatentFit

The common fit artifact should be centered on `CrossProjection`:

```scala
final case class PairedLatentFit(
    method: PairedLatentMethod,
    xWeights: DoubleMatrix,
    yWeights: DoubleMatrix,
    singularValues: DoubleVector,
    projection: CrossProjection,
    diagnostics: ProjectionDiagnostics,
    rawDecomposition: Option[SvdResult] = None
)
```

PLSC and CCA can return thin wrappers over this fit for source compatibility.
Their compatibility `result` values are the raw operator SVDs. RRR should carry
additional directed prediction structure and must not expose its fitted-response
SVD as a generic paired-latent result.

### ReducedRankRegressionFit

RRR is a paired latent fit plus a directed low-rank map:

```scala
final case class ReducedRankRegressionFit(
    latent: PairedLatentFit,
    fullCoefficient: DoubleMatrix,
    workingCoefficient: MatrixMap,
    responsePreprocessor: FittedPreprocessor
)
```

`workingCoefficient` maps preprocessed predictor coordinates to preprocessed
response coordinates. Prediction on the original response scale should be an
explicit method that applies `responsePreprocessor.inverseTransform`.

## Operator Semantics

All three methods should follow this template:

1. Validate and preprocess `x` and `y`.
2. Build a method-specific operator.
3. Compute a rank-limited SVD with an explicit solver capability.
4. Convert factors into typed `MatrixMap`s.
5. Return a `CrossProjection` plus diagnostics.

### PLSC

PLSC builds the covariance operator:

```text
Cxy = X' Y / max(1, n - 1)
Cxy = U D V'
```

Maps:

- `xMap: X -> latent` uses `U`
- `yMap: Y -> latent` uses `V`

Interpretation: shared covariance modes.

### Regularized CCA

Regularized CCA builds a whitened covariance operator:

```text
Cxx = X' X / max(1, n - 1) + lambdaX I
Cyy = Y' Y / max(1, n - 1) + lambdaY I
Cxy = X' Y / max(1, n - 1)
K   = Cxx^-1/2 Cxy Cyy^-1/2
K   = U D V'
```

Maps:

- `xMap: X -> latent` uses `Cxx^-1/2 U`
- `yMap: Y -> latent` uses `Cyy^-1/2 V`

Interpretation: regularized canonical correlations.

The existing raw `ridge: Double` CCA entry point can remain as a convenience
wrapper that constructs `CcaRegularization`.

### Reduced-Rank Regression

RRR builds a directed prediction operator:

```text
Bfull = argmin_B ||Y - X B||^2
Yhat  = X Bfull
Yhat  = U D V'
Brrr  = Bfull V_k V_k'
```

Maps:

- `xMap: X -> latent` uses `Bfull V_k`
- `yMap: Y -> latent` uses `V_k`
- `workingCoefficient: X -> Y` uses `Brrr`

Interpretation: rank-constrained prediction from `X` to `Y`.

Use `linalg` QR/least-squares for the OLS path and `linalg.GramProjection` or a
dedicated `linalg` ridge least-squares capability for the ridge path.

## Implementation Plan

1. Add `PairedDualityDiagram`.
   - Put paired row validation in one smart constructor.
   - Add shared tests for row mismatch, metric compatibility, and stable space
     construction.

2. Add paired latent method/result ADTs.
   - Introduce `PairedLatentMethod`, `CcaRegularization`,
     `RegressionDirection`, `RegressionRegularization`, and `PairedLatentFit`.
   - Keep values small, immutable, and serializable.

3. Extract a paired latent projection builder.
   - Factor the repeated latent-space, `MatrixMap`, scores, scale, and
     diagnostics construction out of PLSC/CCA.
   - Keep the builder internal to `multivar` until the shape settles.

4. Refactor `Plsc.fit`.
   - Preserve existing public API and tests.
   - Route through `PairedDualityDiagram` and the shared builder.
   - Add a regression test proving byte-level public shape is unchanged where
     practical.

5. Refactor `Cca.fit`.
   - Normalize raw ridge values into `CcaRegularization`.
   - Route through the same paired builder.
   - Add tests for asymmetric regularization and invalid ridge rejection.

6. Add `ReducedRankRegression.fit`.
   - Start with `XToY`, OLS, dense working matrices, and portable solvers.
   - Reject `YToX` plans and direct fits until the swapped-direction executor is
     implemented.
   - Return `ReducedRankRegressionFit`.
   - Expose `predictWorking` and `predict` methods to distinguish response
     working coordinates from original response scale.

7. Add synthetic and metamorphic tests.
   - PLSC: cross-covariance SVD equivalence and sign-insensitive loadings.
   - CCA: perfect-correlation recovery, asymmetric ridge behavior, and
     singular covariance stabilization.
   - RRR: exact low-rank recovery, full-rank equivalence to OLS, rank-too-large
     rejection, row mismatch rejection, and original-scale prediction after
     inverse response preprocessing.
   - Paired backbone: common sample-space validation shared by all methods.

8. Add the paired plan boundary without executor integration.
   - Add `PairedMultivarPlan` instead of overloading the current ROI-local
     `MultivarPlan`.
   - The initial paired plan serves PLSC, CCA, and RRR uniformly over whole
     X/Y input pairs only.
   - ROI-by-ROI `X` with global `Y` and paired ROI-set execution remain future
     executor/adapter designs.

## Acceptance Criteria

- `Plsc.fit` and `Cca.fit` still compile and preserve their current public
  entry points.
- RRR is represented as a low-rank directed map plus the same paired latent fit
  structure used by PLSC and CCA.
- All shared tests pass on both JVM and Scala.js:

```text
sbt "multivarJVM/test" "multivarJS/test"
```

- No JVM-only dependency appears in `modules/multivar/shared`.
- Solver capabilities remain owned by `linalg`.
- `docs/module-relations.md` is updated if new public types materially change
  the module description.

## Open Design Checks

- Whether `PairedDualityDiagram` should require identical row metrics or allow
  one missing row metric to be lifted to identity.
- Whether RRR ridge belongs in existing `GramProjection` or needs a named
  `LeastSquaresSolver` trait in `linalg`.
- Whether `CrossProjection.transfer` is semantically too symmetric for RRR and
  should remain secondary to the explicit directed prediction artifact.
- Initial paired plan execution scope is whole-input X/Y pairs only. ROI-by-ROI
  `X` with global `Y` and paired ROI sets are explicitly deferred until a
  paired executor is designed.
