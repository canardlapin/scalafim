# LowRankModels.jl differential oracle

This directory defines ScalaFIM's reproducible secondary oracle for generalized
low-rank models. It pins `LowRankModels.jl` 1.1.1 at commit
`a18f0df45f1a6ce37634bf4e347062b6090397eb`; generated fixtures must record that
commit, the Julia version, and the resolved package version.

Use Julia 1.6 through 1.10. The upstream DataFrames 0.21 dependency graph does
not resolve under Julia 1.12 because its SortingAlgorithms compatibility range
predates that runtime. This is an oracle-environment constraint, not a reason
to relax or patch the upstream dependency graph silently.

The oracle checks agreement between explicitly matched mathematical programs.
It is not a proof oracle, and agreement with it cannot upgrade a stationary or
unresolved ScalaFIM result to a global guarantee.

## Admitted semantic mapping

Let ScalaFIM store row codes as `U` with shape `m x k` and decoder coefficients
as `V` with shape `n x k` for scalar-width features. LowRankModels.jl stores
`X = U'` with shape `k x m` and `Y = V'` with shape `k x n`, and reconstructs
`X' * Y = U * V'`.

The first fixture version admits only these mappings:

| ScalaFIM | LowRankModels.jl | Required adaptation |
|---|---|---|
| `Quadratic` | `QuadLoss(0.5)` | Julia's unscaled loss is the full squared residual. |
| `Logistic` | `LogisticLoss(1.0)` | Scala values `0/1` map to Julia `Bool`; natural parameters agree. |
| `Poisson` | `PoissonLoss()` | Compare gradients directly; compare values after removing each implementation's response-only constant. |
| `CategoricalCrossEntropy` | `MultinomialLoss(levels)` | Scala level indices are zero-based and Julia levels are one-based; compare common-shift-invariant probabilities and centered natural parameters. |
| `ElementwiseL1` with weight `lambda` | `OneReg(lambda)` | No scale change. |
| `SquaredFrobenius` with weight `lambda` | `QuadReg(lambda / 2)` | ScalaFIM defines the functional as `0.5 * squaredNorm`. |

Only unit-weight point observations belong to the initial shared subset.
LowRankModels.jl has a declared observed-entry set, but no direct equivalent of
ScalaFIM's arbitrary per-cell observation weights, structural
inapplicability, or censoring states. Sparse implicit zeros must never be used
to encode observed zeros; fixtures carry an explicit mask.

Offsets, automatic scaling, probability scaling, and random initialization are
disabled. Both implementations receive explicit factors or initial factors.

## Deliberately excluded from version 1

- Huber loss: the pinned Julia implementation's value and gradient conventions
  do not define the same standard half-quadratic Huber function used by
  ScalaFIM.
- Ordinal losses: the packages expose different parameterizations and gauges;
  parity requires a separately reviewed mathematical map.
- hinge, periodic, quantile, one-vs-all, and bigger-vs-smaller losses: ScalaFIM
  does not currently expose the same estimands.
- nonnegative, simplex, and sparsity constraints: admit only after their
  proximal and feasibility conventions have independent analytic tests.
- literal factor equality after fitting: factors are non-identifiable under
  sign, permutation, gauge, and invertible latent-coordinate changes.

## Evidence hierarchy

1. Fixed-factor loss, gradient, penalty, reconstruction, and decoding fixtures.
2. Frozen-decoder convex row-code fits with objective and stationarity checks.
3. Full nonconvex fits compared by reconstruction, cross-evaluated objective,
   decoded predictions, stationarity residuals, and trajectories across named
   deterministic starts.

Every fitted comparison retains ScalaFIM's analytic and independent convex
oracles. LowRankModels.jl is always secondary evidence.

## Reproduction

Instantiate from this directory with an isolated depot and a supported Julia
runtime. `instantiate.jl` adds the git source explicitly because Julia 1.6 does
not interpret the newer `[sources]` table in `Project.toml`:

```sh
JULIA_DEPOT_PATH=/tmp/scalafim-lowrankmodels-depot \
  julia --project=. instantiate.jl

JULIA_DEPOT_PATH=/tmp/scalafim-lowrankmodels-depot \
  julia --project=. generate_fixtures.jl

JULIA_DEPOT_PATH=/tmp/scalafim-lowrankmodels-depot \
  julia --project=. generate_encoding_fixtures.jl

JULIA_DEPOT_PATH=/tmp/scalafim-lowrankmodels-depot \
  julia --project=. generate_fitted_fixtures.jl

python3 verify_fixtures.py
python3 render_scala_fixture.py
python3 verify_encoding_fixtures.py
python3 render_scala_encoding_fixture.py
python3 verify_fitted_fixtures.py
python3 render_scala_fitted_fixture.py
```

Commit `Manifest.toml` after successful resolution. Generated JSON must validate
against `fixture.schema.json` before shared JVM and Scala.js tests consume it.
`verify_fixtures.py` independently checks provenance, dimensions, reconstruction,
losses, gradients, decoders, mask behavior, penalty scaling, and objective
accounting; it does not replace execution of the pinned Julia generator.
`render_scala_fixture.py` verifies the JSON again and embeds its canonical form
in shared test source, avoiding JVM-only resource-loading APIs so the identical
external evidence runs on JVM and Scala.js. The encoding verifier and renderer
apply the same policy to `encoding-v1.json`, including independent objective,
proximal-residual, support, decoding, and monotone-trajectory checks. The fitted
pair verifies `fit!(GLRM, ProxGradParams)` across named starts, recomputes the
full objective and stationarity independently, and preserves the upstream
history-field limitation instead of using that field as a convergence claim.
