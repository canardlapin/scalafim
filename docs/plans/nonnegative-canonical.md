# Nonnegative coordinate-constrained canonical MVPA

## Frozen estimand

For training-run effect and residual moments \(H\) and \(E\), and a declared
residual regularization \(E_\lambda\), version one estimates

\[
  \hat w = \arg\max_{w \ge 0,\; w^\top E_\lambda w = 1}
    w^\top H w,
  \qquad
  \hat\rho = \frac{\hat w^\top H\hat w}
                    {\hat w^\top E_\lambda\hat w}.
\]

This is not an orientation convention for ordinary CCA. The nonnegative cone
is part of the scientific model: it removes sign symmetry, retains coordinate
permutation symmetry, and is not invariant to arbitrary feature rotations.
Consequently it is appropriate only when the chosen feature coordinates have a
meaning under nonnegative combination.

## Architectural lowering

`ConstrainedCanonicalProblem` is a separate typed problem. It constructs the
ordinary certified effect and regularized-residual operators, then declares a
`GeneralizedRayleigh` objective, its residual normalization, and a
`ConstraintTerm(NonnegativeOrthant)` in one `OperatorProgram`. The inferred
result semantics require a stationary point and coordinate-identified frame;
the API does not claim a global spectral optimum.

The numerical iteration belongs to Gale. `gale.optim.ProjectedRayleigh`
projects every iterate into the declared cone, normalizes directly in the
denominator geometry without forming an inverse, and returns separate KKT
stationarity, constraint-violation, and normalization certificates. ScalaFIM
validates those certificates before constructing `OperatorProgramFit`.

## Held-out fold contract

`NonnegativeCanonicalMvpa` consumes the existing `CanonicalEffectDataset` and
`PreparedContrastGeometry`. For a feature set and outer held-out run it:

1. resolves temporal preparation for the exact training-run scope;
2. accumulates only training-run `Z'Z` and `Z'X` sufficient statistics;
3. fits the constrained canonical frame;
4. accesses the held-out response for the first time;
5. evaluates the frozen direction's held-out effect/residual quotient.

Temporal nuisance remains owned by `PreparedContrastGeometry`. No trialwise
beta table, spatial nuisance regression, or time-by-time projector is inserted.
`NonnegativeCanonicalModelSpec` fixes residual regularization and solver policy
before the folds are constructed. Version one deliberately has no
response-selected tuning; a future tuned model must use nested training folds
and preserve the same access order.

## Evidence

The independent base-R generator enumerates every non-empty active coordinate
face. Within each face it solves the symmetric-definite generalized eigenproblem
using dense base-R `chol`, `solve`, and `eigen`, then selects the best feasible
root. Shared JVM/Scala.js tests compare the complete leave-one-run-out roots,
directions, ridge amounts, and held-out scores to that oracle. Additional laws
cover inactive constraints, coordinate permutations, rotation non-invariance,
typed invalid solver settings, held-out perturbation leakage, ordinary MVPA
summaries, and streaming traversal.
