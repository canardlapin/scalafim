# Linear Algebra Backend Strategy

ScalaFIM needs numerical linear algebra that is portable across JVM and
Scala.js, but JVM users should still be able to use mature libraries such as
Breeze for larger dense problems. The project strategy is therefore:

> `linalg` owns the solver contracts and portable reference implementations;
> platform-specific libraries are optional adapters behind those contracts.

This is tracked in mote as epic `bd-01KX1P6Z3RW3T4GSZ4MT86Z7QQ`.

## Goals

- Keep shared scientific code cross-platform and deterministic.
- Avoid duplicating eigensolver, SVD, inverse, QR, and decomposition helpers in
  domain modules.
- Allow JVM-only acceleration without leaking Breeze or native types into
  shared APIs.
- Make solver choice an explicit typed capability, not string configuration.
- Preserve parity and synthetic tests on both JVM and Scala.js.

## Boundary

`modules/linalg/shared` is the home for:

- `DoubleMatrix`, `DoubleVector`, and row-major primitive storage.
- Validated numeric domain types such as square, symmetric, positive-definite,
  tolerance, and decomposition-rank types.
- Solver traits and result types for symmetric eigendecomposition, SVD,
  generalized eigendecomposition, QR/least-squares, Cholesky, SPD inverse, and
  related primitive operations.
- Portable reference implementations that work on JVM and Scala.js.

ScalaFIM domain modules such as `connectivity`, `fit`, `mvpa`, and `group`
depend on `linalg` capabilities. Standalone `multivar` depends directly on
Gale's portable matrix, operator, spectral, and first-order capabilities. No
domain module may define a private solver family unless the operation is
genuinely domain-specific and cannot live below it.

## Backend Shape

Shared code should depend on small traits:

```scala
trait SymmetricEigenSolver:
  def decompose(matrix: DoubleMatrix): Either[LinearAlgebraError, SymmetricEigenResult]

trait DenseSvdSolver:
  def decompose(matrix: DoubleMatrix, rank: DecompositionRank): Either[LinearAlgebraError, SvdResult]
```

Algorithms receive solvers as typed capabilities:

```scala
def fit(input: DoubleMatrix)(using eigen: SymmetricEigenSolver): Either[DomainError, Fit]
```

This is an appropriate use of `given`/`using`: the solver is a capability. It is
not a hidden global configuration object.

## Portable Reference Backends

The reference backend is pure Scala and lives in shared code. It should cover:

- Cholesky factorization and solves for positive-definite systems.
- QR decomposition, rank checks, least-squares solves, and residualization.
- Symmetric Jacobi eigendecomposition.
- Operator-oriented smallest/largest-k symmetric eigendecomposition with
  ordered results, residual norms, repeated-eigenspace semantics, and an
  explicit dense-reference maximum-order boundary.
- Gram-based SVD built from the symmetric eigensolver.
- Generalized eigendecomposition through symmetric reductions where suitable.

Reference implementations do not need to be the fastest path. They need to be
correct, deterministic, inspectable, cross-platform, and well tested.

## JVM Optional Backends

Breeze belongs behind a JVM-only adapter boundary, preferably in a dedicated
module such as `linalg-breeze`, not in shared `linalg` and not in domain
modules.

Rules:

- Breeze types must not appear in shared APIs.
- Domain modules must never import Breeze directly.
- Adapters translate at the edge: `DoubleMatrix <-> breeze.linalg.DenseMatrix`.
- The default backend remains the portable shared implementation.
- JVM code may explicitly opt into Breeze through the same solver traits.

This leaves room for future JS/WASM or native JVM adapters without changing
domain APIs.

## Migration Plan

1. Move ScalaFIM solver traits and result types into `linalg`.
2. Move reusable portable matrix/operator/solver capabilities into Gale.
3. Repoint standalone `multivar` directly to Gale.
4. Repoint `connectivity` to the `linalg` solver capabilities and delete the
   connectivity-local Jacobi helper.
5. Add an optional JVM Breeze backend adapter module.
6. Add shared solver contract tests and JVM-only differential tests comparing
   Breeze against the portable backend.

Current implementation status:

- `linalg` now owns `SymmetricEigenSolver`, `DenseSvdSolver`,
  `GeneralizedEigenSolver`, `SpdInverseSolver`, `SymmetricEigenResult`,
  `SvdResult`, `DecompositionRank`, and portable
  Jacobi/Gram/generalized/Cholesky-SPD-inverse reference solvers.
- Standalone `multivar` keeps its sparse-aware `MatrixView` adapter, uses Gale
  matrix/operator/spectral capabilities directly, and maps numerical failures
  into `MultivarError` only at domain API boundaries.
- Gale owns the portable proximal-gradient, projected-gradient, primal-dual,
  exact null-space reduction, and first-order certificate layer formerly
  implemented in ScalaFIM `linalg`.
- `connectivity` delegates its symmetric eigendecomposition boundary to
  `linalg` and no longer carries a private Jacobi implementation.
- `linalg-breeze` is the optional JVM-only adapter module. It exposes
  Breeze-backed symmetric eigen, dense SVD, generalized eigen, SPD inverse,
  Cholesky lower-factor, and full-rank least-squares adapters while keeping
  Breeze types out of shared and domain-module APIs.
- JVM-only backend differential tests compare Breeze adapters against portable
  references for eigen, SVD, generalized eigen, Cholesky/SPD inverse,
  least-squares/QR-equivalent solves, and failure boundaries.

## Test Requirements

Every solver backend must satisfy shared algebraic contracts:

- Symmetric eigen: `A V ~= V Lambda`, orthonormal eigenvectors, sorted
  deterministic values, finite results, and explicit non-convergence/failure
  behavior.
- SVD: reconstruction within tolerance, sorted singular values, orthonormal
  factors, rank-deficient behavior.
- QR: rank checks, least-squares recovery, residual orthogonality.
- Cholesky/SPD inverse: solve residuals, symmetry, positive-definite rejection.
- Generalized eigen: reduction consistency and deterministic ordering.

Portable tests run on JVM and Scala.js. Breeze differential tests are JVM-only
and compare against the portable contracts on deterministic matrices rather
than becoming the sole semantic authority.

## Mote Work Items

- Epic: `bd-01KX1P6Z3RW3T4GSZ4MT86Z7QQ`
- Docs/policy: `bd-01KX1P7J724GR8BDXARXTR372C`
- Solver algebra extraction: `bd-01KX1P7J7MMTD79YJ0KKV9VS2K`
- Consumer migration: `bd-01KX1P7J84GA94FGW5EQ8XDEJP`
- Optional JVM Breeze backend: `bd-01KX1P7J8AV2FNZSY7HPN4ZGJ3`
- Backend differential tests: `bd-01KX1P7J8R59M5RE7F8SKC9QZZ`
