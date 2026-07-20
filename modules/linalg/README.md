# scalafim-linalg

Small, cross-built numerical primitives for the neuroimaging core.

This module intentionally uses primitive `Array[Double]` backed vectors,
matrices, linear solves, and linear-map operators. It avoids Breeze and
JVM-only numerics so that core fitting and spatial-operator code remains
available on both the JVM and Scala.js.

`linalg` is also the project-level home for solver contracts and portable
reference implementations. Domain modules should depend on typed solver
capabilities from this module rather than defining private eigensolver, SVD,
inverse, or decomposition helper families. JVM-only libraries such as Breeze
belong in explicit adapter modules behind the same contracts; the current JVM
adapter is `linalg-breeze`. See
[`../../docs/plans/linalg-backend-strategy.md`](../../docs/plans/linalg-backend-strategy.md).

The shared operator layer provides:

- `LinearMap`, a small source-samples to target-samples map contract.
- `SparseTriplets`, a zero-based sparse interchange format.
- `CsrMatrix`, a primitive-array sparse operator with matrix/vector apply and
  an explicit adjoint.
- composition, row/column restriction, and block-diagonal composition helpers.
- `SymmetricOperator` evidence plus smallest/largest-k
  `PartialSymmetricEigenSolver` contracts whose successful results carry
  residual norms and explicit ordering. The portable dense reference path has
  an explicit configurable maximum order rather than silently materializing
  arbitrarily large sparse operators.
- Portable solver contracts and reference implementations for QR, Cholesky,
  symmetric eigendecomposition, SVD, generalized eigendecomposition, SPD inverse,
  and related primitive operations should live here as they are introduced.

Public sparse constructors validate with `Either[LinearMapError, ...]`; hot
apply paths stay allocation-conscious and primitive-array based.

`GramProjection` provides the shared least-squares contract used by latent
encoders: orthonormal dictionaries reduce to `B'Y`, while general dictionaries
solve `(B'B + ridge I) C = B'Y`.
