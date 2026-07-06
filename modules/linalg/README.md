# scalafim-linalg

Small, cross-built numerical primitives for the neuroimaging core.

This module intentionally uses primitive `Array[Double]` backed vectors,
matrices, linear solves, and linear-map operators. It avoids Breeze and
JVM-only numerics so that core fitting and spatial-operator code remains
available on both the JVM and Scala.js.

The shared operator layer provides:

- `LinearMap`, a small source-samples to target-samples map contract.
- `SparseTriplets`, a zero-based sparse interchange format.
- `CsrMatrix`, a primitive-array sparse operator with matrix/vector apply and
  an explicit adjoint.
- composition, row/column restriction, and block-diagonal composition helpers.

Public sparse constructors validate with `Either[LinearMapError, ...]`; hot
apply paths stay allocation-conscious and primitive-array based.

`GramProjection` provides the shared least-squares contract used by latent
encoders: orthonormal dictionaries reduce to `B'Y`, while general dictionaries
solve `(B'B + ridge I) C = B'Y`.
