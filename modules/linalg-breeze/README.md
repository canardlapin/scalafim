# linalg-breeze

`linalg-breeze` is the JVM-only Breeze adapter for ScalaFIM linear algebra.
It depends on `linalgJVM` and implements the same public solver contracts over
`DoubleMatrix` and `DoubleVector`; Breeze matrices and vectors stay inside this
module.

Shared modules should depend on `linalg`, not on this adapter. JVM callers may
opt into `scalafim.linalg.breeze.BreezeSolvers` when they want Breeze-backed
dense decompositions.

`BreezeSolvers.partialSymmetricEigen` implements the operator-oriented partial
eigen contract by materializing the declared symmetric operator at this JVM
adapter boundary, selecting smallest or largest eigenpairs, and reporting
operator residual norms. Individual repeated-eigenvalue vectors are not a
cross-backend contract; differential tests compare their invariant subspaces.
