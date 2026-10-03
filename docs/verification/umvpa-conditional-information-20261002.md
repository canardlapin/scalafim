# UMVPA M4.03 Gaussian conditional information

Status: independently reviewed source and JVM/Scala.js numerical/refusal gates pass.
Packet `bd-01M2BNH845EVA9T7ZQ98TYS8C0`; isolated local branch
`work/umvpa-finish-20261001`, parent `9485e822520cad8f04d5784cdd1c6ed669cd7291`.
No publication, inference calibration or whole-brain allocation claim.

## Scientific contract

For `y=Bz`, `z~N(0,S)`, `S=LL^T`, `x=AC^T y+epsilon`, with positive
definite residual covariance Psi, the implementation computes
`P=Psi^-1`, `G=A^T P A`, `T=L^T B^T C`,
`K=T^T (I+T G T^T)^-1 T`, and
`t_v=(P A)_v K (P A)_v^T / P_vv`. The information map is
`-.5*log1p(-t_v)` in nats, conditional on all other neural coordinates.
Woodbury gives marginal precision diagonal `P_vv*(1-t_v)`; the conditional
variance ratio proves the Gaussian LOVO identity. This is model-based
information, distinct from forward loading, held-out performance and significance.

An explicit injective supported basis and positive supported prior allow a
singular full-target covariance without inferred task rank or a pseudo-inverse.
Categorical targets refuse. Foreign axes, deficient or ill-conditioned support,
nonfinite coefficients, negative values beyond rounding policy and coefficients
at/near one return typed errors. Tiny negative rounding is recorded in diagnostics.

One residual precision application, one precision-diagonal call and one small
supported posterior factorization/solve produce the whole map. There is no
voxel-square matrix, voxel-by-full-target materialization or per-voxel fit.
Numerical identity binds actual factor, covariance, basis and prior values,
complete coordinate signatures, source/training identity and all numerical
policies. No silent jitter or inferred subspace is introduced.

## Work and cancellation boundary

Admission uses BigInt counts and per-array primitive-capacity checks. Known
residual-covariance resident and planned work plus local intermediates are
counted conservatively; opaque provider/Gale-private scratch and process RSS
are excluded explicitly. Supported-basis construction counts incremental
repository-owned normalized matrix, Gram, scales and norms: `q*s+s*s+2*s`.
Full support additionally owns the identity basis and preflights `3*q*q+2*q`
before allocation. Borrowed inputs and Gale-private factor scratch are excluded
from that constructor contract. These are cell ceilings, not measured memory.
Cancellation checkpoints occur before work, around precision phases and between
voxels; no partial map is returned. An ongoing kernel/factorization cannot be
interrupted by those checkpoints.

## Independent evidence

The shared suite contains eight tests per platform. Dense two-voxel determinants
include a zero-forward-loading noise-cancelling voxel with positive information.
A declared rank-one subspace in two target coordinates matches the scalar model.
Three-voxel dense cofactors, reciprocal factor scales/signs, zero signal, and
coordinate permutations check independent laws. A separate two-component,
rank-two nonorthogonal basis in three target coordinates with correlated prior
and residual noise uses independently computed dense LOVO expectations:
`[0.28075327930913685, 0.06521806666890578, 0.09498140772710029]`, tolerance
`1e-11`. Independent Python/NumPy formula review agrees within `1.94e-16`.
Additional assertions cover numerical identity, exact constructor budgets,
foreign support, near-one refusal and mid-map cancellation.

`gate118-conditional-final.log`: exit 0, eight JVM and eight Scala.js tests,
no compiler warning/error lines. Command:

```sh
python3 tools/build/sbt-warm 'mvpaJVM/testOnly scalafim.fmri.mvpa.pattern.ConditionalInformationSuite' 'mvpaJS/testOnly scalafim.fmri.mvpa.pattern.ConditionalInformationSuite'
```

Raw log SHA-256: `0ae0d3599ac2b7b2c24d3f6eb59acf791996506463a628df91abb173ed9a2523`.
The two source hashes are frozen in `gate118-conditional-sources.json`.
The prior integrated gate117 passed owner/consumer tests and warning-clean
`scalafimCompileAll`; gate118 recompiled both platforms after the final budget
repair and added oracle. Draft gate114/115 compilation failures and the
confirmed identity-writer infinite loop interrupted in gate116 are retained as
nonpassing evidence. Independent source review approves the final hashes.

Evidence directory: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.
The model remains experimental; calibration, matched performance, subject/group
transport and release qualification retain their own packets.
