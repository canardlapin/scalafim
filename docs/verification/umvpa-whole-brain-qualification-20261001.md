# UMVPA M3.12 independent whole-brain slice qualification

Mote packet: `bd-01M2BNGTQ54BA008ZQFRMS0CF3`. Source base:
`d023793b9bcb554c6c82a20eaeecf66dd98b2d03`, isolated branch
`work/umvpa-finish-20261001`. Final source identity is attached in Mote.

The shared `WholeBrainPatternQualificationSuite` exercises real two-stage
pilot/covariance/final fits and consumes their rich global artifacts through
classification, Gaussian decoding, forward encoding and marginal hard-ROI
prediction. Independent dense cofactors compute conditioning expectations from
literal `D+U U^T` covariance assembly, without using the precision implementation
as the oracle. Output inspection and local-head derivation make no additional
source reads or structured/covariance fits. Existing two-stage receipts agree
with independent provider counters, with two structured fits, one covariance
fit and the declared residual pass. No singleton ROI or second fitting runtime
is needed.

A three-dimensional cofactor oracle independently checks factorized precision.
A consistent change of target units and prior covariance preserves neural
means and scales target decoding. Null targets and nearly dependent coordinates
refuse with named numerical-rank diagnostics. Invalid noise rank and forced
one-iteration covariance nonconvergence preserve exact error outcomes. Existing
structured outer-exhaustion and fixed-covariance objective/gradient oracles
remain part of the owning suite; alternating convergence does not establish a
global optimum.

The 2x2 weighted checkerboard support graph is consumed by the real proximal
optimizer. Independent convex arithmetic gives a constant envelope 2/3,
loadings `(2/3,0,0,2/3)` and objective 2/3; disconnecting that graph returns the
literal reference and zero objective. This qualifies that graph specimen,
not support recovery or calibration on arbitrary whole-brain data.

Poison metadata sentinels admit p=50,000 where p squared exceeds primitive-array
capacity, and p=100,000/q=40,000 where p*q does, without source reads. Their
planned precision blocks have width one; required q squared capacity remains
explicit and refuses q=50,000. These are pre-execution shape/admission checks,
not runtime allocation traces. Source inspection and the executed column-work
fixtures exclude compulsory neural-square or neural-by-target effect arrays in
the adapter path. Provider-private kernels, process peak memory, realistic
whole-brain latency and final allocation audit remain separate boundaries.

Final exact-source gate108 passed all 344 mvpa tests on the JVM and all 344
on Scala.js; `scalafimCompileAll` passed without compiler warnings/errors.
Gate106 separately passed all 107 mvpa-dataset and 20 mvpa-spatial tests on
each platform. Its consumer sources were unchanged; the final interpretation
scoring-policy and unit-scaling fixture edits were reverified by gate108.
The 189 source/fixture entries in `gate108-sources.json` remained unchanged
through the final gate. New-file and staged whitespace checks passed.

A draft unit-scaling test used a duplicate local variable name; the failed
gate107 log is retained and excluded from acceptance. Final verification uses
gate108. Read-only interpretation review approved the formulas and final
policy-consistency correction. Parent review independently checked the dense
conditioning, checkerboard convex optimum and qualification boundaries.

Raw log SHA-256:

- gate106-wholebrain-interpretation-final.log: `aca15e3327fdea2c14721c4d8c9ea5b9f2e3e8dd2a16712616866b45e0cef805`
- gate108-pattern-exact-final.log: `739b6903e078a50f789159fcddbde288b1fedfbfdad238d0201b34f1902b477d`

Evidence directory:
`/private/tmp/scalafim-umvpa-finish-evidence-20261001`.
