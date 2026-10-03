# UMVPA M4.01 calibrated filters and empirical Haufe diagnostics

Mote packet: `bd-01M2BNH46Y3V9H8PY3R8N450MW`. Source base:
`d023793b9bcb554c6c82a20eaeecf66dd98b2d03`, isolated branch
`work/umvpa-finish-20261001`. Final source identity is attached in Mote.

`PatternInterpretation.calibrated` returns forward task loadings separately from
calibrated filters `W = Psi^-1 A G^-1`, with the requested relative pivot
tolerance. The named scores use those admitted filters directly, so an explicitly relaxed
interpretation policy does not delegate to a stricter prediction-head refusal.
With the same policy they agree with unshrunk calibrated prediction scores.
Singular or relatively deficient precision Grams refuse explicitly; no added
jitter, ridge or pseudoinverse changes the estimand.

Empirical diagnostics compute `Cov(x, score) Cov(score)^-1` on declared
independent rows. Four distinct entry points identify raw components,
calibrated components, posterior components and posterior target estimates.
They retain actual neural/score axes, prediction identity, row scope,
selection/tuning declarations, covariance denominator and source/content-bound
identity. Overlap with training, selection or tuning is rejected using ordinals
in a common parent before scoring. Scope declarations cannot authenticate
unrecorded analyst access. Empirical score covariance must support the admitted
full-rank solve; deficient scores refuse instead of silently using a
pseudoinverse. Maps are neither sparse-projected nor assigned significance.

Independent fixtures use `Psi=[[2,1],[1,1]]`, `A=(1,0)` to obtain
`W=(1,-1)` and `W^T A=1`. The suppressor observations `x1=s+n, x2=n` recover
`s` while the forward task loading of x2 stays zero. Orthogonal Walsh signal,
nuisance and residual columns give `Cov(x,z)=(2,0)` and `Var(z)=2`, proving
model-implied Haufe `(1,0)=A`. A different independent diagnostic population
gives `(1.5,.5)` rather than sparse A; posterior shrinkage doubles its map.
A rank-two nontrivial target coordinate rotation and unequal target priors give
the literal mixed map `[[6/5,16/15],[-8/5,4/5]]`. Separate fixtures cover
training/selection/tuning reuse, singular empirical covariance, deficient G,
explicitly relaxed pivot tolerance, changed evidence identity and budget refusal.

Generic solves use Gale through existing prediction helpers. Dense ceilings
cover adapter arrays, not object overhead, process RSS or undeclared upstream
solver workspace. This is interpretation capability and independent finite
fixture evidence, not statistical calibration, causal attribution or inference.

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
