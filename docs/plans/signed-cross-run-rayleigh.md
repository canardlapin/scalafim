# Signed cross-run Rayleigh estimand

This extension is a signed, fold-safe estimand over the runwise sufficient
statistics already used by one-shot canonical-effect MVPA. It is not a second
CCA engine and it does not materialize trialwise betas.

For run `r`, let `u_r = Z_r' X_r (X_r'X_r)^-1 C'`, let `v_r =
C(X_r'X_r)^-1C'`, and let `R_r` be the residual SSCP. For held-out run `l`, a
direction `w_-l` is learned only from the within-run effect and residual moments
of the other runs. The fold statistic is

```text
q_l = [(w_-l' sum_{r != l} u_r) (w_-l' u_l) / v_l]
      / [w_-l' (sum_{r != l} R_r + ridge I) w_-l].
```

The numerator is the rank-one form of `fmrireg.cca`'s cross-run Rayleigh block
`sum G_r K_C^(r->l) G_l'`, evaluated at a frozen training direction. Unlike the
R reference kernel, ScalaFIM deliberately does not maximize that block after it
has inspected `G_l`: held-out data may change the score, but cannot change the
direction, ridge, training operator program, or its diagnostics.

The statistic is signed. A negative value means the held-out contrast
projection opposes the aggregate training projection. Direction orientation is
irrelevant because both projections change sign together. A global contrast
sign or nonzero contrast scaling therefore leaves the statistic unchanged.
The feature-set result is the arithmetic mean of fold statistics; it is not
clamped and has no canonical-correlation transform.

The null exchangeability action is an independent sign flip of each run's
contrast score, leaving residual moments fixed. This contract is recorded in
every fold result. Temporal nuisance preparation remains owned by `fit` and is
resolved at the exact training scope before moments are accumulated; no
trial-level nuisance covariate enters this estimand.

The implementation lives in `mvpa-fit`, consumes `CanonicalEffectProblem` and
its `OperatorProgramFit`, and traverses ordinary `FeatureSetPlan` regional or
searchlight collections. Generalized eigensolves remain Gale-backed through
`multivar`.
