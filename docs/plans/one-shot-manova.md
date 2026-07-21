# One-shot multiple-contrast MANOVA

For a prepared design (X), coefficient covariance (K=(X^\top X)^{-1}), and
a full-row-rank q-by-p hypothesis (C), the temporal layer constructs

\[
  Q = K C^\top L^{-\top}, \qquad
  L L^\top = C K C^\top.
\]

Thus (Q^\top X^\top XQ=I_q). Replacing (C) by (AC) for any nonsingular
q-by-q matrix changes only the basis inside the same hypothesis subspace.
`PreparedManovaGeometry` stores this normalized basis, its contrast covariance,
estimability rank, temporal scope, and whitening receipt. A singular contrast
covariance is a typed `NonEstimableContrast`, not a pseudoinverse convention.

For a selected feature set and run response (Z_r), the execution layer keeps
only

\[
  G_r=Z_r^\top X_r,\quad T_r=Z_r^\top Z_r,\quad
  H_r=(G_rQ_r)(G_rQ_r)^\top,\quad
  E_r=T_r-G_rK_rG_r^\top.
\]

No coefficient-by-feature table or time-by-time projector is required.
Training runs sum (H_r) and (E_r); `CanonicalEffectProblem.fitSpectrum(q)`
solves their symmetric-definite generalized eigensystem through Gale. Its
q-column `FunctionalFrame` is the sole fitted latent parameter, and the
inspectable `OperatorProgram` is the normalized maximize-trace program whose
value is the Hotelling-Lawley trace.

## Held-out estimand

For held-out run l, the learned training frame (W_{-l}) is frozen before its
moments are accessed. The held-out spectrum is computed only from

\[
  W_{-l}^\top H_lW_{-l}, \qquad
  W_{-l}^\top E_lW_{-l}.
\]

Rotations within the frozen q-dimensional frame cannot change the generalized
roots. This permits repeated training roots: each repeated block is represented
by its Euclidean projector and multiplicity rather than an arbitrary ordered
set of axes.

For roots λ1,...,λq, the named estimands are

- Roy largest root: max λ;
- Wilks lambda: product 1/(1+λ);
- Pillai trace: sum λ/(1+λ);
- Hotelling-Lawley trace: sum λ.

`ManovaMvpa` returns all four per fold and their fold means. The ordinary
`MvpaResult` carries the same four named metrics for regional and searchlight
execution; typed payloads retain the training spectrum fit, held-out roots,
temporal receipts, and `RunwiseSufficientStatistics` execution evidence.

The committed fixture generator uses independent base-R dense products,
Cholesky whitening, and `eigen` to anchor the complete leave-one-run-out path.
