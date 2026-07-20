# One-shot MVPA by canonical contrast effect

Status: **estimand frozen; implementation pending**

Epic: `bd-01KXZZZWCEEDVD963AZA40FHE7` (`CCA`)

This plan extracts one small method from the broader `fmrireg.cca` family. It
learns a spatial direction directly from prepared scan-level responses and a
first-level contrast, without constructing trialwise beta maps. It is a
canonical *contrast-effect* detector, not ordinary paired CCA and not a
directed classifier.

## Version-one estimand

Run (r) supplies a prepared design and response in these orientations:

| Name | Shape | Meaning | Owner |
|---|---:|---|---|
| (X_r) | (T_r \times p_r) | selected and whitened time-domain design | `fit` |
| (Z_r) | (T_r \times k) | identically selected and whitened response features | `mvpa-fit` |
| (C_r) | (1 \times p_r) | one estimable contrast, aligned to the run design | `fit` |
| (H_r) | (k \times k) | response total cross-product, (Z_r^\top Z_r) | `mvpa-fit` |
| (G_r) | (k \times p_r) | response-design cross-product, (Z_r^\top X_r) | `mvpa-fit` |
| (S_r) | (p_r \times p_r) | design cross-product, (X_r^\top X_r) | `fit` |
| (E_r) | (k \times k) | contrast-effect sum-of-products | `multivar` problem input |
| (R_r) | (k \times k) | residual sum-of-products | `multivar` problem input |

The design must have full column rank, the contrast must be nonzero and
estimable, all values must be finite, (T_r > \operatorname{rank}(X_r)), and
every run in a fold must use the same ordered feature axis. Version one rejects
rank-deficient designs rather than silently changing the estimand with a
pseudoinverse.

For

\[
M_r=S_r^{-1}, \qquad
V_r=C_rM_rC_r^\top,
\]

the run effect and residual matrices are

\[
E_r=G_rM_rC_r^\top V_r^{-1}C_rM_rG_r^\top,
\qquad
R_r=H_r-G_rM_rG_r^\top.
\]

These are equal to the explicit projector construction

\[
E_r=Z_r^\top P_{C,r}Z_r, \qquad
P_{C,r}=X_rM_rC_r^\top V_r^{-1}C_rM_rX_r^\top,
\]

and

\[
R_r=Z_r^\top (I-X_rM_rX_r^\top)Z_r.
\]

The production path uses moments; it never materializes either (T_r \times
T_r) projector. A direct projector implementation exists only in shared test
sources as an independent oracle.

### Leave-one-independent-run-out protocol

For held-out run (l), aggregate only training runs:

\[
E_{-l}=\sum_{r\ne l}E_r, \qquad
R_{-l}=\sum_{r\ne l}R_r.
\]

Residual regularization is an explicit model value. For a trace-scaled ridge
fraction (\alpha>0),

\[
\epsilon_{-l}=\alpha\,\operatorname{tr}(R_{-l})/k,
\qquad
B_{-l}=R_{-l}+\epsilon_{-l}I.
\]

The method solves only the leading symmetric-definite generalized root

\[
E_{-l}w_l=\lambda_{-l}B_{-l}w_l,
\qquad w_l^\top B_{-l}w_l=1.
\]

The learned (w_l) is frozen before the held-out run is inspected. Its held-out
root uses the unregularized held-out scientific quantities:

\[
\tilde\lambda_l=
\frac{w_l^\top E_lw_l}{w_l^\top R_lw_l}.
\]

A nonpositive held-out denominator is a typed degeneracy error, not an
infinite score. The feature-set statistic is the arithmetic mean

\[
\bar\lambda=\frac{1}{m}\sum_l\tilde\lambda_l,
\]

with the monotone descriptive transformation

\[
\rho=\sqrt{\frac{\max(\bar\lambda,0)}{1+\max(\bar\lambda,0)}}.
\]

Both (\bar\lambda) and (\rho) are returned. Residual degrees of freedom
(\nu_r=T_r-\operatorname{rank}(X_r)) are recorded but do not rescale (R_r)
in version one.

This is fold-safe effect detection: no held-out response can affect its training
direction, ridge, eigensolve, orientation, or diagnostics. It is not the signed,
cross-run `friman_step1_rayleigh_cv` statistic. That statistic estimates a
different cross-root and may be added later under a different method name. The
unconstrained (Ew=\lambda Rw) training core agrees with the F-optimization
path in `fmrireg.cca`; simplex/nonnegative spatial filters and its second-stage
channel construction are deliberately excluded.

## Identification and result contract

Weights are signed and unconstrained. For a simple leading root, presentation
orientation makes the largest-magnitude coefficient positive, with the lowest
feature index breaking ties. This sign convention has no statistical meaning
and does not enter the held-out ratio.

If the leading root is repeated within the configured spectral tolerance, an
individual vector is not identifiable. `multivar` must return an invariant
leading-subspace result, or explicitly mark the representative direction as
non-identifiable and report multiplicity and eigengap. Tests compare projectors
onto repeated-root subspaces, never arbitrary eigenvectors.

The typed result must distinguish:

- the nonnegative training canonical root;
- a simple direction or leading invariant subspace;
- normalization and sign-orientation facts;
- residual ridge amount and scale;
- numerical rank, leading-root multiplicity, eigengap, and Gale residual;
- run/fold, feature-axis, temporal-preparation, and solver provenance; and
- held-out roots, their mean, and the root-to-correlation transformation.

## The two nuisance domains stay separate

Temporal nuisance belongs to `fit`. Censoring, drift, motion regressors,
high-pass bases, and whitening alter the rows and geometry of (X_r) and must
be applied identically to (Z_r). `PreparedContrastGeometry` will carry the
factored quantities and a `TemporalPreparationReceipt` naming row selection,
whitening, nuisance rank, contrast rank and estimability, residual degrees of
freedom, and whether preparation was fixed, learned per run, or learned inside
a training fold. Response-learned preparation may not inspect held-out data
unless the receipt explicitly identifies it as held-out evaluation rather than
training preparation.

Trial/sample covariates do not enter this estimand. They belong to the distinct
trial-space adjustment and conditional-prediction contracts described in
[`one-shot-mvpa.md`](one-shot-mvpa.md). There is no untyped `confounds` input and
no implicit conversion between temporal and trial domains.

## Module design

The vertical slice follows existing boundaries rather than creating a CCA
engine:

```text
Gale matrices + symmetric-definite eigensolve
                    |
        multivar CanonicalEffectProblem/Fit
                    ^
                    |
fit PreparedContrastGeometry -- mvpa-fit fold/feature traversal
                                      |
                         mvpa result and feature-set contracts
```

- **Gale** owns general dense matrices, positive-definite evidence, spectral
  selection, and the symmetric-definite eigensolve.
- **`multivar`** owns the scientific generalized-Rayleigh problem, typed
  regularization, invariant result, canonical orientation, diagnostics, and
  error adaptation. It knows nothing about scans, contrasts, folds, or ROIs.
- **`fit`** owns prepared temporal design geometry and contrast estimability. It
  exposes sufficient factorizations, not a multivariate solver.
- **`mvpa-fit`** owns response-feature selection, streaming (H_r,G_r), run
  aggregation, fold isolation, and composition with existing ROI/searchlight
  traversal.
- **`mvpa`** owns generic feature-set results and evaluation contracts; it does
  not acquire fMRI design types.

Ordinary paired CCA and directed reduced-rank regression remain owned by
`bd-01KXSGZ3JXDTCAKBHWN8G549B8`. They may share Gale primitives but must not
share a misleading universal `CcaOptions` record or silently route through this
contrast-effect estimand.

## Gale boundary audit

ScalaFIM pins Gale revision `ef540198b0cfd5678e14f85cdc7ea904f87812ba`.
At that revision, shared JVM/Scala.js code provides the required matrix
carriers, positive-definite checks, eigenvalue selection, residual diagnostics,
and `gale.spectral.Eigen.eigSymmetricGeneralized`. The current gap list is
empty. Any later missing operation that is independent of fMRI and canonical
contrast semantics must be added to Gale and consumed through an advanced
immutable pin; ScalaFIM will not grow a private generic eigensolver.

## Failure algebra

Public construction or fitting must represent at least these failures directly:

- empty run collection or fewer than two independent runs for leave-one-run-out;
- nonfinite design, response, contrast, moment, or regularization value;
- timepoint, design-column, feature-axis, or matrix-shape mismatch;
- zero or multiple contrast rows in the version-one entry point;
- rank-deficient design, non-estimable contrast, or nonpositive contrast variance;
- nonpositive residual degrees of freedom or residual trace;
- non-symmetric effect/residual matrices beyond tolerance;
- a residual matrix that remains non-positive-definite after regularization;
- generalized eigensolver failure or residual above the declared tolerance;
- nonpositive held-out denominator; and
- missing or incompatible temporal-preparation scope.

No API boundary returns `null`, an untyped options map, or a caught exception as
normal control flow.

## Committed independent oracles

Three oracle families anchor the implementation:

1. The analytic rank-one problem (E=aa^\top), (a=(2,1)^\top),
   (R=\operatorname{diag}(2,3)) has leading root
   (a^\top R^{-1}a=7/3), direction proportional to
   (R^{-1}a=(1,1/3)^\top), and correlation (\sqrt{0.7}).
2. `CanonicalEffectDenseOracle` materializes (P_C) and (I-P_X) in test code,
   using an independent pivoted Gauss-Jordan inverse.
3. `CanonicalEffectReferenceFixtures` is emitted by
   `tools/r-parity/generate_cca_one_shot_fixtures.R`. Base R computes three
   runwise moments, verifies them against dense projectors, learns each frozen
   direction with the unconstrained `fmrireg.cca` F-optimization equations, and
   records fold roots and the aggregate transformation with all orientations
   documented.

Completion still requires metamorphic, leakage, null-calibration, held-out
recovery, allocation, and full JVM/Scala.js gates in CCA4. Agreement among two
Scala paths alone is not evidence of scientific correctness.

## Explicit non-goals

Version one does not include multiple contrasts/full MANOVA, the signed
cross-run Rayleigh or cvMANOVA statistics, ordinary paired CCA, classification
probabilities, RSA, trial betas, spatial derivative channels, simplex or
nonnegative weights, minimax contrast aggregation, probabilistic CCA,
permutation inference, or a new execution engine.
