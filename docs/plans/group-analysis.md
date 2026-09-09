# Group Analysis — `scalafim-fmri-group`

Second-level (group) fMRI analysis in ScalaFIM. This module combines
per-subject first-level effect maps into population inferences: is a contrast
reliably non-zero across a group, does it depend on covariates, and where does
it survive multiple-comparison correction.

The reference is the R package `fmrigds` ("Lazy, Format-Agnostic Group Analysis
for fMRI"). We keep its *statistical* content and its central insight — that a
"sample" can be a voxel, a parcel, a surface vertex, or a latent component, and
group analysis is the same computation regardless — but we leave behind its R
surface: string-tagged plan lists, mutable environment registries, `list()`
option bags, and `model.matrix` non-standard evaluation.

## Design stance

The most idiomatic Scala 3 rendering separates a **pure, inspectable
description** from its **execution**, exactly as `scalafim-fmri-model` /
`scalafim-fmri-fit` already do for the first level:

- A `GroupModel` is an immutable value you can print, validate, and reason about
  before it touches a byte of data. It composes through small combinators.
- A total interpreter (`GroupEngine.fit`) folds that description into typed
  results. Failure modes are values (`GroupError`), not exceptions or `NULL`.

fmrigds's open, runtime **reducer registry** becomes a *closed* `sealed` algebra
of estimators. For a numerical core, an exhaustive `match` over a sealed set is
safer and more transparent than a mutable string-keyed map; third-party
extension, if ever needed, is a later `trait`-based capability, not the default.

The single unifying idea: **ordinary least squares and inverse-variance
meta-analysis are one weighted GLM** parameterized by where the weights come
from. `Unweighted` gives the group OLS/t-test; `InverseVariance` gives
fixed-effects meta-analysis; `RandomEffects` selects DerSimonian–Laird or Paule–Mandel with an explicit
normal or modified Knapp–Hartung reference policy. An
intercept-only design collapses each to the classic meta-analytic aggregation;
a covariate design yields meta-regression — no new machinery.

## The nouns

| Type | Role |
|---|---|
| `GroupSpace` | the sample axis (sealed): `SampleAxis`, `VoxelAxis`, `ParcelAxis`. `VoxelAxis` carries a `NeuroSpace` + packed sample→voxel indices for later spatial correction. |
| `GroupResponse` | the primitive: one first-level contrast as `effects [subjects × samples]` plus optional `variances`. Mirrors `fit.ResponseBlock`. |
| `GroupData` | the canonical cube: subjects × samples × (first-level) contrasts, as role-typed responses over a `GroupSpace`. |
| `GroupDesign` | the second-level design `[subjects × terms]` with named terms. Built by combinators: `intercept`, `twoSample`, `covariates(DataTable, …)`. |
| `GroupWeighting` | the estimator choice (sealed): `Unweighted`, `InverseVariance`, `RandomEffects(tau, inference)`. |
| `GroupModel` | pure description = `GroupData` + `GroupDesign` + `GroupWeighting`, row-checked, inspectable via `.summary`. |
| `GroupFit` / `GroupResult` | typed outputs: per-term coefficient/SE maps, statistic kind (`StudentT(df)` vs `Normal`), heterogeneity (`tau2`, `Q`, `I2`). |
| `GroupContrast` | name-keyed linear combination of design terms → per-sample estimate/SE/statistic/p. Mirrors `fit.TContrast`. |

## The verbs

```
GroupData ── GroupModel.build ──▶ GroupModel ── GroupEngine.fit ──▶ GroupResult
                              │                                   │
                              └── weighting                       └── contrast(GroupContrast) ──▶ GroupContrastResult
                                                                  └── Fdr.benjaminiHochberg ──▶ q-maps
```

`GroupData` is assembled from raw arrays, or bridged directly from first-level
results: `FirstLevel.groupData(space, subjects, contrasts, results)` reads each
subject's estimate as the effect and `se²` as the variance. That bridge is why
the module depends on `scalafim-fmri-fit`.

## Statistics in the first cut

- **Unweighted group GLM (OLS).** One-sample and two-sample group t-tests,
  ANCOVA-style covariate models. Estimates residual variance; reports
  `StudentT(df = n − p)`.
- **Fixed-effects meta-analysis.** Inverse-variance weights `w = 1/var`;
  `μ = Σwβ/Σw`, `var_μ = 1/Σw`; heterogeneity `Q`, `I² = max(0,(Q−(k−1))/Q)`.
  Known variances, so `Normal` statistics.
- **Random-effects meta-analysis.** DerSimonian–Laird uses `τ² = max(0,(Q−df)/C)`;
  Paule–Mandel solves `Q(τ²) = df` with a zero boundary and a checked iteration
  limit. Here `df = n-p`. Reweight with `w* = 1/(var+τ²)`.
- **Inference policy.** Plug-in normal testing preserves the legacy DL/z
  convention. Modified Knapp–Hartung scales coefficient and contrast covariance
  by `max(1, Q_RE/df)` and uses `t(df)`. The new `GroupModel.mixedEffects` requires
  explicit estimator and inference arguments; calibration does not justify an
  automatic small-sample default. The [2026-09-08 repair qualification](../verification/group-repair-2026-09-08.md)
  records independent R parity and rejection rates for unequal, known and
  estimated first-level variances.
- **Meta-regression.** Either weighting with a covariate design → per-term
  `coef/se/statistic` maps, solved per sample.
- **Group t-contrasts.** Name-keyed weights over design terms; correct scale
  from `(XᵀWX)⁻¹`; distribution follows the fit's statistic kind.
- **Multiple comparisons.** Benjamini–Hochberg and Benjamini–Yekutieli FDR
  (pure, per-map). `Distributions` provides the normal and Student-t CDFs
  (`erf`, regularized incomplete beta) so p-values are self-contained.

Everything cross-compiles to JVM and Scala.js, is pure Scala (no Breeze), and is
covered by invariant tests plus numerical fixtures against hand-computed and
`fmrigds`/`p.adjust` values.

## Known bounds and policy

- **Per-sample covariance memory.** Weighted fits store each sample's `(XᵀWX)⁻¹`
  as a packed lower triangle in a single backing matrix (`O(samples · terms²/2)`,
  one object rather than one per sample), so any post-hoc group contrast stays
  cheap. That is comfortable for ROI/parcel maps and moderate voxel counts; for
  very large `terms` on high-resolution whole-brain WLS, recompute-from-retained-
  weights is a further follow-up. OLS keeps a single shared `(XᵀX)⁻¹`, so it has
  no such cost.
- **Residual degrees of freedom.** The engine requires more subjects than design
  terms (`n > p`) for every estimator, so heterogeneity (`Q`, `I²`) is always
  defined. Saturated known-variance meta-analysis (`n = p`, e.g. a single-study
  summary) is intentionally out of scope for now.
- **Validation and availability.** Smart constructors returning `Either` reject
  ordinary invalid input as typed errors; explicitly unsafe helpers and direct
  constructors enforce their documented `require` contracts. Gale pivoted QR
  checks rank after unit normalization and centering on an existing constant.
  Weighted solves use an orthonormal basis, normalized weights and a weighted-QR
  fallback when imbalance makes normal equations unsuitable. Global rank
  failures are explicit. Per-sample numerical failures carry positions and
  reasons through term and contrast results; all-failed weighted maps return a
  typed error. First-level voxel indices are aligned, but callers must establish
  physical co-registration and the correspondence between subject IDs and design
  rows. First-level residual df and cross-response covariance are not recovered
  by the current group bridge.

## Deliberately deferred

These remain separate implementation and scientific-admission requirements:

1. **Restricted LMM** (random intercept / +slope) — needs a profiled-REML
   optimizer. `GroupWeighting` gains a `Mixed(...)` case.
2. **Calibrated finite-sample inference.** Design-specific resampling or
   alternative adjusted inference needs independent null/power calibration,
   subject exchangeability and nuisance handling, and provenance for estimated
   first-level SEs/df. Pointwise resampling does not require spatial thresholding.
   Spatial FWER/TFCE/cluster procedures are a distinct admission layer; see
   `docs/plans/neurothresh.md`.
3. **Spatial (parcel) FDR** — Simes-within-parcel + weighted BH; pure, small,
   an easy follow-up to BH/BY.
4. **Evidence combiners** (Stouffer, Fisher, Lancaster).
5. **Adapters / ingestion** (NIfTI, HDF5 catalog sweep) — an IO layer over the
   cube, mirroring `dataset`'s backend contract; never in the numeric core.
6. **A lazy verb-pipeline façade** (`subset/derive/mask/reduce/posthoc/write`)
   over this core, if the composable-plan aesthetic proves worth the machinery.
7. **Variance propagation for spatial maps** (`map_to`/`align`, `M²·var` /
   `wᵀΣw`) once cross-space projection lands.

## Module wiring

`modules/group`, cross-compiled, `name := "scalafim-fmri-group"`, package
`scalafim.fmri.group`. Depends on `linalg` (numerics), `image` (`NeuroSpace`),
`dataset` (`SubjectId`), `design` (`DataTable`), and `fit` (first-level bridge).


### Bounded symmetry admission — 8 September 2026

The next qualified alternative to the PM/mKH approximation is a separate
common-center, two-sided sign-flip test for one valid contrast per independent
subject, under symmetry conditional on selection/precisions. Exact enumeration
and identity-corrected Monte Carlo tests use resample4s core at the existing
native 6bc4172 pin. Subject-bound plans support dense/block reuse without
retaining a resampling distribution for every voxel. Equal-subject and fixed
inverse-variance scores have explicit assumption and availability receipts.

The [held-out qualification](../verification/group-symmetry-2026-09-08.md) includes
null rejection, power, true-center acceptance (confidence-set coverage), full
small-orbit oracles, portable tests and completed map performance. This bounded
admission does not close general inference bd-01M20TKX7VYFT3BHCM7QAMA6NG or
estimator expansion. Next compare null-restricted studentized wild bootstrap
and CR2/Satterthwaite candidates for nuisance-adjusted contrasts, with independent
references and new held-out designs; do not infer finite-sample validity from
the bootstrap name. Preserve first-level uncertainty provenance/df under
bd-01M210WJ4BWVCXEMTARHDR2AC7. Manifest-backed execution remains
bd-01KX6G9BFJRERNFRWR17WT0YGN. Pointwise tests, spatial correction and reusable
GLM application delivery remain separately verified responsibilities.
