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
fixed-effects meta-analysis; `RandomEffects` gives DerSimonian–Laird. An
intercept-only design collapses each to the classic meta-analytic aggregation;
a covariate design yields meta-regression — no new machinery.

## The nouns

| Type | Role |
|---|---|
| `GroupSpace` | the sample axis (sealed): `SampleAxis`, `VoxelAxis`, `ParcelAxis`. `VoxelAxis` carries a `SomeSampleSpace` + packed sample→voxel indices for later spatial correction. |
| `GroupResponse` | the primitive: one first-level contrast as `effects [subjects × samples]` plus optional `variances`. Mirrors `fit.ResponseBlock`. |
| `GroupData` | the canonical cube: subjects × samples × (first-level) contrasts, as role-typed responses over a `GroupSpace`. |
| `GroupDesign` | the second-level design `[subjects × terms]` with named terms. Built by combinators: `intercept`, `twoSample`, `covariates(DataTable, …)`. |
| `GroupWeighting` | the estimator choice (sealed): `Unweighted`, `InverseVariance`, `RandomEffects(tau)`. |
| `GroupModel` | pure description = `GroupData` + `GroupDesign` + `GroupWeighting`, row-checked, inspectable via `.summary`. |
| `GroupFit` / `GroupResult` | typed outputs: per-term coefficient/SE maps, statistic kind (`StudentT(df)` vs `Normal`), heterogeneity (`tau2`, `Q`, `I2`). |
| `GroupContrast` | name-keyed linear combination of design terms → per-sample estimate/SE/statistic/p. Mirrors `fit.TContrast`. |

## The verbs

```
GroupData ── withDesign ──▶ GroupModel ── GroupEngine.fit ──▶ GroupResult
                              │                                   │
                              └── weighting                       └── contrast(GroupContrast) ──▶ GroupContrastResult
                                                                  └── Fdr.benjaminiHochberg ──▶ q-maps
```

`GroupData` is assembled from raw arrays, or bridged directly from first-level
results: `GroupData.fromFirstLevel(subject → TContrastResult)` reads each
subject's estimate as the effect and `se²` as the variance. That bridge is why
the module depends on `scalafim-fmri-fit`.

## Statistics in the first cut

- **Unweighted group GLM (OLS).** One-sample and two-sample group t-tests,
  ANCOVA-style covariate models. Estimates residual variance; reports
  `StudentT(df = n − p)`.
- **Fixed-effects meta-analysis.** Inverse-variance weights `w = 1/var`;
  `μ = Σwβ/Σw`, `var_μ = 1/Σw`; heterogeneity `Q`, `I² = max(0,(Q−(k−1))/Q)`.
  Known variances, so `Normal` statistics.
- **Random-effects meta-analysis.** DerSimonian–Laird `τ² = max(0,(Q−(k−1))/C)`,
  `C = Σw − Σw²/Σw`, reweight `w* = 1/(var+τ²)`.
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
- **Totality.** Every public smart constructor returns `Either[GroupError, …]`
  and never throws for ordinary input: non-finite designs, non-numeric covariate
  columns, and duplicate contrast names are typed errors. Meta-analytic fits use
  a scale-relative positive-definiteness tolerance, so a well-posed system with
  very small weights (very large first-level variances) is not falsely singular.

## Deliberately deferred

Each is a clean later phase, not a gap in the core:

1. **Restricted LMM** (random intercept / +slope) — needs a profiled-REML
   optimizer. `GroupWeighting` gains a `Mixed(...)` case.
2. **Permutation inference** (sign-flip / label-permutation) and **spatial
   correction** (TFCE, cluster-extent) — depend on `image` mask gather/scatter
   and the planned `scalafim-fmri-threshold` module
   (`docs/plans/neurothresh.md`); `VoxelAxis` already carries the geometry.
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
`scalafim.fmri.group`. Depends on `linalg` (numerics), `image` (`SomeSampleSpace`),
`dataset` (`SubjectId`), `design` (`DataTable`), and `fit` (first-level bridge).
</content>
</invoke>
