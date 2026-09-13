# Unified MVPA inference and known-truth calibration protocol

Status: frozen M0.05 protocol, version 1, 2026-09-13

Mote owner: `bd-01M2BNEGA9DJWA4DEQKD88BH11`

Authority: [PRD v0.2](unified-mvpa-prd.md), especially INF-01 through
INF-05, GRP-01, GRP-02, and NUM-01. The
[scientific notes](unified-pattern-first-mvpa-notes.md) retain the derivations;
the [migration ledger](unified-mvpa-migration-ledger.md) owns cutover rather
than inferential admission.

This document freezes what must be generated, tested, retained, and decided
before a unified-MVPA inferential result is admitted. It is a protocol, not a
simulation report. No type-I, coverage, power, group, threshold, or rank claim
passes merely because this document exists.

## 1. Claim ladder and non-substitution rules

Every inferential artifact records one of these claim classes. A result from a
lower class cannot be relabeled as a higher class after results are inspected.

| Code | Claim | Required evidence | Initial availability |
| --- | --- | --- | --- |
| `D0` | Descriptive fitted association or map | Identified inputs, estimand, fit scope, and numerical validity | May be available without inferential admission; never rendered as significance. |
| `P1` | Held-out predictive performance for the observed evaluation units | Leakage-safe design, loss estimand, independent units, and admitted uncertainty for those units | Required predictive route; not a population or prevalence claim by itself. |
| `C1` | Fixed-representation conditional inference | All discovery choices frozen on independent evidence; confirmation-only nuisance fit and sufficient statistics | Candidate first inferential release after M4.04--M4.09 qualification. |
| `C2` | Complete selection-aware inference | Every selection, preparation, split, tuning, rotation, rank, and spatial choice that depends on randomized evidence is rerun in each replicate | Unavailable until separately implemented and calibrated. Frozen-representation resampling cannot satisfy it. |
| `G1` | Population mean task-linked effect over independent subjects | Commensurate subject estimands, transported uncertainty, explicit group model, and subject-level calibration | Required group route, conditional on the existing group and uncertainty prerequisites. |
| `G2` | Generalization to a new subject | Held-out subjects and a declared subject-prediction loss or association estimand | Separate from `G1`; unavailable until M5 qualification. |
| `G3` | Population prevalence of information | A prevalence estimand and a procedure calibrated for it | Explicitly unavailable in the first qualified release. Ordinary second-level accuracy or mean-effect tests do not substitute. |

`C1` conditions on the realized discovery representation. It does not average
over representations that could have been selected. `C2` must repeat the
complete selection procedure and use the same outer evidence-design law as the
observed analysis. `G1`, `G2`, and `G3` answer different population questions.
An API must make an unavailable class unconstructable or return a typed
`InferenceUnavailable`; it must not return an empty p-value, `NaN`, or a
descriptive score in an inferential wrapper.

Any outer-confirmation score, map, diagnostic, timing probe, or plot is an
evidence exposure. A choice made after that exposure invalidates the untouched
holdout claim. The workflow receipt records purpose, actor or role, evidence
scope, and later selection dependency. Unknown external exposure remains
`Unknown`; it is not silently promoted to `Untouched`.

## 2. Inferential result contract

An admitted result binds all of the following before execution:

1. claim class and scientific estimand;
2. point, complete family, or sequential family null;
3. discovery and confirmation evidence identities;
4. independent unit and any exchangeability blocks;
5. nuisance design, error model, and covariance-estimation scope;
6. frozen stages, recomputed stages, and refitted stages per replicate;
7. family membership and multiplicity procedure;
8. exact or Monte Carlo transform count, stopping rule, and seed lineage;
9. local failures, family completeness, and availability status; and
10. implementation/provider revisions plus the protocol and scenario hashes.

All members of a max-statistic or stepdown family receive the same replicate
transformation. Every admitted replicate is family-complete. Local estimates
may remain available after a member failure, but a failed or omitted member
blocks the family-level inferential result. Fixed-`B` results report the
completed `B`, never a planned value after cancellation. Sequential stopping
is unavailable until a separate stopping rule is calibrated.

Cross-validation folds are dependent analysis products, not independent
observations. Their different labels do not create new inferential units.

## 3. Deterministic streams and result quarantine

The protocol stream namespace is the UTF-8 string
`scalafim/umvpa/inference-calibration/v1`. A scenario data seed is derived as
follows:

```text
seedText = namespace + NUL + phase + NUL + scenarioId + NUL + replicateIndex
digest   = SHA-256(UTF-8(seedText))
seed64   = bigEndianUnsigned(digest[0..7]) & 0x7fffffffffffffff
seed64   = if seed64 == 0 then 1 else seed64
```

`phase` is exactly one of `fixture`, `simulator`, `pilot`, or `confirmation`.
`replicateIndex` is an unpadded base-10 integer beginning at zero. Within a
study, resampling, fit initialization, simulator noise, censoring, and
bootstrap stages derive named child streams from this root through the adopted
resample4s/Alder lineage. Mutable global PRNG state and scheduling order may
not affect a result. The implementation must land a cross-language seed-vector
fixture before any pilot.

Pilots may use only `pilot` streams. Confirmation output is written once to an
immutable receipt containing every scenario and failure. Viewing any
`confirmation` result before method, thresholds, source revisions, and scenario
hashes are locked makes that run exploratory; a new protocol version and stream
namespace are then required for a confirmatory claim. Unfavorable cells are
never removed from the receipt.

## 4. Frozen simulation populations

All continuous variables are centered at their declared population means.
Effect sizes below are population quantities, not sample-normalized values.
Unless a row overrides it, noise innovations are Gaussian, residual voxel SD
is one, target components have unit variance, and active signs alternate by
ascending voxel identity. The simulator records the exact population
covariance and realized design.

### 4.1 Evidence designs

| ID | Independent units and dimensions | Dependence and nuisance | Purpose and permitted action |
| --- | --- | --- | --- |
| `IID80` | 80 independent rows, 256 voxels, 4 target components | Intercept; Gaussian, standardized Student-t(5), and centered log-normal innovations as separate cells | Closed-form and unrestricted row-exchangeable baselines. |
| `IID240` | 240 independent rows, 2,048 voxels, 4 target components | Intercept plus two fixed nuisance columns correlated `0.4` with target columns | Large-family and nuisance-adjustment baseline. Residual shuffling is permitted only through an admitted nuisance transform. |
| `TIME1` | One subject, 6 independent runs of 96 rows, 256 voxels, 4 target components | Per-run intercept, linear and quadratic drift, 6 motion columns; AR(1) innovation `phi` in `{0.0, 0.4, 0.8}`; zero or 10% predeclared censoring | First-level coefficient/SE calibration. Time rows are never treated as freely exchangeable. Six runs do not support a generic asymptotic population claim. |
| `REPEAT24` | 24 independent subjects, 2 sessions per subject, 2 runs of 96 rows per session, 256 voxels, 4 components | `TIME1` with `phi=0.4`; subject intercept SD `0.5`, component-slope SD `0.2`, within-subject slope correlation `0.4` | Repeated-measure and subject-generalization cells. Resampling acts on whole subject blocks and preserves within-subject rows. |
| `REPEAT80` | 80 independent subjects with the same repeated layout | `TIME1` with `phi` in `{0.4, 0.8}` and 10% censoring | Group, noisy-uncertainty, and heterogeneity confirmation. Subject is the independent unit. |

For `TIME1`, zero-censor cells remove no rows. Predeclared 10% censor cells
remove rows whose within-run ordinal ends in `7`, then the smallest remaining
ordinals until exactly `floor(0.10 * 96)` rows are removed. The adverse
motion-linked censor cell instead removes the same count by descending absolute
value of the first generated motion column; ties use row identity. This rule is
part of the simulator and is regenerated in a complete-procedure replicate.

Temporal task columns include one alternating 12-row block contrast and two
event trains at within-run ordinals `[4, 20, 36, 52, 68, 84]` and
`[12, 28, 44, 60, 76, 92]`, convolved with the frozen HRF used by the fit
receipt. The nuisance effect has population SD `0.30` in the signal.
The simulator validates the convolved design, censor identities, empirical
innovation covariance, and noiseless coefficient recovery before fit results
are examined.

### 4.2 Voxel and component truth families

Let `T` be the independently observed confirmation target-score matrix and

```text
X = T A.transpose + N.
```

The following families are mandatory:

| ID | Truth | Primary null family |
| --- | --- | --- |
| `V0` | `A = 0` at every voxel and component | Complete null for all 256 by 4 component tests and all 256 omnibus tests. |
| `V1` | For zero-based component `k`, voxels `16*k` through `16*k+15` have alternating loadings `+/-0.15`; sets are disjoint | Partial null with 64 active and 192 null voxels; FWER counts any rejected known-null member and FDP divides rejected known-null members by all rejections. |
| `V2` | Same support as `V1`, loading magnitude `0.30` | Standard-effect power and interval calibration. |
| `V3` | Same support as `V2`; residuals occur in 32-voxel Toeplitz blocks with correlation `0.6^distance` | Spatially dependent complete-family calibration without treating voxels as independent. |
| `V4` | A two-voxel residual block has correlation `0.8`; voxel 0 has loading `0`, voxel 1 has loading `0.30` | Suppressor control: zero forward loading is retained as a true voxel null even if conditional predictive information is positive. |
| `V5` | `V2` with 5% of voxels assigned infinite/non-finite input in predeclared complete-family variants | Typed local failure and family-incompleteness behavior; never a significance calibration cell. |

Component association uses population correlations `0.00`, `0.15`, `0.30`,
and `0.50`. Incremental prediction uses population loss improvements equivalent
to `Delta R-squared` of `0.00`, `0.01`, `0.04`, and `0.09`, with correlated
components at off-diagonal correlation `0.50`. The reduced prediction head is
refit on its authorized training evidence. Direct coefficient deletion is an
adverse counterfactual, not the incremental-prediction estimand.

For voxelwise multiplicity, the frozen families are: all component-specific
tests (`p * r`), all voxel omnibus tests (`p`), and each predeclared component
slice (`p`). A result must name which family it controls. BH/FDR is evaluated
only after valid marginal p-values exist; all-null BH is also checked as a FWER
control. Max-statistic transforms are common across the whole named family.

### 4.3 Projected-rank truth families

Discovery freezes brain and target subspaces with dimensions `P=6` and `Q=4`;
confirmation rank is therefore at most `K=4`. Gaussian confirmation
populations use identity marginal covariance and these canonical correlations:

| ID | Population canonical correlations | Nulls that must remain calibrated |
| --- | --- | --- |
| `R0` | `[0, 0, 0, 0]` | `H1` through `H4` under closed testing. |
| `R1` | `[0.50, 0, 0, 0]` | `H2` through `H4`; the first relationship remains present. |
| `R2` | `[0.50, 0.30, 0, 0]` | `H3` and `H4`; the first two relationships remain present. |
| `R3` | `[0.50, 0.30, 0.20, 0]` | `H4`; the first three relationships remain present. |
| `R4` | `[0.50, 0.30, 0.20, 0.12]` | Power for the fourth root; no higher-rank upper-bound claim. |

Each family runs at `n` in `{80, 160}`. Nuisance variants add three full-rank
columns, two correlated `0.4` with each side. Repeated-unit variants use 40
independent subject blocks of size two. Dimension-imbalance variants use
`(P,Q)` in `{(6,4), (4,6)}`. Null-space omission, one-step estimation, plain
residual permutation, and a single global shuffle used for every root are
retained adverse counterfactuals.

The sequential hypotheses are

```text
Hk: rho(k) = rho(k+1) = ... = rho(K) = 0.
```

Rejection of `Hk` supports at least `k` detectable dimensions inside the fixed
candidate subspaces. Nonrejection is not a global upper bound on brain/task
association and does not count anatomical networks.

## 5. Candidate compact-space rank procedure

M4.07 must implement and M4.09 must independently calibrate this candidate
before rank significance is available:

1. Learn the candidate brain and target projections only in discovery data.
   Apply those fixed projections to confirmation data.
2. Center confirmation matrices and bind all nuisance designs. Include the
   null-space complements of the canonical coefficient matrices; dropping the
   complement is an explicit invalid counterfactual.
3. With no nuisance, operate on exchangeable independent rows. With nuisance,
   construct the Huh--Jhun semi-orthogonal residual basis. When declared
   exchangeability blocks require the Theil/BLUS route, freeze the full-rank row
   selection before outcomes are read and use the same compatible selection on
   both sides.
4. Fit the observed CCA. For each `k`, refit CCA stepwise using canonical
   variables `k..P` and `k..Q`, so variance represented by modes `1..k-1` is
   treated as already-explained nuisance and is not returned to the shuffled
   null problem.
5. For each legal replicate, transform one side in the exchangeable residual
   basis, repeat the stepwise fits, and compute the Wilks statistic
   `Tk = -sum(i=k..K, log1p(-rho(i)^2))`. Larger is more extreme.
6. Compute fixed-`B` plus-one p-values and closed-testing adjusted values
   `pAdjusted(k) = max(p(1), ..., p(k))`. Reject sequentially only until the
   first nonrejection.

This follows the stepwise, null-space-complete, closed-testing construction of
Winkler, Renaud, Smith, and Nichols,
[Permutation inference for canonical correlation analysis](https://doi.org/10.1016/j.neuroimage.2020.117065)
(NeuroImage 2020; [open manuscript](https://pmc.ncbi.nlm.nih.gov/articles/PMC7573815/)).
That paper shows why simple residual permutation can violate exchangeability
and why one-step permutation is invalid beyond the first canonical
correlation.

The candidate assumes fixed independent discovery subspaces, finite matrices,
adequate residual rank, and genuinely exchangeable independent rows or a valid
predeclared block action after nuisance handling. Row-wise transforms of
temporally dependent fMRI are unavailable. If no valid action exists, the
result is `RankInferenceUnavailable`; a global permutation that erases every
association is permitted only for the rank-zero null and cannot be substituted
for `H2` or higher.

## 6. Temporal, repeated, and nuisance rules

For independent Gaussian confirmation rows with a fixed full-rank nuisance
design, admitted small-design t/F calculations may be calibrated against exact
matrix formulas. Estimated temporal covariance, censoring, and run pooling are
part of the stochastic procedure, not metadata that makes rows exchangeable.

The initial supported temporal route may do either of the following, but each
is separately calibrated:

- fit the declared covariance model and use a qualified analytic or parametric
  reference conditional on its stated assumptions; or
- reduce to run- or subject-bound sufficient statistics and resample whole
  independent units with a qualified sign, wild, or bootstrap action.

Residualization alone never licenses arbitrary permutations. A within-subject
action preserves all rows of a subject and the declared session/run structure.
For `REPEAT24` and `REPEAT80`, population inference resamples subjects; sessions
and runs are repeated measurements. A single subject or six runs cannot be
promoted to an independent-subject population result. Unsupported covariance,
too few legal transformations, rank loss after censoring, or incompatible
blocks produce typed unavailability before any p-value is returned.

## 7. Group and uncertainty calibration

Group qualification uses subject-bound effects, covariance, component/task
identity, anatomical measurement identity, and known/estimated/approximate or
unknown uncertainty provenance. Equal shapes are insufficient. Unknown
first-level degrees of freedom remain unknown; they are never reconstructed
from a standard-error array.

The group truth grid contains `n` in `{8, 20, 80}`, mean standardized effects
in `{0, 0.20, 0.50}`, true between-subject variance in `{0, 0.04, 0.20}`, and
sampling-variance profiles:

- `equal`: every variance is `0.25`;
- `spread`: equally spaced from `0.04` through `1.00` by subject identity;
- `reverse`: the spread profile paired in reverse order with the nuisance
  leverage ordering; and
- `estimated-df`: each declared variance is multiplied by an independent
  `chiSquared(df) / df`, with `df` in `{8, 25, 50}`.

Nuisance designs include intercept-only, balanced binary, quarter/reverse
leverage, and three-column continuous designs. Innovation families are
Gaussian, symmetric Student-t(5), and centered skewed log-normal. Coverage,
null rejection, effect bias, empirical-to-reported variance, finite failure,
and effect/variance dependence are all retained by cell. Group fitting never
receives the simulation truth as a feasible input.

### 7.1 Mandatory historical adverse cases

These cases are retained even if their originating candidate is not on current
main. They are evidence against silently choosing a familiar default, not
proof that every related method fails.

| ID | Frozen adverse case | Existing owner |
| --- | --- | --- |
| `GADV-DL-PM` | `n=8`, null mean, known unequal variances `0.04..1.00`, `tauSquared=0.20`; compare named DL/z, DL/mKH, and PM/mKH candidates | Group baseline `bd-01M20TKX7VYFT3BHCM7QAMA6NG` |
| `GADV-DF8` | `n=80`, three regressors, reverse variances, `tauSquared=0`, first-level `df=8`; retain known-variance and estimated-variance controls | Group baseline and first-level calibration `bd-01M21VA8DYMNJS9PZ8076SBWWJ` |
| `GADV-NUISANCE` | `n` in `{20,80}`, quarter/reverse leverage, Gaussian and skewed errors; retain CR2/Satterthwaite and null-restricted HC2 wild candidates as adverse comparators | Group baseline `bd-01M20TKX7VYFT3BHCM7QAMA6NG` |
| `GADV-FEASIBLE-WEIGHT` | The full 24-cell Gaussian estimated-SE grid; retain equal-subject HC3 and feasible inverse-estimated-variance HC3 results, including conservative cells and power loss | Joint bootstrap research `bd-01M21BNZR9ZBRAYY9JD5WCQ8KX` |
| `GADV-FIRSTLEVEL` | HRF and FIR, OLS and shared/voxelwise estimated-AR GLS, censoring, unequal runs, and every admitted run-combination strategy; measure both moment and inverse-moment effective df | First-level calibration `bd-01M21VA8DYMNJS9PZ8076SBWWJ` |
| `GADV-MAXNULL` | Null draws `1..19`, `alpha=0.05`, observed `19` and `nextUp(19)`, plus ties and finite extrema; cutoff decisions must equal public `adjustedP <= alpha` decisions | Threshold audit `bd-01M23ZX1KQB38EPD7W3PM56SE9` |

The conditional historical `dCritical=12` result remains a diagnostic for its
tested group model and grid. It is not a universal first-level-df cutoff. If an
admitted method assumes a scaled-chi-squared variance model, both
`dEffective = 2 * mean(vHat)^2 / variance(vHat)` and the inverse-moment
`dInverse` are reported. A 90% interval for their ratio must lie inside
`[0.90, 1.10]` before one shared df approximation can be used; otherwise the
approximation is unavailable and direct calibration of the joint
effect/variance procedure is required.

## 8. Replicate counts and decision rules

All thresholds in this section precede new unified-MVPA results.

### 8.1 Fixed budgets

| Phase | Independent simulated datasets per cell | Random non-identity transforms per dataset | Admission use |
| --- | ---: | ---: | --- |
| Exact fixture | 1 | Enumerate the complete finite group where feasible | Algebra, identity, tie, and external-oracle tests only. |
| Simulator QA | 10,000 | 0 | Generator moments, covariance, independence, censoring, and design validation only. |
| Pilot | 200 | 199 | Runtime sizing and gross-defect detection only. Never an admission result. |
| Null/rate confirmation | 10,000 | 1,999 | Type-I, FWER, FDR, coverage, refusal, and failure gates. |
| Alternative confirmation | 5,000 | 1,999 | Power, bias, interval width, and monotonic-effect summaries. |

The observed identity plus 1,999 sampled non-identity transforms gives 2,000
reference values. Monte Carlo p-values are
`(1 + count(Tstar >= Tobserved)) / 2000`. Exact enumeration includes the
identity and divides by the complete group size. Ties use the public method's
declared inclusive extremeness rule. A method cannot lower `B`, pool incomplete
families, or add replicates after inspecting a borderline cell. If M0.06 finds
the fixed confirmation budget infeasible, the claim stays unavailable until a
new protocol version is approved; the threshold is not loosened post hoc.

### 8.2 Admission decisions

- **Pointwise type-I and FWER:** for every primary supported null cell, the
  two-sided 90% Clopper--Pearson interval for the rejection rate lies wholly
  within `[0.035, 0.065]` at nominal `alpha=0.05`. No averaging across cells.
- **Coverage:** the two-sided 90% Clopper--Pearson interval for nominal 95%
  interval coverage lies wholly within `[0.935, 0.965]`.
- **FDR:** in each partial-null family, the upper one-sided 95% deterministic
  nonparametric-bootstrap bound for mean realized FDP is at most `0.060`.
  The all-null FDR procedure also passes the FWER gate.
- **Bias and reported variance:** the 90% bootstrap interval for standardized
  coefficient bias lies within `[-0.05, 0.05]`, and the interval for
  `mean(reported variance) / empirical variance` lies within `[0.90, 1.10]`.
- **Power:** weak effects are reported without a pass threshold. In the primary
  IID standard-effect cells (`|A|=0.30`, association `rho=0.30`, incremental
  `Delta R-squared=0.04`, or newly tested canonical root `rho=0.20`), the lower
  one-sided 95% Clopper--Pearson bound is at least `0.80`. The 95%
  bootstrap interval for strong-minus-weak power must be entirely positive.
- **Rank partial nulls:** `R1/H2`, `R2/H3`, and `R3/H4` each pass the same
  pointwise type-I gate while earlier nonzero roots remain in the population.
  The closed sequence passes the FWER gate under `R0`.
- **Refusal and completeness:** an unsupported design returns typed
  unavailability in all 10,000 confirmation replicates and never emits a
  finite inferential result. An admitted family has zero silent omissions and
  zero non-finite p-values. Any local failure is retained and marks the family
  incomplete.
- **Threshold decisions:** exact decision equality between threshold/cutoff and
  adjusted-p routes is required for every finite exact fixture, including ties
  and attainable-alpha boundaries. A Monte Carlo rate cannot compensate for a
  deterministic disagreement.

Bootstrap intervals use 9,999 deterministic dataset-level resamples from a
named child seed. They resample independent datasets, not voxels, folds, runs,
or components. Every interval method and tail is stored in the receipt. Stress
cells outside a method's declared assumptions must either pass the same gates
or refuse; they cannot be omitted from the report or counted as evidence for a
broader claim.

## 9. Independent evidence and simulator gates

Before a candidate implementation sees confirmation streams:

1. An R or NumPy generator that does not call ScalaFIM fit, CCA, group, or
   threshold helpers produces population moments and small exact fixtures.
2. Exact small-design coefficient, covariance, t/F, Wilks, rank, and max-null
   calculations agree with an independent implementation at combined absolute
   and relative tolerance `1e-10`, tightened where exact rational enumeration
   is available.
3. The 10,000-dataset `simulator` runs recover means within `0.02` residual SD
   and every declared target covariance plus residual-block lags zero through
   two within `0.03` absolute error; noiseless signals recover to `1e-12`
   combined tolerance.
4. Temporal generators recover each declared AR coefficient within `0.03`,
   reproduce censor/run identities exactly, and keep independent runs and
   subjects independent by construction.
5. Scenario, generator, provider, source, and protocol hashes are recorded
   before the confirmation command is enabled.

The independent oracle and the implementation may share the written
mathematical specification and serialized inputs, but not the same numerical
helper. A green JVM/Scala.js unit suite is engineering evidence, not the
Monte Carlo admission result.

## 10. Future artifacts and exact commands

The implementation packets own these planned paths:

- `tools/mvpa-inference/generate_known_truth.R`
- `docs/scenarios/fixtures/mvpa.inference-known-truth.v1.r.json`
- `docs/audits/unified-mvpa-inference-calibration-v1.json.gz`
- `modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/inference/FixedRepresentationInferenceSuite.scala`
- `modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/inference/RankConfirmationSuite.scala`
- `modules/group/shared/src/test/scala/scalafim/group/UnifiedMvpaGroupCalibrationSuite.scala`

M4.04, M4.07, M4.09, and M5.02 must preserve or explicitly version these
commands. Platform runs remain bounded so Scala.js linking does not accumulate:

```sh
LC_ALL=C LANG=C Rscript tools/mvpa-inference/generate_known_truth.R --check
sbt "mvpaJVM/testOnly scalafim.fmri.mvpa.inference.FixedRepresentationInferenceSuite"
sbt "mvpaJS/testOnly scalafim.fmri.mvpa.inference.FixedRepresentationInferenceSuite"
sbt "mvpaJVM/testOnly scalafim.fmri.mvpa.inference.RankConfirmationSuite"
sbt "mvpaJS/testOnly scalafim.fmri.mvpa.inference.RankConfirmationSuite"
sbt "groupJVM/testOnly scalafim.group.UnifiedMvpaGroupCalibrationSuite"
sbt "groupJS/testOnly scalafim.group.UnifiedMvpaGroupCalibrationSuite"
```

The future confirmation driver must require a frozen manifest and reject pilot
or source hashes. Its receipt reports every cell, decision interval, local
failure, refusal, budget, completed replicate count, elapsed/CPU evidence,
provider revision, and exposure event. M0.06 owns resource approval; resource
success cannot waive a scientific gate, and a scientifically valid but
infeasible procedure remains unadmitted for the corresponding product route.

## 11. Handoff boundary

M0.05 closes only the specification task. M4.04 owns discovery/confirmation
execution and nuisance binding; M4.05 and M4.06 own voxel/component procedures;
M4.07 owns sequential rank; M4.08 owns family assembly; M4.09 owns independent
calibration; M5.02 owns group bridging. Existing group, uncertainty, and
threshold tickets retain their native ownership and adverse evidence.

Changing a null, independent unit, effect size, scenario, seed namespace,
replicate count, interval, decision band, or supported assumption after pilot
requires a documented protocol revision. Changing one after confirmation
exposure also requires new confirmation streams. This prevents implementation
success, runtime pressure, or an attractive scientific result from rewriting
the criterion it was supposed to meet.
