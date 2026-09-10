# ProfileHrf plan review

Date: 2026-09-10. Reviews [profile-hrf.md](profile-hrf.md) and
[profile-hrf-work-items.md](profile-hrf-work-items.md) for accuracy, CPU
performance and fit inside scalafim. Repository facts below were checked
against the current tree and the pinned Gale revision `d55fe2f9`; cost figures
are estimates from operation counts, not measurements.

## Verdict

The algebra is right and the corrections (ridge-and-release, the full-data
determinant, the coupled normalization/penalty units, the `transportPenalty`
sign error) are real and worth keeping. The plan's problems are structural:

1. It imports execution contracts from the trial regime (two evaluated
   references, no iteration, bounded router, pilot-only pooling, stencil jets)
   into the condition regime, where the operation counts make them
   unnecessary and they cost accuracy. In compact coordinates a full grid scan
   plus a safeguarded Newton refinement costs about as much as the projection
   itself.
2. The compact condition backend is described as a new compiler with its own
   observable-family dictionary, but it factors as **an ordinary basis GLM
   followed by a per-voxel post-solve in coefficient space**. Most of it already
   exists in `design`/`fit`.
3. Several placement facts are wrong and one dependency is missing entirely:
   `gale.sparse.Banded` has no factorization, solve or log-determinant at the
   pinned revision, and `build.sbt` has no local-override hook for Gale.
4. The seven-state `FiniteState` backend is a third engine for one family. The
   family itself is fine; the backend should be deferred until the banded
   backend's measured crossover shows it is needed.

Recommended shape of the change: keep the model, criteria and laws; replace
the compact condition compiler with the kernel-basis formulation below; convert
every hard structural cap into a work-counter budget; defer PHRF-10; fix the
placement table; add the missing build/infra tickets.

## 1. Kernel-basis factorization: the one change that simplifies everything

Every admitted family is a scalar kernel `h_theta(t)` on a truncated support
`[0, T_h]`. Sample `h_theta`, its first and mixed second parameter derivatives
on a fine `theta` grid over the declared domain at the kernel's fine time
resolution, and take the SVD of that sample matrix. The leading `m` left
singular vectors `Phi` (`T_h_fine x m`) give

```
h_theta  ~=  Phi c(theta),      c(theta) = Phi' h_theta,
c_p(theta) = Phi' d_p h_theta,   c_pq(theta) = Phi' d_pq h_theta.
```

This is the FLOBS construction (Woolrich, Behrens and Smith, 2004) and the
LS+SVD step of the user's own `hrfals`; `manifold_hrf` builds the same object
with a diffusion map. Nothing here is new, which is the point.

Consequences for the condition model, `B(theta) = [S_1 h_theta ... S_C h_theta]`
with `S_c` the (pulse-aware, run-aware) convolution operator of condition `c`:

```
B(theta) = [S_1 Phi ... S_C Phi] (I_C kron c(theta))  =  A_tilde (I_C kron c(theta))
```

`A_tilde` is a fixed `T x Cm` design. After the shared whitening and nuisance
projection it has a thin QR `A_tilde = U R` with `K <= Cm`. The compact design
of the plan is then `D(theta) = R (I_C kron c(theta))`, exactly, with exact
derivative jets `D_p = R (I kron c_p)`, `D_pq = R (I kron c_pq)`. Everything
the plan labels "compile a bounded-rank union of designs and relevant shape
directions", "stencil jets", "continuous interpolation representation" and
"unseen-family-point validation" collapses to one object: `Phi`, whose
approximation error is a one-dimensional kernel-fit error certifiable by dense
sampling of `theta` in seconds.

Further consequences:

- **Continuous readout is exact within the basis at every `theta`**, not only
  at prepared references. The "prepared versus continuous" distinction and its
  receipts disappear for the condition backend.
- **The observable rank `K` is `C*m` by construction.** For LWU/Gaussian at
  1e-4 relative kernel error over a physiological domain, expect `m` around
  12 to 20, so `K` around 36 to 60 at `C = 3`. The demo's `K = 69` was a
  dictionary artifact. `C = 8` at `K <= 96` needs `m <= 12`, which is a
  measurable accuracy statement rather than a guess.
- **Normalization cancels in shape scoring at `alpha = 0`.** The projector onto
  `col(D(theta))` is invariant to a positive scalar `s(theta)`. Score shapes on
  the unnormalized kernel and apply `s(theta_hat)` only at amplitude readout.
  This removes normalization derivatives and the nonsmooth peak-normalization
  problem from the decoder entirely. It does not cancel in the trial model
  because `lambda` couples to units; there, define `lambda` in unnormalized
  kernel units and convert once.
- **The trial design factors the same way**: `X(theta) = X_tilde (I_N kron c(theta))`
  with `X_tilde` a sparse `T x Nm` trial-basis design. `X'X + lambda I` becomes
  a quadratic form in `c(theta)` over `m(m+1)/2` precomputed banded `N x N`
  blocks, all shared across voxels. Per voxel the only new work is the
  `N*m` trial-basis scores `X_tilde' W y`.
- **`Cascade34` becomes a kernel like any other** and runs through both
  backends immediately. The exact finite-state realization is then a
  performance option for the dense-overlap trial case, not a prerequisite.

Tail truncation is the one real cost of this view. Erlang and LWU undershoots
decay slowly: `g_4(t; 0.2/s)` at `t = 32 s` is still about a third of its peak.
`T_h` must follow from the family domain (60 s for slow undershoots), and `m`
grows with it. Record `T_h` and the tail bound in the basis provenance.

## 2. The condition backend is a basis GLM plus a coefficient post-solve

With `Phi` in hand, the per-voxel pipeline is:

1. Fit the ordinary fixed-design GLM with design `[F, A_tilde]` under the
   shared whitening. This is the existing `fit` engine with a `ResponseBasis`
   of `m` columns per condition. It already handles AR whitening, censoring,
   chunking, rank diagnostics and provenance. Its output is the `Cm` coefficient
   vector `c_hat` per voxel, or equivalently `z = U' y_tilde` and `e`.
2. Post-solve in coefficient space: maximize over `theta` the projected energy
   `||P_{D(theta)} z||^2`, then read `beta` from a `K x C` least-squares solve.

Step 1 must retain sufficient statistics (Gram, cross-products, response
squares, non-task rank, as the working tree's `BasisExpandedFitProduct`
already does) or a rank-revealing `(U, R, z, e)`. It must not require unique
unrestricted coefficients: `A_tilde` can be rank deficient while `D(theta)` is
identifiable at a shape. Step 2 costs, per voxel, at
`T = 600, C = 3, m = 16, K = 48, d = 3`:

| Operation | MACs | Notes |
| --- | ---: | --- |
| GLM projection `U' y` (existing engine) | ~32k | includes 6 nuisance columns |
| Value scan over `G = 64` grid nodes | ~9k | `G * K * C`, one GEMM per voxel block |
| Two second-order jets | ~3k | ten `C`-vectors, each an `m`-contraction |
| Up to five exact re-evaluations at continuous `theta` | ~14k | `K*m*C` plus a `K x C` QR each |
| Amplitude readout and nuisance recovery | ~1k | |

The post-solve is roughly equal to the projection. At `V = 100,000` that is
about 3 GMAC after the GLM, well under a second on one JVM core. The 30-second
C0 target is therefore soft by an order of magnitude and the binding constraint
is reading 480 MB of float64 input (240 MB float32). Keep the absolute
30-second and 120-second targets as gating figures and report the ratio to the
fixed-HRF basis GLM alongside as an overhead diagnostic. A ratio alone is not
enough: a larger basis inflates the denominator. These MAC counts are
estimates, not times; the spike measures them.

What this removes from the plan: the observable-family compiler with streamed
candidate blocks (PHRF-18 shrinks to "build `Phi`, certify it, wrap it as a
`ResponseBasis`"), the bounded router and the two-reference cap (PHRF-09
becomes "grid scan plus bounded refinement"), the runtime stencil discussion,
and the pilot-only pooling restriction. What it keeps: `ProfileJet`, the
constrained decoder for `d <= 3`, receipts, the status ADT, sinks.

Memory: the `Cm x V` coefficient matrix that the plan forbids as "mandatory
`K x V` state" is 38 MB at `K = 48, V = 100,000`, and is exactly the coefficient
output the existing engine already materializes. Keep the prohibition for the
trial regime, drop it for conditions.

## 3. Accuracy

- **Replace routing with a grid scan, with backend-specific budgets.** A
  value-only scan over all bank nodes gives the global on-grid maximum for
  every voxel. Sixty-four compact condition scores and sixty-four trial-profile
  evaluations are different workloads; budget them separately. Ambiguity becomes a
  reportable quantity (energy gap between the best and second-best basin)
  instead of a routing failure. The `<= 2 evaluated references` rule should be
  rewritten as a per-voxel work budget: `<= 64` value evaluations, `<= 2` jets,
  `<= 6` exact evaluations. Caps on counts, not on structural form.
- **Safeguarded Newton with exact re-evaluation.** Because the compact energy
  is exactly evaluable at any continuous `theta` for about 3k MACs, every Newton
  step can be verified against its quadratic prediction and rejected on
  overshoot. This is the plan's "residual certification", but it costs nothing
  and certifies the actual objective rather than an interpolated one.
- **Free-form fit as an adequacy diagnostic, SVD as an initializer only.**
  The unrestricted basis fit's energy bounds the parametric fit from above; the
  calibrated gap says when Gaussian or LWU is the wrong family, not just when
  the fit is weakly identified. The top singular pair of `c_hat` reshaped
  `m x C` (the `hrfals` LS+SVD step) is a derivative-free initializer: pick the
  grid node whose `c(theta_g)` has the largest cosine with it. It is not an
  optimal shared-shape fit or an energy bound, because the fitting metric is
  the Gram-weighted distance, not Euclidean coefficient distance; a
  Gram-metric rank-one fit needs bounded alternating least squares (CF-ALS).
- **Sigma.** At `alpha = 0` with no prior, shape decoding is invariant to
  `sigma`; it only sets the prior's relative weight, which is exactly why it
  must stay frozen from an independent preparation step. The nonlinear
  least-squares plug-in `RSS(theta_hat) / (T - rank(F) - C - d)` is a
  selection-dependent estimate; report it as a labeled conditional quantity
  and calibrate its coverage before admitting it. Drop the complement-space
  estimator.
- **Half-cosine.** Grid scan plus quadratic interpolation through neighboring
  nodes is derivative-free and sidesteps the C2 join problem; analytic jets are
  a refinement, not a prerequisite. This also removes PHRF-03's second
  derivatives from the condition milestone's critical path.
- **Trial regime.** The plan's "no per-voxel factorization" rule is a cost
  belief, not a requirement: a per-voxel banded Cholesky of `X'X + lambda I` at
  the decoded `theta_hat` costs `N b^2` = 120k MACs at `N = 300, b = 20`. That
  makes exact readout at the decoded shape a measured option at B0 and the
  one-correction rule a crossover to report, valid for `N = 1,200` and wide
  bands.
- **Pooling.** In compact coordinates the pooled node score of a region is
  `sum_v ||Q_g' z_v||^2`, 64 doubles per region, accumulated in the same GEMM
  as the per-voxel scan. Full-cohort empirical-Bayes priors are free; the
  2,048-voxel pilot is a trial-regime compromise that the condition backend
  does not need. A second pass with the frozen regional prior needs the stored
  `z` (`K x V` doubles) rather than a re-read of the data; count that storage
  and the second-pass reads, they are cheap but not free. Keep per-block partial sums reduced in
  a fixed order so chunk order cannot change the frozen prior.

## 4. Performance tricks, in priority order

1. **Kernel basis `Phi`** (section 1). Biggest win, removes the compiler.
2. **Coefficient-space post-solve on the existing GLM** (section 2).
3. **Stacked-node GEMM.** Precompute `Q_g = R (I kron c_g) G_g^{-1/2}` for all
   `G` nodes, stack `Q_g'` into one `(G*C) x K` matrix, and score a block of
   `Vb` voxels as one `(G*C) x K` by `K x Vb` product. BLAS-3 shape, register
   tiled, `while` loops, `Vb = 256` keeps the block in L2.
4. **Score shapes unnormalized; normalize at readout** (section 1).
5. **Nested bases.** Order `Phi` by singular value; a coarse scan can use the
   first `m_0` rows of `z` and the fine refinement the full `m`.
6. **Voxelwise AR without per-voxel Grams.** For AR(p) with voxel coefficients
   `phi_v`, `(W_v A)'(W_v A) = sum_{j,k} phi_j phi_k (L_j A)'(L_k A)`. Precompute
   the `(p+1)(p+2)/2` lag Grams once; assemble per voxel in `O(p^2 K^2)`, about
   14k MACs for AR(2) at `K = 48`. This lifts the "one shared W0" restriction
   cheaply in a later release. It does not help per-voxel masks; keep the
   shared-mask restriction.
7. **Trial backend Gram blocks.** `X_tilde' X_tilde` as `m(m+1)/2` banded
   `N x N` blocks (12 MB at `N = 300, b = 20, m = 16`), so each reference's
   `G(theta)` is a `c(theta)`-weighted sum plus one banded Cholesky, all
   shared. Per voxel: `N*m` scores, `O(N b)` solves. B0 estimate is 250k
   MACs per voxel, about 25 GMAC total, 5 to 10 seconds on one core.
8. **Volume-major projection.** If the data source is volume-major (NIfTI), the
   projection `U' Y` is a rank-`K` accumulation over volumes and never needs a
   transpose; the `K x V` accumulator is the coefficient output.
9. **Float32 input, float64 accumulation.** Halves memory traffic, which is the
   actual bottleneck.
10. **JS.** Confirm what the linker emits for `Array[Double]` at the project's
    ES target (typed arrays under ES2015+, plain JS arrays otherwise); Gale
    keeps an explicit `Float64Array` opaque type in `PlatformArrays` for this
    reason, and hot kernels should go through it if the check is negative. Do
    not use `Math.fma` in shared loops: Scala.js emulates it in software. Use
    Gale's `PlatformMath.fma` if fused accumulation is wanted.
11. **Defer `FiniteState`.** Its per-voxel cost is `O(D T r^2)` = 294k MACs at
    `r = 7, D = 10`, the same order as the banded backend, and it is a full
    Kalman engine with derivative states, event covariance and off-grid
    transitions for one family. Ship `Cascade34` as a kernel first.

## 5. Architecture fit: what the survey found

Confirmed by the code:

- `FitPlan` (`modules/model/.../FitPlan.scala:15`) is a product over a
  materialized `FmriModel`; the sum is `FitStrategy` (`FitConfig.scala:1095`).
  A shape-varying operator has no home there. The plan's sibling
  `ProfileHrfPlan` is right for the trial regime. For the condition milestone
  the basis-GLM formulation needs **no new strategy case at all**: it is the
  existing engine with a `ResponseBasis` plus a post-solve stage over
  coefficient blocks.
- `TrialReadout` (`modules/fit/.../TrialReadout.scala:77-82`) documents a
  response-independent linear map and its method enum has one case. The plan's
  refusal to reuse it for `y -> a(theta_hat(y))` is correct; `mvpa-fit` consumes
  it through `RunTrialReadout` (`OneShotDataset.scala:53`).
- `transportPenalty` (`modules/hrf/.../Basis.scala:512-534`) applies `S Q S`
  where a coefficient penalty needs `S^-1 Q S^-1`; `transportCovariance`
  (`:557`) is correct. PHRF-17 stands.
- `LwuBasis.normalizePrimary` scales only column 0; `Deriv` differentiates lag,
  not parameters; `Hrfs.lwu` inherits `Support.Unbounded`. All as stated.
- Chunk interpreters collect a `Vector` of all pieces; there is no sink
  anywhere in `fit`. Receipt types exist (`TemporalPreparationReceipt`,
  `TrialReadoutReceipt`, `VolumeWeightingReceipt`) and are the right vocabulary.
- AR whitening (`modules/ar/.../WhiteningPlan.scala:233-264`) is already a
  causal short-memory filter per segment, so it preserves bandedness for pure
  AR. The MA branch is recursive on the output and does not; the plan should
  say "pure AR only" where it claims locality.

Wrong or missing in the plan:

- **`ResponseBasis` and `BasisTransform` live in `modules/hrf` (`Basis.scala`),
  not `modules/response`.** `modules/response` is the response-block IO layer.
  `RunTrialReadout` is in `modules/mvpa-fit`. Fix the placement table.
- **Gale at `d55fe2f9` has no banded factorization, solve or log-determinant.**
  `gale.sparse.Banded` offers matvec, transpose matvec and constructors only;
  `Cholesky` has `solve` but no `logDet`. Nothing in scalafim references
  `Banded`. PHRF-06 is not "reinspect the pin"; it is "write banded SPD
  Cholesky, multi-RHS solve and log-determinant upstream", per the
  no-private-solver rule in AGENTS.md.
- **`build.sbt` has no `scalafim.gale.build` override property**, unlike Ravel
  and locus4s. Upstream Gale co-development is blocked by the build until that
  is added. This is a concrete ticket and belongs before PHRF-06/07.
- **No vectorized HRF evaluation exists.** `Hrf.evaluateInSupport` is
  scalar-lag-at-a-time returning an allocated `Vec`; `eval` builds a `Vector`
  then copies. The kernel-basis builder needs an `evalInto(times, out)` on
  `ScalarHrf` plus parameter-derivative evaluation. Neither is a work item.
- **Concurrency.** `FitParallelism` is barrier-batched `Future.sequence`, not a
  bounded work queue, and `modules/fit/js` has no sources. The "8 workers of
  256 voxels" budget presumes an executor that does not exist. A bounded
  work-stealing loop over blocks is a small JVM-only addition.
- **JS benchmark harness** does not exist; JMH projects exist for `fit` and
  `hrf` on the JVM only (`benchmarks/fit-jvm`, `benchmarks/hrf-jvm`). PHRF-20
  should say JMH for JVM and a hand-rolled `munit`-driven timer for JS.
- `modules/linalg` and `modules/linalg-breeze` are stale `target/` directories
  absent from `build.sbt`; the plan's mention of Breeze behind `jvm` refers to
  nothing current.

Placement recommendation with the kernel-basis formulation:

| Piece | Module | Why |
| --- | --- | --- |
| Parametric family contract: validated params, chart, `evalInto`, first and mixed second parameter derivatives, support, normalization rule; `Cascade34` kernel | `hrf` | No Gale dependency in `hrf`; keep it scalar and allocation-free. |
| `Phi` builder (sample family, SVD, tail/error certificate) wrapped as a `ResponseBasis` with family/domain provenance | `design` | `design` has Gale and `Regressor.prepareConvolution` already samples kernels at fine resolution. |
| Condition post-solve: `ProfileJet`, decoder, grid scan, readout, statuses, pooling accumulators | `fit/profile` | Matches the `LatentSketch`/`ReducedRankGls` precedent of algorithms inside `fit`. |
| Trial banded backend, sinks, `ProfileHrfPlan` | `fit/profile` and `model` | As planned, after Gale banded lands. |
| Laws | `first-level-laws` | Already depends on `fit` and `hrfLaws`; `GeneratedLawSuite` with `NumericalEvidence` budgets is the right pattern. |
| Banded SPD Cholesky, multi-RHS solve, logdet | Gale | Generic numerical capability; not allowed here. |

Whether `fit/profile` later deserves its own cross-project depends on whether
the trial backend brings a Gale dependency `fit` does not already have; it does
not, so staying inside `fit` is consistent with the repo.

## 6. Process and document

- **Split the normative contract from the rationale.** The plan is 1,100 lines
  and the index 400; an implementer needs about 200 lines of contract. Move the
  review evidence, demo hashes and the supplied-artifact table to an appendix,
  and the spatial-manifold and fingerprint-geometry paragraphs to a separate
  downstream note. They are not engine requirements.
- **PHRF-01 is a freeze-everything ticket before any code.** Replace it with a
  one-week spike: `Phi` for the causal Gaussian, the basis GLM through the
  existing engine, the grid scan and one Newton step on JVM and JS, with the
  direct time-domain oracle. That spike settles `m`, `K`, tail bounds and the
  cost model with evidence and makes the remaining freezes short.
- **Reorder the critical path** for the condition milestone: family contract
  and `evalInto` (hrf), `Phi` as `ResponseBasis` (design), post-solve
  (fit/profile), sinks, laws, PHRF-21. PHRF-03's second derivatives, PHRF-09's
  router, PHRF-18's compiler and all of PHRF-04/06/07/10 leave the critical
  path.
- **Add tickets** for: Gale local-override property in `build.sbt`; upstream
  banded SPD factorization; `ScalarHrf.evalInto` and parameter derivatives; a
  bounded block executor; a JS timing harness; sink abstraction with receipts.
- **Rewrite caps as counters.** Every "at most two references", "no iteration",
  "no per-voxel factorization" becomes a measured budget with a default and a
  reported actual. Structural prohibitions cost accuracy and are the wrong
  place to enforce cost.

## 7. What to keep unchanged

Ridge-and-release; the full-data determinant identity and the projected
counterexample; explicit `PenalizedProfile` versus `TrialRandomEffectsML`;
frozen `sigma` for the trial criterion; the amplitude-structure ADT with finite
positive `alpha`; the `E = s - b' G^-1 b` jet formulas and the `s_p` regression;
the status ADT and independent identification/approximation axes; signed
queries compiled as bounded `T x J` factors; the sink-and-receipt executor;
`Cascade34` as a continuous-time family with intrinsic positive-component-area
normalization; the validation matrix's four separated comparisons; the rule
that unmet targets stay unmet.
