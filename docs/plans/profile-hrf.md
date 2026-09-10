# ProfileHrf: compiled condition and trial response fitting

Status: consolidated implementation plan, 2026-09-10. This unifies the supplied
condition, trial, innovation and alignment proposals; it does not implement a
production estimator. The existing Mote epic is
`bd-01M24MQABKEBQXW89VZ8XWBB8H`. Its executable order and acceptance criteria are
indexed in [profile-hrf-work-items.md](profile-hrf-work-items.md).

## Decision

Build **one scientific model, one criterion, one bounded shape decoder and one
conditional-readout contract**, with specialized compact-condition, trial-banded
and exact finite-state backends. Keep one shape per voxel across the selected
conditions/trials; keep amplitudes signed and local. Shape sharing within a voxel
and borrowing shape evidence across voxels are separate policies.

Condition-only fitting is the exact zero-trial-variance specialization. Compile
it directly from aggregated condition drives, without trial-sized designs,
factors, covariance preparation or workspaces. Deliver and qualify this path
before requiring the trial backends. Use the existing epic, with a separately
closable condition milestone; do not create a competing engine or top-level module.

Extreme CPU performance and accuracy are co-gates. Preserve shared preparation,
at most two evaluated references per voxel, bounded quadratic decoding, at most
one trial-amplitude correction, and streaming. Introduce measurements before
backend implementation, rather than postponing performance evidence to release.
Condition compilation must reduce the supplied demo's center/stencil scoring;
its 60-center runtime is not the production execution policy.

The central algebra is correct. Exact local differentiation does not make the
one-shot nonlinear estimate exact. The release needs separate evidence for
family fidelity, conditional solves, shape approximation, identification,
statistical calibration, and resource bounds.

Do not make the seven-state family the scientific default merely to obtain
the fast backend. Gaussian and LWU must remain their specified kernels;
half-cosine must retain its piecewise timing semantics. Do not assume an
earlier compact compiler or quadratic decoder exists: no `ProfileHrf` engine,
compiled likelihood, or corresponding decoder was found in the inspected
source.

This is a specialized application of variable projection: eliminate linear
parameters before optimizing nonlinear ones. The reusable contribution here
is preparation shared across voxels, structure-aware elimination, and bounded
decoding/readout. Describe it in that context rather than claiming linear
elimination itself as a new statistical method. [O'Leary and Rust](https://www.cs.umd.edu/users/oleary/software/varpro.pdf)
provide the relevant variable-projection background.

## Repository placement

The existing dependency path is `hrf -> design -> model -> fit`; `fit` also
depends on `ar` and standalone Gale. No new cross-project is needed initially.
Use the package `scalafim.fmri.fit.profile` consistently. The public model policy
is `ProfileHrf`; compiler/backend types start package-private. Reuse mechanisms
and scientific identities without forcing the backends into one matrix layout.

| Owner | Existing integration points | Proposed responsibility |
| --- | --- | --- |
| `modules/hrf/shared/.../scalafim/fmri/hrf/` | `Hrf`, `HrfDescriptor`, `HrfParams`, `Support`, `HrfNormalization`, `Pulse`, `PulseResponse`, `Primitive` | Constrained scalar families, parameter coordinates, normalized value/first/second shape derivatives, smoothness domains, optional realization recipes. No fitting or regional policy. |
| `modules/design/shared/.../scalafim/fmri/design/` | `event/EventSchedule`, `event/EventPhase`, `EventRowProvenance`, `TrialId`, `ConditionId`, `RowLayout`, `DesignFingerprint` | A validated `HrfDrivePlan` with condition aggregation and trial lowering: structural amplitude/shape-sharing axes, run identity, complete drives, acquisition geometry, and forward/adjoint derivative kernels. No fit-specific compression. |
| `modules/model/shared/.../scalafim/fmri/model/` | `FitStrategy`, `FitEngine`, `FitPlan`, `CoefficientScope`, typed controls | Declarative `ProfileHrfSpec` and `ProfileHrfPlan`: criterion, amplitude structure, family, noise preparation, prior, and execution budgets. |
| `modules/fit/shared/.../scalafim/fmri/fit/profile/` | `FitPlanExecutor`, `ResponsePreparationPlan`, `ObservationPattern`, `RankDiagnostics`, block/chunk execution | Observed-family compiler, common profile jets/criterion, reference routing, constrained decoder, pooling, conditional readout and sinks; specialized `CompactCondition`, `TrialBanded`, `FiniteState` loops. |
| Standalone Gale | `DoubleLinearOperator`, `Factorizations`, `gale.sparse.Banded` | Reusable banded SPD factors, multi-RHS/transpose solves, stable determinant and rank diagnostics. Generic innovation-filter machinery belongs upstream if exposed as a general numerical capability; HRF assembly and scientific policy remain here. |
| `fmri-workflow`, downstream consumers | Dataset/workflow adapters | Regional pilot preparation, typed spatial routing, storage/export adapters, training-fold boundaries. Core `fit` consumes frozen routing/prior inputs without an atlas dependency. |

Important source constraints:

- `FmriModel` currently materializes a fixed design and `FitPlan` assumes it.
  A shape-varying operator should first have a sibling `ProfileHrfPlan` in
  `model`; do not fabricate a dense reference design and pretend it describes
  every fitted voxel. Later dispatch integration must preserve this distinction.
  `FitInterpreter.prepare/fitChunk/merge` is useful lifecycle precedent, not a
  drop-in adaptive-plan API: it accepts fixed `FitPlan` and collects chunk results.
  Start with the sibling plan and sink executor; add dispatch only through a
  deliberate plan sum/adapter with fixed-plan compatibility tests.
- `TrialReadout` currently promises a **response-independent linear operator**
  and its method enum contains LSS. At a fixed shape our conditional readout is
  linear, but `y -> a(thetaHat(y))` is nonlinear. Reuse identity/block concepts,
  not the LSS method name or an unqualified global linear-operator contract.
  This also matters to `mvpa-fit` adjoints and cross-validation.
- `Deriv` differentiates lag/time. `LwuBasis` provides finite-difference first
  shape derivatives only, and its `normalizePrimary` option rescales only the
  primary column. Neither supplies consistent second derivatives of a normalized
  family. Do not repurpose basis coefficients as fitted trial shape parameters.
- Gaussian and LWU `Hrf` objects are causal through `Hrf.apply`, with
  `Support.Unbounded`; `span` is not a truncation guarantee. Half-cosine is
  explicitly compact. Low-level raw functions and R have intentional causality
  differences. The compiler must consume the public kernel/drive semantics.
- The current `galeRevision` is `d55fe2f97196a76ab7879e1a12f1e92403aeba06`.
  The earlier review inspected another revision. Reinspect the exact pin for
  banded factor/solve/logdet capability in PHRF-06. Generic QR/SVD/rank operations
  for condition compression also belong in Gale; condition qualification must
  not wait for unrelated banded-provider implementation.
- `BasisTransform.Diagonal` currently scales coefficients forward and also
  transports penalties forward (`Basis.scala`); its test repeats that rule.
  A coefficient penalty instead needs inverse congruence. PHRF-17 repairs this
  contract and tests invariant quadratic values. Keep covariance transport
  forward. `ResponseBasis` remains a fixed scientific basis, not a parametric
  shape family or an observed compact coordinate system.
- Existing chunk interpreters collect vectors of completed pieces. Reusing
  chunk scheduling does not by itself give bounded total memory: the new
  executor must flush payloads to a sink and retain only small receipts.

## Mathematical contract and corrections

### 1. Domain and estimand

After a declared, shared noise transformation, use

\[
y=F\gamma+X_\theta a+\epsilon,\quad
a=M\beta+u,\quad M^Tu=0,\quad
P=I-M(M^TM)^{-1}M^T.
\]

Expose an amplitude-structure ADT:

```text
ConditionMeans                         alpha = 0, u = 0 exactly
ConditionCenteredTrials(positiveAlpha)  alpha = 1/lambda > 0
```

`positiveAlpha` is finite and validated; callers do not pass infinity or an
epsilon cutoff to choose a model. For the first trial release, membership is
one-hot with exactly one condition per trial and every active condition has at
least one trial. Fractional membership or trial-specific penalties are different
contracts. Store membership as indices, never a dense projector.
Condition-only compilation accepts structural condition channels directly;
the mathematical trial expansion is optional provenance, not required storage.
More general modulator/coding channels require explicit coefficient estimands;
do not call an arbitrary regression coefficient an arithmetic condition mean.
The arithmetic condition mean is `beta=(M'M)^-1 M'a`, in the units set by the
family normalization and pulse convention.

Require full column rank of `Z=[F, XM]` for unique fixed coefficients. Positive
`lambda` makes ordinary ridge SPD, but cannot identify an unobserved condition
mean or remove collinearity between `F` and `XM`. Equivalently, the projected
trial system is SPD only when `A M` has full condition rank. Use rank-revealing
Gale operations and typed alias/identification failures; do not silently add
ridge to means. Rank must remain constant throughout a differentiable cell.

Handle absent declared levels explicitly rather than inverting zero condition
counts. A single-trial condition has no deviation to shrink. Coincident trials
can have a unique regularized solution without individually data-identified
amplitudes. Distinguish those facts in the result.

`lambda -> infinity` recovers condition amplitudes under these rank conditions.
`lambda=0` requires a separate unpenalized rank policy; the finite-state formulas
with `1/lambda` do not support it. Very small/large positive lambda also requires
conditioning checks. Run-specific versus across-run condition means must be
declared in membership/coefficient scope. The shape can remain shared either way.

Every fit reports an **effective response under its declared neural drive**.
Data-only rank/curvature can establish identification within that model; vascular
interpretation needs separate assumptions/evidence. Neural duration/integration
and vascular dispersion can be confounded. A kernel-family constraint does not
resolve that confounding. Fit provenance must include drive and spatial
preprocessing/measurement scale, independently of approximation and identification.

### 2. Ridge-and-release is exact

Set `S=I+XX'/lambda`, `R=S^-1`, and `Z=[F,XM]`. Then

\[
c=(Z^TRZ)^{-1}Z^TRy,\quad q=R(y-Zc),\quad
a=M\beta+\lambda^{-1}X^Tq.
\]

Equivalently use `S=I+alpha XX'` and `a=M beta+alpha X'q`. At `alpha=0`,
`R=I`, `a=M beta`, and `E=y'q` is ordinary condition-model residual energy.
Compile this branch using the aggregated design `B(theta)=X(theta)M`; never
construct a large trial solver and take a numerical infinite-lambda limit.

The small normal equations imply `F'q=0` and `M'X'q=0`, hence `M'u=0`.
Also `y-F gamma-Xa=q` and `X'q=lambda P a`, exactly the original optimality
conditions. Inverses here mean stable factor applications.

The minimized penalized energy is

\[
E=y^TRy-(Z^TRy)^T(Z^TRZ)^{-1}(Z^TRy).
\]

It also equals `y'q = q'q + lambda ||u||^2` in the trial model. Keep penalized
energy, actual residual energy and noise-scale preparation as separate values.

With `R=W'W`, accumulate `v=Wy`, `B=WZ`, `t=B'v`, `H=B'B` to obtain
`E=v'v-t'H^-1t`. This removes trial coefficients from shape scoring when a
cheap temporal application exists. The general backend still pays for
trial-space operator applications. Condition labels affect the small release,
not `S`, when the event geometry, normalization and lambda stay fixed.

Prefer this contract to a double low-rank downdate of
`X'X+lambda I`. The original downdate identity is correct, but its small Schur
matrix approaches singularity when means are aliased and can suffer cancellation.
Ridge-and-release makes that unpenalized rank boundary easier to expose.

### 3. Correct the random-effects likelihood

For constrained Gaussian trial deviations, the full observation covariance is

\[
K=I+\lambda^{-1}XPX^T.
\]

Here `u ~ N(0, sigma^2 P/lambda)` is supported on the contrast subspace.
Within a condition of size n its marginal trial-deviation variance is
`sigma^2(1-1/n)/lambda`, with negative covariance between distinct deviations.
This exact zero-sum model differs from ordinary independent random trial
effects whose covariance uses `S` and whose condition means are population
means. They share the fitting solution after releasing means, but not the ML
normalization. Choose the scientific random-effects assumption explicitly;
`K` is correct for the proposed constrained model, not universally for every
model called trial random effects.

After profiling **both** condition means and nuisance coefficients, with fixed
sigma and lambda, the full-data ML score is

\[
\ell_{\rm ML}=-E/(2\sigma^2)-\tfrac12\log\det K+\text{constant}.
\]

Its profiled energy equals the ridge-and-release energy: the difference between
`S` and `K` lies in the span of `XM`, already included in the free mean design.
The determinant does **not** disappear under that profiling.

Part 2 has the correct determinant identity:

\[
\log\det K=\log\det S+
\log\det(I_C-U^TRU),\qquad
U=\lambda^{-1/2}XM(M^TM)^{-1/2}.
\]

Part 1 instead uses `A=(I-Q_F Q_F')X` inside the covariance determinant.
That is the covariance volume of nuisance **error contrasts**, not generally
the full-data covariance volume above. If `Q` is an orthonormal basis of
`F`'s complement, its determinant is `det(Q'KQ)`, and

\[
\log\det(Q^TKQ)=\log\det K+
\log\det(F^TK^{-1}F)-\log\det(F^TF).
\]

The extra term depends on shape. Thus that expression can define an explicit
likelihood for nuisance error contrasts, but is not interchangeable with
profiling nuisance effects in full-data ML. The determinant identity using
`B_r=A'A+lambda I` is algebraically valid for its **projected** covariance;
the statistical labeling is the issue.

Provide explicit `PenalizedProfile` and `TrialRandomEffectsML` criteria.
Require callers to choose initially. The latter is preferable only when its
exchangeable within-condition Gaussian contrast assumption is intended.
Do not label either REML. A restricted/integrated fixed-effect criterion would
also involve `log det(Z'K^-1 Z)` and a declared contrast/measure convention;
here `XM` itself varies with shape, so comparisons need particular care.

Freeze sigma from an explicit preparation step for the first implementation.
If sigma is profiled instead, with lambda fixed, the ML energy term becomes
`-T/2 log(E/T)` (using the admitted observation count). The stated derivative
and Hessian scaling by fixed `1/sigma^2` would then be wrong. Estimated weights,
AR coefficients, lambda and regional priors introduce further plug-in uncertainty.
Response-derived heuristic weights do not automatically define a generative
Gaussian likelihood.

The determinant correction is data-independent only conditional on shared
geometry and frozen lambda. It still contributes once **per voxel** to pooled
evidence; it must not be added only once per region.

### 4. Profile derivatives and amplitude correction

The common internal reduction is `E=s-b'G^-1 b`. Put `w=G^-1 b` and
`r_p=b_p-G_p w`. Its derivatives are

\[
E_p=s_p-2b_p^Tw+w^TG_pw,
\]
\[
E_{pq}=s_{pq}-2b_{pq}^Tw+w^TG_{pq}w-2r_p^TG^{-1}r_q.
\]

In the condition model, `s` is the residualized response energy. In the temporal
backend, `s=||W_theta y||^2` changes with shape. Omitting its derivatives is a
cross-backend correctness bug. A `ProfileJet` carries energy/value, gradient,
symmetric Hessian, reference/chart, rank and approximation receipt. Criterion
assembly applies sigma scaling and the chosen determinant jet once, then the
shared decoder adds the prior. An expected-information matrix is a distinct
curvature capability, never an observed-Hessian substitute.

Share this reduction and its laws, without requiring public `G`, `G_p` or dense
derivative arrays. Backend-owned response encodings and workspaces allow compact
vectors for conditions and response/innovation blocks for trials. Dispatch once
per block; no virtual calls or generic AD object graphs in numeric inner loops.

For a fixed projection and penalty, `Ga=b` gives
`a_p=G^-1(b_p-G_p a)`. Differentiating `f=b'G^-1 b/2` confirms both supplied
formulas:

\[
g_p=b_p^Ta-\tfrac12a^TG_pa,
\quad H_{pq}=b_{pq}^Ta-\tfrac12a^TG_{pq}a+
(b_p-G_pa)^TG^{-1}(b_q-G_qa).
\]

Include the mixed Gram products `X_p'X_q+X_q'X_p`. Differentiate the
normalization, convolution, observation transformation, and parameter
coordinate maps consistently. These formulas assume lambda, observation
geometry and the relevant rank are fixed as shape varies.

At second order, `a=a0+a1+a2+O(||delta||^3)` satisfies
`G0 a2=b2-G1 a1-G2 a0`. The proposed residual correction adds precisely this
quadratic term plus higher-order terms. Its third-order error requires bounded
third derivatives and a uniformly well-conditioned inverse. The analogous full
coefficient formula is valid if the full normal matrix is nonsingular.

Contract before solving when shape derivatives have already been obtained from
the filter: reference solve, one directional solve, one residual correction.
This does not remove the individual derivative solves needed by a general
backend's observed Hessian. Count score and readout work separately. At a
prepared reference the terminal readout is exact; at a continuous decoded
shape it is approximate until an admitted readout scheme passes its budget.

The residual bound is correct. If
`G(theta) >= (1-eta) G0`, then coefficient error energy equals
`r'G(theta)^-1 r <= r'G0^-1 r/(1-eta)`. The penalized objective gap is that
energy divided by `2 sigma^2`. Computing `r'G0^-1 r` itself costs another
inverse application unless a conservative precomputed bound is substituted.
The advertised `d+2` solves omit this optional certification cost.

A residual checked only against interpolated equations cannot certify the
original equations. An exact residual for an infinite-support family may
also break the fast-path budget. Specify admissible operator-error bounds and
the cost of verification. Sampling points in a cell is empirical validation,
not a uniform proof of `eta<1`; name the receipt accordingly.

Amplitude energy accuracy is not a shape-accuracy certificate. Near a flat
profile, very different shapes have nearly equal energies. Shape acceptance
needs a likelihood-remainder bound plus identification, or an explicitly
empirical comparison against a global reference over the declared domain.

### 5. Shape decoding, pooling and information

For maximization, the prior is **subtracted**:
`score - (theta-mu)' Lambda (theta-mu)/2`. The supplied interior solution has
the correct signs if `-H/sigma^2+Lambda` is positive definite. Add log-determinant
derivatives for ML before decoding.

An observed Hessian need not be negative semidefinite. Never invert it blindly
or silently clip eigenvalues and call the result observed curvature. For a
box and `d<=3`, enumerate all face interiors, stationary candidates and
boundaries, score feasible candidates, and handle singular faces/ties explicitly.
There are `3^d` face patterns, but nonlinear physiological constraints are not
boxes. Use an admitted coordinate chart or an explicit feasibility contract.
Compare cells using score **values**, not gradients/Hessians alone.

Report data-only curvature separately from prior-augmented curvature. A proper
prior can select a shape in an unidentifiable voxel; it cannot turn this into
data identification. Inverse observed curvature is at most a conditional local
Laplace summary, not an automatically calibrated confidence interval.
For a penalized deterministic estimator, amplitude sampling covariance also
differs from simply `sigma^2 G^-1`: at fixed shape with white measurement
noise it is `sigma^2 G^-1 A'A G^-1`. Posterior, sampling and plug-in uncertainty
are distinct products. Do not reuse ordinary OLS t/F degrees of freedom.

With fixed geometry/scales the response-dependent score is quadratic in `y`,
so global sign reversal leaves shape evidence unchanged. Regional accumulation
of the three gradient and six Hessian entries is valid at a common reference
and chart. Keep the scalar score and voxel/weight counts as well. Summing
gradients at different centers without transporting/recomputing them is invalid.

A summed voxel likelihood assumes independence; spatial dependence can make
regional curvature overconfident. Arbitrary weights yield a weighted/composite
criterion. Use a deterministic bounded pilot and a frozen prior with nonzero
spread, but calibrate shrinkage under spatial correlation and avoid interpreting
that spread as established population heterogeneity. Protect held-out runs and
folds when used downstream for prediction; shape and hyperparameter estimation
must respect the training boundary.

Expected information is an optional optimization. With fixed sigma and a
specified Gaussian covariance, its mean term can be projected against `Z` and
its covariance term formed from `K^-1 K_p`. Estimating sigma or lambda changes
the efficient-information projection; REML has another convention. It is not
the observed Hessian and must not silently replace it, especially when condition
means vanish but trial covariance carries shape information.

### 6. Coupled normalization and penalty units

Normalization declares an amplitude quantity and therefore its prior scale.
For `h*(theta)=s(theta) h(theta)`, with positive bounded smooth `s`, equivalent
predictions use `a*=a/s` and the same trial penalty requires
`lambda*=s^2 lambda` (equivalently `alpha*=alpha/s^2`). Test invariance of
predictions, released amplitudes in common units, energy, full covariance and
its determinant. A shape-dependent conversion also contributes first/second
penalty derivatives. Freeze lambda within one declared convention in v1;
do not silently compare equal numeric lambdas across conventions as equal priors.

For a basis-coordinate change `beta*=T beta`, coefficient-penalty transport is
`Q*=T^-T Q T^-1`; covariance uses `T Cov T'`. These are different operations.
PHRF-17 fixes the current diagonal penalty transport with scalar, non-diagonal
quadratic-form and round-trip regressions on both platforms. Zero/nonfinite
diagonal scales require explicit failure when an inverse is needed.

Condition-only free amplitudes need no trial penalty, and their profile subspace
is invariant to a nonzero common kernel rescaling. Amplitude units and derivatives
still need consistent transport. Optional stimulus coefficients additionally
require `Gamma*=s^2 Gamma` when `w*=w/s` and feature coordinates stay fixed.

## Scientific family admission

| Family | Scientific parameters and normalization | Initial backend and restrictions |
| --- | --- | --- |
| Gaussian | Existing `mean, sd`; fit `tau, log(sd)` with stated bounds. Public ScalaFIM kernel is causal. Fix a normalization convention and reference domain. | General backend. No exact small fixed-order recurrence for the untruncated sampled Gaussian in general. Tail truncation/state approximation is explicit. |
| LWU | `exp(-(t-tau)^2/(2 sigma^2)) - rho exp(-(t-tau-2 sigma)^2/(2(1.6 sigma)^2))`; fit `tau, log(sigma), rho` with positive-lobe orientation and bounded rho. | General backend first. One shared rho per voxel. Two independently fitted Gaussian amplitudes per trial would change the scientific model. |
| Half-cosine | Four positive segment durations `h1..h4`, levels `f1,f2`, peak at `h1+h2`. Full existing family has six parameters. | Compact-support banded backend. A 2–3 parameter subfamily must explicitly declare which parameters are tied/fixed; do not silently narrow the family. Moving joins need smoothness admission. |
| New three-plus-four-stage cascade | A newly named family with fixed stage counts, stable positive rates, negative undershoot weight, orientation and normalization. Report actual peak/FWHM/undershoot summaries. | Exact finite-state backend only for its specified recurrence/discretization and admitted drives. Prototype two rates plus undershoot weight before considering additional freedoms. No default scientific endorsement yet. |

The [LWU reference](https://bbuchsbaum.github.io/fmrihrf/reference/hrf_lwu.html)
specifies the two-Gaussian construction; the
[half-cosine reference](https://bbuchsbaum.github.io/fmrihrf/reference/hrf_half_cosine.html)
specifies the four transitions. Source equations, rather than a convenient
substitute kernel, define parity. For LWU, `tau` is the positive component's
center, not generally the maximum of the combined response; `sigma` is not
its measured FWHM, and rho is not the attained trough/peak ratio.

Normalization must be the same **rule** over the entire family; its numerical
scale may depend on shape and then must be differentiated. Grid-maximum peak
normalization is nonsmooth when the maximizing sample changes. Signed area
normalization can be unstable when lobes cancel. Prefer a declared smooth
normalization with bounded, nonzero scale for the first derivative compiler;
an analytic positive-lobe area is a possible new convention, not a silent
reinterpretation of existing `UnitIntegral` or `UnitPeak`. Record the rule,
domain and amplitude units in every compiled receipt.

The raw LWU scalar/series compatibility paths and `Hrfs.lwu` currently have
different normalization behavior. Admit the selected public constructor
semantics, or introduce an explicitly named smooth family convention. Do not
mix raw derivatives with normalized evaluation.

Half-cosine segments are generally C1 but not C2 at joins. Moving a duration
can move a join across a sample, making that sample's second shape derivative
undefined. Fixed branch cells, one-sided boundary handling, integrated-drive
regularity proofs, or an explicitly different smooth family are possible;
asserting global second-order smoothness is not. The number of safe cells
required by many sample/join crossings is itself a resource risk. A typed
unsupported-cell result is preferable to hidden smoothing.

## Compact condition backend

This is a required first deliverable, not an optional trial optimization.
Let `B(theta)` contain the convolved condition drives and let `W0` be the shared,
frozen observation-noise transform (distinct from the shape-dependent innovation
factor above). With `W0 F=Q_F R_F`, define

\[
A(\theta)=(I-Q_FQ_F^T)W_0B(\theta)\simeq U D(\theta),
\quad U^TU=I_K,
\]
\[
z=U^T(I-Q_FQ_F^T)W_0y,\qquad
e=\|(I-Q_FQ_F^T)W_0y\|^2.
\]

The profile energy is `E=e-z'P_theta z`, where `P_theta` projects onto
`col(D(theta))`. Amplitudes solve `min_beta ||z-D(theta) beta||^2`.
Use QR/rank-aware solves, not explicit projectors. The formulas are exact when
the observed family lies in `U`; otherwise they define a qualified approximation.
The full-model covariance at `alpha=0` is identity, so the two admitted trial
criteria reduce to the same condition profile (with fixed sigma).

### Compile once, project once, decode in compact coordinates

1. Lower condition drives directly, preserving amplitude identities, pulses,
   modulators if admitted, run scope and one shape group. Build each candidate
   using the scientific family; never expose candidate or `U` columns as
   `BasisElement`, `StructuralColumn` or scientific coefficients.
2. Apply shared whitening and nuisance projection using thin products. Compile
   a bounded-rank union of designs and relevant shape directions with Gale
   QR/SVD/rank capabilities. Stream candidate blocks rather than retaining one
   large `T x (C * compilerNodes)` dictionary. Compilation has its own peak
   memory, candidate-count and elapsed-work budgets.
3. Compile `D`, its local derivative/interpolation representation, and reference
   factors. Prefer analytic/AD coefficient jets during shared preparation.
   Compile-time stencils may produce jets, but runtime must not evaluate 19
   likelihood nodes per chosen reference. Their finite-difference error is a
   separate receipt. Low-rank signed quadratic factors are an optional lowering
   if their rank and score error are measured; dense `K x K` jets are not mandatory.
4. Project each response block through fixed filters once. Keep `z`, `e`, frozen
   noise scales and small nuisance projections only for the active block. All
   subsequent condition shape scoring and readout uses compact coordinates.
5. Evaluate at most two local jets, use the shared constrained decoder, and
   read condition amplitudes from the qualified continuous `D(thetaHat)`.
   A small `K x C` QR is allowed; no trial/time-sized factorization is allowed.
   At a prepared reference the conditional readout is exact for the admitted
   compact design. Continuous interpolation and original-family discrepancy
   must be separately bounded/tested.

Retain `Q_F' W0 y` if fixed/nuisance coefficients are requested. Recover them
using `R_F^-1(Q_F' W0 y-Q_F' W0 B(thetaHat) betaHat)`, with the second factor
compiled consistently. `z` and `e` alone do not retain these coefficients.
Residual energy can be obtained from compact sufficient statistics, with
cancellation-aware computation and a declared approximation error.

### Rank and coverage are admission gates

`K` is the dimension of the observable response family, not the number of shape
parameters. Neither `K=3` nor `K=32` is a scientific guarantee. The supplied LWU
demo retained 69 dimensions. Bound rank, bank size and compiler resources before
execution; return `CompilationBudgetExceeded` if fidelity cannot fit those caps.
Do not relax accuracy, truncate important directions or increase rank silently.

Singular-value energy of a training dictionary is insufficient. Validate unseen
family points, derivatives, condition rank and projector error under the actual
noise/nuisance geometry. Small design error can produce large projector error
near a rank loss. Require a minimum singular-value/conditioning margin in each
admitted cell; rank-changing cells are unsupported. For an admitted projector
error `epsilon`, score error is at most `epsilon * ||z||^2/(2 sigma^2)` within
the compact model; account separately for loss outside `U` when certifying the
original time-domain criterion. This is not a uniform global certificate merely
because sampled points pass.

The outside-space variance formula `(e-||z||^2)/(n-rank(W0 F)-K)` is available
only with positive adequate residual degrees of freedom, correct fixed whitening
and qualified family coverage. With discarded signal it is biased. It is not
the ordinary residual variance after selecting shape, and is not transferable
to a trial model whose deviations lie outside the condition response space.
Default to explicit frozen scale preparation; label any such estimator's scope.

### Bounded routing without a hidden dictionary scan

The default bank has at most eight local references per observation geometry,
and a voxel evaluates at most two. Select candidates by frozen regional/pilot
routing or a separately validated bounded router on compact summaries; charge
router computation and memory. The router may refuse. Scoring all reference
centers and then calling only two of them "evaluated" violates this contract.
The same applies to hidden stencil-node likelihood evaluations. Compilation
may inspect many shapes within its explicit preparation budget.

Routing is approximate and must achieve the declared acceptance/accuracy target
on the complete attempted cohort. Wide domains may fail this policy. A larger
bank/search policy would be a separately measured configuration and recorded
target revision, not a silent relaxation of the baseline. The 60-center supplied
demo is an algebra/decoder reference, not evidence that the bounded router exists.

### Pool shared evidence without a mandatory whole-brain pass

One shared `ShapeEvidencePool` accepts score/gradient/Hessian summaries at common
references and charts for all backends. The compact identity
`sum_v w_v ell_v(theta)=tr(P_theta S_r)/2+constant`, with
`S_r=sum_v w_v z_v z_v'/sigma_v^2`, is an optional exact accumulator within that
compact model. It preserves sign-reversed evidence. Do not require `O(K^2 V)`
outer products just to get a regional prior: the bounded pilot can accumulate
the ten local jet components instead. Cap regions and all retained accumulators.

Default execution prepares a deterministic pilot of at most 2,048 voxels in at
most two preparation passes, freezes priors/routing, then projects and fits each
full response block once. Pilot IO is reported. An optional full-cohort pooling
mode may spool compact scores and make a second compact pass; declare storage,
precision and fold boundaries, and use a bounded store rather than a mandatory
`K x V` matrix in `Prepared`. Geometry and response-derived preparation are
separate immutable artifacts. Chunk order must not change the frozen fit.

## Concrete support for the seven-state cascade

Proposed public name: `Cascade34`. Define the scientific family in continuous
time so changing TR changes sampling, not the HRF itself. The previous
discrete-cascade probe remains a separate numerical test family.

For `t>=0`, define unit-area Erlang components

\[
g_n(t;\kappa)=\frac{\kappa^n t^{n-1}e^{-\kappa t}}{(n-1)!},\qquad
h(t)=g_3(t;\kappa_p)-\rho g_4(t;\kappa_u).
\]

Set `h(t)=0` for negative lag. Use three validated parameters:
`kappa_p > kappa_u > 0` and `rho >= 0`, with a finite application-specific
shape domain. The rates have units of inverse seconds. The slower undershoot
branch has a later component peak: `2/kappa_p` versus `3/kappa_u`.
Rho is the ratio of component **areas**, not their attained heights.

The initial normalization is intrinsic: each component has unit integral,
with coefficient +1 on the positive component. The full signed integral is
`1-rho`; do not divide by this quantity. This smooth rule remains usable at
rho=1. Record it as the family's positive-component-area convention; it is
not generic signed `UnitIntegral`. Other normalizations can follow with their
own consistent derivatives. Require positive-response orientation and expose
actual peak time, peak height, width and undershoot as derived summaries.

One possible decoder chart is
`(log(kappa_p), logit(kappa_u/kappa_p), rho)`. A bounded box in these coordinates
enforces the rate ordering; further scientific constraints need explicit
admission. At rho=0 the tail rate is unidentifiable and must be reported as
such. Rates determine both latency and width: this three-parameter family
does not supply arbitrary independent control of those quantities. In
particular, the positive component has standard deviation `sqrt(3)/kappa_p`.
Additional stage-specific rates would be a separately validated extension.

The exact state equations are

\[
\dot p_1=\kappa_p(u-p_1),\quad
\dot p_2=\kappa_p(p_1-p_2),\quad
\dot p_3=\kappa_p(p_2-p_3),
\]
\[
\dot z_1=\kappa_u(u-z_1),\quad
\dot z_j=\kappa_u(z_{j-1}-z_j)\ (j=2,3,4),\quad
y=p_3-\rho z_4.
\]

These are seven dynamical states and three shape parameters. A trial impulse
of amplitude a jumps the state by
`a * b`, where `b=(kappa_p,0,0,kappa_u,0,0,0)'`. The observation vector is
`c=(0,0,1,0,0,0,-rho)'`. Thus both branches use the same trial amplitude.

For the lower-shift matrix `J_n`, each block has generator
`L_n=kappa(J_n-I)`. Its exact interval transition is a finite expression:

\[
A_n(\Delta)=e^{-\kappa\Delta}
\sum_{j=0}^{n-1}\frac{(\kappa\Delta)^j}{j!}J_n^j.
\]

Assemble `A=blockdiag(A_3,A_4)`. No time-stepping approximation or generic
matrix exponential is needed for this equal-rate-within-branch family.
First and second shape derivatives can be evaluated through these scalar
expressions and the parameter chart. Propagate the normalization/injection
derivatives too. Keep analytic derivative and dense-exponential test oracles
independent.

This also supports exact off-grid **impulses** without binning. Between two
observations propagate with `A(Delta)` and add process covariance

\[
Q_t=\lambda^{-1}\sum_{i:\,t_{prev}<e_i\le t}
w_i^2 v_i v_i^T,\qquad v_i=A(t-e_i)b.
\]

Define event-at-run-start handling and initial conditions explicitly. This
formula includes coincident independent trials and the cross-branch terms.
All event-dependent covariance work is shared preparation. Readout still
visits each trial to apply its adjoint injection. Observation-time averaging,
if requested, needs a corresponding observation map; point samples are the
first contract.

Implement in this order:

1. `hrf/Cascade34.scala`: validated parameters, scalar kernel, primitive for
   ordinary pulse convolution, support/normalization metadata and derived
   shape summaries. Add `HrfKind`/`HrfParams` cases and exhaustive dispatches.
2. `hrf` family/realization contract: exact interval transition and injection/
   observation derivatives. Keep scientific recipes portable and adapt to
   Gale at the numerical boundary.
3. `fit/profile/FiniteStateTrialLikelihood.scala`: prepare innovations from
   the event schedule, lambda and geometry; release means/nuisance; compile
   local score derivatives and exact prepared-shape readout.
4. Add bounded continuous shape decoding/readout through the same admission
   contract as the general engine, then integrate with the block sink.

`Pulse.BoxHeight` and `Pulse.BoxMass` can use the family's analytic primitive
in ordinary convolution immediately. Their random-trial finite-state
elimination remains separately gated: repeated forcing from one random
amplitude cannot be replaced by independent process noise. This separates
scientific family support from fast-backend eligibility.

The accompanying probe now verifies the proposed continuous kernel against
the seven-state realization, the finite transition against SciPy's generic
matrix exponential, the semigroup identity, the signed area, and an off-grid
event covariance inverse against a dense design. At rates 0.4 and 0.2 per
second and rho=0.35, transition maximum absolute error was 3.61e-15 and
off-grid inverse relative error was 3.37e-16. These parameters are test values,
not recommended physiological defaults. Continuous-family shape derivatives,
conditional readout and scientific fit quality still need their own tests.

## Backend and temporal geometry contracts

### General backends

`TrialBanded` factors `X'X+lambda I`; `TimeBanded` factors
`I+XX'/lambda`. Build bands directly from support/event intersections, not by
forming a dense matrix and detecting zeros. Trial bandwidth depends on maximum
overlap, event density and drive duration, not just the nominal HRF span.
Time covariance bandwidth is bounded by each trial column's full temporal
support; variable-duration boxes enlarge it. Both preserve the small release.

For Gaussian/LWU, exact full-tail designs are generally not banded. Use a
declared tail-error budget covering values **and derivatives**, conditioning,
and induced score/readout error. A per-sample kernel tolerance alone does not
control errors accumulated across many trials. Finite-state truncation of an
otherwise infinite recurrence is also a different operator unless represented
explicitly.

Shared FIR/short-memory observation transformations can preserve locality.
Generic ARMA inverse filters need not have finite support. Dense whitening
destroys this banded guarantee, although separately admitted state-space noise
augmentation may still work. Masks do not necessarily destroy locality; they
destroy reuse if every voxel has different geometry. Group by validated
`ObservationPattern`, cap the number of preparations, and account for the
extra memory/time. First release: one shared mask and frozen temporal transform.
Do not change run/censor-gap semantics to gain reuse.

### Finite-state backend

For impulses on the admitted temporal grid, an r-state recurrence with input
vector `b` generates `XX'/lambda` when process covariance is

\[
Q_t=\lambda^{-1}\sum_{i:\,onset_i=t} w_i^2 bb^T.
\]

Both positive and negative cascades receive the **same** trial input: `b b'`
contains cross-branch covariances. Independent branch noise would implement
two trial coefficients and violate shared undershoot. Innovations compilation
must explicitly include `Q_t`, observation noise, state initialization,
event-before-observation timing, and resets between runs. A stationary initial
state would invent unobserved pre-run random trials unless those are modeled.

Compile gains and innovation variances, and all their first/second parameter
derivatives, without response data. Differentiate the covariance recursion,
including `b`, rates, normalization, transition and observation maps; derivatives
of `A` and `c` alone are insufficient. Use stable covariance updates/factors
and validate positive innovation variances. At runtime propagate response jets
and accumulate energy and condition/nuisance statistics. This is exact local
observed curvature for the admitted state model, up to numerical error.

The state-space/innovation connection is established numerical machinery;
exactness depends on the representation. [Hartikainen and Särkkä](https://users.aalto.fi/~ssarkka/pub/gp-ts-kfrts.pdf)
distinguish exact rational state representations from approximations in temporal
Gaussian-process inference. Their stationary GP construction is background,
not a proof for our event-driven nonstationary covariance; our dense event
covariance is the test oracle.

Off-grid impulses require exact interval transitions and correctly timed input
maps, or an explicit sampling approximation. Splitting one trial into independent
neighboring-bin impulses is wrong. Continuous-time cascades sampled via matrix
exponentials can have dense triangular transitions even if the differential
equations are sparse: charge `O(r^2)` unless an exact `O(r)` application is
demonstrated. A discrete bidiagonal cascade is its own family, not automatically
the sampled continuous cascade.

Variable-duration pulses, repeated phases, and multiple impulses sharing one
trial coefficient create correlated forcing across time. Independent onset and
offset shocks are wrong. An exact augmentation can require active-trial memory,
duration delays or additional states. Admit the **complete drive realization**,
not just the HRF. Start finite-state support with one impulse per trial;
use the general backend for `Pulse.BoxHeight`, `Pulse.BoxMass`, and shared
multiphase drives until their state size and semantics are proved.

Fused backward whitening/HRF adjoints are valid at prepared shapes, with
careful direct-feedthrough indexing. Keep the forward innovation buffer for
the current response block. Verify the adjoint identity independently, then
compare fused and separate passes before admitting the optimization.

### Continuous readout and cache rules

Choose directional second-order readout before coefficient interpolation for
the first continuous state backend; it uses prepared operators with explicit
local order. Interpolation must later certify complete forward/adjoint behavior,
normal equations, positivity and derivatives, not merely pointwise gains.
No unreported per-voxel covariance preparation or unbounded fallback loop.

Cache key: scientific family/version/chart/normalization, reference/cell,
complete drive and row geometry, run initialization, observation transform,
amplitude structure and its alpha/penalty convention, numeric precision, and
derivative/approximation policy. Split the
label-independent ridge/filter cache from the condition/nuisance release cache.
The compact condition cache includes condition coding and fixed-regressor
projection because these determine its observed family. Query caches additionally
include the exact query axis/weights, units and precision. Criterion/noise-scale
and response-derived prior/routing identities belong to their preparation layer;
they must not be silently reused across incompatible folds or preparations.

The continuous integral identity for common pure delay is correct. It does
**not** imply equality of discrete sampled Gram matrices for arbitrary
fractional delays. In general `sum_k h(k Delta-delta)^2` depends on delta even
without window loss. Admit delay reuse only after proving invariance for the
actual geometry, e.g. integer-grid row shifts with complete support or a
specified unitary circular/bandlimited translation operator. Boundaries,
censoring, weights, phase-dependent sampling, causal truncation and nuisance
release still matter. Matching autocorrelation is sufficient only for the
trial ridge factor; response correlations and release matrices remain specific.

## Readouts, functional queries and downstream boundaries

One output request distinguishes condition amplitudes, trial amplitudes and
signed linear queries, with structural axes and units. Condition mode returns
condition coefficients directly. It may expand `M beta` only on explicit request,
labeling that output as repeated condition means, not identified trial effects.

For a declared trial query `Q` with `J` rows, compile the conditional readout

\[
f=Q\hat a=QM\hat\beta+\alpha(X_\theta Q^T)^Tq.
\]

At `alpha=0` this reduces exactly to `QM beta`; condition-native queries act on
the condition axis directly. Query compilation stores bounded `T x J` factors
or structured equivalents, never requires a full trial-amplitude output, and
retains signs. At continuous decoded shapes, certify the chosen readout's local
approximation; an amplitude-vector relative error does not automatically bound
a near-zero contrast. Use absolute/scaled per-query tolerances and account for
query operator norm, original-system error and output precision. Avoid claiming
that output compression eliminates every `O(N)` workspace/certification visit.

Optional after core qualification: a frozen second-order readout field can
represent `Bhat=sum_k L_k Y diag(w_k)` and its forward/transpose actions without
materializing trial maps. At `d=3` there are ten Taylor terms per reference, with
reference-routing masks included. This is exact for the chosen Taylor polynomial;
it is not automatically identical to a residual-corrected readout that retains
higher-order terms. Preserve backend structure in `L_k`; do not store ten dense
`N x T` matrices or ten `N x V` fields. Count response rereads, reference groups,
derivative solves and operator applications, and compare against cached blocks
for repeated queries. Its transpose is conditional, not the adaptive Jacobian.
`mvpa-fit` needs a field-aware adapter; existing `RunTrialReadout` assumes a
shared temporal map. Held-out target labels must never choose shrinkage coding,
HRF preparation or feature transforms for supervised decoding.

Optional stimulus-aware amplitudes use `a=M beta+Phi w+u`, `M'Phi=M'u=0`, and
penalties `lambda ||u||^2+w'Gamma w`. The same ridge covariance releases
`[F,XM,XPhi]` with `blockdiag(0,0,Gamma)` in its small system. Require bounded
feature rank/count, feature scaling/centering fitted on training data, and a
separately admitted criterion. Generally `lambda Phi'u=Gamma w`, so residual
trial effects are not feature-orthogonal. The decomposition depends on both
penalties. Normalization conversions transform Gamma as well as lambda.
Continuous stimulus streams and correlated repeated random forcing are separate
drive contracts, not an implicit extension of independent trial impulses.

The core result carries signed responses plus query meaning, stimulus coverage,
uncertainty availability and fit/preparation provenance. Shape-score jets are
sign-invariant and must not become functional fingerprints. At fixed preparation,
readout error cross-covariance is `L_i Omega_ij L_j'`; exact elimination transforms
correlated noise rather than removing it. Shared preprocessing can also mix
signals with different kernels. Preserve this distinction from regional-prior
influence and anatomical adjacency. No dense whole-brain spatial covariance or
spatial inverse problem is required in the engine.

Geometry admission belongs downstream. Require held-out response probes, checks
for false neighborhoods under spatial noise/mixing, HRF variation, neural timing,
prior partition/strength and uneven stimulus coverage, and evidence of similarity
within a declared tolerance. Broad overlapping uncertainty is not such evidence.
Crossvalidated distances require independent split errors and a fixed/admitted
metric; they do not undo shared shrinkage or misspecified neural/vascular
factorization, and negative estimates are not ready-made mesh edge lengths.
Keep tuning geometry distinct from interaction claims. A smooth fingerprint map
from a two-dimensional anatomical surface has differential rank at most two;
protrusions or high graph degree do not by themselves establish non-manifold
structure. These are downstream validation boundaries, not added voxel-loop work.

## Resource and approximation receipts

Use `D=1+d+d(d+1)/2`, `k=C+rank(F)`, block size `Vb`, and state dimension `r`.
Avoid using `r` for both derivative count and state size.

- Trial-band preparation includes approximately `O(N b^2)` factor work and
  `O(N b)` storage, plus matched filters, derivative Grams and small corrections.
  Applying a prepared trial solve is `O(Nb)` plus release work, not free.
- A time-band factor has analogous costs with `T` and temporal bandwidth.
- Dense state covariance compilation costs approximately `O(D T r^3)` with a
  small fixed d; reusable coefficients require time-dependent storage. Runtime
  filtering is approximately `O(D T r^2 V)` plus `O(D T k V)` statistics and
  small solves; a demonstrated structured transition may reduce the r factor.
- Finite-state scoring avoids trial-sized coefficient jets. Geometry preparation
  and output still cost at least `O(N)`. Computing/readout of off-grid events
  or nontrivial drives may add event-dependent runtime. Do not promise runtime
  independent of trial count for all admitted designs.
- Block memory includes response/innovation buffers `O(T Vb)`, output
  `O(N Vb)`, derivative states `O(D r Vb)` and small accumulators. Shared
  derivative condition designs can cost `O(D T k)` per reference. Bound the
  reference bank and geometry groups as well as the voxel workspace.
- Materialized float32 trial output alone is `4 N V` bytes: 300 trials by
  100,000 voxels is 120 MB decimal. Accumulate/factor in Double; downstream
  output conversion needs declared precision and finite-value checks.

Receipts should separate tail/drive approximation, derivative error, cell
Taylor error, conditional readout residual, score comparison error, and
identification. Useful statuses include `AcceptedLocal`, `WeaklyIdentified`,
`PriorDominated`, `BoundaryCandidate`, `AmbiguousCells`, `RankDeficientMeans`,
`UnsupportedGeometry`, `NonsmoothCell`, and `ApproximationBudgetExceeded`.
Identification and approximation are independent axes, not one success boolean.
Record empirical versus certified bounds. Failure preserves diagnostics and
does not manufacture a precise shape or silently substitute a family.

### Performance is an early gate

PHRF-20 establishes the benchmark harness, empty receipts and work counters after
specification. PHRF-18/19 measure condition compilation and the first complete
condition loop; PHRF-07/10/11 measure trial scoring and full readout as they land.
PHRF-21 qualifies the condition milestone. PHRF-15 is final cross-backend
qualification, not the first opportunity to discover an expensive design.

| Budget | Condition baseline C0 | Trial baseline B0 (preserved) |
| --- | --- | --- |
| Cohort | T=600, C=3, six full-rank fixed columns, V=100,000; condition aggregation of the same 300-event schedule; d<=3 | T=600, N=300, C=3, six full-rank fixed columns, V=100,000; d=3, r=7 for Cascade34 |
| Capacity admission | K<=96 and C<=8 for this baseline; K=32 is an illustrative measurement point, not forced compression | N=1,200 stress case as well; family and overlap determine trial bandwidth |
| Shared bank/runtime | <=8 references per geometry; <=2 evaluated per voxel, including routing/final scoring | Same; one amplitude correction and <=1 additional reference inverse application for certification |
| Working memory | <=256 MiB engine live memory, including compilation, bank, active worker buffers and output conversions | Same; original B0 target unchanged |
| Blocks/concurrency | <=256 voxels per worker, <=8 JVM workers | Same |
| CPU compute target | <=30 seconds on the same declared JVM host; proposed engineering target, not a measured result | <=120 seconds complete two-reference profile + readout + certification; original target unchanged |
| Condition overhead goal | Post-projection routing/decoding/readout <= the initial compact-projection time for C0; report actual K and achieved accuracy | No equivalent compact-space assumption imposed on trial mode |
| Output | Float64 computation, Float32 condition/query output with conversion checks | Float64 computation, Float32 trial output; 120 MB decimal if all coefficients are written |

Freeze the actual CPU/OS/JDK/heap/power/thread settings and exact provider/source
revisions in PHRF-01. The CPU-only JVM host has at least 32 GiB RAM. Exclude
input acquisition and disk persistence from the stated compute target, but
report them and cold geometry/pilot preparation separately and in complete
elapsed totals. A reused-cache result cannot stand for cold preparation.
The C0 30-second goal gives the simpler model its own demanding target; it does
not revise the existing B0 target. Freeze full family domains and signal/noise
regimes before measuring; no narrowed post-hoc cohort to meet either goal.

At fixed blocks/concurrency, V=10,000 -> 100,000 should increase engine live
memory by <=max(32 MiB,10% of baseline); V=10,000 -> 20,000 should cost <=2.3x.
Emit scalar receipts in bounded form too. Report process peak RSS separately.
Compile candidate buffers, pilot statistics and optional compact-spool caches
are part of the budget, not excluded "preparation" allocations.

Keep the original trial goals: complete one-reference profile/readout <=12x
the same backend's exact fixed-shape fit/readout, two-reference <=24x; finite-state
observed-Hessian profile/readout >=3x faster than TrialBanded on the frozen dense
overlap case, using the same Cascade34 family, criterion and accuracy budget.
Report the sparse crossover. State score-only runtime should change <=20% for
N=300 -> 1,200 at fixed T/C/F/d/r/reference count after preparation; charge event
preparation and terminal `O(N)` readout separately. The condition path must omit
trial dimensions after aggregation; splitting an event into equivalent known
condition-drive contributions cannot create trial covariance work.

Condition measurements include K=32/64/96 where accuracy admits, T=600/1,000,
C=3/8 and V scaling. Compare to exact fixed-shape condition regression as well
as the K-column projection; do not call the latter a canonical-HRF GLM cost.
Do not extrapolate the condition projection shortcut to positive trial variance:
trial deviations/covariance can carry information outside the condition union.

Accuracy co-gates for the preregistered locally identifiable cohorts: >=95%
admitted, p95 decoded-vs-sufficiently-searched same-model oracle peak latency
<=0.02 s, FWHM <=0.05 s and relative amplitude L2 error <=0.1%, with original
score/residual budgets. Assess condition and trial outputs separately. Near-zero
amplitudes use a predeclared absolute scale; undefined summaries, nulls and
ambiguities remain in declared separate strata with all attempts counted.
These are approximation-agreement goals, not real-fMRI physiological precision.

JVM and JS run correctness and relative scaling; JS uses one worker and at
least V=10,000 and reports its own throughput. Warm up to stability, then record
at least five measured runs, median/p95, allocations/GC, work counters and raw
timings. No build/startup in steady-state compute. Instrument zero per-voxel
large factorization/covariance preparation, no runtime stencil bank scan, and
no unbounded nonlinear fallback. An unmet target remains unmet; changes require
an evidence-backed decision with old/new values and the tradeoff.

## Implementation sequence and acceptance gates

Use one expanded epic with existing PHRF-01..16 IDs preserved and new work
PHRF-17..24. Dependency edges, rather than numeric ticket order, define execution.
See [the work-item index](profile-hrf-work-items.md) for exact IDs and dependencies.

| Stage | Work | Gate |
| --- | --- | --- |
| Scientific and cost contracts | PHRF-01, then penalty repair PHRF-17 and early benchmark harness PHRF-20 | Exact amplitude specializations, criteria/units, source-backed repair, frozen C0/B0 cohorts and empty measured-receipt schema. |
| Shared foundations | PHRF-02/03/05/08 | Family and complete drive semantics; exact alpha=0 lowering; common energy jets and independent dense oracles. No production optimizer. |
| Condition compiler and shared decoder | PHRF-18/09, then condition runtime PHRF-19 | Observable-family/rank admission, bounded router, two reference jets, compact continuous readout and original-model error checks; measure as built. |
| First condition milestone | PHRF-12/13/22, then PHRF-21 | Shared bounded sinks, frozen pilot pooling, signed queries, condition-specific numerical/scientific calibration, JVM+JS performance and integration evidence. No dependence on trial/state completion. |
| Trial backends | PHRF-06/07 and PHRF-04/10; then PHRF-11 | Banded provider qualification and exact seven-state Cascade34; common jet laws, full ML determinant and corrected readout/query parity. These can develop alongside the condition path after shared contracts. |
| Full qualification | PHRF-14/15/16 | Cross-backend and scientific courts, all attempted-cohort performance, complete integrated JVM+JS gates, examples and real-consumer/export inspection. |
| Optional follow-ons | PHRF-23 frozen readout field; PHRF-24 stimulus features | Separate measured/scientific admission after core release; no prerequisite edge into v1. |

Condition milestone PHRF-21 is usable only for its qualified families, domains
and outputs. It is not completion of the trial epic. PHRF-16 closes after all
required work PHRF-01..22 has evidence. Optional PHRF-23/24 remain visible without
blocking the core epic. Continuous-drive augmentation, TimeBanded, partial-linear
single-condition LWU, expected-information curvature, coefficient interpolation
alternatives and delay reuse remain explicitly deferred unless separately admitted.
A single-condition LWU cone optimization must enforce its amplitude-sign and rho
constraints and cannot supply independent rho values per condition in shared mode.

Run affected suites on both platforms and relevant laws/scenarios. Condition
milestone covers hrf/design/model/fit and its actual provider dependencies; trial
release adds the banded/state qualifications. Full integration uses
`sbt scalafimCompileAll` and bounded invocations of `<module>JVM/test` and
`<module>JS/test` covering the `scalafimTestAll` set. Never run the full JS aggregate
in one long-lived sbt process on the constrained VM. JavaFX exclusions follow
AGENTS.md. New scenarios use one `ScenarioResult` with explicit caveat policy.
Pending/unrelated gate failures remain visible and are not passing evidence.

### Scientific validation matrix

Use independent direct designs and dense factorizations for algebra; analytic
derivatives or automatic differentiation checked against finite-difference step
sweeps for jets; dense nonlinear searches only in test oracles. Do not compare
two implementations that share the same incorrect normalization/drive builder.

Exercise each admitted family across latency, width, undershoot and causal
boundaries; long and short TR; sub-TR onsets; isolated, dense and coincident
trials; variable pulses; unequal/singleton/absent conditions; multiple runs;
short runs and retained tails; nuisance confounding; shared masks/weights/AR;
null and signed signals; zero condition means with nonzero trial variability;
small/large lambda; ill-conditioned means; and parameter-dependent joins.

Separate four comparisons:

1. **Conditional numerical agreement:** original equations, backend solves,
   derivatives, determinants and readout at exactly the same shape.
2. **Decoder agreement:** difference from a sufficiently searched nonlinear
   reference for the same model, including worst cases, ambiguous cells and
   acceptance/failure rates. A local optimizer alone is not a global oracle.
3. **Scientific recovery:** bias/RMSE against known generating shapes and
   amplitudes, model mismatch, false identification under null signals,
   conditional versus end-to-end uncertainty coverage, prior and lambda
   sensitivity, and cross-validated prediction. Include spatially correlated
   voxels and nonexchangeable trial effects.
4. **Resources:** preparation, filters, release, readout, certification and IO
   timed separately; trial/time/state backend crossovers; peak memory and
   accepted fraction, on JVM and JS. No accuracy gain obtained by unreported
   fallback work. A two-cell cap is an execution option to validate, not an
   established coverage result.

A local quadratic cannot guarantee every response in an unbounded noise model
has a small shape error. State the admitted probabilistic/data-dependent domain
and allow refusal. Real fMRI validation should include test-retest/held-out
prediction and parameter plausibility, without claiming ground-truth shape
accuracy from real data that lack a known HRF.

## Research evidence and its limits

Run:

```sh
OPENBLAS_NUM_THREADS=1 python3 tools/validation/profile_hrf_review.py
```

The deterministic probe uses seed `20260909`, Python 3.14.7, NumPy 2.4.3 and
SciPy 1.17.1. It checks a causal Gaussian design (120 observations, 24 trials,
three conditions, two nuisance columns) and a distinct seven-state **discrete**
cascade (72 observations, 18 trials, three conditions, two nuisance columns).
The state test uses second-order forward Taylor algebra through the covariance
and innovation recursions; its independent score oracle builds all trial columns
and uses dense covariance solves, with Richardson finite differences.

| Probe | Observed result |
| --- | ---: |
| Banded-plus-low-rank Gram identity, relative error | 1.17e-16 |
| Ridge-and-release amplitudes versus original equations, relative error | 1.96e-15 |
| Explicitly truncated banded ridge versus dense ridge, relative error | 1.74e-16 |
| Gaussian profile gradient / Hessian, maximum absolute discrepancies | 3.57e-9 / 6.81e-9 |
| Full condition-centered determinant identity, absolute error | 5.55e-15 |
| Projected minus full log determinant, two shapes | -0.0580283 / -0.0548826 |
| First-order / corrected amplitude relative error at first displacement | 0.1965% / 0.01420% |
| Corrected error after halving / quartering displacement | 0.001783% / 0.0002234% |
| Seven-state inverse / trial readout, relative error | 3.91e-16 / 3.39e-15 |
| Innovation log determinant, absolute error | 1.78e-15 |
| Filter profile gradient / Hessian, maximum absolute discrepancies | 7.72e-11 / 3.66e-7 |
| Largest state-profile solve | 5 by 5 |
| Boundary-free narrow Gaussian sampled-energy change after half-sample delay | 74.0% |
| Half-cosine one-sided second derivatives at moving peak join | -0.12085 / -0.19737 |

The delay example deliberately uses a narrow Gaussian to disprove a universal
identity; it is not an estimate of typical fMRI error. The amplitude bound is
checked pointwise, not certified uniformly over a cell. The approximately
eightfold error reduction per halved displacement supports local third-order
readout error in this probe only.

These checks support the elimination/derivative architecture and expose the
likelihood, sampling and smoothness restrictions. They do not validate a
production LWU/half-cosine derivative compiler, continuous
state readout, all ML determinant derivatives, fused adjoints, realistic noise
recovery, broad-domain coverage, uncertainty calibration or production throughput.
No Scala production sources were changed and no JVM/JS feature-completion claim
is made by this review.

### Supplied artifacts reviewed and rerun on 2026-09-10

All supplied Python sources were read before execution; scripts that write beside
`__file__` were imported or run from temporary extracted copies. Original Downloads
files were preserved. The exact input hashes are recorded below. The two state
scripts inside the alignment archive match those in the innovations archive.

| Artifact/check | Rerun finding | Boundary |
| --- | --- | --- |
| `compiled_hrf_demo.py` and its supplied JSON | K=69; tau/sigma/rho RMSE 0.0348473/0.0347386/0.0103831; amplitude relative RMSE 0.00809494 | Noise-free, 60 runtime centers plus two 19-node stencils, exact-family final amplitude oracle; not the bounded production compiler. |
| `trial_hrf_schur_demo.py` and its supplied JSON | Corrected amplitude relative error 0.000435860; original-system residual bound passes | One local Gaussian anchor; penalized criterion, clipped interior demo, local optimizer oracle. Its determinant check uses nuisance-projected covariance, not full-data ML. |
| Innovations archive: identity | Released trial amplitude relative discrepancy 1.52e-14 | Discrete cascade with sampled-peak normalization; not continuous Cascade34 or other named families. |
| Innovations archive: derivative jets | Hessian relative discrepancy 1.16e-8; maximum absolute 1.14e-6 | Local compile-time finite differences; stores derivative time series for demonstration. |
| Innovations archive: expected-information stress | Zero-mean tau decoder RMSE 0.0224191 versus 0.0100647 for observed Hessian; wider-cell corrected amplitude relative error 0.00174861 | Fewer solves can sacrifice accuracy; expected information stays optional. |
| Alignment archive | Exact alpha=0 condition law, shared energy jets, determinant correction, impulse aggregation and polynomial forward/transpose assertions pass | Local dense research checks; Taylor field is not the full adaptive estimator. |

[`profile_hrf_unification.py`](../../tools/validation/profile_hrf_unification.py)
retains the supplied alignment audit with attribution and adds independent
augmented-least-squares checks for feature/query algebra, normalization transport,
compact condition energy and fixed-coefficient recovery. Its self-hashed output is
[`profile_hrf_unification_checks.json`](profile_hrf_unification_checks.json).
Run it with `OPENBLAS_NUM_THREADS=1 python3 tools/validation/profile_hrf_unification.py`.
The separate [context probe](profile_hrf_context_checks.py) also reruns successfully;
its spatial-distance ratio is an algebraic example, not a Monte Carlo recovery test.
These checks do not implement a Scala backend or demonstrate JVM/JS throughput.

| Supplied file | SHA256 |
| --- | --- |
| `compiled_hrf_demo.py` | `f3abce6c677629c887924acab6e48c1bb73327ad55f54b5894a4e426c53d1889` |
| `compiled_hrf_demo_results.json` | `f5ab9e2213efbd821752752974daa68ce5ca35d067df133a9b0389e7d64b690b` |
| `trial_hrf_schur_demo.py` | `9f913713b057958a41f19f027318a13b70227aa3bdcaaad019bd707bb067e935` |
| `trial_hrf_schur_demo_results.json` | `d197ceddfb501597983e61770f531e3b3f124d334193537335b6cfe622b0fe6e` |
| `hrf_algebra_innovations_checks.zip` | `a8bc322a75bcca50628118491a0df6650c03bc7b8f780ecc12089b5e16a86256` |
| `hrf_alignment_review.zip` | `759292c42239baeb7351fb063660e3035ff4d11317102a8e561f0b119118295a` |
