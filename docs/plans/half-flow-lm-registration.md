# HalfFlow-LM: compact symmetric nonlinear registration

> Historical design record. The engine and its verification assets moved to
> `canardlapin/reframe4s` at commit `e7f469c`. This document preserves the
> original ScalaFIM plan and is no longer a module roadmap for this repository.

- Status: proposed implementation plan
- Date: 2026-07-21
- Tracker epic: `bd-01KY3J2R58MPZNKX7ZKTVYPQTM`

## Decision

Build HalfFlow-LM as a new cross-compiled `registration` module. Its first
release is a supplied-affine, T1-to-T1 nonlinear registration engine with:

- two inverse-tracked pull maps meeting in a common work frame;
- opposite half-flow updates from one physical velocity field;
- robust local normalized-intensity features;
- a pointwise factored or symmetric 3x3 LM step selected by measurement;
- physical Sobolev shaping;
- paired scaling-and-squaring;
- gain-ratio acceptance and accumulated-map topology guards; and
- a physical coarse-to-fine schedule.

The new module should remain small because it owns registration mathematics,
not because kernels are hidden or omitted. General field operations belong in
`image`; reusable small linear algebra and operator/workspace improvements may
belong in Gale. No registration-specific voxel stencil should be pushed into
Gale merely to claim reuse.

The first milestone does not include FFT proposals, learned features, a GPU
backend, half precision, or a fully symmetric affine factorization. Those are
experiments gated by evidence from the deterministic core.

## Product claim and success condition

The credible initial claim is:

> Given a supplied rigid or affine prealignment, HalfFlow-LM performs compact,
> symmetric nonlinear T1 registration with an explicitly tracked inverse,
> guarded topology, JVM and Scala.js parity, and allocation-controlled kernels.

It is not yet credible to claim that the whole pipeline is swap-symmetric, that
sampled forward and inverse fields are exact inverses, or that it matches ANTs.
Those are testable outcomes rather than properties of a class name.

The first cohort-level success condition is all of:

1. A preregistered noninferiority margin is met for label Dice or surface Dice
   against one named ANTs SyN configuration using the same preprocessing and
   affine initialization.
2. No evaluated in-mask voxel has a non-positive accumulated-map Jacobian.
3. Forward-inverse composition error stays within a calibrated physical or
   voxel-space budget.
4. The median end-to-end runtime is lower with a confidence interval that does
   not cross parity. A 2x speedup and at most 75% of peak memory are stretch
   targets, not release facts.
5. All portable behavior compiles and passes on both JVM and Scala.js.

## Repository fit

The current module boundaries make the ownership decision fairly sharp:

- `image` owns grids, spatial domains, scalar volumes, dense vector fields,
  morphisms, interpolation, and resampling.
- `motion` owns rigid fMRI motion estimation and QC. Its module contract
  explicitly excludes heavy registration engines.
- `spatial` owns transform routing, operator planning, and provenance. It can
  consume a completed registration as an image morphism but must not be a
  dependency of the numerical engine.
- Gale is the external linear-algebra layer. ScalaFIM currently pins it as a
  source dependency and cross-compiles against its core on JVM and JS.

Therefore add:

```text
image ───────────────┐
                    ├── registration ──> exported DenseFieldMorphism pair
Gale core ──────────┘

spatial ──> image morphisms (consumer only)
motion  ──> remains independent rigid-fMRI machinery
```

The implementation phase will add `registrationJVM` and `registrationJS` to
`build.sbt`, the root aggregates and aliases, `README.md`, and
`docs/module-relations.md`. Those files are already modified in the current
working tree, so this planning slice deliberately does not touch them.

## Existing assets and measured gaps

### Image

Useful foundations already exist:

- `GridSpec`, `SpatialDomain`, `SpatialMorphism`, and
  `DenseFieldMorphism` express physical-space mappings.
- `DenseVectorField` already stores vector components in a structure-of-arrays
  layout.
- trilinear scalar sampling and dense-field interpolation semantics already
  exist.
- dense-field inversion and numerical Jacobians are useful reference paths.
- the image suites already use independent signed-axis, analytic-affine, and
  nibabel-backed orientation/resampling oracles.

The current high-level paths are not registration-loop kernels. Several build
boxed `Vector[WorldPoint]`, `Vector[VoxelPoint]`, per-point stencils, or small
matrix objects. Registration needs destination-writing loops over primitive
storage, reusable workspaces, fused validity handling, and reductions that do
not materialize one object per voxel.

This is an opportunity to strengthen `image` without making it registration
specific.

### Gale

The pinned Gale API provides typed dense matrices/vectors, tiny matrices,
matrix-free operators, iterative solvers, and increasingly mature
destination/workspace APIs. Its accelerated backend boundary currently covers
dense BLAS-like operations; it is not a 3D tensor, convolution, field, FFT, or
GPU runtime.

The HalfFlow work should stress Gale in two legitimate areas:

1. allocation-free symmetric 3x3 algebra; and
2. reusable matrix-free operator/workspace contracts for a Helmholtz reference
   solve.

Gale's opaque array storage and the image module's `NArray` storage do not
currently form a public zero-copy interchange. That is a profiling question,
not permission to copy full volumes into `DVec` for every iteration. Hot image
fields stay in image-native primitive storage unless a small, general Gale
interop contract is justified by at least two consumers.

## Mathematical contract

### Pull-map convention

`DensePull[A, B]` maps coordinates in frame `A` to sampling coordinates in
frame `B`. Pulling a source image in `B` through this map produces an image in
`A`.

Let:

- `A: W -> F` map the work grid to the fixed source;
- `B: W -> M` map the work grid to the moving source.

The work-space images are `F(A(x))` and `M(B(x))`. For a velocity `v` expressed
in physical coordinates of `W`, construct:

```text
h+ = exp(+v / 2)
h- = exp(-v / 2)
A' = h+ >>> A
B' = h- >>> B
```

The fixed-to-moving result is `A.inverse >>> B`; its paired reverse is carried
alongside it.

### Local model

For a feature channel `c`, define the midpoint residual:

```text
r_c(x; v) = f_c(h+(x)) - m_c(h-(x))
```

At zero velocity its spatial derivative is:

```text
j_c = 0.5 * (grad(f_c) + grad(m_c))
```

The robust local quadratic uses:

```text
H = sum_c w_c * j_c * j_c^T
b = sum_c w_c * j_c * r_c
(H + lambda I) delta = -b
```

The implementation will compare two variants:

- a paper-aligned rank-one/scalar factored update; and
- a fused multi-channel symmetric 3x3 update.

The second is a plausible extension, not something established by the
factored-LM paper. It wins only if cohort accuracy per unit runtime and memory
is better.

### What inverse tracking does and does not prove

Composing both directions from the same paired flow makes inverse consistency
an algebraic invariant in an exact function space. Sampled fields are not exact
functions: interpolation, composition, rounding, and resolution transitions
introduce error.

Every accepted update must therefore measure:

- minimum and quantiles of `det(J)` for each local half-flow;
- the same statistics for the accumulated candidate maps;
- RMS and maximum `forward >>> backward` error;
- RMS and maximum `backward >>> forward` error; and
- valid-domain coverage for all reductions.

Checking only the proposed half-flow is insufficient. The accumulated
candidate can fold during sampled composition or regridding even when the
increment is locally safe.

### Trust-region semantics

An attempted step is accepted only when:

```text
actualDrop    = oldObjective - candidateObjective
predictedDrop = -(g^T v + 0.5 v^T H v)
rho           = actualDrop / predictedDrop
```

are finite, both drops are positive, and `rho >= etaAccept`.

The proposed algorithm must not accept every positive actual decrease while
merely using `rho` to update damping. That is damped monotone descent, not a
gain-ratio trust method.

Sobolev shaping changes the local step nonlocally. If `H` and `b` are not
materialized, `predictedDrop` requires a second fused pass over residuals,
gradients, weights, and the shaped velocity. The extra pass is part of the
algorithm and its benchmark. If it is removed, the API and documentation must
call the acceptance rule backtracking rather than LM trust-region acceptance.

Track `acceptedSteps` and `attempts` separately. A level terminates on the
accepted-step target, `maxAttempts`, a minimum useful update, or a diagnostic
failure. A rejected proposal does not consume an accepted iteration.

## Concrete Scala surface

The v1 public API should use concrete image-field types with phantom frames,
not expose a higher-kinded category framework to application users. The
category interpretation remains useful internally and in documentation.

Illustrative shape:

```scala
package scalafim.registration

opaque type Mm = Double

object Mm:
  def from(value: Double): Either[RegistrationError, Mm] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(RegistrationError.InvalidLength(value))

  extension (value: Mm)
    inline def toDouble: Double = value

sealed trait FixedFrame
sealed trait MovingFrame
sealed trait WorkFrame

final case class Frame[A] private (
    domain: SpatialDomainId,
    grid: GridSpec
)

final case class DensePull[A, B] private[registration] (
    from: Frame[A],
    to: Frame[B],
    sourceCoordinates: DenseVectorField
)

final case class InversePair[A, B] private[registration] (
    forward: DensePull[A, B],
    backward: DensePull[B, A]
):
  def inverse: InversePair[B, A] =
    InversePair(backward, forward)

final case class Midpoint[W, F, M] private[registration] (
    fixed: InversePair[W, F],
    moving: InversePair[W, M]
)

final case class RegistrationResult[F, M] (
    transform: InversePair[F, M],
    diagnostics: RegistrationDiagnostics
)
```

Runtime `Frame[A]` values complement the phantom type: the compiler prevents
obvious direction mistakes, while smart constructors reject grids or domain
identifiers that do not agree at runtime. An unsafe/internal constructor is
allowed only after those checks at a trusted kernel boundary.

The application-facing entry point should remain narrow:

```scala
val plan =
  T1HalfFlowPlan.default
    .withLevels(levels)
    .withGuard(guard)

val result =
  HalfFlowLm.register(
    fixed = fixedT1,
    moving = movingT1,
    initial = InitialTransform.Supplied(affinePair),
    plan = plan
  )
```

Return `Either[RegistrationError, RegistrationResult[FixedFrame, MovingFrame]]`
at the trust boundary. Constant images, empty overlap, invalid fields, exhausted
attempts, and unsafe flows are typed failures with diagnostics, not `NaN`
results or thrown cross-platform exceptions.

## Ownership and kernel boundary

| Capability | Owner | Reason |
| --- | --- | --- |
| Primitive scalar/vector pull into destination | `image` | General resampling operation |
| Dense pull-map composition into destination | `image` | General morphism operation |
| Dense pull-map regridding | `image` | General physical-grid operation |
| Analytic finite-difference Jacobian reduction | `image` | General field diagnostic |
| Mask-aware anti-aliased pyramid | `image` | General image operation |
| Frame-safe inverse pair and midpoint state | `registration` | Registration algebra |
| Local normalized T1 features | `registration` | Metric-specific policy |
| Robust local model and pointwise solve orchestration | `registration` | Registration objective |
| Sobolev step shaping and strain/displacement caps | `registration` | Deformation geometry |
| Paired exponential and trust acceptance | `registration` | Registration algorithm |
| Tiny symmetric 3x3 primitive | Gale, if general | Reusable linear algebra |
| Matrix-free screened-Poisson reference solve | Gale contracts | Reusable operator/solver oracle |
| Registration-specific smoother/multigrid stencil | `registration` | Domain kernel unless generalized later |
| Route/provenance integration | `spatial` consumer | Higher-level orchestration |
| NIfTI/ANTs benchmark harness | JVM test/tooling | Platform IO and external process use |

### Minimum honest kernel vocabulary

The implementation is likely more than seven kernels. Keep the vocabulary
small, but count responsibilities honestly:

1. `pullScalarInto` and `pullChannelsInto`;
2. `composePullInto`;
3. `regridPullInto`;
4. `jacobianAndInverseErrorReduce`;
5. `buildPyramidLevelInto`;
6. `normalizedFeaturesAndGradientInto`;
7. `normalStepInto`;
8. `predictedDropReduce`;
9. `shapeVelocityInto`;
10. `expPairInto`, orchestrating repeated composition.

`normalStepInto` may fuse residual, robust weights, local curvature, the 3x3
solve, and objective reduction. It writes only three velocity components plus
small reduction state. The predicted-drop pass uses the accepted shaped
velocity and must not silently substitute the unshaped raw step.

Every hot kernel has:

- a checked allocation-owning public wrapper;
- an internal destination-writing implementation;
- a reusable workspace where scratch is unavoidable;
- an independent small-volume reference implementation;
- explicit valid-mask and outside-domain behavior; and
- a JVM JMH plus Scala.js construction-counter benchmark when portable.

## Storage and coordinate policy

- Store scalar volumes and vector components in image-native primitive
  `NArray[Double]` initially.
- Keep vector fields structure-of-arrays to match `DenseVectorField` and allow
  contiguous component passes.
- Express velocities, smooth lengths, displacement caps, gradients, and
  Jacobians in physical coordinates.
- Convert physical feature radii into separate per-axis voxel radii for
  anisotropic grids.
- Accumulate maps in source physical coordinates and resample original source
  data after each accepted transform. Never recursively warp already-warped
  intensities.
- Treat mask/validity as a first-class channel. Outside values must not enter
  local means, variances, robust energy, predicted decrease, or topology
  reductions.
- Start in `Double` on JVM and JS. Float, BF16, or FP16 storage is an explicit
  differential experiment after the reference engine passes.

## T1 feature experiment

Use bias-resistant local normalized channels and a physical gradient channel as
the minimal metric family. Initial candidate radii are hypotheses, not API
constants.

Two feature execution strategies must be compared:

### Reference: warp, then normalize

At each accepted state, pull original intensities into the work grid, compute
masked local statistics there, and differentiate in work coordinates. This
matches the written objective most directly but repeats window operations.

### Fast candidate: normalize, then warp

Precompute normalized feature pyramids in each source frame, pull the channels
into the work grid, and compute work-grid gradients. This reduces accepted-step
cost but does not commute exactly with interpolation or spatially varying
warps.

Do not choose between them by intuition. Compare objective consistency,
registration accuracy, accepted-step count, runtime, and memory. Do not warp a
source-frame gradient as a plain three-vector without the appropriate Jacobian
transformation; recomputing gradients in the work grid is the simpler safe
choice.

Use Charbonnier or Huber influence with a scale tied to robust residual
statistics. A single hard-coded `1e-3` is not meaningful across intensity
normalizations and feature channels.

## Deformation geometry and the Gale stress test

The conceptual step is:

```text
local LM force
  -> mask projection
  -> inverse of (I - ell^2 Laplacian)^p
  -> strain cap
  -> physical displacement cap
```

Start with `p = 2` as an experiment. Boundary conditions, mask boundaries, and
the discrete Laplacian are part of the contract; they cannot be left implicit.

Implement two lanes:

1. A matrix-free Gale `LinearOperator` plus a reusable iterative-solver
   workspace as the portable differential reference.
2. A specialized image-grid implementation, initially separable or multigrid,
   as the performance candidate.

Verify the fast lane by:

- residual norms against the declared discrete operator;
- analytic Fourier modes on periodic test grids where applicable;
- constant and linear-field preservation appropriate to the boundary policy;
- comparison with the Gale reference on small 3D grids; and
- downstream objective and topology behavior.

The stress-test ledger records each discovered Gale limitation as one of:

- use the existing Gale API;
- improve a general Gale destination/workspace API with its own tests and
  benchmarks;
- keep the operation in ScalaFIM because it is field-specific; or
- defer because no measured bottleneck exists.

No upstream change is accepted solely because registration would like a more
convenient private shortcut.

## Paired flow and resolution transitions

For the requested half time, choose a squaring depth `n` from a conservative
bound on the physical velocity gradient, initialize:

```text
p0 = id + v / 2^(n + 1)
q0 = id - v / 2^(n + 1)
```

and square both `n` times. This approximates `exp(+v/2)` and `exp(-v/2)`.

At every squaring step, reuse destination buffers and alternate workspaces.
Measure local inverse error and Jacobians before constructing the trusted
`InversePair`. After composing with the current midpoint state, repeat the
guard on the accumulated candidate.

At a resolution transition, trilinearly regrid source-coordinate maps, rebuild
identity coordinates from the new physical grid, and rerun both guards. Cubic
map interpolation is excluded from v1 because overshoot can create folds even
when the coarse map was safe.

## Initialization scope

The nonlinear milestone accepts a supplied inverse-tracked rigid or affine
pair. This separates deformation correctness from initialization quality and
allows the ANTs comparison to use exactly the same affine starting point.

A later initializer must solve three additional contracts:

- robust pairwise rigid-affine estimation rather than fMRI time-series motion;
- an inverse pair without numerical dense-field inversion; and
- a midpoint factorization or an explicitly narrower symmetry claim.

Given only an affine `T: F -> M`, choosing `A = id` and `B = T` produces the
correct final transform but privileges the fixed frame. A true symmetric split
requires a well-defined affine square root or equivalent construction and a
swap-equivariance oracle. Until that is implemented, describe v1 as having
symmetric nonlinear updates after supplied affine initialization.

The private pairwise routines in `motion` may inform the design, but the new
initializer should not reach into private APIs or turn `motion` into a
nonlinear-registration dependency.

## Coarse proposals

FFT correlation is a conditional extension after the local engine is measured
on cases with large residual displacement. It is not part of the minimum
credible core.

Any proposal interface returns only an untrusted velocity candidate. FFT,
landmark, or learned proposals pass through the same shaping, paired flow,
objective, accumulated-map Jacobian, inverse-error, and trust gates.

Do not reuse the private FFT implementation in `hrf`. If FFT correlation earns
its place, first determine whether Gale needs a general FFT capability with a
second consumer or whether a registration JVM adapter should use a focused
backend. The full block-matching/interpolation procedure must be benchmarked;
the asymptotic cost of one FFT does not establish that the proposal is cheap.

BrainMorph/ONNX assistance is post-v1. It must remain optional and cannot alter
the deterministic deformation contract.

## Dependency-ordered implementation roadmap

### P0 — freeze contracts and baselines

Tracker: `bd-01KY3J2SE1Y5JXR135B3VKFXW4`

- Write an algorithm note with pull direction, physical units, masks, boundary
  conditions, robust loss, trust ratio, and termination rules.
- Build analytic identity, translation, affine, sinusoidal-velocity, and
  generated-diffeomorphism fixtures.
- Freeze the named ANTs command, version, thread count, initialization,
  interpolation, masks, preprocessing, and resource-measurement protocol.
- Record current image and Gale kernel baselines before optimizing them.

Exit: the synthetic truth and benchmark protocol can be replayed from a clean
checkout and produce machine-readable receipts.

### P1 — allocation-controlled image field kernels

Tracker: `bd-01KY3J2SQHTGX7Y9TA4SY3KJS1`

- Add destination-writing scalar/channel pull, map composition, map regridding,
  Jacobian reduction, inverse-error reduction, and anti-aliased pyramid kernels.
- Preserve current high-level APIs; delegate where semantics match.
- Verify against existing image references and independent analytic fields on
  JVM and JS.
- Add allocation/construction and throughput receipts.

Exit: no registration iteration needs boxed point or stencil collections.

### P2 — Gale tiny solves and operator workspaces

Tracker: `bd-01KY3J2T4910MKS45KQB3E37JT`

- Benchmark a local hand-written symmetric 3x3 reference against Gale's tiny
  types.
- Add a general destination-writing symmetric solve to Gale only if the public
  contract is sound and measurement supports it.
- Exercise matrix-free Helmholtz operators with explicit reusable workspaces.
- Keep exact pin-switch compile/test evidence for any Gale revision adopted by
  ScalaFIM.

Exit: the chosen tiny solve and reference smoothing path have JVM/JS correctness
and allocation receipts; upstream ownership is explicit.

### P3 — typed inverse pair, midpoint, and safe paired flow

Tracker: `bd-01KY3J2TH1KFTN5PZZ6HKVJ4BB`

- Add `registration` with frame-safe maps and runtime grid checks.
- Implement identity, composition, regridding, `Midpoint.advance`, and paired
  scaling-and-squaring on top of P1 kernels.
- Guard local and accumulated candidates.
- Export the result as paired `DenseFieldMorphism` values.

Exit: analytic flows pass identity, inverse, swap, Jacobian, and resolution
transition properties on JVM and JS.

### P4 — T1 features, factored LM, and honest trust acceptance

Tracker: `bd-01KY3J2TWJSASSVSVVD43P35Q0`

- Implement masked local normalization and work-grid gradients.
- Implement and ablate rank-one and multi-channel 3x3 updates.
- Implement the shaped-step predicted-decrease pass.
- Separate attempt and accepted-step accounting.
- Emit per-attempt diagnostics sufficient to explain every rejection.

Exit: finite-difference directional derivatives and quadratic predictions agree
on small grids, and accepted objective values are monotone under the declared
rule.

### P5 — Sobolev shaping and the nonlinear engine

Tracker: `bd-01KY3J2VD02GN44RAGAKMM44R1`

- Implement the Gale reference and specialized smoothing lanes.
- Add strain/displacement limiting, physical multi-resolution schedules, and
  original-image resampling.
- Compare the two feature execution strategies.
- Tune only from versioned experiment receipts.

Exit: supplied-affine synthetic and public T1 pairs complete without folds,
with bounded inverse error and replayable performance reports.

### P6 — robust pairwise rigid-affine initialization

Tracker: `bd-01KY3J2VQN18101KYHZKB96W55`

- Implement a reusable pairwise initializer outside `motion` internals.
- Add robust multistart behavior and an inverse-tracked result.
- Decide whether affine midpoint factorization is supported; if it is, require
  a swap-equivariance oracle.

Exit: the end-to-end pipeline has a precisely stated symmetry contract and does
not weaken the supplied-affine nonlinear path.

### P7 — cohort validation and performance hardening

Tracker: `bd-01KY3J2W0Y7VBEZG5CZ1MYMYKE`

- Run labeled in-domain and scanner/site-shift cohorts.
- Compare against the frozen ANTs protocol.
- Run component ablations and failure stratification.
- Optimize only the kernels demonstrated to dominate profiles.
- Publish exact environment, commits, commands, arrays, summaries, and logs.

Exit: the success condition is either met with replayable evidence or the gap
is localized without expanding the framework by default.

### PX — optional coarse proposals

Tracker: `bd-01KY3J2WABXY3ZM1K87J8BK363`

- Add FFT block correlation only if P7 identifies capture range as a material
  residual failure.
- Add learned keypoints/features only after the deterministic proposal has an
  honest baseline.

Exit: each optional proposal earns its runtime, memory, and maintenance cost in
an ablation and retains the common safety/acceptance path.

## Verification strategy

### Contract and oracle tests

| Contract | Primary oracle | Cross-check |
| --- | --- | --- |
| Pull direction and physical orientation | Analytic affine field | nibabel or SimpleITK fixture |
| Trilinear scalar/channel sampling | Analytic linear intensity | existing image sampler |
| Map composition | Analytic affine composition | pointwise reference |
| Jacobian determinant | Affine determinant and sinusoidal field | finite differences in Python/ITK |
| Paired flow | Constant translation and generated small SVF | independent scaling/squaring fixture |
| Local gradient/model | Central finite-difference objective | hand-computed one-voxel cases |
| Symmetric 3x3 solve | Residual of `(H + lambda I)x + b` | dense high-precision solver fixture |
| Helmholtz shaping | Discrete residual and Fourier modes | Gale operator solve |
| Inverse pair | Both composition orders | independent sampled-point evaluation |
| Pyramid transfer | Physical-coordinate analytic map | fine/coarse/fine round trip |

Tests produced from the same implementation count as consistency checks, not
independent oracles.

### Metamorphic and property tests

- Identity images and identity initialization produce a zero accepted update.
- A common positive affine intensity transform leaves normalized-channel
  correspondence approximately invariant away from masks and boundaries.
- Swapping fixed/moving and inverting the supplied initialization produces the
  inverse nonlinear result within sampled-field tolerance.
- Re-expressing an analytic case on a physically equivalent anisotropic grid
  preserves the physical transformation within discretization tolerance.
- Tighter damping reduces local step magnitude for the same model.
- Every accepted state has no larger declared objective than its predecessor.
- Compose/regrid never changes frame endpoints.
- `pair.inverse.inverse == pair` structurally; sampled compositions satisfy the
  calibrated numerical budget.
- Results are deterministic for a fixed backend, schedule, and thread policy.

### Adversarial tests

- constant and nearly constant images;
- empty, tiny, or disjoint valid overlap;
- strong bias field, gain/offset changes, lesions, and skull-strip mismatch;
- partial field of view;
- anisotropic and oblique grids;
- large residual translation after supplied affine;
- nearly singular local normal systems;
- velocity near the topology threshold;
- invalid source values, non-finite reductions, and exhausted attempts; and
- resolution transfer near mask and domain boundaries.

Each case must either return a safe typed result or a typed diagnostic failure.

### Regression receipts

Every algorithm experiment records:

- ScalaFIM and Gale commit hashes;
- JVM, Scala.js, JDK, Node, OS, CPU, and thread configuration;
- dataset/fixture checksums and preprocessing;
- complete plan and threshold serialization;
- per-level attempts, accepts, rejections, objective, `rho`, damping, minimum
  Jacobian, inverse error, and valid coverage;
- accuracy arrays rather than only aggregate means;
- wall time, CPU time where possible, peak RSS, and allocation counters; and
- the exact ANTs command and version for comparison.

### Performance gates

Use JMH on JVM for 64^3, 128^3, and 256^3 representative fields, including
`-prof gc`. Use Scala.js full-link benchmarks and construction counters rather
than JVM allocation claims.

After output/workspace setup, destination-writing kernels should allocate no
full-volume temporaries. A provisional JVM microbenchmark gate is at most 64
bytes/op for leaf `Into` kernels; any larger result needs an explained receipt,
not a relaxed assertion. End-to-end steps may rotate preallocated full-volume
buffers but must record their count and lifetime.

Do not put absolute wall-clock thresholds in portable CI. Store distributions
and compare named baselines on controlled runners. Profile before adding
parallelism, SIMD, Float storage, or a new backend.

## Thirty design options considered

Scores are relative: impact and risk are high/medium/low; effort is
small/medium/large. Confidence refers to the current repository evidence.

| # | Candidate | Impact | Effort | Risk | Confidence | Evidence or dependency | Verdict |
| ---: | --- | --- | --- | --- | --- | --- | --- |
| 1 | Add a dedicated `registration` crossProject | High | Medium | Low | High | Existing module contracts exclude nonlinear work from `motion` | Keep |
| 2 | Use concrete phantom-frame dense pulls | High | Medium | Low | High | Prevents direction errors and matches image morphisms | Keep |
| 3 | Expose a generic higher-kinded category API in v1 | Low | Medium | Medium | High | Elegant algebra, but no second public implementation exists | Reject for v1 |
| 4 | Pair phantom frames with checked runtime grid tags | High | Small | Low | High | Phantom types cannot validate loaded data alone | Keep |
| 5 | Maintain forward/backward maps together | High | Medium | Medium | High | Central midpoint algebra; still needs sampled guards | Keep |
| 6 | Add destination-writing image pull kernels | High | Medium | Low | High | Current boxed plans are unsuitable for iterative use | Keep |
| 7 | Add destination-writing dense-map composition | High | Medium | Medium | High | Required by every squaring and accepted update | Keep |
| 8 | Add fused Jacobian/inverse-error reductions | High | Medium | Medium | High | Existing pointwise/matrix paths allocate heavily | Keep |
| 9 | Add a mask-aware anti-aliased physical pyramid | High | Medium | Medium | High | Current block downsampling is not the full contract | Keep |
| 10 | Recompute normalized features after every warp | Medium | Medium | Low | Medium | Closest match to written objective; potentially expensive | Keep as reference |
| 11 | Precompute source feature pyramids and warp channels | High | Medium | Medium | Medium | Faster, but normalization and warping do not commute | Benchmark candidate |
| 12 | Implement paper-aligned rank-one local LM | High | Small | Medium | High | Closest to factored-LM evidence | Keep |
| 13 | Implement fused multi-channel symmetric 3x3 LM | High | Medium | Medium | Medium | Natural extension, not directly established by cited paper | Keep as ablation |
| 14 | Generalize an allocation-free tiny symmetric solve in Gale | Medium | Medium | Medium | Medium | Reusable only if API and benchmark justify upstream work | Conditional keep |
| 15 | Compute predicted drop after Sobolev shaping | High | Medium | Low | High | Required for honest gain-ratio acceptance | Keep |
| 16 | Accept steps by an explicit gain-ratio threshold | High | Small | Low | High | Fixes the positive-drop-only acceptance flaw | Keep |
| 17 | Use Gale CG as the production Helmholtz path | Medium | Small | Medium | Low | Useful contract, but current generic iteration may allocate or lag | Reference only initially |
| 18 | Build a specialized separable or multigrid shaper | High | Large | Medium | Medium | Likely speed path; must match declared discrete geometry | Conditional keep |
| 19 | Use paired adaptive scaling-and-squaring | High | Medium | Medium | High | Compact diffeomorphic update with shared diagnostics | Keep |
| 20 | Guard only the incremental half-flow | Low | Small | High | High | Sampled accumulated composition can still fold | Reject |
| 21 | Guard both local and accumulated candidates | High | Medium | Low | High | Measures the actual accepted state | Keep |
| 22 | Use trilinear map transfer between levels | High | Small | Low | High | Avoids cubic overshoot; still recheck topology | Keep |
| 23 | Ship supplied-affine nonlinear registration first | High | Small | Low | High | Isolates nonlinear correctness and enables fair baselines | Keep |
| 24 | Implement a symmetric affine midpoint split immediately | Medium | Large | Medium | Low | Requires matrix-function and swap-equivariance contracts | Defer to P6 |
| 25 | Reuse private fMRI motion estimation as the public initializer | Low | Small | High | High | Wrong module/API boundary and different workflow contract | Reject |
| 26 | Add coarse FFT block proposals in the core milestone | Medium | Large | Medium | Medium | Useful only if capture range is measured as limiting | Defer to PX |
| 27 | Add ONNX learned keypoints or features in v1 | Medium | Large | High | Medium | Optional value, substantial platform/distribution surface | Defer to PX |
| 28 | Store features in FP16/BF16 immediately | Medium | Medium | High | Low | No current backend or accuracy evidence | Reject for v1 |
| 29 | Create a generic GPU tensor backend before profiling | Medium | Very large | High | High | Gale does not currently provide this boundary | Reject |
| 30 | Publish fair ANTs cohort and ablation receipts | High | Large | Low | High | Only credible basis for parity and speed claims | Keep |

## The three highest-leverage bets

1. **Allocation-controlled image field kernels.** They are prerequisites for
   every algorithm variant, directly improve a core module, and make speed
   claims measurable rather than architectural speculation.
2. **An honest guarded midpoint engine.** Accumulated-map checks, shaped-step
   prediction, and accepted-step accounting turn the attractive algebra into a
   falsifiable numerical contract.
3. **Dual reference/fast lanes for local algebra and smoothing.** Gale-backed
   references plus specialized field kernels let this project stress Gale
   constructively without forcing registration storage or stencils into a
   generic matrix library.

The largest unknowns are feature execution order, rank-one versus full 3x3
updates, smoothing implementation, and residual affine capture range. The plan
keeps each one behind a differential test or ablation instead of growing a
transform/metric/optimizer framework.

## Notes discipline

Keep two durable records during implementation:

1. Tracker notes for decisions, dependencies, blockers, and acceptance evidence.
2. Versioned benchmark receipts under a dedicated registration results or
   benchmark directory with exact commands and raw arrays.

Every performance-driven Gale change records the original ScalaFIM profile,
the general Gale contract, Gale-local tests/benchmarks, the adopted Gale pin,
and ScalaFIM JVM/JS pin-switch proof. Every image optimization records the
reference path it matches and an independent oracle where available.

Do not report a green suite as accuracy parity, a zero-fold synthetic fixture as
cohort topology evidence, or an inverse-pair data type as proof of zero sampled
inverse error.

## Primary references checked

- [ANTs registration stages and multiresolution guidance](https://github.com/ANTsX/ANTs/wiki/Anatomy-of-an-antsRegistration-call)
- [SyN: symmetric diffeomorphic registration](https://pmc.ncbi.nlm.nih.gov/articles/PMC2276735/)
- [Factored Levenberg-Marquardt for image registration](https://arxiv.org/html/2603.19371v1)
- [SITReg and constructive inverse consistency](https://arxiv.org/abs/2303.10211)
- [FireANTs and stable field upsampling](https://arxiv.org/html/2404.01249v5)
- [FFT-based intermediate deformation proposal](https://www.nature.com/articles/s41598-026-40961-1)
- [BrainMorph keypoint initialization](https://arxiv.org/abs/2405.14019)
- [Beyond the LUMIR challenge](https://arxiv.org/abs/2505.24160)
