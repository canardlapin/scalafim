# HalfFlow-LM v1 numerical contract

> Historical design record. The implemented contract is now owned by
> `canardlapin/reframe4s` at commit `e7f469c`; paths below describe the original
> ScalaFIM implementation before extraction.

- Status: frozen P0 contract
- Date: 2026-07-21
- Epic: `bd-01KY3J2R58MPZNKX7ZKTVYPQTM`
- P0: `bd-01KY3J2SE1Y5JXR135B3VKFXW4`
- Design plan: [`half-flow-lm-registration.md`](half-flow-lm-registration.md)
- Independent fixture:
  [`half-flow-synthetic.json`](../../modules/registration/fixtures/v1/half-flow-synthetic.json)

This document is the implementation contract for the first HalfFlow-LM
nonlinear engine. It freezes directions, units, masks, objective semantics,
trust acceptance, safety checks, termination, diagnostics, and oracle classes
before optimized kernels exist.

Terms such as MUST, MUST NOT, SHOULD, and MAY are normative.

## Scope

V1 is a supplied-initialization, same-modality T1 nonlinear registration
engine. The core accepts two inverse-tracked maps from a work frame to the
fixed and moving source frames. A convenience entry point MAY construct an
asymmetric initial midpoint from a supplied fixed-to-moving affine, but it MUST
label the resulting whole-pipeline symmetry claim accordingly.

V1 includes:

- physical multiresolution grids;
- midpoint residuals and opposite half-flow updates;
- masked, local normalized-intensity features;
- robust rank-one or multi-channel 3x3 local models;
- physical Sobolev shaping;
- paired scaling-and-squaring;
- gain-ratio acceptance;
- accumulated-map topology and inverse-error guards; and
- paired dense pull maps as output.

V1 excludes FFT proposals, learned features, half precision, a GPU abstraction,
and a guarantee of symmetric affine factorization.

## Frames, points, and maps

### Physical coordinates

Every registration map, velocity, displacement cap, feature radius, smoothing
length, inverse error, and derivative is expressed in physical world
coordinates. Length is measured in millimetres. Voxel indices are used only to
address sampled storage.

A `Frame[A]` combines a phantom frame parameter with a runtime
`SpatialDomainId` and `GridSpec`. Constructors MUST reject mismatched runtime
endpoints even when phantom types agree through an unsafe cast or decoded
data.

### Pull direction

`DensePull[A, B]` maps a point in frame `A` to a sampling point in frame `B`.
Pulling `Image[B]` through it produces `Image[A]`.

For maps:

```text
ab: A -> B
bc: B -> C
ab >>> bc: A -> C
(ab >>> bc)(x) = bc(ab(x))
```

For homogeneous column points, the composed affine matrix is `M_bc * M_ab`.
This exact convention is encoded in the independent fixture.

No public API may use an ambiguous field name such as `transform` without
exposing both endpoint frames or source/target domain identifiers.

### Dense storage

Dense coordinate and velocity fields MUST use structure-of-arrays storage:

```text
x[0 .. nVoxels)
y[0 .. nVoxels)
z[0 .. nVoxels)
```

V1 storage is `NArray[Double]`. An `Into` kernel MUST accept caller-owned
destination and scratch storage. An allocating wrapper MAY exist at the public
boundary.

## Midpoint state

The state consists of:

```text
A: W -> F, with A^-1: F -> W
B: W -> M, with B^-1: M -> W
```

where `W` is the work frame, `F` is the fixed source frame, and `M` is the
moving source frame.

Work-space images are always sampled from their original level sources:

```text
F_W(x) = F(A(x))
M_W(x) = M(B(x))
```

Given paired half-flows in `W`:

```text
h+ = exp(+0.5 v)
h- = exp(-0.5 v)
```

an accepted update is:

```text
A' = h+ >>> A
B' = h- >>> B
```

The result pair is:

```text
fixedToMoving = A^-1 >>> B
movingToFixed = B^-1 >>> A
```

The implementation MUST carry both directions through composition. It MUST NOT
numerically invert the final dense field during ordinary optimization.

### Symmetry claim

The nonlinear update is symmetric under swapping fixed and moving, negating
the velocity, and swapping the midpoint arms, within sampled-field tolerance.

The core SHOULD accept a complete initial `Midpoint[W, F, M]`. If a convenience
constructor receives only `T: F -> M`, v1 MAY use:

```text
W = F
A = identity[F]
B = T
```

This preserves the requested final initialization but privileges `F` as the
work frame. Results from that convenience path MUST be described as symmetric
nonlinear refinement after supplied affine initialization, not a fully
swap-symmetric pipeline. A stronger claim requires an affine midpoint split and
a swap-equivariance oracle.

## Level construction and masks

Each level is constructed from the original source images and an explicit
physical work grid. It MUST NOT downsample an already-warped image or recursively
warp intensities from an earlier accepted state.

Before a level starts:

1. low-pass filter each original source consistently with its shrink factor;
2. sample the filtered sources or source features at the level resolution;
3. regrid the accumulated coordinate maps with trilinear interpolation;
4. rebuild identity coordinates from the new physical `GridSpec`;
5. calculate a stable evaluation mask; and
6. guard both regridded accumulated maps before optimization.

The evaluation mask is fixed for all attempts at a level. It is the intersection
of the user masks, finite source support, feature-window support, derivative
support, and a boundary margin at least as large as the permitted level
displacement. A candidate that cannot sample every active voxel is rejected;
it cannot improve the mean objective by discarding difficult voxels.

Every energy, predicted decrease, Jacobian summary, and inverse-error summary
MUST report its denominator or valid voxel count. Empty or insufficient support
is a typed failure.

Outside-domain map composition MUST NOT clamp to the nearest valid coordinate.
It produces invalid support and therefore rejection or a typed boundary error.

## Feature contract

The reference feature path is:

1. pull original level intensities into `W`;
2. compute masked local means and variances in `W`;
3. form normalized channels; and
4. differentiate the work-space channels in physical coordinates.

For physical radius `r`, axis `d` with spacing `s_d` uses an explicitly recorded
voxel radius derived from `r / s_d`. The rounding policy and minimum valid window
fraction are serialized in the plan.

A normalized intensity channel is:

```text
psi_r(I) = (I - mu_r(I)) / sqrt(variance_r(I) + epsilon_r^2)
```

`epsilon_r` is tied to a robust level/channel scale and MUST NOT be a universal
raw-intensity constant.

A faster normalize-then-warp path MAY be implemented. It is an approximation
because normalization and interpolation do not commute. It requires a
differential accuracy test against the reference path and a cohort ablation.

Source-frame gradients MUST NOT be pulled as ordinary three-channel intensities
unless the correct covector/Jacobian transformation is applied. Recomputing
gradients in `W` is the reference behavior.

## Residual and robust local model

At the current state, for channel `c`:

```text
r_c = fixedFeature_c - movingFeature_c
j_c = 0.5 * (gradFixed_c + gradMoving_c)
rho_epsilon(r) = sqrt(r^2 + epsilon_c^2) - epsilon_c
w_c = 1 / sqrt(r_c^2 + epsilon_c^2)
```

The local IRLS quadratic is:

```text
H_x = sum_c w_c j_c j_c^T
b_x = sum_c w_c j_c r_c
(H_x + lambda I) delta_x = -b_x
```

The energy is the sum of robust channel losses divided by the fixed active
voxel count. All reductions use at least `Double` accumulation in v1.

Two solver variants are admitted:

- the scalar/rank-one closed form aligned with the factored-LM reference; and
- a fused multi-channel symmetric 3x3 solve.

The 3x3 solve MUST check finite inputs and a declared positive-definiteness or
pivot condition after damping. A failed solve produces a zero raw force at that
voxel plus a counted diagnostic; if the failed fraction exceeds the serialized
level budget, the attempt fails.

The default variant is selected by accuracy, runtime, peak memory, and accepted
step count. The 3x3 extension is not accepted merely because it appears more
second-order.

## Sobolev shaping

The raw local result is a force. The candidate velocity is:

```text
v = (I - ell^2 Laplacian)^(-p) delta
```

followed by mask projection, a physical strain cap, and a physical displacement
cap. V1 starts by evaluating `p = 2`; this is not a universal constant.

The discrete Laplacian, boundary condition, mask-boundary behavior, solver
tolerance, and maximum iterations are serialized. A fast separable or multigrid
path MUST be differential-tested against a Gale `LinearOperator` reference on
small grids and MUST report the residual of the declared discrete system.

If the smoothing solve does not meet its residual contract, the attempt is
rejected. It must not pass a partially converged field to the flow silently.

## Predicted decrease

Trust prediction is evaluated at the final shaped and capped velocity, not the
raw pointwise solve:

```text
predictedDrop = -sum_x (b_x^T v_x + 0.5 * v_x^T H_x v_x)
```

The damping term is a solve stabilizer and is not included in the declared
image-model decrease. If `H` and `b` are fused rather than stored, a second
fused pass recomputes the local quantities and reduces this expression.

Using the raw step's predicted decrease after nonlocal shaping violates the v1
contract. Omitting predicted decrease changes the algorithm to monotone
backtracking and requires a different type and diagnostic schema.

## Paired exponential

For physical velocity `v`, choose the smallest non-negative squaring depth `n`
that satisfies both configured initial-step bounds:

```text
0.5 * maxNorm(v) / 2^n <= maxInitialDisplacementMm
0.5 * maxOperatorNorm(grad(v)) / 2^n <= maxInitialGradient
```

Initialize:

```text
p0(x) = x + v(x) / 2^(n + 1)
q0(x) = x - v(x) / 2^(n + 1)
```

and perform `n` self-compositions of each map using trilinear interpolation.
Alternate caller-owned work buffers. The result is the untrusted approximation
to `exp(+0.5 v)` and `exp(-0.5 v)`.

The depth, observed displacement and gradient bounds, every composition's
valid support, and final local-pair diagnostics are recorded.

## Numerical safety guard

Safety is evaluated twice:

1. on the proposed local half-flow pair; and
2. on the accumulated candidate midpoint maps after composition.

For both directions, the guard records:

- finite field values;
- minimum, 1st, 5th, 50th, 95th, and 99th percentiles of `det(J)`;
- count and fraction with `det(J) <= 0`;
- count and fraction below the configured positive determinant floor;
- RMS and maximum forward-backward composition error in millimetres and in
  local voxel units; and
- valid voxel count and fraction.

Jacobian derivatives use physical-coordinate finite differences. Central
differences are required on the guard mask; the level mask is eroded so
one-sided image-boundary estimates cannot decide topology acceptance.

A candidate is unsafe if any value is non-finite, any active determinant is
non-positive, the configured positive floor is crossed, either inverse-error
budget is crossed, or valid support changes. Unsafe candidates never reach
objective acceptance.

Inverse tracking is structural; zero sampled inverse error is not assumed.

## Trust acceptance and damping

For a safe candidate evaluated on the unchanged active mask:

```text
actualDrop = currentValue - candidateValue
gainRatio = actualDrop / predictedDrop
```

Accept only if all of these hold:

- current, candidate, actual, predicted, and ratio values are finite;
- `actualDrop > 0`;
- `predictedDrop > 0`; and
- `gainRatio >= etaAccept`.

Initial v1 policy values are hypotheses and are serialized:

```text
etaAccept = 0.10
lowGain = 0.25
highGain = 0.75
```

Damping is clamped to declared finite positive bounds. With current damping
`lambda`:

```text
gainRatio < lowGain   -> min(lambdaMax, 4 * lambda)
gainRatio > highGain  -> max(lambdaMin, 0.5 * lambda)
otherwise             -> lambda
```

An unsafe candidate, a non-positive prediction, or a non-finite calculation is
rejected and multiplies damping by four. State, current warped images, and the
linearization remain unchanged after rejection.

The independent fixture freezes representative trust decisions, including an
accepted low-gain step whose damping still increases.

## Iteration and termination

The following counters are distinct:

- `attempts`: every proposed velocity after a local solve;
- `safeAttempts`: proposals passing both numerical guards;
- `acceptedSteps`: safe proposals passing objective acceptance; and
- rejection counts partitioned by typed reason.

A rejected attempt increments `attempts` but not `acceptedSteps`. The schedule's
iteration count means accepted steps.

A level terminates on the first of:

- target accepted-step count;
- maximum attempt count;
- relative objective change below tolerance for the configured accepted-step
  patience;
- maximum physical velocity below tolerance;
- damping reaches its maximum without an acceptable step; or
- a typed unrecoverable data, support, or numerical failure.

The result distinguishes convergence, accepted-step budget exhaustion,
attempt-budget exhaustion, and failure.

## Error model

The public boundary returns `Either[RegistrationError, RegistrationResult]`.
The error ADT MUST distinguish at least:

- invalid image or grid;
- frame/domain mismatch;
- invalid initialization pair;
- empty or insufficient stable support;
- non-finite feature or objective;
- excessive local-solve failures;
- smoothing failure;
- unsafe flow with attached guard diagnostics;
- attempt budget exhausted; and
- unsupported platform/backend request.

Thrown exceptions and `NaN` sentinel results are not cross-platform API
behavior.

## Required diagnostics

Every run serializes:

- schema and algorithm version;
- ScalaFIM and Gale revisions;
- input and mask checksums;
- source/work grids and coordinate conventions;
- initialization provenance;
- complete feature, loss, smoothing, flow, guard, trust, and schedule settings;
- per-level and per-attempt counters;
- current/candidate objective, actual drop, predicted drop, gain ratio, and
  damping;
- local and accumulated guard summaries;
- elapsed times and owned full-volume buffer counts by phase; and
- terminal status and typed reason.

Diagnostics are part of the acceptance surface, not optional debug logging.

## Executable test obligations

### Contract tests

- phantom and runtime endpoints reject invalid compositions;
- pull direction agrees with the fixture's matrix convention;
- candidate rejection leaves state and current samples unchanged;
- accumulated maps, not only local flows, pass the guard;
- candidate energy uses original source data and the stable active mask;
- accepted and attempted counters follow the independent fixture; and
- buffer ownership and aliasing preconditions are checked at public boundaries.

### Analytic and differential oracles

- identity, affine translation, oblique affine, midpoint composition, swapped
  direction, and constant-velocity half time use the checked-in closed-form
  fixture;
- the slow point-flow reference matches the RK4 trajectory and variational
  Jacobian fixture;
- sampled scaling-and-squaring converges toward the RK4 point oracle under grid
  refinement;
- image pull and regridding retain existing analytic-affine and nibabel-backed
  orientation oracles;
- the 3x3 solve is checked by the residual of its original system and an
  independent dense solver; and
- the fast Sobolev lane is compared with the Gale operator solve.

### Metamorphic properties

- identity input produces zero accepted deformation;
- swapping images and midpoint arms produces the inverse result within the
  sampled budget;
- positive common intensity gain and offset leave normalized correspondence
  invariant away from mask boundaries;
- physically equivalent anisotropic grids agree under refinement;
- increasing damping does not increase the raw local step norm for the same
  model;
- accepted objective values are monotone; and
- pair inversion is involutive structurally and bounded numerically.

### Adversarial properties

- constant and nearly constant images;
- empty, tiny, and disjoint support;
- anisotropic and oblique grids;
- partial field of view and source-boundary motion;
- lesions and mask disagreement;
- singular and nearly singular local models;
- non-finite source values and reductions;
- velocities at both sides of the determinant guard; and
- repeated rejection until the attempt/damping limit.

### Performance properties

- reference and fast paths return the same checksum within their numerical
  contract;
- leaf `Into` kernels allocate no full-volume temporaries after setup;
- JVM reports time and allocation with JMH and a GC profiler;
- Scala.js reports full-link timings and explicit owned-volume construction
  counters; and
- no portable CI assertion uses an absolute wall-clock value.

## Tolerance policy

The fixture's strict tolerances apply to slow Double reference operations, not
blindly to sampled production fields.

Production tolerances are frozen only after a convergence ladder across at
least three grid resolutions and two squaring depths. A tolerance must be above
observed oracle/reference noise and below the smallest known contract-breaking
perturbation. Each tolerance records units, absolute and relative components,
the calibration fixture, and conditioning assumptions.

No tolerance may be widened solely to make JVM and Scala.js agree. A platform
difference first receives a differential diagnosis.

## Change control

Changing a direction, mask, objective, guard, trust, or termination rule
requires:

1. a tracker decision note;
2. an update to this contract;
3. regeneration or versioning of affected fixtures;
4. a regression test that distinguishes the old and new semantics; and
5. a new benchmark receipt if work or allocation changes.

The fixture schema is append-only within v1. Breaking meaning creates v2.
