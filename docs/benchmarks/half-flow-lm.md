# HalfFlow-LM benchmark and evaluation protocol

- Status: frozen protocol; P1-P6 admissions complete; first P7 real-data diagnostic captured
- Date: 2026-07-21
- Epic: `bd-01KY3J2R58MPZNKX7ZKTVYPQTM`
- Numerical contract:
  [`../plans/half-flow-lm-contract.md`](../plans/half-flow-lm-contract.md)
- P0 structural receipt:
  [`receipts/half-flow-lm-p0-baseline-2026-07-21.json`](receipts/half-flow-lm-p0-baseline-2026-07-21.json)

This protocol prevents a fast but different computation from being reported as
a registration speedup. Correctness, numerical behavior, work, allocation,
runtime, memory, and cohort accuracy are separate measurements.

## Admission order

A result is interpreted in this order:

1. fixture and invariant contracts pass;
2. output checksum or differential tolerance passes;
3. topology and inverse-error policy passes;
4. work and allocation receipts are valid;
5. timing and memory are interpreted; and
6. cohort accuracy is compared with the frozen baseline.

Timing from a failed earlier stage is diagnostic only.

## P0 baseline

P0 precedes the `registration` crossProject and the new allocation-controlled
image field kernels. There is no semantically comparable HalfFlow leaf kernel
to time yet. The current image API offers the required meanings, but several
paths materialize grid points, boxed vectors, or per-point stencils.

The P0 receipt therefore freezes:

- the ScalaFIM commit and immutable Gale revision;
- source hashes for the image field/resampling paths;
- the JVM, Node, Python, and ANTs availability;
- the current storage and visibility constraints; and
- the exact workloads the first optimized paths must measure.

This is a structural baseline, not a fabricated runtime comparison. P1 MUST
place reference and destination-writing implementations in the same benchmark
harness before either path is removed or its semantics change.

Capture the baseline:

```sh
python3 tools/registration/capture_halfflow_p0_baseline.py
```

Validate the historical receipt and its frozen hash inventory:

```sh
python3 tools/registration/capture_halfflow_p0_baseline.py --check
```

When the current checkout is intentionally expected to reproduce the original
capture exactly, add `--live-sources` to audit selected sources and the local
environment for drift.

The receipt intentionally uses selected source hashes rather than requiring a
clean global worktree; this repository is shared with unrelated active work.

## Kernel benchmark matrix

All deterministic inputs use seed `0x48464c4d` (ASCII `HFLM`) and physical
grids with the same generated values on JVM and Scala.js.

### K1: scalar pull

- Shapes: 64^3, 128^3, 256^3 on JVM; through 128^3 on Scala.js.
- Source: analytic world-linear plus smooth nonlinear intensity field.
- Maps: identity, oblique affine, smooth dense coordinates.
- Interpolation: nearest and trilinear; cubic is not a v1 registration path.
- Output: caller-owned scalar destination and validity mask.
- Checksum: weighted finite sum plus valid count.

Compare the current checked/reference route with `pullScalarInto` and
`pullChannelsInto`.

### K2: pull-map composition

- Same shapes and physical grids as K1.
- Inputs: identity/translation, oblique affine sampled as dense coordinates,
  and the smooth fixture velocity sampled at the grid.
- Operation: trilinear `left >>> right` into a caller-owned SoA destination.
- Output checks: analytic affine composition, valid count, inverse error, and
  checksum.

Record composition calls because scaling-and-squaring amplifies their cost.

### K3: regridding

- Transitions: 32^3 -> 64^3, 64^3 -> 128^3, and anisotropic
  48x64x40 -> 96x128x80.
- Input: absolute physical source coordinates, not voxel displacements.
- Operation: trilinear map regrid.
- Checks: analytic physical affine, smooth-field refinement error, determinant
  distribution, valid support, and checksum.

### K4: Jacobian and inverse reduction

- Inputs: identity, affine with known determinant, smooth fixture flow, and one
  intentionally folded field.
- Outputs: determinant quantiles/counts, RMS/max errors in millimetres and voxel
  units, valid count, and checksum.
- Contract: the folded field must fail admission before timing is interpreted.

Compare the fused reduction with an allocation-owning pointwise reference.

### K5: pyramid construction

- Shapes: 128^3 and 256^3.
- Grid cases: isotropic, anisotropic, and oblique.
- Inputs: analytic low/high-frequency mixture plus mask boundary.
- Levels: shrink 8, 4, 2, 1.
- Outputs: scalar pyramid, mask, physical grids, checksum, and alias-energy
  diagnostic.

Compare against an independent small-volume separable-convolution reference.

### K6: normalized features and gradients

- Channels: three physical-window normalized intensities plus gradient
  magnitude.
- Masks: full, eroded, and partial-FOV.
- Strategies: warp-then-normalize reference and normalize-then-warp candidate.
- Outputs: channels, physical gradients, valid counts, residual energy, and
  checksum.

The fast strategy must publish its differential error; timing alone cannot
select it.

### K7: local step and predicted decrease

- Local models: rank one, full rank, nearly singular, high dynamic range, and
  non-finite rejection.
- Variants: paper-aligned rank-one closed form and symmetric 3x3 solve.
- Outputs: raw velocity, failed-solve count, energy, shaped-step predicted drop,
  and checksum.

The 3x3 result is checked from the original system residual, not against another
copy of the same formula.

### K8: Sobolev shaping

- Shapes: 32^3, 64^3, 128^3.
- Forces: impulse, constant, analytic Fourier modes, and deterministic random
  smooth field.
- Lengths: coarse, middle, and fine physical scales.
- Outputs: velocity, declared operator residual, iterations/cycles, full-volume
  buffers, and checksum.

Compare the production path with the Gale matrix-free reference on small
grids. The reference is not assumed to be fastest.

### K9: paired exponential

- Shapes: 32^3, 64^3, 128^3.
- Velocities: zero, translation, fixture smooth field, and a near-guard field.
- Squaring depths: selected adaptive depth plus adjacent depths for convergence.
- Outputs: both maps, composition calls, Jacobian summary, inverse error, valid
  count, owned buffers, and checksum.

Compare sampled fixture points with the independent RK4 oracle and report error
under refinement.

### K10: accepted nonlinear step

- One complete attempt from already-built level sources and features.
- Cases: accepted high gain, accepted low gain, trust rejection, and topology
  rejection.
- Outputs: phase timings, attempted/accepted counters, allocation/work counts,
  objective model, guard summaries, and state checksum before and after.

Rejected cases must prove that current state and current warped images did not
change.

## JVM measurement

P1 adds an isolated JMH configuration rather than placing wall-clock assertions
in MUnit. The canonical single-thread diagnostic command will be:

```sh
sbt 'imageBenchJVM/Jmh/run -wi 5 -i 10 -f 3 -t 1 -prof gc .*HalfFlowImageKernelBenchmark.*'
```

Registration-owned kernels use:

```sh
sbt 'registrationBenchJVM/Jmh/run -wi 5 -i 10 -f 3 -t 1 -prof gc .*HalfFlowRegistrationBenchmark.*'
```

The image and registration commands are executable now. Each JMH
receipt includes:

- warmup, measurement, fork, and thread settings;
- JVM and GC flags;
- CPU and operating-system identity;
- shape, channel count, interpolation, and workspace policy;
- score distribution and units;
- `gc.alloc.rate.norm`, collection count, and time;
- valid count and deterministic checksum; and
- ScalaFIM/Gale revisions and source hashes.

The first admission run uses one thread. Parallel scaling is a separate receipt
at 1, 2, 4, and 8 threads and never replaces the single-thread comparison.

After output/workspace setup, leaf `Into` kernels must allocate no full-volume
temporary. The provisional leaf target is at most 64 bytes/op on the JVM. A
miss is investigated and documented; the numerical contract is not weakened to
meet it.

### P1 image-kernel receipt

The first clean canonical JMH run is recorded in
[`receipts/half-flow-image-kernels-p1-2026-07-21.json`](receipts/half-flow-image-kernels-p1-2026-07-21.json).
For representative 64^3 workloads on this Apple Silicon runner, 30
measurements across three forks gave:

| Path | Mean ms/op | `gc.alloc.rate.norm` | Collections |
| --- | ---: | ---: | ---: |
| K1 `pullScalarInto` | 3.245 | 54.258 B/op | 0 |
| K2 `composePullInto` | 8.831 | 54.242 B/op | 0 |
| K3 `regridPullInto` | 6.630 | 54.258 B/op | 0 |
| K4 Jacobian reduction | 2.163 | 53.350 B/op | 0 |
| K4 inverse-pair reduction | 17.770 | 54.217 B/op | 0 |
| K5 `pyramidLevelInto` | 13.063 | 51.117 B/op | 0 |
| K2 dynamic plan plus sample | 59.496 | 354,248,743 B/op | 55 |
| K2 static prepared sample | 10.276 | 53,647,813 B/op | 49 |

Every production path passes the provisional 64 B/op leaf-kernel allocation
gate and recorded no collections. K2 is 6.74x faster than the semantically
credible dynamic-map reference and 1.16x faster than the favorable static
prepared-plan reference. The latter still allocates approximately 53.6 MB per
composition. Production benchmarks batch 128 logical operations per JMH
invocation, except K5 which batches 256, so the profiler/invocation floor is
amortized; JMH scores remain per logical operation. The independent K2
thread-allocation diagnostic reports 24 B/op.

Setup is an admission gate, not part of the timed result. K1 is checked against
an analytic world-linear scalar field; K2 against the checked composition
route; K3 against an analytic physical affine across grids; K4 against a known
affine determinant and exact inverse translations; and K5 against a
mask-normalized constant-field oracle. These checks run in every fork before
timing. This completes the representative K1-K5 P1 leaf-kernel matrix. The
larger sizes and variants frozen above remain scale/stress experiments rather
than substitutes for this correctness gate.

### P2 Gale stress receipt

The P2 stress run is recorded in
[`receipts/half-flow-gale-p2-2026-07-21.json`](receipts/half-flow-gale-p2-2026-07-21.json).
It separates reusable linear-algebra improvements from registration-specific
code. Gale gained only the general `CgWorkspace` and `cgWith` destination-style
API for allocation-controlled matrix-free conjugate-gradient solves. The
periodic Helmholtz operator, HalfFlow fixture, and 3x3 alternatives remain
ScalaFIM benchmarks and tests; no registration stencil or HalfFlow-specific
solver was added to Gale.

The canonical single-thread command on the final immutable Gale revision was:

```sh
sbt 'galeBenchJVM/Jmh/clean' \
  'galeBenchJVM/Jmh/run -wi 5 -i 10 -f 3 -t 1 -w 250ms -r 250ms -prof gc .*HalfFlowGaleBenchmark.*'
```

Thirty measurements across three forks gave:

| Path | Mean ns/op | `gc.alloc.rate.norm` | Collections |
| --- | ---: | ---: | ---: |
| stock Gale CG | 923,358.306 | 2,623,288.995 B/op | 53 |
| reusable Gale `cgWith` | 894,155.172 | 56.013 B/op | 0 |
| Gale dense 3x3 Cholesky oracle | 86.188 | 552.002 B/op | 108 |
| Gale `Mat3` adjugate candidate | 3.705 | below profiler floor | 0 |
| fused scalar 3x3 Cholesky | 5.468 | below profiler floor | 0 |
| periodic 32^3 Helmholtz `applyTo` | 90,429.395 | 2.424 B/op | 0 |

The reusable Gale API is 3.16% faster in this diagnostic and reduces measured
allocation by approximately 46,834x. The result supports a reusable workspace
contract in Gale; it does not choose conjugate gradient as HalfFlow's final
production Helmholtz implementation.

For the pointwise LM solve, the fused scalar Cholesky path is selected despite
the adjugate candidate being 1.48x faster. Both agree on ordinary well-scaled
systems, but an ill-conditioned SPD adversary makes the adjugate's residual in
the original system more than 100x the Cholesky residual. Gale dense Cholesky
remains an independent small-system oracle, not the per-voxel hot path.

The Scala.js probe uses 32,768 systems and a 24^3 Helmholtz fixture. It reports
zero benchmark-owned scalar, vector, workspace, or scratch constructions in
every timed operation. The shared tests pass the original-system 3x3 residual,
pivot rejection, ill-conditioned adversary, analytic Fourier-operator, analytic
inverse, and workspace-overwrite contracts on both JVM and Scala.js.

The adopted Gale revision is
`d7e661a12033a6b0949a606f5ea73baf5a34574a`. An isolated pin switch first
rejected the workspace-only revision because it did not contain the previously
pinned constrained-optimizer branch. The adopted merge preserves that API and
the general workspace addition; `compileAll` and the complete `testAll` then
passed from the clean detached ScalaFIM baseline.

### P3 typed midpoint and paired-flow receipt

The P3 admission is recorded in
[`receipts/half-flow-flow-p3-2026-07-21.json`](receipts/half-flow-flow-p3-2026-07-21.json).
It adds the concrete `registration` cross-project: phantom-frame pull maps,
explicit inverse pairs, symmetric midpoint advancement, linear-only map
regridding, paired adaptive scaling-and-squaring, dense-morphism export, and
local plus accumulated topology/inverse guards.

The canonical 32^3 fixture is a smooth boundary-zero physical velocity. Its
adaptive depth is three, so every exponential performs six dynamic pull-map
compositions. Benchmark setup admits the result only after comparison with an
independent 4,096-step RK4 point-flow oracle and the complete Jacobian and
two-direction inverse guard. Shared tests additionally cover exact identity and
translation, convergence toward a closed-form linear-flow exponential, an
inverse-consistent reflection that must be rejected, accumulated-map rejection,
midpoint direction, runtime endpoint checks, and physical-coordinate regridding.
All nine tests pass on both JVM and Scala.js.

Thirty measurements across three single-threaded forks gave:

| Path | Mean ms/op | `gc.alloc.rate.norm` | Collections |
| --- | ---: | ---: | ---: |
| owning paired exponential | 5.179 | 3,281,626.859 B/op | 15 |
| reusable paired exponential | 5.213 | 891.565 B/op | 0 |
| owning topology and inverse guard | 4.525 | 1,025,370.017 B/op | 3 |
| reusable topology and inverse guard | 4.484 | 1,720.336 B/op | 0 |

The workspace path removes all per-call full-volume temporaries: it owns four
coordinate and four validity ping-pong buffers for paired flow, while the guard
owns four scalar and two validity buffers. Remaining measured allocation is
small result/report construction. This reduces allocation by approximately
3,681x for the flow and 596x for the guard without a material runtime penalty;
no collection occurred in either reusable path. Returned flow maps borrow their
workspace and are valid only until that workspace is reused.

The P3 receipt records the guard storage at that admission point. P5 later
shares the forward and backward determinant, validity, and quantile scratch
sequentially, reducing a guard workspace to two scalar and one validity buffer
without changing its report.

This optimization is registration-specific and remains in ScalaFIM. P3 found
no reusable Gale defect and made no further Gale change. The 64^3 and 128^3 K9
scale runs remain stress experiments; they do not replace this oracle-gated
32^3 admission.

### P4 masked T1 features, local LM, and trust receipt

The P4 admission is recorded in
[`receipts/half-flow-local-lm-p4-2026-07-22.json`](receipts/half-flow-local-lm-p4-2026-07-22.json).
It adds mask-normalized physical-window T1 channels, work-frame physical
gradients, robust symmetric residuals, rank-one and fused 3x3 LM steps, shaped
predicted decrease, and a trust state machine with exact rollback and separate
attempt, safe-attempt, accepted-step, and rejection counters.

Window radii are stated in millimetres and converted independently on each
voxel axis with `ceil(radiusMm / spacingMm)`. Boundary support is measured
against the nominal full window, so a cropped window does not silently pass as
fully observed. One source integral-image build serves every radius. Gradients
are transformed as physical covectors and become invalid unless the complete
central-difference stencil is valid.

The robust intensity scale initially used the standard-library dual-pivot sort.
JFR localized a full 32^3 temporary allocation to that sort. A small in-place
median selector removed the temporary and reduced the feature diagnostic from
2.982 to 1.432 ms/op and from 264,499 to 1,664 B/op. This was a ScalaFIM
feature-workspace defect; it did not justify a Gale change.

Thirty measurements across three single-threaded forks gave:

| Path | Mean ms/op | `gc.alloc.rate.norm` | Collections |
| --- | ---: | ---: | ---: |
| three-channel T1 features and gradients | 1.432 | 1,664.072 B/op | 0 |
| rank-one local LM | 0.384 | 899.957 B/op | 3 |
| three-channel fused 3x3 LM | 0.837 | 1,000.911 B/op | 0 |
| shaped-step predicted decrease | 0.249 | 86.664 B/op | 0 |

These public checked calls miss the provisional 64 B/op leaf target because
they construct small typed results, metadata vectors, and `Either` values. They
allocate no full-volume temporary: feature, gradient, velocity, validity,
integral, and residual scratch volumes are all caller-owned and reused. The
rank-one collections total three across all 30 measurement iterations and are
reported rather than hidden; allocation per operation remains below 1 KB.

Rank one is 2.18x faster on this fixture. It remains an explicit fast ablation,
not the default: the default is the three-channel 3x3 solve because it uses all
admitted correspondence channels. A synthetic-recovery and labeled-cohort
ablation must justify changing that accuracy-first choice.

Independent shared tests compare integral windows with a naive masked loop,
recover an analytic world covector on an oblique grid, verify positive
gain/offset invariance, check the rank-one closed form, evaluate the fused 3x3
step in its original damped system, recompute predicted decrease independently
at a shaped velocity, and prove trust rollback and counter partitioning. The
focused suites pass on both JVM and Scala.js.

### P5 Sobolev shaping and nonlinear-engine receipt

The P5 admission is recorded in
[`receipts/half-flow-engine-p5-2026-07-22.json`](receipts/half-flow-engine-p5-2026-07-22.json).
It completes the first supplied-affine nonlinear engine: physical
coarse-to-fine levels, current-support candidate evaluation, local LM,
Helmholtz/Sobolev shaping, opposite paired half flows, local and accumulated
topology guards, gain-ratio acceptance, exact rollback, typed diagnostics, and
fixed-to-moving plus moving-to-fixed export.

ScalaFIM owns the physical masked no-flux stencil. Gale remains general: P5
uses its reusable `CgWorkspace`, relative-residual convergence, initial-guess,
and preconditioner capabilities, but adds no registration operator to Gale.
Precomputing the operator's Jacobi diagonal and reusing the right-hand-side
view reduced the accuracy-first power-two diagnostic from the initial short
probe of roughly 27 ms/op to the admitted 21.395 ms/op. The production engine
uses the same checked Gale solve and requires every component and pass to meet
the declared relative tolerance.

Thirty measurements across three single-threaded forks gave:

| Path | Mean ms/op | p95 ms/op | `gc.alloc.rate.norm` | Collections |
| --- | ---: | ---: | ---: | ---: |
| power-two Sobolev shaping | 21.395 | 21.799 | 1,764.991 B/op | 0 |
| power-one Sobolev ablation | 11.056 | 11.250 | 1,507.158 B/op | 0 |
| complete accepted 32^3 nonlinear level | 65.831 | 66.704 | 24,483,760.333 B/op | 8 |

Power one is 1.94x faster for this smoother fixture, but power two remains the
accuracy-first geometry until recovery and cohort ablations justify changing
it. Neither leaf smoother allocates a full-volume temporary per call. The
approximately 24.5 MB full-level figure includes construction of the complete
single-level execution state and final dense inverse pair; it is not a leaf
allocation claim.

The full-level allocation short probe began near 36.2 MB/op. P5 reduced it by
about 32% through lifetime changes that also matter at realistic volumes:

- fixed and moving pyramid construction share one source-sized workspace;
- an unsmoothed shrink-one level aliases the original image;
- identity regridding is skipped;
- fixed and moving feature extraction share scratch sequentially;
- all local and accumulated guards share one capacity-sized scratch;
- rejected midpoint proposals reuse their four map destinations; and
- the Sobolev output overwrites the consumed raw LM step after an alias oracle.

The remaining full-level volume storage is explicit rather than hidden. The
largest next memory opportunity is a compact paired-feature representation
that stores one symmetric gradient instead of separate fixed and moving
gradient volumes; that change needs a speed and recovery experiment before it
can replace the simpler P5 representation.

Shared tests compare the Helmholtz operator with an independent masked stencil,
check its analytic discrete-Neumann frequency response, enforce mask and
physical strain/displacement caps, and prove aliased and disjoint destinations
agree. End-to-end tests cover stationary identity, two-level sub-voxel
recovery with objective decrease and a safe accumulated transform, fixed
candidate support, and impossible-gain rejection with bit-exact state
rollback. All 32 registration tests pass on both JVM and Scala.js.

Unrelated development servers were present during the laptop run. The tight
30-sample intervals are retained, but this receipt is a local engineering
admission, not the ANTs runtime comparison. The Docker-only ANTs comparison
remains blocked until Docker Desktop is responsive and must run serially in a
separate work directory.

### P6 robust affine initialization receipt

The P6 admission is recorded in
[`receipts/half-flow-affine-p6-2026-07-22.json`](receipts/half-flow-affine-p6-2026-07-22.json).
It adds a typed supplied-affine boundary and a deterministic estimator with a
center-of-mass seed, physical rigid then affine coordinate search, clipped
gain/offset-invariant correlation, overlap admission, and two physical pyramid
levels. Every score is bidirectional. Independently estimated fixed-to-moving
and moving-to-fixed candidates are reconciled by their affine-group midpoint,
then split into opposite inverse half maps before nonlinear refinement.

This structure matters more than another inverse-consistency penalty. Reversing
the image order exchanges the two directional fits; the reconciled transform
becomes the numerical inverse and the midpoint arms exchange. Shared tests
enforce that property on both the supplied and estimated paths, and compare the
estimated transform with independently generated analytic world-coordinate
images at four landmarks. The accuracy-first native-grid refinement stays under
the frozen 0.8 mm landmark RMS gate; stopping at shrink two missed that gate at
about 1.0 mm and was rejected despite being faster.

Thirty measurements across three single-threaded forks on the 32^3 initializer
fixture gave:

| Path | Mean ms/op | p95 ms/op | `gc.alloc.rate.norm` | Collections |
| --- | ---: | ---: | ---: | ---: |
| complete robust affine initialization | 116.516 | 119.037 | 9,660,218.756 B/op | 3 |

The measured operation includes pyramid construction, robust normalization,
both directional searches, affine reconciliation and square root, and all four
dense midpoint maps. It is an initialization-stage measurement, not a leaf
allocation claim. Most owned bytes are image pyramids, score samples, and the
final inverse-tracked dense maps; no optimizer-state volume or automatic
differentiation graph is created.

P6 found no Gale defect worth an upstream change. The hot work is registration
sampling and objective evaluation; Gale remains a general numerical library.
The small 4x4 affine control algebra runs once per initialization and did not
justify adding a registration-shaped primitive to Gale. The image pyramid
kernel is reused directly. A general sparse world-point sampling kernel remains
a plausible image-module performance experiment for P7, but P6 does not add a
registration-only sampler to the image API.

The four new shared tests cover supplied half splitting, analytic affine
recovery, exact swap/inverse equivariance, and typed failure on degenerate
intensity support. The complete registration module now passes 32 tests on the
JVM and 32 on Scala.js. This receipt is still a local engineering admission;
the ANTs comparison is unchanged and remains blocked on the unresponsive Docker
Desktop daemon.

### P1 composition diagnostic

P1 also retains a deliberately labeled diagnostic probe for the dynamic
pull-map composition that scaling-and-squaring repeats:

```sh
sbt 'imageJVM/Test/runMain scalafim.image.HalfFlowImageKernelProbe 64 6 11'
```

It reports two references. `referencePlanAndSample` rebuilds interpolation
stencils because the left map changes between squarings. The intentionally
favorable `referencePreparedSample` reuses static stencils and is retained as a
lower bound rather than hidden. Both are compared with a caller-owned
`primitiveComposeInto` destination and a reusable `DenseFieldSampler`.

This probe uses `System.nanoTime` and thread-allocation counters. It is useful
for local optimization and allocation regressions, but it is not the JMH
admission receipt and must not be presented as one.

## Scala.js measurement

Scala.js uses a fully linked deterministic benchmark under Node rather than
claiming JVM allocation measurements:

```sh
sbt 'registrationJS/Test/fullLinkJS'
node <full-link-output>/main.js
```

The final executable path may differ with the linker output and is recorded in
the receipt. The benchmark reports:

- warmup and measured repetitions;
- median, p95, minimum, and maximum milliseconds;
- owned full-volume constructions by scalar, vector, mask, and scratch role;
- kernel work counts;
- valid count and deterministic checksum;
- Node/V8 and operating-system identity; and
- linked artifact hash.

Construction counters are explicit test instrumentation at allocation-owning
boundaries. A JavaScript heap snapshot is diagnostic, not the primary portable
gate.

The P1 Scala.js diagnostic can run from test sources without changing the
shared build definition:

```sh
sbt \
  'set imageJS / Test / scalaJSUseTestModuleInitializer := false' \
  'set imageJS / Test / scalaJSUseMainModuleInitializer := true' \
  'imageJS / Test / run'
```

It reports timing and exact output parity for the dynamic, prepared, and
primitive composition paths, plus K1, K3, both K4 reductions, and K5. Explicit
allocation-boundary counters cover scalar, vector, mask, and scratch
full-volume roles. The admitted run reports zero benchmark-owned full-volume
constructions inside every timed production operation; setup-owned sources,
destinations, samplers, reductions, and pyramid workspaces are excluded and
counted before timing.

## End-to-end workloads

### Synthetic ladder

Run identity, translation, affine-only, smooth fixture flow, large smooth flow,
partial FOV, mask disagreement, and deliberately unsafe velocity cases. Report
recovery error because the truth is known.

### Public labeled T1 cohorts

Use at least:

- one OASIS-style labeled cohort;
- LPBA40 or a comparable landmark/label cohort;
- a LUMIR-style set where licensing and preprocessing are reproducible; and
- a scanner/site-shift cohort not used to choose parameters.

Labels and landmarks are evaluation-only. Dataset version, subject exclusions,
split, preprocessing commands, and file checksums are frozen before the final
comparison.

For every pair report:

- Dice and surface Dice by label plus macro summaries;
- landmark TRE when available;
- determinant distribution and non-positive fold count;
- both inverse-composition errors;
- valid coverage;
- accepted/rejected steps and terminal reason;
- wall time and peak RSS; and
- per-phase and per-kernel profiles.

Retain pair-level arrays. Aggregate means alone are not sufficient evidence.

## ANTs comparator profiles

ANTs is Docker-only on this laptop. Neither `antsRegistration` nor
`antsApplyTransforms` is installed natively. The Docker context and socket are:

```text
context: desktop-linux
socket:  /Users/bbuchsbaum/.docker/run/docker.sock
host:    Apple Silicon
image:   amd64 under emulation
```

### Docker preflight

Before any ANTs command:

```sh
docker context use desktop-linux
docker info
docker run --rm --platform linux/amd64 \
  antsx/ants:latest antsRegistration --version
```

`docker info` MUST succeed before a benchmark starts. A socket existing at the
expected path is not proof that the daemon is responsive. At the P0 correction
point the context and socket were correct, but the daemon was unresponsive.
Docker Desktop must be started or restarted by the user before continuing; an
agent MUST NOT launch a comparison against that state.

The amd64 emulation is part of the runtime environment and MUST be recorded.
Native and emulated timings are different comparator classes.

### Credible end-to-end profile

The established sub-18 baseline is owned by the Hodgeflow benchmark harness:

```text
/Users/bbuchsbaum/code/hodgeflow/inst/benchmarks/
  benchmark_sub18_synquick_fixedmask.R
```

It is a deterministic SyNQuick-style rigid -> affine -> SyN pipeline with a
fixed/template mask during registration and the moving BET mask reserved for
evaluation. It is not a supplied-affine nonlinear-only comparison.

From the Hodgeflow repository root, set:

```sh
export HF_ANTS_DOCKER_IMAGE=antsx/ants:latest
export HF_ANTS_RANDOM_SEED=1
export HF_ANTS_THREADS=1
export HF_ANTS_PROFILE=synquick_fixedmask
export HF_SUB18_SYNQUICK_OUT_DIR=tmp/sub18_hodgeflow_vs_ants/halfflow-agent-unique

Rscript inst/benchmarks/benchmark_sub18_synquick_fixedmask.R
```

The output directory MUST be unique to the agent/run. The harness localizes
inputs into that work directory and mounts it at `/work` inside the container.
Agents MUST run ANTs comparisons serially on this laptop. Do not overlap ANTs,
HalfFlow, Hodgeflow, or other CPU-heavy registration benchmarks; contention
invalidates runtime comparisons.

The profile settings above are part of the comparator identity. The harness's
resolved ANTs command, fixed and moving images, fixed registration mask, moving
evaluation mask, random seed, threads, metrics, outputs, and work directory are
retained in the receipt.

### Immutable Docker identity

`antsx/ants:latest` is only a retrieval alias and can drift. Once Docker is
responsive, record both the local image ID and every repository digest before
the first comparison:

```sh
docker image inspect antsx/ants:latest --format '{{.Id}}'
docker image inspect antsx/ants:latest --format '{{json .RepoDigests}}'
```

The materialized comparator id is:

```text
hodgeflow-synquick-fixedmask-docker-v1@<repo-digest-or-image-id>
```

All later runs in the same comparison series SHOULD use the digest-qualified
image reference rather than `latest`. A changed digest creates a new comparator
series even when `antsRegistration --version` is unchanged.

### Materialized sub-18 comparator — 2026-07-22

P7 recovered the previously unresponsive Docker daemon and materialized the
first fresh comparator in this series:

```text
ANTs:        2.6.5.dev1-gfdce4d2 (compiled 2026-01-14)
image ID:    sha256:ac096b2f75f67866feb6606fde2a549d395675f83baec9fd17610c9a7053fa18
repo digest: antsx/ants@sha256:59c45f54a1f1dc69134f63bec91a726e41c71c64a16cc21cda0b54526910a3c3
platform:    linux/amd64 under Apple Silicon emulation
profile:     synquick_fixedmask, seed 1, one thread
```

The isolated ANTs-only sub-18 run completed successfully:

| Metric | Before | After |
| --- | ---: | ---: |
| mask Dice | 0.660948 | 0.979359 |
| global NCC | 0.567851 | 0.945710 |
| local NCC | 0.126293 | 0.672298 |
| gradient NCC | -0.107224 | 0.439946 |
| centre-of-mass distance (fixed-grid voxels) | 17.720558 | 0.308368 |

The measured ANTs registration time was 49.915 seconds. Fifteen unrelated
Docker services were present but idle at admission; no other ANTs, Hodgeflow,
HalfFlow, or registration benchmark ran concurrently. Treat this as the
emulated-laptop comparator class, not as a native-ANTs timing.

The default paired `hodgeflow,ants` harness run exposed an external reporting
defect after registration: method-specific rows have different column sets and
base `rbind` rejects them. The ANTs-only rerun avoids that reporting defect
without changing inputs, command profile, seed, or thread count. The frozen
receipt records both the successful result and failed paired-run provenance:

```text
docs/benchmarks/receipts/half-flow-ants-sub18-p7-2026-07-22.json
```

The next P7 admission is a ScalaFIM run on these exact input hashes. It must
report the same intensity/mask metrics plus Jacobian quantiles, fold fraction,
inverse-composition error, accepted/rejected steps, peak RSS, and phase timing.

### First ScalaFIM sub-18 diagnostic — 2026-07-22

The JVM evaluation runner consumed the exact four input hashes in the ANTs
receipt. The fixed mask was registration support; the moving BET mask remained
held out until nearest-neighbour output scoring. Moving intensity was
pre-resampled once from its 1 mm grid to the fixed 2 mm grid, while every
accepted candidate was sampled from that level's original source rather than
from a recursively warped image.

The first attempt found a real guard-model defect before objective evaluation:
one `minimumValidFraction` threshold was being used for both Jacobian support
and inverse-composition overlap. A translated map can have complete Jacobian
support while its inverse composition is evaluable on only the overlapping
rectangular field of view. `GuardConfig` now represents those thresholds
separately, with a JVM/Scala.js contract covering the distinction.

With 0.60 minimum map support and inverse overlap, HalfFlow accepted 22 of 69
attempts. The independent Hodgeflow evaluator produced:

| Metric | Header only | ScalaFIM affine | HalfFlow-LM | ANTs SyNQuick |
| --- | ---: | ---: | ---: | ---: |
| mask Dice | 0.660948 | 0.931893 | 0.932542 | 0.979359 |
| global NCC | 0.567852 | 0.911354 | 0.917451 | 0.945710 |
| local NCC | 0.126293 | 0.485883 | 0.540958 | 0.672298 |
| gradient NCC | -0.107225 | 0.245591 | 0.275251 | 0.439946 |
| centre-of-mass distance (fixed-grid voxels) | 17.720558 | 1.253336 | 1.180702 | 0.308368 |

The earlier `mm` label for the external centre-of-mass result was incorrect:
the frozen Hodgeflow evaluator computes Euclidean distance in array-index
coordinates without multiplying by spacing. ScalaFIM's physical-coordinate
diagnostic is 35.4411 -> 2.3614 mm on this 2 mm grid.

HalfFlow's end-to-end JVM time was 25.206 seconds: 4.883 read, 0.668 initial
resample, 0.907 affine, 18.211 nonlinear, and 0.485 output seconds. The external
ANTs elapsed time was 49.915 seconds, so this single diagnostic is 1.98x faster
under the recorded but not perfectly identical host/container accounting. Peak
JVM resident set size was 2.664 GB.

This run does **not** pass P7 admission. Although both exported maps had zero
non-positive Jacobians (minimum determinants 0.429 and 0.725), the final
forward-backward composition error was 0.557 mm / 0.279 fixed-grid voxels,
above the configured 0.50 mm / 0.25 voxel limit. Accuracy also remains below
ANTs: Dice by 0.0468, global NCC by 0.0283, local NCC by 0.1313, and gradient
NCC by 0.1647. Most of the current accuracy comes from the fast affine stage;
the nonlinear contribution is real but small.

The next work is therefore not blind iteration-count tuning. First, make dense
self-map composition use an explicit and tested boundary-extension contract so
that legitimate FOV loss does not dominate accumulated-map rejection or erode
inverse consistency. Then rerun this exact case and inspect why the shrink-8
level accepts no step. Only after that should feature windows or the coarse FFT
proposal be tuned. No HalfFlow-shaped primitive belongs in Gale; any Gale
change must remain a generally useful solver or workspace improvement.

The immutable diagnostic receipt is:

```text
docs/benchmarks/receipts/half-flow-sub18-p7-2026-07-22.json
```

### Boundary and support ablation — 2026-07-22

The first P7 diagnosis was tested directly rather than accepted from source
inspection. Dense self-flow composition now has an explicit opt-in identity
extension. Ordinary coordinate-map composition remains strict: an in-grid
invalid sample or an out-of-grid query is still invalid unless the caller
declares that the right-hand map is an identity-extended self-map. Paired
scaling-and-squaring and the two midpoint inverse-arm compositions make that
declaration; image pulling and endpoint-map composition do not.

Three shared regression contracts cover the defect on both JVM and Scala.js:

1. strict composition still rejects an out-of-grid query;
2. an identity-extended self-map preserves the query and validity; and
3. exact-identity and microscopic midpoint updates remain guard-safe after an
   affine initialization whose inverse arms leave the finite work grid.

The proposed full-grid Sobolev change was also implemented and measured. It
was rejected rather than retained: treating the local-model mask only as a
force mask made the current fixed-support trust objective discontinuous at the
mask boundary, added many non-finite candidate evaluations, reduced local NCC,
and increased runtime. A zero outer-boundary variant removed the gross map
excursion but still underperformed the compact masked solve. No Gale change
was indicated; Gale's reusable CG contract converged as requested in every
variant.

The unassisted shrink-8 stage was independently harmful. Once the composition
bug no longer prevented it from moving, it reduced its own coarse objective
from 1.384984 to 1.302223, but left the shrink-4 stage nearly stationary and
reduced final local NCC to 0.488248. Until a large-displacement proposal is
admitted, the compact evaluation schedule therefore starts at shrink 4.

| Variant | Accepted | Dice | Global NCC | Local NCC | Gradient NCC | Nonlinear s | End-to-end s |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| first P7 diagnostic | 22 | 0.932542 | 0.917451 | 0.540958 | 0.275251 | 18.211 | 25.206 |
| full-grid, no-flux boundary | 3 | 0.925776 | 0.905132 | 0.482078 | not scored | 19.651 | 26.749 |
| full-grid, zero boundary | 25 | 0.932159 | 0.912180 | 0.492201 | 0.248142 | 28.387 | 37.420 |
| masked solve with shrink 8 | 17 | 0.931884 | 0.911662 | 0.488248 | not scored | 17.454 | 24.701 |
| retained masked solve, shrink 4/2/1 | 22 | 0.932466 | 0.917513 | 0.541874 | 0.279891 | 20.355 | 27.852 |

The retained result is a small accuracy improvement over the first diagnostic
for global, local, and gradient NCC, with essentially unchanged Dice. Timing
varied enough across these single runs that no speed improvement is claimed.
It still fails P7 admission: the final fixed-to-moving dense composition has
zero folds but a 0.557 mm / 0.279 fixed-grid-voxel forward-inverse error, and
its backward Jacobian coverage is below the stricter 0.80 threshold.

This locates the remaining representation problem more narrowly. Identity
extension is correct for incremental work-space self-flows, but it cannot
correctly extrapolate a work-to-endpoint map that already contains an affine
half-transform. The next fix must preserve the affine component analytically
or define and validate a general dense-map displacement extrapolation policy;
loosening the guard or adding HalfFlow-specific Gale code would only conceal
the failure.

The ablation receipt is:

```text
docs/benchmarks/receipts/half-flow-boundary-ablation-p7-2026-07-22.json
```

### Exact-affine midpoint factorization — 2026-07-22

The boundary ablation identified a representation error rather than a failure
of midpoint optimization.  Each midpoint arm had stored the affine half-map as
a sampled work-to-endpoint field.  Identity extension is mathematically valid
for a residual self-map, but not for that sampled affine map: outside the work
grid it silently discarded the affine component.

The retained state now factors each arm into exactly two pieces:

```text
work residual self-map >>> exact affine endpoint map
```

Only the work-space residual is regridded and identity-extended.  Affines are
reframed analytically, image pulls fuse the affine with the source sampler, and
the exported fixed-to-moving map first composes the relative residual on the
work grid and samples it once before applying the endpoint affine.  The hot
path therefore owns less dense state and does not interpolate an affine field.

Candidate acceptance guards the relative residual that becomes the returned
transform.  Its Jacobian floor is divided by the exact affine determinant, and
its inverse-error allowance is divided by a conservative induced-norm bound on
the affine in world and voxel coordinates.  This accounts for affine
amplification without a general SVD, an eigensolver, or a HalfFlow-specific
Gale operation.  Reusable composition scratch removes the associated
full-volume allocation.  Regrid transitions still guard both residual arms and
their relative composition.

The successive frozen sub-18 diagnostics were:

| Variant | Accepted | Dice | Global NCC | Local NCC | Gradient NCC | Nonlinear s | End-to-end s | Forward inverse mm | Safe |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | :---: |
| factored arms, arm guards only | 11/51 | 0.932490 | 0.914900 | 0.521563 | not scored | 12.846 | 20.777 | 0.812 | no |
| relative-result guard, owning scratch | 24/61 | 0.932730 | 0.919657 | 0.563346 | 0.294601 | 21.509 | 28.877 | 0.516 | no |
| relative-result guard, reused scratch | 24/61 | 0.932730 | 0.919657 | 0.563346 | 0.294601 | 15.808 | 23.195 | 0.516 | no |
| retained affine-scaled guard | 24/58 | 0.932590 | 0.917763 | 0.545256 | 0.280816 | 16.476 | 23.943 | 0.404 | yes |

The first variant proves that checking two individually plausible midpoint arms
is insufficient: the returned relative map is the contract.  Reusing its
composition scratch reproduced the owning result exactly while removing 5.70
seconds from this single nonlinear run.  The retained guard is deliberately
more conservative; it gives up the unsafe variant's small score gain and
returns zero folds, minimum Jacobians 0.402 and 0.744, and two-direction maximum
inverse errors 0.404 and 0.368 mm.

The public engine evaluates the exported dense pair once, refuses an unsafe
result, and carries the verified report in `RegistrationResult`; the evaluation
CLI reuses that report instead of repeating the full-volume guard.

Against the preceding retained HalfFlow diagnostic, nonlinear time fell 19.1%
and end-to-end time fell 14.0%, while Dice and all three correlation scores were
essentially unchanged or slightly improved.  Against the frozen containerized
ANTs run, this single run is 2.08x faster end to end, but its Dice (0.932590
versus 0.979359), local NCC (0.545256 versus 0.672298), and gradient NCC
(0.280816 versus 0.439946) remain materially worse.  The factorization fixes
support, inverse consistency, and avoidable work; it does not close the
correspondence/accuracy gap.

The immutable receipt is:

```text
docs/benchmarks/receipts/half-flow-factored-affine-p7-2026-07-22.json
```

### Controlled nonlinear profile

The algorithm-isolation comparison remains separate: nonlinear-only SyN and
HalfFlow receive the same supplied affine and preprocessed images. Before that
profile is admitted, its affine adapter must pass:

1. known physical landmarks transformed by ScalaFIM and containerized ANTs;
2. an analytic world-linear image resampled by both tools away from boundaries;
   and
3. explicit ScalaFIM world-coordinate to ANTs/ITK LPS direction checks.

Do not report the Hodgeflow `synquick_fixedmask` result as this controlled
profile: it estimates its own rigid and affine stages. Conversely, the
controlled profile does not replace the credible end-to-end baseline.

For both profiles, measure peak RSS and wall time under an explicit container
and host accounting policy. Record whether image pull/startup, file IO, and
initialization are included. HalfFlow and ANTs receive the same cold/warm cache
and inclusion policy within a comparison class.

## Statistical decision

Freeze the primary accuracy metric, noninferiority margin, and resampling unit
before inspecting the final held-out cohort.

Use paired subject/pair differences and a 95% confidence interval that respects
the cohort's dependence structure. Registration accuracy is admitted when the
lower confidence bound is above the negative noninferiority margin. Runtime is
claimed faster only when the confidence interval for paired runtime improvement
excludes parity under the same thread and IO policy.

The initial stretch target is at least 2x median end-to-end speed and at most
75% of peak memory while meeting accuracy and topology admission. Missing the
stretch target does not falsify the algorithm, but it forbids that performance
claim.

## Required ablations

On the same frozen pair set, remove or replace one component at a time:

- rank-one versus multi-channel 3x3 local model;
- midpoint opposite-half updates versus one-direction refinement;
- warp-then-normalize versus normalize-then-warp features;
- Sobolev production path versus Gale reference on feasible sizes;
- adaptive versus fixed squaring depth;
- accumulated-map guard versus local-only diagnostics; and
- FFT proposal or learned initialization, if either is later admitted.

Report accuracy, folds, inverse error, accepted attempts, runtime, and peak
memory for every ablation. A component remains only if it earns its complexity.

## Receipt schemas

Kernel receipt id: `scalafim-half-flow-kernel-benchmark-v1`.

End-to-end receipt id: `scalafim-half-flow-registration-benchmark-v1`.

Each receipt contains raw observations plus summary values and, at minimum:

```text
schema
timestamp and duration
repository and Gale revisions
dirty-path/source hashes
toolchain and hardware
dataset/fixture checksums
algorithm/comparator id
complete serialized plan
thread and cache policy
correctness and topology admission
work/allocation counters
timing and peak memory
raw per-repetition or per-pair arrays
terminal status
```

Receipts are append-only evidence. A summary document links them; it does not
replace them.

## CI and scheduled gates

### Every pull request

- fixture generator `--check`;
- fast shared JVM/JS contract and metamorphic tests;
- adversarial degenerate cases;
- deterministic checksums; and
- structural allocation counters where stable.

### Nightly or controlled runner

- JMH leaf kernels with GC profiler;
- Scala.js full-link benchmarks;
- grid/squaring refinement ladders;
- fuzzed valid fields and near-guard fields; and
- performance comparison with the previous admitted receipt.

### Release candidate

- complete labeled cohorts;
- frozen ANTs comparator;
- all ablations;
- exact environment and raw arrays; and
- independent replay from a clean checkout.

No CI failure is based on a raw wall-clock number from an uncontrolled runner.
Stable work counts, construction counts, checksums, numerical bounds, and
controlled-runner relative comparisons are the gates.
