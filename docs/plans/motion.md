# Motion Module - `scalafim-fmri-motion`

Retrospective fMRI motion correction in ScalaFIM. This module ports the useful
core of `~/code/volregger` into a typed, cross-compiled Scala 3 API without
copying the R S3 surface, list-shaped controls, CLI, or Rcpp boundary.

The durable computation is:

```text
4D run + optional mask + motion plan
  -> rigid motion estimate + frame diagnostics
  -> optional corrected run
  -> motion/intensity QC
```

`volregger` is a good reference for algorithms, test cases, and numerical
semantics. It is not the target API.

Current status: the shared module now has the core ADTs, motion metrics, QC,
one-pass rigid application, typed controls/profiles, small inline fixture
oracles, and a portable baseline `RigidRobust` estimator with optional pyramid
levels, rotational capture seeds, robust/valid-frame template refresh, and
thresholded low-motion pose shrink plus opt-in temporal pose regularization and
frame-mean nuisance residual removal. Implemented profile components are
separated from planned components. Spline acquisition timing, JVM IO,
CLI/reporting, and larger external parity fixtures remain later layers.

## Mote Completion Plan

The live tracker home for completing the useful `volregger` port as idiomatic
Oderskyan Scala 3 is:

- `bd-01KWX6PCMAKNTGYQQV1R2WVVZX` - `EPIC: Complete volregger motion port as
  idiomatic Scala 3`.

The dependency order is:

1. Ready now:
   - `bd-01KWX6PQBRFA6DG6AH15PG9Q8Z` - motion parity matrix and `volregger`
     fixture generator.
   - `bd-01KWX6Q1AMRZ7358V932QBZWC6` - public API audit for Oderskyan Scala 3
     domain shape.
2. Blocked on both ready items:
   - `bd-01KWX6QE4NYEWX7CESR04WFYSB` - spline and acquisition timing core.
   - `bd-01KWX6QSTFHZ99M93YR9V7JTGC` - IC stencil and whitening policies.
   - `bd-01KWX6R3MTXPWA3FWRGYEB810M` - deterministic parallel frame execution.
3. Blocked on the API audit and spline/timing:
   - `bd-01KWX6RF9Z5K67HMPX0N3MG898` - JVM IO, BIDS, CLI, and report adapters.
4. Blocked on the shared feature streams and JVM adapters:
   - `bd-01KWX6RR3PQYN1H48791VYD161` - real-data differential benchmark
     harness.
5. Final closeout:
   - `bd-01KWX6S0NG16V6Y8HC387CJSY3` - API and verification audit. The epic is
     blocked on this gate.

## Home

Create a new cross-compiled module:

```text
modules/motion
name := "scalafim-fmri-motion"
package scalafim.fmri.motion
dependsOn(image, linalg)
```

This should not live in `image`: image owns generic volumes, spaces, affine
morphisms, and resampling primitives. Motion correction owns temporal fMRI
assumptions, reference selection, optimizer policy, acquisition timing, frame
diagnostics, and censor hints.

This should not live in `fit`: first-level fitting estimates statistical model
parameters from already prepared response blocks. Motion correction is spatial
preprocessing over a 4D run.

The shared module must not depend on `bids`, `dataset`, `model`, `fit`, JVM
image IO, plotting, or process execution. Later JVM adapters can sit on top.

## Design Stance

The public API should separate pure descriptions from execution:

```scala
val plan =
  MotionPlan(
    reference = ReferenceStrategy.Middle,
    engine = MotionEngine.RigidRobust,
    control = MotionControl.fastFmri
  )

val estimate =
  MotionEstimator.estimate(run, mask = Some(mask), plan)

val corrected =
  estimate.flatMap(est => MotionApplier.apply(run, est.trace, ApplyControl.linear))
```

The core rules:

- public construction returns `Either[MotionError, T]` where bad user input is
  ordinary;
- once built, values are valid by construction;
- hot kernels use primitive arrays and `while` loops;
- JVM/Scala.js parity is required for every shared feature;
- no hidden repeated resampling in the core pipeline;
- no file IO, report writing, plotting, or CLI behavior in shared code.

## Core Types

| Type | Role |
|---|---|
| `FrameIndex` | checked zero-based frame index. |
| `Millimeters`, `Radians`, `Seconds` | opaque checked scalars for public units. |
| `HeadRadius` | positive radius for FD/displacement summaries. |
| `RigidPose` | six-DOF pose: translations in mm, rotations in radians. |
| `RigidTransform` | pose plus 4x4 homogeneous matrix conversion through `DMat` and image-space affine morphisms. |
| `MotionTrace` | non-empty frame-indexed sequence of `RigidPose`, length-checked against a run. |
| `ReferenceStrategy` | `Middle`, `RobustMean`, or `Frame(FrameIndex)`. |
| `MotionEngine` | `RigidRobust` first; `RigidSpline` later. |
| `Interpolation` | `Linear` first; high-order modes only after generic sampler policy is settled. |
| `PadMode` | `Clamp` or `Zero`; `zpad` as explicit apply-time control. |
| `SliceTiming`, `PacketTiming` | validated acquisition timing for later packet-aware application. |
| `PyramidControl`, `OptimizerControl`, `CaptureControl`, `TemplateControl`, `TemporalControl`, `ExecutionControl` | typed control groups replacing one large R list. |
| `MotionControl` | product of control groups plus smart defaults. |
| `MotionProfile` | named constructors such as `denseBaseline`, `fastNative`, `fastFmri`, and later `sliceSpline`. |
| `ApplyControl` | final resampling policy: interpolation, padding, `zpad`, and packet-aware apply settings. |
| `MotionPlan` | pure estimate/apply/QC description. |
| `FrameFitDiagnostics` | per-frame initial/final cost, iterations, overlap, restart flag, convergence flag. |
| `MotionEstimate` | trace, frame diagnostics, and control used. |
| `MotionQc` | FD, DVARS, robust DVARS, cost drop, motion spikes, fit failures, censor suggestions. |
| `MotionCorrectionResult` | estimate plus optional corrected run and QC. |
| `MotionError` | shape mismatch, empty trace, invalid scalar, bad mask, singular transform, unsupported interpolation, timing mismatch, non-convergence. |

## Package Layout

Start small and keep files aligned with the unit under test:

```text
modules/motion/shared/src/main/scala/scalafim/fmri/motion/
  Units.scala
  MotionError.scala
  RigidPose.scala
  MotionTrace.scala
  MotionMetrics.scala
  MotionQc.scala
  MotionControl.scala
  MotionProfile.scala
  MotionPlan.scala
  ApplyControl.scala
  MotionSampling.scala
  MotionEstimate.scala
  MotionApplier.scala
  MotionEstimator.scala

modules/motion/shared/src/test/scala/scalafim/fmri/motion/
  RigidPoseSuite.scala
  MotionTraceSuite.scala
  MotionMetricsSuite.scala
  MotionQcSuite.scala
  MotionControlSuite.scala
  MotionApplierSuite.scala
  MotionEstimatorSuite.scala
  fixtures/VolreggerFixtures.scala
```

JVM-only adapters, when needed:

```text
modules/motion/jvm/src/main/scala/scalafim/fmri/motion/io/
  MotionNiftiIo.scala
  MotionReportWriter.scala
```

No JVM files are needed for the first shared slice.

## First Implementation Slice

Historical goal: establish a useful module that compiles and tests on both
platforms before the rigid estimator slice.

1. Add module wiring to `build.sbt`, root aggregates, `compileAll`, and
   `testAll`; add `modules/motion/README.md`.
2. Add core ADTs and checked values: units, `FrameIndex`, `HeadRadius`,
   `MotionError`, `ReferenceStrategy`, `MotionEngine`, `Interpolation`,
   `PadMode`.
3. Implement `RigidPose` and `MotionTrace`.
   - pose -> 4x4 matrix using the same ZYX rotation convention as `volregger`;
   - 4x4 matrix -> pose for finite rigid matrices;
   - transform inversion and composition;
   - trace length validation.
4. Implement cheap metrics.
   - `framewiseDisplacement(trace, radius)`;
   - `dvars(run, mask, robust)`;
   - transform displacement at the radius-50 cube corners;
   - masked displacement summaries.
5. Implement `MotionQc.from(...)` from trace, optional corrected run, mask, and
   frame costs.
6. Implement `MotionApplier.apply` for a `NeuroVec[Double]` and `MotionTrace`
   using linear interpolation, clamp/zero padding, and one final resampling
   pass.
7. Add fixture-backed tests generated from `volregger` for pose matrices, FD,
   DVARS, and displacement summaries.

Acceptance for this slice:

- `sbt motionJVM/test` and `sbt motionJS/test` pass;
- `sbt testAll` remains green;
- shared code has no JVM-only dependencies;
- invalid dimensions, masks, traces, and settings return `MotionError`;
- all floating-point comparisons use explicit tolerances;
- the apply path performs exactly one resampling pass.

## Second Slice: Controls And Profiles

Port the control surface before porting the estimator. This prevents the
estimator from inheriting a large untyped option bag.

Control groups:

- `PyramidControl`: downsample schedule, level sample counts, max iterations.
- `OptimizerControl`: Huber threshold, damping, step/cost tolerances, SE(3)
  update policy.
- `TemplateControl`: robust template, valid-frame-only refresh, edge exclusion.
- `CaptureControl`: capture boost, translation/rotation search half-width,
  top-k seed count.
- `TemporalControl`: warm-start policy, temporal regularization, low-motion
  pose smoothing/shrink controls.
- `ExecutionControl`: deterministic/parallel frame execution, thread request.

Profiles should be named typed constructors, not strings:

```scala
enum MotionProfile:
  case DenseBaseline
  case FastNative
  case FastFmri
  case IcStencil
  case IcWhiten
  case SliceSpline
```

`MotionProfile.control` should return a complete `MotionControl` plus a compact
set of implemented component tags. Planned but unsupported components should
remain inspectable as planned metadata, not active behavior. If user overrides
are supported, expose conflict diagnostics as values rather than warnings.

Acceptance:

- every control smart constructor has edge-case tests;
- profile defaults match documented `volregger` behavior where intentionally
  preserved;
- active versus planned profile capabilities are deterministic and testable;
- estimator code can consume `MotionControl` without checking strings or
  missing fields.

## Third Slice: Rigid Robust Estimator

The baseline portable estimator is implemented in shared code. It deliberately
keeps the first surface smaller than `volregger`: deterministic dense samples,
translation capture, finite-difference damped Gauss-Newton updates, Huber loss,
outward reference traversal, rotational capture seeds, robust/valid-frame
template refresh, optional pyramid levels, thresholded low-motion pose shrink,
opt-in temporal pose regularization, frame-mean nuisance residual removal,
explicit unsupported-control errors, and per-frame diagnostics.
Whitening beyond nuisance mean removal, parallel frame execution, and large
real-data benchmarks remain follow-on work.

Estimator phases:

1. Build or validate mask.
2. Choose reference from `ReferenceStrategy`.
3. Build a template: reference frame or mean template.
4. Generate deterministic dense sample points.
5. Fit each frame with trilinear sampling and a small LM/Gauss-Newton solver,
   optionally using coarse-to-fine pyramid sample levels.
6. Traverse outward from reference with warm starts.
7. Use translation and rotation capture to recover simple off-center starts.
8. Refresh templates from aligned valid frames and reject high-cost outlier
   frames for robust templates.
9. Optionally shrink subthreshold low-motion poses toward identity after fitting.
10. Optionally smooth pose traces with deterministic temporal regularization.
11. Optionally remove frame-mean residuals as an intensity-offset nuisance term.
12. Add whitening residual options only after broader recovery tests
   pass.

The first estimator does not need every `volregger` feature. It needs a stable
typed surface, deterministic behavior, and honest diagnostics.

Acceptance:

- identity run estimates near-zero poses;
- known synthetic translations and rotations improve the objective and recover
  parameters within stated tolerances;
- costs, overlaps, iterations, and convergence flags are finite and
  frame-indexed;
- reference traversal order is tested;
- rotational capture is tested independently from optimizer iterations;
- robust template refresh is tested against a high-cost outlier frame;
- enabled pyramid schedules refine on their finest level;
- low-motion pose shrink is thresholded and leaves larger motion estimates
  unchanged;
- temporal regularization smooths isolated poses while preserving the reference;
- frame-mean nuisance residual mode removes global intensity offsets without
  inventing motion;
- no per-frame public API loops are required by users;
- both platforms pass the same estimator contract tests, even if large
  performance tests are JVM-only.

## Fourth Slice: Spline And Acquisition Timing

`RigidSpline` is layered over a rigid estimate. It is not a replacement for the
rigid estimator.

Initial spline behavior:

- smooth pose traces with a deterministic cubic/B-spline-like temporal stencil;
- validate `SliceTiming` and `PacketTiming` against the run's z dimension;
- evaluate packet-time poses during final application;
- report packet correction magnitude in `MotionQc`.

Acceptance:

- packet offsets differ from volume-center times when slice timing is supplied;
- packet-aware application is byte-identical to rigid application when offsets
  are all zero;
- invalid slice timing and multiband grouping are typed errors.

## Later JVM Layers

Keep these outside the shared module until the core is stable:

- NIfTI read/write preserving affine, voxel size, and TR metadata;
- BIDS/fMRIPrep scan discovery and sidecar-derived TR/slice timing;
- command-line `estimate`, `apply`, `run`, and `report` commands;
- report bundle generation: motion TSV, matrices CSV, summary CSV, overview
  plot data;
- benchmark harnesses and real-data comparison scripts.

JVM adapters should depend on the shared `motion` API. Shared code should never
call into them.

## R Reference Map

Use these `volregger` files to anchor behavior while keeping the Scala API
typed and smaller:

| ScalaFIM area | R/C++ reference |
|---|---|
| pose algebra and displacement metrics | `R/transform_metrics.R`, `src/api_apply.cpp`, `src/api_estimate.cpp` |
| FD/DVARS and QC flags | `R/fd_dvars.R`, `R/qc.R`, `src/api_qc.cpp` |
| input shape and metadata conventions | `R/adapters-neuroim2.R`, `R/apply_motion.R`, `R/estimate_motion.R` |
| controls and profiles | `R/control.R`, `R/profiles.R` |
| rigid robust estimation | `src/api_estimate.cpp`, `tests/testthat/test-volreg-basic.R`, `tests/testthat/test-synthetic-correctness-grid.R`, `tests/testthat/test-synthetic-rigorous.R` |
| final resampling and interpolation | `R/apply_motion.R`, `src/api_apply.cpp` |
| slice/packet-aware smoothing | `src/api_spline.cpp`, `tests/testthat/test-synthetic-ablation-modes.R` |
| CLI/reporting adapters | `R/cli.R`, `R/reporting.R`, `tests/testthat/test-reporting-cli.R` |

Reference files should guide fixtures and invariants, not public names or data
shapes. If an R helper exists mostly to compensate for list/S3 flexibility, the
Scala version should usually become a constructor or an error ADT case.

## Parity Fixtures

Use `~/code/volregger` as the fixture generator, not as a runtime dependency.
Current tiny fixture oracles live in
`modules/motion/shared/src/test/scala/scalafim/fmri/motion/fixtures`.
The first external fixture generator is
`tools/motion/generate_volregger_fixtures.R`; it writes small static files under
`modules/motion/shared/src/test/resources/motion/` and records the `volregger`
commit plus source files used for generation. Shared JVM/Scala.js tests parse a
Scala mirror of the generated core fixture, while a JVM-only resource test
checks that the mirror matches `motion/volregger_core.fixture`.

Fixture families:

- pose vectors and expected 4x4 matrices;
- inverse and composition examples;
- FD vectors for fixed traces;
- DVARS and robust DVARS for tiny 4D arrays with masks;
- masked displacement summaries for known transforms;
- identity and known-motion synthetic runs once the estimator exists.

Fixture policy:

- store expected values with enough precision to catch convention drift;
- document the `volregger` commit or local version used to generate them;
- keep fixtures small enough for Scala.js tests;
- keep slow real-data and benchmark fixtures JVM-only and outside the default
  test suite.

## Build Wiring Checklist

When the module lands:

1. Add `lazy val motion` crossProject.
2. Add `motionJS` and `motionJVM`.
3. Add both to root aggregate.
4. Add `motionJVM/compile`, `motionJS/compile`, `motionJVM/test`, and
   `motionJS/test` to aliases.
5. Add `modules/motion/README.md`.
6. Add a README module blurb in the root `README.md`.
7. Keep dependency direction `motion -> image,linalg`.

## Non-Goals For The Core

- a direct R API clone: no S3 classes, list-shaped control objects, or warning
  side channels;
- Rcpp, Breeze, native libraries, or JVM-only dependencies in shared code;
- nonlinear registration, surface motion correction, or atlas-aware correction
  before rigid fMRI is tested;
- plot/report/CLI features in shared code;
- implicit repeated resampling hidden in a convenience workflow.
