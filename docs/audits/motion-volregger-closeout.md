# Motion Volregger Closeout Audit

Date: 2026-07-07

This audit closes the useful `~/code/volregger` port into the ScalaFIM motion
module as an idiomatic Scala 3 API, not an R surface clone.

## Scope

Audited module:

- shared core: `modules/motion/shared/src/main/scala/scalafim/fmri/motion`
- JVM adapters: `modules/motion/jvm/src/main/scala/scalafim/fmri/motion`
- tests and fixtures: `modules/motion/{shared,jvm}/src/test`

The tracker epic is `bd-01KWX6PCMAKNTGYQQV1R2WVVZX`.

## API Shape

The public shared API is typed and inspectable:

- domain scalars are opaque types with checked constructors in `Units.scala`
  and `MotionTiming.scala`;
- closed alternatives are represented by enums: `ReferenceStrategy`,
  `MotionEngine`, `Interpolation`, `PadMode`, `AcquisitionTiming`,
  `DvarsPolicy`, `MotionProfile`, `MotionCapability`, and the control-policy
  enums in `MotionControl.scala`;
- invariant-carrying records use private constructors and smart constructors:
  `RigidPose`, `MotionTrace`, `ApplyControl`, `SliceTiming`, `PacketTiming`,
  `PyramidControl`, `OptimizerControl`, `CaptureControl`, `TemporalControl`,
  `ExecutionControl`, `InformationContentStencil`, and QC fit-cost wrappers;
- public failure paths return `Either[MotionError, T]` or
  JVM-local `Either[MotionIoError, T]` / `Either[MotionBenchmarkError, T]`;
- active and planned features are separated through `MotionProfile` and
  `MotionCapability`, with unsupported execution reported as
  `MotionError.UnsupportedControl`.

The result surface is data-first: `MotionPlan` describes intent,
`MotionEstimate` stores frame-aligned poses/diagnostics, `MotionQc` stores QC
metrics, and `MotionCorrectionResult` bundles estimate, optional corrected run,
QC, and controls.

## Dependency Boundary

The shared motion module depends only on `image` and `linalg`:

```scala
lazy val motion = ...dependsOn(image, linalg)
```

JVM-only adapters add the BIDS dependency:

```scala
lazy val motionJVM = motion.jvm.dependsOn(bidsJVM)
```

The shared source tree contains no file IO, BIDS, plotting, process execution,
or JVM-only numeric libraries. JVM-only code is isolated in:

- `scalafim.fmri.motion.io`
- `scalafim.fmri.motion.benchmark`
- `MotionPlatform.scala`

Scala.js keeps only its platform shim and rejects unsupported parallel frame
execution as a typed control error.

## Implemented Evidence

Shared/core:

- rigid pose algebra, matrix conversion, inverse/compose, motion traces;
- FD, DVARS, robust DVARS, displacement summaries, and QC;
- linear final application with clamp/zero padding;
- validated slice and packet timing with packet-aware application;
- rigid robust estimator, pyramid levels, rotational capture, robust template
  refresh, low-motion shrink, temporal regularization, frame-mean residual
  removal, IC stencil sampling, and frame-mean whitening;
- deterministic JVM ordered parallel frame mapping, with Scala.js typed-off.

JVM adapters:

- NIfTI/sidecar read-write adapter preserving affine, voxel size, TR, and slice
  timing metadata;
- BIDS/fMRIPrep scan discovery through the typed BIDS project API;
- executable typed CLI commands for `estimate`, `apply`, `run`, and `report`;
- motion TSV, transform matrix CSV, and summary CSV bundle writer;
- opt-in synthetic/external benchmark harness with threshold summaries and
  external volregger/RNiftyReg guardrail CSV validation.

Fixture and benchmark evidence:

- core fixture generator:
  `tools/motion/generate_volregger_fixtures.R`;
- checked-in fixture:
  `modules/motion/shared/src/test/resources/motion/volregger_core.fixture`;
- parity ledger:
  `modules/motion/shared/src/test/resources/motion/parity_matrix.md`;
- benchmark command:

```sh
sbt "motionJVM/runMain scalafim.fmri.motion.benchmark.MotionBenchmarkCli --profile default --out modules/motion/jvm/target/motion-benchmark"
```

The benchmark command writes `motion_benchmark_raw.csv`,
`motion_benchmark_checks.csv`, and `motion_benchmark_summary.md`.

## Verification

Current verification passed:

```sh
sbt motionJVM/test
sbt motionJS/test
sbt "motionJVM/runMain scalafim.fmri.motion.benchmark.MotionBenchmarkCli --profile default --out modules/motion/jvm/target/motion-benchmark"
sbt compileAll
sbt testAll
git diff --check -- modules/motion docs/plans/motion.md
```

Observed focused motion counts:

- `motionJVM/test`: 88 tests passed
- `motionJS/test`: 77 tests passed

The documented default benchmark command produced passing threshold rows for
`low_motion`, `moderate_motion`, and `hard_motion_plus_nuisance`.

## Unsupported Feature Ledger

These are intentionally not claimed complete:

- full template-mode IC whitening numeric recovery:
  `WhiteningPolicy.IcWhiten` remains a planned/unsupported control;
- packaged shell-command distribution: `MotionCli` executes typed commands via
  JVM entrypoints, but no installed launcher script is provided;
- compressed NIfTI writing: `.nii.gz` reads are supported through image IO, but
  the lightweight writer intentionally writes uncompressed `.nii` plus sidecar;
- overview plots/report graphics;
- external real-data benchmark datasets and Docker/RNiftyReg/AFNI backends:
  these remain opt-in external inputs validated through the benchmark CSV
  summary path;
- richer packet-aware estimator semantics beyond deterministic spline smoothing
  plus packet-aware final application.

## Verdict

The volregger motion port has a complete typed ScalaFIM core, JVM adapter layer,
fixture/benchmark evidence path, and documented unsupported-feature boundary.
The final release gate is satisfied for the current experimental library state.
