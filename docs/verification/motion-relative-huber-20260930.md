# Relative Huber threshold for rigid motion (2026-09-30)

Mote: `bd-01M3R7YC946W1YRSX87YHBYWWV`. Base: local main `9504d696` (which has the
scale-invariant solve). Found in the independent review of that solve: the
Huber threshold `huberK = 1.5` was an absolute intensity difference, so robust
weighting, and therefore the estimated motion, depended on the image intensity
scale.

## Estimand change

`OptimizerControl` gains a typed `huberScale: HuberScale`.

- `HuberScale.RobustResidual` (new default): `huberK` counts robust residual
  standard deviations. For each frame and template the absolute threshold is
  `huberK * s`, where `s = 1.4826 * MAD` of the frame's residuals against the
  template at the identity pose over the diagnostic samples. The same `s` is
  used for that frame's capture, every pyramid level, the reference cost and
  the diagnostics recomputation.
  - It is computed at the identity pose, not the warm start, so it is
    deterministic and independent of frame processing order.
  - Scaling all intensities by `c` scales every residual and `s` by `c`. The
    Huber weights are unchanged, and every cost scales by `c²`. Because the
    stopping rule is relative (`9504d696`), the estimated motion and iteration
    counts are invariant.
  - A zero MAD means more than half the residuals are identical (for example a
    pure intensity offset), so the robust scale is degenerate. It then falls
    back to the residual RMS, and for an exact match to `1e-12` of the mean
    absolute template value. Both fallbacks are covariant.
- `HuberScale.Absolute` (`OptimizerControl.volreggerParity`): the previous
  volregger estimand, kept explicitly for parity studies.

The robust-template refresh compares final costs across frames. Its MAD floor
is now `1e-12 * |median cost|` instead of an absolute `1e-12`.

## Line-search exhaustion

Previously, six failed step halvings set `levelConverged = true` and the frame
reported `converged`. Exhaustion still ends the level, but it now counts as
converged only when the final halved step is at or below `stepTolerance`
(`MotionEstimator.lineSearchResolved`). A larger step that cannot reduce the
cost is reported as a stalled, non-converged level.

## Evidence

- `motionJVM/testOnly scalafim.fmri.motion.*`: 101/101. `motionJS`: 88/88. This
  includes the unchanged `VolreggerParitySuite`, whose parity tolerances still
  hold under the new default, so no switch to `Absolute` was needed there.
- New `MotionHuberScaleSuite`:
  - Defaults: `RobustResidual` is the default, and `volreggerParity` is
    `Absolute` with the same `huberK`.
  - Invariance, with Huber genuinely active: an artefact block, and the fit
    measurably differs from an effectively infinite threshold (least squares).
    At shifts of 0.6 and 1.4 voxels, all six pose parameters agree within 1e-6
    across intensity scales 1e-6, 1e-3, 1, 1e3 and 1e6, with equal iteration
    counts and convergence flags.
  - `Absolute` still shows its intensity-scale dependence (a documented
    control).
  - The line-search resolution rule is tested at, below and above the
    tolerance.
- `MotionSolverScalingSuite`: its comment no longer claims Huber is inactive.
- Degenerate-scale control: the first candidate used a template-magnitude
  fallback for a zero MAD. `MotionEstimatorSuite`'s frame-mean nuisance tests
  (a pure +12 offset) exposed it: the threshold collapsed and the raw cost fell
  below its `> 15` check. The RMS fallback fixes this; those tests are unchanged.
- Mutation: forcing the absolute threshold under `RobustResidual` fails the
  invariance test (`logs/motion-huber-mutantA.log`). The source was restored
  and verified by hash.

Not claimed: any change to real-data motion accuracy. On real data the new
default changes robust weights relative to volregger's absolute threshold, and
that comparison is not measured here.
