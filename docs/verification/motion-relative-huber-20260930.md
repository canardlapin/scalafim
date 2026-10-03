# Relative Huber threshold for rigid motion (2026-09-30)

Mote: `bd-01M3R7YC946W1YRSX87YHBYWWV`. Base: local main `9504d696` (which has the
scale-invariant solve). Found in the independent review of that solve: the
Huber threshold `huberK = 1.5` was an absolute intensity difference, so robust
weighting, and therefore the estimated motion, depended on the image intensity
scale.

## Estimand change

`OptimizerControl` gains a typed `huberScale: HuberScale`.

- `HuberScale.RobustResidual` (new default): `huberK` counts robust residual
  standard deviations, and the absolute threshold is `huberK * s`.
  - `s = 1.4826 * MAD` of the frame's residuals over the diagnostic samples. It
    is estimated at the warm-start pose for the coarse capture search, then
    re-estimated once at the captured pose, where residuals are nearer
    alignment. It is then fixed for that frame's pyramid levels and diagnostics,
    and the stored per-frame values are reused when diagnostics are recomputed.
  - The first candidate took `s` at the identity pose. The independent review
    showed that this inflates `s` for high-motion frames, making Huber nearly
    inactive exactly where robustness matters. Its stated benefit (independence
    from processing order) was also false, since fitting is already sequential
    and warm-started.
  - Scaling all intensities by `c` scales every residual and `s` by `c`. The
    Huber weights are unchanged and every cost scales by `c²`. With the relative
    stopping rule (`9504d696`), the motion estimates and iteration counts are
    invariant.
  - Degenerate scales use covariant fallbacks, with round-off defined as `1e-9`
    of the residual RMS:
    - a MAD at or below round-off (more than half the residuals identical) falls
      back to the MAD of the deviations above round-off, for exactly matching
      background;
    - then to the residual RMS when all residuals are equal (a pure intensity
      offset);
    - then, for an exact match, to `1e-12` of the mean absolute template value.
- `HuberScale.Absolute` (`OptimizerControl.volreggerParity`): the previous
  volregger estimand, kept explicitly for parity studies.

The robust-template refresh compares final costs across frames. Its MAD floor
is now `1e-12 * |median cost|` instead of an absolute `1e-12`.

## Line-search exhaustion

Previously, six failed step halvings set `levelConverged = true`, so the frame
reported `converged`. Exhaustion still ends the level, but it now counts as
converged only when the untried final step `step / 64` is at or below
`stepTolerance` (`MotionEstimator.lineSearchResolved`). So a Gauss-Newton step
up to 64 times `stepTolerance` still counts as resolved; a larger step that
cannot reduce the cost makes the level stalled and the frame non-converged.

Consequence for the default pipeline: template refresh keeps only converged
frames (`refreshValidOnly = true`), so stalled frames are now excluded from the
refreshed template where they were previously included.

Finding: on the 7x5x5 estimator fixture, the search stalls at the capture grid
point after one iteration under either threshold. The previous rule reported
this as converged; it is now reported honestly. That fixture is therefore used
only as a stall witness. Accuracy is established on a realistic volume.

## Evidence

- Unit tests pass: 105/105 on `motionJVM/testOnly scalafim.fmri.motion.*`, and on
  JS in the final gate. This includes the unchanged `VolreggerParitySuite`,
  whose tolerances still hold under the new default. They do not discriminate
  between the two thresholds, so parity under `volreggerParity` is not
  separately claimed.
- `MotionHuberScaleSuite`:
  - Defaults: `RobustResidual` is the default, and `volreggerParity` is
    `Absolute` with the same `huberK`.
  - Realistic 20x18x14 volume with an artefact block: the robust fit converges
    with overlap 1.0 and recovers the true x shift with the correct sign (tx =
    0.583 at a true 0.6 and 1.385 at a true 1.4). |ty| and |tz| are below 0.05
    and all rotations below 0.02 rad. Least squares is pulled well away, and the
    pose is invariant across intensity scales 1e-6 to 1e6.
  - Template refresh that actually runs: three patterned frames on the
    realistic volume, all converged. Poses with the refresh differ from the
    first pass by more than 1e-4, so the second pass used a refreshed template.
    With the refresh, poses are invariant across scales, convergence flags are
    equal, and every `costFinal` scales by `c²` (relative 1e-5; poses agree to
    about 1e-7).
  - Small 7x5x5 fixture with Huber active: all six pose parameters are equal
    within 1e-6 across scales 1e-6 to 1e6, with equal iteration counts and
    convergence flags.
  - Zero-background (exactly matching) fixture: poses are invariant across
    scales. This test checks covariance only; it does not distinguish which
    fallback branch fires.
  - End-to-end stall: the 7x5x5 fixture stalls and is reported non-converged
    after one iteration under both thresholds, while realistic fits stay
    converged. This relies on the fixture's incidental stall, not on a stall
    constructed deliberately. The rule is also unit-tested at, below and above
    the tolerance.
  - `Absolute` still shows its intensity-scale dependence (a documented
    control).
- Mutations (logs `motion-huber-rev2-mutant{A,B,C,D}.log`; source restored and
  verified by hash):
  - A: forcing the absolute threshold under `RobustResidual` fails 4 tests
    (invariance, realistic accuracy, refresh invariance, stall).
  - B: no re-estimation at the captured pose (warm-start scale throughout)
    fails 1 test, the realistic-accuracy test, through its translation/rotation
    bounds.
  - C: no round-off guard fails 2 tests, the existing pure-offset
    `MotionEstimatorSuite` tests (a spurious 1e-15 scale from rounding in
    `(a + 12) - a`). The new zero-background test does not catch it.
  - D: taking the scale at the identity pose (the first candidate) fails 1
    test, the realistic-accuracy test.

Limitations:

- The captured pose comes from a coarse grid (translation steps up to 1 mm),
  so the fixed scale can still include misregistration of up to half a grid
  step.
- When more than half the residuals are exactly zero (for example unmasked
  zero background), the scale switches to the MAD of the non-zero deviations.
  So the scale can jump as that fraction crosses one half; a brain mask avoids
  this regime.
- Final costs are compared across frames in the robust-template rule without
  per-frame normalisation.

Candidate follow-ups (not blocking): re-estimate the scale after the first
pyramid level; normalise costs across frames in the template rule; construct a
deliberate stall fixture.

Not claimed: any change to real-data motion accuracy. The new default changes
robust weights relative to volregger's absolute threshold, and real-data
comparison is not measured here.
