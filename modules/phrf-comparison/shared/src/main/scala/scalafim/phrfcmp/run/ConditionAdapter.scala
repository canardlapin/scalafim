package scalafim.phrfcmp.run

import scalafim.phrfcmp.score.{ConditionResponse, Method, VoxelOutcome}

/**
  * Adapts the S3 condition arms to the S8 scorer contract (`score.ConditionDataset.of`): per voxel `Estimated(mise)`,
  * `Refused` or `Failed`.
  */
object ConditionAdapter:

  /** The scorer's method for an arm. */
  def method(arm: ConditionArm): Method = arm match
    case ConditionArm.Phrf => Method.Phrf
    case ConditionArm.Can  => Method.Can
    case ConditionArm.Inf3 => Method.Inf3
    case ConditionArm.Fir  => Method.Fir

  /**
    * Status mapping. `Refused` stays a refusal; `Failed` stays a failure; `NotRun` has no estimate either and is
    * reported as `Failed` (a voxel the arm never produced counts against it like any other missing value, which is the
    * conservative reading; the pilot never schedules an arm without running it, so this is a guard, not a path).
    */
  def outcome[A](status: ConditionArmStatus, estimate: => Option[A]): VoxelOutcome[A] = status match
    case ConditionArmStatus.Estimated  => estimate.fold[VoxelOutcome[A]](VoxelOutcome.Failed)(VoxelOutcome.Estimated(_))
    case ConditionArmStatus.Refused(_) => VoxelOutcome.Refused
    case ConditionArmStatus.Failed(_)  => VoxelOutcome.Failed
    case ConditionArmStatus.NotRun     => VoxelOutcome.Failed

  /**
    * Integrated squared error of a fitted response against the true one, averaged over conditions: the trapezoid rule
    * on the shared E-resp grid (data units squared times seconds). `None` if the grids or condition counts differ or a
    * value is not finite.
    */
  def mise(fitted: ConditionResponse, truth: ConditionResponse): Option[Double] =
    val g = fitted.grid
    val ok = fitted.conditions == truth.conditions && fitted.conditions > 0 && g.size == truth.grid.size && g.size > 1 &&
      (0 until g.size).forall(i => g.lag(i) == truth.grid.lag(i))
    if !ok then None
    else
      var total = 0.0
      var c = 0
      while c < fitted.conditions do
        var acc = 0.0
        var i = 0
        while i + 1 < g.size do
          val d0 = fitted.curves(c)(i) - truth.curves(c)(i)
          val d1 = fitted.curves(c)(i + 1) - truth.curves(c)(i + 1)
          acc += 0.5 * (d0 * d0 + d1 * d1) * (g.lag(i + 1) - g.lag(i))
          i += 1
        total += acc
        c += 1
      val v = total / fitted.conditions
      if v.isNaN || v.isInfinite then None else Some(v)

  /** Per-voxel scorer outcomes of one arm against the per-voxel true responses (one entry per voxel, same order). */
  def toScorer(result: ConditionArmResult, truth: Vector[ConditionResponse]): Vector[VoxelOutcome[Double]] =
    if truth.length != result.voxels.length then Vector.fill(result.voxels.length)(VoxelOutcome.Failed)
    else result.voxels.zip(truth).map((v, t) => outcome(v.status, v.response.flatMap(mise(_, t))))
