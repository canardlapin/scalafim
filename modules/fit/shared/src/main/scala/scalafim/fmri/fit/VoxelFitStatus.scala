package scalafim.fmri.fit

import gale.linalg.{DMat, DVec}

/** Whether one response column can support coefficient inference.
  *
  * Degenerate columns remain addressable by their source voxel id, but they do
  * not enter contrast statistics. This keeps missingness out of primitive
  * coefficient/statistic arrays while allowing healthy voxels in the same fit
  * to proceed.
  */
enum VoxelFitStatus:
  case Estimable
  case AllZero
  case Constant
  case NonFinite
  case NoObservedResponses
  case InsufficientResidualDegreesOfFreedom
  case RankDeficientObservedDesign
  case ZeroResidualVariance

  def label: String =
    this match
      case Estimable           => "ok"
      case AllZero             => "all_zero"
      case Constant            => "constant"
      case NonFinite           => "nonfinite"
      case NoObservedResponses => "no_observed_responses"
      case InsufficientResidualDegreesOfFreedom => "insufficient_residual_degrees_of_freedom"
      case RankDeficientObservedDesign => "rank_deficient_observed_design"
      case ZeroResidualVariance => "zero_residual_variance"

  def supportsInference: Boolean = this == Estimable

object VoxelFitStatus:
  private val ConstantVarianceTolerance = 2.220446049250313e-16

  private[fit] def classify(response: ResponseBlock): Vector[VoxelFitStatus] =
    classify(response.value, (0 until response.timepoints).toVector)

  private[fit] def classify(response: DMat, rows: Vector[Int]): Vector[VoxelFitStatus] =
    require(rows.forall(row => row >= 0 && row < response.rows), "voxel-status rows must be in response bounds")
    Vector.tabulate(response.cols)(voxel => classifyColumn(response, rows, voxel))

  private[fit] def refine(
      initial: Vector[VoxelFitStatus],
      residualVariance: DVec
  ): Vector[VoxelFitStatus] =
    require(initial.length == residualVariance.length, "voxel statuses must match residual variances")
    initial.indices.toVector.map { voxel =>
      initial(voxel) match
        case VoxelFitStatus.Estimable if !residualVariance(voxel).isFinite =>
          VoxelFitStatus.NonFinite
        case VoxelFitStatus.Estimable if residualVariance(voxel) <= 0.0 =>
          VoxelFitStatus.ZeroResidualVariance
        case status => status
    }

  /** Collapse several run-local failure states into one stable public status.
    * Non-finite data take precedence over numerical variance failures, followed
    * by exact-zero and constant response degeneracy.
    */
  private[fit] def aggregate(statuses: Vector[VoxelFitStatus]): VoxelFitStatus =
    if statuses.contains(VoxelFitStatus.NoObservedResponses) then VoxelFitStatus.NoObservedResponses
    else if statuses.contains(VoxelFitStatus.RankDeficientObservedDesign) then VoxelFitStatus.RankDeficientObservedDesign
    else if statuses.contains(VoxelFitStatus.InsufficientResidualDegreesOfFreedom) then VoxelFitStatus.InsufficientResidualDegreesOfFreedom
    else if statuses.contains(VoxelFitStatus.NonFinite) then VoxelFitStatus.NonFinite
    else if statuses.contains(VoxelFitStatus.ZeroResidualVariance) then VoxelFitStatus.ZeroResidualVariance
    else if statuses.contains(VoxelFitStatus.AllZero) then VoxelFitStatus.AllZero
    else if statuses.contains(VoxelFitStatus.Constant) then VoxelFitStatus.Constant
    else VoxelFitStatus.Estimable

  private def classifyColumn(response: DMat, rows: Vector[Int], voxel: Int): VoxelFitStatus =
    if rows.isEmpty then VoxelFitStatus.Constant
    else
      var allZero = true
      var mean = 0.0
      var sumSquares = 0.0
      var count = 0
      var i = 0
      while i < rows.length do
        val value = response(rows(i), voxel)
        if !value.isFinite then return VoxelFitStatus.NonFinite
        if value != 0.0 then allZero = false
        count += 1
        val delta = value - mean
        mean += delta / count.toDouble
        sumSquares += delta * (value - mean)
        i += 1

      if allZero then VoxelFitStatus.AllZero
      else if count < 2 || sumSquares / (count - 1).toDouble <= ConstantVarianceTolerance then
        VoxelFitStatus.Constant
      else VoxelFitStatus.Estimable

/** A fit-status observation tied to its source voxel identity. */
final case class VoxelFitStatusRecord(
    voxelIndex: Int,
    status: VoxelFitStatus
):
  require(voxelIndex >= 0, "voxel status index must be non-negative")

/** A source voxel deliberately omitted from a numerical fit or finite contrast. */
final case class VoxelInferenceExclusion(
    voxelIndex: Int,
    status: VoxelFitStatus
):
  require(voxelIndex >= 0, "excluded voxel index must be non-negative")
  require(!status.supportsInference, "an estimable voxel cannot be an inference exclusion")

  def statusRecord: VoxelFitStatusRecord =
    VoxelFitStatusRecord(voxelIndex, status)

private[fit] object VoxelInferenceExclusions:
  def combine(
      groups: IterableOnce[VoxelInferenceExclusion]*
  ): Either[FitError, Vector[VoxelInferenceExclusion]] =
    val out = Vector.newBuilder[VoxelInferenceExclusion]
    val seen = scala.collection.mutable.HashMap.empty[Int, VoxelFitStatus]
    val iterator = groups.iterator.flatMap(_.iterator)
    while iterator.hasNext do
      val exclusion = iterator.next()
      seen.get(exclusion.voxelIndex) match
        case None =>
          seen.update(exclusion.voxelIndex, exclusion.status)
          out += exclusion
        case Some(status) if status == exclusion.status => ()
        case Some(status) =>
          return Left(FitError.IncompatibleFitBlocks(
            s"voxel ${exclusion.voxelIndex} has conflicting exclusion statuses ${status.label} and ${exclusion.status.label}"
          ))
    Right(out.result())

  def validateDisjoint(
      retainedVoxelIndices: Vector[Int],
      exclusions: Vector[VoxelInferenceExclusion],
      component: String
  ): Unit =
    require(exclusions.map(_.voxelIndex).distinct.length == exclusions.length, s"$component exclusions must be unique")
    require(
      exclusions.forall(exclusion => !retainedVoxelIndices.contains(exclusion.voxelIndex)),
      s"$component retained and excluded voxels must be disjoint"
    )
