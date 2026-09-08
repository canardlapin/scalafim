package scalafim.fmri.fit

import gale.linalg.DMat
import scalafim.fmri.design.{CoefficientAxis, ColumnId, RunwiseDesignSlice}

/** The source-column selection and its independently fitted run coordinates.
  * Source IDs identify the request; the run axis identifies the returned values.
  * Optional primary-output uncertainty does not request run uncertainty.
  */
final case class RunCoefficientRetentionDescription private[fit] (
    partition: RunPartition,
    sourceColumnIds: Vector[ColumnId],
    coefficientAxis: CoefficientAxis
):
  require(sourceColumnIds.nonEmpty && sourceColumnIds.distinct.size == sourceColumnIds.size)
  require(sourceColumnIds.size == coefficientAxis.predictors)
  def uncertainty: EstimateUncertaintyRequest = EstimateUncertaintyRequest.None

/** Owned raw coefficients for all input voxels, including explicitly marked
  * voxels excluded from precision pooling. A zero coefficient is not missing.
  */
final case class RetainedRunCoefficientBlock private[fit] (
    description: RunCoefficientRetentionDescription,
    estimates: DMat,
    voxelIndices: Vector[Int],
    statuses: Vector[VoxelFitStatus]
):
  require(estimates.rows == description.coefficientAxis.predictors)
  require(voxelIndices.nonEmpty && voxelIndices.distinct.size == voxelIndices.size)
  require(estimates.cols == voxelIndices.size && statuses.size == voxelIndices.size)
  def uncertainty: OlsEstimateUncertainty = OlsEstimateUncertainty.NotRequested

sealed trait RunCoefficientRetentionResult
object RunCoefficientRetentionResult:
  case object NotRequested extends RunCoefficientRetentionResult
  final case class Retained private[fit] (blocks: Vector[RetainedRunCoefficientBlock])
      extends RunCoefficientRetentionResult:
    require(blocks.nonEmpty, "retained run coefficients must identify at least one run")
    require(blocks.map(_.description.partition.runIndex).distinct.size == blocks.size)

private[fit] object RunCoefficientRetention:
  def describe(
      partition: RunPartition,
      slice: RunwiseDesignSlice,
      sourceAxis: CoefficientAxis,
      requestedSourceColumns: Vector[Int]
  ): Either[FitError, Option[RunCoefficientRetentionDescription]] =
    val included = requestedSourceColumns.filter(slice.sourceColumnIndices.contains)
    if included.isEmpty then Right(None)
    else
      slice.axis.select(included.map(slice.sourceColumnIndices.indexOf))
        .left.map(error => FitError.InvalidFitAxis("retained run coefficient axis", error.message))
        .map(axis => Some(RunCoefficientRetentionDescription(partition, included.map(sourceAxis.columnIds), axis)))
