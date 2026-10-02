package scalafim.fmri.mvpa.fit

import scalafim.dataset.RunId
import scalafim.fmri.fit.{ResponseBlock, TrialCoefficientId, TrialEstimability, TrialReadout}
import scalafim.fmri.mvpa.{
  FeatureIndex,
  MvpaError,
  PatternCopyBudget,
  PatternMatrix,
  PatternOperator,
  PatternOperatorProvenance
}

enum ReadoutFitScope:
  case DesignOnly

final class RunTrialReadout private (
    val runId: RunId,
    val timeSeries: ResponseBlock,
    val readout: TrialReadout,
    val featureIndices: Vector[FeatureIndex],
    val patterns: PatternOperator
):
  require(timeSeries.timepoints == readout.timepoints, "time series and readout timepoints must match")
  require(timeSeries.voxels == featureIndices.length, "feature axis must match time-series columns")
  require(patterns.samples == readout.trials, "pattern rows must match readout trials")
  require(patterns.featureIndices == featureIndices, "pattern feature axis must match run feature axis")

  val fitScope: ReadoutFitScope = ReadoutFitScope.DesignOnly

  def trials: Int = readout.trials
  def features: Int = featureIndices.length

  /** Materializing trial coefficients is an explicit caller-budgeted copy.
    * Relation consumers use `patterns` directly and need no dense trial table.
    */
  def explicitPatterns(budget: PatternCopyBudget): Either[OneShotMvpaError, PatternMatrix] =
    patterns.materialize(budget).left.map(OneShotMvpaError.MvpaFailure.apply)

  private[fit] def selectColumns(features: IndexedSeq[FeatureIndex]): Either[MvpaError, PatternOperator] =
    patterns.selectColumns(features)

object RunTrialReadout:
  def make(
      runId: RunId,
      timeSeries: ResponseBlock,
      readout: TrialReadout
  ): Either[OneShotMvpaError, RunTrialReadout] =
    make(
      runId = runId,
      timeSeries = timeSeries,
      readout = readout,
      featureIndices = Vector.tabulate(timeSeries.voxels)(FeatureIndex.apply)
    )

  def make(
      runId: RunId,
      timeSeries: ResponseBlock,
      readout: TrialReadout,
      featureIndices: Vector[FeatureIndex]
  ): Either[OneShotMvpaError, RunTrialReadout] =
    if readout.timepoints != timeSeries.timepoints then
      Left(OneShotMvpaError.TimepointMismatch(runId, readout.timepoints, timeSeries.timepoints))
    else if featureIndices.length != timeSeries.voxels then
      Left(OneShotMvpaError.FeatureAxisLengthMismatch(runId, featureIndices.length, timeSeries.voxels))
    else
      for
        composed <- readout.operator.compose(timeSeries.value).left.map(OneShotMvpaError.CompositionFailure.apply)
        patterns <- PatternOperator
          .fromOperator(
            samples = readout.trials,
            featureIndices = featureIndices,
            linear = composed,
            provenance = PatternOperatorProvenance.composed
          )
          .left
          .map(OneShotMvpaError.MvpaFailure.apply)
      yield new RunTrialReadout(runId, timeSeries, readout, featureIndices, patterns)
