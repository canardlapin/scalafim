package scalafim.fmri.mvpa.fit

import scalafim.dataset.RunId
import scalafim.fmri.fit.{ResponseBlock, TrialCoefficientId, TrialEstimability, TrialReadout}
import scalafim.fmri.mvpa.{
  FeatureIndex,
  FeatureSet,
  Fold,
  FoldPlan,
  MvpaError,
  OperatorPatternSource,
  PatternMatrix,
  PatternOperator,
  PatternOperatorProvenance,
  PatternSource,
  SampleAxis,
  SampleIndex
}
import gale.linalg.Matrix

enum ReadoutFitScope:
  case DesignOnly

opaque type RunOrdinal = Int

object RunOrdinal:
  private[fit] def apply(value: Int): RunOrdinal =
    require(value >= 0, "run ordinal must be non-negative")
    value

  extension (ordinal: RunOrdinal)
    inline def value: Int = ordinal

opaque type TrialWithinRunIndex = Int

object TrialWithinRunIndex:
  private[fit] def apply(value: Int): TrialWithinRunIndex =
    require(value >= 0, "trial-within-run index must be non-negative")
    value

  extension (index: TrialWithinRunIndex)
    inline def value: Int = index

final case class OneShotTrialRow private[fit] (
    sampleIndex: SampleIndex,
    runId: RunId,
    runOrdinal: RunOrdinal,
    trialWithinRun: TrialWithinRunIndex,
    trialId: TrialCoefficientId,
    estimability: TrialEstimability
)

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

  def explicitPatterns: Either[OneShotMvpaError, PatternMatrix] =
    readout
      .forward(timeSeries)
      .left
      .map(error => OneShotMvpaError.ReadoutFailure(runId, error))
      .map: coefficients =>
        PatternMatrix(
          value = coefficients.value,
          sampleIndices = Vector.tabulate(trials)(SampleIndex.apply),
          featureIndices = featureIndices
        )

  private[fit] def patternsFor(
      featureSet: FeatureSet,
      positions: Array[Int]
  ): Either[MvpaError, PatternOperator] =
    require(positions.length == featureSet.size, "feature positions must match the requested feature set")
    val selected = Matrix.newBuilder(timeSeries.timepoints, featureSet.size)
    var timepoint = 0
    while timepoint < timeSeries.timepoints do
      var feature = 0
      while feature < positions.length do
        selected(timepoint, feature) = timeSeries.value(timepoint, positions(feature))
        feature += 1
      timepoint += 1

    for
      composed <- readout.operator
        .compose(selected.result())
        .left
        .map(MvpaError.PatternOperatorFailed.apply)
      sampleAxis <- SampleAxis(readout.trials)
      patterns <- PatternOperator.fromOperator(
        sampleAxis = sampleAxis,
        featureIndices = featureSet.featureIndices,
        linear = composed,
        provenance = PatternOperatorProvenance.composed.afterFeatureSelection
      )
    yield patterns

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
        sampleAxis <- SampleAxis(readout.trials).left.map(OneShotMvpaError.MvpaFailure.apply)
        patterns <- PatternOperator
          .fromOperator(
            sampleAxis = sampleAxis,
            featureIndices = featureIndices,
            linear = composed,
            provenance = PatternOperatorProvenance.composed
          )
          .left
          .map(OneShotMvpaError.MvpaFailure.apply)
      yield new RunTrialReadout(runId, timeSeries, readout, featureIndices, patterns)

final class OneShotDataset private (
    val runs: Vector[RunTrialReadout],
    val patterns: PatternOperator,
    val trialRows: Vector[OneShotTrialRow]
):
  require(runs.nonEmpty, "one-shot dataset must contain at least one run")
  require(patterns.samples == trialRows.length, "pattern rows must match trial metadata")

  def samples: Int = patterns.samples
  def features: Int = patterns.features
  def featureIndices: Vector[FeatureIndex] = patterns.featureIndices
  def patternSource: OperatorPatternSource = cachedPatternSource

  private lazy val cachedPatternSource: OperatorPatternSource =
    OneShotPatternSource(this)

  def leaveOneRunOut: Either[OneShotMvpaError, FoldPlan] =
    if runs.length < 2 then Left(OneShotMvpaError.InsufficientRunsForCrossValidation(runs.length))
    else
      FoldPlan
        .leaveOneBlockOut(trialRows.map(_.runOrdinal.value))
        .left
        .map(OneShotMvpaError.MvpaFailure.apply)

  def patternsFor(fold: Fold, partition: FoldPartition): Either[OneShotMvpaError, PatternOperator] =
    val rows =
      partition match
        case FoldPartition.Training => fold.train
        case FoldPartition.Test     => fold.test
    patterns.selectRows(rows).left.map(OneShotMvpaError.MvpaFailure.apply)

  def explicitPatterns: Either[OneShotMvpaError, PatternMatrix] =
    val runPatterns = Vector.newBuilder[PatternMatrix]
    var run = 0
    while run < runs.length do
      runs(run).explicitPatterns match
        case Left(error)   => return Left(error)
        case Right(values) => runPatterns += values
      run += 1

    val matrices = runPatterns.result()
    val out = Matrix.newBuilder(samples, features)
    var rowOffset = 0
    run = 0
    while run < matrices.length do
      val matrix = matrices(run).value
      var row = 0
      while row < matrix.rows do
        var col = 0
        while col < matrix.cols do
          out(rowOffset + row, col) = matrix(row, col)
          col += 1
        row += 1
      rowOffset += matrix.rows
      run += 1

    Right(
      PatternMatrix(
        value = out.result(),
        sampleIndices = Vector.tabulate(samples)(SampleIndex.apply),
        featureIndices = featureIndices
      )
    )

enum FoldPartition:
  case Training
  case Test

private final case class OneShotPatternSource(
    dataset: OneShotDataset
) extends OperatorPatternSource:
  private val featurePositions =
    dataset.featureIndices.zipWithIndex.map((feature, index) => feature.value -> index).toMap

  override def samples: Int = dataset.samples
  override def featureIndices: Vector[FeatureIndex] = dataset.featureIndices

  override def selectFeatures(featureSet: FeatureSet): Either[MvpaError, PatternOperator] =
    val positions = new Array[Int](featureSet.size)
    var feature = 0
    while feature < featureSet.size do
      val requested = featureSet.featureIndices(feature)
      featurePositions.get(requested.value) match
        case Some(position) => positions(feature) = position
        case None           => return Left(MvpaError.MissingFeature(featureSet.id, requested))
      feature += 1

    val operators = Vector.newBuilder[PatternOperator]
    var run = 0
    while run < dataset.runs.length do
      dataset.runs(run).patternsFor(featureSet, positions) match
        case Left(error)     => return Left(error)
        case Right(patterns) => operators += patterns
      run += 1
    PatternOperator.stackRows(operators.result())

object OneShotDataset:
  def make(runs: Seq[RunTrialReadout]): Either[OneShotMvpaError, OneShotDataset] =
    val blocks = runs.toVector
    if blocks.isEmpty then Left(OneShotMvpaError.EmptyRuns)
    else
      val duplicateRuns = blocks
        .groupBy(_.runId)
        .collect:
          case (runId, occurrences) if occurrences.length > 1 => runId
        .toVector
        .sortBy(_.value)
      if duplicateRuns.nonEmpty then Left(OneShotMvpaError.DuplicateRuns(duplicateRuns))
      else
        val featureAxis = blocks.head.featureIndices
        val mismatch = blocks.find(_.featureIndices != featureAxis)
        mismatch match
          case Some(run) =>
            Left(
              OneShotMvpaError.FeatureAxisMismatch(
                runId = run.runId,
                expected = featureAxis,
                actual = run.featureIndices
              )
            )
          case None =>
            PatternOperator
              .stackRows(blocks.map(_.patterns))
              .left
              .map(OneShotMvpaError.MvpaFailure.apply)
              .map: stacked =>
                val rows = Vector.newBuilder[OneShotTrialRow]
                var sampleOffset = 0
                var runOrdinal = 0
                while runOrdinal < blocks.length do
                  val block = blocks(runOrdinal)
                  var trial = 0
                  while trial < block.trials do
                    rows += OneShotTrialRow(
                      sampleIndex = SampleIndex(sampleOffset + trial),
                      runId = block.runId,
                      runOrdinal = RunOrdinal(runOrdinal),
                      trialWithinRun = TrialWithinRunIndex(trial),
                      trialId = block.readout.axis.ids(trial),
                      estimability = block.readout.axis.estimability(trial)
                    )
                    trial += 1
                  sampleOffset += block.trials
                  runOrdinal += 1
                new OneShotDataset(blocks, stacked, rows.result())
