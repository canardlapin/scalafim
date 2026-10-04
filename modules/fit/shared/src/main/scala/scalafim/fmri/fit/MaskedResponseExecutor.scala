package scalafim.fmri.fit

import scalafim.dataset.{
  DataSelection,
  DatasetSeriesReader,
  FmriSeries,
  IndexSelection
}
import scalafim.fmri.model.{
  FitEngine,
  FitPlan,
  FitStrategy,
  FixedWeightAlignment,
  MissingDataPolicy,
  NuisanceProjection,
  VolumeWeighting
}
import gale.linalg.Matrix

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

private[fit] final case class PlannedObservationPattern(
    pattern: ObservationPattern,
    series: FmriSeries
)

/** The response-independent result of finding voxel-specific finite-row masks.
  *
  * Chunked execution keeps this metadata only; fitted response matrices are read
  * again per pattern through the normal chunked executor. The metadata is not
  * bounded by the block size: it holds one row mask per distinct observation
  * pattern plus every voxel's membership, so it is O(P·T + V) and degrades to
  * O(V·T) when every voxel has its own mask.
  */
private[fit] final case class MaskedResponseMetadata(
    sourceTimepoints: Vector[Int],
    sourceVoxels: Vector[Int],
    patterns: Vector[ObservationPattern],
    exclusions: Vector[VoxelInferenceExclusion],
    missingValueCount: Long
):
  require(sourceTimepoints.nonEmpty, "masked response metadata must retain source timepoints")
  require(sourceVoxels.nonEmpty, "masked response metadata must retain source voxels")
  require(patterns.nonEmpty || exclusions.length == sourceVoxels.length, "masked response metadata must fit or exclude every voxel")
  require(missingValueCount >= 0, "masked response missing-value count must be non-negative")

  def hasMissingValues: Boolean = missingValueCount > 0

private[fit] final case class MaskedResponsePlan(
    metadata: MaskedResponseMetadata,
    patterns: Vector[PlannedObservationPattern]
)

private[fit] object MaskedResponsePlanner:

  def plan(series: FmriSeries): Either[FitError, MaskedResponsePlan] =
    val positionsByRows = mutable.LinkedHashMap.empty[Vector[Int], Vector[Int]]
    val exclusions = Vector.newBuilder[VoxelInferenceExclusion]
    var missingCount = 0L
    var voxelPosition = 0
    while voxelPosition < series.nVoxels do
      val observedRows = Vector.newBuilder[Int]
      var row = 0
      while row < series.nTimepoints do
        if series.data(row, voxelPosition).isFinite then observedRows += row
        else missingCount += 1
        row += 1
      val rows = observedRows.result()
      if rows.isEmpty then
        exclusions += VoxelInferenceExclusion(
          series.voxelIndices(voxelPosition),
          VoxelFitStatus.NoObservedResponses
        )
      else
        positionsByRows.update(rows, positionsByRows.getOrElse(rows, Vector.empty) :+ voxelPosition)
      voxelPosition += 1

    val patterns = Vector.newBuilder[PlannedObservationPattern]
    var patternId = 0
    val grouped = positionsByRows.iterator
    while grouped.hasNext do
      val (rowPositions, voxelPositions) = grouped.next()
      val timepoints = rowPositions.map(series.timepoints)
      val voxelIndices = voxelPositions.map(series.voxelIndices)
      val pattern = ObservationPattern.unsafe(patternId, rowPositions, timepoints, voxelIndices)
      val data = new Array[Double](rowPositions.length * voxelPositions.length)
      var localRow = 0
      while localRow < rowPositions.length do
        var localVoxel = 0
        while localVoxel < voxelPositions.length do
          val value = series.data(rowPositions(localRow), voxelPositions(localVoxel))
          require(value.isFinite, "observation-pattern construction must retain only finite values")
          data(localRow * voxelPositions.length + localVoxel) = value
          localVoxel += 1
        localRow += 1
      val patternSeries = FmriSeries
        .make(
          data = Matrix.dense(rowPositions.length, voxelPositions.length, data.toIndexedSeq),
          voxelIndices = voxelPositions.map(series.voxelIndexValues),
          timepoints = rowPositions.map(series.timepointIndices),
          shape = series.shape,
          metadata = series.metadata
        )
        .left
        .map(error => FitError.InvalidFitAxis("masked response", error.message))
      patternSeries match
        case Left(error) => return Left(error)
        case Right(value) => patterns += PlannedObservationPattern(pattern, value)
      patternId += 1

    val plannedPatterns = patterns.result()
    val metadata = MaskedResponseMetadata(
      sourceTimepoints = series.timepoints,
      sourceVoxels = series.voxelIndices,
      patterns = plannedPatterns.map(_.pattern),
      exclusions = exclusions.result(),
      missingValueCount = missingCount
    )
    Right(MaskedResponsePlan(metadata, plannedPatterns))

  def discover(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection,
      chunking: FitChunkingStrategy
  ): Either[FitError, MaskedResponseMetadata] =
    for
      _ <-
        if reader.dataset.id == plan.model.dataset.id &&
            reader.dataset.shape == plan.model.dataset.shape
        then Right(())
        else Left(FitError.InvalidFitAxis(
          "dataset reader",
          s"reader dataset '${reader.dataset.id.value}' does not match model dataset '${plan.model.dataset.id.value}'"
        ))
      chunks <- FitChunkPlan.fromSelection(plan, selection, chunking)
      metadata <- discoverChunks(reader, chunks)
    yield metadata

  private def discoverChunks(
      reader: DatasetSeriesReader,
      chunks: FitChunkPlan
  ): Either[FitError, MaskedResponseMetadata] =
    val positionsByRows = mutable.LinkedHashMap.empty[Vector[Int], Vector[Int]]
    val exclusions = Vector.newBuilder[VoxelInferenceExclusion]
    // Missing cells across a whole selection can exceed Int.MaxValue (V x T), so count in Long.
    var missingCount = 0L
    var chunkIndex = 0
    while chunkIndex < chunks.length do
      val chunk = chunks.indexed(chunkIndex)
      val series = ChunkedFitExecutor.readChunk(reader, chunk) match
        case Left(error) => return Left(error)
        case Right(value) => value
      var voxelPosition = 0
      while voxelPosition < series.nVoxels do
        val observedRows = Vector.newBuilder[Int]
        var row = 0
        while row < series.nTimepoints do
          if series.data(row, voxelPosition).isFinite then observedRows += row
          else missingCount += 1
          row += 1
        val rows = observedRows.result()
        val voxel = chunk.voxelIndices(voxelPosition)
        if rows.isEmpty then
          exclusions += VoxelInferenceExclusion(voxel, VoxelFitStatus.NoObservedResponses)
        else
          positionsByRows.update(rows, positionsByRows.getOrElse(rows, Vector.empty) :+ voxel)
        voxelPosition += 1
      chunkIndex += 1

    val patterns = Vector.newBuilder[ObservationPattern]
    var patternId = 0
    val grouped = positionsByRows.iterator
    while grouped.hasNext do
      val (rows, voxels) = grouped.next()
      patterns += ObservationPattern.unsafe(
        patternId,
        rows,
        rows.map(chunks.timepoints),
        voxels
      )
      patternId += 1
    Right(MaskedResponseMetadata(
      sourceTimepoints = chunks.timepoints,
      sourceVoxels = chunks.voxelIndices,
      patterns = patterns.result(),
      exclusions = exclusions.result(),
      missingValueCount = missingCount
    ))

/** Executes voxel-specific row omission without admitting non-finite values to
  * `ResponseBlock` or pretending that unlike observed designs share one
  * inference geometry.
  */
private[fit] object MaskedResponseExecutor:

  def fit(plan: FitPlan, series: FmriSeries): Either[FitError, FmriFitResult] =
    for
      _ <- validateSupported(plan)
      masked <- MaskedResponsePlanner.plan(series)
      fitted <- fitPatterns(plan, masked.metadata, masked.patterns.map(_.pattern)) { (childPlan, pattern) =>
        val series = masked.patterns(pattern.id.value).series
        FitInterpreters.forPlan(childPlan).flatMap(_.fit(childPlan, series))
      }
    yield fitted

  def fitChunked(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection,
      chunking: FitChunkingStrategy
  ): Either[FitError, FmriFitResult] =
    for
      _ <- validateSupported(plan)
      masked <- MaskedResponsePlanner.discover(reader, plan, selection, chunking)
      fitted <- fitPatterns(plan, masked, masked.patterns) { (childPlan, pattern) =>
        ChunkedFitExecutor.fit(reader, childPlan, selectionFor(pattern), chunking)
      }
    yield fitted

  def fitChunkedFuture(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection,
      chunking: FitChunkingStrategy,
      parallelism: FitParallelism
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    validateSupported(plan) match
      case Left(error) => Future.successful(Left(error))
      case Right(_) =>
        MaskedResponsePlanner.discover(reader, plan, selection, chunking) match
          case Left(error) => Future.successful(Left(error))
          case Right(masked) =>
            fitPatternsFuture(plan, masked, chunking, parallelism, reader)

  private def fitPatterns(
      plan: FitPlan,
      masked: MaskedResponseMetadata,
      patterns: Vector[ObservationPattern]
  )(
      execute: (FitPlan, ObservationPattern) => Either[FitError, FmriFitResult]
  ): Either[FitError, FmriFitResult] =
    val successes = Vector.newBuilder[ObservationPatternFitResult]
    val exclusions = Vector.newBuilder[VoxelInferenceExclusion]
    exclusions ++= masked.exclusions
    var index = 0
    while index < patterns.length do
      val pattern = patterns(index)
      childPlan(plan, masked, pattern) match
        case Left(error) => return Left(error)
        case Right(strictPlan) =>
          execute(strictPlan, pattern) match
            case Right(result) =>
              val recorded = withPatternProvenance(result, pattern, masked.sourceTimepoints.length)
              successes += ObservationPatternFitResult(pattern, recorded)
              exclusions ++= recorded.fitExclusions
            case Left(error) =>
              omissionStatus(error) match
                case None => return Left(error)
                case Some(status) =>
                  exclusions ++= pattern.sourceVoxels.map(VoxelInferenceExclusion(_, status))
      index += 1
    assemble(plan, masked, successes.result(), exclusions.result())

  private def fitPatternsFuture(
      plan: FitPlan,
      masked: MaskedResponseMetadata,
      chunking: FitChunkingStrategy,
      parallelism: FitParallelism,
      reader: DatasetSeriesReader
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    def loop(
        index: Int,
        successes: Vector[ObservationPatternFitResult],
        exclusions: Vector[VoxelInferenceExclusion]
    ): Future[Either[FitError, FmriFitResult]] =
      if index >= masked.patterns.length then
        Future.successful(assemble(plan, masked, successes, exclusions))
      else
        val pattern = masked.patterns(index)
        childPlan(plan, masked, pattern) match
          case Left(error) => Future.successful(Left(error))
          case Right(strictPlan) =>
            FutureChunkedFitExecutor
              .fit(reader, strictPlan, selectionFor(pattern), chunking, parallelism)
              .flatMap {
                case Right(result) =>
                  val recorded = withPatternProvenance(result, pattern, masked.sourceTimepoints.length)
                  loop(
                    index + 1,
                    successes :+ ObservationPatternFitResult(pattern, recorded),
                    exclusions ++ recorded.fitExclusions
                  )
                case Left(error) =>
                  omissionStatus(error) match
                    case None => Future.successful(Left(error))
                    case Some(status) =>
                      loop(
                        index + 1,
                        successes,
                        exclusions ++ pattern.sourceVoxels.map(VoxelInferenceExclusion(_, status))
                      )
              }
    loop(0, Vector.empty, masked.exclusions)

  private def assemble(
      plan: FitPlan,
      masked: MaskedResponseMetadata,
      successes: Vector[ObservationPatternFitResult],
      rawExclusions: Vector[VoxelInferenceExclusion]
  ): Either[FitError, FmriFitResult] =
    for
      combined <- VoxelInferenceExclusions.combine(rawExclusions)
      exclusions =
        val byVoxel = combined.iterator.map(exclusion => exclusion.voxelIndex -> exclusion).toMap
        masked.sourceVoxels.flatMap(byVoxel.get)
      result <-
        if successes.isEmpty then Left(FitError.AllVoxelsExcluded(exclusions))
        else if successes.length == 1 then
          FmriFitResults.mergeFitExclusions(successes.head.result, exclusions)
        else
          val retainedSet = successes.iterator.flatMap(_.result.voxelIndices).toSet
          val retained = masked.sourceVoxels.filter(retainedSet.contains)
          Right(PatternedFmriFitResult(
            patternResults = successes,
            voxelIndices = retained,
            timepoints = masked.sourceTimepoints,
            summary = plan.summary,
            fitExclusions = exclusions
          ))
    yield result

  private def validateSupported(plan: FitPlan): Either[FitError, Unit] =
    plan.strategy match
      case FitStrategy.RunwiseGeneralizedLeastSquares(_, _) =>
        Left(FitError.UnsupportedMissingDataPolicy(
          s"${MissingDataPolicy.OmitRowsPerVoxel} is not implemented for runwise GLS; unlike row patterns require separately qualified run-local AR receipts"
        ))
      case _ =>
        plan.engine match
          case FitEngine.OrdinaryLeastSquares |
              FitEngine.GeneralizedLeastSquares |
              FitEngine.RunwiseLeastSquares |
              FitEngine.FixedEffects =>
            plan.config.volumeWeighting match
              case VolumeWeighting.Estimated(_) =>
                Left(FitError.UnsupportedVolumeWeighting(
                  "response-derived weights do not yet define how unlike observation patterns share a temporal weight estimate"
                ))
              case _ =>
                plan.config.nuisanceProjection match
                  case NuisanceProjection.MatrixProjection(_, _) =>
                    Left(FitError.UnsupportedMissingDataPolicy(
                      "voxel-specific row omission does not yet subset an explicit nuisance-projection matrix"
                    ))
                  case NuisanceProjection.Disabled => Right(())
          case unsupported =>
            Left(FitError.UnsupportedMissingDataPolicy(
              s"${MissingDataPolicy.OmitRowsPerVoxel} is not implemented for $unsupported"
            ))

  private def childPlan(
      plan: FitPlan,
      masked: MaskedResponseMetadata,
      pattern: ObservationPattern
  ): Either[FitError, FitPlan] =
    val retainedTimepoints = pattern.sourceTimepoints.toSet
    val autocorrelation = plan.config.autocorrelation.copy(
      censoredTimepoints = plan.config.autocorrelation.censoredTimepoints.filter(retainedTimepoints.contains)
    )
    val weighting = plan.config.volumeWeighting match
      case VolumeWeighting.Fixed(weights, FixedWeightAlignment.SelectedRows) =>
        if weights.length != masked.sourceTimepoints.length then
          return Left(FitError.InvalidVolumeWeights(
            s"selected-row length ${weights.length} does not match outer selected response rows ${masked.sourceTimepoints.length}"
          ))
        VolumeWeighting.Fixed(pattern.rows.map(weights), FixedWeightAlignment.SelectedRows)
      case other => other
    val config = plan.config.copy(
      missingData = MissingDataPolicy.Error,
      autocorrelation = autocorrelation,
      volumeWeighting = weighting
    )
    FitPlan
      .makeLegacy(plan.model, plan.engine, config)
      .left
      .map(error => FitError.UnsupportedMissingDataPolicy(error.message))

  private def selectionFor(pattern: ObservationPattern): DataSelection =
    DataSelection(
      time = IndexSelection.indices(pattern.sourceTimepoints*),
      voxels = IndexSelection.indices(pattern.sourceVoxels*)
    )

  private def omissionStatus(error: FitError): Option[VoxelFitStatus] =
    error match
      case FitError.NonPositiveResidualDegreesOfFreedom(_) | FitError.EmptyRunPartition(_) =>
        Some(VoxelFitStatus.InsufficientResidualDegreesOfFreedom)
      case FitError.RankDeficientDesign(_) |
          FitError.StructuralRankDeficientDesign(_) |
          FitError.SingularDesign(_) =>
        Some(VoxelFitStatus.RankDeficientObservedDesign)
      case FitError.RunwiseFitFailed(_, cause) => omissionStatus(cause)
      case _ => None

  private def withPatternProvenance(
      result: FmriFitResult,
      pattern: ObservationPattern,
      outerRowCount: Int
  ): FmriFitResult =
    val detail =
      s"observation pattern ${pattern.id.value} retained ${pattern.rows.length}/$outerRowCount selected rows for voxels ${pattern.sourceVoxels.mkString(",")}"
    val provenance = result.preparationProvenance.map { current =>
      current.copy(records = current.records.map {
        case ResponsePreparationRecord(
              ResponsePreparationStep.MissingData(MissingDataPolicy.Error),
              _
            ) =>
          ResponsePreparationRecord(
            ResponsePreparationStep.MissingData(MissingDataPolicy.OmitRowsPerVoxel),
            ResponsePreparationDisposition.Applied(detail)
          )
        case record => record
      })
    }
    result match
      case dense: DenseFmriFitResult => dense.copy(preparationProvenance = provenance)
      case runwise: RunwiseFmriFitResult => runwise.copy(preparationProvenance = provenance)
      case fixed: FixedEffectsFmriFitResult => fixed.copy(preparationProvenance = provenance)
      case lss: LssFmriFitResult => lss.copy(preparationProvenance = provenance)
      case patterned: PatternedFmriFitResult => patterned
