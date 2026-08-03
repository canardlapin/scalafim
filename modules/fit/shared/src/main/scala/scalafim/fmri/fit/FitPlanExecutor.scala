package scalafim.fmri.fit

import scalafim.dataset.{
  DataSelection,
  DatasetSeriesReader,
  FmriSeries,
  SynchronousFmriDataset
}
import scalafim.fmri.design.event.{ConvolvedTerm, EventTermColumnRole}
import scalafim.fmri.model.FitPlan

import scala.concurrent.{ExecutionContext, Future}

object FitPlanExecutor:
  /** Synchronous compatibility overload. */
  def fit(
      plan: FitPlan
  ): Either[FitError, FmriFitResult] =
    fit(plan, DataSelection.All)

  /** Synchronous compatibility overload. */
  def fit(
      plan: FitPlan,
      selection: DataSelection
  ): Either[FitError, FmriFitResult] =
    legacyReader(plan).flatMap(fit(_, plan, selection))

  def fit(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection = DataSelection.All
  ): Either[FitError, FmriFitResult] =
    for
      _ <-
        if reader.dataset.id == plan.model.dataset.id then Right(())
        else
          Left(FitError.InvalidFitAxis(
            "dataset reader",
            s"reader dataset '${reader.dataset.id.value}' does not match model dataset '${plan.model.dataset.id.value}'"
          ))
      interpreter <- FitInterpreters.forPlan(plan)
      series <- reader
        .seriesEither(selection)
        .left
        .map(FitChunkPlan.mapDatasetError)
      result <- interpreter.fit(plan, series)
    yield result

  def unsafeFit(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection = DataSelection.All
  ): FmriFitResult =
    fit(reader, plan, selection)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Synchronous compatibility overload. */
  def unsafeFit(
      plan: FitPlan
  ): FmriFitResult =
    unsafeFit(plan, DataSelection.All)

  /** Synchronous compatibility overload. */
  def unsafeFit(
      plan: FitPlan,
      selection: DataSelection
  ): FmriFitResult =
    fit(plan, selection)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Synchronous compatibility overload. */
  def fitChunked(
      plan: FitPlan
  ): Either[FitError, FmriFitResult] =
    fitChunked(
      plan,
      DataSelection.All,
      FitChunkingStrategy.WholeSelection
    )

  /** Synchronous compatibility overload. */
  def fitChunked(
      plan: FitPlan,
      selection: DataSelection
  ): Either[FitError, FmriFitResult] =
    fitChunked(plan, selection, FitChunkingStrategy.WholeSelection)

  /** Synchronous compatibility overload. */
  def fitChunked(
      plan: FitPlan,
      chunking: FitChunkingStrategy
  ): Either[FitError, FmriFitResult] =
    fitChunked(plan, DataSelection.All, chunking)

  /** Synchronous compatibility overload. */
  def fitChunked(
      plan: FitPlan,
      selection: DataSelection,
      chunking: FitChunkingStrategy
  ): Either[FitError, FmriFitResult] =
    legacyReader(plan).flatMap(fitChunked(_, plan, selection, chunking))

  def fitChunked(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection = DataSelection.All,
      chunking: FitChunkingStrategy = FitChunkingStrategy.WholeSelection
  ): Either[FitError, FmriFitResult] =
    ChunkedFitExecutor.fit(reader, plan, selection, chunking)

  def fitChunkedFuture(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection = DataSelection.All,
      chunking: FitChunkingStrategy = FitChunkingStrategy.WholeSelection,
      parallelism: FitParallelism = FitParallelism.unbounded
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    FutureChunkedFitExecutor.fit(
      reader,
      plan,
      selection,
      chunking,
      parallelism
    )

  /** Synchronous compatibility overload. */
  def fitChunkedFuture(
      plan: FitPlan
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    fitChunkedFuture(
      plan,
      DataSelection.All,
      FitChunkingStrategy.WholeSelection,
      FitParallelism.unbounded
    )

  /** Synchronous compatibility overload. */
  def fitChunkedFuture(
      plan: FitPlan,
      selection: DataSelection
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    fitChunkedFuture(
      plan,
      selection,
      FitChunkingStrategy.WholeSelection,
      FitParallelism.unbounded
    )

  /** Synchronous compatibility overload. */
  def fitChunkedFuture(
      plan: FitPlan,
      selection: DataSelection,
      chunking: FitChunkingStrategy
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    fitChunkedFuture(
      plan,
      selection,
      chunking,
      FitParallelism.unbounded
    )

  /** Synchronous compatibility overload. */
  def fitChunkedFuture(
      plan: FitPlan,
      selection: DataSelection,
      chunking: FitChunkingStrategy,
      parallelism: FitParallelism
  )(using ExecutionContext): Future[Either[FitError, FmriFitResult]] =
    legacyReader(plan) match
      case Left(error) =>
        Future.successful(Left(error))
      case Right(reader) =>
        fitChunkedFuture(reader, plan, selection, chunking, parallelism)

  private def legacyReader(
      plan: FitPlan
  ): Either[FitError, SynchronousFmriDataset] =
    SynchronousFmriDataset
      .readerFor(plan.model.dataset)
      .left
      .map(error =>
        FitError.InvalidFitAxis("dataset reader", error.message)
      )

  private[fit] def fitBlockInput(
      plan: FitPlan,
      series: FmriSeries,
      partitions: Vector[RunPartition] = Vector.empty,
      lssDesign: Option[Either[FitError, LssBlockDesign]] = None
  ): Either[FitError, FitBlockInput] =
    preparedFitBlockInput(plan, series, partitions, lssDesign).map(_.input)

  private[fit] def preparedFitBlockInput(
      plan: FitPlan,
      series: FmriSeries,
      partitions: Vector[RunPartition] = Vector.empty,
      lssDesign: Option[Either[FitError, LssBlockDesign]] = None
  ): Either[FitError, PreparedFitBlockInput] =
    for
      design <- MatrixAdapters.designMatrix(plan.model, series.timepoints)
      response <- MatrixAdapters.responseBlock(series)
      lss <- lssDesign match
        case None        => Right(None)
        case Some(value) => value.map(design => Some(design): Option[LssBlockDesign])
      input = FitBlockInput(
        design = design,
        response = response,
        voxelIndices = series.voxelIndices,
        timepoints = series.timepoints,
        partitions = partitions,
        lssDesign = lss
      )
      prepared <- ResponsePreparationPlan.fromPlan(plan).prepare(input)
    yield prepared

  private[fit] def denseResult(plan: FitPlan, block: DenseFitBlockResult): DenseFmriFitResult =
    DenseFmriFitResult(
      coefficients = block.coefficients,
      inference = block.inference,
      residualVariance = block.residualVariance,
      residualDegreesOfFreedom = block.residualDegreesOfFreedom,
      columnNames = plan.model.columnNames,
      voxelIndices = block.voxelIndices,
      timepoints = block.timepoints,
      engine = block.engine,
      summary = plan.summary,
      olsDiagnostics = block.olsDiagnostics,
      autocorrelation = block.autocorrelation,
      robustDiagnostics = block.robustDiagnostics
    )

  private[fit] def lssExecutionDesign(plan: FitPlan, timepoints: Vector[Int]): Either[FitError, LssBlockDesign] =
    for
      trialTerm <- selectTrialwiseTerm(plan)
      termCols <- trialColumns(plan, trialTerm)
      trialMatrix <- LssTrialDesign.fromMatrix(
        MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, timepoints, termCols.trials),
        termCols.trials.map(plan.model.eventModel.columnNames)
      )
      fixed <- fixedDesign(plan, timepoints, excludedEventCols = termCols.trials ++ termCols.aggregates)
      prepared <- LeastSquaresSeparate.prepare(
        trials = trialMatrix,
        fixed = fixed,
        options = LssOptions(eps = plan.config.lss.eps, rankTol = plan.config.lss.rankTol)
      )
    yield LssBlockDesign(prepared)

  private final case class TrialwiseTerm(key: String, index: Int, term: ConvolvedTerm)
  private final case class TrialColumnSelection(trials: Vector[Int], aggregates: Vector[Int])

  private def selectTrialwiseTerm(plan: FitPlan): Either[FitError, TrialwiseTerm] =
    val trialwiseTerms =
      plan.model.eventModel.terms.zipWithIndex.collect {
        case ((key, term: ConvolvedTerm), index) if term.isTrialwise =>
          TrialwiseTerm(key, index, term)
      }

    plan.config.lss.trialTerm match
      case Some(requested) =>
        trialwiseTerms.find(_.key == requested).toRight(FitError.UnknownLssTrialTerm(requested))
      case None =>
        trialwiseTerms match
          case Vector()       => Left(FitError.MissingLssTrialTerm)
          case Vector(single) => Right(single)
          case many           => Left(FitError.AmbiguousLssTrialTerms(many.map(_.key)))

  private def trialColumns(plan: FitPlan, selected: TrialwiseTerm): Either[FitError, TrialColumnSelection] =
    if selected.term.hrf.nbasis != 1 then
      Left(FitError.UnsupportedLssDesign(s"trialwise term '${selected.key}' has ${selected.term.hrf.nbasis} HRF basis columns; core LSS supports scalar trial regressors only"))
    else
      val nTrials = selected.term.term.onsets.length
      if nTrials <= 0 then Left(FitError.EmptyDesign)
      else
        val (start, endExcl) = plan.model.eventModel.termSpans(selected.index)
        val spanCols = (start until endExcl).toVector
        val roles = selected.term.resolvedColumnRoles
        if roles.length != spanCols.length then
          Left(
            FitError.UnsupportedLssDesign(
              s"trialwise term '${selected.key}' metadata has ${roles.length} roles for ${spanCols.length} design columns"
            )
          )
        else
          val trialCols = roles.zip(spanCols).collect {
            case (EventTermColumnRole.Trial, col) => col
          }
          val aggregateCols = roles.zip(spanCols).collect {
            case (EventTermColumnRole.TrialAggregate, col) => col
          }
          if trialCols.length != nTrials then
            Left(
              FitError.UnsupportedLssDesign(
                s"trialwise term '${selected.key}' exposes ${trialCols.length} trial columns for $nTrials events"
              )
            )
          else Right(TrialColumnSelection(trials = trialCols, aggregates = aggregateCols))

  private def fixedDesign(plan: FitPlan, timepoints: Vector[Int], excludedEventCols: Vector[Int]): Either[FitError, LssFixedDesign] =
    val excludedSet = excludedEventCols.toSet
    val eventFixedCols =
      plan.model.eventModel.columnNames.indices.filterNot(excludedSet.contains).toVector

    val eventFixed =
      MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, timepoints, eventFixedCols)
    val baselineFixed =
      MatrixAdapters.fromHrfMatrixRows(plan.model.baselineModel.designMatrix, timepoints)
    val fixedMatrix = MatrixAdapters.bindColumns(eventFixed, baselineFixed)
    val fixedNames = eventFixedCols.map(plan.model.eventModel.columnNames) ++ plan.model.baselineModel.columnNames

    if fixedMatrix.cols == 0 then Right(LssFixedDesign.empty(timepoints.length))
    else LssFixedDesign.fromMatrix(fixedMatrix, fixedNames)
