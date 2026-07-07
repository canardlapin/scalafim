package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, FmriSeries}
import scalafim.fmri.design.event.{ConvolvedTerm, EventTermColumnRole}
import scalafim.fmri.model.{FitEngine, FitPlan}

object FitPlanExecutor:
  def fit(
      plan: FitPlan,
      selection: DataSelection = DataSelection.All
  ): Either[FitError, FmriFitResult] =
    plan.engine match
      case FitEngine.OrdinaryLeastSquares =>
        val series = plan.model.dataset.series(selection)
        for
          input <- fitBlockInput(plan, series)
          dense <- FitKernel.fitDense(input, plan.engine, plan.config)
        yield denseResult(plan, dense)

      case FitEngine.LeastSquaresSeparate =>
        val series = plan.model.dataset.series(selection)
        for
          input <- fitBlockInput(plan, series, lssDesign = Some(lssExecutionDesign(plan, series.timepoints)))
          lss <- FitKernel.fitLss(input)
        yield LssFmriFitResult(
          coefficients = lss.coefficients,
          trialNames = lss.trialNames,
          lssDiagnostics = lss.diagnostics,
          voxelIndices = lss.voxelIndices,
          timepoints = lss.timepoints,
          engine = plan.engine,
          summary = plan.summary
        )

      case FitEngine.RunwiseLeastSquares =>
        val series = plan.model.dataset.series(selection)
        val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
        for
          input <- fitBlockInput(plan, series, partitions = partitions)
          runwise <- FitKernel.fitRunwise(input)
        yield RunwiseFmriFitResult(
          runs = runwise.runs,
          columnNames = plan.model.columnNames,
          voxelIndices = runwise.voxelIndices,
          timepoints = runwise.timepoints,
          engine = plan.engine,
          summary = plan.summary
        )

      case FitEngine.GeneralizedLeastSquares =>
        val series = plan.model.dataset.series(selection)
        val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
        for
          input <- fitBlockInput(plan, series, partitions = partitions)
          dense <- FitKernel.fitDense(input, plan.engine, plan.config)
        yield denseResult(plan, dense)

      case other =>
        Left(FitError.UnsupportedEngine(other.toString))

  def unsafeFit(
      plan: FitPlan,
      selection: DataSelection = DataSelection.All
  ): FmriFitResult =
    fit(plan, selection).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def fitBlockInput(
      plan: FitPlan,
      series: FmriSeries,
      partitions: Vector[RunPartition] = Vector.empty,
      lssDesign: Option[Either[FitError, LssBlockDesign]] = None
  ): Either[FitError, FitBlockInput] =
    for
      design <- MatrixAdapters.designMatrix(plan.model, series.timepoints)
      response <- MatrixAdapters.responseBlock(series)
      lss <- lssDesign match
        case None        => Right(None)
        case Some(value) => value.map(design => Some(design): Option[LssBlockDesign])
    yield FitBlockInput(
      design = design,
      response = response,
      voxelIndices = series.voxelIndices,
      timepoints = series.timepoints,
      partitions = partitions,
      lssDesign = lss
    )

  private def denseResult(plan: FitPlan, block: DenseFitBlockResult): DenseFmriFitResult =
    DenseFmriFitResult(
      coefficients = block.coefficients,
      standardErrors = block.standardErrors,
      normalizedCovariance = block.normalizedCovariance,
      residualVariance = block.residualVariance,
      residualDegreesOfFreedom = block.residualDegreesOfFreedom,
      columnNames = plan.model.columnNames,
      voxelIndices = block.voxelIndices,
      timepoints = block.timepoints,
      engine = block.engine,
      summary = plan.summary,
      autocorrelation = block.autocorrelation
    )

  private def lssExecutionDesign(plan: FitPlan, timepoints: Vector[Int]): Either[FitError, LssBlockDesign] =
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
