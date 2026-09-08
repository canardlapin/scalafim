package scalafim.fmri.fit

import gale.backend.Backend
import gale.linalg.Matrix
import scalafim.dataset.{DataSelection, DatasetSeriesReader, FmriDataset}
import scalafim.fmri.design.{CoefficientAxis, ColumnId, DesignSchema, StructuralColumn}
import scalafim.fmri.model.{FitEngine, FitPlan, MissingDataPolicy, VolumeWeighting}

enum EstimateOutputId:
  case Coefficient(column: ColumnId)
  case Contrast(contrast: ContrastId)

/** Output order is intentional. Selecting a nuisance column is an explicit
  * request to retain it; unselected nuisance terms still enter the model.
  */
enum EstimateOutput:
  case Coefficient(column: ColumnId)
  case Contrast(contrast: StructuralTContrast)

  def id: EstimateOutputId = this match
    case Coefficient(column) => EstimateOutputId.Coefficient(column)
    case Contrast(contrast) => EstimateOutputId.Contrast(contrast.id)

/** Primary output and uncertainty selection. `retainRunCoefficients` names
  * additional source columns to return separately from each selected fitted
  * run where they occur. It defaults to off and never requests run uncertainty.
  * Run retention requires the runwise fixed-effects executor; shared OLS
  * nuisance coefficients can be selected as ordinary primary outputs.
  */
final class FirstLevelEstimateRequest private (
    val outputs: Vector[EstimateOutput],
    val uncertainty: EstimateUncertaintyRequest,
    val retainRunCoefficients: Vector[ColumnId]
):
  def compile(
      schema: DesignSchema,
      rankTolerance: OlsRankTolerance = OlsRankTolerance.ScaleAware
  ): Either[FitError, CompiledEstimateSelection] =
    FirstLevelEstimates.compileOutputs(schema, this, rankTolerance)

object FirstLevelEstimateRequest:
  def make(
      outputs: Vector[EstimateOutput],
      uncertainty: EstimateUncertaintyRequest = EstimateUncertaintyRequest.None,
      retainRunCoefficients: Vector[ColumnId] = Vector.empty
  ): Either[FitError, FirstLevelEstimateRequest] =
    if outputs.isEmpty then
      Left(FitError.InvalidFitAxis("estimate outputs", "select at least one coefficient or contrast"))
    else if outputs.map(_.id).distinct.size != outputs.size then
      Left(FitError.InvalidFitAxis("estimate outputs", "output identities must be unique"))
    else if retainRunCoefficients.distinct.size != retainRunCoefficients.size then
      Left(FitError.InvalidFitAxis("retained run coefficients", "source column identities must be unique"))
    else Right(new FirstLevelEstimateRequest(outputs, uncertainty, retainRunCoefficients))

enum EstimateOutputMetadata:
  case Coefficient(column: StructuralColumn)
  case Contrast(hypothesis: HypothesisMetadata)

  def id: EstimateOutputId = this match
    case Coefficient(column) => EstimateOutputId.Coefficient(column.id)
    case Contrast(hypothesis) => EstimateOutputId.Contrast(hypothesis.id)

/** Structural output selection, reusable with the same scientific design.
  * Alignment to a pooled axis may omit only columns with zero readout weight.
  */
final class CompiledEstimateSelection private[fit] (
    val request: FirstLevelEstimateRequest,
    val coefficientAxis: CoefficientAxis,
    val outputs: Vector[EstimateOutputMetadata],
    val readout: CoefficientReadout
):
  private[fit] def alignTo(axis: CoefficientAxis): Either[FitError, CoefficientReadout] =
    if axis.designFingerprint != coefficientAxis.designFingerprint then
      Left(FitError.InvalidFitAxis("estimate selection", "design fingerprint differs from the fitted coefficient axis"))
    else
      val positions = axis.columnIds.map(coefficientAxis.columnIds.indexOf)
      val included = positions.toSet
      if positions.exists(_ < 0) then
        Left(FitError.InvalidFitAxis("estimate selection", "fitted coefficient axis contains an unknown column"))
      else if coefficientAxis.columnIds.indices.exists(col =>
          !included(col) && (0 until readout.outputs).exists(row => readout.weights(row, col) != 0.0)) then
        Left(FitError.InvalidFitAxis("estimate selection", "a requested output depends on a coefficient absent from the fitted axis"))
      else CoefficientReadout.fromMatrix(Matrix.tabulate(readout.outputs, positions.length)((row, col) => readout.weights(row, positions(col))))

/** A delivered block owns its output matrices. The executor retains no prior
  * blocks: callers choose a bounded writer or explicitly collect results.
  */
final case class FirstLevelEstimateBlock private[fit] (
    ordinal: ChunkOrdinal,
    voxelIndices: Vector[Int],
    result: OlsEstimateResult
)

enum EstimateExecutionOutcome:
  case Completed(chunks: Int, voxels: Int)
  case Cancelled(completedChunks: Int, completedVoxels: Int)

/** Prepared only from the model and selected axes. No response is read during
  * preparation. This first compiled route admits shared-design, finite-input
  * OLS; other models return an explicit unsupported error before data reads.
  *
  * The model axis retains basis identity (including physical FIR coordinates),
  * while `chunks.timepoints` identifies the actual rows used by the solver.
  * Contrast metadata describes structural alignment; `diagnostics` reports
  * numerical rank on these actual selected rows.
  */
final class FirstLevelEstimatePlan private[fit] (
    val fitPlan: FitPlan,
    val request: FirstLevelEstimateRequest,
    val coefficientAxis: CoefficientAxis,
    val outputs: Vector[EstimateOutputMetadata],
    val chunks: FitChunkPlan,
    val preparation: ResponsePreparationProvenance,
    private val solver: OlsEstimatePlan
):
  def diagnostics: OlsDiagnostics = solver.diagnostics
  def readout: CoefficientReadout = solver.request.readout
  def timepoints: Vector[Int] = chunks.timepoints
  def maxBlockVoxels: Int = chunks.iterator.map(_.voxelIndices.length).max
  def computesResidualVariance: Boolean = request.uncertainty != EstimateUncertaintyRequest.None

  /** Cancellation is checked before each read and before each delivery. A sink
    * failure stops execution before the next read. Input reader, sink and
    * backend lifetimes remain owned by the caller.
    */
  def foreachBlock(
      reader: DatasetSeriesReader,
      consume: FirstLevelEstimateBlock => Either[FitError, Unit],
      cancelled: () => Boolean = () => false
  )(using Backend): Either[FitError, EstimateExecutionOutcome] =
    EstimateBlockExecutor.foreachBlock(fitPlan, chunks, reader,
      (chunk, response) => solver.estimate(response).map(fit => FirstLevelEstimateBlock(chunk.ordinal, chunk.voxelIndices, fit)),
      consume, cancelled)

private[fit] object EstimateBlockExecutor:
  def foreachBlock[A](
      fitPlan: FitPlan,
      chunks: FitChunkPlan,
      reader: DatasetSeriesReader,
      evaluate: (FitChunkSpec, ResponseBlock) => Either[FitError, A],
      consume: A => Either[FitError, Unit],
      cancelled: () => Boolean
  ): Either[FitError, EstimateExecutionOutcome] =
    FirstLevelEstimates.validateReader(reader.dataset, fitPlan.model.dataset).flatMap { _ =>
      var completed = 0
      var voxels = 0
      val iterator = chunks.iterator
      var failure = Option.empty[FitError]
      var stopped = false
      while iterator.hasNext && failure.isEmpty && !stopped do
        if cancelled() then stopped = true
        else
          val chunk = iterator.next()
          val result = for
            series <- reader.seriesEither(chunk.selection).left.map(FitChunkPlan.mapDatasetError)
            _ <-
              if series.timepoints == chunk.timepoints && series.voxelIndices == chunk.voxelIndices &&
                  series.shape == fitPlan.model.dataset.shape then Right(())
              else Left(FitError.InvalidFitAxis("estimate response", "reader returned different time, voxel or shape identities"))
            response <- MatrixAdapters.responseBlock(series, MissingDataPolicy.Error)
            value <- evaluate(chunk, response.response)
          yield value
          result match
            case Left(error) => failure = Some(error)
            case Right(block) =>
              if cancelled() then stopped = true
              else consume(block) match
                case Left(error) => failure = Some(error)
                case Right(_) =>
                  completed += 1
                  voxels += chunk.voxelIndices.length
      failure match
        case Some(error) => Left(error)
        case None if stopped => Right(EstimateExecutionOutcome.Cancelled(completed, voxels))
        case None => Right(EstimateExecutionOutcome.Completed(completed, voxels))
    }

object FirstLevelEstimates:
  /** Prepare selected products without reading response data. `blockSize`
    * bounds each dataset request; backend and sink memory remain their owners'
    * responsibility. No full-fit result is constructed behind this interface.
    */
  def prepare(
      plan: FitPlan,
      request: FirstLevelEstimateRequest,
      blockSize: ChunkSize,
      selection: DataSelection = DataSelection.All,
      policy: OlsSolvePolicy = OlsSolvePolicy.Default
  ): Either[FitError, FirstLevelEstimatePlan] =
    for
      preparation <- admittedPreparation(plan, FitEngine.OrdinaryLeastSquares)
      _ <- if request.retainRunCoefficients.isEmpty then Right(())
           else Left(FitError.UnsupportedEngine("Run coefficient retention requires independently fitted runs; select shared coefficients directly for shared OLS"))
      schema <- plan.model.designSchema.toRight(FitError.InvalidFitAxis("estimate design", "a structural design schema is required"))
      chunks <- FitChunkPlan.fromSelection(plan, selection, FitChunkingStrategy.ByVoxelCount(blockSize))
      design <- MatrixAdapters.designMatrix(plan.model, chunks.timepoints)
      selected <- request.compile(schema, policy.rankTolerance)
      solver <- Ols.prepareEstimates(design, OlsEstimateRequest(selected.readout, request.uncertainty), policy)
    yield new FirstLevelEstimatePlan(plan, request, schema.coefficientAxis, selected.outputs, chunks, preparation, solver)

  private[fit] def admittedPreparation(plan: FitPlan, engine: FitEngine): Either[FitError, ResponsePreparationProvenance] =
    val preparation = ResponsePreparationPlan.fromPlan(plan)
    if plan.engine != engine then
      Left(FitError.UnsupportedEngine(s"${plan.engine}: this compiled selective route requires $engine"))
    else if plan.config.missingData != MissingDataPolicy.Error then
      Left(FitError.UnsupportedMissingDataPolicy("compiled selective execution currently requires finite selected responses"))
    else if preparation.volumeWeighting != VolumeWeighting.Disabled then
      Left(FitError.UnsupportedVolumeWeighting("compiled selective execution does not yet admit temporal weights"))
    else if preparation.provenance.deferred.nonEmpty then
      Left(FitError.UnsupportedLeastSquaresPolicy(preparation.provenance.deferred.map(_.disposition.detailText).mkString("; ")))
    else Right(preparation.provenance)

  private[fit] def compileOutputs(
      schema: DesignSchema,
      request: FirstLevelEstimateRequest,
      tolerance: OlsRankTolerance
  ): Either[FitError, CompiledEstimateSelection] =
    val outputs = request.outputs
    val axis = schema.coefficientAxis
    request.retainRunCoefficients.find(id => !axis.columnIds.contains(id)) match
      case Some(id) => return Left(FitError.InvalidFitAxis("retained run coefficients", s"unknown structural source column '${id.value}'"))
      case None => ()
    val weights = Matrix.newBuilder(outputs.length, axis.predictors)
    val metadata = Vector.newBuilder[EstimateOutputMetadata]
    var row = 0
    while row < outputs.length do
      outputs(row) match
        case EstimateOutput.Coefficient(id) =>
          val index = axis.columnIds.indexOf(id)
          if index < 0 then return Left(FitError.InvalidFitAxis("estimate coefficient", s"unknown structural column '${id.value}'"))
          weights(row, index) = 1.0
          metadata += EstimateOutputMetadata.Coefficient(axis.columns(index))
        case EstimateOutput.Contrast(contrast) =>
          contrast.compile(schema, tolerance) match
            case Left(error) => return Left(error)
            case Right(compiled) =>
              var col = 0
              while col < axis.predictors do
                weights(row, col) = compiled.weights(0, col)
                col += 1
              metadata += EstimateOutputMetadata.Contrast(compiled.metadata)
      row += 1
    CoefficientReadout.fromMatrix(weights.result()).map(readout =>
      new CompiledEstimateSelection(request, axis, metadata.result(), readout))

  private[fit] def validateReader(actual: FmriDataset, expected: FmriDataset): Either[FitError, Unit] =
    if actual.id == expected.id && actual.shape == expected.shape &&
        actual.voxelDomain.indices == expected.voxelDomain.indices && actual.timeAxis.blocks == expected.timeAxis.blocks then Right(())
    else Left(FitError.InvalidFitAxis("estimate dataset", "reader does not match the prepared dataset identity, spatial domain or time axis"))
