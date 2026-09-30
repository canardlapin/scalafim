package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.{DataSelection, DatasetId, DatasetSeriesReader, TimepointSelection, VoxelSelection}
import scalafim.fmri.ar.{ArmaCoefficients, CoefficientScope, NoisePooling, TimeSegment, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.DesignFingerprint
import scalafim.fmri.model.{FitEngine, FitPlan, FitStrategy, MissingDataPolicy}

/** A trace names the space in which its three signals are reported. */
enum FitTraceSpace:
  case Original
  case ModelPrepared
  case Transformed

/** Autocorrelation uses scan-index lags and pairs only within one uninterrupted
  * whitening segment. The normalized dot product is uncentered: residuals are
  * already model centered. A lag with no eligible or nonzero pairs is absent.
  */
enum FitTraceGapPolicy:
  case WithinContiguousSegment

final case class FitTraceAcf(lagScans: Int, pairs: Int, normalizedDot: Option[Double])

final case class FitTraceIdentity(
    datasetId: DatasetId,
    designFingerprint: Option[DesignFingerprint],
    timepoints: Vector[Int],
    runIndices: Vector[Int],
    voxelIndices: Vector[Int],
    engine: FitEngine,
    preparation: Option[ResponsePreparationProvenance],
    autocorrelation: Option[ArDiagnostics]
)

final case class FitTraceRequest(
    identity: FitTraceIdentity,
    voxelIndex: Int,
    rows: Vector[Int],
    space: FitTraceSpace,
    maxAcfLagScans: Int,
    gapPolicy: FitTraceGapPolicy = FitTraceGapPolicy.WithinContiguousSegment
)

final case class FitTraceResult(
    identity: FitTraceIdentity,
    voxelIndex: Int,
    rows: Vector[Int],
    timepoints: Vector[Int],
    runIndices: Vector[Int],
    space: FitTraceSpace,
    observed: Vector[Double],
    fitted: Vector[Double],
    residual: Vector[Double],
    acf: Vector[FitTraceAcf],
    gapPolicy: FitTraceGapPolicy
)

enum FitTraceFailure:
  case Unavailable(reason: String)
  case Refused(reason: String)
  case Fit(error: FitError)

/** This handle is made by a single bounded read and fit. It keeps exactly that
  * prepared selected response and design, so later trace requests never refit
  * or reread a possibly changed source. Ordinary historical estimates have no
  * such handle and cannot be diagnosed retroactively.
  */
final class CapturedFitTrace private[fit] (
    val fit: DenseFmriFitResult,
    val identity: FitTraceIdentity,
    private val design: DMat,
    private val observed: DMat,
    private val originalAvailable: Vector[Boolean]
):
  def trace(request: FitTraceRequest): Either[FitTraceFailure, FitTraceResult] =
    if request.identity != identity then Left(FitTraceFailure.Refused("fit trace identity does not match the captured source and model"))
    else if request.maxAcfLagScans < 0 || request.maxAcfLagScans >= observed.rows then
      Left(FitTraceFailure.Refused("ACF lag must be within the captured row budget"))
    else if request.rows.isEmpty || request.rows.distinct.length != request.rows.length ||
        request.rows != request.rows.sorted || request.rows.exists(row => row < 0 || row >= observed.rows) then
      Left(FitTraceFailure.Refused("trace rows must be nonempty, unique, ordered selected-row positions"))
    else
      val voxel = identity.voxelIndices.indexOf(request.voxelIndex)
      if voxel < 0 then Left(FitTraceFailure.Refused("voxel was not present in the captured fit"))
      else if request.space == FitTraceSpace.Original && !originalAvailable(voxel) then
        Left(FitTraceFailure.Unavailable("raw original signal cannot be recovered from this response preparation"))
      else
        val selectedObserved = Matrix.tabulate(observed.rows, 1)((row, _) => observed(row, voxel))
        val selectedCoefficients = Matrix.tabulate(design.cols, 1)((row, _) => fit.coefficients(row, voxel))
        val residual = Gls.residualMatrix(design, selectedObserved, selectedCoefficients)
        val fitted = Matrix.tabulate(observed.rows, 1)((row, _) => selectedObserved(row, 0) - residual(row, 0))
        val transformed = request.space match
          case FitTraceSpace.Original | FitTraceSpace.ModelPrepared => Right((selectedObserved, fitted, residual))
          case FitTraceSpace.Transformed =>
            fit.autocorrelation match
              case None => Right((selectedObserved, fitted, residual)) // OLS has the identity transform.
              case Some(ar) =>
                whiteningFor(ar, voxel).flatMap { plan =>
                  for
                    y <- WhiteningTransform.matrix(plan, selectedObserved).left.map(e => FitTraceFailure.Refused(e.message))
                    xBeta <- WhiteningTransform.matrix(plan, fitted).left.map(e => FitTraceFailure.Refused(e.message))
                    e <- WhiteningTransform.matrix(plan, residual).left.map(e => FitTraceFailure.Refused(e.message))
                  yield (y, xBeta, e)
                }
        transformed.map { (y, xBeta, e) =>
          val segments = segmentIds
          val rows = request.rows
          FitTraceResult(
            identity,
            request.voxelIndex,
            rows,
            rows.map(identity.timepoints),
            rows.map(identity.runIndices),
            request.space,
            rows.map(y(_, 0)),
            rows.map(xBeta(_, 0)),
            rows.map(e(_, 0)),
            acf(rows, e, segments, request.maxAcfLagScans),
            request.gapPolicy
          )
        }

  private def segmentIds: Vector[Int] =
    val out = Array.fill(observed.rows)(-1)
    fit.autocorrelation match
      case Some(ar) =>
        ar.whitening.segments.zipWithIndex.foreach { (segment, id) =>
          (segment.startRow until segment.endRowExclusive).foreach(row => out(row) = id)
        }
      case None =>
        var segment = -1
        var row = 0
        while row < observed.rows do
          if row == 0 || identity.runIndices(row) != identity.runIndices(row - 1) ||
              identity.timepoints(row) != identity.timepoints(row - 1) + 1 then
            segment += 1
          out(row) = segment
          row += 1
    out.toVector

  private def acf(rows: Vector[Int], residual: DMat, segments: Vector[Int], maxLag: Int): Vector[FitTraceAcf] =
    val bySegmentAndScan = rows.iterator.map { row =>
      (segments(row), identity.timepoints(row)) -> row
    }.toMap
    val scale = rows.iterator.map(row => math.abs(residual(row, 0))).max
    (0 to maxLag).toVector.map { lag =>
      var dot = 0.0
      var leftSq = 0.0
      var rightSq = 0.0
      var pairs = 0
      var i = 0
      while i < rows.length do
        val right = rows(i)
        bySegmentAndScan.get((segments(right), identity.timepoints(right) - lag)) match
          case Some(left) =>
            val a = if scale == 0.0 then 0.0 else residual(left, 0) / scale
            val b = if scale == 0.0 then 0.0 else residual(right, 0) / scale
            dot += a * b
            leftSq += a * a
            rightSq += b * b
            pairs += 1
          case None => ()
        i += 1
      val value = if pairs == 0 || leftSq == 0.0 || rightSq == 0.0 then None
        else Some(dot / (math.sqrt(leftSq) * math.sqrt(rightSq)))
      FitTraceAcf(lag, pairs, value)
    }

  private def whiteningFor(ar: ArDiagnostics, voxel: Int): Either[FitTraceFailure, WhiteningPlan] =
    val segments = ar.whitening.segments.map(s => TimeSegment(s.startRow, s.endRowExclusive, s.runIndex))
    val perRun = ar.runs.map { run =>
      val phi = if ar.sharedNormalizedCovariance then run.phi
        else run.voxelwiseCoefficients.lift(voxel).getOrElse(Vector.empty)
      ArmaCoefficients(phi)
    }
    if perRun.exists(_.phi.length != ar.order) then
      Left(FitTraceFailure.Unavailable("fitted voxelwise whitening coefficients are incomplete"))
    else
      val scope = ar.whitening.pooling match
        case NoisePooling.Global => CoefficientScope.Global(perRun.head)
        case NoisePooling.Run => CoefficientScope.ByRun(perRun)
      WhiteningPlan.withScope(scope, segments, ar.whitening.initialCondition, ar.whitening.method)
        .left.map(error => FitTraceFailure.Refused(error.message))

object FitTraceDiagnostics:
  /** Explicitly fit and capture only a budgeted selection. The plan's own
    * dataset descriptor must be the reader's descriptor, and the response is
    * read once before fitting. Estimated shared AR is therefore estimated over
    * precisely the selected voxels named in the returned identity.
    */
  def fitAndCapture(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      selection: DataSelection,
      maxRows: Int,
      maxVoxels: Int,
      cancelled: () => Boolean = () => false
  ): Either[FitTraceFailure, DenseFmriFitResult] =
    if !(reader.dataset eq plan.model.dataset) then
      Left(FitTraceFailure.Refused("reader is not bound to the fit plan's exact dataset descriptor"))
    else if !supportsTraceStrategy(plan.strategy) then
      Left(FitTraceFailure.Unavailable(s"${plan.strategy} has no captured dense OLS/AR GLS trace operator"))
    else if plan.config.missingData == MissingDataPolicy.OmitRowsPerVoxel then
      Left(FitTraceFailure.Unavailable("voxel-specific missing-row fits do not share one trace row axis"))
    else if maxRows <= 0 || maxVoxels <= 0 then
      Left(FitTraceFailure.Refused("positive row and voxel budgets are required"))
    else if obviousOverBudget(reader, selection, maxRows, maxVoxels) then
      Left(FitTraceFailure.Refused("selected trace exceeds the row or voxel budget"))
    else
      for
        resolved <- reader.dataset.resolve(selection).left.map(e => FitTraceFailure.Refused(e.message))
        _ <- if resolved.nTimepoints <= maxRows && resolved.nVoxels <= maxVoxels then Right(())
          else Left(FitTraceFailure.Refused("selected trace exceeds the row or voxel budget"))
        _ <- if !cancelled() then Right(()) else Left(FitTraceFailure.Refused("trace cancelled before read"))
        series <- reader.seriesEither(selection).left.map(e => FitTraceFailure.Fit(FitChunkPlan.mapDatasetError(e)))
        _ <- if series.timepoints == resolved.timepoints && series.voxelIndices == resolved.voxels then Right(())
          else Left(FitTraceFailure.Refused("reader returned reordered or incompatible axes"))
        _ <- if !cancelled() then Right(()) else Left(FitTraceFailure.Refused("trace cancelled after read"))
        partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
        input <- FitPlanExecutor.fitBlockInput(plan, series, partitions).left.map(FitTraceFailure.Fit.apply)
        _ <- if input.response.value.rows <= maxRows && input.response.value.cols <= maxVoxels then Right(())
          else Left(FitTraceFailure.Refused("prepared trace exceeds the row or voxel budget"))
        _ <- if !cancelled() then Right(()) else Left(FitTraceFailure.Refused("trace cancelled before fit"))
        block <- FitKernel.fitDense(input, plan.engine, plan.config).left.map(FitTraceFailure.Fit.apply)
      yield
        val fit = FitPlanExecutor.denseResult(plan, block)
        val runIndices = fit.timepoints.map { timepoint =>
          partitions.find(_.timepoints.contains(timepoint)).get.runIndex
        }
        val identity = FitTraceIdentity(plan.model.dataset.id, plan.designFingerprint, fit.timepoints,
          runIndices, fit.voxelIndices, fit.engine, fit.preparationProvenance, fit.autocorrelation)
        val originalAvailable = fit.voxelIndices.map { voxel =>
          val source = series.voxelIndices.indexOf(voxel)
          source >= 0 && series.timepoints == fit.timepoints &&
            input.preparationProvenance.forall(_.volumeWeighting.isEmpty) &&
            (0 until fit.timepoints.length).forall(row => series.data(row, source) == input.response.value(row, fit.voxelIndices.indexOf(voxel)))
        }
        val capture = new CapturedFitTrace(fit, identity, input.design.value, input.response.value,
          originalAvailable)
        fit.copy(traceCapture = Some(capture))

  def historicalEstimateUnavailable: FitTraceFailure =
    FitTraceFailure.Unavailable("estimate-only artifacts do not retain the fitted nuisance state or whitening operator")

  private def supportsTraceStrategy(strategy: FitStrategy): Boolean =
    strategy match
      case FitStrategy.OrdinaryLeastSquares(_) | FitStrategy.GeneralizedLeastSquares(_, _) => true
      case _ => false

  private def obviousOverBudget(reader: DatasetSeriesReader, selection: DataSelection,
      maxRows: Int, maxVoxels: Int): Boolean =
    val rows = selection.time match
      case TimepointSelection.All => reader.dataset.shape.timepoints
      case TimepointSelection.Indices(values) => values.length
      case TimepointSelection.Window(_, length) => length
      case TimepointSelection.Excluding(values) => reader.dataset.shape.timepoints - values.length
    val voxels = selection.voxels match
      case VoxelSelection.All | VoxelSelection.AllSpatial => reader.dataset.voxelDomain.voxels.length
      case VoxelSelection.Indices(values) => values.length
      case VoxelSelection.Coords(values) => values.length
    rows > maxRows || voxels > maxVoxels
