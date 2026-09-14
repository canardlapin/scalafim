package scalafim.fmri.fit

import scalafim.fmri.design.RunCoefficientProjection
import scalafim.fmri.model.ArOptions
import gale.linalg.{DMat, Matrix}

/** Independently estimated GLS fits, one per source run.
  *
  * Coefficient scope and noise estimation are intentionally separate here:
  * coefficients are always run-specific, while each run retains its own AR
  * diagnostics, whitening segments, residual variance and normalized
  * coefficient covariance. Across-run AR pooling is not silently approximated
  * by this composition.
  */
final case class RunwiseGlsFit(runs: Vector[RunwiseGlsRunFit]):
  require(runs.nonEmpty, "runwise GLS fit must contain at least one run")
  require(runs.map(_.partition.runIndex).distinct.length == runs.length, "runwise GLS run identities must be unique")

final case class RunwiseGlsRunFit(
    partition: RunPartition,
    fit: GlsFit,
    projection: Option[RunCoefficientProjection] = None
):
  require(
    projection.forall(_.run.oneBased == partition.runIndex + 1),
    "runwise GLS projection must match the source run"
  )

private[fit] final case class RunwiseGlsPreparedRun(
    partition: RunPartition,
    solver: GlsPrepared,
    projection: Option[RunCoefficientProjection]
)

final class RunwiseGlsPrepared private[fit] (
    val timepoints: Int,
    private val runs: Vector[RunwiseGlsPreparedRun]
):
  require(timepoints > 0, "runwise GLS preparation must contain selected timepoints")
  require(runs.nonEmpty, "runwise GLS preparation must contain at least one run")

  def runCount: Int = runs.length

  def fit(
      response: ResponseBlock,
      voxelIndices: Vector[Int]
  ): Either[FitError, RunwiseGlsFit] =
    if response.timepoints != timepoints then
      Left(FitError.RowMismatch(timepoints, response.timepoints))
    else
      val out = Vector.newBuilder[RunwiseGlsRunFit]
      var run = 0
      while run < runs.length do
        val prepared = runs(run)
        val runResponse = ResponseBlock.fromMatrix(RunwiseGls.selectRows(response.value, prepared.partition.rowIndices)) match
          case Left(error) => return Left(FitError.RunwiseFitFailed(prepared.partition.runIndex, error))
          case Right(value) => value
        prepared.solver.fit(runResponse, voxelIndices) match
          case Left(error) => return Left(FitError.RunwiseFitFailed(prepared.partition.runIndex, error))
          case Right(value) =>
            out += RunwiseGlsRunFit(
              prepared.partition,
              value.copy(diagnostics = RunwiseGls.bindRunIndex(value.diagnostics, prepared.partition.runIndex)),
              prepared.projection
            )
        run += 1
      Right(RunwiseGlsFit(out.result()))

object RunwiseGls:
  def fit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: IndexedSeq[RunPartition],
      options: ArOptions
  ): Either[FitError, RunwiseGlsFit] =
    fit(design, response, partitions, options, Vector.empty)

  def fit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: IndexedSeq[RunPartition],
      options: ArOptions,
      projections: IndexedSeq[RunCoefficientProjection]
  ): Either[FitError, RunwiseGlsFit] =
    prepare(
      design,
      response,
      partitions,
      options,
      projections,
      (0 until response.voxels).toVector
    ).flatMap(_.fit(response, (0 until response.voxels).toVector))

  private[fit] def prepare(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: IndexedSeq[RunPartition],
      options: ArOptions,
      projections: IndexedSeq[RunCoefficientProjection],
      selectedVoxelIndices: Vector[Int]
  ): Either[FitError, RunwiseGlsPrepared] =
    if response.timepoints != design.timepoints then
      Left(FitError.RowMismatch(design.timepoints, response.timepoints))
    else if partitions.isEmpty then
      Left(FitError.EmptyRunPartition(0))
    else if projections.nonEmpty && projections.length != partitions.length then
      Left(FitError.InvalidFitAxis("runwise GLS projections", s"expected ${partitions.length}, got ${projections.length}"))
    else if options.global then
      Left(FitError.UnsupportedAutocorrelation(
        "runwise GLS requires run-local AR estimation (global=false); use GeneralizedLeastSquares for the separately admitted shared-coefficient estimand"
      ))
    else
      validatePartitions(design.timepoints, partitions, options) match
        case Left(error) => Left(error)
        case Right(_) => prepareValidated(design, response, partitions, options, projections, selectedVoxelIndices)

  private def prepareValidated(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: IndexedSeq[RunPartition],
      options: ArOptions,
      projections: IndexedSeq[RunCoefficientProjection],
      selectedVoxelIndices: Vector[Int]
  ): Either[FitError, RunwiseGlsPrepared] =
    val out = Vector.newBuilder[RunwiseGlsPreparedRun]
    var run = 0
    while run < partitions.length do
      val partition = partitions(run)
      val projection = if projections.isEmpty then None else Some(projections(run))
      projection match
        case Some(value) if value.run.oneBased != partition.runIndex + 1 =>
          return Left(FitError.InvalidFitAxis(
            "runwise GLS projection",
            s"projection run ${value.run.oneBased} does not match partition ${partition.runIndex + 1}"
          ))
        case _ => ()
      val runDesign = DesignMatrix.fromMatrix(
        projection.fold(selectRows(design.value, partition.rowIndices)) { value =>
          selectRowsCols(design.value, partition.rowIndices, value.sourceColumnIndices)
        }
      ) match
        case Left(error) => return Left(FitError.RunwiseFitFailed(partition.runIndex, error))
        case Right(value) => value
      val runResponse = ResponseBlock.fromMatrix(selectRows(response.value, partition.rowIndices)) match
        case Left(error) => return Left(FitError.RunwiseFitFailed(partition.runIndex, error))
        case Right(value) => value
      val localPartition = RunPartition(
        runIndex = 0,
        rowIndices = (0 until partition.rowIndices.length).toVector,
        timepoints = partition.timepoints
      )
      val localOptions = options.copy(
        censoredTimepoints = options.censoredTimepoints.filter(partition.timepoints.contains)
      )
      Gls.prepare(
        runDesign,
        runResponse,
        Vector(localPartition),
        localOptions,
        selectedVoxelIndices
      ) match
        case Left(error) =>
          val bound = projection.fold(error)(value => FitKernel.bindRankFailure(error, value.axis))
          return Left(FitError.RunwiseFitFailed(partition.runIndex, bound))
        case Right(value) => out += RunwiseGlsPreparedRun(partition, value, projection)
      run += 1
    Right(new RunwiseGlsPrepared(design.timepoints, out.result()))

  private def validatePartitions(
      rows: Int,
      partitions: IndexedSeq[RunPartition],
      options: ArOptions
  ): Either[FitError, Unit] =
    val representedRows = partitions.iterator.flatMap(_.rowIndices).toVector
    if representedRows.sorted != (0 until rows).toVector then
      Left(FitError.InvalidFitAxis(
        "runwise GLS partitions",
        "row indices must cover every selected design row exactly once"
      ))
    else if partitions.map(_.runIndex).distinct.length != partitions.length then
      Left(FitError.InvalidFitAxis("runwise GLS partitions", "run indices must be unique"))
    else
      val selectedTimepoints = partitions.iterator.flatMap(_.timepoints).toSet
      val missingCensors = options.censoredTimepoints.distinct.filterNot(selectedTimepoints.contains)
      if missingCensors.nonEmpty then
        Left(FitError.UnsupportedAutocorrelation(
          s"censored timepoints are not selected: ${missingCensors.mkString(", ")}"
        ))
      else Right(())

  private[fit] def bindRunIndex(diagnostics: ArDiagnostics, runIndex: Int): ArDiagnostics =
    diagnostics.copy(
      runs = diagnostics.runs.map(_.copy(runIndex = runIndex)),
      whitening = diagnostics.whitening.copy(
        segments = diagnostics.whitening.segments.map(_.copy(runIndex = runIndex)),
        censorGaps = diagnostics.whitening.censorGaps.map(_.copy(runIndex = runIndex))
      )
    )

  private[fit] def selectRows(matrix: DMat, rows: IndexedSeq[Int]): DMat =
    Matrix.tabulate(rows.length, matrix.cols) { (row, col) =>
      matrix(rows(row), col)
    }

  private def selectRowsCols(
      matrix: DMat,
      rows: IndexedSeq[Int],
      columns: IndexedSeq[Int]
  ): DMat =
    Matrix.tabulate(rows.length, columns.length) { (row, col) =>
      matrix(rows(row), columns(col))
    }
