package scalafim.fmri.fit

import scalafim.fmri.design.RunCoefficientProjection
import gale.linalg.{DMat, Matrix}

final case class RunwiseOlsFit(runs: Vector[RunwiseOlsRunFit]):
  require(runs.nonEmpty, "runwise OLS fit must contain at least one run")

final case class RunwiseOlsRunFit(
    partition: RunPartition,
    fit: OlsFit,
    projection: Option[RunCoefficientProjection] = None
)

object RunwiseOls:
  def fit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: IndexedSeq[RunPartition]
  ): Either[FitError, RunwiseOlsFit] =
    fit(design, response, partitions, Vector.empty)

  /** Fit each run after applying a checked structural column projection.
    *
    * The projection is expressed in source-design column coordinates, so a
    * run-local intercept/drift or an empty shared cell can never enter the
    * factorization as an all-zero predictor.  The returned run fit retains the
    * mapping for result/provenance consumers.
    */
  def fit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: IndexedSeq[RunPartition],
      projections: IndexedSeq[RunCoefficientProjection]
  ): Either[FitError, RunwiseOlsFit] =
    if response.timepoints != design.timepoints then
      Left(FitError.RowMismatch(design.timepoints, response.timepoints))
    else if partitions.isEmpty then
      Left(FitError.EmptyRunPartition(0))
    else if projections.nonEmpty && projections.length != partitions.length then
      Left(FitError.InvalidFitAxis("runwise projections", s"expected ${partitions.length}, got ${projections.length}"))
    else
      val out = Vector.newBuilder[RunwiseOlsRunFit]
      var i = 0
      var failure: Option[FitError] = None
      while i < partitions.length && failure.isEmpty do
        val partition = partitions(i)
        val projection =
          if projections.isEmpty then None
          else Some(projections(i))
        projection.foreach { value =>
          if value.run.oneBased != partition.runIndex + 1 then
            failure = Some(FitError.InvalidFitAxis(
              "runwise projection",
              s"projection run ${value.run.oneBased} does not match partition ${partition.runIndex + 1}"
            ))
        }
        val runResult =
          failure match
            case Some(error) => Left(error)
            case None =>
              for
                runDesign <- DesignMatrix.fromMatrix(
                  projection.fold(selectRows(design.value, partition.rowIndices)) { value =>
                    selectRowsCols(design.value, partition.rowIndices, value.sourceColumnIndices)
                  }
                )
                runResponse <- ResponseBlock.fromMatrix(selectRows(response.value, partition.rowIndices))
                runFit <- Ols.fit(runDesign, runResponse).left.map { error =>
                  projection match
                    case Some(value) => FitKernel.bindRankFailure(error, value.axis)
                    case None        => error
                }
              yield RunwiseOlsRunFit(partition, runFit, projection)

        runResult match
          case Right(value) =>
            out += value
          case Left(error) =>
            failure = Some(FitError.RunwiseFitFailed(partition.runIndex, error))
        i += 1

      failure match
        case None        => Right(RunwiseOlsFit(out.result()))
        case Some(error) => Left(error)

  private def selectRows(matrix: DMat, rows: IndexedSeq[Int]): DMat =
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
