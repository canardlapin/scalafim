package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}

final case class RunwiseOlsFit(runs: Vector[RunwiseOlsRunFit]):
  require(runs.nonEmpty, "runwise OLS fit must contain at least one run")

final case class RunwiseOlsRunFit(
    partition: RunPartition,
    fit: OlsFit
)

object RunwiseOls:
  def fit(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: IndexedSeq[RunPartition]
  ): Either[FitError, RunwiseOlsFit] =
    if response.timepoints != design.timepoints then
      Left(FitError.RowMismatch(design.timepoints, response.timepoints))
    else if partitions.isEmpty then
      Left(FitError.EmptyRunPartition(0))
    else
      val out = Vector.newBuilder[RunwiseOlsRunFit]
      var i = 0
      var failure: FitError | Null = null
      while i < partitions.length && failure == null do
        val partition = partitions(i)
        val runResult =
          for
            runDesign <- DesignMatrix.fromMatrix(selectRows(design.value, partition.rowIndices))
            runResponse <- ResponseBlock.fromMatrix(selectRows(response.value, partition.rowIndices))
            runFit <- Ols.fit(runDesign, runResponse)
          yield RunwiseOlsRunFit(partition, runFit)

        runResult match
          case Right(value) =>
            out += value
          case Left(error) =>
            failure = FitError.RunwiseFitFailed(partition.runIndex, error)
        i += 1

      failure match
        case null  => Right(RunwiseOlsFit(out.result()))
        case error => Left(error)

  private def selectRows(matrix: DMat, rows: IndexedSeq[Int]): DMat =
    Matrix.tabulate(rows.length, matrix.cols) { (row, col) =>
      matrix(rows(row), col)
    }
