package scalafim.fmri.ar

import gale.linalg.{DMat, DVec, LinAlgError, Matrix, QROptions, QRPivoting, Vec}

/** The design-bound bias map, optionally accompanied by the short-memory
  * inverse operator. Decomposition and least squares are supplied by Gale.
  */
private[ar] final case class AcvfCorrection(
    matrix: DMat,
    operator: Option[DMat] = None,
    anchoredDirections: Int = 0
):
  def rows: Int = matrix.rows
  def solve(block: DMat, rhs: DVec): Either[LinAlgError, DVec] = operator match
    case None => block.solve(rhs)
    case Some(value) =>
      if value.rows != rhs.length then Left(LinAlgError.InvalidArgument("prepared tail operator and available lag axis differ"))
      else Right(value * rhs)

private[ar] object AcvfCorrection:
  def prepare(
      matrices: AcvfBiasMatrices,
      layout: NoiseEstimationLayout,
      policy: AcvfCorrectionSolve
  ): Either[ArError, Vector[Option[AcvfCorrection]]] =
    val result = Vector.newBuilder[Option[AcvfCorrection]]
    var run = 0
    while run < matrices.byRun.length do
      val matrix = matrices.byRun(run)
      val rc = AcvfBias.reciprocalCondition(matrix)
      if !rc.isFinite || rc < AcvfBias.ReciprocalConditionFloor then result += None
      else
        val available = layout.segmentsForRun(run).map(_.length).maxOption.getOrElse(0)
        val size = math.min(available, matrix.rows)
        if policy == AcvfCorrectionSolve.Exact || size < 8 then result += Some(AcvfCorrection(matrix))
        else tailOperator(matrix, size) match
          case Left(error) => return Left(ArError.CorrectionSolverPreparationFailed(error.getMessage))
          case Right(value) => result += Some(value)
      run += 1
    Right(result.result())

  private def tailOperator(matrix: DMat, size: Int): Either[LinAlgError, AcvfCorrection] =
    matrix.slice(0, size, 0, size).svd.flatMap(_.requireConverged).flatMap: decomposition =>
      val singular = decomposition.singularValues
      val mid = size / 2
      val median = if size % 2 == 1 then singular(mid) else (singular(mid - 1) + singular(mid)) / 2.0
      val keep = (0 until size).filter(i => singular(i) > 0.1 * median).toVector
      val weak = (0 until size).filterNot(keep.contains).toVector
      if weak.isEmpty then Right(AcvfCorrection(matrix))
      else
        val base = Matrix.tabulate(size, size): (row, col) =>
          var total = 0.0
          var k = 0
          while k < keep.length do
            val index = keep(k)
            total += decomposition.vt(index, row) * decomposition.u(col, index) / singular(index)
            k += 1
          total
        val nullspace = Matrix.tabulate(size, weak.length)((row, col) => decomposition.vt(weak(col), row))
        val tailStart = size - math.ceil(0.25 * size).toInt
        for
          qr <- nullspace.slice(tailStart, size, 0, weak.length).qrScaledRows(Vec.tabulate(size - tailStart)(_ => 1.0), QROptions(pivoting = QRPivoting.Column))
          adjustment <- qr.solveLeastSquares(base.slice(tailStart, size, 0, size))
        yield AcvfCorrection(matrix, Some(base - nullspace * adjustment), weak.length)
