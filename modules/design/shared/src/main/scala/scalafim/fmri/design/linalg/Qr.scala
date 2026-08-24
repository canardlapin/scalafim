package scalafim.fmri.design.linalg

import gale.linalg.{DMat, Matrix, QR, QROptions, QRPivoting}

final class QrDecomposition private (private val core: QR, val rows: Int, val cols: Int):
  val rank: Int = core.diagnostics.rank.getOrElse(math.min(rows, cols))

  /** Absolute diagonal cutoff actually used by the factorization. */
  val rankTolerance: Double =
    core.diagnostics.rankTolerance.getOrElse(0.0)

  /** Original input column represented by each pivoted factor column. */
  val pivotOrder: Vector[Int] =
    core.columnPermutation.toIndexSeq.toVector

  /** Magnitudes of the diagonal of R, in pivot order. */
  val diagonalR: Vector[Double] =
    (0 until math.min(core.r.rows, core.r.cols)).toVector.map(index => math.abs(core.r(index, index)))

  /**
    * A scale-invariant diagonal-R condition estimate for the accepted
    * numerical subspace: `max(abs(diag(R))) / min(abs(diag(R)))` over the
    * first `rank` pivots.  This is deliberately not presented as a 2-norm
    * condition number.
    */
  val conditionEstimate: Option[Double] =
    val accepted = diagonalR.take(rank).filter(_ > 0.0)
    if accepted.isEmpty then None
    else Some(accepted.max / accepted.min)

  def applyQtInPlace(y: Array[Double], yCols: Int): Unit =
    overwrite(y, yCols, core.applyQT)

  def applyQInPlace(y: Array[Double], yCols: Int): Unit =
    overwrite(y, yCols, core.applyQ)

  private def overwrite(
      y: Array[Double],
      yCols: Int,
      transform: DMat => Either[gale.linalg.LinAlgError, DMat]
  ): Unit =
    require(yCols >= 0, "yCols must be non-negative")
    require(y.length == rows * yCols, "y length mismatch")

    val transformed = transform(QrDecomposition.fromRowMajor(rows, yCols, y)) match
      case Right(value) => value
      case Left(error)  => throw error

    var row = 0
    while row < rows do
      var col = 0
      while col < yCols do
        y(row * yCols + col) = transformed(row, col)
        col += 1
      row += 1

object QrDecomposition:

  /** Decompose with Gale's matrix-scale-aware default rank cutoff. */
  def decomposeScaleAware(
      a0: Array[Double],
      rows: Int,
      cols: Int,
      pivoting: Boolean = true
  ): QrDecomposition =
    decomposeWithTolerance(a0, rows, cols, pivoting, None)

  def decompose(
      a0: Array[Double],
      rows: Int,
      cols: Int,
      pivoting: Boolean = true,
      tol: Double = 1e-7
  ): QrDecomposition =
    require(tol >= 0.0 && tol.isFinite, "QR tolerance must be finite and non-negative")
    decomposeWithTolerance(a0, rows, cols, pivoting, Some(tol))

  private def decomposeWithTolerance(
      a0: Array[Double],
      rows: Int,
      cols: Int,
      pivoting: Boolean,
      tolerance: Option[Double]
  ): QrDecomposition =
    require(rows >= 0 && cols >= 0, "rows and cols must be non-negative")
    require(a0.length == rows * cols, "data length mismatch")
    val options = QROptions(
      pivoting = if pivoting then QRPivoting.Column else QRPivoting.Disabled,
      rankTolerance = tolerance
    )
    new QrDecomposition(fromRowMajor(rows, cols, a0).qr(options), rows, cols)

  private def fromRowMajor(rows: Int, cols: Int, values: Array[Double]): DMat =
    val builder = Matrix.newBuilder(rows, cols)
    var i = 0
    while i < values.length do
      builder.updateRowMajor(i, values(i))
      i += 1
    builder.result()
