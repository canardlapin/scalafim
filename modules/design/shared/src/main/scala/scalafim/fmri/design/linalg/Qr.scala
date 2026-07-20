package scalafim.fmri.design.linalg

import gale.linalg.{DMat, Matrix, QR, QROptions, QRPivoting}

final class QrDecomposition private (private val core: QR, val rows: Int, val cols: Int):
  val rank: Int = core.diagnostics.rank.getOrElse(math.min(rows, cols))

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

  def decompose(
      a0: Array[Double],
      rows: Int,
      cols: Int,
      pivoting: Boolean = true,
      tol: Double = 1e-7
  ): QrDecomposition =
    require(rows >= 0 && cols >= 0, "rows and cols must be non-negative")
    require(a0.length == rows * cols, "data length mismatch")
    val options = QROptions(
      pivoting = if pivoting then QRPivoting.Column else QRPivoting.Disabled,
      rankTolerance = Some(tol)
    )
    new QrDecomposition(fromRowMajor(rows, cols, a0).qr(options), rows, cols)

  private def fromRowMajor(rows: Int, cols: Int, values: Array[Double]): DMat =
    val builder = Matrix.newBuilder(rows, cols)
    var i = 0
    while i < values.length do
      builder.updateRowMajor(i, values(i))
      i += 1
    builder.result()
