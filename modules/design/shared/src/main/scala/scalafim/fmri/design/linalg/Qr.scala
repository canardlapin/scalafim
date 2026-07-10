package scalafim.fmri.design.linalg

import scalafim.linalg.{QrDecomposition as CoreQrDecomposition}

final class QrDecomposition private (private val core: CoreQrDecomposition):
  export core.{applyQInPlace, applyQtInPlace, cols, rank, rows}

object QrDecomposition:

  def decompose(
      a0: Array[Double],
      rows: Int,
      cols: Int,
      pivoting: Boolean = true,
      tol: Double = 1e-7
  ): QrDecomposition =
    new QrDecomposition(CoreQrDecomposition.decompose(a0, rows, cols, pivoting, tol))
