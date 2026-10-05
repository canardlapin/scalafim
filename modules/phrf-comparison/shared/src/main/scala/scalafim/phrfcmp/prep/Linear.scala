package scalafim.phrfcmp.prep

import gale.linalg.{DMat, QROptions, QRPivoting}

import scalafim.phrfcmp.ingest.Matrix

/** Dense helpers shared by the S2 preparation: Matrix/DMat bridges and pivoted-QR least-squares residuals. */
private[prep] object Linear:

  /** Absolute rank tolerance on the pivoted-QR diagonal (the S0 spike setting). */
  val RankTolerance: Double = 1e-10

  def toDMat(m: Matrix): DMat = DMat.tabulate(m.rows, m.cols)((r, c) => m(r, c))

  def transposeToDMat(m: Matrix): DMat = DMat.tabulate(m.cols, m.rows)((r, c) => m(c, r))

  def fromDMat(d: DMat): Either[PrepRefusal, Matrix] =
    val out = new Array[Double](d.rows * d.cols)
    var r = 0
    while r < d.rows do
      var c = 0
      while c < d.cols do
        out(r * d.cols + c) = d(r, c)
        c += 1
      r += 1
    Matrix.of(d.rows, d.cols, out).left.map(e => PrepRefusal.Inconsistent(e.message))

  def fromDMatTransposed(d: DMat): Either[PrepRefusal, Matrix] =
    val out = new Array[Double](d.rows * d.cols)
    var r = 0
    while r < d.rows do
      var c = 0
      while c < d.cols do
        out(c * d.rows + r) = d(r, c)
        c += 1
      r += 1
    Matrix.of(d.cols, d.rows, out).left.map(e => PrepRefusal.Inconsistent(e.message))

  def selectRows(m: Matrix, rows: Array[Int]): Matrix =
    val out = new Array[Double](rows.length * m.cols)
    var i = 0
    while i < rows.length do
      System.arraycopy(m.data, rows(i) * m.cols, out, i * m.cols, m.cols)
      i += 1
    Matrix.of(rows.length, m.cols, out).fold(e => throw new IllegalStateException(e.message), identity)

  /** Residuals of `y` after projection onto the column space of `x` (pivoted QR), and the numerical rank of `x`. */
  def residuals(x: DMat, y: DMat): (DMat, Int) =
    val qr = x.qr(QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(RankTolerance)))
    val rank = qr.diagnostics.rank.getOrElse(x.cols)
    if rank == 0 then (y, 0)
    else
      val q = qr.q.slice(0, x.rows, 0, rank)
      (y - q * (q.t * y), rank)

  /** Orthogonal projector `Q Q'` onto the column space of `x` and its rank. */
  def projector(x: DMat): (DMat, Int) =
    val qr = x.qr(QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(RankTolerance)))
    val rank = qr.diagnostics.rank.getOrElse(x.cols)
    if rank == 0 then (DMat.zeros(x.rows, x.rows), 0)
    else
      val q = qr.q.slice(0, x.rows, 0, rank)
      (q * q.t, rank)
