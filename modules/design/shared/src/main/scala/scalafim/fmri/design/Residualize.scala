package scalafim.fmri.design

import scalafim.fmri.design.baseline.BaselineModel
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.design.linalg.QrDecomposition
import scalafim.fmri.hrf.linalg.Mat

object Residualize:

  def apply(
      design: Mat,
      data: Mat,
      cols: Option[Seq[Int]] = None,
      tol: Double = 1e-7,
      pivot: Boolean = true
  ): Mat =
    require(design.rows == data.rows, s"Row mismatch: data.rows=${data.rows}, design.rows=${design.rows}")

    val x =
      cols match
        case None => design
        case Some(cs) if cs.isEmpty => Mat.zeros(design.rows, 0)
        case Some(cs)              => selectCols(design, cs)

    if x.cols == 0 || data.cols == 0 then Mat.unsafe(data.rows, data.cols, data.data.clone)
    else
      val qr = QrDecomposition.decompose(x.data, rows = x.rows, cols = x.cols, pivoting = pivot, tol = tol)
      val out = data.data.clone

      qr.applyQtInPlace(out, yCols = data.cols)

      // Projection residual in Qᵀ-space is [0; qty2] where rank determines the span of X.
      var r = 0
      while r < qr.rank do
        var c = 0
        while c < data.cols do
          out(r * data.cols + c) = 0.0
          c += 1
        r += 1

      qr.applyQInPlace(out, yCols = data.cols)
      Mat.unsafe(data.rows, data.cols, out)

  private def namesToIndices(all: Vector[String], selected: Seq[String]): Vector[Int] =
    val index = all.zipWithIndex.toMap
    selected.iterator.map { n =>
      index.getOrElse(n, throw new IllegalArgumentException(s"Unknown column name: '$n'"))
    }.toVector

  private def selectCols(m: Mat, cols: Seq[Int]): Mat =
    require(cols.nonEmpty, "cols must be non-empty")
    cols.foreach { c =>
      require(c >= 0 && c < m.cols, s"column index out of range: $c")
    }
    val out = new Array[Double](m.rows * cols.length)
    var r = 0
    while r < m.rows do
      var j = 0
      while j < cols.length do
        out(r * cols.length + j) = m.data(r * m.cols + cols(j))
        j += 1
      r += 1
    Mat.unsafe(m.rows, cols.length, out)

  trait HasDesignMatrix[A]:
    def designMatrix(a: A): Mat
    def columnNames(a: A): Vector[String]

  object HasDesignMatrix:
    given HasDesignMatrix[EventModel] with
      def designMatrix(a: EventModel): Mat = a.designMatrix
      def columnNames(a: EventModel): Vector[String] = a.columnNames

    given HasDesignMatrix[BaselineModel] with
      def designMatrix(a: BaselineModel): Mat = a.designMatrix
      def columnNames(a: BaselineModel): Vector[String] = a.columnNames

  extension [A](model: A)(using ev: HasDesignMatrix[A])
    def residualize(data: Mat, cols: Option[Seq[Int]] = None, tol: Double = 1e-7, pivot: Boolean = true): Mat =
      Residualize(ev.designMatrix(model), data, cols = cols, tol = tol, pivot = pivot)

    def residualizeByName(data: Mat, cols: Seq[String], tol: Double = 1e-7, pivot: Boolean = true): Mat =
      Residualize(
        ev.designMatrix(model),
        data,
        cols = Some(namesToIndices(ev.columnNames(model), cols)),
        tol = tol,
        pivot = pivot
      )
