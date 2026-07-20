package scalafim.inference

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.LinAlgError
import gale.linalg.Matrix
import gale.linalg.Vec
import gale.spectral.Eigen
import gale.spectral.EigenDecomposition
import gale.spectral.EigenOrder
import gale.spectral.EigenSelection
import gale.spectral.SingularSelection
import gale.spectral.Svds

/** Allocation-aware adaptations between inference's domain protocols and Gale.
  * The inference API keeps leading components first even though Gale's symmetric
  * eigendecompositions have an ascending result layout.
  */
private[inference] object InferenceNumerics:
  def matrixFromRowMajor(rows: Int, cols: Int, values: Array[Double]): DMat =
    require(values.length == rows * cols, "row-major data length must match matrix shape")
    val out = Matrix.newBuilder(rows, cols)
    var index = 0
    while index < values.length do
      out.updateRowMajor(index, values(index))
      index += 1
    out.result()

  def matrixFromRows(rows: Seq[Seq[Double]]): DMat =
    val rowCount = rows.length
    val colCount = rows.headOption.fold(0)(_.length)
    require(rows.forall(_.length == colCount), "matrix rows must have equal lengths")
    val out = Matrix.newBuilder(rowCount, colCount)
    var row = 0
    rows.foreach { values =>
      var col = 0
      values.foreach { value =>
        out(row, col) = value
        col += 1
      }
      row += 1
    }
    out.result()

  def vectorFromArray(values: Array[Double]): DVec =
    val out = Vec.newBuilder(values.length)
    var index = 0
    while index < values.length do
      out(index) = values(index)
      index += 1
    out.result()

  def vectorFromSeq(values: Seq[Double]): DVec =
    val out = Vec.newBuilder(values.length)
    var index = 0
    values.foreach { value =>
      out(index) = value
      index += 1
    }
    out.result()

  def multiply(left: DMat, right: DMat): DMat =
    left * right

  def transposeMultiply(left: DMat, right: DMat): DMat =
    left.t * right

  def selectRows(matrix: DMat, indices: IndexedSeq[Int]): DMat =
    val out = Matrix.newBuilder(indices.length, matrix.cols)
    var row = 0
    while row < indices.length do
      var col = 0
      while col < matrix.cols do
        out(row, col) = matrix(indices(row), col)
        col += 1
      row += 1
    out.result()

  def selectColumns(matrix: DMat, count: Int): DMat =
    require(count > 0 && count <= matrix.cols, "column count must fit matrix")
    val out = Matrix.newBuilder(matrix.rows, count)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < count do
        out(row, col) = matrix(row, col)
        col += 1
      row += 1
    out.result()

  def takeVector(vector: DVec, count: Int): DVec =
    require(count > 0 && count <= vector.length, "vector count must fit vector")
    val out = Vec.newBuilder(count)
    var index = 0
    while index < count do
      out(index) = vector(index)
      index += 1
    out.result()

extension (matrix: DMat)
  private[inference] def copyData: Array[Double] =
    matrix.valuesRowMajor.toArray

  private[inference] def toRows: Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows) { row =>
      Vector.tabulate(matrix.cols) { col => matrix(row, col) }
    }

  private[inference] def transpose: DMat =
    matrix.t

  private[inference] def selectRows(indices: IndexedSeq[Int]): DMat =
    InferenceNumerics.selectRows(matrix, indices)

extension (vector: DVec)
  private[inference] def toVector: Vector[Double] =
    vector.toSeq.toVector

extension (error: LinAlgError)
  private[inference] def message: String =
    error.getMessage

opaque type DecompositionRank = Int

object DecompositionRank:
  def bounded(value: Int, limit: Int): Either[LinAlgError, DecompositionRank] =
    if value > 0 && value <= limit then Right(value)
    else Left(LinAlgError.InvalidArgument(
      s"requested decomposition rank $value, but at most $limit component(s) are available"
    ))

  extension (rank: DecompositionRank)
    private[inference] inline def value: Int = rank

final case class SymmetricEigenResult(values: DVec, vectors: DMat):
  require(values.length == vectors.cols, "eigenvalue count must match eigenvector columns")

final case class SvdResult(u: DMat, singularValues: DVec, v: DMat):
  require(u.cols == singularValues.length, "left singular vector columns must match singular values")
  require(v.cols == singularValues.length, "right singular vector columns must match singular values")

trait SymmetricEigenSolver:
  def decompose(matrix: DMat): Either[LinAlgError, SymmetricEigenResult]

trait DenseSvdSolver:
  def decompose(input: DMat, rank: DecompositionRank): Either[LinAlgError, SvdResult]

trait GeneralizedEigenSolver:
  def decompose(
      a: DMat,
      b: DMat,
      rank: DecompositionRank
  ): Either[LinAlgError, SymmetricEigenResult]

object LinalgSolvers:
  val symmetricEigen: SymmetricEigenSolver = GaleSymmetricEigenSolver
  val denseSvd: DenseSvdSolver = GaleDenseSvdSolver
  val generalizedEigen: GeneralizedEigenSolver = GaleGeneralizedEigenSolver

private object GaleSymmetricEigenSolver extends SymmetricEigenSolver:
  override def decompose(matrix: DMat): Either[LinAlgError, SymmetricEigenResult] =
    Eigen.eigSymmetric(matrix, EigenSelection.All).map(descending)

private object GaleDenseSvdSolver extends DenseSvdSolver:
  override def decompose(input: DMat, rank: DecompositionRank): Either[LinAlgError, SvdResult] =
    val limit = Math.min(input.rows, input.cols)
    if rank.value > limit then
      Left(LinAlgError.InvalidArgument(
        s"requested decomposition rank ${rank.value}, but at most $limit component(s) are available"
      ))
    else
      Svds.svd(input, SingularSelection.All).map { result =>
        val u = InferenceNumerics.selectColumns(result.u, rank.value)
        val vtRows = InferenceNumerics.selectRows(result.vt, 0 until rank.value)
        SvdResult(
          u,
          InferenceNumerics.takeVector(result.singularValues, rank.value),
          vtRows.t
        )
      }

private object GaleGeneralizedEigenSolver extends GeneralizedEigenSolver:
  override def decompose(
      a: DMat,
      b: DMat,
      rank: DecompositionRank
  ): Either[LinAlgError, SymmetricEigenResult] =
    val selection =
      if rank.value == a.rows then EigenSelection.All
      else EigenSelection.Count(rank.value, EigenOrder.LargestAlgebraic)
    Eigen.eigSymmetricGeneralized(a, b, selection).map(descending)

private def descending(result: EigenDecomposition): SymmetricEigenResult =
  val count = result.size
  val values = Vec.newBuilder(count)
  val vectors = Matrix.newBuilder(result.eigenvectors.rows, count)
  var col = 0
  while col < count do
    val source = count - col - 1
    values(col) = result.eigenvalues(source)
    var row = 0
    while row < result.eigenvectors.rows do
      vectors(row, col) = result.eigenvectors(row, source)
      row += 1
    col += 1
  SymmetricEigenResult(values.result(), vectors.result())
