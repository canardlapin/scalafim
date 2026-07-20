package scalafim.linalg

/** Explicit evidence that a square linear map is intended to be symmetric.
  * `fromDense` validates that claim numerically. `declared` is for operators
  * whose construction establishes symmetry (for example, a graph Laplacian);
  * solver implementations may still validate a materialized representation.
  */
final class SymmetricOperator private (val map: LinearMap):
  def order: Int =
    map.rows

  def toDense: Either[LinearAlgebraError, DoubleMatrix] =
    map
      .forward(DoubleMatrix.eye(order))
      .left
      .map(error => LinearAlgebraError.OperatorApplicationFailed(error.message))

object SymmetricOperator:
  def declared(map: LinearMap): Either[LinearAlgebraError, SymmetricOperator] =
    if map.rows != map.cols then Left(LinearAlgebraError.NonSquareMatrix(map.rows, map.cols))
    else Right(new SymmetricOperator(map))

  def fromDense(
      matrix: DoubleMatrix,
      tolerance: Tolerance = Tolerance.DefaultEigen
  ): Either[LinearAlgebraError, SymmetricOperator] =
    for
      square <- SquareMatrix.from(matrix)
      _ <- DenseDecompositionOps.checkFinite(MatrixValueRole.Data, square.value)
      _ <- DenseDecompositionOps.checkSymmetric(square.value, tolerance.value)
    yield new SymmetricOperator(DenseMatrixLinearMap(square.value))

enum PartialSpectrum:
  case Smallest
  case Largest

/** A successful result contains only converged eigenpairs. Values are ascending
  * for `Smallest` and descending for `Largest`. Individual vectors are defined
  * up to sign; repeated-eigenvalue columns define an invariant subspace rather
  * than a unique basis.
  */
final case class PartialSymmetricEigenResult(
    values: DoubleVector,
    vectors: DoubleMatrix,
    residualNorms: DoubleVector,
    spectrum: PartialSpectrum,
    backend: String,
    iterations: Option[Int]
):
  require(values.length > 0, "partial eigen result must contain at least one pair")
  require(vectors.cols == values.length, "eigenvector columns must match eigenvalue count")
  require(residualNorms.length == values.length, "residual count must match eigenvalue count")
  require(residualNorms.toVector.forall(value => value.isFinite && value >= 0.0), "residual norms must be finite and non-negative")
  require(backend.trim.nonEmpty, "partial eigen backend must be non-empty")
  require(iterations.forall(_ >= 0), "partial eigen iterations must be non-negative")

  def rank: Int =
    values.length

trait PartialSymmetricEigenSolver:
  def decompose(
      operator: SymmetricOperator,
      rank: DecompositionRank,
      spectrum: PartialSpectrum
  ): Either[LinearAlgebraError, PartialSymmetricEigenResult]

  final def smallestK(
      operator: SymmetricOperator,
      rank: DecompositionRank
  ): Either[LinearAlgebraError, PartialSymmetricEigenResult] =
    decompose(operator, rank, PartialSpectrum.Smallest)

  final def largestK(
      operator: SymmetricOperator,
      rank: DecompositionRank
  ): Either[LinearAlgebraError, PartialSymmetricEigenResult] =
    decompose(operator, rank, PartialSpectrum.Largest)

final case class DenseReferencePartialEigenSolver(
    fullSolver: SymmetricEigenSolver = LinalgSolvers.symmetricEigen,
    maximumOrder: Int = 256
) extends PartialSymmetricEigenSolver:
  require(maximumOrder > 0, "maximumOrder must be positive")

  override def decompose(
      operator: SymmetricOperator,
      rank: DecompositionRank,
      spectrum: PartialSpectrum
  ): Either[LinearAlgebraError, PartialSymmetricEigenResult] =
    if rank.value > operator.order then
      Left(LinearAlgebraError.InvalidDecompositionRank(rank.value, operator.order))
    else if operator.order > maximumOrder then
      Left(LinearAlgebraError.OperatorOrderUnsupported("dense reference partial eigensolver", operator.order, maximumOrder))
    else
      for
        dense <- operator.toDense
        full <- fullSolver.decompose(dense)
        partial <- PartialEigenOps.select(operator, full, rank.value, spectrum, "dense-reference", None)
      yield partial

private final class DenseMatrixLinearMap private (matrix: DoubleMatrix) extends LinearMap:
  val rows: Int = matrix.rows
  val cols: Int = matrix.cols

  def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    if input.rows != cols then Left(LinearMapError.DimensionMismatch(cols, input.rows))
    else Right(DoubleMatrix.multiply(matrix, input))

  def adjoint: LinearMap =
    DenseMatrixLinearMap(matrix.transpose)

private object DenseMatrixLinearMap:
  def apply(matrix: DoubleMatrix): DenseMatrixLinearMap =
    new DenseMatrixLinearMap(matrix)

private[scalafim] object PartialEigenOps:
  def select(
      operator: SymmetricOperator,
      full: SymmetricEigenResult,
      count: Int,
      spectrum: PartialSpectrum,
      backend: String,
      iterations: Option[Int]
  ): Either[LinearAlgebraError, PartialSymmetricEigenResult] =
    val order =
      spectrum match
        case PartialSpectrum.Largest => Vector.tabulate(count)(identity)
        case PartialSpectrum.Smallest => Vector.tabulate(count)(offset => full.values.length - 1 - offset)
    val eigenvalues = new Array[Double](count)
    val eigenvectors = new Array[Double](operator.order * count)
    var outputCol = 0
    while outputCol < count do
      val sourceCol = order(outputCol)
      eigenvalues(outputCol) = full.values(sourceCol)
      var row = 0
      while row < operator.order do
        eigenvectors(row * count + outputCol) = full.vectors(row, sourceCol)
        row += 1
      outputCol += 1

    val values = DoubleVector.unsafe(eigenvalues)
    val vectors = DoubleMatrix.unsafe(operator.order, count, eigenvectors)
    residualNorms(operator, values, vectors).map: residuals =>
      PartialSymmetricEigenResult(values, vectors, residuals, spectrum, backend, iterations)

  private def residualNorms(
      operator: SymmetricOperator,
      values: DoubleVector,
      vectors: DoubleMatrix
  ): Either[LinearAlgebraError, DoubleVector] =
    val residuals = new Array[Double](values.length)
    var col = 0
    var error = Option.empty[LinearAlgebraError]
    while col < values.length && error.isEmpty do
      val vector = vectors.col(col)
      operator.map.forward(vector) match
        case Left(value) =>
          error = Some(LinearAlgebraError.OperatorApplicationFailed(value.message))
        case Right(applied) =>
          var squared = 0.0
          var row = 0
          while row < applied.length do
            val difference = applied(row) - values(col) * vector(row)
            squared += difference * difference
            row += 1
          residuals(col) = Math.sqrt(squared)
      col += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(DoubleVector.unsafe(residuals))
