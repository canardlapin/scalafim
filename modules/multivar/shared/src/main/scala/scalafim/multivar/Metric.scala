package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

/** Validation tier applied when constructing a metric from user-supplied storage. */
enum MetricValidation:
  /** Finiteness, symmetry, and a non-negative diagonal; does not prove positive semi-definiteness. */
  case Structural
  /** Structural checks plus a full spectral PSD test (one eigendecomposition of the metric). */
  case StrictPsd(
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      tolerance: Double = 1e-10
  )
  /** Shape checks only; trusted input for hot paths. */
  case Trusted

/** Symmetric positive semi-definite bilinear form over one axis of a data matrix.
  *
  * A metric is axis-agnostic: the same p x p form can weight the columns of X or the
  * rows of X transposed. Estimators check `dim` against the data axis they weight.
  * Positive semi-definiteness beyond the structural checks is enforced spectrally
  * wherever a square root is taken (`MetricSqrt.factor`), so rank-deficient PSD
  * metrics are usable while indefinite ones fail with a typed error.
  */
sealed trait MvMetric:
  /** Size of the axis this metric weights (n for a row metric, p for a column metric). */
  def dim: Int
  def storage: StorageKind
  /** Optional identity of the space this metric weights; checked when present. */
  def space: Option[MvSpace]

  def isIdentity: Boolean =
    this match
      case MvMetric.Identity(_, _) => true
      case _                       => false

  def isDiagonal: Boolean =
    this match
      case MvMetric.Identity(_, _) | MvMetric.Diagonal(_, _) => true
      case _                                                 => false

  /** M * input for a dim x k dense block. */
  def matvec(input: DoubleMatrix): Either[MultivarError, DoubleMatrix]

  /** M * input for a dim-length vector. */
  def applyVector(input: DoubleVector): Either[MultivarError, DoubleVector]

  /** x' M y for equal-length vectors. */
  def innerProduct(x: DoubleVector, y: DoubleVector): Either[MultivarError, Double]

  /** x' M x, non-negative up to roundoff for a PSD metric. */
  def quadNorm(x: DoubleVector): Either[MultivarError, Double] =
    innerProduct(x, x)

  /** Sum of M_ij * g_ij over all entries; equals tr(M g) for symmetric g. */
  private[multivar] def contract(g: DoubleMatrix): Either[MultivarError, Double]

  /** Materialize as a dense dim x dim matrix, subject to policy. */
  def toDense(policy: StoragePolicy = StoragePolicy.AllowDense): Either[MultivarError, DoubleMatrix]

object MvMetric:
  private val StructuralTolerance = 1e-10

  final case class Identity private[multivar] (dim: Int, space: Option[MvSpace]) extends MvMetric:
    override def storage: StorageKind =
      StorageKind.Operator

    override def matvec(input: DoubleMatrix): Either[MultivarError, DoubleMatrix] =
      requireMatvecShape(dim, input).map(_ => input)

    override def applyVector(input: DoubleVector): Either[MultivarError, DoubleVector] =
      requireVectorShape(dim, input).map(_ => input)

    override def innerProduct(x: DoubleVector, y: DoubleVector): Either[MultivarError, Double] =
      requirePairShape(dim, x, y).map { _ =>
        var acc = 0.0
        var i = 0
        while i < dim do
          acc += x(i) * y(i)
          i += 1
        acc
      }

    override private[multivar] def contract(g: DoubleMatrix): Either[MultivarError, Double] =
      requireContractShape(dim, g).map { _ =>
        var acc = 0.0
        var i = 0
        while i < dim do
          acc += g(i, i)
          i += 1
        acc
      }

    override def toDense(policy: StoragePolicy): Either[MultivarError, DoubleMatrix] =
      Right(DoubleMatrix.eye(dim))

  final case class Diagonal private[multivar] (weights: DoubleVector, space: Option[MvSpace]) extends MvMetric:
    override def dim: Int =
      weights.length

    override def storage: StorageKind =
      StorageKind.Operator

    override def matvec(input: DoubleMatrix): Either[MultivarError, DoubleMatrix] =
      requireMatvecShape(dim, input).map(_ => MatrixView.scaleRows(input, weights))

    override def applyVector(input: DoubleVector): Either[MultivarError, DoubleVector] =
      requireVectorShape(dim, input).map(_ => MatrixView.multiply(weights, input))

    override def innerProduct(x: DoubleVector, y: DoubleVector): Either[MultivarError, Double] =
      requirePairShape(dim, x, y).map { _ =>
        var acc = 0.0
        var i = 0
        while i < dim do
          acc += weights(i) * x(i) * y(i)
          i += 1
        acc
      }

    override private[multivar] def contract(g: DoubleMatrix): Either[MultivarError, Double] =
      requireContractShape(dim, g).map { _ =>
        var acc = 0.0
        var i = 0
        while i < dim do
          acc += weights(i) * g(i, i)
          i += 1
        acc
      }

    override def toDense(policy: StoragePolicy): Either[MultivarError, DoubleMatrix] =
      Right(MatrixOps.diagonal(weights))

  final case class DenseSymmetric private[multivar] (matrix: DoubleMatrix, space: Option[MvSpace]) extends MvMetric:
    override def dim: Int =
      matrix.rows

    override def storage: StorageKind =
      StorageKind.Dense

    override def matvec(input: DoubleMatrix): Either[MultivarError, DoubleMatrix] =
      requireMatvecShape(dim, input).map(_ => DoubleMatrix.multiply(matrix, input))

    override def applyVector(input: DoubleVector): Either[MultivarError, DoubleVector] =
      requireVectorShape(dim, input).map { _ =>
        val out = new Array[Double](dim)
        var row = 0
        while row < dim do
          var acc = 0.0
          var col = 0
          while col < dim do
            acc += matrix(row, col) * input(col)
            col += 1
          out(row) = acc
          row += 1
        DoubleVector.unsafe(out)
      }

    override def innerProduct(x: DoubleVector, y: DoubleVector): Either[MultivarError, Double] =
      requirePairShape(dim, x, y).map { _ =>
        var acc = 0.0
        var row = 0
        while row < dim do
          var inner = 0.0
          var col = 0
          while col < dim do
            inner += matrix(row, col) * y(col)
            col += 1
          acc += x(row) * inner
          row += 1
        acc
      }

    override private[multivar] def contract(g: DoubleMatrix): Either[MultivarError, Double] =
      requireContractShape(dim, g).map { _ =>
        var acc = 0.0
        var row = 0
        while row < dim do
          var col = 0
          while col < dim do
            acc += matrix(row, col) * g(row, col)
            col += 1
          row += 1
        acc
      }

    override def toDense(policy: StoragePolicy): Either[MultivarError, DoubleMatrix] =
      Right(matrix)

  final case class SparseSymmetric private[multivar] (view: SparseMatrixView, space: Option[MvSpace]) extends MvMetric:
    override def dim: Int =
      view.rows

    override def storage: StorageKind =
      StorageKind.Sparse

    override def matvec(input: DoubleMatrix): Either[MultivarError, DoubleMatrix] =
      requireMatvecShape(dim, input).flatMap(_ => view.rightMultiply(input))

    override def applyVector(input: DoubleVector): Either[MultivarError, DoubleVector] =
      requireVectorShape(dim, input).map { _ =>
        val out = new Array[Double](dim)
        view.foreachEntry((row, col, value) => out(row) += value * input(col))
        DoubleVector.unsafe(out)
      }

    override def innerProduct(x: DoubleVector, y: DoubleVector): Either[MultivarError, Double] =
      requirePairShape(dim, x, y).map { _ =>
        var acc = 0.0
        view.foreachEntry((row, col, value) => acc += value * x(row) * y(col))
        acc
      }

    override private[multivar] def contract(g: DoubleMatrix): Either[MultivarError, Double] =
      requireContractShape(dim, g).map { _ =>
        var acc = 0.0
        view.foreachEntry((row, col, value) => acc += value * g(row, col))
        acc
      }

    override def toDense(policy: StoragePolicy): Either[MultivarError, DoubleMatrix] =
      view.toDense(policy)

  def identity(dim: Int, space: Option[MvSpace] = None): Either[MultivarError, MvMetric] =
    if dim <= 0 then Left(MultivarError.InvalidDimension("metric dimension", dim))
    else requireSpace(space, dim).map(_ => Identity(dim, space))

  def diagonal(weights: DoubleVector, space: Option[MvSpace] = None): Either[MultivarError, MvMetric] =
    if weights.length <= 0 then Left(MultivarError.InvalidDimension("metric dimension", weights.length))
    else
      var i = 0
      var error = Option.empty[MultivarError]
      while i < weights.length && error.isEmpty do
        val value = weights(i)
        if !value.isFinite then error = Some(MultivarError.NonFiniteValue("diagonal metric", i, value))
        else if value < -StructuralTolerance then
          error = Some(MultivarError.NonPositiveSemiDefinite("diagonal metric", value))
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => requireSpace(space, weights.length).map(_ => Diagonal(weights, space))

  def denseSymmetric(
      matrix: DoubleMatrix,
      validation: MetricValidation = MetricValidation.Structural,
      space: Option[MvSpace] = None
  ): Either[MultivarError, MvMetric] =
    for
      _ <- requireSquare(matrix.rows, matrix.cols)
      _ <- validation match
        case MetricValidation.Trusted =>
          Right(())
        case MetricValidation.Structural =>
          structuralDense(matrix)
        case MetricValidation.StrictPsd(eigenSolver, tolerance) =>
          structuralDense(matrix).flatMap(_ => spectralPsd(matrix, eigenSolver, tolerance, "dense metric"))
      _ <- requireSpace(space, matrix.rows)
    yield DenseSymmetric(matrix, space)

  def sparseSymmetric(
      view: SparseMatrixView,
      validation: MetricValidation = MetricValidation.Structural,
      space: Option[MvSpace] = None
  ): Either[MultivarError, MvMetric] =
    for
      _ <- requireSquare(view.rows, view.cols)
      _ <- validation match
        case MetricValidation.Trusted =>
          Right(())
        case MetricValidation.Structural =>
          structuralSparse(view)
        case MetricValidation.StrictPsd(eigenSolver, tolerance) =>
          for
            _ <- structuralSparse(view)
            dense <- view.toDense(StoragePolicy.AllowDense)
            _ <- spectralPsd(dense, eigenSolver, tolerance, "sparse metric")
          yield ()
      _ <- requireSpace(space, view.rows)
    yield SparseSymmetric(view, space)

  private[multivar] def unsafeIdentity(dim: Int, space: Option[MvSpace] = None): MvMetric =
    Identity(dim, space)

  private[multivar] def unsafeDiagonal(weights: DoubleVector, space: Option[MvSpace] = None): MvMetric =
    Diagonal(weights, space)

  private[multivar] def unsafeDenseSymmetric(matrix: DoubleMatrix, space: Option[MvSpace] = None): MvMetric =
    DenseSymmetric(matrix, space)

  private[multivar] def unsafeSparseSymmetric(view: SparseMatrixView, space: Option[MvSpace] = None): MvMetric =
    SparseSymmetric(view, space)

  private def structuralDense(matrix: DoubleMatrix): Either[MultivarError, Unit] =
    for
      _ <- MatrixOps.checkFinite("dense metric", matrix)
      _ <- MatrixOps.checkSymmetric(matrix, StructuralTolerance)
      _ <- requireNonNegativeDiagonal(matrix)
    yield ()

  private def structuralSparse(view: SparseMatrixView): Either[MultivarError, Unit] =
    var error = Option.empty[MultivarError]
    view.foreachEntry { (row, col, value) =>
      if error.isEmpty then
        if row == col then
          if value < -StructuralTolerance then
            error = Some(MultivarError.NonPositiveSemiDefinite("sparse metric", value))
        else
          val mirrored = view.valueAt(col, row)
          if Math.abs(value - mirrored) > StructuralTolerance then
            error = Some(MultivarError.NonSymmetricMatrix(row, col, value, mirrored))
    }
    error match
      case Some(value) => Left(value)
      case None        => Right(())

  private def spectralPsd(
      matrix: DoubleMatrix,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double,
      role: String
  ): Either[MultivarError, Unit] =
    eigenSolver.decompose(matrix).flatMap { eigen =>
      val largest = Math.max(eigen.values(0), 0.0)
      val cutoff = tolerance * Math.max(1.0, largest)
      val smallest = eigen.values(eigen.values.length - 1)
      if smallest < -cutoff then Left(MultivarError.NonPositiveSemiDefinite(role, smallest))
      else Right(())
    }

  private def requireNonNegativeDiagonal(matrix: DoubleMatrix): Either[MultivarError, Unit] =
    var i = 0
    var error = Option.empty[MultivarError]
    while i < matrix.rows && error.isEmpty do
      val value = matrix(i, i)
      if value < -StructuralTolerance then
        error = Some(MultivarError.NonPositiveSemiDefinite("dense metric", value))
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(())

  private def requireSquare(rows: Int, cols: Int): Either[MultivarError, Unit] =
    if rows <= 0 then Left(MultivarError.InvalidDimension("metric dimension", rows))
    else if rows != cols then
      Left(MultivarError.MatrixShapeMismatch(s"metric must be square, got ${rows}x${cols}"))
    else Right(())

  private def requireSpace(space: Option[MvSpace], dim: Int): Either[MultivarError, Unit] =
    space match
      case Some(value) if value.size != dim =>
        Left(
          MultivarError.MatrixShapeMismatch(
            s"metric space '${value.id.value}' has size ${value.size} but the metric dimension is $dim"
          )
        )
      case _ =>
        Right(())

  private def requireMatvecShape(dim: Int, input: DoubleMatrix): Either[MultivarError, Unit] =
    if input.rows == dim then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"metric of dimension $dim cannot multiply a ${input.rows}x${input.cols} input"
        )
      )

  private def requireVectorShape(dim: Int, input: DoubleVector): Either[MultivarError, Unit] =
    if input.length == dim then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"metric of dimension $dim cannot multiply a vector of length ${input.length}"
        )
      )

  private def requireContractShape(dim: Int, g: DoubleMatrix): Either[MultivarError, Unit] =
    if g.rows == dim && g.cols == dim then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"metric contraction of dimension $dim got a ${g.rows}x${g.cols} matrix"
        )
      )

  private def requirePairShape(dim: Int, x: DoubleVector, y: DoubleVector): Either[MultivarError, Unit] =
    if x.length == dim && y.length == dim then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"metric inner product of dimension $dim got vectors of length ${x.length} and ${y.length}"
        )
      )

/** Square-root factor of a metric applied as a linear operator. */
private[multivar] enum MetricOperator:
  case Identity(size: Int)
  case Diagonal(values: DoubleVector)
  case Dense(matrix: DoubleMatrix)

  def dim: Int =
    this match
      case Identity(size)   => size
      case Diagonal(values) => values.length
      case Dense(matrix)    => matrix.rows

  /** Op * input. */
  def applyLeft(input: DoubleMatrix): DoubleMatrix =
    this match
      case Identity(_)      => input
      case Diagonal(values) => MatrixView.scaleRows(input, values)
      case Dense(matrix)    => DoubleMatrix.multiply(matrix, input)

  /** input * Op. */
  def applyRight(input: DoubleMatrix): DoubleMatrix =
    this match
      case Identity(_)      => input
      case Diagonal(values) => MetricOperator.scaleColumnsDense(input, values)
      case Dense(matrix)    => DoubleMatrix.multiply(input, matrix)

private[multivar] object MetricOperator:
  private[multivar] def scaleColumnsDense(matrix: DoubleMatrix, scale: DoubleVector): DoubleMatrix =
    require(matrix.cols == scale.length, "scale length must match matrix columns")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) *= scale(col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

/** Pseudo square-root pair of a PSD metric: half = M^{1/2}, pinvHalf = M^{-1/2} on the range. */
private[multivar] final case class MetricRoots(half: MetricOperator, pinvHalf: MetricOperator, rank: Int)

private[multivar] object MetricSqrt:
  /** Factor a metric into half / pseudo-inverse-half operators.
    *
    * Eigenvalues within `tolerance * max(1, lambdaMax)` of zero are treated as null
    * directions (pseudo-inverted to zero); eigenvalues below that band are indefinite
    * and rejected. Unlike `MatrixOps.inverseSquareRoot`, rank deficiency is allowed.
    */
  def factor(
      metric: MvMetric,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double,
      policy: StoragePolicy,
      role: String = "metric"
  ): Either[MultivarError, MetricRoots] =
    metric match
      case MvMetric.Identity(dim, _) =>
        Right(MetricRoots(MetricOperator.Identity(dim), MetricOperator.Identity(dim), dim))
      case MvMetric.Diagonal(weights, _) =>
        diagonalRoots(weights, tolerance, role)
      case MvMetric.DenseSymmetric(matrix, _) =>
        denseRoots(matrix, eigenSolver, tolerance, role)
      case MvMetric.SparseSymmetric(view, _) =>
        policy match
          case StoragePolicy.AllowDense =>
            view.toDense(StoragePolicy.AllowDense).flatMap(denseRoots(_, eigenSolver, tolerance, role))
          case _ =>
            Left(MultivarError.DensificationRejected(s"$role square root", StorageKind.Sparse))

  private def diagonalRoots(
      weights: DoubleVector,
      tolerance: Double,
      role: String
  ): Either[MultivarError, MetricRoots] =
    var largest = 0.0
    var i = 0
    while i < weights.length do
      if weights(i) > largest then largest = weights(i)
      i += 1
    val cutoff = tolerance * Math.max(1.0, largest)

    val half = new Array[Double](weights.length)
    val pinv = new Array[Double](weights.length)
    var rank = 0
    i = 0
    var error = Option.empty[MultivarError]
    while i < weights.length && error.isEmpty do
      val value = weights(i)
      if value < -cutoff then error = Some(MultivarError.NonPositiveSemiDefinite(role, value))
      else if value > cutoff then
        half(i) = Math.sqrt(value)
        pinv(i) = 1.0 / half(i)
        rank += 1
      i += 1
    error match
      case Some(value) => Left(value)
      case None =>
        Right(
          MetricRoots(
            MetricOperator.Diagonal(DoubleVector.unsafe(half)),
            MetricOperator.Diagonal(DoubleVector.unsafe(pinv)),
            rank
          )
        )

  private def denseRoots(
      matrix: DoubleMatrix,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double,
      role: String
  ): Either[MultivarError, MetricRoots] =
    eigenSolver.decompose(matrix).flatMap { eigen =>
      val n = eigen.values.length
      val largest = Math.max(eigen.values(0), 0.0)
      val cutoff = tolerance * Math.max(1.0, largest)

      val halfDiag = new Array[Double](n)
      val pinvDiag = new Array[Double](n)
      var rank = 0
      var i = 0
      var error = Option.empty[MultivarError]
      while i < n && error.isEmpty do
        val value = eigen.values(i)
        if value < -cutoff then error = Some(MultivarError.NonPositiveSemiDefinite(role, value))
        else if value > cutoff then
          halfDiag(i) = Math.sqrt(value)
          pinvDiag(i) = 1.0 / halfDiag(i)
          rank += 1
        i += 1
      error match
        case Some(value) => Left(value)
        case None =>
          val half = rebuild(eigen.vectors, DoubleVector.unsafe(halfDiag))
          val pinv = rebuild(eigen.vectors, DoubleVector.unsafe(pinvDiag))
          Right(MetricRoots(MetricOperator.Dense(half), MetricOperator.Dense(pinv), rank))
    }

  /** V diag(values) V'. */
  private def rebuild(vectors: DoubleMatrix, values: DoubleVector): DoubleMatrix =
    val scaled = MetricOperator.scaleColumnsDense(vectors, values)
    DoubleMatrix.multiply(scaled, vectors.transpose)
