package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

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

/** Runtime constructor specification for a symmetric positive-semidefinite
  * operator over one named axis.
  *
  * This is lifecycle/configuration data, not the semantic metric itself. A model
  * boundary must compile it to a role-refined `OpMetric` or `OpCometric` bound to
  * a nominal `SpaceEvidence` before it can enter an operator program. The same
  * p x p payload can describe a row or column metric only after that binding.
  * Estimators check `dim` against the data axis they weight.
  * Positive semi-definiteness beyond the structural checks is enforced spectrally
  * wherever a square root is taken (`MetricSqrt.factor`), so rank-deficient PSD
  * metrics are usable while indefinite ones fail with a typed error.
  */
sealed trait MetricSpec:
  /** Size of the axis this metric weights (n for a row metric, p for a column metric). */
  def dim: Int
  def storage: StorageKind
  /** Optional identity of the space this metric weights; checked when present. */
  def space: Option[MvSpace]

  def isIdentity: Boolean =
    this match
      case MetricSpec.Identity(_, _) => true
      case _                       => false

  def isDiagonal: Boolean =
    this match
      case MetricSpec.Identity(_, _) | MetricSpec.Diagonal(_, _) => true
      case _                                                 => false

  /** Value-based metric identity: same kind, same dimension, and exactly equal entries.
    *
    * Metric storage (`DVec`/`DMat`/`SparseMatrixView`) compares by
    * reference, so case-class equality on metrics is reference equality for every
    * non-identity kind; use this method to decide whether two separately built metrics
    * are the same metric. Exact double equality is intentional — "same metric" is an
    * identity claim across instances, not a numeric-tolerance claim. Space tags are
    * ignored; space compatibility is checked separately where it matters.
    */
  final def sameValues(other: MetricSpec): Boolean =
    (this eq other) || ((this, other) match
      case (MetricSpec.Identity(dim, _), MetricSpec.Identity(otherDim, _)) =>
        dim == otherDim
      case (MetricSpec.Diagonal(weights, _), MetricSpec.Diagonal(otherWeights, _)) =>
        MetricSpec.sameVectorValues(weights, otherWeights)
      case (MetricSpec.DenseSymmetric(matrix, _), MetricSpec.DenseSymmetric(otherMatrix, _)) =>
        MetricSpec.sameMatrixValues(matrix, otherMatrix)
      case (MetricSpec.SparseSymmetric(view, _), MetricSpec.SparseSymmetric(otherView, _)) =>
        MetricSpec.sameSparseValues(view, otherView)
      case _ =>
        false)

  /** M * input for a dim x k dense block. */
  def matvec(input: DMat): Either[MultivarError, DMat]

  /** M * input for a dim-length vector. */
  def applyVector(input: DVec): Either[MultivarError, DVec]

  /** x' M y for equal-length vectors. */
  def innerProduct(x: DVec, y: DVec): Either[MultivarError, Double]

  /** x' M x, non-negative up to roundoff for a PSD metric. */
  def quadNorm(x: DVec): Either[MultivarError, Double] =
    innerProduct(x, x)

  /** Sum of M_ij * g_ij over all entries; equals tr(M g) for symmetric g. */
  private[multivar] def contract(g: DMat): Either[MultivarError, Double]

  /** Materialize as a dense dim x dim matrix, subject to policy. */
  def toDense(policy: StoragePolicy = StoragePolicy.AllowDense): Either[MultivarError, DMat]

object MetricSpec:
  private val StructuralTolerance = 1e-10

  final case class Identity private[multivar] (dim: Int, space: Option[MvSpace]) extends MetricSpec:
    override def storage: StorageKind =
      StorageKind.Operator

    override def matvec(input: DMat): Either[MultivarError, DMat] =
      requireMatvecShape(dim, input).map(_ => input)

    override def applyVector(input: DVec): Either[MultivarError, DVec] =
      requireVectorShape(dim, input).map(_ => input)

    override def innerProduct(x: DVec, y: DVec): Either[MultivarError, Double] =
      requirePairShape(dim, x, y).map { _ =>
        var acc = 0.0
        var i = 0
        while i < dim do
          acc += x(i) * y(i)
          i += 1
        acc
      }

    override private[multivar] def contract(g: DMat): Either[MultivarError, Double] =
      requireContractShape(dim, g).map { _ =>
        var acc = 0.0
        var i = 0
        while i < dim do
          acc += g(i, i)
          i += 1
        acc
      }

    override def toDense(policy: StoragePolicy): Either[MultivarError, DMat] =
      Right(DMat.eye(dim))

  final case class Diagonal private[multivar] (weights: DVec, space: Option[MvSpace]) extends MetricSpec:
    override def dim: Int =
      weights.length

    override def storage: StorageKind =
      StorageKind.Operator

    override def matvec(input: DMat): Either[MultivarError, DMat] =
      requireMatvecShape(dim, input).map(_ => MatrixView.scaleRows(input, weights))

    override def applyVector(input: DVec): Either[MultivarError, DVec] =
      requireVectorShape(dim, input).map(_ => MatrixView.multiply(weights, input))

    override def innerProduct(x: DVec, y: DVec): Either[MultivarError, Double] =
      requirePairShape(dim, x, y).map { _ =>
        var acc = 0.0
        var i = 0
        while i < dim do
          acc += weights(i) * x(i) * y(i)
          i += 1
        acc
      }

    override private[multivar] def contract(g: DMat): Either[MultivarError, Double] =
      requireContractShape(dim, g).map { _ =>
        var acc = 0.0
        var i = 0
        while i < dim do
          acc += weights(i) * g(i, i)
          i += 1
        acc
      }

    override def toDense(policy: StoragePolicy): Either[MultivarError, DMat] =
      Right(MatrixOps.diagonal(weights))

  final case class DenseSymmetric private[multivar] (matrix: DMat, space: Option[MvSpace]) extends MetricSpec:
    override def dim: Int =
      matrix.rows

    override def storage: StorageKind =
      StorageKind.Dense

    override def matvec(input: DMat): Either[MultivarError, DMat] =
      requireMatvecShape(dim, input).map(_ => GaleNumerics.multiply(matrix, input))

    override def applyVector(input: DVec): Either[MultivarError, DVec] =
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
        GaleNumerics.vectorFromArray(out)
      }

    override def innerProduct(x: DVec, y: DVec): Either[MultivarError, Double] =
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

    override private[multivar] def contract(g: DMat): Either[MultivarError, Double] =
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

    override def toDense(policy: StoragePolicy): Either[MultivarError, DMat] =
      Right(matrix)

  /** Sparse symmetric metric over a fully stored symmetric pattern.
    *
    * The backing view must store BOTH triangles explicitly: every operation here
    * (matvec, inner products, contraction) iterates the stored entries as-is, so
    * upper-triangle-only storage silently halves every off-diagonal contribution.
    * `MetricValidation.Structural` rejects a missing mirror entry;
    * `MetricValidation.Trusted` skips that check entirely.
    */
  final case class SparseSymmetric private[multivar] (view: SparseMatrixView, space: Option[MvSpace]) extends MetricSpec:
    override def dim: Int =
      view.rows

    override def storage: StorageKind =
      StorageKind.Sparse

    override def matvec(input: DMat): Either[MultivarError, DMat] =
      requireMatvecShape(dim, input).flatMap(_ => view.rightMultiply(input))

    override def applyVector(input: DVec): Either[MultivarError, DVec] =
      requireVectorShape(dim, input).map { _ =>
        val out = new Array[Double](dim)
        view.foreachEntry((row, col, value) => out(row) += value * input(col))
        GaleNumerics.vectorFromArray(out)
      }

    override def innerProduct(x: DVec, y: DVec): Either[MultivarError, Double] =
      requirePairShape(dim, x, y).map { _ =>
        var acc = 0.0
        view.foreachEntry((row, col, value) => acc += value * x(row) * y(col))
        acc
      }

    override private[multivar] def contract(g: DMat): Either[MultivarError, Double] =
      requireContractShape(dim, g).map { _ =>
        var acc = 0.0
        view.foreachEntry((row, col, value) => acc += value * g(row, col))
        acc
      }

    override def toDense(policy: StoragePolicy): Either[MultivarError, DMat] =
      view.toDense(policy)

  def identity(dim: Int, space: Option[MvSpace] = None): Either[MultivarError, MetricSpec] =
    if dim <= 0 then Left(MultivarError.InvalidDimension("metric dimension", dim))
    else requireSpace(space, dim).map(_ => Identity(dim, space))

  /** Diagonal PSD metric. Weights within `StructuralTolerance` below zero are treated
    * as roundoff and clamped to exactly zero at construction, so every accepted
    * instance is factorizable by `MetricSqrt.factor` regardless of its (tighter)
    * tolerance; weights below that band are rejected as indefinite.
    */
  def diagonal(weights: DVec, space: Option[MvSpace] = None): Either[MultivarError, MetricSpec] =
    if weights.length <= 0 then Left(MultivarError.InvalidDimension("metric dimension", weights.length))
    else
      var i = 0
      var needsClamp = false
      var error = Option.empty[MultivarError]
      while i < weights.length && error.isEmpty do
        val value = weights(i)
        if !value.isFinite then error = Some(MultivarError.NonFiniteValue("diagonal metric", i, value))
        else if value < -StructuralTolerance then
          error = Some(MultivarError.NonPositiveSemiDefinite("diagonal metric", value))
        else if value < 0.0 then needsClamp = true
        i += 1
      error match
        case Some(value) => Left(value)
        case None =>
          requireSpace(space, weights.length).map { _ =>
            val cleaned =
              if !needsClamp then weights
              else
                val out = weights.copyData
                var j = 0
                while j < out.length do
                  if out(j) < 0.0 then out(j) = 0.0
                  j += 1
                GaleNumerics.vectorFromArray(out)
            Diagonal(cleaned, space)
          }

  /** The row metric induced by a whitening operator `W`: `D = W' W`.
    *
    * Row whitening remains design-conditioning geometry; this constructor is the
    * explicit bridge that lets its induced bilinear form serve as the `D` of a
    * duality diagram. Identity whitening stays operator-backed. A nontrivial
    * whitening is materialized once as a dense symmetric metric.
    */
  def fromRowWhitening(
      whitening: RowWhitening,
      space: Option[MvSpace] = None
  ): Either[MultivarError, MetricSpec] =
    whitening.mode match
      case RowWhiteningMode.Identity | RowWhiteningMode.GroupedIdentity =>
        identity(whitening.rows, space)
      case RowWhiteningMode.BlockCholesky =>
        for
          root <- whitening.whiten(DMat.eye(whitening.rows))
          metric = GaleNumerics.crossProduct(root)
          result <- denseSymmetric(metric, MetricValidation.Structural, space)
        yield result

  def denseSymmetric(
      matrix: DMat,
      validation: MetricValidation = MetricValidation.Structural,
      space: Option[MvSpace] = None
  ): Either[MultivarError, MetricSpec] =
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

  /** Build a sparse symmetric metric from a view that stores BOTH triangles.
    *
    * The metric consumes the stored entries directly, so a view holding only the
    * upper (or lower) triangle is not "symmetric by convention" here — it is a
    * different, asymmetric operator whose off-diagonals are silently halved.
    * `MetricValidation.Structural` (the default) rejects entries whose mirror is
    * absent; `MetricValidation.Trusted` performs shape checks only and MUST be given
    * fully mirrored storage.
    */
  def sparseSymmetric(
      view: SparseMatrixView,
      validation: MetricValidation = MetricValidation.Structural,
      space: Option[MvSpace] = None
  ): Either[MultivarError, MetricSpec] =
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

  private[multivar] def unsafeIdentity(dim: Int, space: Option[MvSpace] = None): MetricSpec =
    Identity(dim, space)

  private[multivar] def unsafeDiagonal(weights: DVec, space: Option[MvSpace] = None): MetricSpec =
    Diagonal(weights, space)

  private[multivar] def unsafeDenseSymmetric(matrix: DMat, space: Option[MvSpace] = None): MetricSpec =
    DenseSymmetric(matrix, space)

  private[multivar] def unsafeSparseSymmetric(view: SparseMatrixView, space: Option[MvSpace] = None): MetricSpec =
    SparseSymmetric(view, space)

  private def sameVectorValues(left: DVec, right: DVec): Boolean =
    if left.length != right.length then false
    else
      var i = 0
      var same = true
      while same && i < left.length do
        same = left(i) == right(i)
        i += 1
      same

  private def sameMatrixValues(left: DMat, right: DMat): Boolean =
    if left.rows != right.rows || left.cols != right.cols then false
    else
      var row = 0
      var same = true
      while same && row < left.rows do
        var col = 0
        while same && col < left.cols do
          same = left(row, col) == right(row, col)
          col += 1
        row += 1
      same

  /** Entrywise equality over both stored patterns, so structural zeros on either
    * side compare against the other side's value.
    */
  private def sameSparseValues(left: SparseMatrixView, right: SparseMatrixView): Boolean =
    if left.rows != right.rows || left.cols != right.cols then false
    else
      var same = true
      left.foreachEntry { (row, col, value) =>
        if same && right.valueAt(row, col) != value then same = false
      }
      right.foreachEntry { (row, col, value) =>
        if same && left.valueAt(row, col) != value then same = false
      }
      same

  private def structuralDense(matrix: DMat): Either[MultivarError, Unit] =
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
            error = Some(
              if mirrored == 0.0 then
                MultivarError.MetricMismatch(
                  s"sparse metric entry ($row, $col) = $value has no stored mirror at ($col, $row); " +
                    "SparseSymmetric requires both triangles stored explicitly"
                )
              else MultivarError.NonSymmetricMatrix(row, col, value, mirrored)
            )
    }
    error match
      case Some(value) => Left(value)
      case None        => Right(())

  private def spectralPsd(
      matrix: DMat,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double,
      role: String
  ): Either[MultivarError, Unit] =
    LinalgErrorAdapter.adapt(eigenSolver.decompose(matrix)).flatMap { eigen =>
      val largest = Math.max(eigen.values(0), 0.0)
      val cutoff = tolerance * Math.max(1.0, largest)
      val smallest = eigen.values(eigen.values.length - 1)
      if smallest < -cutoff then Left(MultivarError.NonPositiveSemiDefinite(role, smallest))
      else Right(())
    }

  private def requireNonNegativeDiagonal(matrix: DMat): Either[MultivarError, Unit] =
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

  private def requireMatvecShape(dim: Int, input: DMat): Either[MultivarError, Unit] =
    if input.rows == dim then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"metric of dimension $dim cannot multiply a ${input.rows}x${input.cols} input"
        )
      )

  private def requireVectorShape(dim: Int, input: DVec): Either[MultivarError, Unit] =
    if input.length == dim then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"metric of dimension $dim cannot multiply a vector of length ${input.length}"
        )
      )

  private def requireContractShape(dim: Int, g: DMat): Either[MultivarError, Unit] =
    if g.rows == dim && g.cols == dim then Right(())
    else
      Left(
        MultivarError.MatrixShapeMismatch(
          s"metric contraction of dimension $dim got a ${g.rows}x${g.cols} matrix"
        )
      )

  private def requirePairShape(dim: Int, x: DVec, y: DVec): Either[MultivarError, Unit] =
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
  case Diagonal(values: DVec)
  case Dense(matrix: DMat)

  def dim: Int =
    this match
      case Identity(size)   => size
      case Diagonal(values) => values.length
      case Dense(matrix)    => matrix.rows

  /** Op * input. */
  def applyLeft(input: DMat): DMat =
    this match
      case Identity(_)      => input
      case Diagonal(values) => MatrixView.scaleRows(input, values)
      case Dense(matrix)    => GaleNumerics.multiply(matrix, input)

  /** input * Op. */
  def applyRight(input: DMat): DMat =
    this match
      case Identity(_)      => input
      case Diagonal(values) => MetricOperator.scaleColumnsDense(input, values)
      case Dense(matrix)    => GaleNumerics.multiply(input, matrix)

private[multivar] object MetricOperator:
  private[multivar] def scaleColumnsDense(matrix: DMat, scale: DVec): DMat =
    require(matrix.cols == scale.length, "scale length must match matrix columns")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) *= scale(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

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
      metric: MetricSpec,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double,
      policy: StoragePolicy,
      role: String = "metric"
  ): Either[MultivarError, MetricRoots] =
    metric match
      case MetricSpec.Identity(dim, _) =>
        Right(MetricRoots(MetricOperator.Identity(dim), MetricOperator.Identity(dim), dim))
      case MetricSpec.Diagonal(weights, _) =>
        diagonalRoots(weights, tolerance, role)
      case MetricSpec.DenseSymmetric(matrix, _) =>
        denseRoots(matrix, eigenSolver, tolerance, role)
      case MetricSpec.SparseSymmetric(view, _) =>
        policy match
          case StoragePolicy.AllowDense =>
            view.toDense(StoragePolicy.AllowDense).flatMap(denseRoots(_, eigenSolver, tolerance, role))
          case _ =>
            Left(MultivarError.DensificationRejected(s"$role square root", StorageKind.Sparse))

  /** Factor an already materialized typed metric without demoting it to the
    * legacy `MetricSpec` hierarchy. This is the canonical entry point for
    * operator-program consumers; storage policy is enforced before the dense
    * value reaches this method.
    */
  def factorDense(
      metric: DMat,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double,
      role: String = "metric"
  ): Either[MultivarError, MetricRoots] =
    if metric.rows != metric.cols then
      Left(MultivarError.MatrixShapeMismatch(s"$role must be square, got ${metric.rows}x${metric.cols}"))
    else
      for
        _ <- MatrixOps.checkFinite(role, metric)
        _ <- MatrixOps.checkSymmetric(metric, tolerance)
        roots <- denseRoots(metric, eigenSolver, tolerance, role)
      yield roots

  private def diagonalRoots(
      weights: DVec,
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
            MetricOperator.Diagonal(GaleNumerics.vectorFromArray(half)),
            MetricOperator.Diagonal(GaleNumerics.vectorFromArray(pinv)),
            rank
          )
        )

  private def denseRoots(
      matrix: DMat,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double,
      role: String
  ): Either[MultivarError, MetricRoots] =
    LinalgErrorAdapter.adapt(eigenSolver.decompose(matrix)).flatMap { eigen =>
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
          val half = rebuild(eigen.vectors, GaleNumerics.vectorFromArray(halfDiag))
          val pinv = rebuild(eigen.vectors, GaleNumerics.vectorFromArray(pinvDiag))
          Right(MetricRoots(MetricOperator.Dense(half), MetricOperator.Dense(pinv), rank))
    }

  /** V diag(values) V'. */
  private def rebuild(vectors: DMat, values: DVec): DMat =
    val scaled = MetricOperator.scaleColumnsDense(vectors, values)
    GaleNumerics.multiply(scaled, vectors.transpose)
