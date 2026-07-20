package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

opaque type SpectralRankTolerance = Double

object SpectralRankTolerance:
  def from(value: Double): Either[MultivarError, SpectralRankTolerance] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(MultivarError.InvalidTolerance("spectral rank", value))

  val default: SpectralRankTolerance = 1e-12

  private[multivar] def unsafe(value: Double): SpectralRankTolerance =
    require(value.isFinite && value >= 0.0, "spectral rank tolerance must be finite and non-negative")
    value

  extension (value: SpectralRankTolerance)
    inline def toDouble: Double = value

final case class RayleighRitzDiagnostics(
    generalizedResidual: Double,
    normalizationResidual: Double,
    spectralClusters: Vector[Vector[Int]],
    solver: String
):
  require(generalizedResidual.isFinite && generalizedResidual >= 0.0, "generalized residual must be finite and non-negative")
  require(normalizationResidual.isFinite && normalizationResidual >= 0.0, "normalization residual must be finite and non-negative")
  require(spectralClusters.nonEmpty && spectralClusters.flatten.nonEmpty, "spectral clusters must cover a non-empty spectrum")

final case class RayleighRitzResult(
    values: DVec,
    vectors: DMat,
    diagnostics: RayleighRitzDiagnostics
):
  require(values.length > 0 && values.length == vectors.cols, "Rayleigh-Ritz result must have a non-empty aligned spectrum")

/** Solver-independent lowering of a symmetric-definite generalized Rayleigh
  * problem. Statistical methods assemble operators; this object only validates
  * and executes `A W = B W Lambda` through the supplied Gale-backed capability.
  */
object GeneralizedRayleighRitz:
  def solve(
      numerator: DMat,
      denominator: DMat,
      components: ComponentCount,
      rankTolerance: SpectralRankTolerance = SpectralRankTolerance.default,
      tolerance: CertificateTolerance = CertificateTolerance.strict,
      solver: GeneralizedEigenSolver = DenseSolvers.generalizedEigen
  ): Either[MultivarError, RayleighRitzResult] =
    if numerator.rows != numerator.cols || denominator.rows != denominator.cols || numerator.rows != denominator.rows then
      Left(MultivarError.MatrixShapeMismatch("generalized Rayleigh operators must be square and shape-aligned"))
    else if components.value > numerator.rows then
      Left(MultivarError.InvalidComponentRequest(components.value, numerator.rows))
    else
      for
        _ <- MatrixOps.checkFinite("Rayleigh numerator", numerator)
        _ <- MatrixOps.checkFinite("Rayleigh denominator", denominator)
        raw <- LinalgErrorAdapter.adapt(solver.decompose(numerator, denominator, components))
        retained <- retainPositive(raw, rankTolerance)
        (values, unoriented) = retained
        vectors = orientColumns(unoriented)
        generalizedResidual = equationResidual(numerator, denominator, vectors, values)
        normalizationResidual = orthonormalityResidual(denominator, vectors)
        scale = frobenius(numerator) + maxAbs(values) * frobenius(denominator)
        _ <- withinTolerance("generalized Rayleigh equation", generalizedResidual, tolerance.threshold(scale))
        _ <- withinTolerance("generalized Rayleigh normalization", normalizationResidual, tolerance.threshold(values.length.toDouble))
      yield
        RayleighRitzResult(
          values,
          vectors,
          RayleighRitzDiagnostics(
            generalizedResidual,
            normalizationResidual,
            spectralClusters(values, tolerance),
            "gale.spectral.Eigen.eigSymmetricGeneralized"
          )
        )

  private def retainPositive(
      value: SymmetricEigenResult,
      tolerance: SpectralRankTolerance
  ): Either[MultivarError, (DVec, DMat)] =
    if value.values.length == 0 then Left(MultivarError.SolverFailed("generalized Rayleigh solver returned an empty spectrum"))
    else
      val leading = value.values(0)
      if !leading.isFinite then Left(MultivarError.NonFiniteValue("generalized eigenvalue", 0, leading))
      else
        val cutoff = tolerance.toDouble * Math.max(leading, 0.0)
        var retained = 0
        var invalid = Option.empty[MultivarError]
        while retained < value.values.length && invalid.isEmpty && value.values(retained) > cutoff do
          val current = value.values(retained)
          if !current.isFinite then invalid = Some(MultivarError.NonFiniteValue("generalized eigenvalue", retained, current))
          else retained += 1
        invalid match
          case Some(error) => Left(error)
          case None if retained == 0 => Left(MultivarError.SolverFailed("no positive generalized roots survived the rank tolerance"))
          case None => Right((MatrixOps.takeVector(value.values, retained), MatrixOps.takeColumns(value.vectors, retained)))

opaque type TraceRatioTolerance = Double

object TraceRatioTolerance:
  def from(value: Double): Either[MultivarError, TraceRatioTolerance] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(MultivarError.InvalidTolerance("trace ratio", value))

  val default: TraceRatioTolerance = 1e-10

  extension (value: TraceRatioTolerance)
    inline def toDouble: Double = value

opaque type TraceRatioIterations = Int

object TraceRatioIterations:
  def from(value: Int): Either[MultivarError, TraceRatioIterations] =
    if value > 0 then Right(value)
    else Left(MultivarError.InvalidComponentRequest(value, Int.MaxValue))

  val default: TraceRatioIterations = 200

  extension (value: TraceRatioIterations)
    inline def toInt: Int = value

final case class TraceRatioResult(
    value: Double,
    vectors: DMat,
    iterations: Int,
    stationarityResidual: Double,
    spectralClusters: Vector[Vector[Int]]
):
  require(value.isFinite, "trace ratio must be finite")
  require(vectors.cols > 0, "trace-ratio result must contain a frame")
  require(iterations > 0, "trace-ratio iteration count must be positive")
  require(stationarityResidual.isFinite && stationarityResidual >= 0.0, "trace-ratio residual must be finite and non-negative")

/** Generic trace-ratio optimization under Euclidean frame normalization.
  * The iteration repeatedly solves the symmetric problem `A - rho B`; it is a
  * reusable optimizer for any method whose program declares `TraceRatio`.
  */
object TraceRatioOptimization:
  def solve(
      numerator: DMat,
      denominator: DMat,
      components: ComponentCount,
      tolerance: TraceRatioTolerance = TraceRatioTolerance.default,
      maxIterations: TraceRatioIterations = TraceRatioIterations.default,
      solver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[MultivarError, TraceRatioResult] =
    if numerator.rows != numerator.cols || denominator.rows != denominator.cols || numerator.rows != denominator.rows then
      Left(MultivarError.MatrixShapeMismatch("trace-ratio operators must be square and shape-aligned"))
    else if components.value > numerator.rows then Left(MultivarError.InvalidComponentRequest(components.value, numerator.rows))
    else
      for
        _ <- MatrixOps.checkFinite("trace-ratio numerator", numerator)
        _ <- MatrixOps.checkFinite("trace-ratio denominator", denominator)
        initial <- leadingFrame(numerator, components, solver)
        initialRatio <- ratio(numerator, denominator, initial)
        result <- iterate(numerator, denominator, components, initial, initialRatio, tolerance, maxIterations, solver)
      yield result

  private def iterate(
      numerator: DMat,
      denominator: DMat,
      components: ComponentCount,
      initial: DMat,
      initialRatio: Double,
      tolerance: TraceRatioTolerance,
      maxIterations: TraceRatioIterations,
      solver: SymmetricEigenSolver
  ): Either[MultivarError, TraceRatioResult] =
    var frame = initial
    var current = initialRatio
    var iteration = 0
    var converged = false
    var failure = Option.empty[MultivarError]
    while iteration < maxIterations.toInt && !converged && failure.isEmpty do
      val shifted = MatrixOps.subtract(numerator, MatrixOps.scale(denominator, current))
      leadingFrame(shifted, components, solver) match
        case Left(error) => failure = Some(error)
        case Right(nextFrame) =>
          ratio(numerator, denominator, nextFrame) match
            case Left(error) => failure = Some(error)
            case Right(next) =>
              converged = Math.abs(next - current) <= tolerance.toDouble * Math.max(1.0, Math.abs(current))
              frame = nextFrame
              current = next
      iteration += 1
    failure match
      case Some(error) => Left(error)
      case None if !converged => Left(MultivarError.SolverFailed(s"trace-ratio iteration did not converge in ${maxIterations.toInt} steps"))
      case None =>
        val residual = traceRatioResidual(numerator, denominator, frame, current)
        Right(
          TraceRatioResult(
            current,
            orientColumns(frame),
            iteration,
            residual,
            spectralClustersOfFrame(numerator, denominator, frame, current)
          )
        )

  private def leadingFrame(
      matrix: DMat,
      components: ComponentCount,
      solver: SymmetricEigenSolver
  ): Either[MultivarError, DMat] =
    LinalgErrorAdapter.adapt(solver.decompose(matrix)).map(result => MatrixOps.takeColumns(result.vectors, components.value))

  private def ratio(numerator: DMat, denominator: DMat, frame: DMat): Either[MultivarError, Double] =
    val top = trace(GaleNumerics.multiply(frame.t, GaleNumerics.multiply(numerator, frame)))
    val bottom = trace(GaleNumerics.multiply(frame.t, GaleNumerics.multiply(denominator, frame)))
    if !top.isFinite || !bottom.isFinite then Left(MultivarError.NonFiniteValue("trace ratio", 0, top / bottom))
    else if bottom <= 0.0 then Left(MultivarError.NonInvertibleValue("trace-ratio denominator", 0, bottom))
    else Right(top / bottom)

  private def traceRatioResidual(numerator: DMat, denominator: DMat, frame: DMat, ratio: Double): Double =
    val shifted = MatrixOps.subtract(numerator, MatrixOps.scale(denominator, ratio))
    val projected = GaleNumerics.multiply(frame.t, GaleNumerics.multiply(shifted, frame))
    val residual = MatrixOps.subtract(GaleNumerics.multiply(shifted, frame), GaleNumerics.multiply(frame, projected))
    frobenius(residual)

  private def spectralClustersOfFrame(
      numerator: DMat,
      denominator: DMat,
      frame: DMat,
      ratio: Double
  ): Vector[Vector[Int]] =
    val compressed = GaleNumerics.multiply(
      frame.t,
      GaleNumerics.multiply(MatrixOps.subtract(numerator, MatrixOps.scale(denominator, ratio)), frame)
    )
    DenseSolvers.symmetricEigen.decompose(compressed).toOption match
      case Some(value) => spectralClusters(value.values, CertificateTolerance.strict)
      case None => Vector((0 until frame.cols).toVector)

private[multivar] def orientColumns(matrix: DMat): DMat =
  val out = matrix.copyData
  var col = 0
  while col < matrix.cols do
    var anchor = 0
    var row = 1
    while row < matrix.rows do
      if Math.abs(matrix(row, col)) > Math.abs(matrix(anchor, col)) then anchor = row
      row += 1
    if matrix(anchor, col) < 0.0 then
      row = 0
      while row < matrix.rows do
        out(row * matrix.cols + col) = -out(row * matrix.cols + col)
        row += 1
    col += 1
  GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

private[multivar] def equationResidual(numerator: DMat, denominator: DMat, vectors: DMat, values: DVec): Double =
  val left = GaleNumerics.multiply(numerator, vectors)
  val right = MatrixOps.scaleColumns(GaleNumerics.multiply(denominator, vectors), values)
  frobenius(MatrixOps.subtract(left, right))

private[multivar] def orthonormalityResidual(metric: DMat, vectors: DMat): Double =
  val gram = GaleNumerics.multiply(vectors.t, GaleNumerics.multiply(metric, vectors))
  frobenius(MatrixOps.subtract(gram, DMat.eye(gram.rows)))

private[multivar] def frobenius(matrix: DMat): Double =
  var squared = 0.0
  var row = 0
  while row < matrix.rows do
    var col = 0
    while col < matrix.cols do
      val value = matrix(row, col)
      squared += value * value
      col += 1
    row += 1
  Math.sqrt(squared)

private[multivar] def spectralClusters(values: DVec, tolerance: CertificateTolerance): Vector[Vector[Int]] =
  val out = Vector.newBuilder[Vector[Int]]
  var first = 0
  while first < values.length do
    val current = Vector.newBuilder[Int]
    current += first
    var next = first + 1
    while next < values.length && Math.abs(values(first) - values(next)) <= tolerance.threshold(Math.abs(values(first))) do
      current += next
      next += 1
    out += current.result()
    first = next
  out.result()

private[multivar] def trace(matrix: DMat): Double =
  var total = 0.0
  var index = 0
  while index < Math.min(matrix.rows, matrix.cols) do
    total += matrix(index, index)
    index += 1
  total

private[multivar] def maxAbs(values: DVec): Double =
  var maximum = 0.0
  var index = 0
  while index < values.length do
    maximum = Math.max(maximum, Math.abs(values(index)))
    index += 1
  maximum

private def withinTolerance(label: String, residual: Double, threshold: Double): Either[MultivarError, Unit] =
  if residual <= threshold then Right(())
  else Left(MultivarError.NumericalResidualExceeded(label, residual, threshold))
