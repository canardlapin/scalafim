package scalafim.fmri.mvpa.pattern

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.spectral.SpectralBackend.given
import multivar.family.spectral.{FactorRotation, PromaxReceipt, RotationOptions, RotationReceipt}
import multivar.core.SpaceRole
import scalafim.fmri.mvpa.{AxisDigest, AxisRef}

/** The two display coordinate systems are nominally distinct even when they
  * have the same component count.  They name interpretation coordinates, not
  * a replacement statistical fit. */
final class BrainDisplayAxes private[pattern] (val axis: AxisRef[String], val sourceIdentity: String, val transformIdentity: String)
final class TargetDisplayAxes private[pattern] (val axis: AxisRef[String], val sourceIdentity: String, val transformIdentity: String)

enum PatternRotationError:
  case Shape(detail: String)
  case NonFinite(detail: String)
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case Upstream(detail: String)
  case Condition(number: Double, limit: Double)
  case ScoreCovariance(detail: String)

enum ScoreCovarianceBasis:
  /** Covariance of the original factor coefficients, which transform as B^-1.
    * This is not covariance of raw filter scores, which transform as B^T. */
  case ActualFactorCoordinates(matrix: DMat)
  /** The caller explicitly asserts covariance I for original factor coefficients. */
  case ExplicitlyWhitenedFactorIdentity

enum PatternRotationMethod:
  case Varimax
  case Promax
  case Supplied

enum RotationEvidence:
  case Varimax(receipt: RotationReceipt)
  case Promax(receipt: PromaxReceipt)
  case Supplied(identity: String)

/** Budget covers matrices allocated and retained by this adapter, excluding
  * private Gale and Multivar kernel workspace. */
final case class PatternCoordinateRotationPolicy(
    upstream: RotationOptions = RotationOptions(),
    maximumOwnedCells: Long = 1_000_000L,
    maximumCondition: Double = 1e8
)

/** An explanatory coordinate view.  `artifact` and `prediction` are retained
  * unchanged: classification, decoding, and encoding delegate to the original
  * admitted predictive object, preserving its intercepts, priors and heads.
  */
final class PatternCoordinateRotation[N, Q, R] private[pattern] (
    val artifact: PatternArtifact,
    val prediction: PatternPrediction[N, Q, R],
    val brainAxes: BrainDisplayAxes,
    val targetAxes: TargetDisplayAxes,
    val method: PatternRotationMethod,
    val brainTransform: DMat,
    val targetTransform: DMat,
    val brainInverse: DMat,
    val targetInverse: DMat,
    val middle: DMat,
    val neuralByBrainDisplay: DMat,
    val targetByTargetDisplay: DMat,
    val originalScoreCovariance: DMat,
    val rotatedScoreCovariance: DMat,
    val factorCorrelation: DMat,
    val brainReceipt: RotationEvidence,
    val targetReceipt: RotationEvidence,
    val conditionNumber: Double,
    val rawFilters: DMat
):
  /** Raw filters transform as P B, while raw scores h transform as B' u. */
  val rawScoreTransform: DMat = brainTransform.t
  /** Calibrated filters and calibrated scores use inverse-transpose and inverse
    * respectively; they are intentionally not aliases of raw quantities. */
  val calibratedFilterTransform: DMat = brainInverse.t
  val calibratedScoreTransform: DMat = brainInverse

  /** Materialize A' middle C'-transpose only under a caller-supplied bound.
    * The rotation itself never retains the potentially large neural-by-target map. */
  def materializeForward(maximumCells: Long): Either[PatternRotationError, DMat] =
    val cells = BigInt(neuralByBrainDisplay.rows) * targetByTargetDisplay.rows
    if maximumCells < 0L || cells > maximumCells || cells > Int.MaxValue then Left(PatternRotationError.Budget(cells, maximumCells))
    else
      val out = neuralByBrainDisplay * middle * targetByTargetDisplay.t
      if PatternCoordinateRotation.finite(out) then Right(out) else Left(PatternRotationError.NonFinite("rotated effective forward map"))

  def rawScores(original: DMat, maximumCells: Long = 1_000_000L): Either[PatternRotationError, DMat] =
    scores(original, rawScoreTransform, maximumCells, "raw")

  def calibratedScores(original: DMat, maximumCells: Long = 1_000_000L): Either[PatternRotationError, DMat] =
    scores(original, calibratedScoreTransform, maximumCells, "calibrated")

  private def scores(original: DMat, transform: DMat, maximumCells: Long, role: String): Either[PatternRotationError, DMat] =
    val cells = BigInt(transform.rows) * original.cols
    if original.rows != transform.cols || original.cols <= 0 then Left(PatternRotationError.Shape(s"$role score shape"))
    else if maximumCells < 0L || cells > maximumCells || cells > Int.MaxValue then Left(PatternRotationError.Budget(cells, maximumCells))
    else if !PatternCoordinateRotation.finite(original) then Left(PatternRotationError.NonFinite(s"$role original scores"))
    else
      val result = transform * original
      if !PatternCoordinateRotation.finite(result) then Left(PatternRotationError.NonFinite(s"$role rotated scores")) else Right(result)

  /** The original predictive heads are the invariant source of predictions. */
  def encode(values: AxisValues[?]) = prediction.encode(values)
  def classify(values: AxisValues[?]) = prediction.classify(values)
  def decode(values: AxisValues[?]) = prediction.decode(values)

object PatternCoordinateRotation:
  /** Admit independently supplied display transforms.  Both transforms are
    * checked by Gale's converged full SVD before any factorized readout is made. */
  def supplied[N, Q, R](prediction: PatternPrediction[N, Q, R], brain: DMat, target: DMat,
      scoreCovariance: ScoreCovarianceBasis, policy: PatternCoordinateRotationPolicy = PatternCoordinateRotationPolicy()
  ): Either[PatternRotationError, PatternCoordinateRotation[N, Q, R]] =
    for
      _ <- preflight(prediction, policy)
      _ <- if brain.rows == prediction.factors.componentAxis.size && brain.cols == brain.rows && target.rows == brain.rows && target.cols == brain.rows then Right(())
        else Left(PatternRotationError.Shape("supplied transforms must be square in the original component coordinates"))
      brainInverse <- inverseAdmitted(brain, policy.maximumCondition, "brain supplied transform")
      targetInverse <- inverseAdmitted(target, policy.maximumCondition, "target supplied transform")
      out <- build(prediction, PatternRotationMethod.Supplied, brain, target, brainInverse, targetInverse,
        scoreCovariance, RotationEvidence.Supplied("converged-SVD"), RotationEvidence.Supplied("converged-SVD"), policy)
    yield out
  def varimax[N, Q, R](
      prediction: PatternPrediction[N, Q, R],
      scoreCovariance: ScoreCovarianceBasis,
      policy: PatternCoordinateRotationPolicy = PatternCoordinateRotationPolicy()
  ): Either[PatternRotationError, PatternCoordinateRotation[N, Q, R]] =
    for
      _ <- preflight(prediction, policy)
      brain <- FactorRotation.varimax(prediction.factors.neuralByComponent, policy.upstream).left.map(error => PatternRotationError.Upstream(error.message))
      target <- FactorRotation.varimax(prediction.factors.targetByComponent, policy.upstream).left.map(error => PatternRotationError.Upstream(error.message))
      _ <- if brain.receipt.converged && target.receipt.converged then Right(()) else Left(PatternRotationError.Upstream("varimax retained a non-converged upstream receipt"))
      out <- build(prediction, PatternRotationMethod.Varimax, brain.transform, target.transform, brain.inverseTransform, target.inverseTransform,
        scoreCovariance, RotationEvidence.Varimax(brain.receipt), RotationEvidence.Varimax(target.receipt), policy)
    yield out

  def promax[N, Q, R](
      prediction: PatternPrediction[N, Q, R],
      scoreCovariance: ScoreCovarianceBasis,
      policy: PatternCoordinateRotationPolicy = PatternCoordinateRotationPolicy()
  ): Either[PatternRotationError, PatternCoordinateRotation[N, Q, R]] =
    for
      _ <- preflight(prediction, policy)
      brain <- FactorRotation.promax(prediction.factors.neuralByComponent, policy.upstream).left.map(error => PatternRotationError.Upstream(error.message))
      target <- FactorRotation.promax(prediction.factors.targetByComponent, policy.upstream).left.map(error => PatternRotationError.Upstream(error.message))
      _ <- if brain.receipt.baseline.converged && target.receipt.baseline.converged then Right(()) else Left(PatternRotationError.Upstream("promax retained a non-converged varimax baseline"))
      out <- build(prediction, PatternRotationMethod.Promax, brain.transform, target.transform, brain.inverseTransform, target.inverseTransform,
        scoreCovariance, RotationEvidence.Promax(brain.receipt), RotationEvidence.Promax(target.receipt), policy)
    yield out

  private def build[N, Q, R](prediction: PatternPrediction[N, Q, R], method: PatternRotationMethod,
      brain: DMat, target: DMat, brainInverse: DMat, targetInverse: DMat, covarianceBasis: ScoreCovarianceBasis,
      brainReceipt: RotationEvidence, targetReceipt: RotationEvidence, policy: PatternCoordinateRotationPolicy
  ): Either[PatternRotationError, PatternCoordinateRotation[N, Q, R]] =
    for
      condition <- conditionOf(brain).flatMap(left => conditionOf(target).map(right => Math.max(left, right)))
      _ <- if condition <= policy.maximumCondition then Right(()) else Left(PatternRotationError.Condition(condition, policy.maximumCondition))
      originalCovariance <- covariance(covarianceBasis, brain.rows)
      rotatedCovariance = brainInverse * originalCovariance * brainInverse.t
      correlation <- correlationOf(rotatedCovariance)
      middle = brainInverse * targetInverse.t
      a = prediction.factors.neuralByComponent * brain
      c = prediction.factors.targetByComponent * target
      raw = prediction.rawFilters.neuralByComponent * brain
      _ <- if Vector(brainInverse, targetInverse, middle, a, c, raw).forall(finite) then Right(()) else Left(PatternRotationError.NonFinite("display factors, transforms or raw filters"))
      brainAxis <- brainDisplayAxis(prediction.factors.componentAxis, brain)
      targetAxis <- targetDisplayAxis(prediction.factors.componentAxis, target)
    yield new PatternCoordinateRotation(prediction.artifact, prediction,
      brainAxis, targetAxis, method, brain, target, brainInverse, targetInverse, middle, a, c,
      originalCovariance, rotatedCovariance, correlation, brainReceipt, targetReceipt, condition, raw)

  private def preflight[N, Q, R](prediction: PatternPrediction[N, Q, R], policy: PatternCoordinateRotationPolicy): Either[PatternRotationError, Unit] =
    val p = BigInt(prediction.factors.neuralAxis.size)
    val q = BigInt(prediction.factors.targetAxis.size)
    val r = BigInt(prediction.factors.componentAxis.size)
    val cells = 6 * p * r + 6 * q * r + 16 * r * r
    if policy.maximumOwnedCells < 0L || !policy.maximumCondition.isFinite || policy.maximumCondition < 1.0 then Left(PatternRotationError.Shape("rotation policy"))
    else if cells > policy.maximumOwnedCells || p * r > Int.MaxValue || q * r > Int.MaxValue || r * r > Int.MaxValue then Left(PatternRotationError.Budget(cells, policy.maximumOwnedCells))
    else Right(())

  private def covariance(value: ScoreCovarianceBasis, size: Int): Either[PatternRotationError, DMat] = value match
    case ScoreCovarianceBasis.ExplicitlyWhitenedFactorIdentity => Right(DMat.eye(size))
    case ScoreCovarianceBasis.ActualFactorCoordinates(matrix) =>
      if matrix.rows != size || matrix.cols != size then Left(PatternRotationError.ScoreCovariance("original factor-coordinate covariance shape"))
      else if !finite(matrix) then Left(PatternRotationError.ScoreCovariance("original factor-coordinate covariance is nonfinite"))
      else if (0 until size).exists(row => (0 until row).exists(col => math.abs(matrix(row, col) - matrix(col, row)) > 1e-12)) then Left(PatternRotationError.ScoreCovariance("original factor-coordinate covariance is not symmetric"))
      else matrix.cholesky.left.map(error => PatternRotationError.ScoreCovariance(s"original factor-coordinate covariance is not SPD: $error")).map(_ => matrix)

  private def conditionOf(matrix: DMat): Either[PatternRotationError, Double] =
    matrix.svd.flatMap(_.requireConverged).left.map(error => PatternRotationError.Upstream(error.toString)).flatMap: svd =>
      val values = svd.singularValues
      if values.length == 0 || values(values.length - 1) <= 0.0 then Left(PatternRotationError.Condition(Double.PositiveInfinity, Double.PositiveInfinity))
      else Right(values(0) / values(values.length - 1))

  private def correlationOf(covariance: DMat): Either[PatternRotationError, DMat] =
    if !finite(covariance) then Left(PatternRotationError.ScoreCovariance("rotated score covariance is nonfinite"))
    else
      val scale = Array.ofDim[Double](covariance.rows)
      var index = 0
      while index < scale.length do
        if covariance(index, index) <= 0.0 then return Left(PatternRotationError.ScoreCovariance("rotated score covariance has a nonpositive diagonal"))
        scale(index) = math.sqrt(covariance(index, index))
        index += 1
      val result = DMat.tabulate(covariance.rows, covariance.cols)((row, col) => covariance(row, col) / scale(row) / scale(col))
      if !finite(result) || (0 until result.rows).exists(row => (0 until result.cols).exists(col => math.abs(result(row, col)) > 1.0 + 1e-12)) then
        Left(PatternRotationError.ScoreCovariance("rotated score correlation is nonfinite or outside [-1, 1]"))
      else Right(result)

  private def inverseAdmitted(matrix: DMat, limit: Double, role: String): Either[PatternRotationError, DMat] =
    if matrix.rows != matrix.cols || !finite(matrix) then Left(PatternRotationError.Shape(s"$role must be finite and square"))
    else
      matrix.svd.flatMap(_.requireConverged).left.map(error => PatternRotationError.Upstream(error.toString)).flatMap: svd =>
        val values = svd.singularValues
        if values.length != matrix.rows || values(values.length - 1) <= 0.0 then Left(PatternRotationError.Condition(Double.PositiveInfinity, limit))
        else
          val condition = values(0) / values(values.length - 1)
          if !condition.isFinite || condition > limit then Left(PatternRotationError.Condition(condition, limit))
          else
            val diagonal = DMat.tabulate(values.length, values.length)((row, col) => if row == col then 1.0 / values(row) else 0.0)
            Right(svd.vt.t * diagonal * svd.u.t)

  private def displayAxis[R](source: AxisRef[R], transform: DMat, role: String): Either[PatternRotationError, AxisRef[String]] =
    val digest = AxisDigest.sha256Hex: writer =>
      writer.string(role); writer.string(source.descriptor.coordinateSignature.value)
      writer.intLE(transform.rows); writer.intLE(transform.cols)
      for row <- 0 until transform.rows; col <- 0 until transform.cols do writer.string(java.lang.Double.toHexString(transform(row, col)))
    AxisRef.fromStableKeys(s"$role-$digest", SpaceRole.Latent, Vector.tabulate(source.size)(index => s"$role-$digest-$index"),
      "rotated-display", "dimensionless", "component", Vector(source.descriptor.stableKey, digest))
      .left.map(error => PatternRotationError.Upstream(error.message))

  private def brainDisplayAxis[R](source: AxisRef[R], transform: DMat): Either[PatternRotationError, BrainDisplayAxes] =
    displayAxis(source, transform, "brain-display").map(axis => new BrainDisplayAxes(axis, source.descriptor.stableKey, axis.descriptor.coordinateSignature.value))

  private def targetDisplayAxis[R](source: AxisRef[R], transform: DMat): Either[PatternRotationError, TargetDisplayAxes] =
    displayAxis(source, transform, "target-display").map(axis => new TargetDisplayAxes(axis, source.descriptor.stableKey, axis.descriptor.coordinateSignature.value))

  private def finite(matrix: DMat): Boolean =
    var row = 0
    var okay = true
    while row < matrix.rows && okay do
      var col = 0
      while col < matrix.cols && okay do
        if !matrix(row, col).isFinite then okay = false
        col += 1
      row += 1
    okay
