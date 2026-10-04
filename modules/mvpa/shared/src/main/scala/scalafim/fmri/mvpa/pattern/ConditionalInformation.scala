package scalafim.fmri.mvpa.pattern

import gale.backend.Backend.given
import gale.linalg.{CholeskyOptions, DMat}
import scalafim.fmri.mvpa.{AxisDigest, AxisRef, EvidenceIdentity}

/** Gaussian conditional-information is defined only for an explicitly
  * supported continuous target subspace.  It is an information map in nats,
  * conditional on all neural coordinates except the reported coordinate.
  */
enum ConditionalInformationError:
  case UnsupportedTarget(detail: String)
  case AxisMismatch(field: String)
  case Invalid(detail: String)
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case Covariance(error: ResidualCovarianceError)
  case Factorization(stage: String, detail: String)
  case Cancelled(processed: Int, total: Int)
  case Numerical(feature: Int, t: Double, detail: String)

final case class ConditionalInformationPolicy(
    relativePivotTolerance: Double = 1e-12,
    maximumWorkspaceCells: Long = 10000000L,
    roundingTolerance: Double = 1e-12,
    nearOneTolerance: Double = 1e-10
):
  require(relativePivotTolerance.isFinite && relativePivotTolerance >= 0.0 && relativePivotTolerance < 1.0)
  require(maximumWorkspaceCells >= 0L)
  require(roundingTolerance.isFinite && roundingTolerance >= 0.0)
  require(nearOneTolerance.isFinite && nearOneTolerance > 0.0 && nearOneTolerance < 1.0)

/** A declared Gaussian target subspace. `basis` maps supported target
  * coordinates into the artifact's full target coordinates. It is admitted by
  * a Gale Gram Cholesky at the recorded tolerance; no task-rank inference or
  * pseudo-inverse is used.
  */
final class SupportedGaussianTarget[Q, T] private (
    val targetAxis: AxisRef[Q], val supportedAxis: AxisRef[T], val basis: DMat,
    val prior: TargetPriorCovariance[T], val relativePivotTolerance: Double,
    val sourceIdentity: String
)
object SupportedGaussianTarget:
  // Incremental repository-owned constructor workspace; caller-owned basis
  // and prior, and Gale-private factorization scratch, are excluded.
  private def rankAdmissionCells(q: Int, s: Int): BigInt =
    BigInt(q) * s + BigInt(s) * s + BigInt(2) * s

  def apply[Q, T](targetAxis: AxisRef[Q], supportedAxis: AxisRef[T], basis: DMat,
      prior: TargetPriorCovariance[T], sourceIdentity: String,
      relativePivotTolerance: Double = 1e-12,
      policy: ConditionalInformationPolicy = ConditionalInformationPolicy()): Either[ConditionalInformationError, SupportedGaussianTarget[Q, T]] =
    if targetAxis.size <= 0 || supportedAxis.size <= 0 then Left(ConditionalInformationError.Invalid("target axes must be nonempty"))
    else if supportedAxis.size > targetAxis.size then Left(ConditionalInformationError.Invalid("supported dimension cannot exceed full target dimension"))
    else if prior.axis.descriptor != supportedAxis.descriptor then Left(ConditionalInformationError.AxisMismatch("supported prior axis"))
    else if basis.rows != targetAxis.size || basis.cols != supportedAxis.size then Left(ConditionalInformationError.AxisMismatch("support basis shape"))
    else if rankAdmissionCells(targetAxis.size, supportedAxis.size) > policy.maximumWorkspaceCells ||
        BigInt(targetAxis.size) * supportedAxis.size > Int.MaxValue || BigInt(supportedAxis.size) * supportedAxis.size > Int.MaxValue then
      Left(ConditionalInformationError.Budget(rankAdmissionCells(targetAxis.size, supportedAxis.size), policy.maximumWorkspaceCells))
    else if !ResidualCovariance.finite(basis) || sourceIdentity.trim.isEmpty then Left(ConditionalInformationError.Invalid("support basis must be finite and source identity named"))
    else if !relativePivotTolerance.isFinite || relativePivotTolerance < 0.0 || relativePivotTolerance >= 1.0 then Left(ConditionalInformationError.Invalid("support relative pivot tolerance"))
    else if !TargetGeometry.fullColumnRank(basis, relativePivotTolerance) then Left(ConditionalInformationError.Invalid("support basis must be full column rank at the declared tolerance"))
    else Right(new SupportedGaussianTarget(targetAxis, supportedAxis, basis, prior, relativePivotTolerance, sourceIdentity))

  def fullSupport[Q](targetAxis: AxisRef[Q], prior: TargetPriorCovariance[Q], sourceIdentity: String,
      relativePivotTolerance: Double = 1e-12,
      policy: ConditionalInformationPolicy = ConditionalInformationPolicy()): Either[ConditionalInformationError, SupportedGaussianTarget[Q, Q]] =
    // `eye` is retained as the basis. Rank admission also owns a normalized
    // q-by-q basis, a Gram, and two q-vectors, all before Gale factorization.
    val cells = BigInt(targetAxis.size) * targetAxis.size + rankAdmissionCells(targetAxis.size, targetAxis.size)
    if cells > policy.maximumWorkspaceCells || BigInt(targetAxis.size) * targetAxis.size > Int.MaxValue then Left(ConditionalInformationError.Budget(cells, policy.maximumWorkspaceCells))
    else apply(targetAxis, targetAxis, DMat.eye(targetAxis.size), prior, sourceIdentity, relativePivotTolerance, policy)

final case class ConditionalInformationWork(
    plannedMaximumCells: Long,
    precisionApplications: Int,
    precisionDiagonalCalls: Int,
    posteriorSolves: Int,
    supportDimension: Int,
    componentDimension: Int,
    excludesKernelPrivateScratch: Boolean,
    excludesRss: Boolean
)

final case class ConditionalInformationReceipt(
    sourceIdentity: String,
    artifactIdentity: String,
    neuralAxis: String,
    targetAxis: String,
    supportedAxis: String,
    componentAxis: String,
    supportRankTolerance: Double,
    posteriorPivotTolerance: Double,
    conditioning: String
)

final case class ConditionalInformationDiagnostics(
    roundedNegativeFeatures: Vector[Int],
    mostNegativeRoundedCoefficient: Double
)

final class ConditionalInformationMap[N] private[pattern] (
    val values: AxisValues[N], val numericalIdentity: String,
    val work: ConditionalInformationWork, val receipt: ConditionalInformationReceipt,
    val diagnostics: ConditionalInformationDiagnostics
)

object ConditionalInformation:
  private def factor(matrix: DMat, stage: String, tolerance: Double): Either[ConditionalInformationError, gale.linalg.Cholesky] =
    val scale = (0 until matrix.rows).foldLeft(0.0)((maximum, row) => math.max(maximum, math.abs(matrix(row, row))))
    val pivot = tolerance * scale
    if !ResidualCovariance.finite(matrix) || !pivot.isFinite then Left(ConditionalInformationError.Factorization(stage, "nonfinite matrix or pivot tolerance"))
    else matrix.cholesky(CholeskyOptions(pivot)).left.map(error => ConditionalInformationError.Factorization(stage, error.toString))

  private def planned(p: Int, q: Int, r: Int, s: Int, covariance: ResidualCovariance[?], policy: ConditionalInformationPolicy): Either[ConditionalInformationError, Long] =
    val precision = covariance.precisionWork(r).left.map(ConditionalInformationError.Covariance.apply)
    precision.flatMap: precisionWork =>
      val diagonalWork = covariance.precisionDiagonalWork
      // This includes the covariance module's known resident and planned work
      // plus this kernel's local intermediates. Only opaque provider/Gale
      // scratch and process RSS are explicit exclusions in the receipt.
      val cells = BigInt(covariance.storedCells) + precisionWork.peakCellsUpperBound + diagonalWork.peakCellsUpperBound +
        BigInt(q) * s + BigInt(p) * r + p + BigInt(s) * r * 3 + BigInt(r) * r * 3 + BigInt(s) * s * 3
      val exceedsIntCapacity = Vector(BigInt(q) * s, BigInt(p) * r, BigInt(p), BigInt(s) * r, BigInt(r) * r, BigInt(s) * s).exists(_ > Int.MaxValue)
      if cells > policy.maximumWorkspaceCells || exceedsIntCapacity then Left(ConditionalInformationError.Budget(cells, policy.maximumWorkspaceCells))
      else Right(cells.toLong)

  def fromArtifact[N, Q, R, T](neural: AxisRef[N], target: AxisRef[Q], components: AxisRef[R], artifact: PatternArtifact,
      covariance: ResidualCovariance[N], supported: SupportedGaussianTarget[Q, T], policy: ConditionalInformationPolicy = ConditionalInformationPolicy(),
      cancelled: () => Boolean = () => false): Either[ConditionalInformationError, ConditionalInformationMap[N]] =
    val factors = artifact.factors
    val covarianceAdmitted = artifact.residualCovariance match
      case ResidualCovarianceCapability.DiagonalPlusLowRank(axis, rank) => axis == neural.descriptor && rank == covariance.rank
      case _ => false
    artifact.target match
      case TargetGeometry.Categorical(_) => Left(ConditionalInformationError.UnsupportedTarget("Gaussian conditional information requires a continuous target; categorical targets are refused"))
      case TargetGeometry.Continuous(_) if factors.neuralAxis.descriptor != neural.descriptor || covariance.neuralAxis.descriptor != neural.descriptor => Left(ConditionalInformationError.AxisMismatch("neural artifact coordinates"))
      case TargetGeometry.Continuous(_) if factors.targetAxis.descriptor != target.descriptor || factors.componentAxis.descriptor != components.descriptor => Left(ConditionalInformationError.AxisMismatch("target/component artifact coordinates"))
      case TargetGeometry.Continuous(_) if supported.targetAxis.descriptor != target.descriptor => Left(ConditionalInformationError.AxisMismatch("supported full target axis"))
      case TargetGeometry.Continuous(_) if !covarianceAdmitted => Left(ConditionalInformationError.Invalid("artifact must declare the supplied diagonal-plus-low-rank covariance capability"))
      case TargetGeometry.Continuous(_) =>
        val p = neural.size
        val q = target.size
        val r = components.size
        val s = supported.supportedAxis.size
        if cancelled() then Left(ConditionalInformationError.Cancelled(0, p))
        else planned(p, q, r, s, covariance, policy).flatMap: cells =>
          for
            raw <- covariance.applyPrecision(factors.neuralByComponent).left.map(ConditionalInformationError.Covariance.apply)
            _ <- if cancelled() then Left(ConditionalInformationError.Cancelled(0, p)) else Right(())
            diagonal <- covariance.precisionDiagonal.left.map(ConditionalInformationError.Covariance.apply)
            _ <- if cancelled() then Left(ConditionalInformationError.Cancelled(0, p)) else Right(())
            _ <- if diagonal.forall(value => value > 0.0 && value.isFinite) then Right(()) else Left(ConditionalInformationError.Invalid("precision diagonal must be positive and finite"))
            unsymmetric = factors.neuralByComponent.t * raw
            gram = DMat.tabulate(r, r)((i, j) => 0.5 * (unsymmetric(i, j) + unsymmetric(j, i)))
            _ <- if ResidualCovariance.finite(gram) then Right(()) else Left(ConditionalInformationError.Invalid("nonfinite precision Gram"))
            priorFactor <- factor(supported.prior.matrix, "supported target prior", policy.relativePivotTolerance)
            supportByComponent = supported.basis.t * factors.targetByComponent
            transformed = priorFactor.lower.t * supportByComponent
            posterior = DMat.eye(s) + transformed * gram * transformed.t
            posteriorFactor <- factor(posterior, "supported posterior precision", policy.relativePivotTolerance)
            solved <- posteriorFactor.solve(transformed).left.map(error => ConditionalInformationError.Factorization("supported posterior solve", error.toString))
            kernel = transformed.t * solved
            result <- values(neural, raw, diagonal, kernel, policy, cancelled)
          yield
            val receipt = ConditionalInformationReceipt(supported.sourceIdentity, artifact.trainingBinding.fingerprintDigest,
              neural.descriptor.stableKey, target.descriptor.stableKey, supported.supportedAxis.descriptor.stableKey,
              components.descriptor.stableKey, supported.relativePivotTolerance, policy.relativePivotTolerance,
              "no jitter, pseudo-inverse, or inferred target subspace")
            val identity = AxisDigest.sha256Hex: writer =>
              writer.string("scalafim.conditional-information.v1")
              writer.string(receipt.sourceIdentity); writer.string(receipt.artifactIdentity)
              Vector(neural, target, supported.supportedAxis, components).foreach(axis => writer.string(axis.descriptor.coordinateSignature.value))
              def matrix(value: DMat): Unit =
                writer.intLE(value.rows); writer.intLE(value.cols)
                var row = 0
                while row < value.rows do
                  var column = 0
                  while column < value.cols do
                    writer.string(java.lang.Double.toHexString(value(row, column)))
                    column += 1
                  row += 1
              def vector(value: Vector[Double]): Unit =
                writer.intLE(value.length)
                value.foreach(number => writer.string(java.lang.Double.toHexString(number)))
              matrix(factors.neuralByComponent); matrix(factors.targetByComponent); matrix(covariance.loadingsMatrix); vector(covariance.diagonalValues)
              matrix(supported.basis); matrix(supported.prior.matrix); EvidenceIdentity.writeValues(writer, supported.prior.valueIdentity)
              writer.string(supported.prior.coordinateReceipt)
              writer.string(java.lang.Double.toHexString(receipt.supportRankTolerance)); writer.string(java.lang.Double.toHexString(receipt.posteriorPivotTolerance))
              writer.string(java.lang.Double.toHexString(policy.roundingTolerance)); writer.string(java.lang.Double.toHexString(policy.nearOneTolerance)); writer.string(policy.maximumWorkspaceCells.toString)
            new ConditionalInformationMap(result._1, identity, ConditionalInformationWork(cells, 1, 1, 1, s, r, true, true), receipt, result._2)

  private def values[N](neural: AxisRef[N], raw: DMat, diagonal: Vector[Double], kernel: DMat,
      policy: ConditionalInformationPolicy, cancelled: () => Boolean): Either[ConditionalInformationError, (AxisValues[N], ConditionalInformationDiagnostics)] =
    val output = Vector.newBuilder[Double]
    val rounded = Vector.newBuilder[Int]
    var mostNegative = 0.0
    var feature = 0
    while feature < neural.size do
      if cancelled() then return Left(ConditionalInformationError.Cancelled(feature, neural.size))
      var t = 0.0
      var left = 0
      while left < kernel.rows do
        val hLeft = raw(feature, left) / math.sqrt(diagonal(feature))
        var right = 0
        while right < kernel.cols do
          val hRight = raw(feature, right) / math.sqrt(diagonal(feature))
          t += hLeft * kernel(left, right) * hRight
          right += 1
        left += 1
      if !t.isFinite then return Left(ConditionalInformationError.Numerical(feature, t, "conditional coefficient is nonfinite"))
      if t < -policy.roundingTolerance then return Left(ConditionalInformationError.Numerical(feature, t, "conditional coefficient is negative beyond rounding tolerance"))
      if t >= 1.0 then return Left(ConditionalInformationError.Numerical(feature, t, "conditional coefficient is at least one"))
      if t >= 1.0 - policy.nearOneTolerance then return Left(ConditionalInformationError.Numerical(feature, t, "conditional coefficient is too near one for the declared policy"))
      val admitted = if t < 0.0 then
        rounded += feature
        mostNegative = math.min(mostNegative, t)
        0.0
      else t
      val information = -0.5 * math.log1p(-admitted)
      if !information.isFinite then return Left(ConditionalInformationError.Numerical(feature, t, "conditional information is nonfinite"))
      output += information
      feature += 1
    AxisValues(neural, output.result()).left.map(error => ConditionalInformationError.Invalid(error.toString))
      .map(value => (value, ConditionalInformationDiagnostics(rounded.result(), mostNegative)))
