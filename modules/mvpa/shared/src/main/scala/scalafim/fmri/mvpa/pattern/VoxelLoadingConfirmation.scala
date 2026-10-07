package scalafim.fmri.mvpa.pattern

import gale.backend.Backend.given
import gale.linalg.{DMat, QROptions, QRPivoting}
import multivar.core.SemanticSpace
import scalafim.fmri.mvpa.{AxisDescriptor, EvidenceIdentity, MultiResponse, Observations}

/** Outcomes are conditional on a fixed realized discovery and the admitted
  * Gaussian row-covariance shape, with a separately estimated voxel scale.
  * They assert task-linked forward association, never causal necessity. */
enum LoadingConfirmationError:
  case AxisMismatch(field: String)
  case Invalid(field: String)
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case NonEstimable(detail: String)
  case Numerical(detail: String)
  case Evidence(detail: String)
  case DegenerateResidual(voxel: Int)

final case class LoadingConfirmationBudget(batchVoxels: Int = 128, maximumOwnedCells: Long = 10000000L):
  require(batchVoxels > 0 && maximumOwnedCells >= 0L)

enum LoadingCalibrationStatus:
  case PendingFrozenProtocol

/** Arithmetic intervals at an explicitly supplied critical multiplier.
  * A coverage/probability claim needs a separately qualified reference. */
final case class LoadingIntervals(lower: DMat, upper: DMat, criticalMultiplier: Double)

/** The normalized component covariance is the inverse target Gram after
  * nuisance adjustment and known row-shape whitening. Multiplying it by a
  * voxel's estimated residual variance gives its component covariance.
  * Cross-voxel covariance requires a separate identified residual-feature
  * covariance; marginal errors never imply independence. */
final class VoxelLoadingResult private[pattern] (
    val neuralAxis: AxisDescriptor, val componentAxis: AxisDescriptor,
    val confirmationRows: AxisDescriptor, val independentUnits: AxisDescriptor, val rowUnitOrdinals: Vector[Int],
    val estimates: DMat, val standardErrors: DMat, val tStatistics: DMat,
    val normalizedComponentCovariance: DMat,
    val omnibusF: Vector[Double], val residualScales: Vector[Double],
    val residualDegreesOfFreedom: Int, val componentDegreesOfFreedom: Int,
    val designIdentity: String, val brainEvidenceIdentity: EvidenceIdentity, val targetEvidenceIdentity: EvidenceIdentity,
    val batchReads: Int, val plannedOwnedCells: Long, val errorLaw: ConfirmationErrorLaw,
    val interceptAdded: Boolean, val nuisanceColumns: Int, val designRankTolerance: Double
):
  val calibrationStatus: LoadingCalibrationStatus = LoadingCalibrationStatus.PendingFrozenProtocol

  def intervalsAt(criticalMultiplier: Double, maximumCells: Long): Either[LoadingConfirmationError, LoadingIntervals] =
    val required = BigInt(2) * estimates.rows * estimates.cols
    if !criticalMultiplier.isFinite || criticalMultiplier <= 0.0 then Left(LoadingConfirmationError.Invalid("positive finite critical multiplier"))
    else if required > maximumCells || BigInt(estimates.rows) * estimates.cols > Int.MaxValue then Left(LoadingConfirmationError.Budget(required, maximumCells))
    else
      val lower = DMat.tabulate(estimates.rows, estimates.cols)((i, j) => estimates(i, j) - criticalMultiplier * standardErrors(i, j))
      val upper = DMat.tabulate(estimates.rows, estimates.cols)((i, j) => estimates(i, j) + criticalMultiplier * standardErrors(i, j))
      if !ResidualCovariance.finite(lower) || !ResidualCovariance.finite(upper) then Left(LoadingConfirmationError.Numerical("nonfinite interval"))
      else Right(LoadingIntervals(lower, upper, criticalMultiplier))

object VoxelLoadingConfirmation:
  /** Caller admits repeated application of the source explicitly. A single
    * full-width batch needs only one source application. An intercept is added
    * when its direction is outside the supplied nuisance span. Backend/provider
    * scratch and borrowed evidence are outside the owned-cell bound. */
  def fit[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace, U](
      design: ConfirmationDesign[?, U], observations: Observations[S, N], targets: MultiResponse[S, Q],
      replay: PatternReplay, budget: LoadingConfirmationBudget = LoadingConfirmationBudget()
  ): Either[LoadingConfirmationError, VoxelLoadingResult] =
    val frozen = design.discovery.targetProjection
    val factors = design.discovery.artifact.factors
    val rows = design.confirmation.samples.rows.descriptor
    val n = observations.rows; val p = observations.columns
    val r = frozen.matrix.cols; val nuisance = design.nuisance.matrix
    val zWide = nuisance.cols.toLong + 1L; val mWide = zWide + r
    val m = math.min(mWide, Int.MaxValue.toLong).toInt
    val tolerance = 1e-12
    val b = math.min(p, budget.batchVoxels)
    val covariance = design.errorLaw match
      case ConfirmationErrorLaw.IndependentGaussian => None
      case ConfirmationErrorLaw.DependentTime(bound) => Some(bound.covariance)
      case ConfirmationErrorLaw.RepeatedSubjects(bound) => Some(bound.covariance.covariance)
    val covarianceWork = covariance.map(c => BigInt(c.storedCells) + BigInt(2) * (BigInt(n) + c.rank) * math.max(m, b) + BigInt(n) * math.max(m, b)).getOrElse(BigInt(0))
    // Basis/read/output/QR/Gram and simultaneous block temporaries. This is
    // conservative owned numeric storage, not a whole-process certificate.
    val cells = BigInt(10) * n * m + BigInt(20) * m * m + BigInt(10) * n * b +
      BigInt(p) * b + BigInt(6) * p * r + BigInt(4) * p + covarianceWork
    if observations.sampleAxis != rows || targets.sampleAxis != rows then Left(LoadingConfirmationError.AxisMismatch("actual confirmation rows"))
    else if observations.neuralAxis != factors.neuralAxis.descriptor || targets.featureAxis != frozen.input.descriptor then Left(LoadingConfirmationError.AxisMismatch("frozen neural or target endpoint"))
    else if mWide > Int.MaxValue then Left(LoadingConfirmationError.Budget(BigInt(mWide), Int.MaxValue.toLong))
    else if n <= r || r <= 0 then Left(LoadingConfirmationError.NonEstimable("positive residual degrees of freedom and target dimensions required"))
    else if cells > budget.maximumOwnedCells || Vector(BigInt(n) * m, BigInt(n) * b, BigInt(p) * b, BigInt(p) * r, BigInt(m) * m).exists(_ > Int.MaxValue) then Left(LoadingConfirmationError.Budget(cells, budget.maximumOwnedCells))
    else if b < p && replay == PatternReplay.SinglePass then Left(LoadingConfirmationError.Invalid("multiple brain batches require replay admission"))
    else if (replay match
      case PatternReplay.Repeatable(receipt) => receipt.trim.isEmpty
      case PatternReplay.SinglePass => false) then Left(LoadingConfirmationError.Invalid("nonempty replay receipt"))
    else
      def whiten(matrix: DMat): Either[LoadingConfirmationError, DMat] = covariance match
        case None => Right(matrix)
        case Some(value) => value.whiten(matrix).left.map(error => LoadingConfirmationError.Numerical(error.toString))
      for
        workingNuisance <-
          val augmented = DMat.tabulate(n, zWide.toInt)((i, j) => if j == 0 then 1.0 else nuisance(i, j - 1))
          val rank = augmented.qr(QROptions(QRPivoting.Column, Some(tolerance))).diagnostics.rank
          if rank.contains(nuisance.cols) then Right(nuisance)
          else if rank.contains(zWide.toInt) then Right(augmented)
          else Left(LoadingConfirmationError.NonEstimable("nuisance/intercept rank unavailable at tolerance 1e-12"))
        z = workingNuisance.cols
        m = z + r
        _ <- if n > m then Right(()) else Left(LoadingConfirmationError.NonEstimable("positive residual degrees of freedom required after intercept handling"))
        t <- targets.targets(frozen.matrix).left.map(error => LoadingConfirmationError.Evidence(error.toString))
        _ <- if ResidualCovariance.finite(t) then Right(()) else Left(LoadingConfirmationError.NonEstimable("nonfinite or missing target-derived scores"))
        joint = DMat.tabulate(n, m)((i, j) => if j < z then workingNuisance(i, j) else t(i, j - z))
        weighted <- whiten(joint)
        qr = weighted.qr(QROptions(QRPivoting.Column, Some(tolerance)))
        _ <- if qr.diagnostics.rank.contains(m) then Right(()) else Left(LoadingConfirmationError.NonEstimable("nuisance/target alias or rank-deficient target dimensions at tolerance 1e-12"))
        triangular = DMat.tabulate(m, m)((i, j) => if i <= j then qr.r(i, j) else 0.0)
        triangularInverse <- triangular.solve(DMat.eye(m)).left.map(error => LoadingConfirmationError.Numerical(s"design triangular covariance: $error"))
        pivotedCovariance = triangularInverse * triangularInverse.t
        inverseOrder =
          val order = new Array[Int](m)
          var i = 0
          while i < m do
            order(qr.columnPermutation(i)) = i
            i += 1
          order
        inverse = DMat.tabulate(m, m)((i, j) => pivotedCovariance(inverseOrder(i), inverseOrder(j)))
        _ <- if ResidualCovariance.finite(inverse) then Right(()) else Left(LoadingConfirmationError.Numerical("nonfinite coefficient covariance"))
        targetCovariance = DMat.tabulate(r, r)((i, j) => inverse(z + i, z + j))
        targetFactor <- targetCovariance.cholesky.left.map(error => LoadingConfirmationError.Numerical(s"conditional component covariance: $error"))
        result <-
          val estimates = DMat.newBuilder(p, r); val errors = DMat.newBuilder(p, r); val statistics = DMat.newBuilder(p, r)
          val fValues = Vector.newBuilder[Double]; val scales = Vector.newBuilder[Double]
          var first = 0; var reads = 0
          var failure: Option[LoadingConfirmationError] = None
          while first < p && failure.isEmpty do
            val count = math.min(b, p - first)
            val basis = DMat.tabulate(p, count)((i, j) => if i == first + j then 1.0 else 0.0)
            val batch = for
              brain <- observations.patterns(basis).left.map(error => LoadingConfirmationError.Evidence(error.toString))
              _ <- if ResidualCovariance.finite(brain) then Right(()) else Left(LoadingConfirmationError.Evidence("nonfinite brain batch"))
              x <- whiten(brain)
              beta <- qr.solveLeastSquares(x).left.map(error => LoadingConfirmationError.Numerical(error.toString))
              residual = x - weighted * beta
              effect = DMat.tabulate(r, count)((i, j) => beta(z + i, j))
              tested <- targetFactor.solve(effect).left.map(error => LoadingConfirmationError.Numerical(error.toString))
            yield (beta, residual, effect, tested)
            reads += 1
            batch match
              case Left(error) => failure = Some(error)
              case Right((beta, residual, effect, tested)) =>
                var column = 0
                while column < count && failure.isEmpty do
                  var sse = 0.0; var i = 0
                  while i < n do
                    sse += residual(i, column) * residual(i, column)
                    i += 1
                  val variance = sse / (n - m)
                  var quadratic = 0.0; i = 0
                  while i < r do
                    quadratic += effect(i, column) * tested(i, column)
                    i += 1
                  val omnibus = quadratic / (r * variance)
                  if !variance.isFinite || variance <= 0.0 then failure = Some(LoadingConfirmationError.DegenerateResidual(first + column))
                  else if !omnibus.isFinite || omnibus < 0.0 then failure = Some(LoadingConfirmationError.Numerical("nonfinite or negative omnibus statistic"))
                  else
                    fValues += omnibus; scales += math.sqrt(variance); i = 0
                    while i < r && failure.isEmpty do
                      val estimate = beta(z + i, column)
                      val se = math.sqrt(variance * inverse(z + i, z + i))
                      val statistic = estimate / se
                      if !estimate.isFinite || !se.isFinite || se <= 0.0 || !statistic.isFinite then failure = Some(LoadingConfirmationError.Numerical("component estimate/standard error/statistic"))
                      else
                        estimates.update(first + column, i, estimate)
                        errors.update(first + column, i, se)
                        statistics.update(first + column, i, statistic)
                      i += 1
                  column += 1
            first += count
          failure match
            case Some(error) => Left(error)
            case None => Right(new VoxelLoadingResult(observations.neuralAxis, frozen.output.descriptor, rows, design.confirmation.samples.units.descriptor, design.confirmation.samples.rowUnitOrdinals,
              estimates.result(), errors.result(), statistics.result(), targetCovariance, fValues.result(), scales.result(), n - m, r,
              design.identity, observations.identity, targets.identity, reads, cells.toLong, design.errorLaw, z > nuisance.cols, z, tolerance))
      yield result
