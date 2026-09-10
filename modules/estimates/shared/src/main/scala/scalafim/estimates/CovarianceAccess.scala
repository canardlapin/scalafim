package scalafim.estimates

import gale.linalg.{DMat, Matrix}
import gale.spectral.{Eigen, EigenSelection, EigenVectors}

/** A consumer-selected numerical policy. Dense verification needs O(order^2)
  * working storage; it never expands the sample or observation axes.
  */
final case class CovariancePolicy(maximumOrder: Int, absoluteTolerance: Double, relativeTolerance: Double):
  require(maximumOrder > 0)
  require(absoluteTolerance.isFinite && absoluteTolerance >= 0)
  require(relativeTolerance.isFinite && relativeTolerance >= 0)

/** Absolute covariance in the caller's estimand order, with the applied scale
  * and PSD verification retained. This is not an estimability or inference claim.
  */
final class CheckedCovariance private[estimates] (
    val estimands: Vector[EstimandId],
    val values: DMat,
    val varianceScale: Double,
    val minimumEigenvalue: Double,
    val acceptedTolerance: Double
)

object CovarianceAccess:
  /** Read and validate one principal covariance matrix. The source may have a
    * one-cell block budget. Pair orientation follows the stored catalog order;
    * the returned symmetric matrix follows the caller's order.
    */
  def matrix(source: EstimateSource, product: ProductId, observation: ObservationId, sample: Int,
      estimands: Vector[EstimandId], policy: CovariancePolicy,
      cancelled: () => Boolean = () => false): Either[EstimateError, CheckedCovariance] =
    val unit = source.unit
    if estimands.isEmpty || estimands.distinct.size != estimands.size || estimands.size > policy.maximumOrder then
      Left(EstimateError.Invalid("covariance order must be nonempty, unique and within the consumer limit"))
    else unit.covariance.find(_.product == product).toRight(EstimateError.Invalid("unknown covariance descriptor")).flatMap: covariance =>
      val descriptor = unit.products.find(_.id == product).get
      val axis = descriptor.targets.estimands
      if !estimands.forall(axis.contains) then Left(EstimateError.Invalid("unknown covariance estimand"))
      else
        val n = estimands.size
        if n.toLong * n > Int.MaxValue then Left(EstimateError.Invalid("covariance matrix exceeds indexed storage capacity"))
        else
          val cells = new Array[Double](n * n)
          val value = new Array[Double](1)
          val validity = new Array[Byte](1)
          var failure: Option[EstimateError] = None
          var i = 0
          while i < n && failure.isEmpty do
            var j = i
            while j < n && failure.isEmpty do
              val a = estimands(i)
              val b = estimands(j)
              val pair = if axis.indexOf(a) <= axis.indexOf(b) then EstimandPair(a, b) else EstimandPair(b, a)
              source.readCovariance(product, CovarianceSelection(Vector(observation), Vector(pair), Vector(sample)), value, validity, cancelled) match
                case Left(error) => failure = Some(error)
                case Right(_) =>
                  if validity(0) != Validity.Valid.code || !value(0).isFinite then
                    failure = Some(EstimateError.Invalid("requested covariance contains unavailable or nonfinite entries"))
                  else
                    cells(i * n + j) = value(0)
                    cells(j * n + i) = value(0)
              j += 1
            i += 1
          failure.toLeft(()).flatMap: _ =>
            val scale = covariance.equation match
              case CovarianceEquation.Absolute => Right(1.0)
              case CovarianceEquation.Normalized(scaleProduct) =>
                // Current scalar scale products explicitly repeat their single
                // per-sample variance over estimands. Verify that assertion.
                var common: Option[Double] = None
                var error: Option[EstimateError] = None
                estimands.foreach: id =>
                  if error.isEmpty then
                    source.read(scaleProduct, EstimateSelection(Vector(observation), Vector(id), Vector(sample)), value, validity, cancelled) match
                      case Left(e) => error = Some(e)
                      case Right(_) =>
                        if validity(0) != Validity.Valid.code || !value(0).isFinite || value(0) < 0 || common.exists(_ != value(0)) then
                          error = Some(EstimateError.Invalid("variance scale is unavailable, negative or inconsistent across estimands"))
                        else common = Some(value(0))
                error.toLeft(common.getOrElse(0.0))
            scale.flatMap: multiplier =>
              // Validate U before multiplication: a zero scale cannot hide an
              // indefinite normalized matrix and imply spurious certainty.
              val stored = Matrix.tabulate(n, n)((row, col) => cells(row * n + col))
              validate(stored, policy, cancelled).flatMap: _ =>
                val absolute = Matrix.tabulate(n, n)((row, col) => cells(row * n + col) * multiplier)
                validate(absolute, policy, cancelled).map: (minimum, tolerance) =>
                  new CheckedCovariance(estimands, absolute, multiplier, minimum, tolerance)

  private def validate(matrix: DMat, policy: CovariancePolicy,
      cancelled: () => Boolean): Either[EstimateError, (Double, Double)] =
    if cancelled() then Left(EstimateError.Cancelled)
    else
      var maximum = 0.0
      var invalid = false
      var i = 0
      while i < matrix.rows do
        var j = 0
        while j < matrix.cols do
          val value = matrix(i, j)
          if !value.isFinite || (i == j && value < 0) then invalid = true
          maximum = math.max(maximum, math.abs(value))
          j += 1
        i += 1
      val tolerance = policy.absoluteTolerance + policy.relativeTolerance * maximum
      if invalid || !tolerance.isFinite then Left(EstimateError.Invalid("covariance has nonfinite values, negative diagonal or overflowing tolerance"))
      else
        // The generic spectral algorithm belongs to Gale. This one bounded
        // dense call is not interruptible; cancellation is checked on return.
        Eigen.eigSymmetric(matrix, EigenSelection.All, EigenVectors.ValuesOnly)
          .left.map(e => EstimateError.Invalid(e.toString)).flatMap: decomposition =>
            val minimum = decomposition.eigenvalues(0)
            if cancelled() then Left(EstimateError.Cancelled)
            else if !minimum.isFinite || minimum < -tolerance then Left(EstimateError.Invalid("covariance is not positive semidefinite under the consumer policy"))
            else Right(minimum -> tolerance)
