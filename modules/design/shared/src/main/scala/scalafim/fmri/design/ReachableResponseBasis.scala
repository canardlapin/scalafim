package scalafim.fmri.design

import gale.linalg.{DMat, DVec}
import gale.spectral.{Eigen, EigenSelection}
import scalafim.fmri.hrf.{Hrf, Seconds}
import scala.util.control.NonFatal

/** Why a reachable-basis construction can fail, or why a curve cannot be
  * admitted into it.
  */
enum ReachableBasisError:
  case EmptyFamily
  case InvalidSampling(reason: String)
  case MultiBasisMember(name: String, nbasis: Int)
  case DegenerateMember(name: String)
  case CurveLengthMismatch(expected: Int, actual: Int)
  case DegenerateCurve
  case KernelEvaluation(reason: String)
  case Numeric(detail: String)

  def message: String = this match
    case EmptyFamily =>
      "A reachable basis requires at least one family member."
    case InvalidSampling(reason) => reason
    case MultiBasisMember(name, nbasis) =>
      s"Family member '$name' has $nbasis basis elements; the reachable library is built from single-basis kernels."
    case DegenerateMember(name) =>
      s"Family member '$name' is numerically zero or non-finite on the sampling grid."
    case CurveLengthMismatch(expected, actual) =>
      s"Expected a curve with $expected samples, got $actual."
    case DegenerateCurve =>
      "The projected curve is numerically zero or non-finite on the sampling grid."
    case KernelEvaluation(reason) =>
      s"The response family could not be evaluated: $reason"
    case Numeric(detail) =>
      s"The family eigendecomposition failed: $detail"

/** The coordinates of one response curve in a [[ReachableResponseBasis]],
  * together with the span-residual receipt that justifies (or refuses) a
  * truncation rank.
  *
  * `residualByRank(j - 1)` is the relative l2 residual of the curve after
  * keeping the first `j` basis directions; `residual` is the value at the full
  * retained rank. Residuals are relative to the curve's own norm, so they are
  * invariant to kernel amplitude conventions.
  */
final case class ReachableBasisProjection private[design] (
    coefficients: DVec,
    curveNorm: Double,
    residualByRank: Vector[Double]):
  def rank: Int = coefficients.length
  def residual: Double = residualByRank.last

/** An orthonormal response basis spanning (numerically) the curves reachable by
  * a parametric HRF family on a shared lag grid.
  *
  * `curves` holds the basis as columns (`sampleCount` rows, `rank` columns),
  * orthonormal in the ordinary l2 inner product on the grid and ordered by
  * descending singular value. `energyFractions(j - 1)` is the cumulative
  * fraction of the family's total sampled energy captured by the first `j`
  * directions.
  *
  * The intended use is the live response-model explorer contract: fit once
  * against the basis-expanded design, then re-express any in-family kernel as
  * `curve ~= curves * coefficients` via [[project]] and reproject fitted
  * products without touching response data. The projection receipt makes the
  * approximation error explicit at every rank instead of asserting a global
  * quality claim.
  */
final class ReachableResponseBasis private (
    val times: Vector[Seconds],
    val curves: DMat,
    val singularValues: DVec,
    val energyFractions: Vector[Double]):

  def sampleCount: Int = curves.rows
  def rank: Int = curves.cols

  /** The same grid and directions, keeping only the leading `keep` columns. */
  def truncate(keep: Int): Either[ReachableBasisError, ReachableResponseBasis] =
    if keep < 1 || keep > rank then
      Left(ReachableBasisError.InvalidSampling(s"Truncation rank $keep is outside 1..$rank."))
    else if keep == rank then Right(this)
    else
      Right(
        new ReachableResponseBasis(
          times = times,
          curves = DMat.tabulate(sampleCount, keep)((l, j) => curves(l, j)),
          singularValues = DVec.tabulate(keep)(singularValues(_)),
          energyFractions = energyFractions.take(keep)
        )
      )

  /** Project a single-basis kernel sampled on this basis's own grid. */
  def project(hrf: Hrf): Either[ReachableBasisError, ReachableBasisProjection] =
    if hrf.nbasis != 1 then Left(ReachableBasisError.MultiBasisMember(hrf.name, hrf.nbasis))
    else
      val sampled =
        try Right(hrf.evalDoubles(times.map(_.value)))
        catch
          case NonFatal(error) =>
            Left(ReachableBasisError.KernelEvaluation(Option(error.getMessage).getOrElse(error.toString)))
      sampled.flatMap { mat =>
        val values = new Array[Double](sampleCount)
        var l = 0
        while l < sampleCount do
          values(l) = mat(l, 0)
          l += 1
        projectCurve(values)
      }

  /** Project a raw curve already sampled on this basis's grid. */
  def projectCurve(values: Array[Double]): Either[ReachableBasisError, ReachableBasisProjection] =
    if values.length != sampleCount then
      Left(ReachableBasisError.CurveLengthMismatch(sampleCount, values.length))
    else
      var normSquared = 0.0
      var finite = true
      var l = 0
      while l < values.length do
        val value = values(l)
        if !java.lang.Double.isFinite(value) then finite = false
        normSquared += value * value
        l += 1
      if !finite || normSquared <= 0.0 then Left(ReachableBasisError.DegenerateCurve)
      else
        val j = rank
        val coefficients = new Array[Double](j)
        var column = 0
        while column < j do
          var sum = 0.0
          var row = 0
          while row < sampleCount do
            sum += curves(row, column) * values(row)
            row += 1
          coefficients(column) = sum
          column += 1
        val norm = math.sqrt(normSquared)
        val residuals = Vector.newBuilder[Double]
        var captured = 0.0
        column = 0
        while column < j do
          captured += coefficients(column) * coefficients(column)
          val remaining = math.max(normSquared - captured, 0.0)
          residuals += math.sqrt(remaining) / norm
          column += 1
        Right(
          ReachableBasisProjection(
            coefficients = DVec.tabulate(j)(coefficients(_)),
            curveNorm = norm,
            residualByRank = residuals.result()
          )
        )

object ReachableResponseBasis:

  /** Build the orthonormal reachable basis of `family` on a shared grid of
    * `samples` points over `[0, max span]`.
    *
    * Each member is sampled, checked finite and non-degenerate, and normalized
    * to unit l2 norm so the spectrum reflects shape, not amplitude convention.
    * The basis comes from the eigendecomposition of the smaller Gram matrix
    * (member-by-member or sample-by-sample, whichever has lower order) and is
    * truncated at `maxRank` and at eigenvalues below
    * `relativeEigenvalueFloor` times the leading eigenvalue.
    */
  def fromFamily(
      family: Seq[Hrf],
      samples: Int = 257,
      maxRank: Int = 24,
      relativeEigenvalueFloor: Double = 1e-12
  ): Either[ReachableBasisError, ReachableResponseBasis] =
    if family.isEmpty then Left(ReachableBasisError.EmptyFamily)
    else if samples < 2 then
      Left(ReachableBasisError.InvalidSampling(s"A reachable basis needs at least 2 samples; got $samples."))
    else if maxRank < 1 then
      Left(ReachableBasisError.InvalidSampling(s"maxRank must be at least 1; got $maxRank."))
    else if !(relativeEigenvalueFloor >= 0.0) || relativeEigenvalueFloor >= 1.0 then
      Left(ReachableBasisError.InvalidSampling(s"relativeEigenvalueFloor must lie in [0, 1); got $relativeEigenvalueFloor."))
    else
      family.find(_.nbasis != 1) match
        case Some(member) =>
          Left(ReachableBasisError.MultiBasisMember(member.name, member.nbasis))
        case None =>
          val span = family.iterator.map(_.span.value).max
          if !java.lang.Double.isFinite(span) || span <= 0.0 then
            Left(ReachableBasisError.InvalidSampling(s"The family span must be finite and positive; got $span seconds."))
          else
            val grid = Array.tabulate(samples)(l => l * span / (samples - 1).toDouble)
            sampleMembers(family, grid).flatMap { columns =>
              decompose(columns, samples, maxRank, relativeEigenvalueFloor).map {
                case (basisColumns, sigma, energy) =>
                  new ReachableResponseBasis(
                    times = grid.iterator.map(Seconds(_)).toVector,
                    curves = DMat.tabulate(samples, basisColumns.length)((l, j) => basisColumns(j)(l)),
                    singularValues = DVec.tabulate(sigma.length)(sigma(_)),
                    energyFractions = energy
                  )
              }
            }

  private def sampleMembers(
      family: Seq[Hrf],
      grid: Array[Double]
  ): Either[ReachableBasisError, Array[Array[Double]]] =
    val samples = grid.length
    val columns = new Array[Array[Double]](family.size)
    var failure: Option[ReachableBasisError] = None
    var index = 0
    val members = family.iterator
    while failure.isEmpty && members.hasNext do
      val member = members.next()
      val sampled =
        try Right(member.evalDoubles(grid))
        catch
          case NonFatal(error) =>
            Left(ReachableBasisError.KernelEvaluation(Option(error.getMessage).getOrElse(error.toString)))
      sampled match
        case Left(error) => failure = Some(error)
        case Right(mat) =>
          val curve = new Array[Double](samples)
          var normSquared = 0.0
          var finite = true
          var l = 0
          while l < samples do
            val value = mat(l, 0)
            if !java.lang.Double.isFinite(value) then finite = false
            curve(l) = value
            normSquared += value * value
            l += 1
          if !finite || normSquared <= 0.0 then
            failure = Some(ReachableBasisError.DegenerateMember(member.name))
          else
            val inverseNorm = 1.0 / math.sqrt(normSquared)
            l = 0
            while l < samples do
              curve(l) *= inverseNorm
              l += 1
            columns(index) = curve
            index += 1
    failure.toLeft(columns)

  private def decompose(
      columns: Array[Array[Double]],
      samples: Int,
      maxRank: Int,
      relativeEigenvalueFloor: Double
  ): Either[ReachableBasisError, (Array[Array[Double]], Array[Double], Vector[Double])] =
    val members = columns.length
    val useMemberGram = members <= samples
    val order = if useMemberGram then members else samples
    val gram = DMat.tabulate(order, order) { (a, b) =>
      if useMemberGram then
        val left = columns(a)
        val right = columns(b)
        var sum = 0.0
        var l = 0
        while l < samples do
          sum += left(l) * right(l)
          l += 1
        sum
      else
        var sum = 0.0
        var n = 0
        while n < members do
          val curve = columns(n)
          sum += curve(a) * curve(b)
          n += 1
        sum
    }
    var trace = 0.0
    var d = 0
    while d < order do
      trace += gram(d, d)
      d += 1
    Eigen
      .eigSymmetric(gram, EigenSelection.All)
      .left
      .map(error => ReachableBasisError.Numeric(error.toString))
      .map { decomposition =>
        val values = decomposition.eigenvalues
        val vectors = decomposition.eigenvectors
        val leading = math.max(values(order - 1), 0.0)
        val floor = leading * relativeEigenvalueFloor
        var keep = 0
        while keep < math.min(maxRank, order) &&
          values(order - 1 - keep) > floor &&
          values(order - 1 - keep) > 0.0
        do keep += 1
        val kept = math.max(keep, 1)
        val basis = new Array[Array[Double]](kept)
        val sigma = new Array[Double](kept)
        val energy = Vector.newBuilder[Double]
        var cumulative = 0.0
        var j = 0
        while j < kept do
          val source = order - 1 - j
          val eigenvalue = math.max(values(source), 0.0)
          sigma(j) = math.sqrt(eigenvalue)
          cumulative += eigenvalue
          energy += math.min(cumulative / trace, 1.0)
          val column = new Array[Double](samples)
          if useMemberGram then
            // u_j = A v_j / sigma_j with A's columns the normalized member curves.
            val inverseSigma = if sigma(j) > 0.0 then 1.0 / sigma(j) else 0.0
            var n = 0
            while n < members do
              val weight = vectors(n, source) * inverseSigma
              val curve = columns(n)
              var l = 0
              while l < samples do
                column(l) += weight * curve(l)
                l += 1
              n += 1
          else
            var l = 0
            while l < samples do
              column(l) = vectors(l, source)
              l += 1
          // Deterministic sign: the largest-magnitude sample is positive.
          var maxAbs = 0.0
          var maxAt = 0
          var normSquared = 0.0
          var l = 0
          while l < samples do
            val value = column(l)
            normSquared += value * value
            val magnitude = math.abs(value)
            if magnitude > maxAbs then
              maxAbs = magnitude
              maxAt = l
            l += 1
          val flip = if column(maxAt) < 0.0 then -1.0 else 1.0
          val renormalize =
            if normSquared > 0.0 then flip / math.sqrt(normSquared) else flip
          l = 0
          while l < samples do
            column(l) *= renormalize
            l += 1
          basis(j) = column
          j += 1
        // The Gram route loses orthogonality near the eigenvalue floor
        // (u = A v / sigma amplifies rounding for tiny sigma). One modified
        // Gram-Schmidt sweep restores machine-precision orthonormality while
        // leaving the well-separated leading directions essentially untouched.
        j = 0
        while j < kept do
          val column = basis(j)
          var previous = 0
          while previous < j do
            val other = basis(previous)
            var dot = 0.0
            var l = 0
            while l < samples do
              dot += other(l) * column(l)
              l += 1
            l = 0
            while l < samples do
              column(l) -= dot * other(l)
              l += 1
            previous += 1
          var normSquared = 0.0
          var l = 0
          while l < samples do
            normSquared += column(l) * column(l)
            l += 1
          val inverseNorm = if normSquared > 0.0 then 1.0 / math.sqrt(normSquared) else 0.0
          l = 0
          while l < samples do
            column(l) *= inverseNorm
            l += 1
          j += 1
        (basis, sigma, energy.result())
      }
