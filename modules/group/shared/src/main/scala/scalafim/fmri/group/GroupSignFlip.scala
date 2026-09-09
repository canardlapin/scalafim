package scalafim.fmri.group

import gale.linalg.DVec
import resample4s.core.{Rand, Seed, StreamDomain, StreamPath}
import scalafim.dataset.SubjectId

/** An explicit scientific declaration, not a property inferred from the data.
  * Conditional on selection and any precisions, subject errors about the tested
  * common center must be independent and symmetric. One row per subject.
  */
enum GroupSymmetryAssumption:
  case IndependentSymmetricErrorsConditionalOnSelectionAndPrecisions

enum GroupSymmetryWeighting:
  case EqualSubjects, FixedInverseVariance

enum GroupSignSampling:
  case Exact
  /** Independent uniform actions with replacement; the identity is added separately. */
  case MonteCarlo(draws: Int, seed: Long)

/** Immutable, reusable subject-bound actions. Subject IDs have canonical order,
  * so input row reordering preserves the realized Monte Carlo experiment.
  * The same plan may be reused for spatial blocks; this does not confer FWER control.
  */
final class GroupSignFlipPlan private (
    val subjects: Vector[SubjectId],
    val sampling: GroupSignSampling,
    val draws: Int,
    private val multipliers: Array[Byte]
):
  def sign(draw: Int, subject: Int): Int =
    require(draw >= 0 && draw < draws && subject >= 0 && subject < subjects.length)
    multipliers(draw * subjects.length + subject).toInt

  // All callers provide a checked subject-length buffer. Keep the byte traversal
  // inside its immutable owner instead of checking a public index at every multiply.
  private[group] def countExtreme(values: Array[Double], observed: Double, tolerance: Double): Int =
    require(values.length == subjects.length)
    var count = 0
    var draw = 0
    var offset = 0
    while draw < draws do
      var sum = 0.0
      var i = 0
      while i < values.length do
        sum += multipliers(offset + i) * values(i)
        i += 1
      if math.abs(sum) + tolerance >= observed then count += 1
      offset += values.length
      draw += 1
    count

  val algorithmId: String = "group-symmetry/v1;resample4s-6bc4172;seed-path/v1;domain-1004"

  def signBytes: Long = multipliers.length.toLong
  def minimumP: Double = sampling match
    case GroupSignSampling.Exact => 2.0 / draws // global negation ties in a two-sided test
    case GroupSignSampling.MonteCarlo(_, _) => 1.0 / (draws.toDouble + 1.0)

object GroupSignFlipPlan:
  /** Bound the sign cache before allocating. Exact enumeration is limited to
    * 16 subjects; larger designs require an explicitly selected Monte Carlo budget.
    */
  def compile(
      subjects: Vector[SubjectId],
      sampling: GroupSignSampling,
      maxSignBytes: Long = 64L * 1024 * 1024
  ): Either[GroupError, GroupSignFlipPlan] =
    SubjectAxis.from(subjects).flatMap { _ =>
      val canonical = subjects.sortBy(_.value)
      val n = canonical.length
      val count = sampling match
        case GroupSignSampling.Exact => if n <= 16 then 1 << n else 0
        case GroupSignSampling.MonteCarlo(draws, _) => draws
      if n < 2 then Left(GroupError.InsufficientSubjects(n, 1))
      else if count <= 0 then Left(GroupError.UnsupportedInference(
        "sign inference requires 2..16 subjects for exact enumeration or a positive Monte Carlo draw count"))
      else if maxSignBytes <= 0 || n.toLong * count > maxSignBytes || n.toLong * count > Int.MaxValue then
        Left(GroupError.InferenceResourceLimit(s"sign cache needs ${n.toLong * count} bytes; limit is $maxSignBytes"))
      else
        val signs = new Array[Byte](n * count)
        var draw = 0
        var failure: Option[GroupError] = None
        while draw < count && failure.isEmpty do
          sampling match
            case GroupSignSampling.Exact =>
              var i = 0
              while i < n do
                signs(draw * n + i) = (if (draw & (1 << i)) == 0 then 1 else -1).toByte
                i += 1
            case GroupSignSampling.MonteCarlo(_, seed) =>
              val stream = for
                domain <- StreamDomain.custom(1004)
                path <- StreamPath.of(domain, draw)
              yield Rand.fromSeed(Seed.fromLong(seed).derive(path))
              stream match
                case Left(error) => failure = Some(GroupError.NumericalFailure(error.message))
                case Right(initial) =>
                  var random = initial
                  var i = 0
                  while i < n && failure.isEmpty do
                    random.nextIntBounded(2) match
                      case Left(error) => failure = Some(GroupError.NumericalFailure(error.message))
                      case Right((next, bit)) =>
                        random = next
                        signs(draw * n + i) = (if bit == 0 then 1 else -1).toByte
                    i += 1
          draw += 1
        failure match
          case Some(error) => Left(error)
          case None => Right(new GroupSignFlipPlan(canonical, sampling, count, signs))
    }

/** Pointwise inference on a common center. `score` is a normalized signed sum,
  * not a t statistic; no parametric degrees of freedom or confidence interval is
  * fabricated. Inverting tests of `nullCenter` defines a confidence set.
  */
final class GroupSignFlipResult private[group] (
    val algorithmId: String,
    val contrast: String,
    val subjects: Vector[SubjectId],
    val space: GroupSpace,
    val nullCenter: Double,
    val assumption: GroupSymmetryAssumption,
    val weighting: GroupSymmetryWeighting,
    val sampling: GroupSignSampling,
    val draws: Int,
    val minimumP: Double,
    val estimate: DVec,
    val score: DVec,
    val pValues: DVec,
    val exceedances: Vector[Int],
    val failures: Vector[GroupSampleFailure]
)

object GroupSignFlip:
  /** Two-sided common-center test for an intercept-only design. Precisions, if
    * used, stay fixed under every action; heterogeneity need not be estimated.
    * A paired question must be reduced to one valid contrast per subject first.
    * Covariates, repeated rows, and general group contrasts are not supported.
    */
  def test[V <: VarianceCapability](
      data: GroupData[V],
      design: GroupDesign,
      contrast: String,
      plan: GroupSignFlipPlan,
      assumption: GroupSymmetryAssumption,
      weighting: GroupSymmetryWeighting = GroupSymmetryWeighting.EqualSubjects,
      nullCenter: Double = 0.0
  ): Either[GroupError, GroupSignFlipResult] =
    if !nullCenter.isFinite then Left(GroupError.NonFiniteData("null center"))
    else if data.subjects.toSet != plan.subjects.toSet then
      Left(GroupError.UnsupportedInference("sign plan subject identities do not match the data"))
    else if design.subjects != data.nSubjects then Left(GroupError.subjectMismatch(data.nSubjects, design.subjects))
    else if design.terms != 1 || (0 until design.subjects).exists(i => design.matrix(i, 0) != 1.0) then
      Left(GroupError.UnsupportedInference("sign inference requires an intercept-only column of ones; nuisance covariates and group comparisons need separate admission"))
    else data.response(contrast) match
      case None => Left(GroupError.UnknownContrast(contrast))
      case Some(response) =>
        if weighting == GroupSymmetryWeighting.FixedInverseVariance && !response.hasVariances then
          Left(GroupError.MissingVariances("symmetry:fixed-inverse-variance"))
        else run(data, response, contrast, plan, assumption, weighting, nullCenter)

  private def run[V <: VarianceCapability](
      data: GroupData[V], response: GroupResponse[V], contrast: String,
      plan: GroupSignFlipPlan, assumption: GroupSymmetryAssumption,
      weighting: GroupSymmetryWeighting, nullCenter: Double
  ): Either[GroupError, GroupSignFlipResult] =
    val n = data.nSubjects
    val m = data.nSamples
    val byId = data.subjects.zipWithIndex.toMap
    val rows = plan.subjects.map(byId).toArray
    val estimates = new Array[Double](m)
    val scores = new Array[Double](m)
    val ps = new Array[Double](m)
    val counts = new Array[Int](m)
    val residual = new Array[Double](n)
    val failures = Vector.newBuilder[GroupSampleFailure]
    var sample = 0
    while sample < m do
      var scale = math.abs(nullCenter)
      var minVariance = Double.PositiveInfinity
      var i = 0
      while i < n do
        scale = math.max(scale, math.abs(response.effects(rows(i), sample)))
        if weighting == GroupSymmetryWeighting.FixedInverseVariance then
          minVariance = math.min(minVariance, response.variances.get(rows(i), sample))
        i += 1
      if scale == 0.0 then scale = 1.0
      var sumW = 0.0
      var sumY = 0.0
      var sum = 0.0
      var sumSquares = 0.0
      var valid = true
      i = 0
      while i < n do
        val w = weighting match
          case GroupSymmetryWeighting.EqualSubjects => 1.0
          case GroupSymmetryWeighting.FixedInverseVariance => minVariance / response.variances.get(rows(i), sample)
        if w == 0.0 then valid = false
        val y = response.effects(rows(i), sample) / scale
        val rawDifference = response.effects(rows(i), sample) - nullCenter
        val difference = if rawDifference.isFinite then rawDifference / scale else y - nullCenter / scale
        residual(i) = w * difference
        if residual(i) == 0.0 && response.effects(rows(i), sample) != nullCenter then valid = false
        sumW += w
        sumY += w * y
        sum += residual(i)
        sumSquares += residual(i) * residual(i)
        i += 1
      estimates(sample) = (sumY / sumW) * scale
      // Scaling the residuals again avoids underflow in the sum of squares when
      // precisions suppress the only nonzero subject residuals.
      var maxResidual = 0.0
      i = 0
      while i < n do
        maxResidual = math.max(maxResidual, math.abs(residual(i)))
        i += 1
      if maxResidual > 0.0 then
        sum = 0.0
        sumSquares = 0.0
        i = 0
        while i < n do
          residual(i) /= maxResidual
          sum += residual(i)
          sumSquares += residual(i) * residual(i)
          i += 1
      scores(sample) = if sumSquares == 0.0 then 0.0 else sum / math.sqrt(sumSquares)
      if !estimates(sample).isFinite || !scores(sample).isFinite then valid = false
      if !valid then
        failures += GroupSampleFailure(sample, GroupError.NumericalFailure("sign statistic has unrepresentable precision ratios or effect scale"))
        estimates(sample) = Double.NaN
        scores(sample) = Double.NaN
        ps(sample) = Double.NaN
      else
        val observed = math.abs(sum)
        // After max normalization, each summand is <=1. This conservative bound
        // includes floating-point ties independent of the observed cancellation.
        val tieTolerance = 8.0 * n * n * math.ulp(1.0)
        val count = plan.countExtreme(residual, observed, tieTolerance)
        counts(sample) = count
        ps(sample) = plan.sampling match
          case GroupSignSampling.Exact => count.toDouble / plan.draws
          case GroupSignSampling.MonteCarlo(_, _) => (count.toDouble + 1.0) / (plan.draws.toDouble + 1.0)
      sample += 1
    val failed = failures.result()
    if failed.length == m then Left(GroupError.AllSamplesFailed(failed))
    else Right(new GroupSignFlipResult(plan.algorithmId, contrast, data.subjects, data.space, nullCenter,
      assumption, weighting, plan.sampling, plan.draws, plan.minimumP,
      DVec.fromSeq(estimates.toIndexedSeq), DVec.fromSeq(scores.toIndexedSeq), DVec.fromSeq(ps.toIndexedSeq), counts.toVector, failed))
