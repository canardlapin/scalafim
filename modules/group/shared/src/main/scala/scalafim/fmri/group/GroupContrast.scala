package scalafim.fmri.group

import gale.linalg.{DVec, Vec}

/** A group-level t-contrast: a named linear combination of design terms,
  * keyed by term name and aligned to a fit by name (unknown terms are errors).
  * The second-level analog of `scalafim.fmri.fit.TContrast`.
  */
final case class GroupContrast(name: GroupContrastName, weights: Map[DesignTermName, Double]):
  require(weights.nonEmpty, "contrast weights must be non-empty")
  require(weights.values.forall(_.isFinite), "contrast weights must be finite")

  def evaluate(fit: GroupFit): Either[GroupError, GroupContrastResult] =
    weightVector(fit.termNames).map { w =>
      val samples = fit.samples
      val estimates = Vec.newBuilder(samples)
      val standardErrors = Vec.newBuilder(samples)
      val statistics = Vec.newBuilder(samples)
      val pValues = Vec.newBuilder(samples)

      var s = 0
      while s < samples do
        var estimate = 0.0
        var term = 0
        while term < w.length do
          val wj = w(term)
          if wj != 0.0 then estimate += wj * fit.coefficients(term, s)
          term += 1
        val variance = fit.covariance.contrastVariance(w, s)
        // Clamp tiny negative quadratic forms (rounding on a near-null contrast) to 0.
        val se = if variance < 0.0 && variance > -1e-12 then 0.0 else math.sqrt(variance)
        val stat = estimate / se
        estimates(s) = estimate
        standardErrors(s) = se
        statistics(s) = stat
        pValues(s) = fit.statistic.twoSidedP(stat)
        s += 1

      GroupContrastResult(
        name = name,
        estimates = estimates.result(),
        standardErrors = standardErrors.result(),
        statistics = statistics.result(),
        pValues = pValues.result(),
        statistic = fit.statistic,
        space = fit.space,
        failures = fit.failures
      )
    }

  private[group] def weightVector(termNames: Vector[String]): Either[GroupError, Array[Double]] =
    val known = termNames.toSet
    weights.keys.find(term => !known.contains(term.value)) match
      case Some(unknown) => Left(GroupError.UnknownContrastTerm(unknown.value))
      case None =>
        val out = new Array[Double](termNames.length)
        var nonZero = false
        var i = 0
        while i < termNames.length do
          val value = weights.getOrElse(DesignTermName.unsafe(termNames(i)), 0.0)
          out(i) = value
          if value != 0.0 then nonZero = true
          i += 1
        if nonZero then Right(out) else Left(GroupError.EmptyContrast(name.value))

object GroupContrast:
  def fromStrings(name: String, weights: Map[String, Double]): Either[GroupError, GroupContrast] =
    for
      contrastName <- GroupContrastName(name)
      typedWeights <- parseWeights(weights)
      _ <- if typedWeights.values.exists(_ != 0.0) then Right(()) else Left(GroupError.EmptyContrast(name))
    yield GroupContrast(contrastName, typedWeights)

  def unsafe(name: String, weights: Map[String, Double]): GroupContrast =
    fromStrings(name, weights).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** The unit contrast selecting a single design term. */
  def term(termName: String): GroupContrast =
    unsafe(termName, Map(termName -> 1.0))

  /** A difference of two design terms, e.g. `patients - controls`. */
  def difference(name: String, positive: String, negative: String): GroupContrast =
    val pos = DesignTermName.unsafe(positive)
    val neg = DesignTermName.unsafe(negative)
    val weights = Map(pos -> 1.0).updated(neg, (if pos == neg then 1.0 else 0.0) - 1.0)
    GroupContrast(GroupContrastName.unsafe(name), weights)

  private def parseWeights(weights: Map[String, Double]): Either[GroupError, Map[DesignTermName, Double]] =
    weights.foldLeft[Either[GroupError, Map[DesignTermName, Double]]](Right(Map.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), (term, weight)) =>
        if !weight.isFinite then Left(GroupError.NonFiniteData("contrast weights"))
        else DesignTermName(term).flatMap { name =>
          if acc.contains(name) then Left(GroupError.DuplicateTerms(Vector(name.value)))
          else Right(acc.updated(name, weight))
        }
    }

/** Per-sample group statistics for one contrast: estimate, standard error, test
  * statistic, and two-sided p-value, with the reference distribution attached so
  * FDR correction can be applied.
  */
final case class GroupContrastResult(
    name: GroupContrastName,
    estimates: DVec,
    standardErrors: DVec,
    statistics: DVec,
    pValues: DVec,
    statistic: GroupStatistic,
    space: GroupSpace,
    failures: Vector[GroupSampleFailure] = Vector.empty
):
  require(failures.map(_.sample).distinct.length == failures.length, "sample failure indices must be unique")
  require(failures.forall(f => f.sample >= 0 && f.sample < space.nSamples), "sample failure indices must match space")

  require(estimates.length == space.nSamples, "estimates must match sample space")
  require(standardErrors.length == space.nSamples, "standard errors must match sample space")
  require(statistics.length == space.nSamples, "statistics must match sample space")
  require(pValues.length == space.nSamples, "p-values must match sample space")

  def pValue(sample: Int): Either[GroupError, PValue] =
    PValue(pValues(sample))

  /** FDR-adjusted p-values (q-values) over this contrast's map. */
  def adjustedP(method: FdrMethod = FdrMethod.BenjaminiHochberg): DVec =
    val p = pValues.toSeq.toArray
    val q = method match
      case FdrMethod.BenjaminiHochberg => Fdr.benjaminiHochberg(p)
      case FdrMethod.BenjaminiYekutieli => Fdr.benjaminiYekutieli(p)
    val out = Vec.newBuilder(q.length)
    var i = 0
    while i < q.length do
      out(i) = q(i)
      i += 1
    out.result()

enum FdrMethod:
  case BenjaminiHochberg
  case BenjaminiYekutieli
