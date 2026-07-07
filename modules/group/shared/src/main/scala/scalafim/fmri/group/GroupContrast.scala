package scalafim.fmri.group

import scalafim.linalg.DoubleVector

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
      val estimates = new Array[Double](samples)
      val standardErrors = new Array[Double](samples)
      val statistics = new Array[Double](samples)
      val pValues = new Array[Double](samples)

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
        estimates = DoubleVector.unsafe(estimates),
        standardErrors = DoubleVector.unsafe(standardErrors),
        statistics = DoubleVector.unsafe(statistics),
        pValues = DoubleVector.unsafe(pValues),
        statistic = fit.statistic,
        space = fit.space
      )
    }

  private def weightVector(termNames: Vector[String]): Either[GroupError, Array[Double]] =
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
    yield GroupContrast(contrastName, typedWeights)

  def unsafe(name: String, weights: Map[String, Double]): GroupContrast =
    fromStrings(name, weights).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** The unit contrast selecting a single design term. */
  def term(termName: String): GroupContrast =
    unsafe(termName, Map(termName -> 1.0))

  /** A difference of two design terms, e.g. `patients - controls`. */
  def difference(name: String, positive: String, negative: String): GroupContrast =
    unsafe(name, Map(positive -> 1.0, negative -> -1.0))

  private def parseWeights(weights: Map[String, Double]): Either[GroupError, Map[DesignTermName, Double]] =
    weights.foldLeft[Either[GroupError, Map[DesignTermName, Double]]](Right(Map.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), (term, weight)) =>
        DesignTermName(term).map(name => acc.updated(name, weight))
    }

/** Per-sample group statistics for one contrast: estimate, standard error, test
  * statistic, and two-sided p-value, with the reference distribution attached so
  * FDR correction can be applied.
  */
final case class GroupContrastResult(
    name: GroupContrastName,
    estimates: DoubleVector,
    standardErrors: DoubleVector,
    statistics: DoubleVector,
    pValues: DoubleVector,
    statistic: GroupStatistic,
    space: GroupSpace
):
  require(estimates.length == space.nSamples, "estimates must match sample space")
  require(standardErrors.length == space.nSamples, "standard errors must match sample space")
  require(statistics.length == space.nSamples, "statistics must match sample space")
  require(pValues.length == space.nSamples, "p-values must match sample space")

  def pValue(sample: Int): Either[GroupError, PValue] =
    PValue(pValues(sample))

  /** FDR-adjusted p-values (q-values) over this contrast's map. */
  def adjustedP(method: FdrMethod = FdrMethod.BenjaminiHochberg): DoubleVector =
    val p = pValues.copyData
    val q = method match
      case FdrMethod.BenjaminiHochberg => Fdr.benjaminiHochberg(p)
      case FdrMethod.BenjaminiYekutieli => Fdr.benjaminiYekutieli(p)
    DoubleVector.unsafe(q)

enum FdrMethod:
  case BenjaminiHochberg
  case BenjaminiYekutieli
