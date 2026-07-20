package scalafim.fmri.group

import gale.linalg.{DMat, DVec, Vec}
import scalafim.dataset.SubjectId

import scala.collection.immutable.VectorMap

/** Enough of the coefficient covariance to evaluate any group contrast's
  * variance, stored in whichever form the estimator produced.
  *
  *   - `Shared` — OLS: one `(XᵀX)⁻¹` for all samples, scaled per sample by the
  *     estimated residual variance.
  *   - `PerSample` — weighted GLM: one `(XᵀWX)⁻¹` per sample, variances treated
  *     as known (no residual scaling).
  */
sealed trait GroupCovariance:
  def terms: Int
  def samples: Int

  /** Variance of the linear combination `weightsᵀ β` at sample `s`. */
  def contrastVariance(weights: Array[Double], sample: Int): Double

object GroupCovariance:

  final case class Shared(inverse: DMat, residualVariance: DVec) extends GroupCovariance:
    require(inverse.rows == inverse.cols, "shared covariance must be square")
    require(inverse.rows == 0 || residualVariance.length > 0, "residual variance required")
    def terms: Int = inverse.rows
    def samples: Int = residualVariance.length
    def contrastVariance(weights: Array[Double], sample: Int): Double =
      quadForm(weights, inverse) * residualVariance(sample)

  /** Per-sample `(XᵀWX)⁻¹`, stored compactly as one row of packed lower-triangle
    * entries per sample in a single `[samples × termCount(termCount+1)/2]`
    * matrix — one backing array rather than one matrix object per sample.
    */
  final case class PerSample(termCount: Int, packed: DMat) extends GroupCovariance:
    require(termCount > 0, "per-sample covariance must have at least one term")
    require(packed.cols == termCount * (termCount + 1) / 2, "packed covariance width must match term count")
    def terms: Int = termCount
    def samples: Int = packed.rows
    def contrastVariance(weights: Array[Double], sample: Int): Double =
      // Reconstruct wᵀ Σ w from the packed lower triangle (off-diagonals count twice).
      var out = 0.0
      var idx = 0
      var i = 0
      while i < termCount do
        val wi = weights(i)
        var j = 0
        while j <= i do
          val contribution = wi * packed(sample, idx) * weights(j)
          out += (if i == j then contribution else 2.0 * contribution)
          idx += 1
          j += 1
        i += 1
      out

  private def quadForm(w: Array[Double], m: DMat): Double =
    var out = 0.0
    var i = 0
    while i < w.length do
      val wi = w(i)
      if wi != 0.0 then
        var j = 0
        while j < w.length do
          out += wi * m(i, j) * w(j)
          j += 1
      i += 1
    out

/** Between-subject heterogeneity diagnostics from a meta-analytic fit, one value
  * per sample: `tau2` (between-subject variance), Cochran's `q`, and `i2`.
  */
final case class Heterogeneity(tau2: DVec, q: DVec, i2: DVec):
  require(tau2.length == q.length && q.length == i2.length, "heterogeneity maps must align")

/** The group fit for one first-level contrast: per-term coefficient and standard
  * error maps over the sample axis, plus the reference distribution and (for
  * meta-analytic estimators) heterogeneity. Structurally the second-level analog
  * of `scalafim.fmri.fit.DenseFmriFitResult`.
  */
final case class GroupFit(
    contrast: FirstLevelContrastName,
    termNames: Vector[String],
    coefficients: DMat,
    standardErrors: DMat,
    covariance: GroupCovariance,
    statistic: GroupStatistic,
    heterogeneity: Option[Heterogeneity],
    space: GroupSpace
):
  require(coefficients.rows == termNames.length, "coefficient rows must match term names")
  require(standardErrors.rows == termNames.length, "standard error rows must match term names")
  require(coefficients.cols == standardErrors.cols, "coefficient and standard error samples must align")
  require(coefficients.cols == space.nSamples, "coefficient samples must match space")
  require(covariance.terms == termNames.length, "covariance terms must match term names")
  require(covariance.samples == coefficients.cols, "covariance samples must match coefficient samples")

  def terms: Int = termNames.length
  def samples: Int = coefficients.cols

  /** The per-sample statistic map for a single design term (coefficient / SE). */
  def term(name: String): Option[GroupContrastResult] =
    val row = termNames.indexOf(name)
    if row < 0 then None
    else
      val estimates = Vec.newBuilder(samples)
      val ses = Vec.newBuilder(samples)
      val stats = Vec.newBuilder(samples)
      val ps = Vec.newBuilder(samples)
      var s = 0
      while s < samples do
        val est = coefficients(row, s)
        val se = standardErrors(row, s)
        val t = est / se
        estimates(s) = est
        ses(s) = se
        stats(s) = t
        ps(s) = statistic.twoSidedP(t)
        s += 1
      Some(
        GroupContrastResult(
          name = GroupContrastName.unsafe(name),
          estimates = estimates.result(),
          standardErrors = ses.result(),
          statistics = stats.result(),
          pValues = ps.result(),
          statistic = statistic,
          space = space
        )
      )

/** The result of fitting a `GroupModel`: one `GroupFit` per first-level contrast,
  * sharing subjects, design terms, and sample space.
  */
final case class GroupResult(
    subjects: Vector[SubjectId],
    termNames: Vector[String],
    space: GroupSpace,
    weighting: GroupWeighting,
    fits: VectorMap[String, GroupFit]
):
  require(fits.nonEmpty, "group result must contain at least one contrast fit")

  def contrasts: Vector[String] = fits.keys.toVector
  def fit(contrast: String): Option[GroupFit] = fits.get(contrast)

  /** Evaluate a group contrast on a named first-level contrast's fit. */
  def contrast(firstLevel: String, contrast: GroupContrast): Either[GroupError, GroupContrastResult] =
    fits.get(firstLevel) match
      case Some(f) => contrast.evaluate(f)
      case None    => Left(GroupError.UnknownContrast(firstLevel))
