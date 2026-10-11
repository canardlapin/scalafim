package scalafim.fmri.mvpa.group

import gale.linalg.DMat
import scalafim.fmri.group.*
import scalafim.fmri.mvpa.AxisDigest
import scalafim.fmri.mvpa.pattern.SubjectCoordinateKey

/** Scientific declarations, not evidence that a sampling design is valid.
  * The null is zero marginal mean at each task/measurement coordinate. */
final case class SubjectPopulationContract(
    population: String, independentSubjectSampling: String, conditioning: String,
    firstLevelNuisance: String, multiplicityFamily: String
):
  require(Vector(population, independentSubjectSampling, conditioning, firstLevelNuisance,
    multiplicityFamily).forall(_.trim.nonEmpty))

enum SubjectMeanScope:
  case CommonEffectKnownGaussian
  case ApproximatePopulationMean

/** Every matrix is measurement-by-task. Empirical variance includes sampling
  * noise; it is not tau². Original full joint covariance and individual
  * component/operator expression remain available through input.rows. */
final class SubjectGroupSummary private (
    val fit: SubjectGroupFit, val contract: SubjectPopulationContract, val scope: SubjectMeanScope,
    val mean: DMat, val standardErrors: DMat, val pointwisePValues: DMat,
    val empiricalMean: DMat, val empiricalVariance: DMat,
    val heterogeneity: SubjectHeterogeneitySummary, val identity: String
):
  def input: SubjectGroupInput = fit.model.input
  def subjects: Vector[SubjectCoordinateKey] = input.subjectKeys
  def subjectExpressions: Vector[MaterializedSubjectGroupRow] = input.rows
  val nullHypothesis: String = "zero marginal mean at each task/measurement coordinate"
  val multiplicity: String = "pointwise unadjusted; no joint component or spatial test"
  val fittingScope: String = "separate subject fits followed by frozen coordinate transport; no joint hierarchical spatial optimizer"
  def prevalence: Either[SubjectGroupError, Nothing] = SubjectGroupSummary.prevalence

/** Under common effects tau² is fixed at zero, not estimated. Q and I² still
  * diagnose disagreement with the common-effect model. `cochranQ` is the
  * fixed-effect Q and `iSquared` is Higgins-Thompson max(0, (Q - df) / Q) for
  * every calculation. It is not metafor's tau²-based I² = tau²/(tau² + s²),
  * which differs from this value under Paule-Mandel with unequal variances. */
final class SubjectHeterogeneitySummary private[group] (
    val tauSquared: DMat, val cochranQ: DMat, val iSquared: DMat, val tauEstimated: Boolean
)

object SubjectGroupSummary:
  def prevalence: Either[SubjectGroupError, Nothing] =
    Left(SubjectGroupError.Unavailable("no admitted prevalence procedure; a mean effect or positive predictive improvement establishes neither prevalence nor an all-subject claim"))

  /** Intercept-only summaries deliberately cannot relabel a covariate slope
    * as a population mean. Native group solvers own all inferential numerics.
    * Mixed-effects output retains its explicit approximation policy. */
  def fit(input: SubjectGroupInput, contract: SubjectPopulationContract,
      calculation: SubjectGroupCalculation, maximumSummaryCells: Long = 1_000_000L
  ): Either[SubjectGroupError, SubjectGroupSummary] =
    val p = input.domain.axis.size; val r = input.taskAxis.size; val n = input.subjectKeys.size
    // Summary-owned numeric cells only; native solver scratch and input are excluded.
    val cells = 8 * BigInt(p) * r
    for
      _ <- if n >= 2 then Right(()) else Left(SubjectGroupError.Invalid("a group summary requires at least two independent subjects"))
      _ <- SubjectGroupBridge.budget(cells, maximumSummaryCells)
      model <- input.marginalModel(GroupDesign.intercept(n), input.data.subjects, calculation)
      fitted <- model.fit()
      fits = fitted.native.contrasts.map(name => fitted.native.fits(name))
      _ <- if fits.forall(f => f.failures.isEmpty && f.heterogeneity.nonEmpty) then Right(())
        else Left(SubjectGroupError.Numerical(s"incomplete group summary: ${fits.flatMap(_.failures).mkString(", ")}"))
      mean = DMat.tabulate(p, r)((j, k) => fits(k).coefficients(0, j))
      se = DMat.tabulate(p, r)((j, k) => fits(k).standardErrors(0, j))
      ps = DMat.tabulate(p, r)((j, k) => fits(k).statistic.twoSidedP(mean(j, k) / se(j, k)))
      empirical = DMat.tabulate(p, r): (j, k) =>
        var sum = 0.0; var s = 0
        while s < n do
          sum += input.rows(s).estimates(j, k) / n
          s += 1
        sum
      variance = DMat.tabulate(p, r): (j, k) =>
        var sum = 0.0; var s = 0
        while s < n do
          val delta = input.rows(s).estimates(j, k) - empirical(j, k)
          sum += delta * delta / (n - 1)
          s += 1
        sum
      tau = DMat.tabulate(p, r)((j, k) => fits(k).heterogeneity.get.tau2(j))
      q = DMat.tabulate(p, r)((j, k) => fits(k).heterogeneity.get.q(j))
      i2 = DMat.tabulate(p, r)((j, k) => fits(k).heterogeneity.get.i2(j))
      _ <- if Vector(mean, se, ps, empirical, variance, tau, q, i2).forall(finite) &&
          (0 until p).forall(j => (0 until r).forall(k => se(j, k) > 0.0)) then Right(())
        else Left(SubjectGroupError.Numerical("nonfinite or degenerate group summary; no subject or coordinate was dropped"))
    yield
      val scope = calculation match
        case SubjectGroupCalculation.KnownVarianceGaussianFixedEffects => SubjectMeanScope.CommonEffectKnownGaussian
        case SubjectGroupCalculation.ApproximateMixedEffects(_, _, _) => SubjectMeanScope.ApproximatePopulationMean
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.subject-group-summary.v1"); writer.string(input.identity)
        writer.string(calculation.toString)
        Vector(contract.population, contract.independentSubjectSampling, contract.conditioning,
          contract.firstLevelNuisance, contract.multiplicityFamily).foreach(writer.string)
      new SubjectGroupSummary(fitted, contract, scope, mean, se, ps, empirical, variance,
        new SubjectHeterogeneitySummary(tau, q, i2, scope == SubjectMeanScope.ApproximatePopulationMean), identity)

  private def finite(matrix: DMat): Boolean =
    (0 until matrix.rows).forall(i => (0 until matrix.cols).forall(j => matrix(i, j).isFinite))
