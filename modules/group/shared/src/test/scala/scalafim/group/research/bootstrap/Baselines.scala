package scalafim.group.research.bootstrap

import gale.linalg.Matrix
import scalafim.dataset.SubjectId
import scalafim.fmri.group.{Distributions, GroupData, GroupDesign, GroupEngine, GroupModel, GroupSpace, MetaInference, TauEstimator}

/** A baseline test of the contrast on one study. */
final case class BaselineResult(estimate: Double, standardError: Double, statistic: Double, pValue: Double)

/** HC3 sandwich baselines (§3, O5: research harness only). For weights w
  * (1 for equal-subject OLS, 1/v for inverse-v WLS):
  *   A = (X'WX)^{-1}, beta = A X'W y, r = y - X beta, h_i = w_i x_i' A x_i,
  *   V_HC3 = A [sum_i w_i^2 r_i^2/(1 - h_i)^2 x_i x_i'] A,
  *   T = (c'beta - b0)/sqrt(c' V_HC3 c), referred to t(n - p).
  * Matches R `sandwich::vcovHC(lm(y ~ X - 1, weights = w), type = "HC3")`.
  */
object Hc3:
  def fit(design: ResearchDesign, y: Array[Double], weights: Array[Double], b0: Double): Either[String, BaselineResult] =
    val n = design.n
    val p = design.p
    if !weights.forall(w => w > 0.0 && w.isFinite) then Left("HC3 weights must be positive and finite")
    else
      val g = new Array[Double](p * p)
      val rhs = new Array[Double](p)
      var i = 0
      while i < n do
        var a = 0
        while a < p do
          val wa = weights(i) * design.row(i, a)
          rhs(a) += wa * y(i)
          var b = 0
          while b < p do
            g(a * p + b) += wa * design.row(i, b)
            b += 1
          a += 1
        i += 1
      Inverse.symmetric(g, p).flatMap { inv =>
        val beta = Array.tabulate(p)(a => (0 until p).map(b => inv(a * p + b) * rhs(b)).sum)
        val meat = new Array[Double](p * p)
        var leverageOk = true
        i = 0
        while i < n do
          val xi = Array.tabulate(p)(a => design.row(i, a))
          val r = y(i) - (0 until p).map(a => xi(a) * beta(a)).sum
          val h = weights(i) * (0 until p).map(a => (0 until p).map(b => xi(a) * inv(a * p + b) * xi(b)).sum).sum
          if !(h < 1.0) then leverageOk = false
          val omega = weights(i) * weights(i) * r * r / ((1.0 - h) * (1.0 - h))
          var a = 0
          while a < p do
            var b = 0
            while b < p do
              meat(a * p + b) += omega * xi(a) * xi(b)
              b += 1
            a += 1
          i += 1
        if !leverageOk then Left("HC3 undefined: a leverage is 1")
        else
          val c = design.contrast
          val ac = Array.tabulate(p)(a => (0 until p).map(b => inv(a * p + b) * c(b)).sum)
          val variance = (0 until p).map(a => (0 until p).map(b => ac(a) * meat(a * p + b) * ac(b)).sum).sum
          val estimate = (0 until p).map(a => c(a) * beta(a)).sum
          val se = math.sqrt(variance)
          val t = (estimate - b0) / se
          if !t.isFinite then Left("HC3 statistic is not finite")
          else Right(BaselineResult(estimate, se, t, Distributions.studentTTwoSidedP(t, design.residualDf)))
      }

  def equalWeight(design: ResearchDesign, y: Array[Double], b0: Double): Either[String, BaselineResult] =
    fit(design, y, Array.fill(design.n)(1.0), b0)

  def inverseVariance(design: ResearchDesign, y: Array[Double], v: Array[Double], b0: Double): Either[String, BaselineResult] =
    fit(design, y, v.map(1.0 / _), b0)

private[bootstrap] object Inverse:
  /** Inverse of a small symmetric positive-definite matrix via Cholesky. */
  def symmetric(a: Array[Double], p: Int): Either[String, Array[Double]] =
    val f = a.clone()
    if !Cholesky.factor(f, p, 1e-13) then Left("matrix is not numerically positive definite")
    else
      val out = new Array[Double](p * p)
      val col = new Array[Double](p)
      var j = 0
      while j < p do
        var a0 = 0
        while a0 < p do
          var s = if a0 == j then 1.0 else 0.0
          var k = 0
          while k < a0 do
            s -= f(a0 * p + k) * col(k)
            k += 1
          col(a0) = s / f(a0 * p + a0)
          a0 += 1
        a0 = p - 1
        while a0 >= 0 do
          var s = col(a0)
          var k = a0 + 1
          while k < p do
            s -= f(k * p + a0) * col(k)
            k += 1
          col(a0) = s / f(a0 * p + a0)
          a0 -= 1
        var r = 0
        while r < p do
          out(r * p + j) = col(r)
          r += 1
        j += 1
      Right(out)

/** Native PM + mKH through the public `modules/group` API (the power comparator
  * of §6). Studies of one design are fitted as sample columns of one call,
  * which prepares the design once. b0 is handled by shifting y by b0 X c/|c|^2.
  */
object NativeBaseline:
  final case class Column(statistic: Double, pValue: Double, failed: Boolean)

  def pmMkh(design: ResearchDesign, ys: Vector[Array[Double]], vs: Vector[Array[Double]], b0: Double): Either[String, Vector[Column]] =
    design.contrastTerm match
      case None => Left("the native baseline needs a single-term unit contrast")
      case Some(term) =>
        val n = design.n
        val samples = ys.length
        def value[A](e: Either[scalafim.fmri.group.GroupError, A]): Either[String, A] = e.left.map(_.message)
        for
          groupDesign <- value(GroupDesign.fromMatrix(Matrix.tabulate(n, design.p)((i, j) => design.row(i, j)), design.terms))
          data <- value(
            GroupData.withVariances(
              Vector.tabulate(n)(i => SubjectId(s"s$i")),
              GroupSpace.SampleAxis(samples),
              "effect",
              Matrix.tabulate(n, samples)((i, s) => ys(s)(i) - b0 * design.offsetUnit(i)),
              Matrix.tabulate(n, samples)((i, s) => vs(s)(i))
            )
          )
          model <- value(GroupModel.mixedEffects(data, groupDesign, TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung))
          result <- value(GroupEngine.fit(model))
          fit <- result.fit("effect").toRight("native fit is missing")
          contrast <- fit.term(term).toRight(s"native term $term is missing")
        yield
          val failed = contrast.failures.map(_.sample).toSet
          Vector.tabulate(samples)(s => Column(contrast.statistics(s), contrast.pValues(s), failed.contains(s)))

/** Oracle WLS z with the true sigma^2 + tau^2 (diagnostic, non-feasible: uses simulation truth). */
object OracleZ:
  def test(design: ResearchDesign, y: Array[Double], sigma2: Array[Double], tau2: Double, b0: Double): Either[String, BaselineResult] =
    val fitter = new StudyFitter(design)
    val status = fitter.fitFull(y, sigma2, TauPolicy.Fixed(tau2))
    if !status.ok then Left(status.message)
    else
      val se = math.sqrt(fitter.contrastVariance)
      val z = (fitter.estimate - b0) / se
      Right(BaselineResult(fitter.estimate, se, z, Distributions.normalTwoSidedP(z)))
