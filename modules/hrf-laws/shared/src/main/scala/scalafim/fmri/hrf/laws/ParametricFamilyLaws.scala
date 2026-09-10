package scalafim.fmri.hrf.laws

import scalafim.fmri.hrf.Lag
import scalafim.fmri.hrf.family.{JetLayout, NormalizationRule, ParametricHrfFamily, ShapePoint}

/** Laws every [[ParametricHrfFamily]] must obey, as functions returning failures. */
object ParametricFamilyLaws:

  private def check(law: String, detail: String, error: Double, tol: Double): Option[LawFailure] =
    if error <= tol then None else Some(LawFailure(law, detail, error))

  private def shifted(point: ShapePoint, index: Int, delta: Double): ShapePoint =
    ShapePoint.unsafe(point.coordinates.updated(index, point.coordinates(index) + delta))

  /** Analytic first and mixed second parameter derivatives agree with central
    * finite differences of the value and of the first derivatives.
    */
  def jetsMatchFiniteDifferences(
      family: ParametricHrfFamily,
      point: ShapePoint,
      lags: Array[Double],
      step: Double = 1e-4,
      tol: Double = 1e-6
  ): Vector[LawFailure] =
    val d = family.dimension
    val n = lags.length
    val comps = family.jetComponents
    val jet = new Array[Double](comps * n)
    val plus = new Array[Double](comps * n)
    val minus = new Array[Double](comps * n)
    family.jetInto(lags, point, jet)
    val failures = Vector.newBuilder[LawFailure]
    var p = 0
    while p < d do
      family.jetInto(lags, shifted(point, p, step), plus)
      family.jetInto(lags, shifted(point, p, -step), minus)
      var worstFirst = 0.0
      var i = 0
      while i < n do
        val fd = (plus(i) - minus(i)) / (2.0 * step)
        worstFirst = math.max(worstFirst, math.abs(fd - jet(JetLayout.first(p) * n + i)))
        i += 1
      failures ++= check("jetFirstDerivative", s"${family.name} d/d${family.chart.names(p)} at $point", worstFirst, tol)
      var q = 0
      while q < d do
        var worstSecond = 0.0
        i = 0
        while i < n do
          val fd = (plus(JetLayout.first(q) * n + i) - minus(JetLayout.first(q) * n + i)) / (2.0 * step)
          worstSecond = math.max(worstSecond, math.abs(fd - jet(JetLayout.second(d, p, q) * n + i)))
          i += 1
        failures ++= check("jetSecondDerivative", s"${family.name} d2/d${family.chart.names(p)}d${family.chart.names(q)} at $point", worstSecond, tol)
        q += 1
      p += 1
    failures.result()

  /** Normalisation scale derivatives agree with finite differences of the scale. */
  def scaleJetsMatchFiniteDifferences(
      family: ParametricHrfFamily,
      rule: NormalizationRule,
      point: ShapePoint,
      step: Double = 1e-4,
      tol: Double = 1e-6
  ): Vector[LawFailure] =
    val d = family.dimension
    val comps = family.jetComponents
    val jet = new Array[Double](comps)
    val plus = new Array[Double](comps)
    val minus = new Array[Double](comps)
    family.scaleJetInto(rule, point, jet)
    val failures = Vector.newBuilder[LawFailure]
    var p = 0
    while p < d do
      family.scaleJetInto(rule, shifted(point, p, step), plus)
      family.scaleJetInto(rule, shifted(point, p, -step), minus)
      val fd = (plus(JetLayout.Value) - minus(JetLayout.Value)) / (2.0 * step)
      failures ++= check("scaleFirstDerivative", s"${family.name} ${rule.label} d/d${family.chart.names(p)} at $point", math.abs(fd - jet(JetLayout.first(p))), tol)
      var q = 0
      while q < d do
        val fd2 = (plus(JetLayout.first(q)) - minus(JetLayout.first(q))) / (2.0 * step)
        failures ++= check("scaleSecondDerivative", s"${family.name} ${rule.label} d2 at $point", math.abs(fd2 - jet(JetLayout.second(d, p, q))), tol)
        q += 1
      p += 1
    failures.result()

  /** The family kernel times its library scale reproduces the library kernel. */
  def realisationMatchesLibraryKernel(
      family: ParametricHrfFamily,
      point: ShapePoint,
      lags: Array[Double],
      tol: Double = 1e-12
  ): Vector[LawFailure] =
    val n = lags.length
    val values = new Array[Double](n)
    family.evalInto(lags, point, values)
    val scale = new Array[Double](family.jetComponents)
    family.scaleJetInto(family.libraryNormalization, point, scale)
    val hrf = family.toHrf(point)
    var worst = 0.0
    var i = 0
    while i < n do
      val library = hrf(Lag(lags(i))).data(0)
      worst = math.max(worst, math.abs(values(i) * scale(JetLayout.Value) - library))
      i += 1
    check("realisationMatchesLibraryKernel", s"${family.name} at $point", worst, tol).toVector

  /** Kernel and jets vanish at negative lags. */
  def causality(family: ParametricHrfFamily, point: ShapePoint, negativeLags: Array[Double] = Array(-100.0, -10.0, -1.0, -1e-9)): Vector[LawFailure] =
    val n = negativeLags.length
    val jet = new Array[Double](family.jetComponents * n)
    family.jetInto(negativeLags, point, jet)
    val worst = jet.map(math.abs).maxOption.getOrElse(0.0)
    check("familyCausality", s"${family.name} at $point", worst, 0.0).toVector
