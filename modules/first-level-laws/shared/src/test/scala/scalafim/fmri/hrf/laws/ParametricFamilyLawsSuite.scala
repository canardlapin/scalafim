package scalafim.fmri.hrf.laws

import scalafim.fmri.hrf.family.{Cascade34Family, GaussianFamily, LwuFamily, NormalizationRule}

class ParametricFamilyLawsSuite extends munit.FunSuite:

  test("Cascade34 jets, scale jets, realisation and causality laws hold across its chart"):
    val cascade = Cascade34Family.Default
    for (p, q, r) <- Seq((0.26, 0.11, 0.05), (0.5, 0.4, 0.3), (0.95, 0.79, 0.75)) do
      val point = cascade.chart.point(math.log(p), math.log(q / (1.0 - q)), r).fold(e => fail(e.message), identity)
      val failures =
        ParametricFamilyLaws.jetsMatchFiniteDifferences(cascade, point, Array.tabulate(500)(i => i * 0.2)) ++
          ParametricFamilyLaws.realisationMatchesLibraryKernel(cascade, point, Array(0.0, 0.1, 3.0, 20.0, 100.0)) ++
          ParametricFamilyLaws.causality(cascade, point) ++
          NormalizationRule.values.filter(cascade.supports).toVector.flatMap: rule =>
            ParametricFamilyLaws.scaleJetsMatchFiniteDifferences(cascade, rule, point)
      assert(failures.isEmpty, failures.map(_.message).mkString("\n"))

  private val family = GaussianFamily.Default
  private val lags = Array.tabulate(120)(i => i * 0.2)
  private val points = Seq((3.5, math.log(0.9)), (5.0, math.log(1.5)), (7.5, math.log(2.8)))
    .map { case (t, v) => family.chart.point(t, v).fold(e => fail(e.message), identity) }

  test("Gaussian family jets match finite differences at interior points"):
    points.foreach { point =>
      val failures = ParametricFamilyLaws.jetsMatchFiniteDifferences(family, point, lags)
      assert(failures.isEmpty, failures.map(_.message).mkString("\n"))
    }

  test("Gaussian family scale jets match finite differences under every supported rule"):
    for rule <- NormalizationRule.values if family.supports(rule); point <- points do
      val failures = ParametricFamilyLaws.scaleJetsMatchFiniteDifferences(family, rule, point)
      assert(failures.isEmpty, failures.map(_.message).mkString("\n"))

  test("Gaussian family realisation matches the library kernel and is causal"):
    points.foreach { point =>
      val failures =
        ParametricFamilyLaws.realisationMatchesLibraryKernel(family, point, lags) ++
          ParametricFamilyLaws.causality(family, point)
      assert(failures.isEmpty, failures.map(_.message).mkString("\n"))
    }

  private val lwu = LwuFamily.Default
  private val lwuLags = Array.tabulate(160)(i => i * 0.2)
  private val lwuPoints = Seq((3.5, math.log(0.9), 0.1), (5.0, math.log(1.5), 0.4), (7.5, math.log(2.8), 0.75))
    .map { case (t, v, r) => lwu.chart.point(t, v, r).fold(e => fail(e.message), identity) }

  test("LWU family jets, scale jets, realisation and causality laws hold"):
    lwuPoints.foreach { point =>
      val failures =
        ParametricFamilyLaws.jetsMatchFiniteDifferences(lwu, point, lwuLags) ++
          ParametricFamilyLaws.scaleJetsMatchFiniteDifferences(lwu, NormalizationRule.Unnormalised, point) ++
          ParametricFamilyLaws.realisationMatchesLibraryKernel(lwu, point, lwuLags) ++
          ParametricFamilyLaws.causality(lwu, point)
      assert(failures.isEmpty, failures.map(_.message).mkString("\n"))
    }
