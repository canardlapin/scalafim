package scalafim.multivar

import gale.linalg.{DMat, Matrix}

class CanonicalSpectrumSuite extends munit.FunSuite:

  test("full canonical spectrum exposes the four named MANOVA estimands"):
    val fit = problem(diagonal(4.0, 1.0, 0.0), DMat.eye(3)).fitSpectrum(2).toOption.get

    assertEqualsDouble(fit.roots.values(0).value, 4.0, 1e-10)
    assertEqualsDouble(fit.roots.values(1).value, 1.0, 1e-10)
    assertEqualsDouble(fit.statistics.royLargestRoot, 4.0, 1e-10)
    assertEqualsDouble(fit.statistics.wilksLambda, 0.1, 1e-10)
    assertEqualsDouble(fit.statistics.pillaiTrace, 1.3, 1e-10)
    assertEqualsDouble(fit.statistics.hotellingLawleyTrace, 5.0, 1e-10)
    assertEqualsDouble(fit.programFit.objectiveValue, 5.0, 1e-10)
    assertEquals(fit.programFit.program.objective.label, "maximize-trace")

  test("repeated roots identify a projector-valued subspace rather than individual axes"):
    val fit = problem(diagonal(2.0, 2.0, 0.0), DMat.eye(3)).fitSpectrum(2).toOption.get
    val cluster = fit.diagnostics.clusters.head

    assertEquals(fit.diagnostics.clusters.length, 1)
    assertEquals(cluster.multiplicity, 2)
    assertEqualsDouble(cluster.projector(0, 0), 1.0, 1e-10)
    assertEqualsDouble(cluster.projector(1, 1), 1.0, 1e-10)
    assertEqualsDouble(cluster.projector(2, 2), 0.0, 1e-10)
    assertEqualsDouble(cluster.projector(0, 1), 0.0, 1e-10)

  test("spectrum rank is checked at construction"):
    val candidate = problem(diagonal(1.0, 0.0), DMat.eye(2))

    assertEquals(candidate.fitSpectrum(0).left.toOption, Some(MultivarError.InvalidComponentRequest(0, 2)))
    assertEquals(candidate.fitSpectrum(3).left.toOption, Some(MultivarError.InvalidComponentRequest(3, 2)))

  private def problem(effect: DMat, residual: DMat): CanonicalEffectProblem[? <: SemanticSpace] =
    val space = SpaceRef.of("canonical-spectrum-test", SpaceRole.Observed, effect.rows).toOption.get
    CanonicalEffectProblem
      .fromDense(space.evidence, effect, residual, ResidualRegularization.Unregularized)
      .toOption
      .get

  private def diagonal(values: Double*): DMat =
    val out = Matrix.newBuilder(values.length, values.length)
    var index = 0
    while index < values.length do
      out(index, index) = values(index)
      index += 1
    out.result()
