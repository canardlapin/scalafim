package scalafim.fmri.mvpa.inference

import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.inference.*
import resample4s.kernel.{Permutation, Seed}
import scalafim.fmri.mvpa.analysis.CalibrationBindings

/** External base-R fixtures test the actual upstream kernels used by rank confirmation. */
class RankIndependentOracleSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(e => fail(e.toString), identity)
  private def close(actual: Double, expected: Double): Unit =
    assertEqualsDouble(actual, expected, 1e-10 * (1.0 + math.abs(expected)))

  RankScoreOracleFixtures.cases.foreach: fixture =>
    test(s"independent R observed roots and every tail statistic: ${fixture.name}"):
      val problem = right(StepwiseCanonicalRank.from(fixture.x, fixture.y))
      assertEquals(problem.candidateRank, fixture.roots.size)
      problem.correlations.zip(fixture.roots).foreach((actual, expected) => close(actual, expected))
      problem.observedWilks.zip(fixture.observed).foreach((actual, expected) => close(actual, expected))
      fixture.actions.zip(fixture.statistics).foreach: (action, expected) =>
        val statistics = right(problem.nullStatistics(right(Permutation.from(IArray.from(action)))))
        assertEquals(statistics.size, expected.size)
        statistics.zip(expected).foreach((actual, reference) => close(actual, reference))

    if fixture.exact then
      test("complete finite group matches independent exceedances, plus-one closure, and detectable rank"):
        val problem = right(StepwiseCanonicalRank.from(fixture.x, fixture.y))
        val draws = fixture.actions.size - 1
        val result = right(FixedCanonicalRank.run(problem,
          PermutationAction.unrestricted(right(RowCount(fixture.x.rows))), Seed.fromLong(827461L),
          right(MonteCarloDraws(draws)), right(Alpha(.05)),
          sampling = CanonicalRankSampling.DistinctNonIdentity(20000)))
        assertEquals(result.receipts.map(_.exceedances), fixture.nonIdentityExceedances)
        assertEquals(result.completedCompactFits, draws.toLong * problem.candidateRank)
        val raw = fixture.nonIdentityExceedances.map(n => (n + 1.0) / fixture.actions.size)
        val closed = raw.scanLeft(0.0)(math.max).tail
        result.receipts.map(_.pValue.value).zip(raw).foreach((actual, expected) => close(actual, expected))
        result.adjustedPValues.map(_.value).zip(closed).foreach((actual, expected) => close(actual, expected))
        assertEquals(result.detectableRank, closed.takeWhile(_ <= .05).size)

  test("Huh-Jhun coordinates reproduce the independent nuisance residual projector"):
    val basis = right(CanonicalResidualBasis.from(RankScoreOracleFixtures.nuisance))
    assertEquals(basis.nuisanceRank, 3)
    val projector = basis.matrix * basis.matrix.t
    val reference = RankScoreOracleFixtures.residualProjector
    for i <- 0 until reference.rows; j <- 0 until reference.cols do
      close(projector(i, j), reference(i, j))
    val x = RankScoreOracleFixtures.cases.head.x
    val y = RankScoreOracleFixtures.cases.head.y
    val projected = right(StepwiseCanonicalRank.from(right(basis.project(x)), right(basis.project(y))))
    val residualized = right(StepwiseCanonicalRank.from(reference * x, reference * y))
    projected.correlations.zip(residualized.correlations).foreach((actual, expected) => close(actual, expected))
    projected.correlations.zip(RankScoreOracleFixtures.nuisanceRoots).foreach((actual, expected) => close(actual, expected))

  test("production rank binding matches independent nuisance-adjusted roots"):
    val fixture = RankScoreOracleFixtures.cases.head
    val result = right(CalibrationBindings.rank(fixture.x, fixture.y, RankScoreOracleFixtures.nuisance,
      924781L, 19, "independent-rank-oracle"))
    assertEquals(result.nuisanceRank, 3)
    assertEquals(result.residualRows, 6)
    result.candidateArithmetic.correlations.zip(RankScoreOracleFixtures.nuisanceRoots)
      .foreach((actual, expected) => close(actual, expected))
