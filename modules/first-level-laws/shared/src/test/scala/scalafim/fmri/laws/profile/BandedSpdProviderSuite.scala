package scalafim.fmri.laws.profile

import gale.linalg.{BandedCholesky, DMat, DMatBuilder, ExactSolveFactor}

/** Provider admission for trial-sized ridge Gram systems. This qualifies the
  * Gale seam; the TrialBanded estimator and its scientific gates are PHRF-07.
  */
class BandedSpdProviderSuite extends munit.FunSuite:
  test("overlapping trial Gram solves, energy and determinant match dense factors"):
    for trials <- Vector(12, 40); spacing <- Vector(2, 5); lambda <- Vector(1e-6, 0.3, 1e3) do
      val support = 13
      val rows = trials * spacing + support
      val design = DMat.tabulate(rows, trials): (t, j) =>
        val lag = t - j * spacing
        if lag >= 0 && lag < support then math.sin(math.Pi * (lag + 0.5) / support) else 0.0
      val gram = design.t * design
      val system = DMat.tabulate(trials, trials)((i, j) => gram(i, j) + (if i == j then lambda else 0.0))
      val bandwidth = math.min(trials - 1, (support - 1) / spacing)
      val bands = DMatBuilder.zeros(trials, bandwidth + 1)
      for i <- 0 until trials; d <- 0 to math.min(i, bandwidth) do bands(i, d) = system(i, i - d)
      val factor = bands.consumeBandedCholesky().fold(throw _, identity)
      val capability: ExactSolveFactor = factor
      val dense = system.cholesky.fold(throw _, identity)
      val y = DMat.tabulate(rows, 3)((t, c) => math.sin(t / (3.0 + c)) + math.cos(t * 0.17 + c))
      val scores = design.t * y
      val expected = dense.solve(scores).fold(throw _, identity)
      val actual = capability.solve(scores).fold(throw _, identity)
      for i <- 0 until trials; j <- 0 until 3 do
        assertEqualsDouble(actual(i, j), expected(i, j), 2e-8)
      val residual = system * actual - scores
      for i <- 0 until trials; j <- 0 until 3 do assertEqualsDouble(residual(i, j), 0.0, 3e-11)
      val mutable = DMatBuilder.from(scores)
      assert(factor.solveInPlace(mutable).isRight)
      val inPlace = mutable.result()
      for i <- 0 until trials; j <- 0 until 3 do assertEqualsDouble(inPlace(i, j), actual(i, j), 1e-12)
      val denseLogDet = 2.0 * (0 until trials).map(i => math.log(dense.lower(i, i))).sum
      assertEqualsDouble(factor.logDet, denseLogDet, 2e-9)
      for c <- 0 until 3 do
        val expectedEnergy = (0 until rows).map(t => y(t, c) * y(t, c)).sum -
          (0 until trials).map(i => scores(i, c) * expected(i, c)).sum
        val actualEnergy = (0 until rows).map(t => y(t, c) * y(t, c)).sum -
          (0 until trials).map(i => scores(i, c) * actual(i, c)).sum
        assertEqualsDouble(actualEnergy, expectedEnergy, 2e-9)
      assert(factor.conditioning.conditionNumberUpperBound >= 1.0)
      assertEquals(factor.bandwidth, bandwidth)

  test("provider preserves a pure packed input for reuse across references"):
    val bands = DMat.tabulate(7, 2)((_, d) => if d == 0 then 2.0 else -1.0)
    val first = BandedCholesky.factorLower(bands).fold(throw _, identity)
    val second = BandedCholesky.factorLower(bands).fold(throw _, identity)
    assertEqualsDouble(first.logDet, math.log(8.0), 2e-14)
    assertEqualsDouble(second.logDet, first.logDet, 0.0)
    assertEqualsDouble(bands(3, 0), 2.0, 0.0)
