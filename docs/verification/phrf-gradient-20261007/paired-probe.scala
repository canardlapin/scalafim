package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.fmri.design.hrf.KernelBasisCompilation
import scalafim.fmri.fit.profile.*

/** Fixed-work diagnostic, archived outside the shipping test suite after capture. */
class TrialOracleProfileSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")
  test("fixed B0 oracle calls"):
    val f = DecodedTrialCheckpoint.fixture(DecodedTrialCheckpoint.Config(
      DecodedTrialCheckpoint.Geometry.B0Dense, 16, compilation = KernelBasisCompilation.BlockedPartial(96)))
    val ladder = TrialRecoveryFixture(f)
    val nuisance = DMat.tabulate(f.rows, f.nuisance)((t, j) => f.baseline.designMatrix(t, j))
    val prep = TrialBandedPreparation.prepare(f.expanded, Some(f.whitening), Some(nuisance), 1.0)
      .fold(e => fail(e.message), identity)
    val objective = prep.objective(NodeGrid(f.plan.basis.family.chart, Vector(2, 2, 2)))
      .fold(e => fail(e.message), identity)
    val response = ladder.whiten(ladder.response(0.1))
    objective.pointAt(prep.encodeWhitened(Array.tabulate(f.rows)(t => response(t, 10)))
      .fold(e => fail(e.message), identity))
    val chart = f.plan.basis.family.chart
    val points = Vector.tabulate(16)(i => Array.tabulate(3)(a =>
      chart.lower(a) + chart.width(a) * (0.15 + 0.7 * ((i * (2 * a + 1) + a) % 16) / 15.0)))
    val out = new ProfileJetBuffer(3, 3)
    val first = new ProfileGradientBuffer(3, 3)
    points.foreach: point =>
      assert(objective.jetAt(point, out))
      assert(objective.gradientAt(point, first))
      assertEqualsDouble(first.energy, out.energy, 0.0)
      first.gradient.zip(out.gradient).foreach((a, b) => assertEqualsDouble(a, b, 0.0))
    def run(calls: Int, gradient: Boolean): Double =
      var checksum = 0.0
      var i = 0
      while i < calls do
        val value = if gradient then first else out
        assert(if gradient then objective.gradientAt(points(i % points.length), first)
          else objective.jetAt(points(i % points.length), out))
        checksum += value.energy + value.gradient.sum
        i += 1
      checksum
    run(64, false)
    run(64, true)
    for repeat <- 0 until 5 do
      val order = if repeat % 2 == 0 then Vector(false, true) else Vector(true, false)
      order.foreach: gradient =>
        val before = objective.work.snapshot.attempted
        val start = System.nanoTime()
        val checksum = run(256, gradient)
        val seconds = (System.nanoTime() - start) / 1e9
        val after = objective.work.snapshot.attempted
        val calls = if gradient then after.firstOrderAttempts - before.firstOrderAttempts else after.jetAttempts - before.jetAttempts
        println(s"PHRF_ORACLE_PAIR gradient=$gradient repeat=$repeat seconds=$seconds calls=$calls solves=${after.solveAttempts - before.solveAttempts} rhs=${after.rightHandSideAttempts - before.rightHandSideAttempts} checksum=$checksum")
