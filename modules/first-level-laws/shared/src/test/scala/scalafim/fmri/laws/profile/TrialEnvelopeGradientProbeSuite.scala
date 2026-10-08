package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.fmri.design.hrf.{KernelBasisCompilation, TrialDesignLowering}
import scalafim.fmri.fit.profile.*

/** Fixed-call numerical and work comparison. Timing is diagnostic output,
  * never a wall-clock CI assertion or a complete-workload qualification.
  */
class TrialEnvelopeGradientProbeSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  test("envelope gradient matches full jets with one value reference and three band solves"):
    val f = DecodedTrialCheckpoint.fixture(DecodedTrialCheckpoint.Config(
      DecodedTrialCheckpoint.Geometry.B0Dense, voxels = 1, trials = 300,
      compilation = KernelBasisCompilation.BlockedPartial(96),
      trialPreparation = TrialPreparationPolicy(TrialDesignLowering.Blocked(32))))
    val prep = TrialBandedPreparation.prepare(f.expanded, Some(f.whitening),
      Some(DMat.tabulate(f.rows, f.nuisance)((t, j) => f.baseline.designMatrix(t, j))), 1.0,
      f.config.trialPreparation.maxRetainedValues).fold(e => fail(e.message), identity)
    val grid = NodeGrid(f.plan.basis.family.chart, Vector(2, 2, 2))
    val bank = prep.objective(grid).fold(e => fail(e.message), identity)
    val response = prep.whitenResponses(1, f.rawBlock).flatMap(values => prep.encodeWhitened(values))
      .fold(e => fail(e.message), identity)
    val full = bank.newWorker()
    val envelope = bank.newWorker()
    full.pointAt(response)
    envelope.pointAt(response)
    val jet = new ProfileJetBuffer(3, 3)
    val gradient = new ProfileGradientBuffer(3, 3)
    val chart = f.plan.basis.family.chart
    val points = TrialQualificationAudit.shapes.map(unit =>
      unit.indices.map(i => chart.lower(i) + unit(i) * chart.width(i)).toArray)
    points.foreach: point =>
      assert(full.jetAt(point, jet))
      assert(envelope.gradientAt(point, gradient))
      assertEqualsDouble(gradient.energy, jet.energy, 1e-9)
      gradient.gradient.zip(jet.gradient).foreach((a, b) => assertEqualsDouble(a, b, 1e-9))
    def batch(first: Boolean): (Double, Double) =
      val start = System.nanoTime()
      var checksum = 0.0
      var i = 0
      while i < 64 do
        if first then
          assert(envelope.gradientAt(points(i % points.length), gradient))
          checksum += gradient.energy + gradient.gradient.sum
        else
          assert(full.jetAt(points(i % points.length), jet))
          checksum += jet.energy + jet.gradient.sum
        i += 1
      ((System.nanoTime() - start) / 1e9, checksum)
    batch(false)
    batch(true)
    val beforeFull = full.work.snapshot.attempted
    val beforeEnvelope = envelope.work.snapshot.attempted
    val timings = Vector.tabulate(5): i =>
      val (a, b) = if i % 2 == 0 then (batch(false), batch(true))
        else
          val first = batch(true)
          (batch(false), first)
      assertEqualsDouble(a._2, b._2, 1e-7)
      s"{\"fullSeconds\":${a._1},\"envelopeSeconds\":${b._1},\"fullChecksum\":${a._2},\"envelopeChecksum\":${b._2}}"
    val fullWork = full.work.snapshot.attempted
    val envelopeWork = envelope.work.snapshot.attempted
    assertEquals(fullWork.solveAttempts - beforeFull.solveAttempts, 320L * 21L)
    assertEquals(envelopeWork.solveAttempts - beforeEnvelope.solveAttempts, 320L * 3L)
    assertEquals(envelopeWork.rightHandSideAttempts - beforeEnvelope.rightHandSideAttempts, 320L * 13L)
    println(s"PHRF_ENVELOPE_PROBE={\"callsPerBatch\":64,\"warmupBatchesPerMode\":1,\"repetitions\":[${timings.mkString(",")}]," +
      s"\"searchReferenceBytes\":${bank.estimatedValueReferenceBytes},\"previousFirstOrderReferenceBytes\":${bank.estimatedFirstOrderReferenceBytes}," +
      s"\"fullReferenceBytes\":${bank.estimatedReferenceBytes},\"qualification\":\"not-admitted\"}")
