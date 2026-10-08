package scalafim.fmri.fit.profile

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.hrf.{HrfKernelBasis, KernelBasisSpec, TrialBasisDesign, TrialDesignLowering, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.GaussianFamily

class TrialBandedPreparationSuite extends munit.FunSuite:
  private lazy val basis = HrfKernelBasis.compile(KernelBasisSpec(GaussianFamily.Default,
    PositiveSeconds.unsafe(Seconds(0.2)), Vector(26, 21), tolerance = 1e-4, maxRank = 40))
    .fold(e => fail(e.message), identity)
  private val frame = SamplingFrame(blockLens = Seq(60, 40), tr = Seq(1.0, 1.3))
  private val onsets = Vector(3.3, 1.3, 31.1, 3.3, 14.7, 22.0, 40.1).map(Seconds(_))
  private val runs = Vector(1, 0, 1, 0, 1, 0, 0)
  private val durations = Vector(0.0, 0.3, 0.7, 0.2, 0.0, 0.4, 0.1).map(Seconds(_))
  private val members = TrialMembership.make(Vector(0, 1, 2, 0, 1, 2, 2), 3).fold(e => fail(e.message), identity)
  private val nuisance = Some(DMat.tabulate(100, 2)((t, j) => if j == 0 then 1.0 else t / 100.0))
  private val segments = Vector(TimeSegment(0, 60, 0), TimeSegment(60, 100, 1))

  private def source(mode: TrialDesignLowering, xs: Vector[Seconds] = onsets): TrialBasisDesign =
    TrialBasisDesign.lower(xs, runs, durations, members, frame, basis, Seconds(0.2), mode)
      .fold(e => fail(e.message), identity)

  private def sameValues(actual: Array[Double], expected: Array[Double]): Unit =
    assertEquals(actual.length, expected.length)
    actual.indices.foreach(i => assertEqualsDouble(actual(i), expected(i), 1e-13))

  test("packed geometry and off-node objective agree with dense lowering for IID, AR and MA whitening"):
    val whitenings = Vector(None,
      Some(WhiteningPlan.byRun(Vector(ArmaCoefficients.ar(0.31), ArmaCoefficients.ar(-0.17)), segments)),
      Some(WhiteningPlan.global(ArmaCoefficients(Vector(0.31), Vector(0.2)), segments)))
    whitenings.foreach: whitening =>
      val dense = TrialBandedPreparation.prepare(source(TrialDesignLowering.Dense), whitening, nuisance, 2.5)
        .fold(e => fail(e.message), identity)
      Vector(1, 3, 32).foreach: size =>
        val blocked = TrialBandedPreparation.prepare(source(TrialDesignLowering.Blocked(size)), whitening, nuisance, 2.5)
          .fold(e => fail(e.message), identity)
        assertEquals(blocked.bandwidth, dense.bandwidth)
        assertEquals(blocked.starts.toVector, dense.starts.toVector)
        assertEquals(blocked.ends.toVector, dense.ends.toVector)
        assertEquals(blocked.trialOffsets.toVector, dense.trialOffsets.toVector)
        sameValues(blocked.sparseDesign, dense.sparseDesign)
        sameValues(blocked.gramBlocksData, dense.gramBlocksData)
        sameValues(blocked.basisNuisanceCross, dense.basisNuisanceCross)
        sameValues(blocked.nuisanceGram, dense.nuisanceGram)
        assertEquals(blocked.retainedSourceDesignDataValues, 0L)
        assertEquals(blocked.receipt.retainedDoubles, dense.receipt.retainedDoubles)
        assert(blocked.receipt.maxLoweredBlockValues <= 100L * math.min(size, members.trials) * basis.rank)
        val response = Array.tabulate(100)(t => math.sin(0.13 * t) + 0.1 * math.cos(0.7 * t))
        def evaluate(prep: TrialBandedPreparation): ProfileJetBuffer =
          val bank = prep.objective(NodeGrid(basis.family.chart, Vector(2, 2))).fold(e => fail(e.message), identity)
          val y = prep.whitenResponses(1, response).fold(e => fail(e.message), identity)
          bank.pointAt(prep.encodeWhitened(y).fold(e => fail(e.message), identity))
          val out = new ProfileJetBuffer(2, 3)
          assert(bank.jetAt(Array(5.3, math.log(1.55)), out))
          out
        val expected = evaluate(dense)
        val actual = evaluate(blocked)
        assertEqualsDouble(actual.energy, expected.energy, 1e-12)
        sameValues(actual.gradient, expected.gradient)
        sameValues(actual.hessian, expected.hessian)

  test("storage refusal covers packed values and overlap Gram storage before their allocations"):
    val design = source(TrialDesignLowering.Blocked(2))
    val full = TrialBandedPreparation.prepare(design, None, nuisance, 2.5).fold(e => fail(e.message), identity)
    val cap = full.receipt.retainedDoubles - 1L
    assertEquals(TrialBandedPreparation.prepare(design, None, nuisance, 2.5, cap),
      Left(TrialBandedError.StorageLimit(full.receipt.retainedDoubles, cap)))
    val tight = TrialBandedPreparation.prepare(design, None, None, 2.5, 3L * members.trials + 1L)
    assert(tight.left.exists(_.isInstanceOf[TrialBandedError.StorageLimit]))
    intercept[IllegalArgumentException](TrialPreparationPolicy(maxRetainedValues = 0L))

  test("an unobserved trial in a later block preserves caller trial identity"):
    val design = source(TrialDesignLowering.Blocked(2), onsets.updated(6, Seconds(1000.0)))
    assertEquals(TrialBandedPreparation.prepare(design, None, None, 2.5), Left(TrialBandedError.UnobservedTrial(6)))
