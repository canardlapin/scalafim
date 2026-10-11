package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.fmri.design.hrf.KernelBasisCompilation
import scalafim.fmri.fit.profile.*

/** Same-model recovery controls; the noisy B0 scientific and performance gates remain open. */
class TrialRecoverySuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")
  private val budget = DecodeBudget(
    maxNewtonSteps = 16,
    maxJets = 20,
    maxExactEvaluations = 40,
    maxCandidateAttempts = 12,
    stationarityStepTolerance = 1e-6,
    initialization = DecodeInitialization.ChartCenterProbe
  )
  private lazy val f = DecodedTrialCheckpoint.fixture(
    DecodedTrialCheckpoint.Config(
      DecodedTrialCheckpoint.Geometry.B0Dense,
      16,
      compilation = KernelBasisCompilation.BlockedPartial(96),
      budget = budget
    )
  )
  private lazy val ladder = TrialRecoveryFixture(f)

  test("the matched noise ladder reproduces the frozen B0 responses"):
    ladder.response(2.0).zip(f.rawBlock).foreach((a, b) => assertEqualsDouble(a, b, 1e-14))
    assertEquals(f.plan.basis.rank, 10)
    assertEquals(f.trials, 300)
    assertEquals(f.rows, 600)

  test("banded energy agrees with independent dense/QR reference solutions across all 80 noise-ladder cells"):
    val nuisance = DMat.tabulate(f.rows, f.nuisance)((t, j) => f.baseline.designMatrix(t, j))
    val prep = TrialBandedPreparation
      .prepare(f.expanded, Some(f.whitening), Some(nuisance), 1.0)
      .fold(e => fail(e.message), identity)
    val objective =
      prep.objective(NodeGrid(f.plan.basis.family.chart, Vector(2, 2, 2))).fold(e => fail(e.message), identity)
    TrialRecoveryFixture.noiseRatios.foreach: ratio =>
      val responses = ladder.whiten(ladder.response(ratio))
      TrialRecoveryOracle.cases
        .filter(_.noiseRatio == ratio)
        .foreach: expected =>
          objective.pointAt(
            prep
              .encodeWhitened(Array.tabulate(f.rows)(t => responses(t, expected.voxel)))
              .fold(e => fail(e.message), identity)
          )
          val jet = new ProfileJetBuffer(3, 3)
          val energy = objective.energyAt(expected.coordinates.toArray, jet)
          val scale = f.rows * math.pow(ladder.signalRms(expected.voxel), 2)
          assertEqualsDouble(energy / scale, expected.energy / scale, 1e-9)

  test("center initialization recovers every noiseless voxel against the independent reference"):
    val prepared = f.prepare.fold(e => fail(e.message), identity)
    val outputs = prepared.trialOutputs.fold(e => fail(e.message), identity)
    Vector(0.0).foreach: ratio =>
      val current = f.copy(rawBlock = ladder.response(ratio))
      val consumed = new DecodedTrialCheckpoint.Sink(f.trials)
      val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
        def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
          payload.results.foreach: voxel =>
            val expected = TrialRecoveryOracle.cases.find(c => c.noiseRatio == ratio && c.voxel == voxel.voxelId).get
            assertEquals(voxel.selection.status, DecodeStatus.Accepted, s"ratio=$ratio voxel=${voxel.voxelId}")
            expected.coordinates.indices.foreach: axis =>
              val width = f.plan.basis.family.chart.width(axis)
              assertEqualsDouble(voxel.selection.coordinates(axis) / width, expected.coordinates(axis) / width, 1e-5)
            val scale = f.rows * math.pow(ladder.signalRms(voxel.voxelId), 2)
            assertEqualsDouble(voxel.selection.energy / scale, expected.energy / scale, 1e-9)
          consumed.accept(block, payload)
      val summary = outputs
        .run(new current.Reader, DecodedTrialCheckpoint.request, ProfileTrialReadoutMode.ExactShape, sink)
        .fold(e => fail(e.message), identity)
      assertEquals(consumed.attempted, 16L)
      assertEquals(consumed.emitted, 16L)
      assertEquals(consumed.offNode, 16L)
      assert(summary.progress.decoder.jets <= 16L * budget.maxJets)
      assertEquals(summary.progress.publicReadout.get.successes, 16L)

  test("center initialization preserves the native ML public readout contract"):
    val tiny = DecodedTrialCheckpoint.fixture(
      DecodedTrialCheckpoint.Config(
        DecodedTrialCheckpoint.Geometry.Tiny,
        2,
        gridAxisNodes = 5,
        budget = budget,
        criterion = scalafim.fmri.model.ProfileCriterion.TrialRandomEffectsML(1e-5)
      )
    )
    val outputs = tiny.prepare.flatMap(_.trialOutputs).fold(e => fail(e.message), identity)
    val sink = new DecodedTrialCheckpoint.Sink(tiny.trials)
    val result = outputs
      .run(new tiny.Reader, DecodedTrialCheckpoint.request, ProfileTrialReadoutMode.ExactShape, sink)
      .fold(e => fail(e.message), identity)
    assertEquals(sink.attempted, 2L)
    assert(sink.emitted > 0)
    assertEquals(result.progress.publicReadout.get.successes, sink.emitted)
    assert(result.progress.trialMl.get.nFactorAttempts > 0)
