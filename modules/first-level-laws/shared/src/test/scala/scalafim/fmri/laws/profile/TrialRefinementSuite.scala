package scalafim.fmri.laws.profile

import scalafim.fmri.design.hrf.KernelBasisCompilation
import scalafim.fmri.fit.profile.*
import scalafim.fmri.model.ProfileCriterion

/** Reference agreement and actual public outputs; boundary solutions remain refusals. */
class TrialRefinementSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")
  private val budget = DecodeBudget(
    maxNewtonSteps = 16,
    maxJets = 901,
    maxExactEvaluations = 40,
    maxCandidateAttempts = 30,
    stationarityStepTolerance = 1e-6,
    initialization = DecodeInitialization.BoundedMultistart
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
  private lazy val outputs = f.prepare.flatMap(_.trialOutputs).fold(e => fail(e.message), identity)

  TrialRecoveryFixture.noiseRatios.foreach: ratio =>
    test(s"bounded public search agrees with all reference energies and refuses boundary fits at noise ratio $ratio"):
      val current = f.copy(rawBlock = ladder.response(ratio))
      val consumed = new DecodedTrialCheckpoint.Sink(f.trials)
      var expectedEmitted = 0L
      var searchJets = 0L
      val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
        def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
          payload.results.foreach: voxel =>
            val selected = voxel.selection
            val expected = TrialRecoveryOracle.cases.find(c => c.noiseRatio == ratio && c.voxel == voxel.voxelId).get
            val chart = f.plan.basis.family.chart
            val boundary = expected.coordinates.indices.exists(i =>
              math.abs(expected.coordinates(i) - chart.lower(i)) < 1e-7 * chart.width(i) ||
                math.abs(expected.coordinates(i) - chart.upper(i)) < 1e-7 * chart.width(i)
            )
            val scale = f.rows * math.pow(ladder.signalRms(voxel.voxelId), 2)
            assertEqualsDouble(selected.energy / scale, expected.energy / scale, 1e-9)
            if boundary then assert(selected.status != DecodeStatus.Accepted)
            else
              expectedEmitted += 1
              assertEquals(selected.status, DecodeStatus.Accepted, s"ratio=$ratio voxel=${voxel.voxelId}")
            if ratio <= 0.1 || (ratio == 1.0 && voxel.voxelId == 3) then
              expected.coordinates.indices.foreach: i =>
                assertEqualsDouble(
                  selected.coordinates(i) / chart.width(i),
                  expected.coordinates(i) / chart.width(i),
                  1e-5
                )
            val search = selected.search.get
            assertEquals(search.requestedStarts, 9)
            assertEquals(search.trajectories.length, 9)
            searchJets += search.trajectories.map(_.result.work.evaluations.toLong).sum
            assert(search.trajectories.forall(_.result.work.evaluations <= 90))
            voxel.output match
              case ProfileTrialOutputOutcome.Emitted(_, value)
                  if (ratio == 0.0 && voxel.voxelId == 0) || (ratio == 0.1 && voxel.voxelId == 10) =>
                val oracle = current.oracle(voxel.voxelId, selected.coordinates)
                (value.trialAmplitudes.get ++ value.nuisanceCoefficients)
                  .zip(oracle)
                  .foreach((a, b) => assertEqualsDouble(a, b, 1e-7))
              case _ => ()
          consumed.accept(block, payload)
      val result = outputs
        .run(new current.Reader, DecodedTrialCheckpoint.request, ProfileTrialReadoutMode.ExactShape, sink)
        .fold(e => fail(e.message), identity)
      assertEquals(consumed.attempted, 16L)
      assertEquals(consumed.emitted, expectedEmitted)
      assertEquals(consumed.outputValues, expectedEmitted * f.trials)
      assertEquals(result.progress.publicReadout.get.successes, expectedEmitted)
      assert(result.progress.decoder.jets > searchJets)
      assert(result.progress.decoder.jets <= 16L * budget.maxJets)
      assert(result.progress.decoder.exactEvaluations <= 16L * budget.maxExactEvaluations)
      assert(consumed.maxPreparedResidual < 1e-8)

  test("bounded search retains native ML terminal evidence and public readout coherence"):
    val tiny = DecodedTrialCheckpoint.fixture(
      DecodedTrialCheckpoint.Config(
        DecodedTrialCheckpoint.Geometry.Tiny,
        2,
        gridAxisNodes = 5,
        budget = budget,
        criterion = ProfileCriterion.TrialRandomEffectsML(1e-5)
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
    assert(result.progress.decoder.jets <= 2L * budget.maxJets)
