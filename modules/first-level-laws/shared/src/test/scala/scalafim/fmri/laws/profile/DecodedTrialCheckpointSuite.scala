package scalafim.fmri.laws.profile

import scalafim.fmri.fit.profile.*
import scalafim.fmri.model.ProfileCriterion

class DecodedTrialCheckpointSuite extends munit.FunSuite:
  import DecodedTrialCheckpoint.*

  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  // Control-only budget. B0 uses DecodeBudget() unless explicitly marked as an expanded diagnostic.
  private val control = DecodeBudget(
    maxNewtonSteps = 16,
    maxJets = 20,
    maxExactEvaluations = 40,
    maxCandidateAttempts = 12,
    stationarityStepTolerance = 1e-8
  )
  private lazy val tiny = fixture(Config(Geometry.Tiny, voxels = 4, blockSize = 2, gridAxisNodes = 5, budget = control))

  test("public decoded readout agrees with independent augmented QR at its returned shape"):
    val prepared = tiny.prepare.fold(e => fail(e.message), identity)
    val outputs = prepared.trialOutputs.fold(e => fail(e.message), identity)
    val reader = new tiny.Reader
    val consumed = new Sink(tiny.trials)
    var checked = 0
    val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
        payload.results.foreach: voxel =>
          voxel.output match
            case ProfileTrialOutputOutcome.DecodeRefused(_)      => ()
            case ProfileTrialOutputOutcome.ReadoutRefused(error) => fail(error.message)
            case ProfileTrialOutputOutcome.Emitted(_, value)     =>
              val oracle = tiny.oracle(voxel.voxelId, voxel.selection.coordinates)
              val actual = value.trialAmplitudes.get ++ value.nuisanceCoefficients
              actual.zip(oracle).foreach((a, b) => assertEqualsDouble(a, b, 1e-7))
              checked += 1
        consumed.accept(block, payload)
    val summary =
      outputs.run(reader, request, ProfileTrialReadoutMode.ExactShape, sink).fold(e => fail(e.message), identity)
    assert(checked > 0, s"no outputs checked: ${consumed.statuses}")
    assert(consumed.offNode > 0, "fixture must exercise continuous returned coordinates")
    assertEquals(consumed.attempted, 4L)
    assertEquals(consumed.emitted, checked.toLong)
    assertEquals(consumed.outputValues, checked.toLong * tiny.trials)
    assertEquals(reader.values, 4L * tiny.rows)
    assert(reader.largestSeriesValues <= 2L * tiny.rows)
    assertEquals(summary.progress.deliveredVoxels, 4)
    assert(summary.progress.decoder.jets > 0)
    assert(summary.progress.trial.get.continuousFactors > 0)
    assertEquals(summary.progress.publicReadout.get.successes, checked.toLong)
    assertEquals(consumed.retainedOutputBytes, 4L * tiny.trials)

  test("unavailable original-family evidence refuses before any dataset read or sink delivery"):
    val prepared = tiny.prepare.fold(e => fail(e.message), identity)
    val outputs = prepared.trialOutputs.fold(e => fail(e.message), identity)
    val reader = new tiny.Reader
    val sink = new Sink(tiny.trials)
    val result = outputs.run(
      reader,
      request,
      ProfileTrialReadoutMode.ExactShape,
      sink,
      evidence = ProfileTrialEvidenceRequest.CertifiedOriginalEquations
    )
    assertEquals(
      result.left.toOption,
      Some(ProfileFitError.TrialOutputAdmission(ProfileTrialReadoutError.CertificateUnavailable))
    )
    assertEquals(reader.calls, 0L)
    assertEquals(sink.attempted, 0L)

  test("exhausted decode budgets remain refused attempts and do not create fast successful outputs"):
    val f = fixture(
      Config(
        Geometry.Tiny,
        voxels = 3,
        blockSize = 1,
        budget = DecodeBudget(maxNewtonSteps = 0, maxJets = 1, maxExactEvaluations = 0)
      )
    )
    val outputs = f.prepare.flatMap(_.trialOutputs).fold(e => fail(e.message), identity)
    val sink = new Sink(f.trials)
    val summary =
      outputs.run(new f.Reader, request, ProfileTrialReadoutMode.ExactShape, sink).fold(e => fail(e.message), identity)
    assertEquals(sink.attempted, 3L)
    assertEquals(sink.emitted, 0L)
    assertEquals(sink.outputValues, 0L)
    assertEquals(summary.progress.publicReadout.get.decodeRefusals, 3L)
    assertEquals(sink.statuses.values.sum, 3L)

  test("native ML public output reports coherent returned-shape coefficients and charged work"):
    val f = fixture(
      Config(
        Geometry.Tiny,
        voxels = 2,
        blockSize = 1,
        gridAxisNodes = 5,
        budget = control,
        criterion = ProfileCriterion.TrialRandomEffectsML(1e-5)
      )
    )
    val prepared = f.prepare.fold(e => fail(e.message), identity)
    val outputs = prepared.trialOutputs.fold(e => fail(e.message), identity)
    val sink = new Sink(f.trials)
    val summary =
      outputs.run(new f.Reader, request, ProfileTrialReadoutMode.ExactShape, sink).fold(e => fail(e.message), identity)
    assertEquals(sink.attempted, 2L)
    assert(sink.emitted > 0, s"no accepted ML readout: ${sink.statuses}")
    assert(summary.progress.trialMl.get.nFactorAttempts > 0)
    assert(summary.progress.trialMl.get.residualEnergyEvaluations > 0)
    assert(summary.setup.mlSetup.get.logDetRecursionAttempts > 0)
    assertEquals(summary.progress.publicReadout.get.successes, sink.emitted)
    assert(outputs.executionDeclaration(request, ProfileTrialReadoutMode.CorrectedReference).isLeft)

  test("blocked diagnostic fixtures retain the same inputs, decoded statuses and Float32 outputs"):
    import scalafim.fmri.design.hrf.TrialDesignLowering
    val blocked = fixture(tiny.config.copy(trialPreparation = TrialPreparationPolicy(TrialDesignLowering.Blocked(5))))
    assertEquals(blocked.rawBlock.length, tiny.rawBlock.length)
    blocked.rawBlock.zip(tiny.rawBlock).foreach((a, b) => assertEqualsDouble(a, b, 1e-14))
    def run(f: DecodedTrialCheckpoint.Fixture): Sink =
      val prepared = f.prepare.fold(e => fail(e.message), identity)
      val outputs = prepared.trialOutputs.fold(e => fail(e.message), identity)
      val sink = new Sink(f.trials)
      outputs.run(new f.Reader, request, ProfileTrialReadoutMode.ExactShape, sink).fold(e => fail(e.message), identity)
      sink
    val actual = run(blocked)
    val expected = run(tiny)
    assertEquals(actual.statuses.toMap, expected.statuses.toMap)
    assertEquals(actual.emitted, expected.emitted)
    assert(actual.emitted > 0L)
    assertEquals(actual.outputValues, expected.outputValues)
    assertEqualsDouble(actual.outputChecksum, expected.outputChecksum, 1e-12)
