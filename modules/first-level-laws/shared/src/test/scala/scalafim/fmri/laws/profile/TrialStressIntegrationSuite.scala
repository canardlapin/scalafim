package scalafim.fmri.laws.profile

import scalafim.fmri.design.hrf.{KernelBasisCompilation, TrialDesignLowering}
import scalafim.fmri.fit.profile.*
import scalafim.scenarios.*

class TrialStressIntegrationSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  test("1200-trial blocked preparation and repaired decoder emit a low-noise public readout"):
    val f = DecodedTrialCheckpoint.fixture(
      DecodedTrialCheckpoint.Config(
        DecodedTrialCheckpoint.Geometry.B0Dense,
        voxels = 1,
        trials = 1200,
        compilation = KernelBasisCompilation.BlockedPartial(96),
        budget = DecodedTrialCheckpoint.repairedBudget,
        trialPreparation = TrialPreparationPolicy(TrialDesignLowering.Blocked(32)),
        noiseRatio = Some(0.1)
      )
    )
    val prepared = f.prepare.fold(e => fail(e.message), identity)
    val outputs = prepared.trialOutputs.fold(e => fail(e.message), identity)
    val sink = new DecodedTrialCheckpoint.Sink(f.trials)
    val run = outputs
      .run(new f.Reader, DecodedTrialCheckpoint.request, ProfileTrialReadoutMode.ExactShape, sink)
      .fold(e => fail(e.message), identity)
    val result = ScenarioResult(
      "phrf-blocked-stress-prepared-basis-integration",
      Vector(
        ScenarioHarness
          .fact("one attempted and emitted", sink.attempted == 1L && sink.emitted == 1L, sink.statuses.toString),
        ScenarioHarness.fact("complete trial output", sink.outputValues == 1200L, s"values=${sink.outputValues}"),
        ScenarioHarness
          .scalar("prepared equation residual", sink.maxPreparedResidual, 0.0, ScenarioTolerance.absolute(1e-8)),
        ScenarioHarness.fact(
          "bounded repaired search",
          run.progress.decoder.firstOrderAttempts > 0L &&
            run.progress.decoder.jets <= DecodedTrialCheckpoint.repairedBudget.maxJets,
          run.progress.decoder.toString
        ),
        ScenarioHarness.fact(
          "no retained dense trial source",
          prepared.setup.expandedTrialLoweringDoubles.contains(0L),
          prepared.setup.expandedTrialLoweringDoubles.toString
        )
      )
    )
    assert(result.ciPass, result.render)
