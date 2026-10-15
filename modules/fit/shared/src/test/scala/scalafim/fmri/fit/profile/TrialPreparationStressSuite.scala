package scalafim.fmri.fit.profile

import scalafim.dataset.*
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.{ConditionId, TrialId}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept, NuisanceCheck}
import scalafim.fmri.design.event.EventSchedule
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisCompilation, KernelBasisSpec, TrialDesignLowering, TrialMembership}
import scalafim.fmri.fit.CanonicalTemporalWhitening
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.Cascade34Family
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, ProfileHrfPlan, ProfileTrialDrive}
import scalafim.image.SampleSpaces

/** Frozen stress geometry only: successful preparation does not admit decoding,
  * original-family certification, throughput or total engine peak memory.
  */
class TrialPreparationStressSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  test("N=1200 public preparation crosses the dense convolution cap using bounded blocks"):
    val rows = 600
    val trials = 1200
    val family = Cascade34Family.Default
    val started = System.nanoTime()
    val basis = HrfKernelBasis.compile(KernelBasisSpec(family, PositiveSeconds.unsafe(Seconds(0.1)),
      Vector(9, 9, 7), tolerance = 1e-3, maxRank = 32, compilation = KernelBasisCompilation.BlockedPartial(96)))
      .fold(e => fail(e.message), identity)
    val basisSeconds = (System.nanoTime() - started) / 1e9
    assertEquals(basis.rank, 10)
    val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(1.0))
    val rng = new scala.util.Random(20260910L)
    val lastOnset = rows - family.horizon.value - 1.0
    val onsets = Vector.fill(trials)(math.floor(rng.nextDouble() * lastOnset * 10.0) / 10.0).sorted.map(Seconds(_))
    val membership = TrialMembership.make(Vector.tabulate(trials)(_ % 3), 3).fold(e => fail(e.message), identity)
    val schedule = EventSchedule.fromParts(onsets, Vector.fill(trials)(Seconds(0.0)), Vector.fill(trials)(0))
      .fold(e => fail(e.message), identity)
    val dense = ExpandedTrialDesign.lower(onsets, schedule.blockIds, schedule.durations, membership,
      frame, basis, Seconds(0.1))
    assert(dense.left.exists(_.message.contains("convolved term output")))
    val labels = Vector("A", "B", "C").map(ConditionId.unsafe)
    val drive = ProfileTrialDrive.make(schedule, membership, labels,
      Vector.tabulate(trials)(i => TrialId.unsafe(s"trial-$i")), membership.conditionOfTrial.map(labels))
      .fold(e => fail(e.message), identity)
    val values = Array.tabulate(rows * 5): index =>
      val t = index / 5
      val j = index % 5 + 1
      val x = t.toDouble / (rows - 1) * 2.0 - 1.0
      j match
        case 1 => x
        case 2 => x * x - 1.0 / 3.0
        case k => math.cos(math.Pi * (k - 2) * (t + 0.5) / rows)
    val baseline = BaselineModel.build(frame, BaselineBasis.Constant, intercept = Intercept.Global,
      nuisanceList = Some(Seq(Mat.unsafe(rows, 5, values))), nuisanceCheck = NuisanceCheck.Error,
      nuisanceNames = Some(Seq(Vector("linear", "quadratic", "cos1", "cos2", "cos3"))))
    val shape = DatasetShape.unsafe(SampleSpaces(Vector(1, 1, 1)), rows)
    val dataset = FmriDataset.describe(DatasetId("bounded-trial-preparation-stress"), shape,
      VoxelDomain.fullUnsafe(shape), DatasetMetadata.Empty, frame, RunId("run-1"))
      .fold(e => fail(e.message), identity)
    val config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), global = true, rho = Some(0.3)))
    val plan = ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, config, basis, 1.0)
      .fold(e => fail(e.message), identity)
    val ar = WhiteningPlan.global(ArmaCoefficients.ar(0.3), Vector(TimeSegment(0, rows, 0)))
    val policy = ProfileDecodePolicy(Vector(2, 2, 2), DecodeBudget(), None,
      trialPreparation = TrialPreparationPolicy(TrialDesignLowering.Blocked(32)))
    val prepareStarted = System.nanoTime()
    val prepared = ProfileHrfFit.prepare(plan, DataSelection.All, CanonicalTemporalWhitening.Shared(ar), policy)
      .fold(e => fail(e.message), identity)
    val prepareSeconds = (System.nanoTime() - prepareStarted) / 1e9
    val receipt = prepared.setup.trial.getOrElse(fail("missing trial preparation receipt"))
    assertEquals(receipt.trials, trials)
    assertEquals(receipt.rows, rows)
    assertEquals(receipt.nuisanceColumns, 6)
    assertEquals(receipt.loweredBlocks, 38)
    assertEquals(receipt.maxLoweredBlockValues, 192000L)
    assertEquals(prepared.setup.expandedTrialLoweringDoubles, Some(0L))
    assert(receipt.retainedDoubles <= policy.trialPreparation.maxRetainedValues)
    assertEquals(prepared.setup.bankSetup.map(_.nodeReferenceAttempts), Some(8L))
    println(s"PHRF_BOUNDED_PREPARATION {\"rows\":$rows,\"trials\":$trials,\"rank\":${basis.rank}," +
      s"\"blocks\":${receipt.loweredBlocks},\"maxBlockValues\":${receipt.maxLoweredBlockValues}," +
      s"\"retainedValues\":${receipt.retainedDoubles},\"bandwidth\":${receipt.bandwidth}," +
      s"\"retainedSourceValues\":0,\"bankBytes\":${prepared.setup.retainedReferenceBytes.getOrElse(0L)}," +
      s"\"basisSeconds\":$basisSeconds,\"prepareSeconds\":$prepareSeconds,\"qualification\":\"not-admitted\"}")
