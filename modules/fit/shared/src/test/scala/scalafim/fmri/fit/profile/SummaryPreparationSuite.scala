package scalafim.fmri.fit.profile

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.{DataSelection, DatasetSeriesReader, DatasetId, FmriDataset, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.{ConditionId, FactorId, FactorLevelSet, TrialId}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.{Event, EventModel, EventSchedule, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.fit.{CanonicalTemporalWhitening, FitError}
import scalafim.fmri.hrf.{Hrf, HrfDescriptor, HrfKind, PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{FamilySummaryError, GaussianFamily, NormalizationRule, ParametricHrfFamily, ShapeChart, ShapePoint, ShapeSummary}
import scalafim.fmri.model.{FitConfig, FitPlan, FmriModel, ProfileHrfPlan, ProfileTrialDrive}
import scalafim.image.SampleSpaces

class SummaryPreparationSuite extends munit.FunSuite:
  private def checked[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)
  private val refusal = FamilySummaryError.SampleLimitExceeded(1000001.0, 1000000)

  private class ProbeFamily extends ParametricHrfFamily:
    private val source = GaussianFamily.Default
    var summaryChecks = 0
    var summaryCalls = 0
    var rejectSummary = false
    var preflightFailure = false
    var summaryFailure = false
    var summaryOverflow = false
    var scientificCalls = 0
    var forbidScientific = false
    private def scientific(): Unit =
      scientificCalls += 1
      if forbidScientific then fail("scientific callback touched after summary refusal")
    def name: String = source.name
    def kind: HrfKind = source.kind
    def chart: ShapeChart = source.chart
    def horizon: PositiveSeconds = source.horizon
    def supports(rule: NormalizationRule): Boolean = source.supports(rule)
    def libraryNormalization: NormalizationRule = source.libraryNormalization
    override def validateSummaryGrid: Either[FamilySummaryError, Unit] =
      summaryChecks += 1
      if preflightFailure then throw new IllegalArgumentException("summary admission callback failed")
      if rejectSummary then Left(refusal) else Right(())
    def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
      scientific()
      source.evalInto(lags, point, out)
    def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
      scientific()
      source.jetInto(lags, point, out)
    def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit =
      scientific()
      source.scaleJetInto(rule, point, out)
    def summaries(point: ShapePoint): ShapeSummary =
      summaryCalls += 1
      if summaryFailure then throw new IllegalArgumentException("decoded summary callback failed")
      if summaryOverflow then ShapeSummary(Seconds.unsafe(Double.PositiveInfinity), Seconds.unsafe(Double.NaN), None)
      else source.summaries(point)
    def descriptor(point: ShapePoint): HrfDescriptor =
      scientific()
      source.descriptor(point)
    def toHrf(point: ShapePoint): Hrf =
      scientific()
      source.toHrf(point)

  private final class Fixture:
    val family = new ProbeFamily
    val basis = checked(HrfKernelBasis.compile(KernelBasisSpec(family, PositiveSeconds(0.2).toOption.get,
      Vector(8, 7), tolerance = 0.5, maxRank = 4, heldOutPoints = 1)))
    val frame = SamplingFrame(blockLens = Seq(60), tr = Seq(1.0))
    val point = checked(family.chart.point(5.3, math.log(1.55)))
    val schedule = checked(EventSchedule.fromParts(Vector(5.0, 24.0, 42.0).map(Seconds(_)), Vector.fill(3)(Seconds(0.0)), Vector.fill(3)(0)))
    val levels = FactorLevelSet.unsafe(FactorId.unsafe("condition"), Vector("A"))
    val event = checked(Event.factorWithLevels(Vector.fill(3)("A"), "condition", levels))
    val term = checked(EventTerm.fromSchedule(Vector(event), schedule, Some("condition")))
    val expanded = checked(ExpandedConditionDesign.lower(term, frame, basis, Seconds(0.2), dropEmpty = false))
    val convolved = term.convolve(basis.kernel, frame, precision = Seconds(0.2), dropEmpty = false)
    val baseline = BaselineModel.build(frame, BaselineBasis.Constant, intercept = Intercept.Global)
    val nuisance = DMat.tabulate(60, baseline.designMatrix.cols)((t, j) => baseline.designMatrix(t, j))
    val data = Matrix.dense(60, 2, (0 until 60).flatMap(t => Vector(0.7 + math.sin(t * 0.31), 0.9 + math.cos(t * 0.19))))
    val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("summary-preparation"), data, SampleSpaces(Vector(2, 1, 1))), frame).dataset
    val events = EventModel(Vector("task" -> convolved), frame, convolved.data, convolved.columnNames,
      Vector(0 -> convolved.data.cols), Map("task" -> (0 until convolved.data.cols).toVector))
    val fixed = FitPlan(FmriModel(events, baseline, dataset))
    val structure = checked(ConditionProfileFit.structureFor(fixed, convolved))
    val requirements = ObservedFamilyRequirements(0.5, 1e10, 1e-9)
    val fixedAdmission = checked(ObservedFamilyCertification.admitForCondition(fixed, structure, expanded,
      term, frame, Seconds(0.2), None, Some(nuisance), Vector(point), requirements))
    val compactAdmission = checked(ObservedFamilyCertification.admitForCompact(expanded, term, frame,
      Seconds(0.2), None, Some(nuisance), Vector(point), requirements))
    val membership = checked(TrialMembership.make(Vector(0, 0, 0), 1))
    val drive = checked(ProfileTrialDrive.make(schedule, membership, Vector(ConditionId.unsafe("A")),
      Vector.tabulate(3)(i => TrialId.unsafe(s"trial-$i")), Vector.fill(3)(ConditionId.unsafe("A"))))
    val budget = DecodeBudget(maxNewtonSteps = 1, maxJets = 2, maxExactEvaluations = 4)
    def conditionPolicy = ConditionProfilePolicy(basis, structure, Vector(3, 3), budget, None, 1.0,
      OutputRequest.ConditionAmplitudes(NormalizationRule.Density), fixedAdmission, blockSize = 1)
    def profilePolicy(admission: Option[ObservedFamilyAdmission]) = ProfileDecodePolicy(Vector(3, 3), budget, None,
      ExecutionBudget(blockSize = 1, workers = 1), admission)
    def compact = checked(CompactConditionPreparation.prepare(expanded, compactAdmission, None, Some(nuisance), term, frame, Seconds(0.2)))
    def rawPlan(alpha: Double) = checked(ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, FitConfig(), basis, alpha))
    def reader = checked(SynchronousFmriDataset.readerFor(dataset))
    def resetSummaryCounters(): Unit =
      family.summaryChecks = 0
      family.summaryCalls = 0
    def profileSink(payloads: scala.collection.mutable.ArrayBuffer[ProfileFitBlock]) = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
      def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
        payloads += payload
        Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))

  test("standalone condition summary refusal precedes scientific callbacks and reader descriptors"):
    val fixture = new Fixture
    fixture.family.rejectSummary = true
    fixture.family.forbidScientific = true
    val callbacks = fixture.family.scientificCalls
    var descriptors = 0
    var reads = 0
    val sentinel = new DatasetSeriesReader:
      def dataset: FmriDataset =
        descriptors += 1
        fail("reader descriptor touched")
      def seriesEither(selection: DataSelection) =
        reads += 1
        fail("reader touched")
    val sink = new BlockSink[ConditionProfileBlock, ConditionProfileReceipt]:
      def accept(block: VoxelBlock, payload: ConditionProfileBlock): Either[String, ConditionProfileReceipt] = fail("sink touched")
    val result = ConditionProfileFit.prepare(fixture.fixed, fixture.conditionPolicy).flatMap(_.run(sentinel, sink))
    assertEquals(result, Left(FitError.InvalidFitAxis("condition profile summary", refusal.message)))
    assertEquals(fixture.family.scientificCalls, callbacks)
    assertEquals(fixture.family.summaryCalls, 0)
    assertEquals(descriptors, 0)
    assertEquals(reads, 0)

  test("explicit compact summary preparation refuses before geometry callbacks"):
    val fixture = new Fixture
    fixture.family.rejectSummary = true
    fixture.family.forbidScientific = true
    val callbacks = fixture.family.scientificCalls
    val result = CompactConditionPreparation.prepareWithSummaries(fixture.expanded, fixture.compactAdmission,
      None, Some(fixture.nuisance), fixture.term, fixture.frame, Seconds(0.2))
    assertEquals(result, Left(CompactConditionError.Summary(refusal)))
    assertEquals(fixture.family.scientificCalls, callbacks)
    assertEquals(fixture.family.summaryCalls, 0)

  test("compact typed runtime and legacy constructor refuse before decoder callbacks"):
    val fixture = new Fixture
    val prep = fixture.compact
    fixture.family.rejectSummary = true
    fixture.family.forbidScientific = true
    val callbacks = fixture.family.scientificCalls
    val grid = NodeGrid(fixture.family.chart, Vector(3, 3))
    assertEquals(CompactConditionRuntime.prepare(prep, grid, fixture.budget, None, 1.0, NormalizationRule.Density), Left(CompactConditionError.Summary(refusal)))
    intercept[IllegalArgumentException](new CompactConditionRuntime(prep, grid, fixture.budget, None, 1.0, NormalizationRule.Density))
    assertEquals(fixture.family.scientificCalls, callbacks)
    assertEquals(fixture.family.summaryCalls, 0)

  test("raw unified fixed compact and trial outputs never request summary admission or evaluation"):
    val fixture = new Fixture
    fixture.family.rejectSummary = true
    fixture.family.summaryFailure = true
    val fixedPlan = checked(ProfileHrfPlan.fromFixed(fixture.fixed, fixture.convolved, fixture.basis))
    for (plan, admission, expectedRoute) <- Vector(
      (fixedPlan, Some(fixture.fixedAdmission), "fixed-condition-ols"),
      (fixture.rawPlan(0.0), Some(fixture.compactAdmission), "direct-condition-compact"),
      (fixture.rawPlan(0.4), None, "trial-banded")
    ) do
      fixture.family.rejectSummary = false
      fixture.family.summaryFailure = false
      fixture.family.summaryOverflow = false
      val baselinePrepared = checked(ProfileHrfFit.prepare(plan, DataSelection.All, CanonicalTemporalWhitening.Iid, fixture.profilePolicy(admission)))
      val baselinePayloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
      val baselineResult = checked(baselinePrepared.run(fixture.reader, fixture.profileSink(baselinePayloads)))
      fixture.family.rejectSummary = true
      fixture.family.summaryOverflow = true
      fixture.resetSummaryCounters()
      val prepared = checked(ProfileHrfFit.prepare(plan, DataSelection.All, CanonicalTemporalWhitening.Iid, fixture.profilePolicy(admission)))
      val payloads = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
      val result = checked(prepared.run(fixture.reader, fixture.profileSink(payloads)))
      assert(result.setup.route.contains(expectedRoute), result.setup.route)
      assertEquals(result.progress.deliveredVoxels, 2)
      assertEquals(payloads.flatMap(_.results).length, 2)
      assertEquals(fixture.family.summaryChecks, 0)
      assertEquals(fixture.family.summaryCalls, 0)
      assertEquals(result.provenance, baselineResult.provenance)
      assertEquals(result.progress, baselineResult.progress)
      for (actual, expected) <- payloads.flatMap(_.results).zip(baselinePayloads.flatMap(_.results)) do
        assertEquals(actual.coordinates.map(java.lang.Double.doubleToLongBits), expected.coordinates.map(java.lang.Double.doubleToLongBits))
        assertEquals(actual.conditionMeans.map(java.lang.Double.doubleToLongBits), expected.conditionMeans.map(java.lang.Double.doubleToLongBits))
        assertEquals(java.lang.Double.doubleToLongBits(actual.penalizedEnergy), java.lang.Double.doubleToLongBits(expected.penalizedEnergy))
        assertEquals(actual.status, expected.status)

  test("admitted standalone summary results retain Gaussian scientific output"):
    val fixture = new Fixture
    val prepared = checked(ConditionProfileFit.prepare(fixture.fixed, fixture.conditionPolicy))
    val payloads = scala.collection.mutable.ArrayBuffer.empty[ConditionProfileBlock]
    val sink = new BlockSink[ConditionProfileBlock, ConditionProfileReceipt]:
      def accept(block: VoxelBlock, payload: ConditionProfileBlock): Either[String, ConditionProfileReceipt] =
        payloads += payload
        Right(ConditionProfileReceipt(payload.ordinal, payload.results.length, payload.results.length))
    checked(prepared.run(fixture.reader, sink))
    assertEquals(payloads.flatMap(_.results).length, 2)
    for fit <- payloads.flatMap(_.results) do
      val expected = GaussianFamily.Default.summaries(ShapePoint.unsafe(fit.coordinates))
      assertEqualsDouble(fit.summaries.peakLatency.value, expected.peakLatency.value, 1e-14)
      assertEqualsDouble(fit.summaries.fwhm.value, expected.fwhm.value, 1e-14)
      assertEquals(fit.summaries.undershootRatio, None)

  test("decoded summary callback failure is typed while raw execution avoids it"):
    val fixture = new Fixture
    fixture.family.summaryFailure = true
    val prep = fixture.compact
    val runtime = checked(CompactConditionRuntime.prepare(prep, NodeGrid(fixture.family.chart, Vector(3, 3)), fixture.budget, None, 1.0, NormalizationRule.Density))
    val response = Array.tabulate(60)(i => fixture.data(i, 0))
    assert(runtime.fitEither(response, 0, new DecoderCounters).swap.exists(_.isInstanceOf[FamilySummaryError.EvaluationFailed]))
    assertEquals(fixture.family.summaryCalls, 1)

  test("standalone summary failure returns a typed error after previously delivered blocks"):
    val fixture = new Fixture
    val prepared = checked(ConditionProfileFit.prepare(fixture.fixed, fixture.conditionPolicy))
    val payloads = scala.collection.mutable.ArrayBuffer.empty[ConditionProfileBlock]
    val sink = new BlockSink[ConditionProfileBlock, ConditionProfileReceipt]:
      def accept(block: VoxelBlock, payload: ConditionProfileBlock): Either[String, ConditionProfileReceipt] =
        payloads += payload
        fixture.family.summaryFailure = true
        Right(ConditionProfileReceipt(payload.ordinal, payload.results.length, payload.results.length))
    prepared.run(fixture.reader, sink) match
      case Left(error) => assert(error.message.contains("decoded summary callback failed"), error.message)
      case Right(_) => fail("expected decoded summary failure")
    assertEquals(payloads.length, 1)
    assertEquals(payloads.head.results.length, 1)
    assertEquals(fixture.family.summaryCalls, 2)

  test("throwing summary admission callbacks are typed before geometry"):
    val fixture = new Fixture
    fixture.family.preflightFailure = true
    fixture.family.forbidScientific = true
    val expected = FamilySummaryError.EvaluationFailed("summary admission callback failed")
    val callbacks = fixture.family.scientificCalls
    assertEquals(ConditionProfileFit.prepare(fixture.fixed, fixture.conditionPolicy), Left(FitError.InvalidFitAxis("condition profile summary", expected.message)))
    assertEquals(CompactConditionPreparation.prepareWithSummaries(fixture.expanded, fixture.compactAdmission, None,
      Some(fixture.nuisance), fixture.term, fixture.frame, Seconds(0.2)), Left(CompactConditionError.Summary(expected)))
    assertEquals(fixture.family.scientificCalls, callbacks)
    assertEquals(fixture.family.summaryCalls, 0)

  test("unsupported trial outputs are refused without condition summary admission"):
    val fixture = new Fixture
    fixture.family.preflightFailure = true
    fixture.resetSummaryCounters()
    val request = fixture.conditionPolicy.copy(output = OutputRequest.TrialAmplitudes(NormalizationRule.Density))
    ConditionProfileFit.prepare(fixture.fixed, request) match
      case Left(FitError.InvalidFitAxis(axis, detail)) =>
        assertEquals(axis, "condition profile output")
        assert(detail.contains("requires the trial backend"), detail)
      case other => fail(s"expected unsupported trial output, got $other")
    assertEquals(fixture.family.summaryChecks, 0)
    assertEquals(fixture.family.summaryCalls, 0)
