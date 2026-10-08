package scalafim.fmri.fit.profile

import gale.linalg.DMat
import scalafim.dataset.*
import scalafim.fmri.ar.{ArmaCoefficients, InitialConditionPolicy, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.{ConditionId, TrialId}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.{Event, EventModel, EventSchedule, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.fit.CanonicalTemporalWhitening
import scalafim.fmri.hrf.{Hrf, HrfDescriptor, PositiveSeconds, Seconds, Support}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.*
import scalafim.fmri.model.*
import scalafim.image.SampleSpaces
import scalafim.image.world.{TemplateName, WorldSpace}

class ProfileFitIdentitySuite extends munit.FunSuite:
  // A rank-one, exactly representable fixture keeps these structural goldens
  // independent of floating-point variation in basis compilation.
  private object GoldenFamily extends ParametricHrfFamily:
    val name = "identity-golden"
    val kind = GaussianFamily.Default.kind
    val chart = ShapeChart(("shape", 0.0, 1.0))
    val horizon = PositiveSeconds.unsafe(Seconds(2.0))
    def supports(rule: NormalizationRule) = true
    val libraryNormalization = NormalizationRule.Unnormalised
    def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
      var i = 0
      while i < lags.length do
        out(i) = if lags(i) >= 0.0 then math.max(0.0, 1.0 - lags(i)) else 0.0
        i += 1
    def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
      java.util.Arrays.fill(out, 0.0)
      evalInto(lags, point, out)
    def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit =
      java.util.Arrays.fill(out, 0.0)
      out(0) = 1.0
    def summaries(point: ShapePoint) = ShapeSummary(Seconds(0.0), Seconds(1.0), None)
    def descriptor(point: ShapePoint) = HrfDescriptor.derived(name, 1, Seconds(2.0))
    def toHrf(point: ShapePoint) = Hrf.scalar(name, Seconds(2.0), Some(descriptor(point)), Support.Compact(Seconds(2.0))): lag =>
      if lag.value >= 0.0 then math.max(0.0, 1.0 - lag.value) else 0.0

  private lazy val basis = HrfKernelBasis.compile(KernelBasisSpec(GoldenFamily,
    PositiveSeconds.unsafe(Seconds(1.0)), Vector(2), tolerance = 0.1,
    maxRank = 1, includeDerivatives = false, heldOutPoints = 1))
    .fold(error => fail(error.message), identity)
  private val frame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0), precision = 0.1)
  private val schedule = EventSchedule.fromParts(Vector(Seconds(0.0), Seconds(2.0)))
    .fold(error => fail(error.message), identity)
  private val term = EventTerm(Vector(Event.factor(Vector("a|b", "a|b"), "condition")), schedule.onsets)
  private lazy val expanded = ExpandedConditionDesign.lower(term, frame, basis, Seconds(1.0))
    .fold(error => fail(error.message), identity)
  private val metadata = DatasetMetadata.fromValues(Map(
    DatasetFieldId.unsafe("number") -> DatasetValue.Number(24.0),
    DatasetFieldId.unsafe("source") -> DatasetValue.Number(-0.0, Some("-0.0")),
    DatasetFieldId.unsafe("text") -> DatasetValue.Text("x|=;")))
    .fold(error => fail(error.message), identity)
  private lazy val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("identity|dataset"),
    DMat.zeros(4, 1), SampleSpaces.inWorld(SampleSpaces(Vector(1, 1, 1)),
      WorldSpace.Template(TemplateName.unsafe("identity-golden"))).fold(error => fail(error.message), identity), metadata), frame).dataset
  private lazy val baseline = BaselineModel.build(frame, BaselineBasis.Constant, intercept = Intercept.Global)
  private val membership = TrialMembership.make(Vector(0, 0), 1).fold(error => fail(error.message), identity)
  private val drive = ProfileTrialDrive.make(schedule, membership, Vector(ConditionId.unsafe("a|b")),
    Vector(TrialId.unsafe("t;1"), TrialId.unsafe("t;2")), Vector.fill(2)(ConditionId.unsafe("a|b")))
    .fold(error => fail(error.message), identity)
  private lazy val plan = ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, FitConfig(), basis, 0.4)
    .fold(error => fail(error.message), identity)
  private val policy = ProfileDecodePolicy(Vector(2), DecodeBudget(weakSdLimit = Vector(24.0),
    ambiguityEnergy = 0.1), Some(ShapePrior(Vector(-0.0), Vector(1e-6))), ExecutionBudget(3, 1))

  private def prepared(p: ProfileHrfPlan = plan, requested: ProfileDecodePolicy = policy): PreparedProfileHrf =
    ProfileHrfFit.prepare(p, DataSelection.All, CanonicalTemporalWhitening.Iid, requested)
      .fold(error => fail(error.message), identity)

  private def geometry(t: EventTerm = term, f: SamplingFrame = frame, precision: Double = 0.1,
      whitening: Option[WhiteningPlan] = None, nuisance: Option[DMat] = None): String =
    ObservedFamilyAdmission.geometry(expanded, t, f, Seconds(precision), whitening, nuisance)

  test("actual prepared profile provenance has the same literal golden on JVM and JS"):
    assertEquals(prepared().provenance, ProfileFitIdentitySuite.ProfileGolden)


  private lazy val fixedFixture: (ProfileHrfPlan, ProfileDecodePolicy) =
    val convolved = expanded.term
    val events = EventModel(Vector("task" -> convolved), frame, convolved.data, convolved.columnNames,
      Vector(0 -> convolved.data.cols), Map("task" -> (0 until convolved.data.cols).toVector))
    val fixed = FitPlan(FmriModel(events, baseline, dataset))
    val structure = ConditionProfileFit.structureFor(fixed, convolved).fold(error => fail(error.message), identity)
    val nuisance = DMat.tabulate(4, 1)((_, _) => 1.0)
    val admission = ObservedFamilyCertification.admitForCondition(fixed, structure, expanded, term, frame,
      Seconds(1.0), None, Some(nuisance), Vector(ShapePoint.unsafe(Vector(0.5))),
      ObservedFamilyRequirements(0.1, 1e8, 1e-6)).fold(error => fail(error.message), identity)
    val fixedPlan = ProfileHrfPlan.fromFixed(fixed, convolved, basis).fold(error => fail(error.message), identity)
    (fixedPlan, policy.copy(observedAdmission = Some(admission)))

  test("actual fixed-condition provenance has the same literal golden on JVM and JS"):
    val (fixed, requested) = fixedFixture
    assertEquals(prepared(fixed, requested).provenance, ProfileFitIdentitySuite.FixedGolden)

  test("actual ML provenance has the same literal golden on JVM and JS"):
    val mlPlan = ProfileHrfPlan.make(plan.source, basis, plan.amplitudes, ProfileCriterion.TrialRandomEffectsML(0.1))
      .fold(error => fail(error.message), identity)
    assertEquals(prepared(mlPlan).provenance, ProfileFitIdentitySuite.MlGolden)

  test("observed geometry has the same literal golden on JVM and JS"):
    val whitening = WhiteningPlan.globalWithInitialCondition(
      ArmaCoefficients(Vector(-0.0, 0.1), Vector(1e-6)), Vector(TimeSegment(0, 4, 0)),
      InitialConditionPolicy.PrecomputedScale(0.1)).fold(error => fail(error.message), identity)
    val nuisance = DMat.tabulate(4, 1)((row, _) => Vector(24.0, -0.0, 0.1, 1e-6)(row))
    assertEquals(geometry(whitening = Some(whitening), nuisance = Some(nuisance)), ProfileFitIdentitySuite.GeometryGolden)

  test("geometry binds schedule, event weights, conditions, frame, precision, whitening and nuisance"):
    val original = geometry()
    val changedFrame = SamplingFrame(blockLens = Seq(4), tr = Seq(0.1), startTime = Seq(-0.0), precision = 1e-6)
    val changes = Vector(
      geometry(t = term.copy(onsets = Vector(Seconds(0.1), Seconds(2.0)))),
      geometry(t = term.copy(durations = Vector(Seconds(0.1), Seconds(0.0)))),
      geometry(t = term.copy(blockIds = Vector(0, 1))),
      geometry(t = term.copy(events = Vector(Event.variable(Vector(1.0, 0.1), "weight")))),
      geometry(f = changedFrame), geometry(precision = 1e-6),
      geometry(whitening = Some(WhiteningPlan.global(ArmaCoefficients.ar(0.1), Vector(TimeSegment(0, 4, 0))))),
      geometry(nuisance = Some(DMat.tabulate(4, 1)((row, _) => row.toDouble))))
    changes.foreach(assertNotEquals(original, _))
    val renamed = term.copy(events = Vector(Event.factor(Vector("different", "different"), "condition")))
    val renamedExpanded = ExpandedConditionDesign.lower(renamed, frame, basis, Seconds(1.0))
      .fold(error => fail(error.message), identity)
    assertNotEquals(original, ObservedFamilyAdmission.geometry(renamedExpanded, renamed,
      frame, Seconds(0.1), None, None))
    val saved = expanded.term.data.data(0)
    try
      expanded.term.data.data(0) = 0.1
      assertNotEquals(original, geometry())
    finally expanded.term.data.data(0) = saved
    assertNotEquals(geometry(precision = 0.0), geometry(precision = -0.0))
    assertNotEquals(ProfileFitIdentity.strings(Vector("a,b", "c")), ProfileFitIdentity.strings(Vector("a", "b,c")))

  test("actual prepared profile provenance discriminates decoder fields, prior, execution and amplitude"):
    val original = prepared().provenance
    val budgets = Vector(
      policy.budget.copy(coarseStride = policy.budget.coarseStride + 1),
      policy.budget.copy(maxNewtonSteps = policy.budget.maxNewtonSteps + 1),
      policy.budget.copy(maxJets = policy.budget.maxJets + 1),
      policy.budget.copy(maxExactEvaluations = policy.budget.maxExactEvaluations + 1),
      policy.budget.copy(weakSdLimit = Vector(0.1)),
      policy.budget.copy(ambiguityEnergy = 1e-6),
      policy.budget.copy(maxCandidateAttempts = policy.budget.maxCandidateAttempts + 1),
      policy.budget.copy(stationarityStepTolerance = 0.1))
    val policies = budgets.map(b => policy.copy(budget = b)) ++ Vector(
      policy.copy(nodesPerAxis = Vector(3)), policy.copy(prior = None),
      policy.copy(prior = Some(ShapePrior(Vector(0.0), Vector(1e-6)))),
      policy.copy(prior = Some(ShapePrior(Vector(-0.0), Vector(0.1)))),
      policy.copy(execution = ExecutionBudget(4, 1)), policy.copy(execution = ExecutionBudget(3, 2)),
      policy.copy(trialPreparation = TrialPreparationPolicy(scalafim.fmri.design.hrf.TrialDesignLowering.Blocked(1))),
      policy.copy(trialPreparation = TrialPreparationPolicy(maxRetainedValues = 10000L)))
    policies.foreach(p => assertNotEquals(original, prepared(requested = p).provenance))
    val otherAmplitude = ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, FitConfig(), basis, 0.1)
      .fold(error => fail(error.message), identity)
    assertNotEquals(original, prepared(otherAmplitude).provenance)
    val otherCriterion = ProfileHrfPlan.make(plan.source, basis, plan.amplitudes, ProfileCriterion.PenalizedProfile(0.1))
      .fold(error => fail(error.message), identity)
    assertNotEquals(original, prepared(otherCriterion).provenance)
    val otherMetadata = metadata.updatedValue(DatasetFieldId.unsafe("number"), DatasetValue.Number(0.1))
    val otherDataset = FmriDataset.unsafe(InMemoryDatasetBackend(dataset.id, DMat.zeros(4, 1),
      dataset.shape.space, otherMetadata), frame).dataset
    val otherPlan = ProfileHrfPlan.fromTrialEvents(otherDataset, drive, baseline, FitConfig(), basis, 0.4)
      .fold(error => fail(error.message), identity)
    assertNotEquals(original, prepared(otherPlan).provenance)

  test("typed metadata and whitening distinguish number bits, optional source and initial policy"):
    assertNotEquals(ProfileFitIdentity.datasetValue(DatasetValue.Number(0.0)),
      ProfileFitIdentity.datasetValue(DatasetValue.Number(-0.0)))
    assertNotEquals(ProfileFitIdentity.datasetValue(DatasetValue.Number(24.0)),
      ProfileFitIdentity.datasetValue(DatasetValue.Integer(24)))
    assertNotEquals(ProfileFitIdentity.datasetValue(DatasetValue.Number(24.0)),
      ProfileFitIdentity.datasetValue(DatasetValue.Number(24.0, Some("24"))))
    assertNotEquals(ProfileFitIdentity.config(FitConfig()),
      ProfileFitIdentity.config(FitConfig(lss = LssConfig(eps = 0.1))))
    val w = WhiteningPlan.global(ArmaCoefficients.ar(0.1), Vector(TimeSegment(0, 4, 0)))
    val scaled = WhiteningPlan.globalWithInitialCondition(w.coefficients.head, w.segments,
      InitialConditionPolicy.PrecomputedScale(0.1)).fold(error => fail(error.message), identity)
    assertNotEquals(ProfileFitIdentity.whitening(w),
      ProfileFitIdentity.whitening(scaled))
    assertNotEquals(ProfileFitIdentity.whitening(w),
      ProfileFitIdentity.whitening(WhiteningPlan.byRun(w.coefficients, w.segments)))

object ProfileFitIdentitySuite:
  val ProfileGolden = "profile-fit/v3|dataset=797:dataset(16:identity|dataset,331:space(20:indices(1:1,1:1,1:1),43:matrix(1:4,1:4,24:fnv1a64:78704a9e6b24e2e5),164:some(154:scalafim-grid-scalafim-world:template:identity-golden-d3-1x1x1-3ff0000000000000-0-0-0-0-3ff0000000000000-0-0-0-0-3ff0000000000000-0-0-0-0-3ff0000000000000),48:some(39:scalafim-world:template:identity-golden),1:3,10:millimeter,3:ras,6:axes()),1:4,12:indices(1:0),161:frame(22:blockLens=indices(1:4),38:tr=values(24:bits:4607182418800017408),45:startTime=values(24:bits:4602678819172646912),34:precision=bits:4591870180066957722),8:events(),40:time_axis(26:block(1:0,5:run-1,1:0,1:4)),180:fields(61:field(6:number,42:number(24:bits:4627448617123184640,4:none)),71:field(6:source,52:number(25:bits:-9223372036854775808,12:some(4:-0.0))),29:field(4:text,12:text(4:x|=;))),4:none)|selected=selected(24:indices(1:0,1:1,1:2,1:3),12:indices(1:0))|drive=207:trials(112:schedule(38:event(7:event_1,6:bits:0,6:bits:0,1:0),57:event(7:event_2,24:bits:4611686018427387904,6:bits:0,1:0)),13:labels(3:a|b),19:labels(3:t;1,3:t;2),16:indices(1:0,1:0),19:labels(3:a|b,3:a|b))|basis=282:kernel-basis/v2|family=15:identity-golden|chart=chart(50:axis(5:shape,6:bits:0,24:bits:4607182418800017408))|horizon=bits:4611686018427387904|step=bits:4607182418800017408|nodes=nodes(1:2)|derivatives=false|tolerance=bits:4591870180066957722|maxRank=1|heldOutPoints=1|rank=1|seed=11|basis-lags=matrix(1:1,1:3,24:fnv1a64:7fc5a13e70b0f698)|basis-values=matrix(1:3,1:1,24:fnv1a64:6d35889ab17dfbcc)|basis-coefficients=coefficient_jets(43:matrix(1:3,1:1,24:fnv1a64:6d35889ab17dfbcc),43:matrix(1:3,1:1,24:fnv1a64:6d35889ab17dfbcc))|nuisance=matrix(1:4,1:1,24:fnv1a64:d137d9e6997fe665)|config=906:fit_config(819:response-preparation/v1|records=records(123:record(26:step=missing_data(5:error),82:disposition=applied(58:dense input constructors reject non-finite response values)),70:record(31:step=censoring(12:timepoints()),24:disposition=disabled(0:)),73:record(34:step=volume_weights(10:disabled()),24:disposition=disabled(0:)),78:record(39:step=nuisance_projection(10:disabled()),24:disposition=disabled(0:)),217:record(177:step=whitening(157:ar_options(15:structure=iid(),12:iterations=1,12:global=false,15:voxelwise=false,15:exactFirst=true,31:censoredTimepoints=timepoints(),8:rho=none,8:phi=none)),24:disposition=disabled(0:)),170:record(130:step=robust_weights(105:robust_options(14:psi=disabled(),15:maxIterations=2,14:scaleScope=run,31:reestimateAutocorrelation=false)),24:disposition=disabled(0:)))|volumeWeighting=none,67:lss(4:none,24:bits:4427486594234968593,24:bits:4502148214488346440))|whitening=iid()|amplitudes=condition_centered_trials(24:bits:4600877379321698714,24:bits:4612811918334230528)|criterion=penalized_profile(24:bits:4607182418800017408)|grid=indices(1:2)|decode=268:decode-budget/v2|coarseStride=2|maxNewtonSteps=2|maxJets=2|maxExactEvaluations=6|weakSdLimit=values(24:bits:4627448617123184640)|ambiguityEnergy=bits:4591870180066957722|maxCandidateAttempts=4|stationarityStepTolerance=bits:4472406533629990549|initialization=bank-node|prior=some(106:shape_prior(41:mean=values(25:bits:-9223372036854775808),45:precision=values(24:bits:4517329193108106637)))|admission=none|execution=execution(1:3,1:1)|route=12:trial-banded|ml=none|exact-readout=true"
  val GeometryGolden = "observed-family-geometry/v2|basis=282:kernel-basis/v2|family=15:identity-golden|chart=chart(50:axis(5:shape,6:bits:0,24:bits:4607182418800017408))|horizon=bits:4611686018427387904|step=bits:4607182418800017408|nodes=nodes(1:2)|derivatives=false|tolerance=bits:4591870180066957722|maxRank=1|heldOutPoints=1|rank=1|seed=11|drives=schedule(38:event(7:event_1,6:bits:0,6:bits:0,1:0),57:event(7:event_2,24:bits:4611686018427387904,6:bits:0,1:0))|event-weights=matrix(1:2,1:1,24:fnv1a64:2be2cbea19a827c5)|conditions=labels(13:condition.a.b)|expanded=matrix(1:4,1:1,24:fnv1a64:36ece28ef707b20c)|frame=frame(22:blockLens=indices(1:4),38:tr=values(24:bits:4607182418800017408),45:startTime=values(24:bits:4602678819172646912),34:precision=bits:4591870180066957722)|precision=bits:4591870180066957722|whitening=some(238:whitening(124:global(112:arma(64:values(25:bits:-9223372036854775808,24:bits:4591870180066957722),35:values(24:bits:4517329193108106637))),33:segments(20:segment(1:0,1:4,1:0)),1:4,46:precomputed_scale(24:bits:4591870180066957722),5:fixed))|nuisance=some(43:matrix(1:4,1:1,24:fnv1a64:e2c1bfedb905a138))"
  val MlGolden = "profile-fit/v3|dataset=797:dataset(16:identity|dataset,331:space(20:indices(1:1,1:1,1:1),43:matrix(1:4,1:4,24:fnv1a64:78704a9e6b24e2e5),164:some(154:scalafim-grid-scalafim-world:template:identity-golden-d3-1x1x1-3ff0000000000000-0-0-0-0-3ff0000000000000-0-0-0-0-3ff0000000000000-0-0-0-0-3ff0000000000000),48:some(39:scalafim-world:template:identity-golden),1:3,10:millimeter,3:ras,6:axes()),1:4,12:indices(1:0),161:frame(22:blockLens=indices(1:4),38:tr=values(24:bits:4607182418800017408),45:startTime=values(24:bits:4602678819172646912),34:precision=bits:4591870180066957722),8:events(),40:time_axis(26:block(1:0,5:run-1,1:0,1:4)),180:fields(61:field(6:number,42:number(24:bits:4627448617123184640,4:none)),71:field(6:source,52:number(25:bits:-9223372036854775808,12:some(4:-0.0))),29:field(4:text,12:text(4:x|=;))),4:none)|selected=selected(24:indices(1:0,1:1,1:2,1:3),12:indices(1:0))|drive=207:trials(112:schedule(38:event(7:event_1,6:bits:0,6:bits:0,1:0),57:event(7:event_2,24:bits:4611686018427387904,6:bits:0,1:0)),13:labels(3:a|b),19:labels(3:t;1,3:t;2),16:indices(1:0,1:0),19:labels(3:a|b,3:a|b))|basis=282:kernel-basis/v2|family=15:identity-golden|chart=chart(50:axis(5:shape,6:bits:0,24:bits:4607182418800017408))|horizon=bits:4611686018427387904|step=bits:4607182418800017408|nodes=nodes(1:2)|derivatives=false|tolerance=bits:4591870180066957722|maxRank=1|heldOutPoints=1|rank=1|seed=11|basis-lags=matrix(1:1,1:3,24:fnv1a64:7fc5a13e70b0f698)|basis-values=matrix(1:3,1:1,24:fnv1a64:6d35889ab17dfbcc)|basis-coefficients=coefficient_jets(43:matrix(1:3,1:1,24:fnv1a64:6d35889ab17dfbcc),43:matrix(1:3,1:1,24:fnv1a64:6d35889ab17dfbcc))|nuisance=matrix(1:4,1:1,24:fnv1a64:d137d9e6997fe665)|config=906:fit_config(819:response-preparation/v1|records=records(123:record(26:step=missing_data(5:error),82:disposition=applied(58:dense input constructors reject non-finite response values)),70:record(31:step=censoring(12:timepoints()),24:disposition=disabled(0:)),73:record(34:step=volume_weights(10:disabled()),24:disposition=disabled(0:)),78:record(39:step=nuisance_projection(10:disabled()),24:disposition=disabled(0:)),217:record(177:step=whitening(157:ar_options(15:structure=iid(),12:iterations=1,12:global=false,15:voxelwise=false,15:exactFirst=true,31:censoredTimepoints=timepoints(),8:rho=none,8:phi=none)),24:disposition=disabled(0:)),170:record(130:step=robust_weights(105:robust_options(14:psi=disabled(),15:maxIterations=2,14:scaleScope=run,31:reestimateAutocorrelation=false)),24:disposition=disabled(0:)))|volumeWeighting=none,67:lss(4:none,24:bits:4427486594234968593,24:bits:4502148214488346440))|whitening=iid()|amplitudes=condition_centered_trials(24:bits:4600877379321698714,24:bits:4612811918334230528)|criterion=trial_random_effects_ml(24:bits:4591870180066957722)|grid=indices(1:2)|decode=268:decode-budget/v2|coarseStride=2|maxNewtonSteps=2|maxJets=2|maxExactEvaluations=6|weakSdLimit=values(24:bits:4627448617123184640)|ambiguityEnergy=bits:4591870180066957722|maxCandidateAttempts=4|stationarityStepTolerance=bits:4472406533629990549|initialization=bank-node|prior=some(106:shape_prior(41:mean=values(25:bits:-9223372036854775808),45:precision=values(24:bits:4517329193108106637)))|admission=none|execution=execution(1:3,1:1)|route=15:trial-banded-ml|ml=some(316:trial_ml_evidence(27:criterion-form=J=E+sigma2*D,31:sigma2=bits:4591870180066957722,38:native-lambda=bits:4612811918334230528,57:raw-energy=prepared-sparse-residual-plus-centered-penalty,26:response=worker-owned-copy,47:conditional-sd=sqrt(diag(2*sigma2*inverse(HJ))),44:terminal-evidence=required-at-returned-shape))|exact-readout=true"
  val FixedGolden = "profile-fit/v3|dataset=797:dataset(16:identity|dataset,331:space(20:indices(1:1,1:1,1:1),43:matrix(1:4,1:4,24:fnv1a64:78704a9e6b24e2e5),164:some(154:scalafim-grid-scalafim-world:template:identity-golden-d3-1x1x1-3ff0000000000000-0-0-0-0-3ff0000000000000-0-0-0-0-3ff0000000000000-0-0-0-0-3ff0000000000000),48:some(39:scalafim-world:template:identity-golden),1:3,10:millimeter,3:ras,6:axes()),1:4,12:indices(1:0),161:frame(22:blockLens=indices(1:4),38:tr=values(24:bits:4607182418800017408),45:startTime=values(24:bits:4602678819172646912),34:precision=bits:4591870180066957722),8:events(),40:time_axis(26:block(1:0,5:run-1,1:0,1:4)),180:fields(61:field(6:number,42:number(24:bits:4627448617123184640,4:none)),71:field(6:source,52:number(25:bits:-9223372036854775808,12:some(4:-0.0))),29:field(4:text,12:text(4:x|=;))),4:none)|selected=selected(24:indices(1:0,1:1,1:2,1:3),12:indices(1:0))|drive=315:fixed(304:term(112:schedule(38:event(7:event_1,6:bits:0,6:bits:0,1:0),57:event(7:event_2,24:bits:4611686018427387904,6:bits:0,1:0)),72:events(61:categorical(9:condition,16:indices(1:0,1:0),13:labels(3:a|b))),4:none,4:none,13:source_rows(),24:labels(13:condition.a.b),43:matrix(1:2,1:1,24:fnv1a64:2be2cbea19a827c5)))|basis=282:kernel-basis/v2|family=15:identity-golden|chart=chart(50:axis(5:shape,6:bits:0,24:bits:4607182418800017408))|horizon=bits:4611686018427387904|step=bits:4607182418800017408|nodes=nodes(1:2)|derivatives=false|tolerance=bits:4591870180066957722|maxRank=1|heldOutPoints=1|rank=1|seed=11|basis-lags=matrix(1:1,1:3,24:fnv1a64:7fc5a13e70b0f698)|basis-values=matrix(1:3,1:1,24:fnv1a64:6d35889ab17dfbcc)|basis-coefficients=coefficient_jets(43:matrix(1:3,1:1,24:fnv1a64:6d35889ab17dfbcc),43:matrix(1:3,1:1,24:fnv1a64:6d35889ab17dfbcc))|nuisance=matrix(1:4,1:2,24:fnv1a64:633356b7623887fc)|config=906:fit_config(819:response-preparation/v1|records=records(123:record(26:step=missing_data(5:error),82:disposition=applied(58:dense input constructors reject non-finite response values)),70:record(31:step=censoring(12:timepoints()),24:disposition=disabled(0:)),73:record(34:step=volume_weights(10:disabled()),24:disposition=disabled(0:)),78:record(39:step=nuisance_projection(10:disabled()),24:disposition=disabled(0:)),217:record(177:step=whitening(157:ar_options(15:structure=iid(),12:iterations=1,12:global=false,15:voxelwise=false,15:exactFirst=true,31:censoredTimepoints=timepoints(),8:rho=none,8:phi=none)),24:disposition=disabled(0:)),170:record(130:step=robust_weights(105:robust_options(14:psi=disabled(),15:maxIterations=2,14:scaleScope=run,31:reestimateAutocorrelation=false)),24:disposition=disabled(0:)))|volumeWeighting=none,67:lss(4:none,24:bits:4427486594234968593,24:bits:4502148214488346440))|whitening=iid()|amplitudes=condition_means()|criterion=penalized_profile(24:bits:4607182418800017408)|grid=indices(1:2)|decode=268:decode-budget/v2|coarseStride=2|maxNewtonSteps=2|maxJets=2|maxExactEvaluations=6|weakSdLimit=values(24:bits:4627448617123184640)|ambiguityEnergy=bits:4591870180066957722|maxCandidateAttempts=4|stationarityStepTolerance=bits:4472406533629990549|initialization=bank-node|prior=some(106:shape_prior(41:mean=values(25:bits:-9223372036854775808),45:precision=values(24:bits:4517329193108106637)))|admission=some(867:observed-family-geometry/v2|basis=282:kernel-basis/v2|family=15:identity-golden|chart=chart(50:axis(5:shape,6:bits:0,24:bits:4607182418800017408))|horizon=bits:4611686018427387904|step=bits:4607182418800017408|nodes=nodes(1:2)|derivatives=false|tolerance=bits:4591870180066957722|maxRank=1|heldOutPoints=1|rank=1|seed=11|drives=schedule(38:event(7:event_1,6:bits:0,6:bits:0,1:0),57:event(7:event_2,24:bits:4611686018427387904,6:bits:0,1:0))|event-weights=matrix(1:2,1:1,24:fnv1a64:2be2cbea19a827c5)|conditions=labels(13:condition.a.b)|expanded=matrix(1:4,1:1,24:fnv1a64:36ece28ef707b20c)|frame=frame(22:blockLens=indices(1:4),38:tr=values(24:bits:4607182418800017408),45:startTime=values(24:bits:4602678819172646912),34:precision=bits:4591870180066957722)|precision=bits:4607182418800017408|whitening=none|nuisance=some(43:matrix(1:4,1:1,24:fnv1a64:d137d9e6997fe665)))|execution=execution(1:3,1:1)|route=19:fixed-condition-ols|ml=none|exact-readout=false"
