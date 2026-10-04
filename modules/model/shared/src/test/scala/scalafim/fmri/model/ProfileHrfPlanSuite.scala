package scalafim.fmri.model

import gale.linalg.Matrix
import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.{ConditionId, TrialId}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.{Event, EventModel, EventSchedule, EventTerm}
import scalafim.fmri.design.hrf.{HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.GaussianFamily
import scalafim.image.SampleSpaces

class ProfileHrfPlanSuite extends munit.FunSuite:
  private val frame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))
  private def compileBasis =
    HrfKernelBasis.compile(KernelBasisSpec(GaussianFamily.Default, PositiveSeconds.unsafe(Seconds(1.0)), Vector(2, 2), tolerance = 0.5, maxRank = 8, heldOutPoints = 1))
      .fold(error => fail(error.message), identity)
  private lazy val basis = compileBasis
  private lazy val independentlyCompiledBasis = compileBasis
  private lazy val baseline = BaselineModel.build(frame, BaselineBasis.Constant, intercept = Intercept.Global)
  private lazy val dataset: FmriDataset =
    FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("profile-plan"), Matrix.dense(4, 1, Seq(1.0, 2.0, 3.0, 4.0)), SampleSpaces(Vector(1, 1, 1))), frame).dataset

  private def drive(
      labels: Vector[ConditionId] = Vector(ConditionId.unsafe("face"), ConditionId.unsafe("house")),
      trialLabels: Vector[TrialId] = Vector(TrialId.unsafe("t1"), TrialId.unsafe("t2"), TrialId.unsafe("t3")),
      trialConditions: Vector[ConditionId] = Vector(ConditionId.unsafe("face"), ConditionId.unsafe("house"), ConditionId.unsafe("face"))
  ): Either[ProfileHrfPlanError, ProfileTrialDrive] =
    val schedule = EventSchedule.fromParts(Vector(0.0, 1.0, 2.0).map(Seconds.apply)).fold(error => fail(error.message), identity)
    val membership = TrialMembership.make(Vector(0, 1, 0), 2).fold(error => fail(error.message), identity)
    ProfileTrialDrive.make(schedule, membership, labels, trialLabels, trialConditions)

  private lazy val fixed: (FitPlan, scalafim.fmri.design.event.ConvolvedTerm) =
    val term = EventTerm(Vector(Event.factor(Vector("face", "house", "face"), "condition")), Vector(0.0, 1.0, 2.0).map(Seconds.apply)).convolve(basis.kernel, frame, precision = basis.spec.fineStep.seconds, dropEmpty = false)
    val events = EventModel(Vector("task" -> term), frame, term.data, term.columnNames, Vector(0 -> term.data.cols), Map("task" -> (0 until term.data.cols).toVector))
    (FitPlan(FmriModel(events, baseline, dataset)), term)

  test("trial plan retains exact ordered labels and alpha zero stays condition-only") {
    val sourceDrive = drive().fold(error => fail(error.message), identity)
    val plan = ProfileHrfPlan.fromTrialEvents(dataset, sourceDrive, baseline, FitConfig(), basis, alpha = 0.0).fold(error => fail(error.message), identity)
    assertEquals(sourceDrive.conditionLabels.map(_.value), Vector("face", "house"))
    assertEquals(sourceDrive.trialLabels.map(_.value), Vector("t1", "t2", "t3"))
    assertEquals(plan.amplitudes, AmplitudeStructure.ConditionMeans)
    assertEquals(plan.source, ProfileHrfSource.TrialEvents(dataset, sourceDrive, baseline, FitConfig()))
  }

  test("positive alpha retains the trial declaration without lowering a trial design") {
    val plan = ProfileHrfPlan.fromTrialEvents(dataset, drive().fold(error => fail(error.message), identity), baseline, FitConfig(), basis, alpha = 0.25).fold(error => fail(error.message), identity)
    assertEquals(plan.amplitudes, AmplitudeStructure.ConditionCenteredTrials(PositiveAlpha(0.25).toOption.get))
    assert(plan.source.isInstanceOf[ProfileHrfSource.TrialEvents])
  }

  test("trial ML intent is retained as a declaration for the future executor") {
    val plan = ProfileHrfPlan
      .fromTrialEvents(
        dataset,
        drive().fold(error => fail(error.message), identity),
        baseline,
        FitConfig(),
        basis,
        alpha = 0.25,
        criterion = ProfileCriterion.TrialRandomEffectsML(1.5)
      )
      .fold(error => fail(error.message), identity)
    assertEquals(plan.criterion, ProfileCriterion.TrialRandomEffectsML(1.5))
  }

  test("invalid trial identities, geometry, alpha, and noise are typed refusals") {
    assert(drive(labels = Vector(ConditionId.unsafe("face"), ConditionId.unsafe("face"))).left.toOption.exists(_.isInstanceOf[ProfileHrfPlanError.DuplicateConditionLabels]))
    assert(drive(trialConditions = Vector(ConditionId.unsafe("house"), ConditionId.unsafe("house"), ConditionId.unsafe("face"))).left.toOption.exists(_.isInstanceOf[ProfileHrfPlanError.MembershipLabelMismatch]))
    val sourceDrive = drive().fold(error => fail(error.message), identity)
    assert(ProfileHrfPlan.fromTrialEvents(dataset, sourceDrive, baseline, FitConfig(), basis, alpha = -1.0).left.toOption.exists(_.isInstanceOf[ProfileHrfPlanError.InvalidAlpha]))
    assert(ProfileHrfPlan.fromTrialEvents(dataset, sourceDrive, baseline, FitConfig(), basis, alpha = Double.NaN).left.toOption.exists(_.isInstanceOf[ProfileHrfPlanError.InvalidAlpha]))
    assert(ProfileHrfPlan.fromTrialEvents(dataset, sourceDrive, baseline, FitConfig(), basis, alpha = Double.MinPositiveValue).left.toOption.exists(_.isInstanceOf[ProfileHrfPlanError.InvalidLambda]))
    assert(ProfileHrfPlan.fromTrialEvents(dataset, sourceDrive, baseline, FitConfig(), basis, alpha = 0.25, criterion = ProfileCriterion.PenalizedProfile(0.0)).left.toOption.exists(_.isInstanceOf[ProfileHrfPlanError.InvalidNoiseVariance]))
    assert(ProfileHrfPlan.fromTrialEvents(dataset, sourceDrive, baseline, FitConfig(), basis, alpha = 0.25, criterion = ProfileCriterion.TrialRandomEffectsML(Double.PositiveInfinity)).left.toOption.exists(_.isInstanceOf[ProfileHrfPlanError.InvalidNoiseVariance]))
    val wrongBaseline = baseline.copy(designMatrix = scalafim.fmri.hrf.linalg.Mat.zeros(3, baseline.designMatrix.cols))
    assert(ProfileHrfPlan.fromTrialEvents(dataset, sourceDrive, wrongBaseline, FitConfig(), basis, alpha = 0.25).left.toOption.exists(_.isInstanceOf[ProfileHrfPlanError.BaselineRowMismatch]))
  }

  test("fixed condition compatibility is structural and cannot request trials") {
    val (fit, term) = fixed
    assert(ProfileHrfPlan.fromFixed(fit, term, basis).isRight)
    assertEquals(independentlyCompiledBasis.rank, basis.rank)
    assert(ProfileHrfPlan.fromFixed(fit, term, independentlyCompiledBasis).left.toOption.exists(_ == ProfileHrfPlanError.FixedTermKernelMismatch))
    assert(ProfileHrfPlan.make(ProfileHrfSource.FixedCondition(fit, term), basis, AmplitudeStructure.ConditionCenteredTrials(PositiveAlpha(0.25).toOption.get), ProfileCriterion.PenalizedProfile(1.0)).left.toOption.exists(_ == ProfileHrfPlanError.FixedSourceRequiresConditionMeans))
  }
