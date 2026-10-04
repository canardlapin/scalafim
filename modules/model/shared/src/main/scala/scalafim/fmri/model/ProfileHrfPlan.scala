package scalafim.fmri.model

import scalafim.dataset.FmriDataset
import scalafim.fmri.design.{ConditionId, TrialId}
import scalafim.fmri.design.baseline.BaselineModel
import scalafim.fmri.design.event.{ConvolvedTerm, EventSchedule}
import scalafim.fmri.design.hrf.{HrfKernelBasis, TrialMembership}

/** The event-side identity and one-hot membership needed by a trial profile
  * fit. This is deliberately not a lowered trial design.
  */
final class ProfileTrialDrive private (
    val schedule: EventSchedule,
    val membership: TrialMembership,
    val conditionLabels: Vector[ConditionId],
    val trialLabels: Vector[TrialId],
    val conditionForTrial: Vector[ConditionId]
)

object ProfileTrialDrive:
  def make(
      schedule: EventSchedule,
      membership: TrialMembership,
      conditionLabels: Vector[ConditionId],
      trialLabels: Vector[TrialId],
      conditionForTrial: Vector[ConditionId]
  ): Either[ProfileHrfPlanError, ProfileTrialDrive] =
    if conditionLabels.length != membership.conditionCount then
      Left(ProfileHrfPlanError.ConditionCountMismatch(membership.conditionCount, conditionLabels.length))
    else if conditionLabels.distinct.length != conditionLabels.length then
      Left(ProfileHrfPlanError.DuplicateConditionLabels(conditionLabels.map(_.value)))
    else if trialLabels.length != membership.trials then
      Left(ProfileHrfPlanError.TrialCountMismatch(membership.trials, trialLabels.length))
    else if trialLabels.distinct.length != trialLabels.length then
      Left(ProfileHrfPlanError.DuplicateTrialLabels(trialLabels.map(_.value)))
    else if schedule.size != membership.trials then
      Left(ProfileHrfPlanError.ScheduleTrialMismatch(membership.trials, schedule.size))
    else if conditionForTrial.length != membership.trials then
      Left(ProfileHrfPlanError.TrialConditionCountMismatch(membership.trials, conditionForTrial.length))
    else
      var trial = 0
      while trial < membership.trials do
        val expected = conditionLabels(membership.conditionOfTrial(trial))
        if conditionForTrial(trial) != expected then
          return Left(ProfileHrfPlanError.MembershipLabelMismatch(trial, expected, conditionForTrial(trial)))
        trial += 1
      Right(new ProfileTrialDrive(schedule, membership, conditionLabels, trialLabels, conditionForTrial))

enum ProfileHrfSource:
  case FixedCondition(plan: FitPlan, term: ConvolvedTerm)
  case TrialEvents(dataset: FmriDataset, drive: ProfileTrialDrive, baseline: BaselineModel, config: FitConfig)

enum ProfileHrfPlanError:
  case ConditionCountMismatch(expected: Int, actual: Int)
  case TrialCountMismatch(expected: Int, actual: Int)
  case TrialConditionCountMismatch(expected: Int, actual: Int)
  case ScheduleTrialMismatch(expected: Int, actual: Int)
  case DuplicateConditionLabels(labels: Vector[String])
  case DuplicateTrialLabels(labels: Vector[String])
  case MembershipLabelMismatch(trial: Int, expected: ConditionId, actual: ConditionId)
  case FixedSourceRequiresConditionMeans
  case FixedTermNotInPlan
  case FixedTermRowMismatch(expected: Int, actual: Int)
  case FixedTermKernelMismatch
  case TrialFrameMismatch(detail: String)
  case BaselineRowMismatch(expected: Int, actual: Int)
  case ScheduleBlockOutOfRange(block: Int, blocks: Int)
  case InvalidNoiseVariance(value: Double)
  case InvalidAlpha(value: Double)
  case InvalidLambda(value: Double)

  def message: String =
    this match
      case ConditionCountMismatch(expected, actual) => s"trial membership has $expected conditions but received $actual condition labels"
      case TrialCountMismatch(expected, actual) => s"trial membership has $expected trials but received $actual trial labels"
      case TrialConditionCountMismatch(expected, actual) => s"trial membership has $expected trials but received $actual trial condition labels"
      case ScheduleTrialMismatch(expected, actual) => s"trial membership has $expected trials but schedule has $actual events"
      case DuplicateConditionLabels(labels) => s"condition labels must be unique, got ${labels.mkString(", ")}"
      case DuplicateTrialLabels(labels) => s"trial labels must be unique, got ${labels.mkString(", ")}"
      case MembershipLabelMismatch(trial, expected, actual) => s"trial $trial belongs to condition '${expected.value}' by membership but is labelled '${actual.value}'"
      case FixedSourceRequiresConditionMeans => "a fixed condition source cannot request trial deviations"
      case FixedTermNotInPlan => "the fixed condition term is not part of the FitPlan event model"
      case FixedTermRowMismatch(expected, actual) => s"fixed condition term has $actual rows but plan has $expected timepoints"
      case FixedTermKernelMismatch => "the fixed condition term must use the exact kernel object owned by the profile basis"
      case TrialFrameMismatch(detail) => detail
      case BaselineRowMismatch(expected, actual) => s"baseline has $actual rows but dataset has $expected timepoints"
      case ScheduleBlockOutOfRange(block, blocks) => s"schedule block $block is outside the dataset frame's $blocks blocks"
      case InvalidNoiseVariance(value) => s"criterion noise variance must be finite and positive, got $value"
      case InvalidAlpha(value) => s"alpha must be finite and non-negative, got $value"
      case InvalidLambda(value) => s"reciprocal alpha lambda must be finite and positive, got $value"

/** A checked declaration for shape-varying profile-HRF fitting.
  *
  * It owns no expanded design: the fit layer decides whether a condition or
  * trial backend may lower this declaration for its observed response.
  */
final class ProfileHrfPlan private (
    val source: ProfileHrfSource,
    val basis: HrfKernelBasis,
    val amplitudes: AmplitudeStructure,
    val criterion: ProfileCriterion
)

object ProfileHrfPlan:
  def make(
      source: ProfileHrfSource,
      basis: HrfKernelBasis,
      amplitudes: AmplitudeStructure,
      criterion: ProfileCriterion
  ): Either[ProfileHrfPlanError, ProfileHrfPlan] =
    for
      _ <- validateCriterion(criterion)
      _ <- validateAmplitude(amplitudes)
      _ <- validateSource(source, basis, amplitudes)
    yield new ProfileHrfPlan(source, basis, amplitudes, criterion)

  /** Compatibility adapter for an existing fixed condition model. Observed
    * response admission remains the fit interpreter's responsibility.
    */
  def fromFixed(
      plan: FitPlan,
      term: ConvolvedTerm,
      basis: HrfKernelBasis,
      criterion: ProfileCriterion = ProfileCriterion.PenalizedProfile(1.0)
  ): Either[ProfileHrfPlanError, ProfileHrfPlan] =
    make(ProfileHrfSource.FixedCondition(plan, term), basis, AmplitudeStructure.ConditionMeans, criterion)

  /** Raw-alpha convenience for callers at the input boundary. Exact zero is
    * dispatched before any trial lowering and therefore selects condition
    * means without allocating a trial-sized design.
    */
  def fromTrialEvents(
      dataset: FmriDataset,
      drive: ProfileTrialDrive,
      baseline: BaselineModel,
      config: FitConfig,
      basis: HrfKernelBasis,
      alpha: Double,
      criterion: ProfileCriterion = ProfileCriterion.PenalizedProfile(1.0)
  ): Either[ProfileHrfPlanError, ProfileHrfPlan] =
    val amplitudes =
      if alpha == 0.0 then Right(AmplitudeStructure.ConditionMeans)
      else if !alpha.isFinite || alpha < 0.0 then Left(ProfileHrfPlanError.InvalidAlpha(alpha))
      else
        PositiveAlpha(alpha)
          .left
          .map(_ => ProfileHrfPlanError.InvalidAlpha(alpha))
          .map(AmplitudeStructure.ConditionCenteredTrials.apply)
    amplitudes.flatMap { value =>
      make(ProfileHrfSource.TrialEvents(dataset, drive, baseline, config), basis, value, criterion)
    }

  private def validateCriterion(criterion: ProfileCriterion): Either[ProfileHrfPlanError, Unit] =
    val variance = criterion.noiseVariance
    if variance.isFinite && variance > 0.0 then Right(())
    else Left(ProfileHrfPlanError.InvalidNoiseVariance(variance))

  private def validateAmplitude(amplitudes: AmplitudeStructure): Either[ProfileHrfPlanError, Unit] =
    amplitudes match
      case AmplitudeStructure.ConditionMeans => Right(())
      case AmplitudeStructure.ConditionCenteredTrials(alpha) =>
        val lambda = alpha.lambda
        if lambda.isFinite && lambda > 0.0 then Right(())
        else Left(ProfileHrfPlanError.InvalidLambda(lambda))

  private def validateSource(
      source: ProfileHrfSource,
      basis: HrfKernelBasis,
      amplitudes: AmplitudeStructure
  ): Either[ProfileHrfPlanError, Unit] =
    source match
      case ProfileHrfSource.FixedCondition(plan, term) =>
        if amplitudes.hasTrialDeviations then Left(ProfileHrfPlanError.FixedSourceRequiresConditionMeans)
        else if !plan.model.eventModel.terms.exists(_._2 == term) then Left(ProfileHrfPlanError.FixedTermNotInPlan)
        else if term.data.rows != plan.nTimepoints then Left(ProfileHrfPlanError.FixedTermRowMismatch(plan.nTimepoints, term.data.rows))
        else if !(term.hrf eq basis.kernel) then Left(ProfileHrfPlanError.FixedTermKernelMismatch)
        else Right(())
      case ProfileHrfSource.TrialEvents(dataset, drive, baseline, _) =>
        val rows = dataset.shape.timepoints
        if baseline.designMatrix.rows != rows then Left(ProfileHrfPlanError.BaselineRowMismatch(rows, baseline.designMatrix.rows))
        else if baseline.samplingFrame != dataset.samplingFrame then
          Left(ProfileHrfPlanError.TrialFrameMismatch("baseline sampling frame must equal the dataset sampling frame"))
        else
          val blocks = dataset.samplingFrame.blockLens.length
          drive.schedule.blockIds.find(block => block < 0 || block >= blocks) match
            case Some(block) => Left(ProfileHrfPlanError.ScheduleBlockOutOfRange(block, blocks))
            case None        => Right(())
