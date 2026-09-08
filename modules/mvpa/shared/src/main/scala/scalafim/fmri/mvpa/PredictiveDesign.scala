package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import resample4s.core.ContentDigest
import resample4s.core.Coverage
import resample4s.core.Plan
import resample4s.core.PlanCost
import resample4s.core.PlanDiagnostics
import resample4s.core.Seed
import resample4s.core.Selection
import resample4s.core.Split
import resample4s.core.UnitKey
import resample4s.designs.NestedFold

/** Inner validation roles from a Resample4s nested unit. The inner selections retain the source-sample codomain; the
  * parent bound schedule receipt covers the complete nested assignment and its derived seed.
  */
final class BoundNestedValidation[S <: SemanticSpace] private[mvpa] (
    val samples: AxisRef.Aux[SampleId, S],
    private val plan: Plan[Split[Selection], Coverage.ExactOnce],
    val seed: Seed,
    val diagnostics: PlanDiagnostics,
    val cost: PlanCost
):
  def shape = plan.shape
  def keys: IndexedSeq[UnitKey] = plan.keys

  def at(key: UnitKey): Either[BoundScheduleError, BoundSelectionSplit[SampleId, S]] =
    plan
      .at(key)
      .left
      .map(BoundScheduleError.UnknownUnit.apply)
      .flatMap: split =>
        ScheduleUnitBinder
          .selectionSplit[SampleId, S]
          .bind(samples, split)
          .left
          .map(BoundScheduleError.InvalidUnit(key, _))

  def iterator: Iterator[(UnitKey, Either[BoundScheduleError, BoundSelectionSplit[SampleId, S]])] =
    keys.iterator.map(key => key -> at(key))

final class BoundNestedFold[S <: SemanticSpace] private[mvpa] (
    val outer: BoundSelectionSplit[SampleId, S],
    val inner: BoundNestedValidation[S]
)

object BoundNestedFold:
  private[mvpa] def validate(
      value: NestedFold,
      populationSize: Int
  ): Either[ScheduleUnitError, Unit] =
    validateSplit(value.outer, populationSize) match
      case Left(error) => Left(error)
      case Right(_)    =>
        val iterator = value.inner.iterator
        var failure: Option[ScheduleUnitError] = None
        while iterator.hasNext && failure.isEmpty do
          val (_, split) = iterator.next()
          validateSplit(split, populationSize) match
            case Left(error) => failure = Some(error)
            case Right(_)    => ()
        failure.toLeft(())

  private[mvpa] def bind[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      value: NestedFold
  ): Either[ScheduleUnitError, BoundNestedFold[S]] =
    validate(value, samples.size).flatMap: _ =>
      ScheduleUnitBinder
        .selectionSplit[SampleId, S]
        .bind(samples, value.outer)
        .map: outer =>
          new BoundNestedFold(
            outer,
            new BoundNestedValidation(
              samples,
              value.inner,
              value.innerSeed,
              value.innerDiagnostics,
              value.innerCost
            )
          )

  private def validateSplit(
      value: Split[Selection],
      populationSize: Int
  ): Either[ScheduleUnitError, Unit] =
    if value.analysis.codomain != populationSize then
      Left(
        ScheduleUnitError.PopulationMismatch(
          "analysis selection",
          populationSize,
          value.analysis.codomain
        )
      )
    else if value.assessment.codomain != populationSize then
      Left(
        ScheduleUnitError.PopulationMismatch(
          "assessment selection",
          populationSize,
          value.assessment.codomain
        )
      )
    else Right(())

final case class GeneralizationAxis(
    name: ScientificAxisName,
    identity: AxisIdentity
):
  def reference: DesignAxisReference =
    DesignAxisReference(name, identity)

sealed trait CoverageSemantics[Cov <: Coverage]:
  def label: String

object CoverageSemantics:
  given ordinary: CoverageSemantics[Coverage] with
    override val label: String = "ordinary"

  given exact: CoverageSemantics[Coverage.Exact] with
    override val label: String = "exact-per-repeat"

  given exactOnce: CoverageSemantics[Coverage.ExactOnce] with
    override val label: String = "exact-once"

sealed trait ExactOnceEvidence[Cov <: Coverage]:
  private[mvpa] def reconstruct[S <: SemanticSpace, Unit](
      schedule: BoundSchedule[?, Cov, SampleId, S, Unit],
      roles: ValidationRoles[Unit, S]
  ): Either[PredictiveDesignError, OutOfFoldReconstruction[S]]

object ExactOnceEvidence:
  given ExactOnceEvidence[Coverage.ExactOnce] with
    override private[mvpa] def reconstruct[S <: SemanticSpace, Unit](
        schedule: BoundSchedule[?, Coverage.ExactOnce, SampleId, S, Unit],
        roles: ValidationRoles[Unit, S]
    ): Either[PredictiveDesignError, OutOfFoldReconstruction[S]] =
      ValidationDesign.reconstruct(schedule, roles)

trait ValidationRoles[Unit, S <: SemanticSpace]:
  def analysis(
      unit: Unit
  ): ReindexingLeg[S, SampleId, SampleId, Selection]

  def assessment(
      unit: Unit
  ): ReindexingLeg[S, SampleId, SampleId, Selection]

object ValidationRoles:
  given selectionSplit[S <: SemanticSpace]: ValidationRoles[
    BoundSelectionSplit[SampleId, S],
    S
  ] with
    override def analysis(
        unit: BoundSelectionSplit[SampleId, S]
    ): ReindexingLeg[S, SampleId, SampleId, Selection] =
      unit.analysis

    override def assessment(
        unit: BoundSelectionSplit[SampleId, S]
    ): ReindexingLeg[S, SampleId, SampleId, Selection] =
      unit.assessment

  given nestedFold[S <: SemanticSpace]: ValidationRoles[
    BoundNestedFold[S],
    S
  ] with
    override def analysis(
        unit: BoundNestedFold[S]
    ): ReindexingLeg[S, SampleId, SampleId, Selection] =
      unit.outer.analysis

    override def assessment(
        unit: BoundNestedFold[S]
    ): ReindexingLeg[S, SampleId, SampleId, Selection] =
      unit.outer.assessment

final case class AssessmentLocation(
    sample: SampleId,
    unit: UnitKey,
    assessmentPosition: Int
)

final class OutOfFoldReconstruction[S <: SemanticSpace] private[mvpa] (
    val samples: AxisRef.Aux[SampleId, S],
    val locations: Vector[AssessmentLocation]
):
  def location(sample: SampleId): Option[AssessmentLocation] =
    samples.positionOf(sample).map(locations)

enum PredictiveDesignError:
  case Identity(error: ScientificIdentityError)
  case ConflictingAxisReference(
      name: ScientificAxisName,
      first: AxisFingerprint,
      second: AxisFingerprint
  )
  case Schedule(error: BoundScheduleError)
  case DuplicateAssessment(
      sample: SampleId,
      first: AssessmentLocation,
      second: AssessmentLocation
  )
  case MissingAssessment(sample: SampleId)
  case InvalidAssessmentPosition(
      unit: UnitKey,
      error: AxisRefError
  )

  def message: String =
    this match
      case Identity(error)                               => error.message
      case ConflictingAxisReference(name, first, second) =>
        s"design axis '${name.value}' refers to both ${first.value} and ${second.value}"
      case Schedule(error)                            => error.message
      case DuplicateAssessment(sample, first, second) =>
        s"sample '${sample.value}' is assessed by both ${first.unit} and ${second.unit}"
      case MissingAssessment(sample) =>
        s"sample '${sample.value}' has no out-of-fold assessment"
      case InvalidAssessmentPosition(unit, error) =>
        s"assessment unit $unit has an invalid source position: ${error.message}"

/** Predictive assessment semantics. Coverage remains a type parameter: an ordinary or repeated-exact schedule cannot
  * call [[outOfFold]].
  */
sealed trait ValidationDesign[
    S <: SemanticSpace,
    Cov <: Coverage,
    Unit
] extends EvidenceDesign:
  type OrdinalUnit

  val schedule: BoundSchedule[OrdinalUnit, Cov, SampleId, S, Unit]
  private[mvpa] val roles: ValidationRoles[Unit, S]
  val sampleAxisName: ScientificAxisName
  val generalizesOver: GeneralizationAxis
  val coverage: CoverageSemantics[Cov]
  val identity: DesignIdentity
  val referencedAxes: Vector[DesignAxisReference]

  def at(key: UnitKey): Either[BoundScheduleError, Unit] =
    schedule.at(key)

  def outOfFold(using
      evidence: ExactOnceEvidence[Cov]
  ): Either[
    PredictiveDesignError,
    OutOfFoldReconstruction[S]
  ] =
    evidence.reconstruct(schedule, roles)

object ValidationDesign:
  def apply[
      A,
      Cov <: Coverage,
      S <: SemanticSpace,
      Unit
  ](
      schedule: BoundSchedule[A, Cov, SampleId, S, Unit],
      sampleAxisName: ScientificAxisName,
      generalizesOver: GeneralizationAxis
  )(using
      roles: ValidationRoles[Unit, S],
      coverage: CoverageSemantics[Cov]
  ): Either[
    PredictiveDesignError,
    ValidationDesign[S, Cov, Unit] { type OrdinalUnit = A }
  ] =
    for
      references <- designReferences(
        DesignAxisReference(sampleAxisName, schedule.axis.identity),
        generalizesOver.reference
      )
      identity <- DesignIdentity(
        DesignKind.unsafe("validation"),
        identityFields(schedule, coverage.label, generalizesOver)
      ).left.map(PredictiveDesignError.Identity.apply)
    yield
      val admittedSchedule = schedule
      val admittedRoles = roles
      val admittedSampleAxisName = sampleAxisName
      val admittedGeneralization = generalizesOver
      val admittedCoverage = coverage
      val admittedIdentity = identity
      new ValidationDesign[S, Cov, Unit]:
        override type OrdinalUnit = A

        override val schedule: BoundSchedule[
          A,
          Cov,
          SampleId,
          S,
          Unit
        ] = admittedSchedule
        override private[mvpa] val roles: ValidationRoles[Unit, S] =
          admittedRoles
        override val sampleAxisName: ScientificAxisName = admittedSampleAxisName
        override val generalizesOver: GeneralizationAxis = admittedGeneralization
        override val coverage: CoverageSemantics[Cov] = admittedCoverage
        override val identity: DesignIdentity = admittedIdentity
        override val referencedAxes: Vector[DesignAxisReference] = references

  private[mvpa] def reconstruct[
      S <: SemanticSpace,
      Unit
  ](
      schedule: BoundSchedule[?, Coverage.ExactOnce, SampleId, S, Unit],
      roles: ValidationRoles[Unit, S]
  ): Either[PredictiveDesignError, OutOfFoldReconstruction[S]] =
    val locations = Array.fill[Option[AssessmentLocation]](schedule.axis.size)(None)
    val iterator = schedule.iterator
    while iterator.hasNext do
      val (unitKey, bound) = iterator.next()
      bound match
        case Left(error) => return Left(PredictiveDesignError.Schedule(error))
        case Right(unit) =>
          val assessment = roles.assessment(unit)
          var assessmentPosition = 0
          while assessmentPosition < assessment.size do
            assessment.sourcePositionAt(assessmentPosition) match
              case Left(error) =>
                return Left(
                  PredictiveDesignError.InvalidAssessmentPosition(unitKey, error)
                )
              case Right(sourcePosition) =>
                val location = AssessmentLocation(
                  schedule.axis.keys(sourcePosition),
                  unitKey,
                  assessmentPosition
                )
                locations(sourcePosition) match
                  case Some(previous) =>
                    return Left(
                      PredictiveDesignError.DuplicateAssessment(
                        location.sample,
                        previous,
                        location
                      )
                    )
                  case None => locations(sourcePosition) = Some(location)
            assessmentPosition += 1

    val complete = Vector.newBuilder[AssessmentLocation]
    var sourcePosition = 0
    while sourcePosition < locations.length do
      locations(sourcePosition) match
        case Some(location) => complete += location
        case None           =>
          return Left(
            PredictiveDesignError.MissingAssessment(
              schedule.axis.keys(sourcePosition)
            )
          )
      sourcePosition += 1
    Right(new OutOfFoldReconstruction(schedule.axis, complete.result()))

  private def designReferences(
      samples: DesignAxisReference,
      generalization: DesignAxisReference
  ): Either[PredictiveDesignError, Vector[DesignAxisReference]] =
    if samples.name != generalization.name then Right(Vector(samples, generalization).sortBy(_.name.value))
    else if samples.identity == generalization.identity then Right(Vector(samples))
    else
      Left(
        PredictiveDesignError.ConflictingAxisReference(
          samples.name,
          samples.identity.fingerprint,
          generalization.identity.fingerprint
        )
      )

  private def identityFields[
      A,
      Cov <: Coverage,
      S <: SemanticSpace,
      Unit
  ](
      schedule: BoundSchedule[A, Cov, SampleId, S, Unit],
      coverage: String,
      generalization: GeneralizationAxis
  ): Vector[(String, String)] =
    Vector(
      "assignment" -> ResampleScientificIdentity.digest(schedule.receipt.assignment),
      "coverage" -> coverage,
      "design" -> ResampleScientificIdentity.digest(schedule.receipt.design),
      "generalization-axis" -> generalization.name.value,
      "generalization-space" -> generalization.identity.fingerprint.value,
      "labels" -> schedule.receipt.labels
        .map(ResampleScientificIdentity.digest)
        .getOrElse("none"),
      "seed" -> schedule.receipt.seed.value.toString,
      "seed-domain" -> schedule.seedAuthority.domain.value
    )

enum PreparationScope:
  case TargetBlindAnalysis
  case TargetAwareInnerCrossFit

  def label: String =
    this match
      case TargetBlindAnalysis      => "target-blind-analysis"
      case TargetAwareInnerCrossFit => "target-aware-inner-cross-fit"

enum FittingScope:
  case OuterAnalysis
  case InnerAnalysisThenOuterRefit

  def label: String =
    this match
      case OuterAnalysis               => "outer-analysis"
      case InnerAnalysisThenOuterRefit => "inner-analysis-then-outer-refit"

/** A leakage-safety capability distinct from coverage. Its constructor is not public: target-blind preparation may use
  * any exact-once validation design, while target-aware preparation requires an explicit nested unit type.
  */
final class CrossFitDesign[S <: SemanticSpace, Unit] private (
    val validation: ValidationDesign[S, Coverage.ExactOnce, Unit],
    val preparation: PreparationScope,
    val fitting: FittingScope,
    val identity: DesignIdentity,
    val referencedAxes: Vector[DesignAxisReference]
) extends EvidenceDesign:
  def outOfFold: Either[PredictiveDesignError, OutOfFoldReconstruction[S]] =
    validation.outOfFold

object CrossFitDesign:
  def targetBlind[S <: SemanticSpace, Unit](
      validation: ValidationDesign[S, Coverage.ExactOnce, Unit]
  ): Either[PredictiveDesignError, CrossFitDesign[S, Unit]] =
    build(
      validation,
      PreparationScope.TargetBlindAnalysis,
      FittingScope.OuterAnalysis
    )

  def targetAware[S <: SemanticSpace](
      validation: ValidationDesign[
        S,
        Coverage.ExactOnce,
        BoundNestedFold[S]
      ]
  ): Either[PredictiveDesignError, CrossFitDesign[S, BoundNestedFold[S]]] =
    build(
      validation,
      PreparationScope.TargetAwareInnerCrossFit,
      FittingScope.InnerAnalysisThenOuterRefit
    )

  private def build[S <: SemanticSpace, Unit](
      validation: ValidationDesign[S, Coverage.ExactOnce, Unit],
      preparation: PreparationScope,
      fitting: FittingScope
  ): Either[PredictiveDesignError, CrossFitDesign[S, Unit]] =
    DesignIdentity(
      DesignKind.unsafe("cross-fit"),
      Vector(
        "fitting-scope" -> fitting.label,
        "preparation-scope" -> preparation.label,
        "validation" -> validation.identity.fingerprint.value
      )
    ).left
      .map(PredictiveDesignError.Identity.apply)
      .map: identity =>
        new CrossFitDesign(
          validation,
          preparation,
          fitting,
          identity,
          validation.referencedAxes
        )

private[mvpa] object ResampleScientificIdentity:
  def digest(value: ContentDigest): String =
    val hex = value.value.toIArray.iterator
      .map(byte => f"${byte.toInt & 0xff}%02x")
      .mkString
    s"${value.algorithm.value}:$hex"
