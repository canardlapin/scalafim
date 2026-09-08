package scalafim.fmri.mvpa.predictive

import multivar.core.SemanticSpace
import resample4s.core.Coverage
import resample4s.core.Selection
import resample4s.core.Split
import scalafim.fmri.mvpa.*

type ExactClassificationValidation[S <: SemanticSpace] = ValidationDesign[
  S,
  Coverage.ExactOnce,
  BoundSelectionSplit[SampleId, S]
] { type OrdinalUnit = Split[Selection] }

enum CategoricalObservationSourceError:
  case Identity(error: ScientificIdentityError)
  case Observations(error: ObservationsError)
  case SampleAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case SampleWitnessMismatch

  def message: String =
    this match
      case Identity(error)                      => error.message
      case Observations(error)                  => error.message
      case SampleAxisMismatch(expected, actual) =>
        s"categorical target sample axis ${actual.value} does not match evidence ${expected.value}"
      case SampleWitnessMismatch =>
        "categorical target and evidence use different nominal sample witnesses"

/** Observation evidence and its typed categorical target. Value identities participate in the scientific source
  * identity, while dense versus matrix-free representation remains execution evidence only.
  */
final class CategoricalObservationSource[
    Samples <: SemanticSpace,
    NeuralSpace <: SemanticSpace,
    NeuralCoordinate
] private (
    val observations: Observations[Samples, NeuralSpace, NeuralCoordinate],
    val target: CategoricalTarget[Samples],
    val classAxisName: ScientificAxisName,
    val identity: ScientificSourceIdentity
) extends ScientificSource:
  override type Neural = NeuralSpace
  override type NeuralKey = NeuralCoordinate

  def evidence: EvidenceTable[Samples, NeuralSpace, SampleId, NeuralCoordinate] =
    observations.evidence

  def sampleAxisName: ScientificAxisName = observations.sampleAxisName
  def neuralAxisName: ScientificAxisName = observations.neuralAxisName

  override def neuralAxis: AxisRef.Aux[NeuralCoordinate, NeuralSpace] =
    evidence.columns

  def classify[
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      configuration: ClassificationConfiguration[
        LearnerConfiguration,
        Fitted,
        LearnerError,
        Prediction
      ]
  ): CategoricalClassification[
    Samples,
    NeuralSpace,
    NeuralCoordinate,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction
  ] =
    new CategoricalClassification(configuration)

object CategoricalObservationSource:
  private val Protocol = "scalafim-mvpa-categorical-observations/v1"

  def apply[
      S <: SemanticSpace,
      N <: SemanticSpace,
      K
  ](
      evidence: EvidenceTable[S, N, SampleId, K],
      target: CategoricalTarget[S],
      sampleAxisName: ScientificAxisName = ScientificAxisName.unsafe("samples"),
      neuralAxisName: ScientificAxisName = ScientificAxisName.unsafe("neural"),
      classAxisName: ScientificAxisName = ScientificAxisName.unsafe("classes")
  ): Either[
    CategoricalObservationSourceError,
    CategoricalObservationSource[S, N, K]
  ] =
    Observations(evidence, sampleAxisName, neuralAxisName).left
      .map(CategoricalObservationSourceError.Observations.apply)
      .flatMap(observations => fromObservations(observations, target, classAxisName))

  def fromObservations[
      S <: SemanticSpace,
      N <: SemanticSpace,
      K
  ](
      observations: Observations[S, N, K],
      target: CategoricalTarget[S],
      classAxisName: ScientificAxisName = ScientificAxisName.unsafe("classes")
  ): Either[
    CategoricalObservationSourceError,
    CategoricalObservationSource[S, N, K]
  ] =
    if observations.samples.identity != target.samples.identity then
      Left(
        CategoricalObservationSourceError.SampleAxisMismatch(
          observations.samples.identity.fingerprint,
          target.samples.identity.fingerprint
        )
      )
    else if !(observations.samples.evidence eq target.samples.evidence) then
      Left(CategoricalObservationSourceError.SampleWitnessMismatch)
    else
      val targetFingerprint = targetDigest(target)
      ScientificSourceIdentity(
        ScientificSourceKind.unsafe("categorical-observations"),
        Vector(
          ScientificSourceAxis(observations.sampleAxisName, observations.samples.identity),
          ScientificSourceAxis(observations.neuralAxisName, observations.neuralAxis.identity),
          ScientificSourceAxis(classAxisName, target.classes.identity)
        ),
        Vector(
          "observations" -> observations.identity.fingerprint.value,
          "protocol" -> Protocol,
          "target" -> targetFingerprint
        )
      ).left
        .map(CategoricalObservationSourceError.Identity.apply)
        .map: identity =>
          new CategoricalObservationSource(
            observations,
            target,
            classAxisName,
            identity
          )

  private def targetDigest[S <: SemanticSpace](
      target: CategoricalTarget[S]
  ): String =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.string(target.samples.identity.fingerprint.value)
    writer.string(target.classes.identity.fingerprint.value)
    writer.int(target.labels.size)
    var position = 0
    while position < target.labels.size do
      writer.string(target.labels.values(position).value)
      position += 1
    AxisDigest.sha256Hex(writer.result())

enum ClassificationBindRejection:
  case Target(error: CategoricalError)
  case UnsupportedAdaptation(adaptation: ClassifierAdaptation)
  case MeasurementTooSmall(
      measurement: MeasurementId,
      required: Int,
      actual: Int
  )

  def message: String =
    this match
      case Target(error)                     => error.message
      case UnsupportedAdaptation(adaptation) =>
        s"within-domain validation requires inductive-within-domain adaptation, obtained ${adaptation.identity}"
      case MeasurementTooSmall(measurement, required, actual) =>
        s"measurement '${measurement.value}' has $actual features; this classifier requires at least $required"

/** Open predictive estimand whose result retains exact source-keyed OOF predictions. The design and source parameters
  * encode the only compatible scientific evidence types; no analysis registry or runtime kind switch is involved.
  */
final class CategoricalClassification[
    S <: SemanticSpace,
    N <: SemanticSpace,
    K,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction <: CategoricalLearnerPrediction
] private[predictive] (
    val configuration: ClassificationConfiguration[
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ]
) extends Estimand[
      CategoricalObservationSource[S, N, K],
      ExactClassificationValidation[S]
    ]:
  override type Result = OutOfFoldClassification[
    S,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction
  ]
  override type Rejection = ClassificationBindRejection
  override type Failure = ClassificationCompileError[LearnerError]

  override def identity: EstimandIdentity =
    configuration.identity

  override val defaultBoundaries: RequestedBoundaries =
    CategoricalClassification.Boundaries

  override def rejectionMessage(value: ClassificationBindRejection): String =
    value.message

  override def failureMessage(
      value: ClassificationCompileError[LearnerError]
  ): String =
    value.message(configuration.compiler.failureMessage)

object CategoricalClassification:
  private val Boundaries =
    RequestedBoundaries.trusted(
      Vector(
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("out-of-fold-predictions")
        ),
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("decision-scores")
        ),
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("classification-summaries")
        )
      )
    )

final class ClassificationPrepared private[predictive] (
    val samples: AxisIdentity,
    val classes: AxisIdentity,
    val design: DesignIdentity
)

object PredictiveAnalysis:
  given compiler[
      S <: SemanticSpace,
      N <: SemanticSpace,
      K,
      R,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ]: Compile[
    CategoricalObservationSource[S, N, K],
    ExactClassificationValidation[S],
    CategoricalClassification[
      S,
      N,
      K,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ],
    R
  ] with
    override type Prepared = ClassificationPrepared

    override def prepare(
        specification: ScientificSpecification[
          CategoricalObservationSource[S, N, K],
          ExactClassificationValidation[S],
          CategoricalClassification[
            S,
            N,
            K,
            LearnerConfiguration,
            Fitted,
            LearnerError,
            Prediction
          ],
          R
        ]
    ): Either[specification.Rejection, ClassificationPrepared] =
      if specification.estimand.configuration.adaptation !=
          ClassifierAdaptation.InductiveWithinDomain
      then
        Left(
          ClassificationBindRejection.UnsupportedAdaptation(
            specification.estimand.configuration.adaptation
          )
        )
      else
        specification.frame.entries.find(
          _.measurement.local.size < specification.estimand.configuration.definition.minimumFeatures.toInt
        ) match
          case Some(entry) =>
            Left(
              ClassificationBindRejection.MeasurementTooSmall(
                entry.measurement.identity.id,
                specification.estimand.configuration.definition.minimumFeatures.toInt,
                entry.measurement.local.size
              )
            )
          case None =>
            CategoricalTarget
              .validateTrainingCoverage(
                specification.source.target,
                specification.design
              )
              .left
              .map(ClassificationBindRejection.Target.apply)
              .map: _ =>
                new ClassificationPrepared(
                  specification.source.target.samples.identity,
                  specification.source.target.classes.identity,
                  specification.design.identity
                )

  given task[
      S <: SemanticSpace,
      N <: SemanticSpace,
      K,
      R,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ]: MeasurementTask[
    CategoricalObservationSource[S, N, K],
    ExactClassificationValidation[S],
    CategoricalClassification[
      S,
      N,
      K,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ],
    R,
    ClassificationPrepared
  ] with
    override def validate(
        strategy: ExecutionStrategy
    ): Either[ExecutionPlanError, Unit] =
      if strategy.representation != ExecutionRepresentation.Dense then
        Left(
          ExecutionPlanError.UnsupportedRepresentation(
            strategy.representation,
            Vector(ExecutionRepresentation.Dense)
          )
        )
      else if strategy.precision != NumericPrecision.Binary64 then
        Left(
          ExecutionPlanError.UnsupportedPrecision(
            strategy.precision,
            Vector(NumericPrecision.Binary64)
          )
        )
      else if strategy.solver != SolverChoice.NotApplicable then
        Left(ExecutionPlanError.UnsupportedSolver(strategy.solver))
      else
        strategy.materialization match
          case MaterializationPolicy.Reject =>
            Left(
              ExecutionPlanError.MaterializationRequired(
                "Alder learners consume one budgeted local dense pattern table"
              )
            )
          case MaterializationPolicy.Allow(_) => Right(())

    override def execute(
        plan: BoundScientificPlan[
          CategoricalObservationSource[S, N, K],
          ExactClassificationValidation[S],
          CategoricalClassification[
            S,
            N,
            K,
            LearnerConfiguration,
            Fitted,
            LearnerError,
            Prediction
          ],
          R,
          ClassificationPrepared
        ]
    )(
        measurement: MeasurementEntry[
          plan.specification.source.Neural,
          plan.specification.source.NeuralKey,
          ?,
          R
        ],
        context: TaskContext
    ): Either[
      TaskReportError,
      TaskReport[plan.Result, plan.Rejection, plan.Failure]
    ] =
      ClassificationCompiler.run(
        plan.identity.fingerprint,
        plan.specification.source.evidence,
        plan.specification.source.target,
        plan.specification.design,
        measurement.measurement,
        context.strategy.materialization,
        plan.specification.estimand.configuration
      ) match
        case Left(error) =>
          TaskReport.failed(error, context.strategy.target)
        case Right(result) =>
          ExecutionMaterialization(
            ExecutionScope.Measurement(measurement.measurement.identity.id),
            result.preparation.materialization,
            "Alder prediction requires explicit local dense rows"
          ) match
            case Left(error) =>
              TaskReport.failed(
                ClassificationCompileError.ExecutionEvidence(error),
                context.strategy.target,
                operatorApplications = 1L
              )
            case Right(materialization) =>
              TaskReport.success(
                result,
                context.strategy.target,
                operatorApplications = 1L,
                materializations = Vector(materialization)
              )
