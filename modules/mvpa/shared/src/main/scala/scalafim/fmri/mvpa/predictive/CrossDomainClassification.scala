package scalafim.fmri.mvpa.predictive

import gale.linalg.DMat
import multivar.core.SemanticSpace
import scalafim.fmri.mvpa.*

opaque type CrossDomainGeneralizationAxis = String

object CrossDomainGeneralizationAxis:
  def apply(value: String): Either[ScientificIdentityError, CrossDomainGeneralizationAxis] =
    ScientificIdentityText.lowerIdentifier("generalization axis", value)

  private[mvpa] def unsafe(value: String): CrossDomainGeneralizationAxis =
    value

  extension (axis: CrossDomainGeneralizationAxis) inline def value: String = axis

enum CrossDomainSourceError:
  case Identity(error: ScientificIdentityError)
  case NeuralAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case NeuralWitnessMismatch
  case ClassAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case ClassWitnessMismatch

  def message: String =
    this match
      case Identity(error)                      => error.message
      case NeuralAxisMismatch(expected, actual) =>
        s"target-domain neural axis ${actual.value} does not match source-domain correspondence axis ${expected.value}"
      case NeuralWitnessMismatch =>
        "source and target domains use different nominal witnesses for their correspondence axis"
      case ClassAxisMismatch(expected, actual) =>
        s"target-domain class axis ${actual.value} does not match source-domain class axis ${expected.value}"
      case ClassWitnessMismatch =>
        "source and target domains use different nominal class witnesses"

/** Two categorical observation domains already expressed in one exact neural correspondence space. Coordinate alignment
  * is therefore evidence construction, not an analysis-time paired-feature convention.
  */
final class CrossDomainObservationSource[
    SourceSamples <: SemanticSpace,
    TargetSamples <: SemanticSpace,
    NeuralSpace <: SemanticSpace,
    Coordinate
] private (
    val source: CategoricalObservationSource[SourceSamples, NeuralSpace, Coordinate],
    val target: CategoricalObservationSource[TargetSamples, NeuralSpace, Coordinate],
    val sourceSampleAxisName: ScientificAxisName,
    val targetSampleAxisName: ScientificAxisName,
    val correspondenceAxisName: ScientificAxisName,
    val classAxisName: ScientificAxisName,
    val identity: ScientificSourceIdentity
) extends ScientificSource:
  override type Neural = NeuralSpace
  override type NeuralKey = Coordinate

  override def neuralAxisName: ScientificAxisName = correspondenceAxisName

  override def neuralAxis: AxisRef.Aux[NeuralKey, Neural] =
    source.neuralAxis

  def generalize[
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
  ): InductiveCrossDomainClassification[
    SourceSamples,
    TargetSamples,
    Neural,
    NeuralKey,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction
  ] =
    new InductiveCrossDomainClassification(configuration)

object CrossDomainObservationSource:
  private val Protocol = "scalafim-mvpa-cross-domain-observations/v1"

  def apply[
      S <: SemanticSpace,
      T <: SemanticSpace,
      N <: SemanticSpace,
      K
  ](
      source: CategoricalObservationSource[S, N, K],
      target: CategoricalObservationSource[T, N, K],
      sourceSampleAxisName: ScientificAxisName = ScientificAxisName.unsafe("source-samples"),
      targetSampleAxisName: ScientificAxisName = ScientificAxisName.unsafe("target-samples"),
      correspondenceAxisName: ScientificAxisName = ScientificAxisName.unsafe("neural"),
      classAxisName: ScientificAxisName = ScientificAxisName.unsafe("classes")
  ): Either[CrossDomainSourceError, CrossDomainObservationSource[S, T, N, K]] =
    if source.neuralAxis.identity != target.neuralAxis.identity then
      Left(
        CrossDomainSourceError.NeuralAxisMismatch(
          source.neuralAxis.identity.fingerprint,
          target.neuralAxis.identity.fingerprint
        )
      )
    else if !(source.neuralAxis.evidence eq target.neuralAxis.evidence) then
      Left(CrossDomainSourceError.NeuralWitnessMismatch)
    else if source.target.classes.identity != target.target.classes.identity then
      Left(
        CrossDomainSourceError.ClassAxisMismatch(
          source.target.classes.identity.fingerprint,
          target.target.classes.identity.fingerprint
        )
      )
    else if !(source.target.classes.evidence eq target.target.classes.evidence) then
      Left(CrossDomainSourceError.ClassWitnessMismatch)
    else
      ScientificSourceIdentity(
        ScientificSourceKind.unsafe("cross-domain-observations"),
        Vector(
          ScientificSourceAxis(sourceSampleAxisName, source.target.samples.identity),
          ScientificSourceAxis(targetSampleAxisName, target.target.samples.identity),
          ScientificSourceAxis(correspondenceAxisName, source.neuralAxis.identity),
          ScientificSourceAxis(classAxisName, source.target.classes.identity)
        ),
        Vector(
          "protocol" -> Protocol,
          "source-domain" -> source.identity.fingerprint.value,
          "target-domain" -> target.identity.fingerprint.value
        )
      ).left
        .map(CrossDomainSourceError.Identity.apply)
        .map: identity =>
          new CrossDomainObservationSource(
            source,
            target,
            sourceSampleAxisName,
            targetSampleAxisName,
            correspondenceAxisName,
            classAxisName,
            identity
          )

enum CrossDomainDesignError:
  case Identity(error: ScientificIdentityError)

  def message: String =
    this match
      case Identity(error) => error.message

/** A predeclared source-fit/target-assessment relation. It is intentionally not a validation fold: the domains, fit
  * scope, assessment scope, and generalization axis are scientific semantics.
  */
final class CrossDomainAssessmentDesign[
    SourceSamples <: SemanticSpace,
    TargetSamples <: SemanticSpace
] private (
    val sourceSamples: AxisRef.Aux[SampleId, SourceSamples],
    val targetSamples: AxisRef.Aux[SampleId, TargetSamples],
    val sourceSampleAxisName: ScientificAxisName,
    val targetSampleAxisName: ScientificAxisName,
    val generalizesOver: CrossDomainGeneralizationAxis,
    val identity: DesignIdentity,
    val referencedAxes: Vector[DesignAxisReference]
) extends EvidenceDesign

object CrossDomainAssessmentDesign:
  private val Protocol = "scalafim-mvpa-cross-domain-assessment/v1"

  def apply[
      S <: SemanticSpace,
      T <: SemanticSpace,
      N <: SemanticSpace,
      K
  ](
      source: CrossDomainObservationSource[S, T, N, K],
      generalizesOver: CrossDomainGeneralizationAxis
  ): Either[CrossDomainDesignError, CrossDomainAssessmentDesign[S, T]] =
    DesignIdentity(
      DesignKind.unsafe("cross-domain-assessment"),
      Vector(
        "assessment-samples" -> source.target.target.samples.identity.fingerprint.value,
        "assessment-scope" -> "target-domain-only",
        "fit-samples" -> source.source.target.samples.identity.fingerprint.value,
        "fit-scope" -> "source-domain-only",
        "generalizes-over" -> generalizesOver.value,
        "protocol" -> Protocol
      )
    ).left
      .map(CrossDomainDesignError.Identity.apply)
      .map: identity =>
        new CrossDomainAssessmentDesign(
          source.source.target.samples,
          source.target.target.samples,
          source.sourceSampleAxisName,
          source.targetSampleAxisName,
          generalizesOver,
          identity,
          Vector(
            DesignAxisReference(
              source.sourceSampleAxisName,
              source.source.target.samples.identity
            ),
            DesignAxisReference(
              source.targetSampleAxisName,
              source.target.target.samples.identity
            )
          )
        )

enum CrossDomainBindRejection:
  case UnsupportedAdaptation(adaptation: ClassifierAdaptation)
  case MeasurementTooSmall(measurement: MeasurementId, required: Int, actual: Int)

  def message: String =
    this match
      case UnsupportedAdaptation(adaptation) =>
        s"cross-domain assessment requires inductive-frozen-source-domain adaptation, obtained ${adaptation.identity}"
      case MeasurementTooSmall(measurement, required, actual) =>
        s"measurement '${measurement.value}' has $actual features; this classifier requires at least $required"

final class CrossDomainFitReceipt private[predictive] (
    val sourceSamples: AxisIdentity,
    val targetSamples: AxisIdentity,
    val classes: AxisIdentity,
    val adaptation: ClassifierAdaptation,
    val classifier: EstimandIdentity,
    val sourceMaterialization: MaterializationReceipt,
    val targetMaterialization: MaterializationReceipt
)

final class CrossDomainClassificationEstimate[
    T <: SemanticSpace,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction <: CategoricalLearnerPrediction
] private[predictive] (
    val predictions: CategoricalPredictions[T],
    val learnerPredictions: Vector[Prediction],
    val accuracy: AccuracyEstimate,
    val balancedAccuracy: BalancedAccuracyEstimate,
    val confusionMatrix: ConfusionMatrixEstimate,
    val receipt: CrossDomainFitReceipt,
    val configuration: ClassificationConfiguration[
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ]
)

enum CrossDomainCompileError[+LearnerError]:
  case Evidence(error: EvidenceTableError)
  case Learner(error: LearnerError)
  case Column(error: ColumnError)
  case Categorical(error: CategoricalError)
  case ScoreClassOrder(expected: Vector[ClassId], actual: Vector[ClassId])
  case ExecutionEvidence(error: ExecutionReceiptError)

  def message(renderLearnerError: LearnerError => String): String =
    this match
      case Evidence(error)                   => error.message
      case Learner(error)                    => renderLearnerError(error)
      case Column(error)                     => error.message
      case Categorical(error)                => error.message
      case ScoreClassOrder(expected, actual) =>
        s"classifier score order ${actual.map(_.value).mkString("[", ",", "]")} does not match class axis ${expected.map(_.value).mkString("[", ",", "]")}"
      case ExecutionEvidence(error) => error.message

final class InductiveCrossDomainClassification[
    S <: SemanticSpace,
    T <: SemanticSpace,
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
      CrossDomainObservationSource[S, T, N, K],
      CrossDomainAssessmentDesign[S, T]
    ]:
  override type Result = CrossDomainClassificationEstimate[
    T,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction
  ]
  override type Rejection = CrossDomainBindRejection
  override type Failure = CrossDomainCompileError[LearnerError]

  override def identity: EstimandIdentity = configuration.identity

  override val defaultBoundaries: RequestedBoundaries =
    InductiveCrossDomainClassification.Boundaries

  override def rejectionMessage(value: CrossDomainBindRejection): String =
    value.message

  override def failureMessage(value: CrossDomainCompileError[LearnerError]): String =
    value.message(configuration.compiler.failureMessage)

object InductiveCrossDomainClassification:
  private val Boundaries =
    RequestedBoundaries.trusted(
      Vector(
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("target-domain-predictions")
        ),
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("decision-scores")
        ),
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("classification-summaries")
        ),
        OutputBoundaryIdentity.trusted(
          OutputBoundaryId.unsafe("source-fit-receipt")
        )
      )
    )

final class CrossDomainPrepared private[predictive] (
    val sourceSamples: AxisIdentity,
    val targetSamples: AxisIdentity,
    val classes: AxisIdentity,
    val design: DesignIdentity
)

object CrossDomainPredictiveAnalysis:
  given compiler[
      S <: SemanticSpace,
      T <: SemanticSpace,
      N <: SemanticSpace,
      K,
      R,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ]: Compile[
    CrossDomainObservationSource[S, T, N, K],
    CrossDomainAssessmentDesign[S, T],
    InductiveCrossDomainClassification[
      S,
      T,
      N,
      K,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ],
    R
  ] with
    override type Prepared = CrossDomainPrepared

    override def prepare(
        specification: ScientificSpecification[
          CrossDomainObservationSource[S, T, N, K],
          CrossDomainAssessmentDesign[S, T],
          InductiveCrossDomainClassification[
            S,
            T,
            N,
            K,
            LearnerConfiguration,
            Fitted,
            LearnerError,
            Prediction
          ],
          R
        ]
    ): Either[specification.Rejection, CrossDomainPrepared] =
      val configuration = specification.estimand.configuration
      if configuration.adaptation != ClassifierAdaptation.InductiveFrozenSourceDomain then
        Left(CrossDomainBindRejection.UnsupportedAdaptation(configuration.adaptation))
      else
        specification.frame.entries.find(
          _.measurement.local.size < configuration.definition.minimumFeatures.toInt
        ) match
          case Some(entry) =>
            Left(
              CrossDomainBindRejection.MeasurementTooSmall(
                entry.measurement.identity.id,
                configuration.definition.minimumFeatures.toInt,
                entry.measurement.local.size
              )
            )
          case None =>
            Right(
              new CrossDomainPrepared(
                specification.source.source.target.samples.identity,
                specification.source.target.target.samples.identity,
                specification.source.source.target.classes.identity,
                specification.design.identity
              )
            )

  given task[
      S <: SemanticSpace,
      T <: SemanticSpace,
      N <: SemanticSpace,
      K,
      R,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ]: MeasurementTask[
    CrossDomainObservationSource[S, T, N, K],
    CrossDomainAssessmentDesign[S, T],
    InductiveCrossDomainClassification[
      S,
      T,
      N,
      K,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ],
    R,
    CrossDomainPrepared
  ] with
    override def validate(strategy: ExecutionStrategy): Either[ExecutionPlanError, Unit] =
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
                "cross-domain classification requires two budgeted local dense tables"
              )
            )
          case MaterializationPolicy.Allow(_) => Right(())

    override def execute(
        plan: BoundScientificPlan[
          CrossDomainObservationSource[S, T, N, K],
          CrossDomainAssessmentDesign[S, T],
          InductiveCrossDomainClassification[
            S,
            T,
            N,
            K,
            LearnerConfiguration,
            Fitted,
            LearnerError,
            Prediction
          ],
          R,
          CrossDomainPrepared
        ]
    )(
        measurement: MeasurementEntry[
          plan.specification.source.Neural,
          plan.specification.source.NeuralKey,
          ?,
          R
        ],
        context: TaskContext
    ): Either[TaskReportError, TaskReport[plan.Result, plan.Rejection, plan.Failure]] =
      CrossDomainClassificationCompiler.run(
        plan.specification.source,
        measurement.measurement,
        context.strategy.materialization,
        plan.specification.estimand.configuration
      ) match
        case Left(error) =>
          TaskReport.failed(error, context.strategy.target, operatorApplications = 2L)
        case Right(result) =>
          val receipts = Vector(
            result.receipt.sourceMaterialization -> "source-domain classifier fit",
            result.receipt.targetMaterialization -> "target-domain classifier assessment"
          )
          val materializations = receipts.foldLeft(
            Right(Vector.empty): Either[
              ExecutionReceiptError,
              Vector[ExecutionMaterialization]
            ]
          ): (accepted, entry) =>
            accepted.flatMap: values =>
              ExecutionMaterialization(
                ExecutionScope.Measurement(measurement.measurement.identity.id),
                entry._1,
                entry._2
              ).map(values :+ _)
          materializations match
            case Left(error) =>
              TaskReport.failed(
                CrossDomainCompileError.ExecutionEvidence(error),
                context.strategy.target,
                operatorApplications = 2L
              )
            case Right(values) =>
              TaskReport.success(
                result,
                context.strategy.target,
                operatorApplications = 2L,
                materializations = values
              )

private object CrossDomainClassificationCompiler:
  def run[
      S <: SemanticSpace,
      T <: SemanticSpace,
      N <: SemanticSpace,
      K,
      L,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      source: CrossDomainObservationSource[S, T, N, K],
      measurement: Measurement[N, K, L],
      materialization: MaterializationPolicy,
      configuration: ClassificationConfiguration[
        LearnerConfiguration,
        Fitted,
        LearnerError,
        Prediction
      ]
  ): Either[
    CrossDomainCompileError[LearnerError],
    CrossDomainClassificationEstimate[
      T,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ]
  ] =
    for
      sourceMeasured <- source.source.evidence
        .measureColumns(measurement)
        .left
        .map(CrossDomainCompileError.Evidence.apply)
      sourceDense <- sourceMeasured
        .materialize(materialization)
        .left
        .map(CrossDomainCompileError.Evidence.apply)
      targetMeasured <- source.target.evidence
        .measureColumns(measurement)
        .left
        .map(CrossDomainCompileError.Evidence.apply)
      targetDense <- targetMeasured
        .materialize(materialization)
        .left
        .map(CrossDomainCompileError.Evidence.apply)
      fitted <- configuration.compiler
        .fit(
          configuration.learner,
          source.source.target.classes,
          sourceDense.value,
          source.source.target.labels.values.toVector
        )
        .left
        .map(CrossDomainCompileError.Learner.apply)
      predictionResult <- predictTarget(
        source,
        fitted,
        targetDense.value,
        configuration.compiler
      )
      (predictions, learnerPredictions) = predictionResult
      accuracy <- Accuracy
        .evaluate(source.target.target, predictions)
        .left
        .map(CrossDomainCompileError.Categorical.apply)
      balanced <- BalancedAccuracy
        .evaluate(source.target.target, predictions)
        .left
        .map(CrossDomainCompileError.Categorical.apply)
      confusion <- ConfusionMatrix
        .evaluate(source.target.target, predictions)
        .left
        .map(CrossDomainCompileError.Categorical.apply)
    yield
      val receipt = new CrossDomainFitReceipt(
        source.source.target.samples.identity,
        source.target.target.samples.identity,
        source.source.target.classes.identity,
        configuration.adaptation,
        configuration.identity,
        sourceDense.receipt,
        targetDense.receipt
      )
      new CrossDomainClassificationEstimate(
        predictions,
        learnerPredictions,
        accuracy,
        balanced,
        confusion,
        receipt,
        configuration
      )

  private def predictTarget[
      S <: SemanticSpace,
      T <: SemanticSpace,
      N <: SemanticSpace,
      K,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      source: CrossDomainObservationSource[S, T, N, K],
      fitted: Fitted,
      values: DMat,
      compiler: CategoricalLearnerCompiler[
        LearnerConfiguration,
        Fitted,
        LearnerError,
        Prediction
      ]
  ): Either[
    CrossDomainCompileError[LearnerError],
    (CategoricalPredictions[T], Vector[Prediction])
  ] =
    val classes = source.source.target.classes
    val predicted = Vector.newBuilder[ClassId]
    val learnerPredictions = Vector.newBuilder[Prediction]
    val scores = DMat.newBuilder(values.rows, classes.size)
    var row = 0
    while row < values.rows do
      val input = IArray.tabulate(values.cols)(column => values(row, column))
      compiler.predict(fitted, input) match
        case Left(error)       => return Left(CrossDomainCompileError.Learner(error))
        case Right(prediction) =>
          val actualOrder = prediction.scores.map(_._1)
          if actualOrder != classes.keys then
            return Left(
              CrossDomainCompileError.ScoreClassOrder(classes.keys, actualOrder)
            )
          predicted += prediction.predicted
          learnerPredictions += prediction
          var klass = 0
          while klass < classes.size do
            scores(row, klass) = prediction.scores(klass)._2.value
            klass += 1
      row += 1

    for
      predictedColumn <- Column(source.target.target.samples, predicted.result()).left
        .map(CrossDomainCompileError.Column.apply)
      scoreTable <- ClassScoreTable(
        source.target.target.samples,
        classes,
        scores.result()
      ).left.map(CrossDomainCompileError.Categorical.apply)
      result <- CategoricalPredictions(
        source.target.target.samples,
        classes,
        predictedColumn,
        scores = Some(scoreTable)
      ).left.map(CrossDomainCompileError.Categorical.apply)
    yield result -> learnerPredictions.result()
