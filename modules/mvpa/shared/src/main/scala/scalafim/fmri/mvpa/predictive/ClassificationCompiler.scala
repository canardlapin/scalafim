package scalafim.fmri.mvpa.predictive

import alder.data.FeatureView
import alder.data.Resample4sResampler
import alder.data.Schema
import alder.kernel.Audit
import alder.kernel.AuditValue
import alder.kernel.ComponentDescriptor
import alder.kernel.ComponentVersion
import alder.kernel.DataFingerprint
import alder.kernel.Example
import alder.kernel.Failure
import alder.kernel.FitContext
import alder.kernel.FitResult
import alder.kernel.Learner
import alder.kernel.NonEmptyData
import alder.kernel.Pipe
import alder.kernel.PlanFingerprint
import alder.kernel.ProtocolFingerprint
import alder.kernel.Scored
import alder.kernel.Seed as AlderSeed
import alder.kernel.StagePath
import alder.kernel.Trained
import alder.kernel.Use
import alder.metrics.MetricDescriptor
import alder.metrics.MetricError
import alder.metrics.MetricId
import alder.metrics.MetricNumericPolicy
import alder.metrics.MetricVersion
import alder.metrics.ObjectiveDescriptor
import alder.metrics.ObjectiveDirection
import alder.metrics.ObjectiveMetric
import alder.tune.CrossValidatedCandidateEvidence
import alder.tune.FoldScore
import alder.tune.GridStrategy
import alder.tune.PositiveInt
import alder.tune.Search
import alder.tune.SearchError
import alder.tune.Space
import cats.Id
import cats.data.EitherT
import cats.kernel.CommutativeMonoid
import gale.linalg.Matrix
import multivar.core.SemanticSpace
import resample4s.core.Coverage
import resample4s.core.DigestAlgorithm
import resample4s.core.DigestError
import resample4s.core.Selection
import resample4s.core.Split
import resample4s.core.UnitKey
import scalafim.fmri.mvpa.*

enum StandardizationSpecification:
  case CenterScaleRejectConstant
  case CenterScaleEmitZero

  def identity: String =
    this match
      case CenterScaleRejectConstant => "center-scale-reject-constant"
      case CenterScaleEmitZero       => "center-scale-emit-zero"

final class ClassificationConfiguration[
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction <: CategoricalLearnerPrediction
] private (
    val learner: LearnerConfiguration,
    val compiler: CategoricalLearnerCompiler[
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ],
    val definition: CategoricalLearnerDefinition,
    val adaptation: ClassifierAdaptation,
    val identity: EstimandIdentity
)

object ClassificationConfiguration:
  def apply[
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      learner: LearnerConfiguration,
      adaptation: ClassifierAdaptation = ClassifierAdaptation.InductiveWithinDomain
  )(using
      compiler: CategoricalLearnerCompiler[
        LearnerConfiguration,
        Fitted,
        LearnerError,
        Prediction
      ]
  ): Either[
    ScientificIdentityError,
    ClassificationConfiguration[
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ]
  ] =
    val definition = compiler.definition(learner)
    EstimandIdentity(
      EstimandKind.unsafe("classification"),
      Vector(
        "adaptation" -> adaptation.identity,
        "classifier" -> definition.id.text,
        "classifier-parameter" -> definition.parameterIdentity,
        "decision-score" -> definition.scoreIdentity,
        "standardization" -> definition.preparationIdentity,
        "tie-policy" -> "declared-class-order",
        "model-disposition" -> "assessment-only"
      )
    ).map(identity =>
      new ClassificationConfiguration(
        learner,
        compiler,
        definition,
        adaptation,
        identity
      )
    )

/** One terminal prediction produced by the built-in Alder workflow. */
final case class AlderClassPrediction(
    predicted: ClassId,
    scores: Vector[(ClassId, DecisionScore)]
) extends CategoricalLearnerPrediction

/** Sample-keyed observation emitted by one assessment fold. */
final case class FoldClassObservation[
    Prediction <: CategoricalLearnerPrediction
](
    sample: SampleId,
    truth: ClassId,
    prediction: Prediction
)

final class FoldClassificationEstimate[
    Prediction <: CategoricalLearnerPrediction
] private[predictive] (
    val accuracy: AccuracyEstimate,
    val observations: Vector[FoldClassObservation[Prediction]]
)

private final class FoldClassificationAccumulator[
    Prediction <: CategoricalLearnerPrediction
](
    val observations: Vector[FoldClassObservation[Prediction]]
)

private final class FoldClassificationMetric[
    Prediction <: CategoricalLearnerPrediction
](samples: AxisRef[SampleId])
    extends ObjectiveMetric[
      Scored[ClassId, Prediction, SampleId],
      FoldClassificationEstimate[Prediction]
    ]:
  override type Acc = FoldClassificationAccumulator[Prediction]

  override given accumulator: CommutativeMonoid[
    FoldClassificationAccumulator[Prediction]
  ] with
    override def empty: FoldClassificationAccumulator[Prediction] =
      new FoldClassificationAccumulator(Vector.empty)

    override def combine(
        left: FoldClassificationAccumulator[Prediction],
        right: FoldClassificationAccumulator[Prediction]
    ): FoldClassificationAccumulator[Prediction] =
      new FoldClassificationAccumulator(
        (left.observations ++ right.observations).sortBy(observationKey)
      )

  override val direction: ObjectiveDirection = ObjectiveDirection.Maximize

  override val descriptor: MetricDescriptor =
    MetricDescriptor(
      MetricId("scalafim-classification-observations"),
      MetricVersion("1"),
      AuditValue.record(
        "estimate" -> AuditValue.text("accuracy-and-oof-observations"),
        "order" -> AuditValue.text("authoritative-source-axis")
      ),
      MetricNumericPolicy.Reproducible,
      Some(ObjectiveDescriptor(direction, "binary64-decimal-v1"))
    )

  override def auditScore(
      score: FoldClassificationEstimate[Prediction]
  ): AuditValue =
    AuditValue.decimal(score.accuracy.value)

  override def observe(
      scored: Scored[ClassId, Prediction, SampleId]
  ): FoldClassificationAccumulator[Prediction] =
    new FoldClassificationAccumulator(
      Vector(
        FoldClassObservation(
          scored.meta,
          scored.truth,
          scored.prediction
        )
      )
    )

  override def finish(
      accumulated: FoldClassificationAccumulator[Prediction]
  ): Either[MetricError, FoldClassificationEstimate[Prediction]] =
    if accumulated.observations.isEmpty then Left(MetricError.Empty)
    else
      val correct = accumulated.observations.count(value => value.truth == value.prediction.predicted)
      val accuracy = correct.toDouble / accumulated.observations.size.toDouble
      if accuracy.isFinite then
        Right(
          new FoldClassificationEstimate(
            AccuracyEstimate(accuracy),
            accumulated.observations
          )
        )
      else Left(MetricError.NonFiniteResult)

  private def observationKey(
      value: FoldClassObservation[Prediction]
  ): (Int, String) =
    val scores = value.prediction.scores
      .map((label, score) => s"${label.value}:${java.lang.Double.toHexString(score.value)}")
      .mkString("|")
    val ordinal = samples.positionOf(value.sample).getOrElse(Int.MaxValue)
    ordinal ->
      s"${value.truth.value}\u0000${value.prediction.predicted.value}\u0000$scores"

enum CategoricalLearnerFailure[+LearnerError]:
  case Boundary(error: CategoricalClassifierError)
  case Learner(error: LearnerError)

  def message(renderLearnerError: LearnerError => String): String =
    this match
      case Boundary(error) => error.message
      case Learner(error)  => renderLearnerError(error)

private final class CompiledCategoricalModel[
    Input,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction <: CategoricalLearnerPrediction
](
    fitted: Fitted,
    compiler: CategoricalLearnerCompiler[
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ],
    featureView: FeatureView[Input],
    stage: StagePath
) extends Pipe[
      Input,
      CategoricalLearnerFailure[LearnerError],
      Prediction
    ]:
  override def run(
      input: Input
  ): Either[Failure[CategoricalLearnerFailure[LearnerError]], Prediction] =
    featureView
      .read(input)
      .left
      .map(error =>
        stage.failure(
          CategoricalLearnerFailure.Boundary(
            CategoricalClassifierError.FeatureRead(error.toString)
          )
        )
      )
      .flatMap(values =>
        compiler
          .predict(fitted, values)
          .left
          .map(error => stage.failure(CategoricalLearnerFailure.Learner(error)))
      )

private final class CompiledCategoricalLearner[
    Input,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction <: CategoricalLearnerPrediction
](
    classes: AxisRef[ClassId],
    configuration: ClassificationConfiguration[
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ],
    featureView: FeatureView[Input]
) extends Learner[
      Id,
      Input,
      ClassId,
      SampleId,
      Prediction
    ]:
  override type FitError = CategoricalLearnerFailure[LearnerError]
  override type RunError = CategoricalLearnerFailure[LearnerError]
  override type Model = CompiledCategoricalModel[
    Input,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction
  ]

  override def fit[U <: Use.Fit](
      data: NonEmptyData[
        U,
        Example[Input, ClassId, SampleId]
      ]
  )(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
    EitherT.fromEither(
      fitModel(data, context.stagePath).map(model => context.complete(model, data, descriptor))
    )

  private def fitModel[U <: Use.Fit](
      data: NonEmptyData[
        U,
        Example[Input, ClassId, SampleId]
      ],
      stage: StagePath
  ): Either[Failure[FitError], Model] =
    val first = data.data.foldRows(Option.empty[Input]) {
      case (value @ Some(_), _, _) => value
      case (None, _, example)      => Some(example.input)
    }
    val featureCount = first.fold(0)(_ => featureView.size)
    val rows = Vector.newBuilder[Array[Double]]
    val labels = Vector.newBuilder[ClassId]
    val failure = data.data.foldRows(Option.empty[Failure[FitError]]) {
      case (error @ Some(_), _, _) => error
      case (None, _, example)      =>
        classes.positionOf(example.target) match
          case None =>
            Some(
              stage.failure(
                CategoricalLearnerFailure.Boundary(
                  CategoricalClassifierError.UnknownClass(example.target)
                )
              )
            )
          case Some(_) =>
            featureView.read(example.input) match
              case Left(error) =>
                Some(
                  stage.failure(
                    CategoricalLearnerFailure.Boundary(
                      CategoricalClassifierError.FeatureRead(error.toString)
                    )
                  )
                )
              case Right(values) if values.length != featureCount =>
                Some(
                  stage.failure(
                    CategoricalLearnerFailure.Boundary(
                      CategoricalClassifierError.DimensionMismatch(
                        featureCount,
                        values.length
                      )
                    )
                  )
                )
              case Right(values) =>
                var coordinate = 0
                val row = new Array[Double](featureCount)
                var invalid: Option[Failure[FitError]] = None
                while coordinate < featureCount && invalid.isEmpty do
                  val value = values(coordinate)
                  if !value.isFinite then
                    invalid = Some(
                      stage.failure(
                        CategoricalLearnerFailure.Boundary(
                          CategoricalClassifierError.NonFiniteCoordinate(
                            0,
                            coordinate,
                            value
                          )
                        )
                      )
                    )
                  else row(coordinate) = value
                  coordinate += 1
                invalid match
                  case Some(error) => Some(error)
                  case None        =>
                    rows += row
                    labels += example.target
                    None
    }
    failure match
      case Some(error) => Left(error)
      case None        =>
        val materialized = rows.result()
        val matrix = Matrix.tabulate(materialized.length, featureCount): (row, column) =>
          materialized(row)(column)
        configuration.compiler
          .fit(
            configuration.learner,
            classes,
            matrix,
            labels.result()
          )
          .left
          .map(error => stage.failure(CategoricalLearnerFailure.Learner(error)))
          .map(fitted =>
            new CompiledCategoricalModel(
              fitted,
              configuration.compiler,
              featureView,
              stage
            )
          )

  private val descriptor: ComponentDescriptor =
    ComponentDescriptor(
      configuration.definition.componentId,
      ComponentVersion("1"),
      AuditValue.record(
        "adaptation" -> AuditValue.text(configuration.adaptation.identity),
        "classAxis" -> AuditValue.text(classes.identity.fingerprint.value),
        "classes" -> AuditValue.sequence(
          classes.keys.map(value => AuditValue.text(value.value))*
        ),
        "decisionScore" -> AuditValue.text(configuration.definition.scoreIdentity),
        "scaling" -> AuditValue.text(configuration.definition.preparationIdentity),
        "tiePolicy" -> AuditValue.text("declared-class-order")
      ),
      configuration.definition.backend
    )

final class ClassificationFoldReceipt private[predictive] (
    val unit: UnitKey,
    val analysisSamples: Vector[SampleId],
    val assessmentSamples: Vector[SampleId],
    val analysisFingerprint: DataFingerprint,
    val assessmentFingerprint: DataFingerprint,
    val fitAudit: Audit,
    val fitIdentity: ProtocolFingerprint,
    val seed: AlderSeed
)

private[predictive] final class AlderClassificationRun[
    S <: SemanticSpace,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction <: CategoricalLearnerPrediction
](
    val samples: AxisRef.Aux[SampleId, S],
    val classes: AxisRef[ClassId],
    val configuration: ClassificationConfiguration[
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ],
    val folds: Vector[(UnitKey, FoldClassificationEstimate[Prediction])],
    val receipts: Vector[ClassificationFoldReceipt],
    val assignment: DataFingerprint,
    val resampler: ProtocolFingerprint,
    val input: AlderInputReceipt
)

enum ClassificationCompileError[+LearnerError]:
  case Learner(error: LearnerError)
  case Target(error: CategoricalError)
  case Data(error: AlderDataError)
  case AlderResampling(error: alder.data.DataError)
  case ResampleDigest(error: DigestError)
  case SearchResampling(error: alder.data.DataError)
  case NoSuccessfulCandidate
  case UnsupportedAdaptation(adaptation: ClassifierAdaptation)
  case FoldCountMismatch(expected: Int, actual: Int)
  case MissingFoldEvidence(fold: Int)
  case FailedFold(fold: Int)
  case FoldIdentityMismatch(expected: Int, actual: Int)
  case AuditSourceMismatch(fold: Int)
  case Schedule(error: BoundScheduleError)
  case OutOfFold(error: OutOfFoldClassificationError)
  case ExecutionEvidence(error: ExecutionReceiptError)

  def message(renderLearnerError: LearnerError => String): String =
    this match
      case Learner(error)          => renderLearnerError(error)
      case Target(error)           => error.message
      case Data(error)             => error.message
      case AlderResampling(error)  => s"Alder resampling rejected the program: $error"
      case ResampleDigest(error)   => s"Resample4s receipt generation failed: $error"
      case SearchResampling(error) =>
        s"Alder cross-validation resampling failed: $error"
      case NoSuccessfulCandidate =>
        "Alder cross-validation produced no successful learner candidate"
      case UnsupportedAdaptation(adaptation) =>
        s"within-domain validation requires inductive-within-domain adaptation, obtained ${adaptation.identity}"
      case FoldCountMismatch(expected, actual) =>
        s"Alder returned $actual folds, expected $expected from the scientific design"
      case MissingFoldEvidence(fold) =>
        s"Alder returned no successful audit evidence for fold $fold"
      case FailedFold(fold)                       => s"Alder fold $fold failed"
      case FoldIdentityMismatch(expected, actual) =>
        s"Alder fold identity $actual does not match scientific ordinal $expected"
      case AuditSourceMismatch(fold) =>
        s"Alder fold $fold audit does not identify its analysis source"
      case Schedule(error)          => error.message
      case OutOfFold(error)         => error.message
      case ExecutionEvidence(error) => error.message

private enum ClassificationFoldOutcome[
    Prediction <: CategoricalLearnerPrediction
]:
  case Scored(fold: Int, estimate: FoldClassificationEstimate[Prediction])
  case Failed(fold: Int)

object ClassificationCompiler:
  def run[
      Samples <: SemanticSpace,
      Neural <: SemanticSpace,
      NeuralKey,
      LocalKey,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      plan: ScientificPlanFingerprint,
      evidence: EvidenceTable[Samples, Neural, SampleId, NeuralKey],
      target: CategoricalTarget[Samples],
      design: ValidationDesign[
        Samples,
        Coverage.ExactOnce,
        BoundSelectionSplit[SampleId, Samples]
      ] { type OrdinalUnit = Split[Selection] },
      measurement: Measurement[Neural, NeuralKey, LocalKey],
      materialization: MaterializationPolicy,
      configuration: ClassificationConfiguration[
        LearnerConfiguration,
        Fitted,
        LearnerError,
        Prediction
      ]
  ): Either[
    ClassificationCompileError[LearnerError],
    OutOfFoldClassification[
      Samples,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ]
  ] =
    if configuration.adaptation != ClassifierAdaptation.InductiveWithinDomain then
      Left(
        ClassificationCompileError.UnsupportedAdaptation(
          configuration.adaptation
        )
      )
    else
      for
        _ <- CategoricalTarget
          .validateTrainingCoverage(target, design)
          .left
          .map(ClassificationCompileError.Target.apply)
        metadata <- Column(target.samples, target.samples.keys).left
          .map(error => ClassificationCompileError.Data(AlderDataError.Metadata(error)))
        prepared <- AlderDataBoundary
          .prepareAll(
            plan,
            evidence,
            target.labels,
            metadata,
            measurement,
            materialization
          )
          .left
          .map(ClassificationCompileError.Data.apply)
        population <- Resample4sResampler
          .populationFingerprint(prepared.data.fingerprint)
          .left
          .map(ClassificationCompileError.AlderResampling.apply)
        resampler <- Resample4sResampler
          .fromCompiled[
            Example[
              AlderPatternRow[measurement.local.Id],
              ClassId,
              SampleId
            ]
          ](
            design.schedule.compiledValue,
            population
          )(using DigestAlgorithm.fnv1a64)
          .left
          .map(ClassificationCompileError.ResampleDigest.apply)
        result <-
          given Schema[AlderPatternRow[measurement.local.Id]] =
            prepared.capabilities.schema
          val workflow = new CompiledCategoricalLearner[
            AlderPatternRow[measurement.local.Id],
            LearnerConfiguration,
            Fitted,
            LearnerError,
            Prediction
          ](
            target.classes,
            configuration,
            prepared.capabilities.featureView
          )
          Search
            .crossValidatedGridSync[
              ClassificationConfiguration[
                LearnerConfiguration,
                Fitted,
                LearnerError,
                Prediction
              ],
              AlderPatternRow[measurement.local.Id],
              ClassId,
              SampleId,
              Prediction,
              FoldClassificationEstimate[Prediction],
              CompiledCategoricalLearner[
                AlderPatternRow[measurement.local.Id],
                LearnerConfiguration,
                Fitted,
                LearnerError,
                Prediction
              ]
            ](
              Space.constant(configuration),
              GridStrategy(PositiveInt.one),
              resampler,
              (_: ClassificationConfiguration[
                LearnerConfiguration,
                Fitted,
                LearnerError,
                Prediction
              ]) => workflow,
              new FoldClassificationMetric[Prediction](target.samples),
              (estimate: FoldClassificationEstimate[Prediction]) => estimate.accuracy.value,
              AlderSeed(design.schedule.seedAuthority.seed.value),
              PlanFingerprint.content("sha256", plan.value)
            )
            .runUnsplit(prepared.data)
            .left
            .map:
              case SearchError.Resampling(error) =>
                ClassificationCompileError.SearchResampling(error)
              case SearchError.Study(_) =>
                ClassificationCompileError.NoSuccessfulCandidate
        foldOutcomes = result.trials.headOption.map(trial =>
          trial.folds.map:
            case FoldScore.Scored(fold, estimate) =>
              ClassificationFoldOutcome.Scored(fold, estimate)
            case FoldScore.Failed(fold, _) =>
              ClassificationFoldOutcome.Failed(fold)
        )
        compiled <- assemble(
          target,
          design,
          configuration,
          prepared.receipt,
          foldOutcomes,
          result.evidence.headOption,
          result.assignment,
          result.resampler
        )
        outOfFold <- OutOfFoldClassification
          .reconstruct(compiled, target, design)
          .left
          .map(ClassificationCompileError.OutOfFold.apply)
      yield outOfFold

  private def assemble[
      S <: SemanticSpace,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      target: CategoricalTarget[S],
      design: ValidationDesign[
        S,
        Coverage.ExactOnce,
        BoundSelectionSplit[SampleId, S]
      ] { type OrdinalUnit = Split[Selection] },
      configuration: ClassificationConfiguration[
        LearnerConfiguration,
        Fitted,
        LearnerError,
        Prediction
      ],
      input: AlderInputReceipt,
      foldOutcomes: Option[Vector[ClassificationFoldOutcome[Prediction]]],
      evidence: Option[
        CrossValidatedCandidateEvidence[
          ClassificationConfiguration[
            LearnerConfiguration,
            Fitted,
            LearnerError,
            Prediction
          ],
          FoldClassificationEstimate[Prediction]
        ]
      ],
      assignment: DataFingerprint,
      resampler: ProtocolFingerprint
  ): Either[
    ClassificationCompileError[LearnerError],
    AlderClassificationRun[
      S,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ]
  ] =
    val keys = design.schedule.keys.toVector
    (foldOutcomes, evidence) match
      case (Some(scored), Some(audited)) =>
        if scored.length != keys.length then
          Left(
            ClassificationCompileError.FoldCountMismatch(
              keys.length,
              scored.length
            )
          )
        else if audited.folds.length != keys.length then
          Left(
            ClassificationCompileError.FoldCountMismatch(
              keys.length,
              audited.folds.length
            )
          )
        else
          val estimates = Vector.newBuilder[
            (UnitKey, FoldClassificationEstimate[Prediction])
          ]
          val receipts = Vector.newBuilder[ClassificationFoldReceipt]
          var fold = 0
          while fold < keys.length do
            val estimate = scored(fold) match
              case ClassificationFoldOutcome.Scored(actualFold, value) =>
                if actualFold != fold then
                  return Left(
                    ClassificationCompileError.FoldIdentityMismatch(
                      fold,
                      actualFold
                    )
                  )
                value
              case ClassificationFoldOutcome.Failed(actualFold) =>
                return Left(ClassificationCompileError.FailedFold(actualFold))
            val audit = audited.folds(fold)
            if audit.fold != fold then
              return Left(
                ClassificationCompileError.FoldIdentityMismatch(
                  fold,
                  audit.fold
                )
              )
            if !sameFingerprint(audit.audit.data, audit.analysis) then
              return Left(ClassificationCompileError.AuditSourceMismatch(fold))
            val split = design.schedule.at(keys(fold)) match
              case Left(error) =>
                return Left(ClassificationCompileError.Schedule(error))
              case Right(value) => value
            estimates += keys(fold) -> estimate
            receipts += new ClassificationFoldReceipt(
              keys(fold),
              split.analysis.child.keys,
              split.assessment.child.keys,
              audit.analysis,
              audit.assessment,
              audit.audit,
              audit.auditIdentity,
              audit.audit.seed
            )
            fold += 1
          Right(
            new AlderClassificationRun(
              target.samples,
              target.classes,
              configuration,
              estimates.result(),
              receipts.result(),
              assignment,
              resampler,
              input
            )
          )
      case _ => Left(ClassificationCompileError.MissingFoldEvidence(0))

  private def sameFingerprint(
      left: DataFingerprint,
      right: DataFingerprint
  ): Boolean =
    left.policy == right.policy && left.digest == right.digest
