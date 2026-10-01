package scalafim.fmri.mvpa.dataset.predictive

import alder.data.{EvaluationSources, FixedCoverage, IdentifiedRows, Prediction, PredictionReceipt, PredictionResult}
import alder.kernel.*
import cats.Id
import cats.data.EitherT
import gale.linalg.{DMat, Matrix}
import scalafim.fmri.mvpa.*

enum CrossDecodingPreparationScope:
  case Identity
  case SourceOnly(axis: AxisDescriptor, receipt: DataFingerprint)

/** A declared one-to-one feature correspondence.  Equal width alone is not a
  * feature-space contract: ordered descriptors must match exactly. */
final class CrossDecodingFeatureBinding private (val source: AxisDescriptor, val target: AxisDescriptor)
object CrossDecodingFeatureBinding:
  def apply(source: AxisDescriptor, target: AxisDescriptor): Either[AlderCrossDecodingError, CrossDecodingFeatureBinding] =
    if source == target && source.size == target.size then Right(new CrossDecodingFeatureBinding(source, target))
    else Left(AlderCrossDecodingError.FeatureAxisMismatch(source.stableKey, target.stableKey))

final case class AlderCrossDecodingResult(
    classes: Vector[ClassLabel], probabilities: DMat, rows: Vector[SwiftOofRow],
    sourceAxis: AxisDescriptor, targetAxis: AxisDescriptor,
    preparation: CrossDecodingPreparationScope, sourceAudit: Audit,
    sourceMaterialization: MaterializationReceipt, targetMaterialization: MaterializationReceipt,
    evaluationReceipt: PredictionReceipt[Use.Test]
)

enum AlderCrossDecodingError:
  case Admission(error: AlderPredictiveAdmissionError)
  case Mvpa(error: MvpaError)
  case Evaluation(detail: String)
  case SameDomainAxis(axis: String)
  case FeatureAxisMismatch(source: String, target: String)
  case FeatureCountMismatch(source: Int, target: Int)
  case OverlappingNativeId(id: Long)
  case InvalidPreparationScope(expectedSource: String, actual: String)
  case InvalidPreparationReceipt
  case TargetShape(stableKey: String)
  case UnknownTarget(stableKey: String, value: Double)
  case MissingSourceClass
  case DuplicateTargetKey(key: String)

/** Fits source observations and evaluates actual target observations through
  * Alder's precommitted Train/Test route. It makes no calibration claim. */
object AlderCrossDecoding:
  def correlationCentroid[M, N](source: AlderMaterializedRows[M], target: AlderMaterializedRows[N], features: CrossDecodingFeatureBinding, coding: SwiftTargetCoding,
      preparation: CrossDecodingPreparationScope = CrossDecodingPreparationScope.Identity): Either[AlderCrossDecodingError, AlderCrossDecodingResult] =
    evaluate(source, target, features, coding, preparation, CorrelationCentroidClassifier(), "scalafim.cross-domain-correlation-centroid", Vector.empty)

  def swiftCentroid[M, N](source: AlderMaterializedRows[M], target: AlderMaterializedRows[N], features: CrossDecodingFeatureBinding, coding: SwiftTargetCoding,
      scaling: FeatureScaling = FeatureScaling.ZScore,
      preparation: CrossDecodingPreparationScope = CrossDecodingPreparationScope.Identity): Either[AlderCrossDecodingError, AlderCrossDecodingResult] =
    evaluate(source, target, features, coding, preparation, SwiftCentroidClassifier(scaling), "scalafim.cross-domain-swift-centroid", Vector(scaling.toString))

  def ridgeLda[M, N](source: AlderMaterializedRows[M], target: AlderMaterializedRows[N], features: CrossDecodingFeatureBinding, coding: SwiftTargetCoding,
      penalty: RidgePenalty,
      preparation: CrossDecodingPreparationScope = CrossDecodingPreparationScope.Identity): Either[AlderCrossDecodingError, AlderCrossDecodingResult] =
    evaluate(source, target, features, coding, preparation, RidgeLdaClassifier.fromPenalty(penalty), "scalafim.cross-domain-ridge-lda", Vector(java.lang.Double.toHexString(penalty.value)))

  private def evaluate[M, N](source: AlderMaterializedRows[M], target: AlderMaterializedRows[N], features: CrossDecodingFeatureBinding, coding: SwiftTargetCoding,
      preparation: CrossDecodingPreparationScope, classifier: Classifier, component: String, parameters: Vector[String]): Either[AlderCrossDecodingError, AlderCrossDecodingResult] =
    for
      _ <- if source.mapping.axis != target.mapping.axis then Right(()) else Left(AlderCrossDecodingError.SameDomainAxis(source.mapping.axis.stableKey))
      _ <- verifyNativeFeatures(source, features.source)
      _ <- verifyNativeFeatures(target, features.target)
      _ <- if features.source == features.target && features.source.size == source.receipt.inputs && features.target.size == target.receipt.inputs then Right(()) else Left(AlderCrossDecodingError.FeatureAxisMismatch(features.source.stableKey, features.target.stableKey))
      _ <- if source.receipt.inputs == target.receipt.inputs then Right(()) else Left(AlderCrossDecodingError.FeatureCountMismatch(source.receipt.inputs, target.receipt.inputs))
      _ <- preparation match
        case CrossDecodingPreparationScope.Identity => Right(())
        case CrossDecodingPreparationScope.SourceOnly(axis, receipt) if axis == source.mapping.axis && receipt == source.root.fingerprint => Right(())
        case CrossDecodingPreparationScope.SourceOnly(axis, _) if axis == source.mapping.axis => Left(AlderCrossDecodingError.InvalidPreparationReceipt)
        case CrossDecodingPreparationScope.SourceOnly(axis, _) => Left(AlderCrossDecodingError.InvalidPreparationScope(source.mapping.axis.stableKey, axis.stableKey))
      joint <- jointRows(source, target)
      split <- joint.fixedHoldout(source.mapping.nativeIds, target.mapping.nativeIds, FixedCoverage.Exhaustive).left.map(error => AlderCrossDecodingError.Evaluation(error.toString))
      context = FitContext.root(Seed(0L), PlanFingerprint(AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.cross-domain-correlation-centroid.v2")
        writer.string(source.root.fingerprint.digest); writer.string(target.root.fingerprint.digest)
        writer.string(source.mapping.axis.coordinateSignature.value); writer.string(target.mapping.axis.coordinateSignature.value)
        writer.string(features.source.stableKey)
        writer.string(component)
        parameters.foreach(writer.string)
        coding.values.foreach((value, label) =>
          writer.string(java.lang.Double.toHexString(value))
          writer.string(label.value))
        preparation match
          case CrossDecodingPreparationScope.Identity => writer.string("identity")
          case CrossDecodingPreparationScope.SourceOnly(axis, receipt) =>
            writer.string(axis.stableKey)
            writer.string(receipt.digest)
      ), SchemaFingerprint("scalafim.cross-domain-correlation-centroid.v2"), NumericMode.Deterministic)
      trained <- new SourceLearner(source, coding, classifier, component, parameters).fit(split.train)(using context).value.left.map(_.cause)
      sources <- EvaluationSources.precommittedTest(split.train, split.test.data).left.map(error => AlderCrossDecodingError.Evaluation(error.toString))
      prediction <- Prediction.runBy(trained, sources)(_.input).left.map(error => AlderCrossDecodingError.Evaluation(error.toString))
      result <- serve(source, target, coding, preparation, trained, prediction)
    yield result

  private def verifyNativeFeatures[M](rows: AlderMaterializedRows[M], declared: AxisDescriptor): Either[AlderCrossDecodingError, Unit] =
    rows.nativeReadReceipt match
      case Some(receipt) if receipt.observationsIdentity.columns != declared =>
        Left(AlderCrossDecodingError.FeatureAxisMismatch(receipt.observationsIdentity.columns.stableKey, declared.stableKey))
      case _ => Right(())

  private def jointRows[M, N](source: AlderMaterializedRows[M], target: AlderMaterializedRows[N]): Either[AlderCrossDecodingError, IdentifiedRows[Example[Array[Double], Array[Double], Unit]]] =
    val sourceIds = source.mapping.nativeIds.toSet
    target.mapping.nativeIds.find(sourceIds.contains) match
      case Some(id) => Left(AlderCrossDecodingError.OverlappingNativeId(id))
      case None =>
        def row[A](root: IdentifiedRows[Example[Array[Double], Array[Double], A]], id: Long): Example[Array[Double], Array[Double], A] =
          root.training(Vector(id)).toOption.get.data.foldRows(Option.empty[Example[Array[Double], Array[Double], A]])((_, _, value) => Some(value)).get
        val declared = new DataFingerprint(FingerprintPolicy.Summary("scalafim.cross-domain-root.v1"), AxisDigest.sha256Hex: writer =>
          writer.string("scalafim.cross-domain-root.v1"); writer.string(source.root.fingerprint.digest); writer.string(target.root.fingerprint.digest)
          writer.string(source.mapping.axis.coordinateSignature.value); writer.string(target.mapping.axis.coordinateSignature.value)
        )
        val sourceRows = source.mapping.nativeIds.map(id => id -> row(source.root, id)).map((id, value) => id -> Example(value.input, value.target, ()))
        val targetRows = target.mapping.nativeIds.map(id => id -> row(target.root, id)).map((id, value) => id -> Example(value.input, value.target, ()))
        IdentifiedRows.fromRows(sourceRows ++ targetRows, declared).left.map(error => AlderCrossDecodingError.Evaluation(error.toString))

  private def serve[M, N](source: AlderMaterializedRows[M], target: AlderMaterializedRows[N], coding: SwiftTargetCoding,
      preparation: CrossDecodingPreparationScope, trained: Trained[SourcePipe],
      prediction: PredictionResult[Use.Test, Example[Array[Double], Array[Double], Unit], ClassificationPrediction]): Either[AlderCrossDecodingError, AlderCrossDecodingResult] =
    val matrix = Matrix.newBuilder(target.mapping.entriesByOrdinal.length, coding.classes.length)
    val rows = Vector.newBuilder[SwiftOofRow]
    var failure = Option.empty[AlderCrossDecodingError]
    val seen = scala.collection.mutable.HashSet.empty[String]
    prediction.predicted.data.foreachRow: (id, predicted) =>
      if failure.isEmpty then
        val ordinal = target.mapping.nativeIds.indexOf(id.value)
        if ordinal < 0 then failure = Some(AlderCrossDecodingError.Evaluation(s"non-target prediction ${id.value}"))
        else
          val key = target.mapping.entriesByOrdinal(ordinal).stableKey
          if !seen.add(key) then failure = Some(AlderCrossDecodingError.DuplicateTargetKey(key))
          else Classification.reorderProbabilities(predicted.prediction, coding.classes) match
            case Left(error) => failure = Some(AlderCrossDecodingError.Mvpa(error))
            case Right(probability) =>
              if predicted.observation.target.length != 1 then failure = Some(AlderCrossDecodingError.TargetShape(key))
              else coding.label(predicted.observation.target(0)) match
                case None => failure = Some(AlderCrossDecodingError.UnknownTarget(key, predicted.observation.target(0)))
                case Some(observed) =>
                  val values = Vector.tabulate(coding.classes.length)(probability(0, _))
                  values.indices.foreach(column => matrix(ordinal, column) = values(column))
                  rows += SwiftOofRow(key, values, coding.classes(values.indices.maxBy(values)), observed, Vector.empty)
    failure.toLeft(AlderCrossDecodingResult(coding.classes, matrix.result(), rows.result(), source.mapping.axis, target.mapping.axis,
      preparation, trained.audit, source.receipt, target.receipt, prediction.receipt))

  private final class SourceLearner[M](source: AlderMaterializedRows[M], coding: SwiftTargetCoding, classifier: Classifier, component: String, parameters: Vector[String])
      extends Learner[Id, Array[Double], Array[Double], Unit, ClassificationPrediction]:
    type FitError = AlderCrossDecodingError
    type RunError = AlderCrossDecodingError
    type Model = SourcePipe
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Array[Double], Array[Double], Unit]])(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      materialize(data.data, source, coding).flatMap { case (patterns, labels) =>
        if labels.distinct.length != coding.classes.length then Left(AlderCrossDecodingError.MissingSourceClass)
        else classifier.fit(patterns, Response.Categorical(labels)).left.map(AlderCrossDecodingError.Mvpa.apply)
      } match
        case Left(error) => EitherT.leftT(context.stagePath.failure(error))
        case Right(model) => EitherT.rightT(context.complete(new SourcePipe(model, context.stagePath), data,
          ComponentDescriptor(ComponentId(component), ComponentVersion("2"), AuditValue.record("parameters" -> AuditValue.text(parameters.mkString(";"))), BackendFingerprint("scalafim", "1", AuditValue.record()))))

  private final class SourcePipe(model: ClassifierModel, stage: StagePath) extends Pipe[Array[Double], AlderCrossDecodingError, ClassificationPrediction]:
    def run(input: Array[Double]): Either[Failure[AlderCrossDecodingError], ClassificationPrediction] =
      model.predict(PatternMatrix.fromRows(Vector(input.toVector))).left.map(error => stage.failure(AlderCrossDecodingError.Mvpa(error)))

  private def materialize[M](data: Data[?, Example[Array[Double], Array[Double], Unit]], source: AlderMaterializedRows[M], coding: SwiftTargetCoding): Either[AlderCrossDecodingError, (PatternMatrix, Vector[ClassLabel])] =
    val input = Vector.newBuilder[Vector[Double]]
    val labels = Vector.newBuilder[ClassLabel]
    var failure = Option.empty[AlderCrossDecodingError]
    data.foreachRow: (id, example) =>
      if failure.isEmpty then
        val ordinal = source.mapping.nativeIds.indexOf(id.value)
        if ordinal < 0 then failure = Some(AlderCrossDecodingError.Evaluation(s"target row ${id.value} reached source learner"))
        else
          val key = source.mapping.entriesByOrdinal(ordinal).stableKey
          if example.target.length != 1 then failure = Some(AlderCrossDecodingError.TargetShape(key))
          else coding.label(example.target(0)) match
            case None => failure = Some(AlderCrossDecodingError.UnknownTarget(key, example.target(0)))
            case Some(label) => input += example.input.toVector; labels += label
    failure.toLeft((PatternMatrix.fromRows(input.result()), labels.result()))
