package scalafim.fmri.mvpa.dataset.predictive

import alder.data.FixedCoverage
import alder.kernel.*
import cats.Id
import cats.data.EitherT
import gale.linalg.{DMat, Matrix}
import resample4s.core.DigestAlgorithm
import scalafim.fmri.mvpa.*

final class SwiftTargetCoding private (val values: Vector[(Double, ClassLabel)]):
  def classes: Vector[ClassLabel] = values.map(_._2)
  def label(value: Double): Option[ClassLabel] = values.collectFirst { case (coded, label) if coded == value => label }
object SwiftTargetCoding:
  def apply(values: Vector[(Double, String)]): Either[AlderSwiftCentroidError, SwiftTargetCoding] =
    if values.length < 2 || values.exists((value, label) => !value.isFinite || label.trim.isEmpty) then
      Left(AlderSwiftCentroidError.InvalidTargetCoding)
    else
      val coded = values.map { case (value, label) => value -> ClassLabel(label) }
      if coded.map(_._1).distinct.length != coded.length || coded.map(_._2).distinct.length != coded.length then
        Left(AlderSwiftCentroidError.InvalidTargetCoding)
      else Right(new SwiftTargetCoding(coded))

final case class SwiftOofRow(
    stableKey: String,
    probabilities: Vector[Double],
    predicted: ClassLabel,
    observed: ClassLabel,
    trainingStableKeys: Vector[String]
)

final case class SwiftAssessment(correct: Long, samples: Long, confusion: Vector[Vector[Long]]):
  def accuracy: Double = correct.toDouble / samples.toDouble

final case class SwiftFoldFit(
    unit: resample4s.core.UnitKey,
    trainingStableKeys: Vector[String],
    means: Vector[Double],
    scales: Vector[Double],
    classes: Vector[ClassLabel],
    priors: Vector[Double],
    audit: Audit
)
final case class AlderSwiftCentroidResult(
    classes: Vector[ClassLabel],
    probabilities: DMat,
    rows: Vector[SwiftOofRow],
    planReceipt: resample4s.core.PlanReceipt,
    fits: Vector[SwiftFoldFit],
    assessment: SwiftAssessment,
    materialization: MaterializationReceipt,
    nativeRead: Option[NativeReadReceipt]
):
  def trainingFingerprints: Vector[DataFingerprint] = fits.map(_.audit.data)

enum AlderSwiftCentroidError:
  case Admission(error: AlderPredictiveAdmissionError)
  case Evidence(error: EvidenceError)
  case Mvpa(error: MvpaError)
  case InvalidTargetCoding
  case TargetShape(stableKey: String)
  case UnknownTarget(stableKey: String, value: Double)
  case MissingClass(fold: Int)
  case DuplicateAssessment(stableKey: String)
  case MissingAssessment(stableKey: String)

object AlderSwiftCentroid:
  def crossValidate[S <: multivar.core.SemanticSpace, K, M](rows: AlderMaterializedRows[M], design: CrossFitDesign[S, K], coding: SwiftTargetCoding, classifier: SwiftCentroidClassifier = SwiftCentroidClassifier())(using DigestAlgorithm): Either[AlderSwiftCentroidError, AlderSwiftCentroidResult] =
    for
      _ <- AlderPredictiveAdmission.crossFit(rows, design).left.map(AlderSwiftCentroidError.Admission.apply)
      result <- evaluate(rows, design, coding, classifier)
    yield result

  private def evaluate[S <: multivar.core.SemanticSpace, K, M](rows: AlderMaterializedRows[M], design: CrossFitDesign[S, K], coding: SwiftTargetCoding, classifier: SwiftCentroidClassifier): Either[AlderSwiftCentroidError, AlderSwiftCentroidResult] =
    val out = Matrix.newBuilder(design.samples.size, coding.classes.length)
    val assembled = Array.fill[Option[SwiftOofRow]](design.samples.size)(None)
    val fits = Vector.newBuilder[SwiftFoldFit]
    val ordinalByNative = rows.mapping.nativeIds.zipWithIndex.toMap
    var fold = 0
    var failure: Option[AlderSwiftCentroidError] = None
    while fold < design.keys.length && failure.isEmpty do
      design.at(design.keys(fold)) match
        case Left(error) => failure = Some(AlderSwiftCentroidError.Evidence(error))
        case Right(unit) =>
          val trainIds = unit.analysis.ordinals.toVector.map(rows.mapping.nativeIds)
          val testIds = unit.assessment.ordinals.toVector.map(rows.mapping.nativeIds)
          rows.fixedHoldout(trainIds, testIds, FixedCoverage.Exhaustive) match
            case Left(error) => failure = Some(AlderSwiftCentroidError.Admission(AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString)))
            case Right(split) =>
              val learner = new SwiftLearner[M](rows, coding, classifier, fold)
              val context = FitContext.root(
                Seed.fromLong(design.receipt.seed.value),
                PlanFingerprint(AxisDigest.sha256Hex: writer =>
                  writer.string("scalafim.swift-fold.v1")
                  writer.string(rows.root.fingerprint.digest)
                  writer.string(design.samples.descriptor.stableKey)
                  writer.intLE(unit.key.repeat)
                  writer.intLE(unit.key.fold)
                  val assignment = design.receipt.assignment.value.toIArray
                  writer.intLE(assignment.length)
                  assignment.foreach(byte => writer.intLE(byte & 0xff))
                  coding.values.foreach: (value, label) =>
                    writer.string(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(value)))
                    writer.string(label.value)
                ),
                SchemaFingerprint("scalafim.swift-centroid.v1"),
                NumericMode.Deterministic
              )
              learner.fit(split.train)(using context).value match
                case Left(failureValue) => failure = Some(failureValue.cause)
                case Right(trained) =>
                  val trainKeys = keys(split.train.data, rows)
                  val model = trained.artifact.model
                  fits += SwiftFoldFit(unit.key, trainKeys, model.scaler.means.toVector,
                    model.scaler.scales.toVector, model.classes, model.priors, trained.audit)
                  split.test.data.foreachRow: (id, example) =>
                    if failure.isEmpty then
                      trained.artifact.run(example.input) match
                        case Left(error) => failure = Some(error.cause)
                        case Right(prediction) =>
                          val ordinal = ordinalByNative(id.value)
                          val stableKey = rows.mapping.entriesByOrdinal(ordinal).stableKey
                          if assembled(ordinal).nonEmpty then failure = Some(AlderSwiftCentroidError.DuplicateAssessment(stableKey))
                          else Classification.reorderProbabilities(prediction, coding.classes) match
                            case Left(error) => failure = Some(AlderSwiftCentroidError.Mvpa(error))
                            case Right(probabilities) =>
                              val values = Vector.tabulate(coding.classes.length)(column => probabilities(0, column))
                              var column = 0
                              while column < coding.classes.length do
                                out(ordinal, column) = values(column)
                                column += 1
                              if example.target.length != 1 then failure = Some(AlderSwiftCentroidError.TargetShape(stableKey))
                              else coding.label(example.target(0)) match
                                case None => failure = Some(AlderSwiftCentroidError.UnknownTarget(stableKey, example.target(0)))
                                case Some(observed) =>
                                  var best = 0
                                  var candidate = 1
                                  while candidate < values.length do
                                    if values(candidate) > values(best) then best = candidate
                                    candidate += 1
                                  assembled(ordinal) = Some(SwiftOofRow(stableKey, values, coding.classes(best), observed, trainKeys))
      fold += 1
    failure.orElse(assembled.zipWithIndex.collectFirst { case (None, ordinal) => AlderSwiftCentroidError.MissingAssessment(rows.mapping.entriesByOrdinal(ordinal).stableKey) }) match
      case Some(error) => Left(error)
      case None =>
        val completed = assembled.toVector.flatten
        val index = coding.classes.zipWithIndex.toMap
        val confusion = Array.fill(coding.classes.length, coding.classes.length)(0L)
        completed.foreach(row => confusion(index(row.observed))(index(row.predicted)) += 1L)
        val assessment = SwiftAssessment(completed.count(row => row.observed == row.predicted).toLong,
          completed.length.toLong, confusion.toVector.map(_.toVector))
        Right(AlderSwiftCentroidResult(coding.classes, out.result(), completed, design.receipt, fits.result(), assessment, rows.receipt, rows.nativeReadReceipt))

  private def keys[M](data: Data[?, Example[Array[Double], Array[Double], M]], rows: AlderMaterializedRows[M]): Vector[String] =
    data.foldRows(Vector.empty[String])((keys, id, _) => keys :+ rows.mapping.entriesByOrdinal(rows.mapping.nativeIds.indexOf(id.value)).stableKey)

  private final class SwiftLearner[M](rows: AlderMaterializedRows[M], coding: SwiftTargetCoding, classifier: SwiftCentroidClassifier, fold: Int) extends Learner[Id, Array[Double], Array[Double], M, ClassificationPrediction]:
    type FitError = AlderSwiftCentroidError
    type RunError = AlderSwiftCentroidError
    type Model = SwiftPipe
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Array[Double], Array[Double], M]])(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      materialize(data.data, rows, coding).flatMap { case (patterns, labels) =>
        if labels.distinct.length != coding.classes.length then Left(AlderSwiftCentroidError.MissingClass(fold))
        else classifier.fit(patterns, Response.Categorical(labels)).left.map(AlderSwiftCentroidError.Mvpa.apply)
      } match
        case Left(error) => EitherT.leftT(context.stagePath.failure(error))
        case Right(model) =>
          model match
            case swift: SwiftCentroidModel =>
              val pipe = new SwiftPipe(swift, context.stagePath)
              EitherT.rightT(context.complete(pipe, data, ComponentDescriptor(
                ComponentId("scalafim.swift-centroid"), ComponentVersion("1"),
                AuditValue.record("scaling" -> AuditValue.text(classifier.scaling match
                  case FeatureScaling.None => "none"
                  case FeatureScaling.ZScore => "zscore"
                  case FeatureScaling.DiagonalShrinkage(alpha) => s"diagonal-shrinkage:${java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(alpha.value))}"
                )), BackendFingerprint("scalafim", "1", AuditValue.record()))))
            case _ => EitherT.leftT(context.stagePath.failure(AlderSwiftCentroidError.Mvpa(
              MvpaError.InvalidClassifierInput("Swift learner returned a foreign model"))))

  private final class SwiftPipe(val model: SwiftCentroidModel, stage: StagePath)
      extends Pipe[Array[Double], AlderSwiftCentroidError, ClassificationPrediction]:
    def run(input: Array[Double]): Either[Failure[AlderSwiftCentroidError], ClassificationPrediction] =
      model.predict(PatternMatrix.fromRows(Vector(input.toVector)))
        .left.map(error => stage.failure(AlderSwiftCentroidError.Mvpa(error)))

  private def materialize[M](data: Data[?, Example[Array[Double], Array[Double], M]], rows: AlderMaterializedRows[M], coding: SwiftTargetCoding): Either[AlderSwiftCentroidError, (PatternMatrix, Vector[ClassLabel])] =
    val inputs = Vector.newBuilder[Vector[Double]]
    val labels = Vector.newBuilder[ClassLabel]
    var error: Option[AlderSwiftCentroidError] = None
    data.foreachRow: (id, example) =>
      if error.isEmpty then
        val ordinal = rows.mapping.nativeIds.indexOf(id.value)
        if ordinal < 0 then error = Some(AlderSwiftCentroidError.Admission(AlderPredictiveAdmissionError.CrossFitPopulationMismatch))
        else if example.target.length != 1 then error = Some(AlderSwiftCentroidError.TargetShape(rows.mapping.entriesByOrdinal(ordinal).stableKey))
        else coding.label(example.target(0)) match
          case None => error = Some(AlderSwiftCentroidError.UnknownTarget(rows.mapping.entriesByOrdinal(ordinal).stableKey, example.target(0)))
          case Some(label) =>
            inputs += example.input.toVector
            labels += label
    error match
      case Some(problem) => Left(problem)
      case None => Right((PatternMatrix.fromRows(inputs.result()), labels.result()))
