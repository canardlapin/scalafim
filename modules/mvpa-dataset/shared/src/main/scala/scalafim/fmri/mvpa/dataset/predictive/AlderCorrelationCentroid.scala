package scalafim.fmri.mvpa.dataset.predictive

import alder.data.FixedCoverage
import alder.kernel.*
import cats.Id
import cats.data.EitherT
import gale.linalg.{DMat, Matrix}
import resample4s.core.Coverage
import scalafim.fmri.mvpa.*

/** Alder-lifecycle adapter for the correlation-centroid head.
  *
  * This intentionally does not share the Swift-centroid learner: correlation
  * normalizes each row and centroid after its own offset is removed, whereas
  * Swift uses training-fitted feature scaling and prior-weighted linear scores.
  */
final case class CorrelationCentroidFoldFit(
    unit: resample4s.core.UnitKey,
    trainingStableKeys: Vector[String],
    classes: Vector[ClassLabel],
    audit: Audit
)

final case class AlderCorrelationCentroidResult(
    classes: Vector[ClassLabel],
    probabilities: DMat,
    rows: Vector[SwiftOofRow],
    validationReceipt: resample4s.core.PlanReceipt,
    fits: Vector[CorrelationCentroidFoldFit],
    assessment: SwiftAssessment,
    materialization: MaterializationReceipt,
    nativeRead: Option[NativeReadReceipt]
)

enum AlderCorrelationCentroidError:
  case Admission(error: AlderPredictiveAdmissionError)
  case Evidence(error: EvidenceError)
  case Mvpa(error: MvpaError)
  case ValidationPopulationMismatch
  case TargetShape(stableKey: String)
  case UnknownTarget(stableKey: String, value: Double)
  case MissingClass(fold: Int)
  case DuplicateAssessment(stableKey: String)
  case MissingAssessment(stableKey: String)

object AlderCorrelationCentroid:
  def crossValidate[S <: multivar.core.SemanticSpace, K, M, Cov <: Coverage.Exact](
      rows: AlderMaterializedRows[M],
      design: ValidationDesign[S, K, Cov],
      coding: SwiftTargetCoding
  ): Either[AlderCorrelationCentroidError, AlderCorrelationCentroidResult] =
    for
      _ <- NativeAxisMapping.verify(design.samples.descriptor, rows.mapping, rows.mapping.declaredSource).left.map(AlderCorrelationCentroidError.Admission.apply)
      _ <- if rows.root.ids == rows.mapping.nativeIds then Right(()) else Left(AlderCorrelationCentroidError.ValidationPopulationMismatch)
      result <- crossValidateWith(rows, design, coding, CorrelationCentroidClassifier(), "scalafim.correlation-centroid")
    yield result

  /** Shared Alder fit/OOF lifecycle for dense classifiers.  Public heads keep
    * their own named results, while all fitting remains in `FitContext.complete`.
    */
  private[predictive] def crossValidateWith[S <: multivar.core.SemanticSpace, K, M, Cov <: Coverage.Exact](
      rows: AlderMaterializedRows[M], design: ValidationDesign[S, K, Cov], coding: SwiftTargetCoding,
      classifier: Classifier, componentId: String, fingerprintParameters: Vector[String] = Vector.empty
  ): Either[AlderCorrelationCentroidError, AlderCorrelationCentroidResult] =
    for
      _ <- NativeAxisMapping.verify(design.samples.descriptor, rows.mapping, rows.mapping.declaredSource).left.map(AlderCorrelationCentroidError.Admission.apply)
      _ <- if rows.root.ids == rows.mapping.nativeIds then Right(()) else Left(AlderCorrelationCentroidError.ValidationPopulationMismatch)
      result <- evaluate(rows, design, coding, classifier, componentId, fingerprintParameters)
    yield result

  private def evaluate[S <: multivar.core.SemanticSpace, K, M, Cov <: Coverage.Exact](
      rows: AlderMaterializedRows[M], design: ValidationDesign[S, K, Cov], coding: SwiftTargetCoding,
      classifier: Classifier, componentId: String, fingerprintParameters: Vector[String]
  ): Either[AlderCorrelationCentroidError, AlderCorrelationCentroidResult] =
    val probabilitySums = Matrix.newBuilder(design.samples.size, coding.classes.length)
    val assessment = Array.fill[Option[ClassLabel]](design.samples.size)(None)
    val contributions = Array.fill(design.samples.size)(Vector.empty[AssessmentContribution])
    val assessmentCounts = Array.fill(design.samples.size)(0)
    val fits = Vector.newBuilder[CorrelationCentroidFoldFit]
    val ordinalByNative = rows.mapping.nativeIds.zipWithIndex.toMap
    var fold = 0
    var failure: Option[AlderCorrelationCentroidError] = None
    while fold < design.keys.length && failure.isEmpty do
      design.at(design.keys(fold)) match
        case Left(error) => failure = Some(AlderCorrelationCentroidError.Evidence(error))
        case Right(unit) =>
          val trainIds = unit.analysis.ordinals.toVector.map(rows.mapping.nativeIds)
          val testIds = unit.assessment.ordinals.toVector.map(rows.mapping.nativeIds)
          rows.fixedHoldout(trainIds, testIds, FixedCoverage.Exhaustive) match
            case Left(error) => failure = Some(AlderCorrelationCentroidError.Admission(error))
            case Right(split) =>
              val context = FitContext.root(
                Seed(design.receipt.seed.value),
                PlanFingerprint(AxisDigest.sha256Hex: writer =>
                  writer.string("scalafim.correlation-centroid-fold.v1")
                  writer.string(componentId)
                  writer.string(classifier.name)
                  fingerprintParameters.foreach(writer.string)
                  writer.string(rows.root.fingerprint.digest)
                  writer.string(design.samples.descriptor.stableKey)
                  writer.intLE(unit.key.repeat)
                  writer.intLE(unit.key.fold)
                  coding.values.foreach((value, label) =>
                    writer.string(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(value)))
                    writer.string(label.value))
                ),
                SchemaFingerprint("scalafim.correlation-centroid.v1"), NumericMode.Deterministic
              )
              new CorrelationLearner[M](rows, coding, fold, classifier, componentId, fingerprintParameters).fit(split.train)(using context).value match
                case Left(value) => failure = Some(value.cause)
                case Right(trained) =>
                  val trainKeys = keys(split.train.data, rows)
                  fits += CorrelationCentroidFoldFit(unit.key, trainKeys, trained.artifact.model.classes, trained.audit)
                  split.test.data.foreachRow: (id, example) =>
                    if failure.isEmpty then
                      trained.artifact.run(example.input) match
                        case Left(error) => failure = Some(error.cause)
                        case Right(prediction) =>
                          val ordinal = ordinalByNative(id.value)
                          val stableKey = rows.mapping.entriesByOrdinal(ordinal).stableKey
                          Classification.reorderProbabilities(prediction.classes, prediction.probabilities, coding.classes) match
                            case Left(error) => failure = Some(AlderCorrelationCentroidError.Mvpa(error))
                            case Right(probabilities) =>
                              if example.target.length != 1 then failure = Some(AlderCorrelationCentroidError.TargetShape(stableKey))
                              else coding.label(example.target(0)) match
                                case None => failure = Some(AlderCorrelationCentroidError.UnknownTarget(stableKey, example.target(0)))
                                case Some(observed) =>
                                  assessment(ordinal) match
                                    case Some(prior) if prior != observed => failure = Some(AlderCorrelationCentroidError.UnknownTarget(stableKey, example.target(0)))
                                    case _ =>
                                      var column = 0
                                      while column < coding.classes.length do
                                        probabilitySums(ordinal, column) = probabilitySums(ordinal, column) + probabilities(0, column)
                                        column += 1
                                      assessmentCounts(ordinal) += 1
                                      assessment(ordinal) = Some(observed)
                                      contributions(ordinal) = contributions(ordinal) :+ AssessmentContribution(unit.key, trainKeys)
      fold += 1
    failure.orElse(assessmentCounts.zipWithIndex.collectFirst { case (0, ordinal) => AlderCorrelationCentroidError.MissingAssessment(rows.mapping.entriesByOrdinal(ordinal).stableKey) }) match
      case Some(error) => Left(error)
      case None =>
        val completed = Vector.tabulate(design.samples.size): ordinal =>
          val values = Vector.tabulate(coding.classes.length): column =>
            probabilitySums(ordinal, column) / assessmentCounts(ordinal)
          val observed = assessment(ordinal).get
          var best = 0
          var candidate = 1
          while candidate < values.length do
            if values(candidate) > values(best) then best = candidate
            candidate += 1
          SwiftOofRow(rows.mapping.entriesByOrdinal(ordinal).stableKey, values, coding.classes(best), observed, contributions(ordinal))
        val index = coding.classes.zipWithIndex.toMap
        val confusion = Array.fill(coding.classes.length, coding.classes.length)(0L)
        completed.foreach(row => confusion(index(row.observed))(index(row.predicted)) += 1L)
        val summary = SwiftAssessment(completed.count(row => row.observed == row.predicted).toLong, completed.length.toLong, confusion.toVector.map(_.toVector))
        val output = Matrix.newBuilder(design.samples.size, coding.classes.length)
        completed.zipWithIndex.foreach: (row, ordinal) =>
          row.probabilities.zipWithIndex.foreach: (value, column) =>
            output(ordinal, column) = value
        Right(AlderCorrelationCentroidResult(coding.classes, output.result(), completed, design.receipt, fits.result(), summary, rows.receipt, rows.nativeReadReceipt))

  private def keys[M](data: Data[?, Example[Array[Double], Array[Double], M]], rows: AlderMaterializedRows[M]): Vector[String] =
    data.foldRows(Vector.empty[String])((out, id, _) => out :+ rows.mapping.entriesByOrdinal(rows.mapping.nativeIds.indexOf(id.value)).stableKey)

  private final class CorrelationLearner[M](rows: AlderMaterializedRows[M], coding: SwiftTargetCoding, fold: Int, classifier: Classifier, componentId: String, parameters: Vector[String])
      extends Learner[Id, Array[Double], Array[Double], M, CategoricalProbabilities]:
    type FitError = AlderCorrelationCentroidError
    type RunError = AlderCorrelationCentroidError
    type Model = CorrelationPipe
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[Array[Double], Array[Double], M]])(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      materialize(data.data, rows, coding).flatMap { case (patterns, labels) =>
        if labels.distinct.length != coding.classes.length then Left(AlderCorrelationCentroidError.MissingClass(fold))
        else classifier.fit(patterns, labels).left.map(AlderCorrelationCentroidError.Mvpa.apply)
      } match
        case Left(error) => EitherT.leftT(context.stagePath.failure(error))
        case Right(model) => EitherT.rightT(context.complete(new CorrelationPipe(model, context.stagePath), data,
          ComponentDescriptor(ComponentId(componentId), ComponentVersion("1"), AuditValue.record("parameters" -> AuditValue.text(parameters.mkString(";")), "classOrder" -> AuditValue.text(coding.classes.map(_.value).mkString(";"))), BackendFingerprint("scalafim", "1", AuditValue.record()))))

  private final class CorrelationPipe(val model: ClassifierModel, stage: StagePath)
      extends Pipe[Array[Double], AlderCorrelationCentroidError, CategoricalProbabilities]:
    def run(input: Array[Double]): Either[Failure[AlderCorrelationCentroidError], CategoricalProbabilities] =
      model.predict(DMat.dense(1, input.length, input.toVector)).left.map(error => stage.failure(AlderCorrelationCentroidError.Mvpa(error)))

  private def materialize[M](data: Data[?, Example[Array[Double], Array[Double], M]], rows: AlderMaterializedRows[M], coding: SwiftTargetCoding): Either[AlderCorrelationCentroidError, (DMat, Vector[ClassLabel])] =
    val inputs = Vector.newBuilder[Vector[Double]]
    val labels = Vector.newBuilder[ClassLabel]
    var error: Option[AlderCorrelationCentroidError] = None
    data.foreachRow: (id, example) =>
      if error.isEmpty then
        val ordinal = rows.mapping.nativeIds.indexOf(id.value)
        if ordinal < 0 then error = Some(AlderCorrelationCentroidError.Admission(AlderPredictiveAdmissionError.CrossFitPopulationMismatch))
        else if example.target.length != 1 then error = Some(AlderCorrelationCentroidError.TargetShape(rows.mapping.entriesByOrdinal(ordinal).stableKey))
        else coding.label(example.target(0)) match
          case None => error = Some(AlderCorrelationCentroidError.UnknownTarget(rows.mapping.entriesByOrdinal(ordinal).stableKey, example.target(0)))
          case Some(label) => inputs += example.input.toVector; labels += label
    error.toLeft((dense(inputs.result()), labels.result()))

  private def dense(rows: Vector[Vector[Double]]): DMat =
    val columns = rows.headOption.fold(0)(_.length)
    if rows.exists(_.length != columns) then
      throw new IllegalArgumentException("materialized categorical rows must have a common width")
    DMat.dense(rows.length, columns, rows.flatten)
