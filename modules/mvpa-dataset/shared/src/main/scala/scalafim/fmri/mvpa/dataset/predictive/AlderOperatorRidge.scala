package scalafim.fmri.mvpa.dataset.predictive

import alder.data.FixedCoverage
import alder.kernel.*
import cats.Id
import cats.data.EitherT
import gale.linalg.{DMat, Matrix}
import resample4s.core.Coverage
import scalafim.fmri.mvpa.*

/** A non-materializing admission receipt.  Operator values remain behind the
  * admitted PatternOperator; only one categorical target is retained per row. */
final case class OperatorRidgeAdmissionReceipt(rows: Int, targetCells: Long, maximumCells: Long)

final class AlderOperatorRidgeRows private[predictive] (
    val root: alder.data.IdentifiedRows[Example[AlderOperatorRidgeRow, Array[Double], Unit]],
    val mapping: NativeAxisMapping,
    val operator: PatternOperator,
    val receipt: OperatorRidgeAdmissionReceipt,
    val softClasses: Option[Vector[ClassLabel]]
)

final case class AlderOperatorRidgeRow private[predictive] (nativeId: Long, ordinal: Int)

final case class AlderOperatorRidgeFoldFit(
    unit: resample4s.core.UnitKey,
    trainingStableKeys: Vector[String],
    audit: Audit,
    model: OperatorRidgeModel
)

final case class AlderOperatorRidgeResult(
    prediction: OperatorRidgePrediction,
    validationReceipt: resample4s.core.PlanReceipt,
    fits: Vector[AlderOperatorRidgeFoldFit],
    admission: OperatorRidgeAdmissionReceipt
)

enum AlderOperatorRidgeError:
  case Admission(error: AlderPredictiveAdmissionError)
  case Evidence(error: EvidenceError)
  case Operator(error: OperatorRidgeError)
  case Mvpa(error: MvpaError)
  case TargetShape(stableKey: String)
  case UnknownTarget(stableKey: String, value: Double)
  case MissingClass(fold: Int)
  case DuplicateAssessment(stableKey: String)
  case MissingAssessment(stableKey: String)
  case OperatorSampleCount(expected: Int, actual: Int)
  case OperatorRowMismatch(nativeId: Long, sampleOrdinal: Int)
  case Budget(required: Long, maximum: Long)
  case CodingMismatch(expected: Vector[ClassLabel], actual: Vector[ClassLabel])

/** Alder lifecycle for OperatorRidge.  It stores no dense pattern matrix: the
  * learner derives its operator restriction solely from the Fit-role rows. */
object AlderOperatorRidge:
  def admit(
      operator: PatternOperator,
      targets: Vector[Double],
      mapping: NativeAxisMapping,
      budget: MaterializationBudget
  ): Either[AlderOperatorRidgeError, AlderOperatorRidgeRows] =
    for
      _ <- NativeAxisMapping.verify(mapping.axis, mapping, mapping.declaredSource).left.map(AlderOperatorRidgeError.Admission.apply)
      _ <- if operator.samples == mapping.axis.size then Right(()) else Left(AlderOperatorRidgeError.OperatorSampleCount(mapping.axis.size, operator.samples))
      _ <- if targets.length == mapping.axis.size then Right(()) else Left(AlderOperatorRidgeError.Admission(AlderPredictiveAdmissionError.MatrixShapeMismatch("targets", targets.length, 1, mapping.axis.size, 1)))
      required = targets.length.toLong
      _ <- if required <= budget.maximumCells then Right(()) else Left(AlderOperatorRidgeError.Budget(required, budget.maximumCells))
      root <- alder.data.IdentifiedRows.fromRows(
        mapping.nativeIds.zipWithIndex.map((id, ordinal) => id -> Example(AlderOperatorRidgeRow(id, ordinal), Array(targets(ordinal)), ())),
        mapping.declaredMappingIdentity
      ).left.map(error => AlderOperatorRidgeError.Admission(AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString)))
    yield new AlderOperatorRidgeRows(root, mapping, operator, OperatorRidgeAdmissionReceipt(targets.length, required, budget.maximumCells), None)

  /** Admits simplex memberships without reading or materializing operator
    * features. The learner later restricts these memberships to Fit rows. */
  def admitSoft(operator: PatternOperator, membership: ClassMembership, mapping: NativeAxisMapping, budget: MaterializationBudget): Either[AlderOperatorRidgeError, AlderOperatorRidgeRows] =
    for
      _ <- NativeAxisMapping.verify(mapping.axis, mapping, mapping.declaredSource).left.map(AlderOperatorRidgeError.Admission.apply)
      _ <- if operator.samples == mapping.axis.size && membership.samples == mapping.axis.size then Right(()) else Left(AlderOperatorRidgeError.OperatorSampleCount(mapping.axis.size, operator.samples))
      required = membership.samples.toLong * membership.classCount.toLong
      _ <- if required <= budget.maximumCells then Right(()) else Left(AlderOperatorRidgeError.Budget(required, budget.maximumCells))
      root <- alder.data.IdentifiedRows.fromRows(
        mapping.nativeIds.zipWithIndex.map((id, ordinal) => id -> Example(AlderOperatorRidgeRow(id, ordinal), Array.tabulate(membership.classCount)(klass => membership.values(ordinal, klass)), ())),
        mapping.declaredMappingIdentity
      ).left.map(error => AlderOperatorRidgeError.Admission(AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString)))
    yield new AlderOperatorRidgeRows(root, mapping, operator, OperatorRidgeAdmissionReceipt(membership.samples, required, budget.maximumCells), Some(membership.classes))

  def crossValidate[S <: multivar.core.SemanticSpace, K](
      rows: AlderOperatorRidgeRows,
      design: ValidationDesign[S, K, Coverage.ExactOnce],
      coding: SwiftTargetCoding,
      config: OperatorRidgeConfig
  ): Either[AlderOperatorRidgeError, AlderOperatorRidgeResult] =
    for
      _ <- NativeAxisMapping.verify(design.samples.descriptor, rows.mapping, rows.mapping.declaredSource).left.map(AlderOperatorRidgeError.Admission.apply)
      _ <- if rows.root.ids == rows.mapping.nativeIds then Right(()) else Left(AlderOperatorRidgeError.Admission(AlderPredictiveAdmissionError.CrossFitPopulationMismatch))
      _ <- rows.softClasses.fold[Either[AlderOperatorRidgeError, Unit]](Right(()))(classes => if classes == coding.classes then Right(()) else Left(AlderOperatorRidgeError.CodingMismatch(coding.classes, classes)))
      result <- evaluate(rows, design, coding, config)
    yield result

  private def evaluate[S <: multivar.core.SemanticSpace, K](rows: AlderOperatorRidgeRows, design: ValidationDesign[S, K, Coverage.ExactOnce], coding: SwiftTargetCoding, config: OperatorRidgeConfig): Either[AlderOperatorRidgeError, AlderOperatorRidgeResult] =
    val scores = Matrix.newBuilder(design.samples.size, coding.classes.length)
    val assembled = Array.fill[Option[OperatorRidgePrediction]](design.samples.size)(None)
    val fits = Vector.newBuilder[AlderOperatorRidgeFoldFit]
    var fold = 0
    var failure = Option.empty[AlderOperatorRidgeError]
    while fold < design.keys.length && failure.isEmpty do
      design.at(design.keys(fold)) match
        case Left(error) => failure = Some(AlderOperatorRidgeError.Evidence(error))
        case Right(unit) =>
          val trainIds = unit.analysis.ordinals.toVector.map(rows.mapping.nativeIds)
          val testIds = unit.assessment.ordinals.toVector.map(rows.mapping.nativeIds)
          rows.root.fixedHoldout(trainIds, testIds, FixedCoverage.Exhaustive) match
            case Left(error) => failure = Some(AlderOperatorRidgeError.Admission(AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString)))
            case Right(split) =>
              val context = FitContext.root(Seed(design.receipt.seed.value), PlanFingerprint(AxisDigest.sha256Hex: writer =>
                writer.string("scalafim.operator-ridge-fold.v1"); writer.string(rows.root.fingerprint.digest)
                writer.intLE(unit.key.repeat); writer.intLE(unit.key.fold)
                writer.string(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(config.penalty.value)))
                writer.string(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(config.tolerance.value)))
                writer.intLE(config.maxIterations.value)
                coding.values.foreach((value, label) =>
                  writer.string(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(value))); writer.string(label.value))
              ), SchemaFingerprint("scalafim.operator-ridge.v1"), NumericMode.Deterministic)
              new RidgeLearner(rows, coding, config, fold).fit(split.train)(using context).value match
                case Left(error) => failure = Some(error.cause)
                case Right(trained) =>
                  fits += AlderOperatorRidgeFoldFit(unit.key, stableKeys(split.train.data, rows), trained.audit, trained.artifact.model)
                  split.test.data.foreachRow: (id, example) =>
                    if failure.isEmpty then
                      trained.artifact.run(example.input) match
                        case Left(error) => failure = Some(error.cause)
                        case Right(prediction) =>
                          val ordinal = example.input.ordinal
                          if id.value != rows.mapping.nativeIds(ordinal) then failure = Some(AlderOperatorRidgeError.OperatorRowMismatch(id.value, ordinal))
                          else if assembled(ordinal).nonEmpty then failure = Some(AlderOperatorRidgeError.DuplicateAssessment(rows.mapping.entriesByOrdinal(ordinal).stableKey))
                          else if prediction.classes.toSet != coding.classes.toSet then failure = Some(AlderOperatorRidgeError.CodingMismatch(coding.classes, prediction.classes))
                          else
                            var column = 0
                            while column < coding.classes.length do
                              scores(ordinal, column) = prediction.scores(0, prediction.classes.indexOf(coding.classes(column)))
                              column += 1
                            assembled(ordinal) = Some(prediction)
      fold += 1
    failure.orElse(assembled.zipWithIndex.collectFirst { case (None, ordinal) => AlderOperatorRidgeError.MissingAssessment(rows.mapping.entriesByOrdinal(ordinal).stableKey) }) match
      case Some(error) => Left(error)
      case None => Right(AlderOperatorRidgeResult(OperatorRidgePrediction(coding.classes, scores.result(), Vector.tabulate(design.samples.size)(SampleIndex.apply)), design.receipt, fits.result(), rows.receipt))

  private final class RidgeLearner(rows: AlderOperatorRidgeRows, coding: SwiftTargetCoding, config: OperatorRidgeConfig, fold: Int)
      extends Learner[Id, AlderOperatorRidgeRow, Array[Double], Unit, OperatorRidgePrediction]:
    type FitError = AlderOperatorRidgeError
    type RunError = AlderOperatorRidgeError
    type Model = RidgePipe
    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[AlderOperatorRidgeRow, Array[Double], Unit]])(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      training(data.data, rows, coding).flatMap { case (ordinals, labels, soft) =>
        val hasAllClasses = rows.softClasses match
          case None => labels.distinct.length == coding.classes.length
          case Some(classes) => classes.indices.forall(klass => soft.exists(values => values(klass) > 0.0))
        if !hasAllClasses then Left(AlderOperatorRidgeError.MissingClass(fold))
        else for
          operator <- rows.operator.selectRowPositions(ordinals).left.map(AlderOperatorRidgeError.Mvpa.apply)
          membership <- rows.softClasses match
            case None => ClassMembership.hard(labels).left.map(AlderOperatorRidgeError.Mvpa.apply)
            case Some(classes) => ClassMembership.simplex(classes, DMat.dense(soft.length, classes.length, soft.flatten)).left.map(AlderOperatorRidgeError.Mvpa.apply)
          model <- OperatorRidge.fit(operator, membership, config).left.map(AlderOperatorRidgeError.Operator.apply)
        yield model
      } match
        case Left(error) => EitherT.leftT(context.stagePath.failure(error))
        case Right(model) => EitherT.rightT(context.complete(new RidgePipe(rows.operator, model, context.stagePath), data,
          ComponentDescriptor(ComponentId("scalafim.operator-ridge"), ComponentVersion("1"), AuditValue.record(
            "penalty" -> AuditValue.text(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(config.penalty.value))),
            "tolerance" -> AuditValue.text(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(config.tolerance.value))),
            "maxIterations" -> AuditValue.text(config.maxIterations.value.toString),
            "classes" -> AuditValue.sequence(coding.classes.map(label => AuditValue.text(label.value))*)
          ), BackendFingerprint("scalafim", "1", AuditValue.record()))))

  private final class RidgePipe(operator: PatternOperator, val model: OperatorRidgeModel, stage: StagePath) extends Pipe[AlderOperatorRidgeRow, AlderOperatorRidgeError, OperatorRidgePrediction]:
    def run(input: AlderOperatorRidgeRow): Either[Failure[AlderOperatorRidgeError], OperatorRidgePrediction] =
      operator.selectRowPositions(Vector(input.ordinal)).left.map(error => stage.failure(AlderOperatorRidgeError.Mvpa(error))).flatMap: selected =>
        model.predict(selected).left.map(error => stage.failure(AlderOperatorRidgeError.Operator(error)))

  private def training(data: Data[?, Example[AlderOperatorRidgeRow, Array[Double], Unit]], rows: AlderOperatorRidgeRows, coding: SwiftTargetCoding): Either[AlderOperatorRidgeError, (Vector[Int], Vector[ClassLabel], Vector[Vector[Double]])] =
    val ordinals = Vector.newBuilder[Int]
    val labels = Vector.newBuilder[ClassLabel]
    val soft = Vector.newBuilder[Vector[Double]]
    var failure = Option.empty[AlderOperatorRidgeError]
    data.foreachRow: (id, example) =>
      if failure.isEmpty then
        val ordinal = example.input.ordinal
        if ordinal < 0 || ordinal >= rows.mapping.nativeIds.length || id.value != example.input.nativeId || id.value != rows.mapping.nativeIds(ordinal) then failure = Some(AlderOperatorRidgeError.OperatorRowMismatch(id.value, ordinal))
        else rows.softClasses match
          case Some(classes) if example.target.length == classes.length =>
            val values = example.target.toVector
            val label = classes(values.indices.maxBy(values))
            ordinals += ordinal; labels += label; soft += values
          case Some(_) => failure = Some(AlderOperatorRidgeError.TargetShape(rows.mapping.entriesByOrdinal(ordinal).stableKey))
          case None if example.target.length != 1 => failure = Some(AlderOperatorRidgeError.TargetShape(rows.mapping.entriesByOrdinal(ordinal).stableKey))
          case None => coding.label(example.target(0)) match
            case None => failure = Some(AlderOperatorRidgeError.UnknownTarget(rows.mapping.entriesByOrdinal(ordinal).stableKey, example.target(0)))
            case Some(label) => ordinals += ordinal; labels += label
    failure.toLeft((ordinals.result(), labels.result(), soft.result()))

  private def stableKeys(data: Data[?, Example[AlderOperatorRidgeRow, Array[Double], Unit]], rows: AlderOperatorRidgeRows): Vector[String] =
    data.foldRows(Vector.empty[String])((out, _, example) => out :+ rows.mapping.entriesByOrdinal(example.input.ordinal).stableKey)
