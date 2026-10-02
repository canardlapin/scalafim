package scalafim.fmri.mvpa.dataset.predictive

import alder.data.FixedCoverage
import alder.kernel.*
import cats.Id
import cats.data.EitherT
import gale.linalg.{DMat, Matrix}
import multivar.family.canonical.TrialNuisanceDesign
import resample4s.core.Coverage
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.fit.*

/** Admission stores only response metadata. Pattern values remain behind the
  * operator and are restricted from Fit-role rows by the learner. */
final case class SoftLdaAdmissionReceipt(rows: Int, targetCells: Long, nuisanceCells: Long, maximumCells: Long)

final class AlderSoftLdaRows private[predictive] (
    val root: alder.data.IdentifiedRows[Example[AlderSoftLdaRow, Array[Double], Unit]],
    val mapping: NativeAxisMapping,
    val operator: PatternOperator,
    val receipt: SoftLdaAdmissionReceipt,
    val softClasses: Option[Vector[ClassLabel]],
    val trialNuisance: Option[TrialNuisanceDesign]
)

final case class AlderSoftLdaRow private[predictive] (nativeId: Long, ordinal: Int)

final case class AlderSoftLdaFoldFit(
    unit: resample4s.core.UnitKey,
    trainingStableKeys: Vector[String],
    audit: Audit,
    model: SoftLdaModel
)

final case class AlderSoftLdaResult(
    prediction: ClassificationPrediction,
    targetMse: Double,
    targetArgmaxAccuracy: Double,
    validationReceipt: resample4s.core.PlanReceipt,
    fits: Vector[AlderSoftLdaFoldFit],
    admission: SoftLdaAdmissionReceipt
)

enum AlderSoftLdaError:
  case Admission(error: AlderPredictiveAdmissionError)
  case Evidence(error: EvidenceError)
  case Mvpa(error: MvpaError)
  case SoftLda(error: SoftLdaError)
  case TargetShape(stableKey: String)
  case UnknownTarget(stableKey: String, value: Double)
  case MissingClass(fold: Int)
  case DuplicateAssessment(stableKey: String)
  case MissingAssessment(stableKey: String)
  case OperatorSampleCount(expected: Int, actual: Int)
  case OperatorRowMismatch(nativeId: Long, sampleOrdinal: Int)
  case Budget(required: Long, maximum: Long)
  case CodingMismatch(expected: Vector[ClassLabel], actual: Vector[ClassLabel])
  case ConfiguredNuisanceMustBeAdmitted

/** Native Alder lifecycle for soft LDA.  It deliberately fits the extracted
  * single-fit numerical kernel. */
object AlderSoftLda:
  def admit(
      operator: PatternOperator,
      targets: Vector[Double],
      mapping: NativeAxisMapping,
      budget: MaterializationBudget,
      trialNuisance: Option[TrialNuisanceDesign] = None
  ): Either[AlderSoftLdaError, AlderSoftLdaRows] =
    val nuisanceCells = trialNuisance.fold(0L)(design => design.samples.toLong * design.columns.toLong)
    for
      _ <- NativeAxisMapping.verify(mapping.axis, mapping, mapping.declaredSource).left.map(AlderSoftLdaError.Admission.apply)
      _ <- if operator.samples == mapping.axis.size then Right(()) else Left(AlderSoftLdaError.OperatorSampleCount(mapping.axis.size, operator.samples))
      _ <- if targets.length == mapping.axis.size then Right(()) else Left(AlderSoftLdaError.Admission(AlderPredictiveAdmissionError.MatrixShapeMismatch("targets", targets.length, 1, mapping.axis.size, 1)))
      _ <- if trialNuisance.forall(_.samples == mapping.axis.size) then Right(()) else Left(AlderSoftLdaError.Admission(AlderPredictiveAdmissionError.MatrixShapeMismatch("trial nuisance", trialNuisance.fold(0)(_.samples), trialNuisance.fold(0)(_.columns), mapping.axis.size, -1)))
      required = targets.length.toLong + nuisanceCells
      _ <- if required <= budget.maximumCells then Right(()) else Left(AlderSoftLdaError.Budget(required, budget.maximumCells))
      root <- alder.data.IdentifiedRows.fromRows(
        mapping.nativeIds.zipWithIndex.map((id, ordinal) => id -> Example(AlderSoftLdaRow(id, ordinal), Array(targets(ordinal)), ())),
        mapping.declaredMappingIdentity
      ).left.map(error => AlderSoftLdaError.Admission(AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString)))
    yield new AlderSoftLdaRows(root, mapping, operator, SoftLdaAdmissionReceipt(targets.length, targets.length.toLong, nuisanceCells, budget.maximumCells), None, trialNuisance)

  def admitSoft(
      operator: PatternOperator,
      membership: ClassMembership,
      mapping: NativeAxisMapping,
      budget: MaterializationBudget,
      trialNuisance: Option[TrialNuisanceDesign] = None
  ): Either[AlderSoftLdaError, AlderSoftLdaRows] =
    val nuisanceCells = trialNuisance.fold(0L)(design => design.samples.toLong * design.columns.toLong)
    for
      _ <- NativeAxisMapping.verify(mapping.axis, mapping, mapping.declaredSource).left.map(AlderSoftLdaError.Admission.apply)
      _ <- if operator.samples == mapping.axis.size && membership.samples == mapping.axis.size then Right(()) else Left(AlderSoftLdaError.OperatorSampleCount(mapping.axis.size, operator.samples))
      _ <- if trialNuisance.forall(_.samples == mapping.axis.size) then Right(()) else Left(AlderSoftLdaError.Admission(AlderPredictiveAdmissionError.MatrixShapeMismatch("trial nuisance", trialNuisance.fold(0)(_.samples), trialNuisance.fold(0)(_.columns), mapping.axis.size, -1)))
      targetCells = membership.samples.toLong * membership.classCount.toLong
      required = targetCells + nuisanceCells
      _ <- if required <= budget.maximumCells then Right(()) else Left(AlderSoftLdaError.Budget(required, budget.maximumCells))
      root <- alder.data.IdentifiedRows.fromRows(
        mapping.nativeIds.zipWithIndex.map((id, ordinal) => id -> Example(AlderSoftLdaRow(id, ordinal), Array.tabulate(membership.classCount)(klass => membership.values(ordinal, klass)), ())),
        mapping.declaredMappingIdentity
      ).left.map(error => AlderSoftLdaError.Admission(AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString)))
    yield new AlderSoftLdaRows(root, mapping, operator, SoftLdaAdmissionReceipt(membership.samples, targetCells, nuisanceCells, budget.maximumCells), Some(membership.classes), trialNuisance)

  def crossValidate[S <: multivar.core.SemanticSpace, K](
      rows: AlderSoftLdaRows,
      design: ValidationDesign[S, K, Coverage.ExactOnce],
      coding: SwiftTargetCoding,
      config: SoftLdaConfig
  ): Either[AlderSoftLdaError, AlderSoftLdaResult] =
    for
      _ <- NativeAxisMapping.verify(design.samples.descriptor, rows.mapping, rows.mapping.declaredSource).left.map(AlderSoftLdaError.Admission.apply)
      _ <- if rows.root.ids == rows.mapping.nativeIds then Right(()) else Left(AlderSoftLdaError.Admission(AlderPredictiveAdmissionError.CrossFitPopulationMismatch))
      _ <- rows.softClasses.fold[Either[AlderSoftLdaError, Unit]](Right(()))(classes => if classes == coding.classes then Right(()) else Left(AlderSoftLdaError.CodingMismatch(classes, coding.classes)))
      _ <- if config.trialNuisance.isEmpty then Right(()) else Left(AlderSoftLdaError.ConfiguredNuisanceMustBeAdmitted)
      result <- evaluate(rows, design, coding, config)
    yield result

  private def evaluate[S <: multivar.core.SemanticSpace, K](rows: AlderSoftLdaRows, design: ValidationDesign[S, K, Coverage.ExactOnce], coding: SwiftTargetCoding, config: SoftLdaConfig): Either[AlderSoftLdaError, AlderSoftLdaResult] =
    val output = Matrix.newBuilder(design.samples.size, coding.classes.length)
    val assembled = Array.fill[Option[ClassificationPrediction]](design.samples.size)(None)
    val fits = Vector.newBuilder[AlderSoftLdaFoldFit]
    var fold = 0
    var failure = Option.empty[AlderSoftLdaError]
    while fold < design.keys.length && failure.isEmpty do
      design.at(design.keys(fold)) match
        case Left(error) => failure = Some(AlderSoftLdaError.Evidence(error))
        case Right(unit) =>
          val trainIds = unit.analysis.ordinals.toVector.map(rows.mapping.nativeIds)
          val testIds = unit.assessment.ordinals.toVector.map(rows.mapping.nativeIds)
          rows.root.fixedHoldout(trainIds, testIds, FixedCoverage.Exhaustive) match
            case Left(error) => failure = Some(AlderSoftLdaError.Admission(AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString)))
            case Right(split) =>
              val context = fitContext(rows, design, unit.key, coding, config)
              new SoftLdaLearner(rows, coding, config, fold).fit(split.train)(using context).value match
                case Left(error) => failure = Some(error.cause)
                case Right(trained) =>
                  val trainingKeys = stableKeys(split.train.data, rows)
                  fits += AlderSoftLdaFoldFit(unit.key, trainingKeys, trained.audit, trained.artifact.model)
                  split.test.data.foreachRow: (id, example) =>
                    if failure.isEmpty then
                      trained.artifact.run(example.input) match
                        case Left(error) => failure = Some(error.cause)
                        case Right(prediction) =>
                          val ordinal = example.input.ordinal
                          if id.value != rows.mapping.nativeIds(ordinal) then failure = Some(AlderSoftLdaError.OperatorRowMismatch(id.value, ordinal))
                          else if assembled(ordinal).nonEmpty then failure = Some(AlderSoftLdaError.DuplicateAssessment(rows.mapping.entriesByOrdinal(ordinal).stableKey))
                          else Classification.reorderProbabilities(prediction.classes, prediction.probabilities, coding.classes) match
                            case Left(error) => failure = Some(AlderSoftLdaError.Mvpa(error))
                            case Right(probabilities) =>
                              var column = 0
                              while column < coding.classes.length do
                                output(ordinal, column) = probabilities(0, column)
                                column += 1
                              assembled(ordinal) = Some(prediction)
      fold += 1
    failure.orElse(assembled.zipWithIndex.collectFirst { case (None, ordinal) => AlderSoftLdaError.MissingAssessment(rows.mapping.entriesByOrdinal(ordinal).stableKey) }) match
      case Some(error) => Left(error)
      case None =>
        val prediction = ClassificationPrediction(coding.classes, output.result(), Vector.tabulate(design.samples.size)(SampleIndex.apply))
        targetMembership(rows, coding).map: target =>
          val (mse, accuracy) = metrics(prediction, target)
          AlderSoftLdaResult(prediction, mse, accuracy, design.receipt, fits.result(), rows.receipt)

  private final class SoftLdaLearner(rows: AlderSoftLdaRows, coding: SwiftTargetCoding, config: SoftLdaConfig, fold: Int)
      extends Learner[Id, AlderSoftLdaRow, Array[Double], Unit, ClassificationPrediction]:
    type FitError = AlderSoftLdaError
    type RunError = AlderSoftLdaError
    type Model = SoftLdaPipe

    def fit[U <: Use.Fit](data: NonEmptyData[U, Example[AlderSoftLdaRow, Array[Double], Unit]])(using context: FitContext): FitResult[Id, FitError, Trained[Model]] =
      (training(data.data, rows, coding, fold).flatMap: (ordinals, membership) =>
        for
          nuisance <- rows.trialNuisance match
            case None => Right(None)
            case Some(design) => TrialNuisanceDesign.from(selectRows(design.values, ordinals)).left.map(error => AlderSoftLdaError.SoftLda(SoftLdaError.LdaFailure(s"alder-soft-lda-$fold", error))).map(Some.apply)
          operator <- rows.operator.selectRowPositions(ordinals).left.map(AlderSoftLdaError.Mvpa.apply)
          model <- SoftLda.fit(operator, membership, config.copy(trialNuisance = nuisance), s"alder-soft-lda-$fold", fold).left.map(AlderSoftLdaError.SoftLda.apply)
        yield model
      ) match
        case Left(error) => EitherT.leftT(context.stagePath.failure(error))
        case Right(model) => EitherT.rightT(context.complete(new SoftLdaPipe(rows.operator, model, context.stagePath), data,
          ComponentDescriptor(ComponentId("scalafim.soft-lda"), ComponentVersion("1"), auditConfig(config, coding, rows), BackendFingerprint("scalafim", "1", AuditValue.record()))))

  private final class SoftLdaPipe(operator: PatternOperator, val model: SoftLdaModel, stage: StagePath)
      extends Pipe[AlderSoftLdaRow, AlderSoftLdaError, ClassificationPrediction]:
    def run(input: AlderSoftLdaRow): Either[Failure[AlderSoftLdaError], ClassificationPrediction] =
      operator.selectRowPositions(Vector(input.ordinal)).left.map(error => stage.failure(AlderSoftLdaError.Mvpa(error))).flatMap(selected => model.predict(selected).left.map(error => stage.failure(AlderSoftLdaError.SoftLda(error))))

  private def training(data: Data[?, Example[AlderSoftLdaRow, Array[Double], Unit]], rows: AlderSoftLdaRows, coding: SwiftTargetCoding, fold: Int): Either[AlderSoftLdaError, (Vector[Int], ClassMembership)] =
    val ordinals = Vector.newBuilder[Int]
    val values = Vector.newBuilder[Vector[Double]]
    val labels = Vector.newBuilder[ClassLabel]
    var failure = Option.empty[AlderSoftLdaError]
    data.foreachRow: (id, example) =>
      if failure.isEmpty then
        val ordinal = example.input.ordinal
        if ordinal < 0 || ordinal >= rows.mapping.nativeIds.length || id.value != example.input.nativeId || id.value != rows.mapping.nativeIds(ordinal) then failure = Some(AlderSoftLdaError.OperatorRowMismatch(id.value, ordinal))
        else rows.softClasses match
          case Some(classes) if example.target.length == classes.length => ordinals += ordinal; values += example.target.toVector
          case Some(_) => failure = Some(AlderSoftLdaError.TargetShape(rows.mapping.entriesByOrdinal(ordinal).stableKey))
          case None if example.target.length != 1 => failure = Some(AlderSoftLdaError.TargetShape(rows.mapping.entriesByOrdinal(ordinal).stableKey))
          case None => coding.label(example.target(0)) match
            case None => failure = Some(AlderSoftLdaError.UnknownTarget(rows.mapping.entriesByOrdinal(ordinal).stableKey, example.target(0)))
            case Some(label) => ordinals += ordinal; labels += label
    failure match
      case Some(error) => Left(error)
      case None =>
        val membershipValues = values.result()
        rows.softClasses match
          case None =>
            val hardLabels = labels.result()
            if hardLabels.distinct.length != coding.classes.length then Left(AlderSoftLdaError.MissingClass(fold))
            else ClassMembership.hard(hardLabels).left.map(AlderSoftLdaError.Mvpa.apply).map(ordinals.result() -> _)
          case Some(classes) =>
            val missing = classes.indices.exists(column => membershipValues.forall(_(column) <= 0.0))
            if missing then Left(AlderSoftLdaError.MissingClass(fold))
            else ClassMembership.simplex(classes, DMat.dense(membershipValues.length, classes.length, membershipValues.flatten)).left.map(AlderSoftLdaError.Mvpa.apply).map(ordinals.result() -> _)

  private def targetMembership(rows: AlderSoftLdaRows, coding: SwiftTargetCoding): Either[AlderSoftLdaError, ClassMembership] =
    rows.root
      .training(rows.root.ids)
      .left.map(error => AlderSoftLdaError.Admission(AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString)))
      .flatMap(all => training(all.data, rows, coding, -1).map(_._2))
      .flatMap: membership =>
        val index = membership.classes.zipWithIndex.toMap
        val values = Matrix.tabulate(membership.samples, coding.classes.length): (row, column) =>
          membership.values(row, index(coding.classes(column)))
        ClassMembership.simplex(coding.classes, values).left.map(AlderSoftLdaError.Mvpa.apply)

  private def fitContext[S <: multivar.core.SemanticSpace, K](rows: AlderSoftLdaRows, design: ValidationDesign[S, K, Coverage.ExactOnce], unit: resample4s.core.UnitKey, coding: SwiftTargetCoding, config: SoftLdaConfig): FitContext =
    FitContext.root(Seed(design.receipt.seed.value), PlanFingerprint(AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.soft-lda-fold.v1")
      writer.string(rows.root.fingerprint.digest)
      writer.string(rows.mapping.declaredSource.digest)
      writer.string(rows.mapping.axis.stableKey)
      writer.string(design.samples.descriptor.stableKey)
      writer.intLE(unit.repeat); writer.intLE(unit.fold)
      writer.string(config.withinPolicy.toString); writer.string(config.objective.toString); writer.string(config.components.toString)
      writer.string(nuisanceIdentity(rows))
      coding.values.foreach((value, label) => { writer.string(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(value))); writer.string(label.value) })
    ), SchemaFingerprint("scalafim.soft-lda.v1"), NumericMode.Deterministic)

  private def auditConfig(config: SoftLdaConfig, coding: SwiftTargetCoding, rows: AlderSoftLdaRows): AuditValue =
    AuditValue.record(
      "withinPolicy" -> AuditValue.text(config.withinPolicy.toString),
      "objective" -> AuditValue.text(config.objective.toString),
      "components" -> AuditValue.text(config.components.toString),
      "trialNuisanceColumns" -> AuditValue.text(rows.trialNuisance.fold(0)(_.columns).toString),
      "trialNuisanceIdentity" -> AuditValue.text(nuisanceIdentity(rows)),
      "declaredSource" -> AuditValue.text(rows.mapping.declaredSource.digest),
      "classes" -> AuditValue.sequence(coding.classes.map(label => AuditValue.text(label.value))*)
    )

  private def nuisanceIdentity(rows: AlderSoftLdaRows): String = AxisDigest.sha256Hex: writer =>
    writer.string("scalafim.soft-lda.nuisance.v1")
    rows.trialNuisance match
      case None => writer.string("none")
      case Some(design) =>
        writer.intLE(design.samples)
        writer.intLE(design.columns)
        var row = 0
        while row < design.samples do
          var column = 0
          while column < design.columns do
            writer.string(java.lang.Double.toHexString(design.values(row, column)))
            column += 1
          row += 1

  private def stableKeys(data: Data[?, Example[AlderSoftLdaRow, Array[Double], Unit]], rows: AlderSoftLdaRows): Vector[String] =
    data.foldRows(Vector.empty[String])((out, _, example) => out :+ rows.mapping.entriesByOrdinal(example.input.ordinal).stableKey)

  private def selectRows(values: DMat, positions: Vector[Int]): DMat =
    val out = Matrix.newBuilder(positions.length, values.cols)
    var row = 0
    while row < positions.length do
      var col = 0
      while col < values.cols do
        out(row, col) = values(positions(row), col)
        col += 1
      row += 1
    out.result()

  private def metrics(prediction: ClassificationPrediction, target: ClassMembership): (Double, Double) =
    var squared = 0.0
    var correct = 0
    var row = 0
    val expected = target.argmaxLabels
    val predicted = prediction.predicted
    while row < prediction.probabilities.rows do
      var col = 0
      while col < prediction.probabilities.cols do
        val difference = prediction.probabilities(row, col) - target.values(row, col)
        squared += difference * difference
        col += 1
      if predicted(row) == expected(row) then correct += 1
      row += 1
    (squared / (prediction.probabilities.rows * prediction.probabilities.cols), correct.toDouble / prediction.probabilities.rows)
