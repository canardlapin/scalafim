package scalafim.fmri.mvpa.predictive

import gale.linalg.DMat
import multivar.core.SemanticSpace
import resample4s.core.Coverage
import resample4s.core.UnitKey
import scalafim.fmri.mvpa.*

import scala.reflect.ClassTag

opaque type ClassId = String

object ClassId:
  def apply(value: String): Either[CategoricalError, ClassId] =
    AxisKey(value).left.map(CategoricalError.InvalidText.apply).map(_.value)

  private[mvpa] def unsafe(value: String): ClassId =
    value

  extension (id: ClassId) inline def value: String = id

  given AxisKeyCodec[ClassId] with
    override def encode(key: ClassId): AxisKey = AxisKey.unsafe(key.value)

  given ClassTag[ClassId] = ClassTag(classOf[String])

opaque type DecisionScore = Double

object DecisionScore:
  def apply(value: Double): Either[CategoricalError, DecisionScore] =
    if value.isFinite then Right(value)
    else Left(CategoricalError.NonFiniteScore(value))

  extension (score: DecisionScore) inline def value: Double = score

  given ClassTag[DecisionScore] = ClassTag.Double

enum PositiveClassScope:
  case Binary
  case OneVsRest

  def label: String =
    this match
      case Binary    => "binary"
      case OneVsRest => "one-vs-rest"

final class PositiveClassPolicy private (
    val classes: AxisIdentity,
    val positive: ClassId,
    val negative: Vector[ClassId],
    val scope: PositiveClassScope
)

object PositiveClassPolicy:
  def binary(
      classes: AxisRef[ClassId],
      positive: ClassId
  ): Either[CategoricalError, PositiveClassPolicy] =
    CategoricalTarget
      .validateClassAxis(classes)
      .flatMap: _ =>
        if classes.size != 2 then Left(CategoricalError.BinaryPolicyRequiresTwoClasses(classes.size))
        else build(classes, positive, PositiveClassScope.Binary)

  def oneVsRest(
      classes: AxisRef[ClassId],
      positive: ClassId
  ): Either[CategoricalError, PositiveClassPolicy] =
    CategoricalTarget
      .validateClassAxis(classes)
      .flatMap(_ => build(classes, positive, PositiveClassScope.OneVsRest))

  private def build(
      classes: AxisRef[ClassId],
      positive: ClassId,
      scope: PositiveClassScope
  ): Either[CategoricalError, PositiveClassPolicy] =
    if !classes.keys.contains(positive) then Left(CategoricalError.UnknownClass(positive))
    else
      Right(
        new PositiveClassPolicy(
          classes.identity,
          positive,
          classes.keys.filterNot(_ == positive),
          scope
        )
      )

enum CategoricalError:
  case InvalidText(error: AxisIdentityError)
  case Axis(error: AxisRefError)
  case ClassAxisPurpose(actual: AxisPurpose)
  case TooFewClasses(actual: Int)
  case TargetAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case TargetWitnessMismatch
  case UnknownClass(label: ClassId)
  case MissingObservedClass(label: ClassId)
  case Schedule(error: BoundScheduleError)
  case MissingTrainingClasses(unit: UnitKey, labels: Vector[ClassId])
  case ShapeMismatch(expectedRows: Int, expectedClasses: Int, actualRows: Int, actualClasses: Int)
  case NonFiniteScore(value: Double)
  case SampleAlignmentMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case ClassAlignmentMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case PredictionWitnessMismatch(axis: String)
  case MissingDecisionScores
  case BinaryPolicyRequiresTwoClasses(actual: Int)
  case PositivePolicyMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case MeasureConstruction(error: ScientificIdentityError)
  case CountOutOfBounds(truth: Int, prediction: Int, classes: Int)

  def message: String =
    this match
      case InvalidText(error)       => error.message
      case Axis(error)              => error.message
      case ClassAxisPurpose(actual) =>
        s"categorical classes require a '${AxisPurpose.Classes.value}' axis, obtained '${actual.value}'"
      case TooFewClasses(actual) =>
        s"categorical analysis requires at least two classes, obtained $actual"
      case TargetAxisMismatch(expected, actual) =>
        s"target belongs to sample axis ${actual.value}, expected ${expected.value}"
      case TargetWitnessMismatch =>
        "target and sample axis use different nominal witnesses"
      case UnknownClass(label) =>
        s"unknown class '${label.value}'"
      case MissingObservedClass(label) =>
        s"target contains no observation of class '${label.value}'"
      case Schedule(error)                      => error.message
      case MissingTrainingClasses(unit, labels) =>
        s"analysis unit $unit is missing training classes ${labels.map(_.value).mkString("[", ",", "]")}"
      case ShapeMismatch(expectedRows, expectedClasses, actualRows, actualClasses) =>
        s"class table expected ${expectedRows}x$expectedClasses values, obtained ${actualRows}x$actualClasses"
      case NonFiniteScore(value)                     => s"decision score must be finite, obtained $value"
      case SampleAlignmentMismatch(expected, actual) =>
        s"prediction sample axis is ${actual.value}, expected ${expected.value}"
      case ClassAlignmentMismatch(expected, actual) =>
        s"prediction class axis is ${actual.value}, expected ${expected.value}"
      case PredictionWitnessMismatch(axis) =>
        s"prediction and target use different nominal $axis witnesses"
      case MissingDecisionScores =>
        "the requested measure requires uncalibrated decision scores"
      case BinaryPolicyRequiresTwoClasses(actual) =>
        s"binary positive-class policy requires exactly two classes, obtained $actual"
      case PositivePolicyMismatch(expected, actual) =>
        s"positive-class policy belongs to ${actual.value}, expected ${expected.value}"
      case MeasureConstruction(error)                   => error.message
      case CountOutOfBounds(truth, prediction, classes) =>
        s"confusion-matrix index ($truth,$prediction) is outside $classes classes"

object ClassAxis:
  def create(
      id: AxisId,
      classes: Seq[ClassId],
      coordinateProvenance: CoordinateProvenance
  ): Either[CategoricalError, AxisRef[ClassId]] =
    AxisRef
      .create(
        id,
        AxisPurpose.Classes,
        classes,
        CoordinateBasis.unsafe("ordered-class-labels"),
        None,
        AxisScale.nominal,
        coordinateProvenance
      )
      .left
      .map(CategoricalError.Axis.apply)

/** Categorical truth values bound to exact sample and class axes. */
final class CategoricalTarget[S <: SemanticSpace] private (
    val samples: AxisRef.Aux[SampleId, S],
    val classes: AxisRef[ClassId],
    val labels: Column[S, ClassId]
)

object CategoricalTarget:
  def apply[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      classes: AxisRef[ClassId],
      labels: Column[S, ClassId]
  ): Either[CategoricalError, CategoricalTarget[S]] =
    for
      _ <- validateClassAxis(classes)
      _ <- validateLabelColumn(samples, classes, labels, requireCoverage = true)
    yield new CategoricalTarget(samples, classes, labels)

  /** Bind-time validation that every analysis partition can fit the declared categorical estimand. Assessment rows
    * never contribute to this proof.
    */
  def validateTrainingCoverage[
      S <: SemanticSpace,
      Cov <: Coverage
  ](
      target: CategoricalTarget[S],
      design: ValidationDesign[
        S,
        Cov,
        BoundSelectionSplit[SampleId, S]
      ]
  ): Either[CategoricalError, Unit] =
    val iterator = design.schedule.iterator
    while iterator.hasNext do
      val (unitKey, bound) = iterator.next()
      bound match
        case Left(error)  => return Left(CategoricalError.Schedule(error))
        case Right(split) =>
          val observed = scala.collection.mutable.HashSet.empty[ClassId]
          var position = 0
          while position < split.analysis.size do
            split.analysis.sourcePositionAt(position) match
              case Left(error) =>
                return Left(
                  CategoricalError.Schedule(
                    BoundScheduleError.InvalidUnit(
                      unitKey,
                      ScheduleUnitError.Axis(error)
                    )
                  )
                )
              case Right(sourcePosition) =>
                observed += target.labels.values(sourcePosition)
            position += 1
          val missing = target.classes.keys.filterNot(observed.contains)
          if missing.nonEmpty then return Left(CategoricalError.MissingTrainingClasses(unitKey, missing))
    Right(())

  private[predictive] def validateClassAxis(
      classes: AxisRef[ClassId]
  ): Either[CategoricalError, Unit] =
    if classes.identity.purpose != AxisPurpose.Classes then
      Left(CategoricalError.ClassAxisPurpose(classes.identity.purpose))
    else if classes.size < 2 then Left(CategoricalError.TooFewClasses(classes.size))
    else Right(())

  private[predictive] def validateLabelColumn[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      classes: AxisRef[ClassId],
      labels: Column[S, ClassId],
      requireCoverage: Boolean
  ): Either[CategoricalError, Unit] =
    if labels.rowIdentity != samples.identity then
      Left(
        CategoricalError.TargetAxisMismatch(
          samples.identity.fingerprint,
          labels.rowIdentity.fingerprint
        )
      )
    else if !(labels.rows eq samples.evidence) then Left(CategoricalError.TargetWitnessMismatch)
    else
      val observed = scala.collection.mutable.HashSet.empty[ClassId]
      var position = 0
      while position < labels.size do
        val label = labels.values(position)
        if !classes.keys.contains(label) then return Left(CategoricalError.UnknownClass(label))
        observed += label
        position += 1
      if requireCoverage then
        classes.keys.find(label => !observed.contains(label)) match
          case Some(label) => Left(CategoricalError.MissingObservedClass(label))
          case None        => Right(())
      else Right(())

final class ClassScoreTable[S <: SemanticSpace] private (
    val samples: AxisRef.Aux[SampleId, S],
    val classes: AxisRef[ClassId],
    private val values: IArray[DecisionScore]
):
  def scoreAt(sample: Int, classPosition: Int): Either[CategoricalError, DecisionScore] =
    if sample < 0 || sample >= samples.size || classPosition < 0 || classPosition >= classes.size then
      Left(CategoricalError.ShapeMismatch(samples.size, classes.size, sample, classPosition))
    else Right(values(sample * classes.size + classPosition))

  private[predictive] def unsafeScore(sample: Int, classPosition: Int): DecisionScore =
    values(sample * classes.size + classPosition)

object ClassScoreTable:
  def apply[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      classes: AxisRef[ClassId],
      values: DMat
  ): Either[CategoricalError, ClassScoreTable[S]] =
    validateShape(samples, classes, values) match
      case Left(error) => Left(error)
      case Right(_)    =>
        val scores = new Array[DecisionScore](values.rows * values.cols)
        var row = 0
        var failure: Option[CategoricalError] = None
        while row < values.rows && failure.isEmpty do
          var column = 0
          while column < values.cols && failure.isEmpty do
            DecisionScore(values(row, column)) match
              case Left(error)  => failure = Some(error)
              case Right(score) => scores(row * values.cols + column) = score
            column += 1
          row += 1
        failure match
          case Some(error) => Left(error)
          case None        =>
            Right(
              new ClassScoreTable(
                samples,
                classes,
                IArray.unsafeFromArray(scores)
              )
            )

  private[predictive] def validateShape[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      classes: AxisRef[ClassId],
      values: DMat
  ): Either[CategoricalError, Unit] =
    if values.rows != samples.size || values.cols != classes.size then
      Left(
        CategoricalError.ShapeMismatch(
          samples.size,
          classes.size,
          values.rows,
          values.cols
        )
      )
    else Right(())

final class CategoricalPredictions[S <: SemanticSpace] private (
    val samples: AxisRef.Aux[SampleId, S],
    val classes: AxisRef[ClassId],
    val predicted: Column[S, ClassId],
    val scores: Option[ClassScoreTable[S]]
)

object CategoricalPredictions:
  def apply[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      classes: AxisRef[ClassId],
      predicted: Column[S, ClassId],
      scores: Option[ClassScoreTable[S]] = None
  ): Either[CategoricalError, CategoricalPredictions[S]] =
    for
      _ <- CategoricalTarget.validateClassAxis(classes)
      _ <- CategoricalTarget.validateLabelColumn(
        samples,
        classes,
        predicted,
        requireCoverage = false
      )
      _ <- validateOptionalAxes(samples, classes, scores)
    yield new CategoricalPredictions(samples, classes, predicted, scores)

  private def validateOptionalAxes[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      classes: AxisRef[ClassId],
      scores: Option[ClassScoreTable[S]]
  ): Either[CategoricalError, Unit] =
    val tables = scores.toVector.map(table => table.samples -> table.classes)
    tables.find(pair => pair._1.identity != samples.identity) match
      case Some(pair) =>
        Left(
          CategoricalError.SampleAlignmentMismatch(
            samples.identity.fingerprint,
            pair._1.identity.fingerprint
          )
        )
      case None =>
        tables.find(pair => pair._2.identity != classes.identity) match
          case Some(pair) =>
            Left(
              CategoricalError.ClassAlignmentMismatch(
                classes.identity.fingerprint,
                pair._2.identity.fingerprint
              )
            )
          case None => Right(())

opaque type AccuracyEstimate = Double

object AccuracyEstimate:
  private[predictive] def apply(value: Double): AccuracyEstimate = value
  extension (estimate: AccuracyEstimate) inline def value: Double = estimate

final class BalancedAccuracyEstimate private[predictive] (
    val value: Double,
    val classes: AxisIdentity,
    val perClassRecall: Vector[(ClassId, Double)]
)

final class ConfusionMatrixEstimate private[predictive] (
    val classes: AxisRef[ClassId],
    private val counts: IArray[Long]
):
  def at(truth: Int, prediction: Int): Either[CategoricalError, Long] =
    if truth < 0 || truth >= classes.size || prediction < 0 || prediction >= classes.size then
      Left(CategoricalError.CountOutOfBounds(truth, prediction, classes.size))
    else Right(counts(truth * classes.size + prediction))

  def rowMajor: Vector[Long] = counts.toVector

final class BinaryRocAucEstimate private[predictive] (
    val value: Double,
    val positive: ClassId,
    val negative: ClassId
)

trait ClassificationMeasure:
  type Result
  def identity: EstimandIdentity
  def evaluate[S <: SemanticSpace](
      truth: CategoricalTarget[S],
      predictions: CategoricalPredictions[S]
  ): Either[CategoricalError, Result]

object Accuracy extends ClassificationMeasure:
  type Result = AccuracyEstimate
  val identity: EstimandIdentity =
    measureIdentity("classification-accuracy", "weighting" -> "per-sample")

  def evaluate[S <: SemanticSpace](
      truth: CategoricalTarget[S],
      predictions: CategoricalPredictions[S]
  ): Either[CategoricalError, AccuracyEstimate] =
    ClassificationMeasures
      .validate(truth, predictions)
      .map: _ =>
        var correct = 0
        var row = 0
        while row < truth.labels.size do
          if truth.labels.values(row) == predictions.predicted.values(row) then correct += 1
          row += 1
        AccuracyEstimate(correct.toDouble / truth.labels.size.toDouble)

object BalancedAccuracy extends ClassificationMeasure:
  type Result = BalancedAccuracyEstimate
  val identity: EstimandIdentity =
    measureIdentity(
      "classification-balanced-accuracy",
      "class-weighting" -> "uniform",
      "per-class-term" -> "recall"
    )

  def evaluate[S <: SemanticSpace](
      truth: CategoricalTarget[S],
      predictions: CategoricalPredictions[S]
  ): Either[CategoricalError, BalancedAccuracyEstimate] =
    ClassificationMeasures
      .validate(truth, predictions)
      .map: _ =>
        val totals = Array.fill(truth.classes.size)(0L)
        val correct = Array.fill(truth.classes.size)(0L)
        var row = 0
        while row < truth.labels.size do
          val truthPosition =
            ClassificationMeasures.classPositionUnsafe(
              truth.classes,
              truth.labels.values(row)
            )
          totals(truthPosition) += 1L
          if truth.labels.values(row) == predictions.predicted.values(row) then correct(truthPosition) += 1L
          row += 1
        val recalls = truth.classes.keys.indices.map: position =>
          truth.classes.keys(position) ->
            (correct(position).toDouble / totals(position).toDouble)
        new BalancedAccuracyEstimate(
          recalls.map(_._2).sum / recalls.length.toDouble,
          truth.classes.identity,
          recalls.toVector
        )

object ConfusionMatrix extends ClassificationMeasure:
  type Result = ConfusionMatrixEstimate
  val identity: EstimandIdentity =
    measureIdentity(
      "classification-confusion-matrix",
      "rows" -> "truth",
      "columns" -> "prediction",
      "weighting" -> "unit-count"
    )

  def evaluate[S <: SemanticSpace](
      truth: CategoricalTarget[S],
      predictions: CategoricalPredictions[S]
  ): Either[CategoricalError, ConfusionMatrixEstimate] =
    ClassificationMeasures
      .validate(truth, predictions)
      .map: _ =>
        val counts = Array.fill[Long](truth.classes.size * truth.classes.size)(0L)
        var row = 0
        while row < truth.labels.size do
          val truthPosition =
            ClassificationMeasures.classPositionUnsafe(
              truth.classes,
              truth.labels.values(row)
            )
          val predictionPosition =
            ClassificationMeasures.classPositionUnsafe(
              truth.classes,
              predictions.predicted.values(row)
            )
          val offset = truthPosition * truth.classes.size + predictionPosition
          counts(offset) += 1L
          row += 1
        new ConfusionMatrixEstimate(
          truth.classes,
          IArray.unsafeFromArray(counts)
        )

final class BinaryRocAuc private (
    val policy: PositiveClassPolicy,
    val identity: EstimandIdentity
) extends ClassificationMeasure:
  type Result = BinaryRocAucEstimate

  def evaluate[S <: SemanticSpace](
      truth: CategoricalTarget[S],
      predictions: CategoricalPredictions[S]
  ): Either[CategoricalError, BinaryRocAucEstimate] =
    for
      _ <- ClassificationMeasures.validate(truth, predictions)
      _ <-
        if policy.classes == truth.classes.identity then Right(())
        else
          Left(
            CategoricalError.PositivePolicyMismatch(
              truth.classes.identity.fingerprint,
              policy.classes.fingerprint
            )
          )
      scores <- predictions.scores
        .toRight(CategoricalError.MissingDecisionScores)
        .map(table => (row: Int, column: Int) => table.unsafeScore(row, column).value)
    yield
      val positivePosition =
        ClassificationMeasures.classPositionUnsafe(
          truth.classes,
          policy.positive
        )
      var favorable = 0.0
      var pairs = 0L
      var positiveRow = 0
      while positiveRow < truth.labels.size do
        if truth.labels.values(positiveRow) == policy.positive then
          var negativeRow = 0
          while negativeRow < truth.labels.size do
            if truth.labels.values(negativeRow) != policy.positive then
              val positiveScore = scores(positiveRow, positivePosition)
              val negativeScore = scores(negativeRow, positivePosition)
              if positiveScore > negativeScore then favorable += 1.0
              else if positiveScore == negativeScore then favorable += 0.5
              pairs += 1L
            negativeRow += 1
        positiveRow += 1
      new BinaryRocAucEstimate(
        favorable / pairs.toDouble,
        policy.positive,
        policy.negative(0)
      )

object BinaryRocAuc:
  def apply(
      policy: PositiveClassPolicy
  ): Either[CategoricalError, BinaryRocAuc] =
    if policy.scope != PositiveClassScope.Binary then
      Left(CategoricalError.BinaryPolicyRequiresTwoClasses(policy.negative.length + 1))
    else
      val identity = EstimandIdentity(
        EstimandKind.unsafe("classification-binary-roc-auc"),
        Vector(
          "aggregation" -> "binary",
          "comparison" -> "mann-whitney-pairwise",
          "direction" -> "larger-is-positive",
          "input" -> "uncalibrated-decision-score",
          "negative-class" -> policy.negative(0).value,
          "positive-class" -> policy.positive.value,
          "tie-policy" -> "half-credit",
          "weighting" -> "one-positive-one-negative"
        )
      ).left.map(CategoricalError.MeasureConstruction.apply)
      identity.map(new BinaryRocAuc(policy, _))

private object ClassificationMeasures:
  /** Constructor validation proves membership before numerical evaluation. */
  def classPositionUnsafe(
      classes: AxisRef[ClassId],
      label: ClassId
  ): Int =
    var position = 0
    while classes.keys(position) != label do position += 1
    position

  def validate[S <: SemanticSpace](
      truth: CategoricalTarget[S],
      predictions: CategoricalPredictions[S]
  ): Either[CategoricalError, Unit] =
    if predictions.samples.identity != truth.samples.identity then
      Left(
        CategoricalError.SampleAlignmentMismatch(
          truth.samples.identity.fingerprint,
          predictions.samples.identity.fingerprint
        )
      )
    else if predictions.classes.identity != truth.classes.identity then
      Left(
        CategoricalError.ClassAlignmentMismatch(
          truth.classes.identity.fingerprint,
          predictions.classes.identity.fingerprint
        )
      )
    else if !(predictions.samples.evidence eq truth.samples.evidence) then
      Left(CategoricalError.PredictionWitnessMismatch("sample"))
    else if !(predictions.classes.evidence eq truth.classes.evidence) then
      Left(CategoricalError.PredictionWitnessMismatch("class"))
    else Right(())

/** All callers pass compile-time constants owned by this file. */
private def measureIdentity(
    kind: String,
    fields: (String, String)*
): EstimandIdentity =
  EstimandIdentity.trusted(EstimandKind.unsafe(kind), fields)
