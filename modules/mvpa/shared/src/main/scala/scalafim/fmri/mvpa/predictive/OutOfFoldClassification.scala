package scalafim.fmri.mvpa.predictive

import alder.kernel.Audit
import alder.kernel.ProtocolFingerprint
import gale.linalg.DMat
import multivar.core.SemanticSpace
import resample4s.core.Coverage
import resample4s.core.Selection
import resample4s.core.Split
import resample4s.core.UnitKey
import scalafim.fmri.mvpa.*

/** The numerical meaning of the score row linked to an out-of-fold prediction. Calibration, when introduced, must use a
  * distinct case and receipt rather than relabeling these learner decision scores.
  */
enum ClassificationScoreKind:
  case UncalibratedDecision

final class ClassificationScoreReceipt private[predictive] (
    val classes: AxisIdentity,
    val kind: ClassificationScoreKind
)

/** One source-ordered output receipt. The row position links the typed prediction and score table to its exact
  * validation fold, Alder fit audit, and ScalaFIM preparation/materialization receipt.
  */
final class OutOfFoldPredictionReceipt private[predictive] (
    val sourcePosition: Int,
    val sample: SampleId,
    val unit: UnitKey,
    val assessmentPosition: Int,
    val score: ClassificationScoreReceipt,
    val fold: ClassificationFoldReceipt,
    val preparation: AlderInputReceipt
):
  def modelAudit: Audit = fold.fitAudit
  def modelIdentity: ProtocolFingerprint = fold.fitIdentity

/** Exact-once predictive output on the authoritative sample axis. Fold estimates remain audit evidence; scientific
  * summaries are derived from the typed source-ordered predictions below.
  */
final class OutOfFoldClassification[
    S <: SemanticSpace,
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction <: CategoricalLearnerPrediction
] private (
    val predictions: CategoricalPredictions[S],
    val receipts: Vector[OutOfFoldPredictionReceipt],
    val accuracy: AccuracyEstimate,
    val balancedAccuracy: BalancedAccuracyEstimate,
    val confusionMatrix: ConfusionMatrixEstimate,
    private[predictive] val foldRun: AlderClassificationRun[
      S,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ]
):
  def foldEstimates: Vector[(UnitKey, FoldClassificationEstimate[Prediction])] =
    foldRun.folds

  def foldReceipts: Vector[ClassificationFoldReceipt] =
    foldRun.receipts

  def configuration: ClassificationConfiguration[
    LearnerConfiguration,
    Fitted,
    LearnerError,
    Prediction
  ] =
    foldRun.configuration

  def preparation: AlderInputReceipt =
    foldRun.input

enum OutOfFoldClassificationError:
  case Design(error: PredictiveDesignError)
  case Categorical(error: CategoricalError)
  case Column(error: ColumnError)
  case SampleAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case SampleWitnessMismatch
  case ClassAxisMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case ClassWitnessMismatch
  case FoldCountMismatch(expected: Int, actual: Int)
  case ReceiptCountMismatch(expected: Int, actual: Int)
  case FoldOrderMismatch(position: Int, expected: UnitKey, actual: UnitKey)
  case ReceiptOrderMismatch(position: Int, expected: UnitKey, actual: UnitKey)
  case ReceiptAssessmentMismatch(
      unit: UnitKey,
      expected: Vector[SampleId],
      actual: Vector[SampleId]
  )
  case ForeignSample(unit: UnitKey, sample: SampleId)
  case DuplicateSample(sample: SampleId, first: UnitKey, second: UnitKey)
  case ReorderedOutput(
      unit: UnitKey,
      position: Int,
      expected: SampleId,
      actual: SampleId
  )
  case UnexpectedOutput(unit: UnitKey, position: Int, sample: SampleId)
  case LocationMismatch(
      sample: SampleId,
      expectedUnit: UnitKey,
      actualUnit: UnitKey,
      expectedPosition: Int,
      actualPosition: Int
  )
  case TruthMismatch(sample: SampleId, expected: ClassId, actual: ClassId)
  case ScoreClassOrderMismatch(
      sample: SampleId,
      expected: Vector[ClassId],
      actual: Vector[ClassId]
  )
  case MissingSample(sample: SampleId)

  def message: String =
    this match
      case Design(error)                        => error.message
      case Categorical(error)                   => error.message
      case Column(error)                        => error.message
      case SampleAxisMismatch(expected, actual) =>
        s"compiled predictions use sample axis ${actual.value}, expected ${expected.value}"
      case SampleWitnessMismatch =>
        "compiled predictions and validation design use different nominal sample witnesses"
      case ClassAxisMismatch(expected, actual) =>
        s"compiled predictions use class axis ${actual.value}, expected ${expected.value}"
      case ClassWitnessMismatch =>
        "compiled predictions and target use different nominal class witnesses"
      case FoldCountMismatch(expected, actual) =>
        s"compiled predictions contain $actual fold estimates, expected $expected"
      case ReceiptCountMismatch(expected, actual) =>
        s"compiled predictions contain $actual fold receipts, expected $expected"
      case FoldOrderMismatch(position, expected, actual) =>
        s"fold output at position $position is $actual, expected $expected"
      case ReceiptOrderMismatch(position, expected, actual) =>
        s"fold receipt at position $position is $actual, expected $expected"
      case ReceiptAssessmentMismatch(unit, expected, actual) =>
        s"fold $unit receipt assessment ${renderSamples(actual)} does not match design ${renderSamples(expected)}"
      case ForeignSample(unit, sample) =>
        s"fold $unit emitted foreign sample '${sample.value}'"
      case DuplicateSample(sample, first, second) =>
        s"sample '${sample.value}' was emitted by both $first and $second"
      case ReorderedOutput(unit, position, expected, actual) =>
        s"fold $unit output $position is '${actual.value}', expected '${expected.value}'"
      case UnexpectedOutput(unit, position, sample) =>
        s"fold $unit emitted unexpected output $position for '${sample.value}'"
      case LocationMismatch(
            sample,
            expectedUnit,
            actualUnit,
            expectedPosition,
            actualPosition
          ) =>
        s"sample '${sample.value}' belongs to $expectedUnit assessment position $expectedPosition, obtained $actualUnit position $actualPosition"
      case TruthMismatch(sample, expected, actual) =>
        s"sample '${sample.value}' truth is '${actual.value}', expected '${expected.value}'"
      case ScoreClassOrderMismatch(sample, expected, actual) =>
        s"sample '${sample.value}' scores classes ${renderClasses(actual)} rather than ${renderClasses(expected)}"
      case MissingSample(sample) =>
        s"compiled predictions contain no output for sample '${sample.value}'"

  private def renderSamples(values: Vector[SampleId]): String =
    values.map(_.value).mkString("[", ",", "]")

  private def renderClasses(values: Vector[ClassId]): String =
    values.map(_.value).mkString("[", ",", "]")

object OutOfFoldClassification:
  private[predictive] def reconstruct[
      S <: SemanticSpace,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      run: AlderClassificationRun[
        S,
        LearnerConfiguration,
        Fitted,
        LearnerError,
        Prediction
      ],
      target: CategoricalTarget[S],
      design: ValidationDesign[
        S,
        Coverage.ExactOnce,
        BoundSelectionSplit[SampleId, S]
      ] { type OrdinalUnit = Split[Selection] }
  ): Either[
    OutOfFoldClassificationError,
    OutOfFoldClassification[
      S,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ]
  ] =
    for
      _ <- validateAxes(run, target, design)
      reconstruction <- design.outOfFold.left.map(
        OutOfFoldClassificationError.Design.apply
      )
      assembled <- assemble(run, target, design, reconstruction)
    yield assembled

  private def validateAxes[
      S <: SemanticSpace,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      run: AlderClassificationRun[
        S,
        LearnerConfiguration,
        Fitted,
        LearnerError,
        Prediction
      ],
      target: CategoricalTarget[S],
      design: ValidationDesign[
        S,
        Coverage.ExactOnce,
        BoundSelectionSplit[SampleId, S]
      ]
  ): Either[OutOfFoldClassificationError, Unit] =
    if run.samples.identity != design.schedule.axis.identity then
      Left(
        OutOfFoldClassificationError.SampleAxisMismatch(
          design.schedule.axis.identity.fingerprint,
          run.samples.identity.fingerprint
        )
      )
    else if !(run.samples.evidence eq design.schedule.axis.evidence) then
      Left(OutOfFoldClassificationError.SampleWitnessMismatch)
    else if run.classes.identity != target.classes.identity then
      Left(
        OutOfFoldClassificationError.ClassAxisMismatch(
          target.classes.identity.fingerprint,
          run.classes.identity.fingerprint
        )
      )
    else if !(run.classes.evidence eq target.classes.evidence) then
      Left(OutOfFoldClassificationError.ClassWitnessMismatch)
    else Right(())

  private def assemble[
      S <: SemanticSpace,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction <: CategoricalLearnerPrediction
  ](
      run: AlderClassificationRun[
        S,
        LearnerConfiguration,
        Fitted,
        LearnerError,
        Prediction
      ],
      target: CategoricalTarget[S],
      design: ValidationDesign[
        S,
        Coverage.ExactOnce,
        BoundSelectionSplit[SampleId, S]
      ] { type OrdinalUnit = Split[Selection] },
      reconstruction: OutOfFoldReconstruction[S]
  ): Either[
    OutOfFoldClassificationError,
    OutOfFoldClassification[
      S,
      LearnerConfiguration,
      Fitted,
      LearnerError,
      Prediction
    ]
  ] =
    val keys = design.schedule.keys.toVector
    if run.folds.length != keys.length then
      Left(
        OutOfFoldClassificationError.FoldCountMismatch(
          keys.length,
          run.folds.length
        )
      )
    else if run.receipts.length != keys.length then
      Left(
        OutOfFoldClassificationError.ReceiptCountMismatch(
          keys.length,
          run.receipts.length
        )
      )
    else
      val predicted = new Array[ClassId](run.samples.size)
      val scoreBuilder = DMat.newBuilder(run.samples.size, run.classes.size)
      val rowReceipts = Array.fill[Option[OutOfFoldPredictionReceipt]](
        run.samples.size
      )(None)
      val seen = scala.collection.mutable.HashMap.empty[SampleId, UnitKey]
      val scoreReceipt = new ClassificationScoreReceipt(
        run.classes.identity,
        ClassificationScoreKind.UncalibratedDecision
      )

      var foldPosition = 0
      while foldPosition < keys.length do
        val expectedUnit = keys(foldPosition)
        val (actualUnit, estimate) = run.folds(foldPosition)
        val receipt = run.receipts(foldPosition)
        if actualUnit != expectedUnit then
          return Left(
            OutOfFoldClassificationError.FoldOrderMismatch(
              foldPosition,
              expectedUnit,
              actualUnit
            )
          )
        if receipt.unit != expectedUnit then
          return Left(
            OutOfFoldClassificationError.ReceiptOrderMismatch(
              foldPosition,
              expectedUnit,
              receipt.unit
            )
          )
        val split = design.at(expectedUnit) match
          case Left(error) =>
            return Left(
              OutOfFoldClassificationError.Design(
                PredictiveDesignError.Schedule(error)
              )
            )
          case Right(value) => value
        val expectedSamples = split.assessment.child.keys
        if receipt.assessmentSamples != expectedSamples then
          return Left(
            OutOfFoldClassificationError.ReceiptAssessmentMismatch(
              expectedUnit,
              expectedSamples,
              receipt.assessmentSamples
            )
          )

        var assessmentPosition = 0
        while assessmentPosition < estimate.observations.length do
          val observation = estimate.observations(assessmentPosition)
          val sourcePosition = run.samples.positionOf(observation.sample) match
            case None =>
              return Left(
                OutOfFoldClassificationError.ForeignSample(
                  expectedUnit,
                  observation.sample
                )
              )
            case Some(value) => value
          seen.get(observation.sample) match
            case Some(firstUnit) =>
              return Left(
                OutOfFoldClassificationError.DuplicateSample(
                  observation.sample,
                  firstUnit,
                  expectedUnit
                )
              )
            case None => seen.update(observation.sample, expectedUnit)
          if assessmentPosition >= expectedSamples.length then
            return Left(
              OutOfFoldClassificationError.UnexpectedOutput(
                expectedUnit,
                assessmentPosition,
                observation.sample
              )
            )
          val expectedSample = expectedSamples(assessmentPosition)
          if observation.sample != expectedSample then
            return Left(
              OutOfFoldClassificationError.ReorderedOutput(
                expectedUnit,
                assessmentPosition,
                expectedSample,
                observation.sample
              )
            )
          val location = reconstruction.locations(sourcePosition)
          if location.unit != expectedUnit ||
            location.assessmentPosition != assessmentPosition
          then
            return Left(
              OutOfFoldClassificationError.LocationMismatch(
                observation.sample,
                location.unit,
                expectedUnit,
                location.assessmentPosition,
                assessmentPosition
              )
            )
          val expectedTruth = target.labels.values(sourcePosition)
          if observation.truth != expectedTruth then
            return Left(
              OutOfFoldClassificationError.TruthMismatch(
                observation.sample,
                expectedTruth,
                observation.truth
              )
            )
          val scoreClasses = observation.prediction.scores.map(_._1)
          if scoreClasses != run.classes.keys then
            return Left(
              OutOfFoldClassificationError.ScoreClassOrderMismatch(
                observation.sample,
                run.classes.keys,
                scoreClasses
              )
            )

          predicted(sourcePosition) = observation.prediction.predicted
          var classPosition = 0
          while classPosition < run.classes.size do
            scoreBuilder.update(
              sourcePosition,
              classPosition,
              observation.prediction.scores(classPosition)._2.value
            )
            classPosition += 1
          rowReceipts(sourcePosition) = Some(
            new OutOfFoldPredictionReceipt(
              sourcePosition,
              observation.sample,
              expectedUnit,
              assessmentPosition,
              scoreReceipt,
              receipt,
              run.input
            )
          )
          assessmentPosition += 1
        foldPosition += 1

      val completeReceipts = Vector.newBuilder[OutOfFoldPredictionReceipt]
      var sourcePosition = 0
      while sourcePosition < run.samples.size do
        rowReceipts(sourcePosition) match
          case None =>
            return Left(
              OutOfFoldClassificationError.MissingSample(
                run.samples.keys(sourcePosition)
              )
            )
          case Some(receipt) => completeReceipts += receipt
        sourcePosition += 1

      for
        predictedColumn <- Column
          .fromIArray(run.samples, IArray.unsafeFromArray(predicted))
          .left
          .map(OutOfFoldClassificationError.Column.apply)
        scoreTable <- ClassScoreTable(
          run.samples,
          run.classes,
          scoreBuilder.result()
        ).left.map(OutOfFoldClassificationError.Categorical.apply)
        predictions <- CategoricalPredictions(
          run.samples,
          run.classes,
          predictedColumn,
          scores = Some(scoreTable)
        ).left.map(OutOfFoldClassificationError.Categorical.apply)
        accuracy <- Accuracy
          .evaluate(target, predictions)
          .left
          .map(OutOfFoldClassificationError.Categorical.apply)
        balancedAccuracy <- BalancedAccuracy
          .evaluate(target, predictions)
          .left
          .map(OutOfFoldClassificationError.Categorical.apply)
        confusionMatrix <- ConfusionMatrix
          .evaluate(target, predictions)
          .left
          .map(OutOfFoldClassificationError.Categorical.apply)
      yield new OutOfFoldClassification(
        predictions,
        completeReceipts.result(),
        accuracy,
        balancedAccuracy,
        confusionMatrix,
        run
      )
