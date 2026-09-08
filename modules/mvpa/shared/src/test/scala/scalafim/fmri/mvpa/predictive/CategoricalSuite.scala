package scalafim.fmri.mvpa.predictive

import multivar.core.SemanticSpace
import resample4s.core.*
import resample4s.designs.KFold
import scalafim.fmri.mvpa.*

final class CategoricalSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private val cat = ClassId.unsafe("cat")
  private val dog = ClassId.unsafe("dog")
  private val samples = sampleAxis(4)
  private val classes =
    ClassAxis
      .create(
        AxisId.unsafe("animal-classes"),
        Vector(cat, dog),
        CoordinateProvenance.unsafe("fixture-labels", "v1")
      )
      .toOption
      .get
  private val truth =
    CategoricalTarget(
      samples,
      classes,
      Column(samples, Vector(cat, cat, dog, dog)).toOption.get
    ).toOption.get
  private val predictedLabels =
    Column(samples, Vector(cat, dog, dog, dog)).toOption.get
  private val scores =
    ClassScoreTable(
      samples,
      classes,
      GaleTestMatrix.fromRows(
        Vector(
          Vector(0.9, 0.1),
          Vector(0.8, 0.2),
          Vector(0.2, 0.8),
          Vector(0.1, 0.9)
        )
      )
    ).toOption.get
  private val predictions =
    CategoricalPredictions(
      samples,
      classes,
      predictedLabels,
      scores = Some(scores)
    ).toOption.get

  private def sampleAxis(size: Int): AxisRef[SampleId] =
    AxisRef
      .create(
        AxisId.unsafe(s"categorical-samples-$size"),
        AxisPurpose.Samples,
        Vector.tabulate(size)(index => SampleId.unsafe(s"trial-${100 + index * 7}")),
        CoordinateBasis.unsafe("trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("fixture", "v1")
      )
      .toOption
      .get

  private def bound[
      A,
      Cov <: Coverage,
      S <: SemanticSpace,
      Unit
  ](
      design: Design[A, Cov],
      axis: AxisRef.Aux[SampleId, S]
  )(using
      binder: ScheduleUnitBinder.Aux[A, SampleId, S, Unit]
  ): BoundSchedule[
    A,
    Cov,
    SampleId,
    S,
    Unit
  ] =
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 71L)
    val compiled = design
      .compile(IndexSpace.of(axis.size).toOption.get, authority.seed)
      .toOption
      .get
    BoundSchedule(
      compiled,
      axis,
      AxisPopulationFingerprint.fromAxis(axis).toOption.get,
      ScheduleLabels.fromDesign(axis, design).toOption.get,
      authority
    ).toOption.get

  private def validation(
      axis: AxisRef[SampleId]
  ): ValidationDesign[
    axis.Id,
    Coverage.ExactOnce,
    BoundSelectionSplit[SampleId, axis.Id]
  ] =
    val schedule = bound(KFold.ordered(2), axis)
    ValidationDesign(
      schedule,
      ScientificAxisName.unsafe("samples"),
      GeneralizationAxis(
        ScientificAxisName.unsafe("samples"),
        axis.identity
      )
    ).toOption.get

  test("categorical target binds exact sample and ordered class axes"):
    assertEquals(truth.samples.keys.map(_.value), samples.keys.map(_.value))
    assertEquals(truth.classes.keys.map(_.value), Vector("cat", "dog"))
    assertEquals(truth.labels.toVector.map(_.value), Vector("cat", "cat", "dog", "dog"))
    assertEquals(truth.classes.identity.purpose, AxisPurpose.Classes)

    val unknown = ClassId.unsafe("fox")
    assert(
      CategoricalTarget(
        samples,
        classes,
        Column(samples, Vector(cat, dog, cat, unknown)).toOption.get
      ).left.exists:
        case CategoricalError.UnknownClass(label) => label == unknown
        case _                                    => false
    )
    assert(
      CategoricalTarget(
        samples,
        classes,
        Column(samples, Vector.fill(4)(cat)).toOption.get
      ).left.exists:
        case CategoricalError.MissingObservedClass(label) => label == dog
        case _                                            => false
    )

    val otherSamples = sampleAxis(4)
    val staticMismatch = compileErrors("""
      import scalafim.fmri.mvpa.*
      import scalafim.fmri.mvpa.predictive.*
      import multivar.core.SemanticSpace
      def cannotCross[A <: SemanticSpace, B <: SemanticSpace](
          left: AxisRef.Aux[SampleId, A],
          right: AxisRef.Aux[SampleId, B],
          classes: AxisRef[ClassId],
          labels: Column[B, ClassId]
      ): Either[CategoricalError, CategoricalTarget[A]] =
        CategoricalTarget(left, classes, labels)
    """)
    assert(staticMismatch.nonEmpty)
    assertEquals(otherSamples.identity, samples.identity)

  test("every training partition must retain every declared class"):
    assertEquals(
      CategoricalTarget.validateTrainingCoverage(truth, validation(samples)),
      Right(())
    )

    val blockedTruth =
      CategoricalTarget(
        samples,
        classes,
        Column(samples, Vector(cat, dog, cat, dog)).toOption.get
      ).toOption.get
    assert(
      CategoricalTarget
        .validateTrainingCoverage(blockedTruth, validation(samples))
        .left
        .exists:
          case CategoricalError.MissingTrainingClasses(_, labels) =>
            labels.nonEmpty && labels.forall(label => label == cat || label == dog)
          case _ => false
    )

  test("scores and predictions are distinct ordered values"):
    assertEquals(scores.samples.identity, samples.identity)
    assertEquals(scores.classes.keys.map(_.value), Vector("cat", "dog"))
    assertEqualsDouble(scores.scoreAt(1, 1).toOption.get.value, 0.2, 0.0)
    assertEquals(predictions.predicted.toVector.map(_.value), Vector("cat", "dog", "dog", "dog"))

  test("accuracy, balanced accuracy, and confusion matrix retain exact estimands"):
    val accuracy = Accuracy.evaluate(truth, predictions).toOption.get
    val balanced = BalancedAccuracy.evaluate(truth, predictions).toOption.get
    val confusion = ConfusionMatrix.evaluate(truth, predictions).toOption.get

    assertEqualsDouble(accuracy.value, 0.75, 0.0)
    assertEqualsDouble(balanced.value, 0.75, 0.0)
    assertEquals(
      balanced.perClassRecall.map(pair => pair._1.value -> pair._2),
      Vector("cat" -> 0.5, "dog" -> 1.0)
    )
    assertEquals(confusion.classes.keys.map(_.value), Vector("cat", "dog"))
    assertEquals(confusion.rowMajor, Vector(1L, 1L, 0L, 2L))
    assertEquals(Accuracy.identity.fields.map(field => field.name -> field.value), Vector("weighting" -> "per-sample"))
    assertEquals(
      ConfusionMatrix.identity.fields.map(field => field.name -> field.value),
      Vector(
        "columns" -> "prediction",
        "rows" -> "truth",
        "weighting" -> "unit-count"
      )
    )

  test("binary ROC AUC states positive class, score source, tie rule, and weighting"):
    val policy = PositiveClassPolicy.binary(classes, dog).toOption.get
    val decision = BinaryRocAuc(policy).toOption.get
    val decisionEstimate = decision.evaluate(truth, predictions).toOption.get

    assertEqualsDouble(decisionEstimate.value, 1.0, 0.0)
    assertEquals(decisionEstimate.positive, dog)
    assertEquals(decisionEstimate.negative, cat)
    assertEquals(
      decision.identity.fields.map(field => field.name -> field.value),
      Vector(
        "aggregation" -> "binary",
        "comparison" -> "mann-whitney-pairwise",
        "direction" -> "larger-is-positive",
        "input" -> "uncalibrated-decision-score",
        "negative-class" -> "cat",
        "positive-class" -> "dog",
        "tie-policy" -> "half-credit",
        "weighting" -> "one-positive-one-negative"
      )
    )

    val labelsOnly =
      CategoricalPredictions(samples, classes, predictedLabels).toOption.get
    assertEquals(
      decision.evaluate(truth, labelsOnly),
      Left(CategoricalError.MissingDecisionScores)
    )
