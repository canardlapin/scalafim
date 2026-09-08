package scalafim.fmri.mvpa.predictive

import multivar.core.SemanticSpace
import multivar.core.ValueId
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.*

final class OutOfFoldClassificationSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private val cat = ClassId.unsafe("cat")
  private val dog = ClassId.unsafe("dog")
  private val samples =
    AxisRef
      .create(
        AxisId.unsafe("oof-samples"),
        AxisPurpose.Samples,
        Vector(
          SampleId.unsafe("z-run-0-cat"),
          SampleId.unsafe("a-run-0-dog"),
          SampleId.unsafe("y-run-1-cat"),
          SampleId.unsafe("b-run-1-dog"),
          SampleId.unsafe("x-run-2-cat"),
          SampleId.unsafe("c-run-2-dog"),
          SampleId.unsafe("w-run-3-cat"),
          SampleId.unsafe("d-run-3-dog")
        ),
        CoordinateBasis.unsafe("deliberately-nonlexical-run-trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("oof-fixture", "v1")
      )
      .toOption
      .get
  private val features =
    AxisRef
      .create(
        AxisId.unsafe("oof-features"),
        AxisPurpose.NeuralFeatures,
        Vector(FeatureId.unsafe("voxel-x"), FeatureId.unsafe("voxel-y")),
        CoordinateBasis.unsafe("voxel-table"),
        Some(AxisUnits.unsafe("percent-signal-change")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("oof-mask", "v1")
      )
      .toOption
      .get
  private val classes =
    ClassAxis
      .create(
        AxisId.unsafe("oof-classes"),
        Vector(cat, dog),
        CoordinateProvenance.unsafe("oof-fixture", "v1")
      )
      .toOption
      .get
  private val labels = Vector(cat, dog, cat, dog, cat, dog, cat, dog)
  private val target =
    CategoricalTarget(
      samples,
      classes,
      Column(samples, labels).toOption.get
    ).toOption.get
  private val evidence =
    EvidenceTable
      .dense(
        samples,
        features,
        GaleTestMatrix.fromRows(
          Vector(
            Vector(-4.0, -1.0),
            Vector(4.0, 1.0),
            Vector(-3.0, -0.5),
            Vector(3.0, 0.5),
            Vector(-2.0, -1.5),
            Vector(2.0, 1.5),
            Vector(-5.0, -0.25),
            Vector(5.0, 0.25)
          )
        ),
        ValueId.unsafe("oof-patterns")
      )
      .toOption
      .get
  private val measurement =
    val space = IndexSpace.of(features.size).toOption.get
    Measurement
      .hardSelection(
        features,
        MeasurementId.unsafe("oof-global"),
        Injection.from(indices(0, 1), space).toOption.get
      )
      .toOption
      .get
  private val design =
    val runs =
      Labels.dense(indices(0, 0, 1, 1, 2, 2, 3, 3), samples.size).toOption.get
    val ordinal = LeaveOneGroupOut(runs)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 91L)
    val compiled = ordinal
      .compile(IndexSpace.of(samples.size).toOption.get, authority.seed)
      .toOption
      .get
    val schedule = BoundSchedule(
      compiled,
      samples,
      AxisPopulationFingerprint.fromAxis(samples).toOption.get,
      ScheduleLabels.fromDesign(samples, ordinal).toOption.get,
      authority
    ).toOption.get
    ValidationDesign(
      schedule,
      ScientificAxisName.unsafe("samples"),
      GeneralizationAxis(ScientificAxisName.unsafe("run"), samples.identity)
    ).toOption.get
  private val configuration =
    ClassificationConfiguration(
      StandardizedNearestCentroid(
        StandardizationSpecification.CenterScaleRejectConstant
      )
    ).toOption.get
  private val plan =
    ScientificPlanFingerprint(
      "scalafim-mvpa-plan-v1-" + Vector.fill(64)("f").mkString
    ).toOption.get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  private def compiled =
    ClassificationCompiler
      .run(
        plan,
        evidence,
        target,
        design,
        measurement,
        MaterializationPolicy.Allow(MaterializationBudget.unsafe(16L)),
        configuration
      )
      .toOption
      .get

  private def replaceFirstFold[S <: SemanticSpace](
      run: AlderClassificationRun[
        S,
        StandardizedNearestCentroid,
        StandardizedNearestCentroidFit,
        CategoricalClassifierError,
        AlderClassPrediction
      ],
      observations: Vector[FoldClassObservation[AlderClassPrediction]]
  ): AlderClassificationRun[
    S,
    StandardizedNearestCentroid,
    StandardizedNearestCentroidFit,
    CategoricalClassifierError,
    AlderClassPrediction
  ] =
    val (unit, estimate) = run.folds.head
    val replacement =
      new FoldClassificationEstimate(estimate.accuracy, observations)
    rebuild(run, run.folds.updated(0, unit -> replacement), run.receipts)

  private def rebuild[S <: SemanticSpace](
      run: AlderClassificationRun[
        S,
        StandardizedNearestCentroid,
        StandardizedNearestCentroidFit,
        CategoricalClassifierError,
        AlderClassPrediction
      ],
      folds: Vector[(UnitKey, FoldClassificationEstimate[AlderClassPrediction])],
      receipts: Vector[ClassificationFoldReceipt]
  ): AlderClassificationRun[
    S,
    StandardizedNearestCentroid,
    StandardizedNearestCentroidFit,
    CategoricalClassifierError,
    AlderClassPrediction
  ] =
    new AlderClassificationRun(
      run.samples,
      run.classes,
      run.configuration,
      folds,
      receipts,
      run.assignment,
      run.resampler,
      run.input
    )

  private def reconstruct(
      run: AlderClassificationRun[
        samples.Id,
        StandardizedNearestCentroid,
        StandardizedNearestCentroidFit,
        CategoricalClassifierError,
        AlderClassPrediction
      ]
  ) =
    OutOfFoldClassification.reconstruct(run, target, design)

  test("exact-once output is authoritative, source ordered, and fully linked"):
    val result = compiled

    assertEquals(result.predictions.samples.keys, samples.keys)
    assertEquals(result.predictions.predicted.toVector, labels)
    assertEquals(result.receipts.map(_.sample), samples.keys)
    assertEquals(result.receipts.map(_.sourcePosition), samples.keys.indices.toVector)
    assertEquals(result.accuracy.value, 1.0)
    assertEquals(result.balancedAccuracy.value, 1.0)
    assertEquals(result.confusionMatrix.rowMajor, Vector(4L, 0L, 0L, 4L))
    assertEquals(
      Accuracy.evaluate(target, result.predictions).toOption.get.value,
      result.accuracy.value
    )
    assertEquals(
      BalancedAccuracy.evaluate(target, result.predictions).toOption.get.value,
      result.balancedAccuracy.value
    )
    assertEquals(
      ConfusionMatrix.evaluate(target, result.predictions).toOption.get.rowMajor,
      result.confusionMatrix.rowMajor
    )

    result.receipts.foreach: receipt =>
      assert(receipt.preparation eq result.preparation)
      assert(receipt.fold eq result.foldReceipts.find(_.unit == receipt.unit).get)
      assertEquals(receipt.score.classes, classes.identity)
      assertEquals(
        receipt.score.kind,
        ClassificationScoreKind.UncalibratedDecision
      )
      assertEquals(
        receipt.modelAudit.component.id.render,
        "scalafim.mvpa.standardized-nearest-centroid"
      )
      assertEquals(receipt.modelIdentity, receipt.fold.fitIdentity)
      assertEquals(
        receipt.fold.assessmentSamples(receipt.assessmentPosition),
        receipt.sample
      )

  test("duplicate, missing, foreign, and reordered outputs fail closed"):
    val run = compiled.foldRun
    val observations = run.folds.head._2.observations

    val duplicate = replaceFirstFold(
      run,
      observations.updated(
        1,
        observations(1).copy(sample = observations.head.sample)
      )
    )
    val duplicateRejected = reconstruct(duplicate).left.exists:
      case OutOfFoldClassificationError.DuplicateSample(_, _, _) => true
      case _                                                     => false
    assert(duplicateRejected)

    val missing = replaceFirstFold(run, observations.dropRight(1))
    val missingRejected = reconstruct(missing).left.exists:
      case OutOfFoldClassificationError.MissingSample(sample) =>
        sample == observations.last.sample
      case _ => false
    assert(missingRejected)

    val foreign = replaceFirstFold(
      run,
      observations.updated(
        0,
        observations.head.copy(sample = SampleId.unsafe("foreign-oof-sample"))
      )
    )
    val foreignRejected = reconstruct(foreign).left.exists:
      case OutOfFoldClassificationError.ForeignSample(_, sample) =>
        sample == SampleId.unsafe("foreign-oof-sample")
      case _ => false
    assert(foreignRejected)

    val reordered = replaceFirstFold(run, observations.reverse)
    val reorderedRejected = reconstruct(reordered).left.exists:
      case OutOfFoldClassificationError.ReorderedOutput(_, 0, expected, actual) =>
        expected == observations.head.sample && actual == observations.last.sample
      case _ => false
    assert(reorderedRejected)

  test("truth and class-score order cannot detach from typed predictions"):
    val run = compiled.foldRun
    val observations = run.folds.head._2.observations
    val wrongTruth = replaceFirstFold(
      run,
      observations.updated(0, observations.head.copy(truth = dog))
    )
    val truthRejected = reconstruct(wrongTruth).left.exists:
      case OutOfFoldClassificationError.TruthMismatch(sample, _, _) =>
        sample == observations.head.sample
      case _ => false
    assert(truthRejected)

    val first = observations.head
    val wrongScores = replaceFirstFold(
      run,
      observations.updated(
        0,
        first.copy(
          prediction = first.prediction.copy(
            scores = first.prediction.scores.reverse
          )
        )
      )
    )
    val scoresRejected = reconstruct(wrongScores).left.exists:
      case OutOfFoldClassificationError.ScoreClassOrderMismatch(sample, _, _) =>
        sample == first.sample
      case _ => false
    assert(scoresRejected)

  test("fold estimate and receipt order are part of reconstruction evidence"):
    val run = compiled.foldRun
    val reversedFolds = rebuild(run, run.folds.reverse, run.receipts)
    val foldOrderRejected = reconstruct(reversedFolds).left.exists:
      case OutOfFoldClassificationError.FoldOrderMismatch(0, _, _) => true
      case _                                                       => false
    assert(foldOrderRejected)

    val reversedReceipts = rebuild(run, run.folds, run.receipts.reverse)
    val receiptOrderRejected = reconstruct(reversedReceipts).left.exists:
      case OutOfFoldClassificationError.ReceiptOrderMismatch(0, _, _) => true
      case _                                                          => false
    assert(receiptOrderRejected)
