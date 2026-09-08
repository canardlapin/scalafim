package scalafim.fmri.mvpa.predictive

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.MutableDVec
import multivar.core.OperatorRepresentation
import multivar.core.ValueId
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.*

final class PredictiveFrameExecutionSuite extends munit.FunSuite:
  import PredictiveAnalysis.given

  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private enum TestRendition:
    case Region
    case Global

  private val cat = ClassId.unsafe("cat")
  private val dog = ClassId.unsafe("dog")
  private val samples =
    AxisRef
      .create(
        AxisId.unsafe("frame-samples"),
        AxisPurpose.Samples,
        Vector.tabulate(8)(index => SampleId.unsafe(s"sample-$index")),
        CoordinateBasis.unsafe("run-trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("predictive-frame-fixture", "v1")
      )
      .toOption
      .get
  private val features =
    AxisRef
      .create(
        AxisId.unsafe("frame-features"),
        AxisPurpose.NeuralFeatures,
        Vector(
          FeatureId.unsafe("voxel-x"),
          FeatureId.unsafe("voxel-y"),
          FeatureId.unsafe("voxel-z")
        ),
        CoordinateBasis.unsafe("voxel-table"),
        Some(AxisUnits.unsafe("percent-signal-change")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("predictive-frame-mask", "v1")
      )
      .toOption
      .get
  private val classes =
    ClassAxis
      .create(
        AxisId.unsafe("frame-classes"),
        Vector(cat, dog),
        CoordinateProvenance.unsafe("predictive-frame-fixture", "v1")
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
  private val patterns = GaleTestMatrix.fromRows(
    Vector(
      Vector(-4.0, -1.0, 0.1),
      Vector(4.0, 1.0, 1.1),
      Vector(-3.0, -0.5, 0.2),
      Vector(3.0, 0.5, 1.2),
      Vector(-2.0, -1.5, 0.3),
      Vector(2.0, 1.5, 1.3),
      Vector(-5.0, -0.25, 0.4),
      Vector(5.0, 0.25, 1.4)
    )
  )
  private val design =
    val runs =
      Labels.dense(indices(0, 0, 1, 1, 2, 2, 3, 3), samples.size).toOption.get
    val ordinal = LeaveOneGroupOut(runs)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 109L)
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
      GeneralizationAxis(
        ScientificAxisName.unsafe("samples"),
        samples.identity
      )
    ).toOption.get
  private val configuration =
    ClassificationConfiguration(
      StandardizedNearestCentroid(
        StandardizationSpecification.CenterScaleRejectConstant
      )
    ).toOption.get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  private def denseEvidence =
    EvidenceTable
      .dense(
        samples,
        features,
        patterns,
        ValueId.unsafe("frame-patterns")
      )
      .toOption
      .get

  private def operatorEvidence =
    EvidenceTable
      .operator(
        samples,
        features,
        MatrixFree(patterns),
        ValueId.unsafe("frame-patterns")
      )
      .toOption
      .get

  private def source(
      evidence: EvidenceTable[
        samples.Id,
        features.Id,
        SampleId,
        FeatureId
      ]
  ) =
    CategoricalObservationSource(evidence, target).toOption.get

  private def measurement(id: String, selected: Int*) =
    Measurement
      .hardSelection(
        features,
        MeasurementId.unsafe(id),
        Injection
          .from(indices(selected*), IndexSpace.of(features.size).toOption.get)
          .toOption
          .get
      )
      .toOption
      .get

  private def strategy(maxElements: Long) =
    ExecutionStrategy(
      BackendId.unsafe("alder-portable"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(maxElements)),
      FallbackPolicy.forbidden,
      ResultDelivery.Collected,
      Vector("learner" -> "nearest-centroid")
    ).toOption.get

  private def run[R](
      observations: CategoricalObservationSource[
        samples.Id,
        features.Id,
        FeatureId
      ],
      frame: MeasurementFrame[features.Id, FeatureId, R],
      maxElements: Long
  ) =
    Mvpa
      .run(observations)(
        design,
        frame,
        observations.classify(configuration),
        strategy(maxElements)
      )
      .toOption
      .get

  test("one predictive estimand runs over region and identity-global measurements"):
    val region = measurement("region-x", 0)
    val global = measurement("global-all", 0, 1, 2)
    val regionFrame = MeasurementFrame(features)(
      Vector(MeasurementEntry(region, TestRendition.Region))
    ).toOption.get
    val globalFrame = MeasurementFrame(features)(
      Vector(MeasurementEntry(global, TestRendition.Global))
    ).toOption.get
    val observations = source(denseEvidence)

    val regionResult = run(observations, regionFrame, 8L)
    val globalResult = run(observations, globalFrame, 24L)

    val regionPrediction = success(regionResult.values.head)
    val globalPrediction = success(globalResult.values.head)
    assertEquals(regionPrediction.predictions.predicted.toVector, labels)
    assertEquals(globalPrediction.predictions.predicted.toVector, labels)
    assertEquals(regionPrediction.accuracy.value, 1.0)
    assertEquals(globalPrediction.accuracy.value, 1.0)
    assertEquals(regionResult.values.head.rendition, TestRendition.Region)
    assertEquals(globalResult.values.head.rendition, TestRendition.Global)

  test("one failed measurement does not erase successful frame outcomes"):
    val good = measurement("a-region-good", 0)
    val tooWide = measurement("z-global-too-wide", 0, 1)
    val frame = MeasurementFrame(features)(
      Vector(
        MeasurementEntry(good, TestRendition.Region),
        MeasurementEntry(tooWide, TestRendition.Global)
      )
    ).toOption.get
    val result = run(source(denseEvidence), frame, 8L)

    assertEquals(result.counts.succeeded, 1)
    assertEquals(result.counts.failed, 1)
    assertEquals(success(result.values.head).accuracy.value, 1.0)
    result.values(1).outcome match
      case MeasurementOutcome.Failed(
            ClassificationCompileError.Data(
              AlderDataError.Evidence(
                EvidenceTableError.MaterializationBudgetExceeded(required, budget)
              )
            ),
            receipt
          ) =>
        assertEquals(required, 16L)
        assertEquals(budget.maxElements, 8L)
        assertEquals(receipt.materializedCells, 0L)
      case other => fail(s"expected local materialization failure, obtained $other")

  test("dense and operator sources agree while local densification stays receipted"):
    val global = measurement("global-two-feature", 0, 1)
    val frame = MeasurementFrame(features)(
      Vector(MeasurementEntry(global, NoRendition))
    ).toOption.get
    val dense = run(source(denseEvidence), frame, 16L)
    val operator = run(source(operatorEvidence), frame, 16L)
    val denseValue = success(dense.values.head)
    val operatorValue = success(operator.values.head)

    assertEquals(
      denseValue.predictions.predicted.toVector,
      operatorValue.predictions.predicted.toVector
    )
    assertEquals(denseValue.accuracy.value, operatorValue.accuracy.value)
    var row = 0
    while row < samples.size do
      var column = 0
      while column < classes.size do
        assertEqualsDouble(
          denseValue.predictions.scores.get.scoreAt(row, column).toOption.get.value,
          operatorValue.predictions.scores.get.scoreAt(row, column).toOption.get.value,
          1e-12
        )
        column += 1
      row += 1
    assertEquals(
      denseValue.preparation.measurement.sourceRepresentation,
      OperatorRepresentation.Dense
    )
    assertEquals(
      operatorValue.preparation.measurement.sourceRepresentation,
      OperatorRepresentation.MatrixFree
    )
    assertEquals(
      denseValue.preparation.measurement.measuredRepresentation,
      OperatorRepresentation.MatrixFree
    )
    assertEquals(
      operatorValue.preparation.measurement.measuredRepresentation,
      OperatorRepresentation.MatrixFree
    )
    assertEquals(dense.values.head.outcome.receipt.materializedCells, 16L)
    assertEquals(operator.values.head.outcome.receipt.materializedCells, 16L)
    assertEquals(dense.receipt.work.materializedCells, 16L)
    assertEquals(operator.receipt.work.materializedCells, 16L)

  private def success[R](
      value: MeasurementValue[
        OutOfFoldClassification[
          samples.Id,
          StandardizedNearestCentroid,
          StandardizedNearestCentroidFit,
          CategoricalClassifierError,
          AlderClassPrediction
        ],
        ClassificationBindRejection,
        ClassificationCompileError[CategoricalClassifierError],
        R
      ]
  ): OutOfFoldClassification[
    samples.Id,
    StandardizedNearestCentroid,
    StandardizedNearestCentroidFit,
    CategoricalClassifierError,
    AlderClassPrediction
  ] =
    value.outcome match
      case MeasurementOutcome.Success(result, _) => result
      case other                                 => fail(s"expected success, obtained $other")

  private final class MatrixFree private (matrix: DMat) extends DoubleLinearOperator:
    override def rows: Int = matrix.rows
    override def cols: Int = matrix.cols

    override def applyTo(input: DVec, output: MutableDVec): Unit =
      var row = 0
      while row < rows do
        var total = 0.0
        var column = 0
        while column < cols do
          total += matrix(row, column) * input(column)
          column += 1
        output(row) = total
        row += 1

    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      var column = 0
      while column < cols do
        var total = 0.0
        var row = 0
        while row < rows do
          total += matrix(row, column) * input(row)
          row += 1
        output(column) = total
        column += 1

  private object MatrixFree:
    def apply(matrix: DMat): MatrixFree =
      new MatrixFree(matrix)
