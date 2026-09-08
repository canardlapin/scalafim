package scalafim.fmri.mvpa.predictive

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.MutableDVec
import multivar.core.ValueId
import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.*

final class CrossDomainClassificationSuite extends munit.FunSuite:
  import CrossDomainPredictiveAnalysis.given

  private val classA = ClassId.unsafe("a")
  private val classB = ClassId.unsafe("b")
  private val features = featureAxis("shared-features")
  private val classes =
    ClassAxis
      .create(
        AxisId.unsafe("cross-domain-classes"),
        Vector(classA, classB),
        CoordinateProvenance.unsafe("cross-domain-fixture", "v1")
      )
      .toOption
      .get
  private val sourceSamples = sampleAxis("source-samples", "source")
  private val targetSamples = sampleAxis("target-samples", "target")
  private val sourceLabels = Vector(classA, classA, classB, classB)
  private val targetLabels = Vector(classA, classB, classA, classB)
  private val sourceValues = GaleTestMatrix.fromRows(
    Vector(
      Vector(1.0, -1.0, 0.0),
      Vector(2.0, -2.0, 1.0),
      Vector(-1.0, 1.0, 0.0),
      Vector(-2.0, 2.0, -1.0)
    )
  )
  private val targetValues = GaleTestMatrix.fromRows(
    Vector(
      Vector(3.0, -3.0, 1.0),
      Vector(-3.0, 3.0, -1.0),
      Vector(2.0, -2.0, 0.5),
      Vector(-2.0, 2.0, -0.5)
    )
  )
  private val configuration =
    ClassificationConfiguration(
      CorrelationCentroid(),
      adaptation = ClassifierAdaptation.InductiveFrozenSourceDomain
    ).toOption.get

  private def sampleAxis(
      id: String,
      prefix: String,
      size: Int = 4
  ): AxisRef[SampleId] =
    AxisRef
      .create(
        AxisId.unsafe(id),
        AxisPurpose.Samples,
        Vector.tabulate(size)(index => SampleId.unsafe(s"$prefix-$index")),
        CoordinateBasis.unsafe("domain-observations"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("cross-domain-fixture", "v1")
      )
      .toOption
      .get

  private def featureAxis(id: String): AxisRef[FeatureId] =
    AxisRef
      .create(
        AxisId.unsafe(id),
        AxisPurpose.NeuralFeatures,
        Vector(
          FeatureId.unsafe("voxel-x"),
          FeatureId.unsafe("voxel-y"),
          FeatureId.unsafe("voxel-z")
        ),
        CoordinateBasis.unsafe("ordered-correspondence"),
        Some(AxisUnits.unsafe("percent-signal-change")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("cross-domain-mask", "v1")
      )
      .toOption
      .get

  private def categoricalSource[
      S <: multivar.core.SemanticSpace,
      N <: multivar.core.SemanticSpace
  ](
      samples: AxisRef.Aux[SampleId, S],
      neural: AxisRef.Aux[FeatureId, N],
      values: DMat,
      labels: Vector[ClassId],
      operator: Boolean
  ) =
    val evidence =
      if operator then
        EvidenceTable
          .operator(
            samples,
            neural,
            MatrixFree(values),
            ValueId.unsafe(s"${samples.identity.id.value}-patterns")
          )
          .toOption
          .get
      else
        EvidenceTable
          .dense(
            samples,
            neural,
            values,
            ValueId.unsafe(s"${samples.identity.id.value}-patterns")
          )
          .toOption
          .get
    val target =
      CategoricalTarget(
        samples,
        classes,
        Column(samples, labels).toOption.get
      ).toOption.get
    CategoricalObservationSource(evidence, target).toOption.get

  private def source(operator: Boolean = false) =
    CrossDomainObservationSource(
      categoricalSource(sourceSamples, features, sourceValues, sourceLabels, operator),
      categoricalSource(targetSamples, features, targetValues, targetLabels, operator)
    ).toOption.get

  private val measurement =
    Measurement
      .hardSelection(
        features,
        MeasurementId.unsafe("cross-domain-xy"),
        Injection
          .from(
            IArray.unsafeFromArray(Array(0, 1)),
            IndexSpace.of(features.size).toOption.get
          )
          .toOption
          .get
      )
      .toOption
      .get
  private val frame =
    MeasurementFrame(features)(
      Vector(MeasurementEntry(measurement, NoRendition))
    ).toOption.get

  private def strategy(maxElements: Long = 16L) =
    ExecutionStrategy(
      BackendId.unsafe("cross-domain-portable"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(maxElements)),
      FallbackPolicy.forbidden,
      ResultDelivery.Collected,
      Vector("predictive-scope" -> "source-fit-target-assessment")
    ).toOption.get

  private def runTyped(
      value: CrossDomainObservationSource[
        sourceSamples.Id,
        targetSamples.Id,
        features.Id,
        FeatureId
      ]
  ) =
    val design =
      CrossDomainAssessmentDesign(
        value,
        CrossDomainGeneralizationAxis("dataset").toOption.get
      ).toOption.get
    Mvpa
      .run(value)(
        design,
        frame,
        value.generalize(configuration),
        strategy()
      )
      .toOption
      .get

  test("source-only fit predicts the declared target domain with exact receipts"):
    val result = runTyped(source())
    val estimate = success(result.values.head)

    assertEquals(
      estimate.predictions.predicted.toVector,
      targetLabels
    )
    assertEqualsDouble(estimate.accuracy.value, 1.0, 1e-12)
    assertEqualsDouble(estimate.balancedAccuracy.value, 1.0, 1e-12)
    assertEquals(estimate.confusionMatrix.rowMajor, Vector(2L, 0L, 0L, 2L))
    assertEquals(
      estimate.receipt.adaptation,
      ClassifierAdaptation.InductiveFrozenSourceDomain
    )
    assertEquals(
      estimate.receipt.sourceSamples,
      sourceSamples.identity
    )
    assertEquals(
      estimate.receipt.targetSamples,
      targetSamples.identity
    )
    assertEquals(result.receipt.work.materializedCells, 16L)

  test("dense and operator evidence use one classifier kernel and agree exactly"):
    val dense = success(runTyped(source()).values.head)
    val operator = success(runTyped(source(operator = true)).values.head)

    assertEquals(
      operator.predictions.predicted.toVector,
      dense.predictions.predicted.toVector
    )
    assertEqualsDouble(operator.accuracy.value, dense.accuracy.value, 0.0)
    targetSamples.keys.indices.foreach: row =>
      classes.keys.indices.foreach: klass =>
        assertEqualsDouble(
          operator.predictions.scores.get.scoreAt(row, klass).toOption.get.value,
          dense.predictions.scores.get.scoreAt(row, klass).toOption.get.value,
          1e-12
        )

  test("the base-R cross-domain decision-score oracle survives the clean architecture"):
    val fixture =
      CrossDomainClassificationReferenceFixtures.CorrelationCentroid
    val fixtureFeatures =
      AxisRef
        .create(
          AxisId.unsafe("classification-oracle-correspondence"),
          AxisPurpose.NeuralFeatures,
          Vector.tabulate(fixture.sourceRows.cols)(index => FeatureId.unsafe(s"feature-$index")),
          CoordinateBasis.unsafe("classification-oracle-column-order"),
          None,
          AxisScale.nominal,
          CoordinateProvenance.unsafe(
            "r-parity-generator",
            "cross-domain-correlation-centroid-v1"
          )
        )
        .toOption
        .get
    val fixtureClasses =
      ClassAxis
        .create(
          AxisId.unsafe("classification-oracle-classes"),
          fixture.sourceLabels.distinct.map(ClassId.unsafe),
          CoordinateProvenance.unsafe(
            "r-parity-generator",
            "cross-domain-correlation-centroid-v1"
          )
        )
        .toOption
        .get
    val fixtureSourceSamples =
      sampleAxis(
        "classification-oracle-source-samples",
        "source",
        fixture.sourceRows.rows
      )
    val fixtureTargetSamples =
      sampleAxis(
        "classification-oracle-target-samples",
        "target",
        fixture.targetRows.rows
      )

    def domain[S <: multivar.core.SemanticSpace](
        samples: AxisRef.Aux[SampleId, S],
        values: DMat,
        labels: Vector[String],
        valueId: String
    ) =
      val evidence =
        EvidenceTable
          .dense(
            samples,
            fixtureFeatures,
            values,
            ValueId.unsafe(valueId)
          )
          .toOption
          .get
      val target =
        CategoricalTarget(
          samples,
          fixtureClasses,
          Column(samples, labels.map(ClassId.unsafe)).toOption.get
        ).toOption.get
      CategoricalObservationSource(evidence, target).toOption.get

    val sourceDomain =
      domain(
        fixtureSourceSamples,
        fixture.sourceRows,
        fixture.sourceLabels,
        "classification-oracle-source-patterns"
      )
    val targetDomain =
      domain(
        fixtureTargetSamples,
        fixture.targetRows,
        fixture.targetLabels,
        "classification-oracle-target-patterns"
      )
    val crossDomain =
      CrossDomainObservationSource(sourceDomain, targetDomain).toOption.get
    val cases = fixture.regionalCases ++ fixture.searchlightCases
    val featureSpace = IndexSpace.of(fixtureFeatures.size).toOption.get
    val entries = cases.map: expected =>
      val selected =
        Injection
          .from(
            IArray.unsafeFromArray(expected.featureOrdinals.toArray),
            featureSpace
          )
          .toOption
          .get
      val measurement =
        Measurement
          .hardSelection(
            fixtureFeatures,
            MeasurementId.unsafe(
              s"classification-oracle-${expected.measurementOrdinal}"
            ),
            selected
          )
          .toOption
          .get
      MeasurementEntry(measurement, expected.label)
    val fixtureFrame = MeasurementFrame(fixtureFeatures)(entries).toOption.get
    val fixtureDesign =
      CrossDomainAssessmentDesign(
        crossDomain,
        CrossDomainGeneralizationAxis("dataset").toOption.get
      ).toOption.get
    val fixtureConfiguration =
      ClassificationConfiguration(
        CorrelationCentroid(),
        adaptation = ClassifierAdaptation.InductiveFrozenSourceDomain
      ).toOption.get
    val result =
      Mvpa
        .run(crossDomain)(
          fixtureDesign,
          fixtureFrame,
          crossDomain.generalize(fixtureConfiguration),
          strategy(
            math.max(
              fixture.sourceRows.rows.toLong * fixture.sourceRows.cols.toLong,
              fixture.targetRows.rows.toLong * fixture.targetRows.cols.toLong
            )
          )
        )
        .toOption
        .get

    result.values
      .zip(cases)
      .foreach: (measured, expected) =>
        measured.outcome match
          case MeasurementOutcome.Success(estimate, _) =>
            assertEquals(
              estimate.predictions.classes.keys.map(_.value),
              expected.expectedClasses
            )
            assertEquals(
              estimate.predictions.predicted.toVector.map(_.value),
              expected.expectedPredicted
            )
            assertEqualsDouble(
              estimate.accuracy.value,
              expected.expectedAccuracy,
              1e-12
            )
            val scores = estimate.predictions.scores.get
            var row = 0
            while row < scores.samples.size do
              var column = 0
              while column < scores.classes.size do
                assertEqualsDouble(
                  scores.scoreAt(row, column).toOption.get.value,
                  expected.expectedDecisionScores(row, column),
                  1e-12,
                  s"measurement=${measured.measurement.id.value}, row=$row, class=$column"
                )
                column += 1
              row += 1
          case other =>
            fail(s"expected successful cross-domain oracle estimate, obtained $other")

  test("adaptation scope is explicit and incompatible programs fail at bind time"):
    val value = source()
    val design =
      CrossDomainAssessmentDesign(
        value,
        CrossDomainGeneralizationAxis("subject").toOption.get
      ).toOption.get
    val withinDomain =
      ClassificationConfiguration(
        CorrelationCentroid(),
        adaptation = ClassifierAdaptation.InductiveWithinDomain
      ).toOption.get
    val rejected =
      Mvpa.run(value)(
        design,
        frame,
        value.generalize(withinDomain),
        strategy()
      )

    val unsupported = rejected.left.exists:
      case MvpaRunError.Binding(
            BindError.EstimandRejected(
              _,
              CrossDomainBindRejection.UnsupportedAdaptation(actual),
              _
            )
          ) =>
        actual == ClassifierAdaptation.InductiveWithinDomain
      case _ => false
    assert(
      unsupported,
      "within-domain adaptation must not be admitted for source-to-target generalization"
    )

  test("equal dimensions and unrelated nominal spaces cannot define correspondence"):
    val errors = compileErrors("""
      import multivar.core.SemanticSpace
      import scalafim.fmri.mvpa.*
      import scalafim.fmri.mvpa.predictive.*
      def invalid[
          S <: SemanticSpace,
          T <: SemanticSpace,
          LeftNeural <: SemanticSpace,
          RightNeural <: SemanticSpace,
          K
      ](
          source: CategoricalObservationSource[S, LeftNeural, K],
          target: CategoricalObservationSource[T, RightNeural, K]
      ) = CrossDomainObservationSource(source, target)
    """)

    assert(errors.nonEmpty)
    assert(errors.contains("Required:"))

  private def success[T <: multivar.core.SemanticSpace](
      value: MeasurementValue[
        CrossDomainClassificationEstimate[
          T,
          CorrelationCentroid,
          CorrelationCentroidFit,
          CategoricalClassifierError,
          AlderClassPrediction
        ],
        CrossDomainBindRejection,
        CrossDomainCompileError[CategoricalClassifierError],
        ?
      ]
  ): CrossDomainClassificationEstimate[
    T,
    CorrelationCentroid,
    CorrelationCentroidFit,
    CategoricalClassifierError,
    AlderClassPrediction
  ] =
    value.outcome match
      case MeasurementOutcome.Success(result, _) => result
      case other                                 => fail(s"expected successful cross-domain estimate, obtained $other")

  private final class MatrixFree private (value: DMat) extends DoubleLinearOperator:
    override def rows: Int = value.rows
    override def cols: Int = value.cols

    override def applyTo(
        input: DVec,
        output: MutableDVec
    ): Unit =
      var row = 0
      while row < rows do
        var sum = 0.0
        var column = 0
        while column < cols do
          sum += value(row, column) * input(column)
          column += 1
        output(row) = sum
        row += 1

    override def transposeApplyTo(
        input: DVec,
        output: MutableDVec
    ): Unit =
      var column = 0
      while column < cols do
        var sum = 0.0
        var row = 0
        while row < rows do
          sum += value(row, column) * input(row)
          row += 1
        output(column) = sum
        column += 1

  private object MatrixFree:
    def apply(value: DMat): MatrixFree = new MatrixFree(value)
