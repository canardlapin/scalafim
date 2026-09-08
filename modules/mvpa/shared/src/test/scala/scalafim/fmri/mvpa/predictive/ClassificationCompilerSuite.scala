package scalafim.fmri.mvpa.predictive

import multivar.core.ValueId
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.*

final class ClassificationCompilerSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private val cat = ClassId.unsafe("cat")
  private val dog = ClassId.unsafe("dog")
  private val samples = sampleAxis()
  private val features = featureAxis()
  private val classes =
    ClassAxis
      .create(
        AxisId.unsafe("compiler-animal-classes"),
        Vector(cat, dog),
        CoordinateProvenance.unsafe("compiler-fixture", "v1")
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
        ValueId.unsafe("compiler-patterns")
      )
      .toOption
      .get
  private val measurement =
    val space = IndexSpace.of(features.size).toOption.get
    Measurement
      .hardSelection(
        features,
        MeasurementId.unsafe("compiler-global"),
        Injection.from(indices(0, 1), space).toOption.get
      )
      .toOption
      .get
  private val design = validation()
  private val configuration =
    ClassificationConfiguration(
      StandardizedNearestCentroid(
        StandardizationSpecification.CenterScaleRejectConstant
      )
    ).toOption.get
  private val plan =
    ScientificPlanFingerprint(
      "scalafim-mvpa-plan-v1-" + Vector.fill(64)("c").mkString
    ).toOption.get

  private def sampleAxis(): AxisRef[SampleId] =
    AxisRef
      .create(
        AxisId.unsafe("compiler-samples"),
        AxisPurpose.Samples,
        Vector.tabulate(8)(index => SampleId.unsafe(s"run-${index / 2}-trial-${index % 2}")),
        CoordinateBasis.unsafe("run-trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("compiler-fixture", "v1")
      )
      .toOption
      .get

  private def featureAxis(): AxisRef[FeatureId] =
    AxisRef
      .create(
        AxisId.unsafe("compiler-features"),
        AxisPurpose.NeuralFeatures,
        Vector(FeatureId.unsafe("voxel-x"), FeatureId.unsafe("voxel-y")),
        CoordinateBasis.unsafe("voxel-table"),
        Some(AxisUnits.unsafe("percent-signal-change")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("compiler-mask", "v1")
      )
      .toOption
      .get

  private def validation() =
    val runs = Labels.dense(indices(0, 0, 1, 1, 2, 2, 3, 3), samples.size).toOption.get
    val ordinal = LeaveOneGroupOut(runs)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 83L)
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
        ScientificAxisName.unsafe("run"),
        samples.identity
      )
    ).toOption.get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  private def run =
    ClassificationCompiler.run(
      plan,
      evidence,
      target,
      design,
      measurement,
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(16L)),
      configuration
    )

  test("centroid and ridge-LDA methods share one predictive estimand and Alder lifecycle"):
    def verify[
        Configuration,
        Fitted,
        LearnerError,
        Prediction <: CategoricalLearnerPrediction
    ](
        method: Configuration
    )(using
        compiler: CategoricalLearnerCompiler[
          Configuration,
          Fitted,
          LearnerError,
          Prediction
        ]
    ): EstimandIdentity =
      val configured = ClassificationConfiguration(method).toOption.get
      val result = ClassificationCompiler
        .run(
          plan,
          evidence,
          target,
          design,
          measurement,
          MaterializationPolicy.Allow(MaterializationBudget.unsafe(16L)),
          configured
        )
        .toOption
        .get

      assertEqualsDouble(result.accuracy.value, 1.0, 1e-12)
      assertEquals(
        result.foldReceipts.map(_.fitAudit.component.id.render).distinct,
        Vector(s"scalafim.mvpa.${configured.definition.id.text}")
      )
      assertEquals(result.configuration.adaptation, ClassifierAdaptation.InductiveWithinDomain)
      assert(result.configuration.identity.fingerprint.value.nonEmpty)
      configured.identity

    val identities = Vector(
      verify(
        StandardizedNearestCentroid(
          StandardizationSpecification.CenterScaleRejectConstant
        )
      ),
      verify(CorrelationCentroid()),
      verify(SwiftCentroid(PredictorScaling.ZScore)),
      verify(
        SwiftCentroid(
          PredictorScaling.DiagonalShrinkage(ScalingShrinkage(0.2).toOption.get)
        )
      ),
      verify(RidgeLda(ClassifierRidge(0.01).toOption.get))
    )
    assertEquals(
      identities.distinct.length,
      identities.length
    )

  test("leave-one-run-out compiles through Alder with exact fold roles"):
    val result = run.toOption.get

    assertEquals(result.foldEstimates.length, 4)
    assertEquals(
      result.foldEstimates.map(_._2.accuracy.value),
      Vector.fill(4)(1.0)
    )
    assertEquals(result.foldReceipts.length, 4)
    assertEquals(
      result.foldReceipts.flatMap(_.assessmentSamples),
      samples.keys
    )
    result.foldReceipts.foreach: receipt =>
      assertEquals(
        receipt.analysisSamples.toSet.intersect(receipt.assessmentSamples.toSet),
        Set.empty[SampleId]
      )
      assertEquals(receipt.analysisSamples.length, 6)
      assertEquals(receipt.assessmentSamples.length, 2)
      assertEquals(receipt.seed.value, 83L)
      assert(receipt.fitIdentity.digest.nonEmpty)
      assertEquals(receipt.fitAudit.data.digest, receipt.analysisFingerprint.digest)
      assertEquals(receipt.fitAudit.children, Vector.empty)
      assertEquals(
        receipt.fitAudit.component.id.render,
        "scalafim.mvpa.standardized-nearest-centroid"
      )

    assertEquals(result.preparation.restriction.sourceRows, 8)
    assertEquals(result.preparation.restriction.selectedRows, 8)
    assertEquals(result.preparation.restriction.mappingEntries, 0L)
    assertEquals(result.preparation.materialization.elements, 16L)

  test("assessment values and receipts remain fold-linked"):
    val result = run.toOption.get

    result.foldEstimates
      .zip(result.foldReceipts)
      .foreach: pair =>
        val ((unit, estimate), receipt) = pair
        assertEquals(unit, receipt.unit)
        assertEquals(
          estimate.observations.map(_.sample),
          receipt.assessmentSamples
        )
        assertEquals(
          estimate.observations.map(_.truth),
          estimate.observations.map(_.prediction.predicted)
        )
        estimate.observations.foreach: observation =>
          assertEquals(
            observation.prediction.scores.map(_._1),
            classes.keys
          )

  test("the predictive compiler has no legacy ontology parameter"):
    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import scalafim.fmri.mvpa.predictive.*
      final case class OldFoldCarrier(value: Vector[Int])
      def legacy(
          learner: String,
          target: Vector[String],
          folds: OldFoldCarrier
      ) = ClassificationCompiler.run(learner, target, folds)
    """)
    assert(errors.nonEmpty)
