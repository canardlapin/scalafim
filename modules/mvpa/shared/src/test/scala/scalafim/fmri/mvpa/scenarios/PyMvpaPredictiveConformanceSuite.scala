package scalafim.fmri.mvpa.scenarios

import multivar.core.ValueId
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.predictive.*

final class PyMvpaPredictiveConformanceSuite extends munit.FunSuite:
  import PredictiveAnalysis.given

  private given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private val fixture = MvpaExternalReferenceFixture

  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val samples = right(
    AxisRef.create(
      AxisId.unsafe("external-predictive-samples"),
      AxisPurpose.Samples,
      fixture.predictiveSampleIds.map(SampleId.unsafe),
      CoordinateBasis.unsafe("declared-run-class-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("pymvpa-fixture", "v1")
    )
  )
  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("external-predictive-features"),
      AxisPurpose.NeuralFeatures,
      fixture.predictiveFeatureIds.map(FeatureId.unsafe),
      CoordinateBasis.unsafe("declared-feature-order"),
      Some(AxisUnits.unsafe("activation")),
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("pymvpa-fixture", "v1")
    )
  )
  private val classValues = fixture.classIds.map(ClassId.unsafe)
  private val classes = right(
    ClassAxis.create(
      AxisId.unsafe("external-predictive-classes"),
      classValues,
      CoordinateProvenance.unsafe("pymvpa-fixture", "v1")
    )
  )
  private val target = right(
    CategoricalTarget(
      samples,
      classes,
      right(
        Column(
          samples,
          fixture.predictiveLabels.map(ClassId.unsafe)
        )
      )
    )
  )
  private val evidence = right(
    EvidenceTable.dense(
      samples,
      neural,
      GaleTestMatrix.fromRows(fixture.predictivePatterns),
      ValueId.unsafe("external-predictive-patterns")
    )
  )
  private val source = right(CategoricalObservationSource(evidence, target))

  private def validation(generalizationName: String) =
    val runKeys = fixture.predictiveRunIds.distinct
    val runPositions = fixture.predictiveRunIds.map: run =>
      runKeys.indexOf(run)
    assert(runPositions.forall(_ >= 0))
    val labels = right(
      Labels.dense(
        IArray.unsafeFromArray(runPositions.toArray),
        samples.size
      )
    )
    val ordinal = LeaveOneGroupOut(labels)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 20260826L)
    val compiled = right(
      ordinal.compile(right(IndexSpace.of(samples.size)), authority.seed)
    )
    val schedule = right(
      BoundSchedule(
        compiled,
        samples,
        right(AxisPopulationFingerprint.fromAxis(samples)),
        right(ScheduleLabels.fromDesign(samples, ordinal)),
        authority
      )
    )
    right(
      ValidationDesign(
        schedule,
        ScientificAxisName.unsafe("samples"),
        GeneralizationAxis(
          ScientificAxisName.unsafe(generalizationName),
          samples.identity
        )
      )
    )

  private val executableValidation = validation("samples")
  private val truthfulRunValidation = validation("run")

  private def selection(id: String, ordinals: Vector[Int]) =
    right(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe(id),
        right(
          Injection.from(
            IArray.unsafeFromArray(ordinals.toArray),
            right(IndexSpace.of(neural.size))
          )
        )
      )
    )

  private val whole = right(
    Measurement.identity(neural, MeasurementId.unsafe("00-whole"))
  )
  private val searchlightMeasurements = fixture.searchlights.zipWithIndex.map: (support, index) =>
    selection(s"searchlight-${index + 1}", support.ordinals)
  private val fullFrame = right(
    MeasurementFrame(neural)(
      Vector(MeasurementEntry(whole, "whole")) ++
        searchlightMeasurements
          .zip(fixture.searchlights)
          .map: (measurement, support) =>
            MeasurementEntry(measurement, support.center)
    )
  )
  private val wholeFrame = right(
    MeasurementFrame(neural)(Vector(MeasurementEntry(whole, "whole")))
  )
  private val strategy = right(
    ExecutionStrategy(
      BackendId.unsafe("alder-portable-external-conformance"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(48L)),
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )

  private val centroidConfiguration = right(
    ClassificationConfiguration(
      StandardizedNearestCentroid(
        StandardizationSpecification.CenterScaleRejectConstant
      )
    )
  )

  // BEGIN SCENARIO pymvpa-centroid-searchlights
  test("NFold, formula-matched centroid, and fixed supports reproduce PyMVPA"):
    val result = right(
      Mvpa.run(source)(
        executableValidation,
        fullFrame,
        source.classify(centroidConfiguration),
        strategy
      )
    )
    val estimates = result.values
      .map: value =>
        val estimate = value.outcome match
          case MeasurementOutcome.Success(observed, _) => observed
          case other                                   => fail(s"expected classification success, obtained $other")
        value.rendition -> estimate
      .toMap

    val wholeEstimate = estimates("whole")
    assertEquals(
      wholeEstimate.predictions.samples.keys.map(_.value),
      fixture.predictiveSampleIds
    )
    assertEquals(
      wholeEstimate.predictions.predicted.toVector.map(_.value),
      fixture.expectedCentroidPredictions
    )
    assertEqualsDouble(
      wholeEstimate.accuracy.value,
      fixture.expectedCentroidAccuracy,
      1e-12
    )
    wholeEstimate.foldReceipts
      .zip(fixture.expectedFolds)
      .foreach: (actual, expected) =>
        assertEquals(
          actual.analysisSamples.map(_.value),
          expected.analysis
        )
        assertEquals(
          actual.assessmentSamples.map(_.value),
          expected.assessment
        )

    fixture.expectedSearchlights.foreach: expected =>
      val actual = estimates(expected.center)
      assertEquals(
        actual.predictions.predicted.toVector.map(_.value),
        expected.predicted
      )
      assertEqualsDouble(actual.accuracy.value, expected.accuracy, 1e-12)
  // END SCENARIO pymvpa-centroid-searchlights

  test("ScalaFIM RidgeLda conditionally agrees with PyMVPA LDA predictions"):
    val configuration = right(
      ClassificationConfiguration(
        RidgeLda(right(ClassifierRidge(1e-12)))
      )
    )
    val result = right(
      Mvpa.run(source)(
        executableValidation,
        wholeFrame,
        source.classify(configuration),
        strategy
      )
    )
    val estimate = result.values.head.outcome match
      case MeasurementOutcome.Success(observed, _) => observed
      case other                                   => fail(s"expected RidgeLda success, obtained $other")

    assertEquals(
      estimate.predictions.predicted.toVector.map(_.value),
      fixture.expectedLdaPredictions
    )
    assertEqualsDouble(
      estimate.accuracy.value,
      fixture.expectedLdaAccuracy,
      1e-12
    )
    assertEquals(
      estimate.configuration.definition.id.text,
      "ridge-lda"
    )

  test("the current source identity cannot state run as its generalization axis"):
    Mvpa.run(source)(
      truthfulRunValidation,
      wholeFrame,
      source.classify(centroidConfiguration),
      strategy
    ) match
      case Left(
            MvpaRunError.Specification(
              ScientificSpecificationError.UnknownDesignAxis(name)
            )
          ) =>
        assertEquals(name.value, "run")
      case other => fail(s"expected truthful run-axis rejection, obtained $other")
