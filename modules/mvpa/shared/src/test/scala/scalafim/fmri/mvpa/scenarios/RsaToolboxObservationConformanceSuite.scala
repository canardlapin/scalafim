package scalafim.fmri.mvpa.scenarios

import multivar.core.ValueId
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.ObservationRdm.given

final class RsaToolboxObservationConformanceSuite extends munit.FunSuite:
  private val fixture = MvpaExternalReferenceFixture

  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val samples = right(
    AxisRef.create(
      AxisId.unsafe("external-observation-samples"),
      AxisPurpose.Samples,
      fixture.observationSampleIds.map(SampleId.unsafe),
      CoordinateBasis.unsafe("declared-observation-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("rsa-toolbox-pymvpa-fixture", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("external-observation-features"),
      AxisPurpose.NeuralFeatures,
      fixture.observationFeatureIds.map(FeatureId.unsafe),
      CoordinateBasis.unsafe("declared-feature-order"),
      Some(AxisUnits.unsafe("activation")),
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("rsa-toolbox-pymvpa-fixture", "v1")
    )
  )

  private val evidence = right(
    EvidenceTable.dense(
      samples,
      neural,
      GaleTestMatrix.fromRows(fixture.observationPatterns),
      ValueId.unsafe("external-observation-patterns")
    )
  )
  private val observations = right(Observations(evidence))
  private val design = right(ObservationFitDesign.entireTable(samples))
  private val measurement = right(
    Measurement.identity(neural, MeasurementId.unsafe("all-observation-features"))
  )
  private val frame = right(
    MeasurementFrame(neural)(Vector(MeasurementEntry(measurement, NoRendition)))
  )
  private val strategy = right(
    ExecutionStrategy(
      BackendId.unsafe("portable-external-conformance"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(24L)),
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )

  private def estimate(distance: ObservationDistance): MeasuredObservationRdm =
    estimateFrom(observations, distance)

  private def estimateFrom(
      source: Observations[samples.Id, neural.Id, FeatureId],
      distance: ObservationDistance
  ): MeasuredObservationRdm =
    val result = right(
      Mvpa.run(source)(
        design,
        frame,
        ObservationRdm(source, distance),
        strategy
      )
    )
    result.values.head.outcome match
      case MeasurementOutcome.Success(value, _) => value
      case other                                => fail(s"expected RDM success, obtained $other")

  private def assertValues(actual: Vector[Double], expected: Vector[Double]): Unit =
    assertEquals(actual.length, expected.length)
    actual
      .zip(expected)
      .foreach: (left, right) =>
        assertEqualsDouble(left, right, 1e-12)

  // BEGIN SCENARIO observation-rdm
  test("public observation RDM matches PyMVPA PDist and Python rsatoolbox"):
    val raw = estimate(
      ObservationDistance.SquaredEuclidean(RdmNormalization.Raw)
    )
    val normalized = estimate(
      ObservationDistance.SquaredEuclidean(
        RdmNormalization.DivideByNeuralDimension
      )
    )
    val euclidean = estimate(ObservationDistance.Euclidean)
    val correlation = estimate(ObservationDistance.Correlation)

    val observedPairOrder = raw.domain.pairs.map: pair =>
      pair.first.value -> pair.second.value
    assertEquals(observedPairOrder, fixture.observationPairOrder)
    assertValues(raw.distances.toVector, fixture.squaredEuclideanRaw)
    assertValues(normalized.distances.toVector, fixture.squaredEuclideanPerFeature)
    assertValues(euclidean.distances.toVector, fixture.euclidean)
    assertValues(correlation.distances.toVector, fixture.correlation)
  // END SCENARIO observation-rdm

  test("observation distances obey their declared affine metamorphisms"):
    val scale = 2.5
    val shift = 7.0
    val transformedPatterns = fixture.observationPatterns.map: row =>
      row.map(value => scale * value + shift)
    val transformedEvidence = right(
      EvidenceTable.dense(
        samples,
        neural,
        GaleTestMatrix.fromRows(transformedPatterns),
        ValueId.unsafe("external-observation-affine-transform")
      )
    )
    val transformed = right(Observations(transformedEvidence))
    val baselineSquared = estimate(
      ObservationDistance.SquaredEuclidean(RdmNormalization.Raw)
    ).distances.toVector
    val transformedSquared = estimateFrom(
      transformed,
      ObservationDistance.SquaredEuclidean(RdmNormalization.Raw)
    ).distances.toVector
    val baselineCorrelation =
      estimate(ObservationDistance.Correlation).distances.toVector
    val transformedCorrelation =
      estimateFrom(transformed, ObservationDistance.Correlation).distances.toVector

    assertValues(
      transformedSquared,
      baselineSquared.map(_ * scale * scale)
    )
    assertValues(transformedCorrelation, baselineCorrelation)

  test("PyMVPA target similarity is reproducible downstream from the typed RDM"):
    val distances = estimate(
      ObservationDistance.SquaredEuclidean(RdmNormalization.Raw)
    ).distances.toVector
    val target = Vector(
      0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0
    )

    assertEqualsDouble(
      pearson(distances, target),
      fixture.targetSimilarityPearson,
      1e-12
    )
    assertEqualsDouble(
      pearson(averageRanks(distances), averageRanks(target)),
      fixture.targetSimilaritySpearman,
      1e-12
    )

  test("correlation distance fails locally and explicitly for a constant observation"):
    val constantPatterns = fixture.observationPatterns.updated(
      2,
      Vector.fill(neural.size)(3.0)
    )
    val constantEvidence = right(
      EvidenceTable.dense(
        samples,
        neural,
        GaleTestMatrix.fromRows(constantPatterns),
        ValueId.unsafe("external-observation-constant-row")
      )
    )
    val constantSource = right(Observations(constantEvidence))
    val result = right(
      Mvpa.run(constantSource)(
        design,
        frame,
        ObservationRdm(constantSource, ObservationDistance.Correlation),
        strategy
      )
    )

    result.values.head.outcome match
      case MeasurementOutcome.Failed(
            ObservationRdmFailure.ZeroVarianceSample(sample),
            _
          ) =>
        assertEquals(sample, samples.keys(2))
      case other => fail(s"expected zero-variance failure, obtained $other")

  private def pearson(left: Vector[Double], right: Vector[Double]): Double =
    val leftMean = left.sum / left.length.toDouble
    val rightMean = right.sum / right.length.toDouble
    var numerator = 0.0
    var leftSquare = 0.0
    var rightSquare = 0.0
    var index = 0
    while index < left.length do
      val centeredLeft = left(index) - leftMean
      val centeredRight = right(index) - rightMean
      numerator += centeredLeft * centeredRight
      leftSquare += centeredLeft * centeredLeft
      rightSquare += centeredRight * centeredRight
      index += 1
    numerator / math.sqrt(leftSquare * rightSquare)

  private def averageRanks(values: Vector[Double]): Vector[Double] =
    val sorted = values.zipWithIndex.sortBy(_._1)
    val ranks = Array.ofDim[Double](values.length)
    var start = 0
    while start < sorted.length do
      var end = start + 1
      while end < sorted.length && sorted(end)._1 == sorted(start)._1 do end += 1
      val rank = (start + 1 + end).toDouble / 2.0
      var position = start
      while position < end do
        ranks(sorted(position)._2) = rank
        position += 1
      start = end
    ranks.toVector
