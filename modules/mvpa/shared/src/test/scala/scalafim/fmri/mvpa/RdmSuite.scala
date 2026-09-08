package scalafim.fmri.mvpa

import multivar.core.ValueId
import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.ObservationRdm.given

final class RdmSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val samples = right(
    AxisRef.create(
      AxisId.unsafe("observation-rdm-samples"),
      AxisPurpose.Samples,
      Vector("trial-z", "trial-a", "trial-q").map(SampleId.unsafe),
      CoordinateBasis.unsafe("declared-trial-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("observation-rdm-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("observation-rdm-neural"),
      AxisPurpose.NeuralFeatures,
      Vector("voxel-c", "voxel-a", "voxel-b").map(FeatureId.unsafe),
      CoordinateBasis.unsafe("declared-voxel-order"),
      Some(AxisUnits.unsafe("activation")),
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("observation-rdm-suite", "v1")
    )
  )

  private val matrix = GaleTestMatrix.fromRows(
    Seq(
      Seq(0.0, 0.0, 0.0),
      Seq(3.0, 4.0, 0.0),
      Seq(1.0, 1.0, 0.0)
    )
  )

  private val observations = right(
    Observations(
      right(
        EvidenceTable.dense(
          samples,
          neural,
          matrix,
          ValueId.unsafe("observation-rdm-values")
        )
      )
    )
  )

  private val design = right(ObservationFitDesign.entireTable(samples))

  private val measurement =
    val injection = right(
      Injection.from(
        IArray.unsafeFromArray(Array(0, 1)),
        right(IndexSpace.of(neural.size))
      )
    )
    right(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe("first-two-voxels"),
        injection
      )
    )

  private val frame = right(
    MeasurementFrame(neural)(Vector(MeasurementEntry(measurement, NoRendition)))
  )

  private val strategy = right(
    ExecutionStrategy(
      BackendId.unsafe("observation-rdm-portable"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(6L)),
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )

  private type RdmAnalysis = AnalysisResult[
    MeasuredObservationRdm,
    ObservationRdmBindRejection,
    ObservationRdmFailure,
    NoRendition.type
  ]

  private def estimate(distance: ObservationDistance): RdmAnalysis =
    right(
      Mvpa.run(observations)(
        design,
        frame,
        ObservationRdm(observations, distance),
        strategy
      )
    )

  private def onlySuccess(result: RdmAnalysis): MeasuredObservationRdm =
    result.values match
      case Vector(MeasurementValue(_, _, MeasurementOutcome.Success(value, _))) => value
      case other => fail(s"expected one observation RDM, obtained $other")

  test("sample RDM retains the exact pair axis and canonical declared order"):
    val analysis = estimate(
      ObservationDistance.SquaredEuclidean(RdmNormalization.Raw)
    )
    val rdm = onlySuccess(analysis)

    assertEquals(
      rdm.domain.pairs.map(pair => pair.first -> pair.second),
      Vector(
        samples.keys(0) -> samples.keys(1),
        samples.keys(0) -> samples.keys(2),
        samples.keys(1) -> samples.keys(2)
      )
    )
    assertEquals(rdm.distances.toVector, Vector(25.0, 2.0, 13.0))
    assertEqualsDouble(
      right(rdm.distance(samples.keys(2), samples.keys(0))),
      2.0,
      1e-12
    )
    assertEquals(analysis.values.head.measurement, measurement.identity)
    assertEquals(analysis.values.head.outcome.receipt.measurement, measurement.identity)
    assertEquals(analysis.values.head.outcome.receipt.materializedCells, 6L)

  test("normalization and Euclidean distance are distinct estimands"):
    val rawAnalysis = estimate(
      ObservationDistance.SquaredEuclidean(RdmNormalization.Raw)
    )
    val normalizedAnalysis = estimate(
      ObservationDistance.SquaredEuclidean(
        RdmNormalization.DivideByNeuralDimension
      )
    )
    val euclideanAnalysis = estimate(ObservationDistance.Euclidean)
    val raw = onlySuccess(rawAnalysis)
    val normalized = onlySuccess(normalizedAnalysis)
    val euclidean = onlySuccess(euclideanAnalysis)

    assertEqualsDouble(raw.distances.toVector.head, 25.0, 1e-12)
    assertEqualsDouble(normalized.distances.toVector.head, 12.5, 1e-12)
    assertEqualsDouble(euclidean.distances.toVector.head, 5.0, 1e-12)
    assertNotEquals(rawAnalysis.plan, normalizedAnalysis.plan)

  test("correlation distance has its own numerical oracle"):
    val correlationMatrix = GaleTestMatrix.fromRows(
      Seq(
        Seq(1.0, 2.0, 3.0),
        Seq(1.0, 2.0, 3.0),
        Seq(3.0, 2.0, 1.0)
      )
    )
    val source = right(
      Observations(
        right(
          EvidenceTable.dense(
            samples,
            neural,
            correlationMatrix,
            ValueId.unsafe("observation-correlation-values")
          )
        )
      )
    )
    val result = right(
      Mvpa.run(source)(
        design,
        frame,
        ObservationRdm(source, ObservationDistance.Correlation),
        strategy
      )
    )
    val rdm = result.values.head.outcome match
      case MeasurementOutcome.Success(value, _) => value
      case other                                => fail(s"expected correlation RDM, obtained $other")

    assertEqualsDouble(rdm.distances.toVector(0), 0.0, 1e-12)
    assertEqualsDouble(rdm.distances.toVector(1), 2.0, 1e-12)
    assertEqualsDouble(rdm.distances.toVector(2), 2.0, 1e-12)

  test("correlation distance rejects underspecified measurements at bind time"):
    val oneVoxel =
      val injection = right(
        Injection.from(
          IArray.unsafeFromArray(Array(0)),
          right(IndexSpace.of(neural.size))
        )
      )
      right(
        Measurement.hardSelection(
          neural,
          MeasurementId.unsafe("one-voxel"),
          injection
        )
      )
    val oneVoxelFrame = right(
      MeasurementFrame(neural)(Vector(MeasurementEntry(oneVoxel, NoRendition)))
    )

    Mvpa.run(observations)(
      design,
      oneVoxelFrame,
      ObservationRdm(observations, ObservationDistance.Correlation),
      strategy
    ) match
      case Left(
            MvpaRunError.Binding(
              BindError.EstimandRejected(
                _,
                ObservationRdmBindRejection.MeasurementTooSmall(id, 2, 1),
                _
              )
            )
          ) =>
        assertEquals(id, oneVoxel.identity.id)
      case other => fail(s"expected correlation bind rejection, obtained $other")
