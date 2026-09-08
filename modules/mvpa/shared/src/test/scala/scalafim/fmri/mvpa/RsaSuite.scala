package scalafim.fmri.mvpa

import multivar.core.ValueId
import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.SamplewiseRsa.given

final class RsaSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val samples = right(
    AxisRef.create(
      AxisId.unsafe("samplewise-rsa-samples"),
      AxisPurpose.Samples,
      Vector("trial-9", "trial-2", "trial-8", "trial-1").map(SampleId.unsafe),
      CoordinateBasis.unsafe("declared-trial-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("samplewise-rsa-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("samplewise-rsa-neural"),
      AxisPurpose.NeuralFeatures,
      Vector(FeatureId.unsafe("x"), FeatureId.unsafe("y")),
      CoordinateBasis.unsafe("voxel-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("samplewise-rsa-suite", "v1")
    )
  )

  private val items = right(
    AxisRef.create(
      AxisId.unsafe("samplewise-rsa-items"),
      AxisPurpose.Effects,
      Vector(AxisKey.unsafe("a"), AxisKey.unsafe("b")),
      CoordinateBasis.unsafe("condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("samplewise-rsa-suite", "v1")
    )
  )

  private val blocks = right(
    AxisRef.create(
      AxisId.unsafe("samplewise-rsa-blocks"),
      AxisPurpose.Partitions,
      Vector(PartitionId.unsafe("run-1"), PartitionId.unsafe("run-2")),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("samplewise-rsa-suite", "v1")
    )
  )

  private val itemAssignments = Vector(
    items.keys(0),
    items.keys(1),
    items.keys(0),
    items.keys(1)
  )
  private val blockAssignments = Vector(
    blocks.keys(0),
    blocks.keys(0),
    blocks.keys(1),
    blocks.keys(1)
  )

  private val observations = right(
    Observations(
      right(
        EvidenceTable.dense(
          samples,
          neural,
          GaleTestMatrix.fromRows(
            Seq(
              Seq(0.0, 0.0),
              Seq(10.0, 0.0),
              Seq(0.0, 0.0),
              Seq(10.0, 0.0)
            )
          ),
          ValueId.unsafe("samplewise-rsa-observations")
        )
      )
    )
  )

  private val source = right(
    SamplewiseRsaSource(
      observations,
      ScientificAxisName.unsafe("items"),
      items,
      right(Column(samples, itemAssignments)),
      ScientificAxisName.unsafe("runs"),
      blocks,
      right(Column(samples, blockAssignments))
    )
  )

  private val domain = right(WithinPairDomain(items))
  private val model = right(
    SecondOrderModel.signal(
      domain,
      SecondOrderModelName.unsafe("identity-model"),
      Vector(1.0)
    )
  )

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
        MeasurementId.unsafe("samplewise-rsa-all-features"),
        injection
      )
    )

  private val frame = right(
    MeasurementFrame(neural)(Vector(MeasurementEntry(measurement, NoRendition)))
  )

  private val strategy = right(
    ExecutionStrategy(
      BackendId.unsafe("samplewise-rsa-portable"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(8L)),
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )

  private def estimate(
      value: SamplewiseRsaSource[
        samples.Id,
        neural.Id,
        items.Id,
        blocks.Id,
        FeatureId,
        AxisKey,
        PartitionId
      ] = source,
      comparison: SamplewiseComparison = SamplewiseComparison.Pearson
  ): AnalysisResult[
    MeasuredSamplewiseRsa[samples.Id],
    SamplewiseRsaBindRejection,
    SamplewiseRsaFailure,
    NoRendition.type
  ] =
    val sourceDesign = right(SamplewiseRsaDesign(value))
    right(
      Mvpa.run(value)(
        sourceDesign,
        frame,
        value.estimate(
          model,
          ObservationDistance.Euclidean,
          comparison
        ),
        strategy
      )
    )

  private def onlySuccess(
      result: AnalysisResult[
        MeasuredSamplewiseRsa[samples.Id],
        SamplewiseRsaBindRejection,
        SamplewiseRsaFailure,
        NoRendition.type
      ]
  ): MeasuredSamplewiseRsa[samples.Id] =
    result.values.head.outcome match
      case MeasurementOutcome.Success(result, _) => result
      case other                                 => fail(s"expected samplewise RSA success, obtained $other")

  test("samplewise RSA exposes sample-keyed cross-block scores from typed axes"):
    val analysis = estimate()
    val result = onlySuccess(analysis)

    assertEquals(result.samples.identity, samples.identity)
    assert(result.samples.evidence eq samples.evidence)
    assertEquals(result.scores.rowIdentity, samples.identity)
    assertEquals(result.scores.toVector, Vector.fill(4)(Some(1.0)))
    assertEqualsDouble(result.meanScore.getOrElse(fail("missing mean")), 1.0, 1e-12)
    assertEquals(analysis.values.head.measurement, measurement.identity)
    assertEquals(analysis.values.head.outcome.receipt.materializedCells, 8L)
    assertEquals(
      analysis.plan.estimand.fields.find(_.name == "model").map(_.value),
      Some(model.identity.value)
    )
    assertEquals(
      analysis.plan.estimand.fields.find(_.name == "comparison").map(_.value),
      Some("pearson")
    )

  test("rank comparison uses the same exact item and block axes"):
    val analysis = estimate(comparison = SamplewiseComparison.Spearman)
    val result = onlySuccess(analysis)

    assertEquals(result.scores.toVector, Vector.fill(4)(Some(1.0)))
    assertEquals(
      analysis.plan.estimand.fields.find(_.name == "pairing").map(_.value),
      Some("each-sample-against-other-blocks")
    )
    assertEquals(
      analysis.plan.estimand.fields.find(_.name == "comparison").map(_.value),
      Some("spearman")
    )

  test("undefined row comparisons are represented by None rather than NaN"):
    val twoSamples = right(
      AxisRef.create(
        AxisId.unsafe("samplewise-rsa-two-samples"),
        AxisPurpose.Samples,
        Vector(SampleId.unsafe("left"), SampleId.unsafe("right")),
        CoordinateBasis.unsafe("declared-trial-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("samplewise-rsa-suite", "v1")
      )
    )
    val twoObservations = right(
      Observations(
        right(
          EvidenceTable.dense(
            twoSamples,
            neural,
            GaleTestMatrix.fromRows(Seq(Seq(0.0, 0.0), Seq(1.0, 0.0))),
            ValueId.unsafe("samplewise-rsa-two-observations")
          )
        )
      )
    )
    val twoSource = right(
      SamplewiseRsaSource(
        twoObservations,
        ScientificAxisName.unsafe("items"),
        items,
        right(Column(twoSamples, Vector(items.keys(0), items.keys(1)))),
        ScientificAxisName.unsafe("runs"),
        blocks,
        right(Column(twoSamples, Vector(blocks.keys(0), blocks.keys(1))))
      )
    )
    val twoDesign = right(SamplewiseRsaDesign(twoSource))
    val result = right(
      Mvpa.run(twoSource)(
        twoDesign,
        frame,
        twoSource.estimate(model, ObservationDistance.Euclidean),
        strategy
      )
    ).values.head.outcome match
      case MeasurementOutcome.Success(value, _) => value
      case other                                => fail(s"expected typed undefined scores, obtained $other")

    assertEquals(result.scores.toVector, Vector(None, None))
    assertEquals(result.meanScore, None)

  test("item assignments outside the declared effect axis fail at source binding"):
    val unknown = AxisKey.unsafe("not-an-item")
    val result = SamplewiseRsaSource(
      observations,
      ScientificAxisName.unsafe("items"),
      items,
      right(Column(samples, itemAssignments.updated(2, unknown))),
      ScientificAxisName.unsafe("runs"),
      blocks,
      right(Column(samples, blockAssignments))
    )

    assert(result.left.exists:
      case SamplewiseRsaSourceError.UnknownItem(sample, item) =>
        sample == samples.keys(2) && item == unknown.value
      case _ => false)
