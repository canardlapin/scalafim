package scalafim.fmri.mvpa

import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.RelationalAnalysis.given

final class RelationalFrameExecutionSuite extends munit.FunSuite:
  private enum Rendition:
    case Region
    case Global

  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val effects = right(
    AxisRef.create(
      AxisId.unsafe("frame-relation-effects"),
      AxisPurpose.Effects,
      Vector(AxisKey.unsafe("a"), AxisKey.unsafe("b"), AxisKey.unsafe("c")),
      CoordinateBasis.unsafe("condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-frame-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("frame-relation-neural"),
      AxisPurpose.NeuralFeatures,
      Vector(
        FeatureId.unsafe("voxel-x"),
        FeatureId.unsafe("voxel-y"),
        FeatureId.unsafe("voxel-z")
      ),
      CoordinateBasis.unsafe("voxel-order"),
      Some(AxisUnits.unsafe("percent-signal-change")),
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-frame-suite", "v1")
    )
  )

  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("frame-relation-runs"),
      AxisPurpose.Partitions,
      Vector(PartitionId.unsafe("run-1"), PartitionId.unsafe("run-2")),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-frame-suite", "v1")
    )
  )

  private val partitionAxis = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )

  private def pairing = right(
    PairingDesign.allOrdered(
      partitionAxis,
      PairingReducer.WeightedMean,
      GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity),
      right(
        PartitionIndependenceTestSupport.declareAllPairs(
          source.identity,
          partitionAxis,
          "frame-independent-runs"
        )
      )
    )
  )

  private val training = right(
    AxisRef.create(
      AxisId.unsafe("frame-relation-training"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("time-1"), SampleId.unsafe("time-2")),
      CoordinateBasis.unsafe("time-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-frame-suite", "v1")
    )
  )

  private val relationDesign = right(
    DesignIdentity(DesignKind.unsafe("frame-relation-fit"))
  )

  private val source =
    val estimates = Vector(
      GaleTestMatrix.fromRows(
        Seq(
          Seq(0.0, 0.0, 0.0),
          Seq(1.0, 0.0, 2.0),
          Seq(0.0, 2.0, 1.0)
        )
      ),
      GaleTestMatrix.fromRows(
        Seq(
          Seq(0.1, -0.1, 0.2),
          Seq(1.1, 0.1, 1.8),
          Seq(-0.1, 2.1, 1.2)
        )
      )
    )
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate = right(
        EvidenceTable.dense(
          effects,
          neural,
          estimates(position),
          ValueId.unsafe(s"frame-relation-estimate-${partition.value}")
        )
      )
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"frame-relation-source-${partition.value}")),
          relationDesign,
          right(Estimability(effects, Vector(true, true, true))),
          NormalizationIdentity.none,
          training
        )
      )
      PartitionRelation(
        partition,
        right(Relation(estimate, receipt, right(EstimateOnlyCapabilities(neural))))
      )
    right(PartitionedRelations(partitionAxis, effects, neural, entries))

  private def selection(id: String, ordinals: Int*) =
    val injection = right(
      Injection.from(
        IArray.unsafeFromArray(ordinals.toArray),
        right(IndexSpace.of(neural.size))
      )
    )
    right(Measurement.hardSelection(neural, MeasurementId.unsafe(id), injection))

  private def strategy(materialization: MaterializationPolicy) =
    right(
      ExecutionStrategy(
        BackendId.unsafe("relational-portable"),
        ExecutionRepresentation.SufficientStatistics,
        NumericPrecision.Binary64,
        SolverChoice.NotApplicable,
        Vector.empty,
        Scheduling.serial,
        materialization,
        FallbackPolicy.forbidden,
        ResultDelivery.Collected
      )
    )

  private def success[R](
      value: MeasurementValue[
        MeasuredRelationalRdm[partitions.Id, effects.Id, AxisKey],
        RelationalBindRejection,
        RelationalTaskFailure,
        R
      ]
  ): MeasuredRelationalRdm[partitions.Id, effects.Id, AxisKey] =
    value.outcome match
      case MeasurementOutcome.Success(result, _) => result
      case other                                 => fail(s"expected success, obtained $other")

  test("region and global identity measurements use one relational execution route"):
    val region = selection("a-region-reordered", 2, 0)
    val global = right(Measurement.identity(neural, MeasurementId.unsafe("z-global")))
    val frame = right(
      MeasurementFrame(neural)(
        Vector(
          MeasurementEntry(region, Rendition.Region),
          MeasurementEntry(global, Rendition.Global)
        )
      )
    )
    val result = right(
      Mvpa.run(source)(
        pairing,
        frame,
        RelationalAnalysis.identityRdm(source, RdmNormalization.Raw),
        strategy(MaterializationPolicy.Reject)
      )
    )

    assertEquals(result.counts.succeeded, 2)
    assertEquals(
      result.values.map(_.measurement.id.value),
      Vector("a-region-reordered", "z-global")
    )
    assertEquals(result.values.map(_.rendition), Vector(Rendition.Region, Rendition.Global))
    val regionResult = success(result.values.head)
    val globalResult = success(result.values(1))
    assertEquals(region.local.keys, Vector(neural.keys(2), neural.keys(0)))
    assertEquals(regionResult.measurement, region.identity)
    assertEquals(globalResult.measurement, global.identity)
    assertEquals(regionResult.measurement.kind, MeasurementKind.HardSelection)
    assertEquals(globalResult.measurement.kind, MeasurementKind.Identity)
    assertEquals(regionResult.domain.items.identity, effects.identity)
    assert(regionResult.domain.items.evidence eq effects.evidence)
    assertEquals(regionResult.distances.size, 3)
    assertEquals(globalResult.distances.size, 3)
    assertEquals(regionResult.computation.measurement, region.identity)
    assertEquals(globalResult.computation.measurement, global.identity)

  test("relational frame traversal retains typed failures beside successes"):
    val region = selection("a-region", 0)
    val global = right(Measurement.identity(neural, MeasurementId.unsafe("z-global")))
    val frame = right(
      MeasurementFrame(neural)(
        Vector(
          MeasurementEntry(region, Rendition.Region),
          MeasurementEntry(global, Rendition.Global)
        )
      )
    )
    val result = right(
      Mvpa.run(source)(
        pairing,
        frame,
        RelationalAnalysis.identityRdm(source, RdmNormalization.Raw),
        strategy(MaterializationPolicy.Allow(MaterializationBudget.unsafe(3L)))
      )
    )

    assertEquals(result.counts.succeeded, 1)
    assertEquals(result.counts.failed, 1)
    assertEquals(success(result.values.head).measurement, region.identity)
    result.values(1).outcome match
      case MeasurementOutcome.Failed(
            RelationalTaskFailure.Fit(
              RelationalFitError.Compiler(
                RelationalCompilationError.Evidence(
                  EvidenceTableError.MaterializationBudgetExceeded(required, budget)
                )
              )
            ),
            receipt
          ) =>
        assertEquals(required, 9L)
        assertEquals(budget.maxElements, 3L)
        assertEquals(receipt.materializedCells, 0L)
      case other => fail(s"expected local relational failure, obtained $other")

  test("rank RSA uses the model's exact pair witness across the same frame"):
    val domain = right(WithinPairDomain(effects))
    val model = right(
      SecondOrderModel.signal(
        domain,
        SecondOrderModelName.unsafe("graded-geometry"),
        Vector(1.0, 2.0, 4.0)
      )
    )
    val region = selection("rank-region", 2, 0)
    val frame = right(
      MeasurementFrame(neural)(Vector(MeasurementEntry(region, NoRendition)))
    )
    val result = right(
      Mvpa.run(source)(
        pairing,
        frame,
        RelationalAnalysis.rankRsa(source, model, RdmNormalization.Raw),
        strategy(MaterializationPolicy.Reject)
      )
    )

    result.values.head.outcome match
      case MeasurementOutcome.Success(value, _) =>
        assertEquals(value.measurement, region.identity)
        assertEquals(value.estimate.model, model.identity)
        assert(value.estimate.correlation.isFinite)
        assertEquals(value.computation.measurement, region.identity)
      case other => fail(s"expected rank RSA success, obtained $other")
