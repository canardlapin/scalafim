package scalafim.fmri.mvpa

import gale.linalg.DMat
import multivar.core.ValueId
import multivar.core.ValueIdentity
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.RelationalAnalysis.given
import scalafim.image.*
import scalafim.locus.Selection
import scalafim.locus.SpaceKey

final class RelationalSpatialExecutionSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val volumeSpace =
    VolumeSpace(
      NeuroSpace(
        dims = Vector(3, 1, 1),
        spacing = Some(Vector(1.0, 1.0, 1.0)),
        origin = Some(Vector(0.0, 0.0, 0.0))
      )
    )
  private val packed =
    VolumeDomain.semantic(
      SpaceKey.unsafe("relational-searchlight-volume"),
      volumeSpace
    )
  private type Voxel = packed.S
  private val domain: VolumeDomain[Voxel] = packed.value
  private val neural: IdentifiedLocusAxis[Voxel] =
    right(SpatialAxes.volume(domain, Some(AxisUnits.unsafe("percent-signal-change"))))

  private val effects = right(
    AxisRef.create(
      AxisId.unsafe("relational-searchlight-effects"),
      AxisPurpose.Effects,
      Vector(AxisKey.unsafe("a"), AxisKey.unsafe("b"), AxisKey.unsafe("c")),
      CoordinateBasis.unsafe("condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-searchlight-fixture", "v1")
    )
  )

  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("relational-searchlight-runs"),
      AxisPurpose.Partitions,
      Vector(PartitionId.unsafe("run-1"), PartitionId.unsafe("run-2")),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-searchlight-fixture", "v1")
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
          relations.identity,
          partitionAxis,
          "relational-searchlight-independent-runs"
        )
      )
    )
  )

  private val training = right(
    AxisRef.create(
      AxisId.unsafe("relational-searchlight-training"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("time-1"), SampleId.unsafe("time-2")),
      CoordinateBasis.unsafe("time-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-searchlight-fixture", "v1")
    )
  )
  private val fitDesign = right(
    DesignIdentity(DesignKind.unsafe("relational-searchlight-fit"))
  )

  private val relations =
    val estimates = Vector(
      DMat.dense(
        3,
        3,
        Vector(
          0.0, 0.0, 0.0, 1.0, 2.0, 3.0, 3.0, 1.0, 2.0
        )
      ),
      DMat.dense(
        3,
        3,
        Vector(
          0.1, -0.1, 0.2, 1.2, 1.8, 3.1, 2.8, 1.1, 1.9
        )
      )
    )
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate = right(
        EvidenceTable.dense(
          effects,
          neural.features,
          estimates(position),
          ValueId.unsafe(s"relational-searchlight-estimate-${partition.value}")
        )
      )
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"relational-searchlight-source-${partition.value}")),
          fitDesign,
          right(Estimability(effects, Vector(true, true, true))),
          NormalizationIdentity.none,
          training
        )
      )
      PartitionRelation(
        partition,
        right(
          Relation(
            estimate,
            receipt,
            right(EstimateOnlyCapabilities(neural.features))
          )
        )
      )
    right(PartitionedRelations(partitionAxis, effects, neural.features, entries))

  private val centers = right(
    Selection.fromOrdinals(domain.finiteSpace, Vector(2, 0))
  )
  private val frame = right(
    VolumeFrames.metricSearchlights(
      neural,
      domain,
      SearchlightRadius.make(0.0).toOption.get,
      centers
    )
  )
  private val strategy = right(
    ExecutionStrategy(
      BackendId.unsafe("relational-portable"),
      ExecutionRepresentation.SufficientStatistics,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Reject,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )

  test("searchlight relational results scatter solely from typed rendition metadata"):
    val result = right(
      Mvpa.run(relations)(
        pairing,
        frame,
        RelationalAnalysis.identityRdm(relations, RdmNormalization.Raw),
        strategy
      )
    )

    assertEquals(
      result.values.map(_.measurement.id.value),
      Vector("searchlight-0", "searchlight-2")
    )
    assertEquals(result.values.map(_.rendition.center.ordinal), Vector(1, 0))
    result.values.foreach: value =>
      value.outcome match
        case MeasurementOutcome.Success(rdm, _) =>
          assertEquals(rdm.measurement, value.measurement)
          assertEquals(rdm.computation.measurement, value.measurement)
          assertEquals(rdm.distances.size, 3)
        case other => fail(s"expected successful relational searchlight, obtained $other")

    val scattered = right(
      SpatialResultScatter.searchlights(centers.positions, result)
    )
    val first = centers.positions.index(0).toOption.get
    val second = centers.positions.index(1).toOption.get
    assertEquals(scattered(first).receipt.measurement.id.value, "searchlight-2")
    assertEquals(scattered(second).receipt.measurement.id.value, "searchlight-0")
    scattered.toVector.foreach:
      case MeasurementOutcome.Success(value, _) =>
        assertEquals(value.domain.items.identity, effects.identity)
      case other => fail(s"expected successful scattered RDM, obtained $other")
