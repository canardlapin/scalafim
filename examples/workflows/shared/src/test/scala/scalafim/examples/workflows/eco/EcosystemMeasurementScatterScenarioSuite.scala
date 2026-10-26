package scalafim.examples.workflows.eco

import gale.linalg.DMat
import locus4s.{Bijection, CenteredNeighborhoodSystem, DomainRecord, DomainRegistry, PartialSurjection, Relation, Selection, TotalMap}
import locus4s.data.VectorField
import locus4s.provenance.{Provenance, ProvenanceId, SourceId}
import multivar.core.{SemanticSpace, SpaceRole, ValueId, ValueIdentity}
import mvpa4s.{AxisRef, EvidenceSource, Observations}
import mvpa4s.atlas.AtlasMeasurementFrames
import mvpa4s.locus.*
import mvpa4s.measurement.*
import neuroatlas4s.*
import scalafim.scenarios.ScenarioHarness

/** Synthetic parcel and surface measurements retain their supports through scatter. */
class EcosystemMeasurementScatterScenarioSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  test("atlas and centered surface measurements scatter their measured values"):
    val source = right(AxisRef.fromStableKeys("permuted-samples", SpaceRole.Observed, Vector("v2", "v0", "v1"), "sample-order", "BOLD", "raw"))
    val ambient = right(DomainRegistry.empty.restore(right(DomainRecord.parse("scenario:atlas:samples", "atlas samples", 3))))
    val parcels = right(ambient.registry.restore(right(DomainRecord.parse("scenario:atlas:parcels", "atlas parcels", 2))))
    val assignment = right(PartialSurjection.fromOptionalTargetOrdinals(ambient.space, parcels.space, Vector(Some(0), None, Some(1))))
    val keys = right(VectorField.fromValues(parcels.space, Vector("A", "B")))
    val regions = RegionIndex(Vector(AtlasRegionMetadata(RegionId(1), "left"), AtlasRegionMetadata(RegionId(2), "right")))
    val metadataField = right(VectorField.fromValues(parcels.space, regions.regions))
    val order = right(Selection.fromOrdinals(parcels.space, Vector(1, 0)))
    val atlasRef = AtlasRef.volume("toy", "Scenario", SpaceId.Custom, SpaceId.MNI152)
    val atlas = new AtlasRealization:
      type X = ambient.S
      type P = parcels.S
      val registry = parcels.registry
      val ref = atlasRef
      val provenance = AtlasProvenance.fromRef(atlasRef, regions)
      val parcelAssignment = assignment
      val parcelKeys = keys
      val metadata = metadataField
      val displayOrder = order
      val networkAssignment = None
    val mapping = right(Bijection.fromTotalMap(right(TotalMap.fromTargetOrdinals(ambient.space, source.locus, Vector(1, 2, 0)))))
    val declaration = MeasurementFrameDeclaration("measurement-scatter-scenario", "v1", Vector.empty)
    val atlasFrame = right(AtlasMeasurementFrames.parcels(source, declaration)(atlas)(mapping))
    val subjects = right(AxisRef.fromStableKeys("subjects", SpaceRole.Samples, Vector("s1"), "subject-order", "BOLD", "raw"))
    val id = SourceId.unsafe("scenario-evidence")
    val evidence = right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe("scenario-root"), id)))
    val observations = right(Observations.fromDense(subjects, source, DMat.dense(1, 3, Vector(30.0, 10.0, 20.0)), ValueIdentity.source(ValueId.unsafe("scenario-values")), evidence))
    val visitor = new MeasurementVisitor[subjects.Id, source.Id, String, SpatialMeasurementRendition[source.Locus], Double]:
      def visit[L <: SemanticSpace](entry: PackedMeasurementEntry[source.Id, String, SpatialMeasurementRendition[source.Locus]] { type Local = L }, measured: MeasuredObservations[subjects.Id, L]): Either[MeasurementFailure, Double] =
        Right(right(measured.patterns(DMat.eye(measured.localAxis.size)))(0, 0))

    val atlasResult = atlasFrame.frame.traverse(1)(Right(MeasurementResource(observations)(())))(visitor)
    val atlasOutcomes = atlasResult.value.map(outcome =>
      SpatialLocalOutcome(outcome.descriptor.id, outcome.rendition.support, 1.0, outcome.value.left.map(_.toString)))
    val atlasScatter = right(SpatialMeasurementScatter.scatter(source.locus, atlasOutcomes, SpatialScatterAlgebra.weightedMeanDouble))
    val atlasAmbient = atlasScatter.pullback(mapping.toTotalMap)

    val centers = right(Selection.fromOrdinals(source.locus, Vector(1, 0)))
    val rows = right(Relation.fromOrdinalRows(centers.positions, source.locus, Vector(Vector(1), Vector(0, 2))))
    val neighborhoods = right(CenteredNeighborhoodSystem.fromSelection(centers, rows))
    val surfaceFrame = right(SpatialMeasurementFrames.surfacePatches(source, declaration, neighborhoods))
    val surfaceResult = surfaceFrame.traverse(1)(Right(MeasurementResource(observations)(())))(visitor)
    val surfaceOutcomes = surfaceResult.value.map(outcome =>
      SpatialLocalOutcome(outcome.descriptor.id, outcome.rendition.support, 1.0, outcome.value.left.map(_.toString)))
    val surfaceScatter = right(SpatialMeasurementScatter.scatter(source.locus, surfaceOutcomes, SpatialScatterAlgebra.weightedMeanDouble))
    val surfaceAmbient = surfaceScatter.pullback(mapping.toTotalMap)

    def cell(value: SpatialScatterCell[Double]): Option[Double] = value match
      case SpatialScatterCell.Unvisited => None
      case SpatialScatterCell.Aggregated(number, _, _, failures) if failures.isEmpty => Some(number)
      case other => fail(s"unexpected scatter cell: $other")
    def close(actual: Double, expected: Double): Boolean =
      actual.isFinite && math.abs(actual - expected) <= 1e-12
    def cellsMatch(actual: Vector[Option[Double]], expected: Vector[Option[Double]]): Boolean =
      actual.size == expected.size && actual.zip(expected).forall:
        case (Some(a), Some(e)) => close(a, e)
        case (None, None) => true
        case _ => false
    def measuredMatch(actual: Vector[Either[MeasurementFailure, Double]], expected: Vector[Double]): Boolean =
      actual.size == expected.size && actual.zip(expected).forall((value, target) => value.exists(close(_, target)))
    val atlasCells = atlasAmbient.toVector.map(cell)
    val surfaceCells = surfaceAmbient.toVector.map(cell)
    val result = ScenarioHarness.result("ecosystem-measurement-scatter", Vector(
      ScenarioHarness.fact("atlas-measurements", atlasResult.error.isEmpty && measuredMatch(atlasResult.value.map(_.value), Vector(10.0, 30.0)), atlasResult.value.toString),
      ScenarioHarness.fact("atlas-ambient-scatter", cellsMatch(atlasCells, Vector(Some(10.0), None, Some(30.0))) && atlasAmbient.space.sameRuntimeOwnerAs(ambient.space), atlasCells.toString),
      ScenarioHarness.fact("surface-measurements", surfaceResult.error.isEmpty && measuredMatch(surfaceResult.value.map(_.value), Vector(30.0, 10.0)), surfaceResult.value.toString),
      ScenarioHarness.fact("surface-ambient-scatter", cellsMatch(surfaceCells, Vector(Some(10.0), Some(30.0), Some(30.0))) && surfaceAmbient.space.sameRuntimeOwnerAs(ambient.space), surfaceCells.toString)))
    println(result.render)
    assert(result.ciPass, result.render)
