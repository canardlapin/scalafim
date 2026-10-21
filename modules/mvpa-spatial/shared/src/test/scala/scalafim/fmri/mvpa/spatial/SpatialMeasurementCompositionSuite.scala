package scalafim.fmri.mvpa.spatial

import mvpa4s.locus.*

import locus4s.{Relation, Region, Selection}
import gale.linalg.DMat
import multivar.core.{SemanticSpace, SpaceRole, ValueId, ValueIdentity}
import mvpa4s.{AxisRef, EvidenceSource, Observations}
import mvpa4s.measurement.{MeasuredObservations, MeasurementFailure, MeasurementFrameDeclaration, MeasurementId, MeasurementResource, MeasurementVisitor, PackedMeasurementEntry}
import neuroim4s.image.ExactVolumeSearchlight
import scalafim.locus.{CenteredSearchlight, Searchlight}
import locus4s.data.VectorField
import locus4s.provenance.{Provenance, ProvenanceId, SourceId}

class SpatialMeasurementCompositionSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(name: String): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector("a", "b", "c", "d"), "voxel-order", "psc", "raw"))

  private val declaration =
    MeasurementFrameDeclaration("spatial-fixture", "v1", Vector("radius" -> "1.5"))

  private def observations(samples: AxisRef[String], neural: AxisRef[String]): Observations[samples.Id, neural.Id] =
    val sourceId = SourceId.unsafe("spatial-frame-source")
    val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe("spatial-frame-root"), sourceId)))
    right(Observations.fromDense(samples, neural, DMat.dense(2, 4, Vector(1.0, 10.0, 100.0, 1000.0, 2.0, 20.0, 200.0, 2000.0)), ValueIdentity.source(ValueId.unsafe("spatial-frame-values")), source))

  test("volume neighborhood identities are stable and centers remain rendition metadata"):
    val neural = axis("spatial-neighborhoods")
    val centers = right(Region.fromOrdinals(neural.locus, Vector(2, 0)))
    val relation = right(
      Relation.fromOrdinalRows(
        neural.locus,
        neural.locus,
        Array(Array(0, 1), Array.emptyIntArray, Array(2, 3), Array.emptyIntArray).iterator.map(_.iterator)
      )
    )
    val neighborhoods = right(ExactVolumeSearchlight.fromRelation(centers, relation))
    val labels = right(VectorField.fromValues(neural.locus, Vector("left", "one", "right", "three")))
    val frame = right(SpatialMeasurementFrames.volumeNeighborhoods(neural, declaration, neighborhoods, point => Some(labels(point))))
    assertEquals(frame.identity.declaration.key, declaration.key)
    assertEquals(frame.identity.declaration.revision, declaration.revision)
    assertEquals(frame.identity.declaration.parameters.count(_._1 == "spatial-support"), 2)
    // Creation did not eagerly construct map legs: that work is owned by
    // MeasurementFrame traversal.
    assertEquals(frame.identity.source, neural.descriptor)

  test("sparse volume centers preserve ambient IDs, complete frame identity and measured values"):
    val neural = axis("volume-neighborhood-migration")
    val samples = right(AxisRef.fromStableKeys("volume-neighborhood-migration-samples", SpaceRole.Samples, Vector("s1", "s2"), "sample-order", "psc", "raw"))
    val centers = right(Region.fromOrdinals(neural.locus, Vector(3, 1)))
    val relation = right(Relation.fromOrdinalRows(neural.locus, neural.locus,
      Vector(Vector.empty, Vector(0, 1), Vector.empty, Vector(2, 3))))
    val neighborhoods = right(ExactVolumeSearchlight.fromRelation(centers, relation))
    val actual = right(SpatialMeasurementFrames.volumeNeighborhoods(
      neural, declaration, neighborhoods, index => Some(s"voxel-${index.ordinal}")))
    val ids = Vector(1, 3).map(ordinal => s"volume:${neural.descriptor.coordinateSignature.value}:$ordinal")
    val expected = right(SpatialMeasurementFrames.regions(neural, declaration,
      Vector(1, 3).zip(Vector(Vector(0, 1), Vector(2, 3))).zip(ids).map:
        case ((center, members), id) => SpatialMeasurementSite(
          MeasurementId.unsafe(id),
          SpatialMeasurementSupport.Regional(right(Region.fromOrdinals(neural.locus, members))),
          Some(neural.locus.indexAtValidatedOrdinal(center)))
    ))
    assertEquals(actual.identity, expected.identity)
    val reverseSelection = right(Selection.fromOrdinals(neural.locus, Vector(3, 1)))
    val compactRows = right(Relation.fromOrdinalRows(reverseSelection.positions, neural.locus,
      Vector(Vector(2, 3), Vector(0, 1))))
    val direct = right(locus4s.CenteredNeighborhoodSystem.fromSelection(reverseSelection, compactRows))
    val reordered = right(SpatialMeasurementFrames.volumeNeighborhoods(neural, declaration, direct))
    assertEquals(reordered.identity, expected.identity)
    val visitor = new MeasurementVisitor[samples.Id, neural.Id, String, SpatialMeasurementRendition[neural.Locus], DMat]:
      def visit[L <: SemanticSpace](entry: PackedMeasurementEntry[neural.Id, String, SpatialMeasurementRendition[neural.Locus]] { type Local = L }, measured: MeasuredObservations[samples.Id, L]): Either[MeasurementFailure, DMat] =
        Right(right(measured.patterns(DMat.eye(measured.localAxis.size))))
    val result = actual.traverse(1)(Right(MeasurementResource(observations(samples, neural))(())))(visitor)
    assertEquals(result.error, None)
    assertEquals(result.value.map(_.descriptor.id.value), ids)
    assertEquals(result.value.map(_.rendition.center.map(_.ordinal)), Vector(Some(1), Some(3)))
    assertEquals(result.value.map(_.rendition.label), Vector(Some("voxel-1"), Some("voxel-3")))
    val matrices = result.value.map(_.value.toOption.get)
    val expectedValues = Vector(Vector(Vector(1.0, 10.0), Vector(2.0, 20.0)), Vector(Vector(100.0, 1000.0), Vector(200.0, 2000.0)))
    for
      site <- matrices.indices
      row <- 0 until 2
      column <- 0 until 2
    do assertEqualsDouble(matrices(site)(row, column), expectedValues(site)(row)(column), 0.0)

  test("empty centered surface patches are an explicit frame error"):
    val neural = axis("spatial-empty-surface")
    val searchlight = right(Searchlight.make(Region.empty(neural.locus), Relation.empty(neural.locus, neural.locus)))
    val centered = right(CenteredSearchlight.validate(searchlight))
    val result = SurfaceMeasurementFrames.surfacePatches(neural, declaration, centered)
    assertEquals(result.left.toOption, Some(SpatialMeasurementFrameError.EmptyFrame))
