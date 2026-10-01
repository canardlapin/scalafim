package scalafim.fmri.mvpa.spatial

import scala.compiletime.testing.typeCheckErrors

import locus4s.{Relation, Region, Selection}
import gale.linalg.DMat
import multivar.core.{SemanticSpace, SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, Observations}
import scalafim.fmri.mvpa.measurement.{MeasuredObservations, MeasurementFailure, MeasurementFrameDeclaration, MeasurementId, MeasurementResource, MeasurementVisitor, PackedMeasurementEntry}
import scalafim.image.ExactVolumeSearchlight
import scalafim.locus.{CenteredSearchlight, IndexedField, Searchlight}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class SpatialMeasurementFramesSuite extends munit.FunSuite:
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

  test("identity frame is a neutral direct dense oracle"):
    val samples = right(AxisRef.fromStableKeys("spatial-identity-samples", SpaceRole.Samples, Vector("s1", "s2"), "sample-order", "psc", "raw"))
    val neural = axis("spatial-identity-neural")
    val frame = SpatialMeasurementFrames.identity(neural, declaration, MeasurementId.unsafe("identity"))
    val visitor = new MeasurementVisitor[samples.Id, neural.Id, String, SpatialMeasurementRendition[neural.Locus], DMat]:
      def visit[L <: SemanticSpace](entry: PackedMeasurementEntry[neural.Id, String, SpatialMeasurementRendition[neural.Locus]] { type Local = L }, measured: MeasuredObservations[samples.Id, L]): Either[MeasurementFailure, DMat] =
        Right(right(measured.patterns(DMat.eye(measured.localAxis.size))))
    val result = frame.traverse(1)(Right(MeasurementResource(observations(samples, neural))(())))(visitor)
    assertEquals(result.error, None)
    val actual = result.value.head.value.toOption.get
    val expected = Vector(Vector(1.0, 10.0, 100.0, 1000.0), Vector(2.0, 20.0, 200.0, 2000.0))
    var row = 0
    while row < expected.length do
      var column = 0
      while column < expected(row).length do
        assertEqualsDouble(actual(row, column), expected(row)(column), 1e-12)
        column += 1
      row += 1

  test("regional selections preserve explicit selection order while regions use domain order"):
    val neural = axis("spatial-source")
    val region = right(Region.fromOrdinals(neural.locus, Vector(3, 1)))
    val selection = right(Selection.fromOrdinals(neural.locus, Vector(3, 1)))
    val frame = right(
      SpatialMeasurementFrames.regions(
        neural,
        declaration,
        Vector(
          SpatialMeasurementSite(MeasurementId.unsafe("region"), SpatialMeasurementSupport.Regional(region)),
          SpatialMeasurementSite(MeasurementId.unsafe("selection"), SpatialMeasurementSupport.Selected(selection))
        )
      )
    )
    assertEquals(frame.identity.declaration.key, declaration.key)
    assertEquals(frame.identity.declaration.revision, declaration.revision)
    assert(frame.identity.declaration.parameters.contains("caller-declaration" -> declaration.fingerprint))
    assertEquals(frame.identity.declaration.parameters.count(_._1 == "spatial-support"), 2)

  test("foreign locus owners are rejected at the typed support boundary"):
    val errors = typeCheckErrors("""import locus4s.Region
import scalafim.fmri.mvpa.AxisRef
import scalafim.fmri.mvpa.measurement.{MeasurementFrameDeclaration, MeasurementId}
import scalafim.fmri.mvpa.spatial.{SpatialMeasurementFrames, SpatialMeasurementSite, SpatialMeasurementSupport}
def invalid[K, S, T](source: AxisRef[K] { type Locus = S }, declaration: MeasurementFrameDeclaration, support: Region[T]) =
  val site = SpatialMeasurementSite[T](MeasurementId.unsafe("foreign"), SpatialMeasurementSupport.Regional(support), None, None)
  SpatialMeasurementFrames.regions[K, S](source, declaration, Vector(site))
""")
    assertEquals(errors.length, 1)
    assert(errors.head.lineContent.contains("SpatialMeasurementFrames.regions"), errors.head.lineContent)
    assert(errors.head.message.contains("SpatialMeasurementSite"), errors.head.message)

  test("frame identity binds supports but excludes rendition metadata"):
    val neural = axis("spatial-identity-manifest")
    val left = right(Region.fromOrdinals(neural.locus, Vector(0, 1)))
    val rightSupport = right(Region.fromOrdinals(neural.locus, Vector(1, 2)))
    val id = MeasurementId.unsafe("site")
    val base = right(
      SpatialMeasurementFrames.regions(
        neural,
        declaration,
        Vector(SpatialMeasurementSite(id, SpatialMeasurementSupport.Regional(left)))
      )
    )
    val relabelled = right(
      SpatialMeasurementFrames.regions(
        neural,
        declaration,
        Vector(SpatialMeasurementSite(id, SpatialMeasurementSupport.Regional(left), Some(neural.locus.indexAtValidatedOrdinal(0)), Some("display only")))
      )
    )
    val changedSupport = right(
      SpatialMeasurementFrames.regions(
        neural,
        declaration,
        Vector(SpatialMeasurementSite(id, SpatialMeasurementSupport.Regional(rightSupport)))
      )
    )
    assertEquals(base.identity, relabelled.identity)
    assertNotEquals(base.identity, changedSupport.identity)

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
    val labels = right(IndexedField.fromValues(neural.locus, Vector("left", "one", "right", "three")))
    val frame = right(SpatialMeasurementFrames.volumeNeighborhoods(neural, declaration, neighborhoods, point => Some(labels(point))))
    assertEquals(frame.identity.declaration.key, declaration.key)
    assertEquals(frame.identity.declaration.revision, declaration.revision)
    assertEquals(frame.identity.declaration.parameters.count(_._1 == "spatial-support"), 2)
    // Creation did not eagerly construct map legs: that work is owned by
    // MeasurementFrame traversal.
    assertEquals(frame.identity.source, neural.descriptor)

  test("empty centered surface patches are an explicit frame error"):
    val neural = axis("spatial-empty-surface")
    val searchlight = right(Searchlight.make(Region.empty(neural.locus), Relation.empty(neural.locus, neural.locus)))
    val centered = right(CenteredSearchlight.validate(searchlight))
    val result = SpatialMeasurementFrames.surfacePatches(neural, declaration, centered)
    assertEquals(result.left.toOption, Some(SpatialMeasurementFrameError.EmptyFrame))

  test("identity adapter binds the output measurement id but excludes its label"):
    val neural = axis("identity-id")
    val one = SpatialMeasurementFrames.identity(neural, declaration, MeasurementId.unsafe("one"), Some("red"))
    val renamed = SpatialMeasurementFrames.identity(neural, declaration, MeasurementId.unsafe("one"), Some("blue"))
    val two = SpatialMeasurementFrames.identity(neural, declaration, MeasurementId.unsafe("two"), Some("red"))
    assertEquals(one.identity, renamed.identity)
    assertNotEquals(one.identity, two.identity)
