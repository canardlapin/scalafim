package scalafim.spatial

import neuroim4s.spaces.SubjectId

import neuroim4s.image.{SampleSpaces}
import neuroim4s.image.SampleSpaces.*
import locus4s.{Region, Relation, Selection, TotalMap}
import neurosurf4s.surface.{
  Hemisphere,
  SurfaceField,
  SurfaceGeometry,
  SurfaceKind,
  SurfaceRoi,
  TriangleMesh,
  VertexId
}

class DomainLocusSuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def volumeDomain(name: String, size: Int = 4): Domain =
    val id = value(DomainId(name))
    val subject = value(SubjectId("sub-01").asSpatial)
    val modality = value(Modality("bold"))
    val geometry =
      value(
        SamplingGeometry.volume(
          SampleSpaces(Vector(size, 1, 1), affine = Some(ProviderAffines.identity))
        )
      )
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  test("domain locus lowers unordered regions and ordered selections distinctly"):
    val domain = volumeDomain("native")
    val locus = domain.locus
    val region =
      Region
        .fromOrdinals(locus.space, Vector(3, 1, 3))
        .fold(error => fail(error.message), identity)
    val selection =
      Selection
        .fromOrdinals(locus.space, Vector(3, 1))
        .fold(error => fail(error.message), identity)

    assertEquals(
      locus.regionDemand(region).map(_.spatial),
      Right(SpatialDemand.Roi(Vector(1, 3)))
    )
    assertEquals(
      locus.selectionDemand(selection).map(_.spatial),
      Right(SpatialDemand.Rows(Vector(3, 1)))
    )

  test("same-sized semantic domains retain distinct unforgeable locus owners"):
    val locus = volumeDomain("native").locus
    val foreign = volumeDomain("foreign-same-shape").locus
    val nativeRegion =
      Region
        .fromOrdinals(locus.space, Vector(1, 3))
        .fold(error => fail(error.message), identity)
    val foreignSelection =
      Selection
        .fromOrdinals(foreign.space, Vector(3, 1))
        .fold(error => fail(error.message), identity)

    assert(locus.space.align(foreign.space).isLeft)
    assertEquals(
      locus.regionDemand(nativeRegion).map(_.spatial),
      Right(SpatialDemand.Roi(Vector(1, 3)))
    )
    assertEquals(
      foreign.selectionDemand(foreignSelection).map(_.spatial),
      Right(SpatialDemand.Rows(Vector(3, 1)))
    )

  test("exact maps, crisp relations, and sampled operators remain distinct contracts"):
    val domain = volumeDomain("native")
    val other = volumeDomain("other")
    val locus = domain.locus
    val exact = TotalMap.identity(locus.space)
    val crisp =
      Relation
        .fromOrdinalRows(
          locus.space,
          locus.space,
          Iterator.tabulate(locus.space.size)(ordinal => Iterator.single(ordinal))
        )
        .toOption
        .get

    assertEquals(exact.targetOrdinals.toVector, Vector(0, 1, 2, 3))
    assertEquals(
      crisp.ordinalRows.map(_.toVector).toVector,
      Vector(Vector(0), Vector(1), Vector(2), Vector(3))
    )
    assert(exact.from.align(other.locus.space).isLeft)
    assert(crisp.from.align(other.locus.space).isLeft)
    assert(!classOf[SpatialOperator].isAssignableFrom(exact.getClass))
    assert(!classOf[SpatialOperator].isAssignableFrom(crisp.getClass))

  test("surface mask compatibility requires exact ordered topology"):
    val vertices =
      Vector(
        Vector(0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(1.0, 1.0, 0.0)
      )
    val sampled =
      SurfaceGeometry(
        TriangleMesh.fromRows(vertices, Vector((0, 1, 2), (1, 3, 2))),
        Hemisphere.Left,
        SurfaceKind.Midthickness
      )
    val reordered =
      SurfaceGeometry(
        TriangleMesh.fromRows(vertices, Vector((0, 1, 3), (0, 3, 2))),
        Hemisphere.Left,
        SurfaceKind.Midthickness
      )
    val field = SurfaceField.full(reordered, Vector(true, true, true, true))
    val mask =
      SurfaceRoi.fromField(
        field,
        Vector(VertexId(0), VertexId(1), VertexId(2), VertexId(3))
      )

    assertEquals(
      SamplingGeometry.surface(sampled, Some(mask)).left.toOption,
      Some(SpatialError.MaskSpaceMismatch("surface"))
    )
