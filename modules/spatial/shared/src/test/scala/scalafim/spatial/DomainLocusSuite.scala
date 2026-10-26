package scalafim.spatial

import neurosurf4s.routing.*

import neuroim4s.spaces.SubjectId

import neuroim4s.image.{SampleSpaces}
import neuroim4s.image.SampleSpaces.*
import locus4s.{Region, Selection}

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
