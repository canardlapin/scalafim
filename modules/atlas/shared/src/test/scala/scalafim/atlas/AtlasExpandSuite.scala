package scalafim.atlas

import locus4s.data.Field
import scalafim.atlas.fixtures.ParcelExpansionFixture
import scalafim.atlas.syntax.*
import scalafim.image.*

class AtlasExpandSuite extends munit.FunSuite:
  private def atlas(): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(AtlasRef.volume("expansion", "Oracle", SpaceId.Custom, SpaceId.Custom),
      RegionIndex(Vector(7, 19, 42).map(id => AtlasRegionMetadata.fromStrings(RegionId(id), s"Parcel $id"))),
      AtlasTestImages.labelVolume(SampleSpaces(Vector(2, 2, 2)), ParcelExpansionFixture.labels.toArray))

  test("coordinate-keyed neuroatlas oracle covers reordered, partial and background values"):
    val a = atlas()
    val r: a.realization.type = a.realization
    ParcelExpansionFixture.cases.foreach: fixture =>
      val missing = if fixture.partial then MissingParcelPolicy.Fill(Double.NaN) else MissingParcelPolicy.Reject
      val values: Field[r.P, Double] = right(ParcelFields.fromSourceIds(r)(r,
        fixture.ids.map(RegionId.apply).zip(fixture.values), missing = missing))
      val expanded: ScalarVolume[r.parcellation.sampleSpace.type, Double] = right(a.expand(values, fixture.background))
      val sampled = NeuroVolume.sampled(expanded)
      assert(sampled.sampleSpace.grid.eq(r.domain.grid))
      assert(sampled.sampleSpace.eq(r.parcellation.sampleSpace))
      sampled.data.iterator.toVector.zip(fixture.expected).foreach: (actual, expected) =>
        if expected.isNaN then assert(actual.isNaN) else assertEqualsDouble(actual, expected, 0.0)
      assertEquals(a.labelVolume.data.iterator.toVector, ParcelExpansionFixture.labels)

  test("expansion rejects foreign parcel owners at compilation"):
    val errors = compileErrors("""
      import scalafim.atlas.*
      import locus4s.data.Field
      def invalid(a: VolumeAtlas, b: VolumeAtlas)(values: Field[b.realization.P, Double]) =
        AtlasExpand.volume(a)(values, 0.0)
    """)
    assert(errors.nonEmpty)

  private def right[A](result: Either[?, A]): A = result.fold(e => fail(e.toString), identity)
