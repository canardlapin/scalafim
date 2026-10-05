package scalafim.atlas

import locus4s.data.VectorField
import scalafim.atlas.fixtures.ParcelMetricInteropFixture
import scalafim.image.SampleSpaces

private object ParcelMetricInteropData:
  val bits: Vector[String] = Vector("0000000000000000", "8000000000000000", "7ff0000000000000", "fff0000000000000",
    "0000000000000001", "7fefffffffffffff", "7ff8000000000000", "bff4000000000000")
  def atlas(): VolumeAtlas =
    val ids = Vector(7, 19, 42, 63, 88, 101, 209, 360)
    VolumeAtlas.fromLabelVolume(AtlasRef.volume("interop", "Float64", SpaceId.Custom, SpaceId.Custom, source = Some("synthetic")),
      RegionIndex(ids.map(id => AtlasRegionMetadata.fromStrings(RegionId(id), s"Parcel $id"))),
      AtlasTestImages.labelVolume(SampleSpaces(Vector(8, 1, 1)), ids.toArray))

class ParcelMetricInteropProducerSuite extends munit.FunSuite:
  test("produce the platform metric document"):
    val r = ParcelMetricInteropData.atlas().realization
    val values = VectorField.tabulate(r.parcelDomain)(p => ParcelMetricJson.Float64.decode(ParcelMetricInteropData.bits(p.ordinal), p.ordinal).toOption.get)
    val schema = ParcelMetricSchema.from("β metric", Some("%"), Map("description" -> "café")).toOption.get
    val document = ParcelMetricJson.encode(r)(values, schema).fold(e => fail(e.message), identity)
    println(s"ATLAS_METRIC_DOCUMENT=$document")
    assertEquals(ParcelMetricJson.decode(r)(document).toOption.get.values.toVector.map(ParcelMetricJson.Float64.encode), ParcelMetricInteropData.bits)

class ParcelMetricInteropSuite extends munit.FunSuite:
  test("actual JVM and Scala.js documents decode on either platform"):
    assertEquals(ParcelMetricInteropFixture.documents.length, 2)
    val r = ParcelMetricInteropData.atlas().realization
    ParcelMetricInteropFixture.documents.foreach: document =>
      val result = ParcelMetricJson.decode(r)(document).fold(e => fail(e.message), identity)
      assertEquals(result.values.toVector.map(ParcelMetricJson.Float64.encode), ParcelMetricInteropData.bits)
      assertEquals(result.schema.name, "β metric")
      assertEquals(result.schema.attributes.toMap("description"), "café")
