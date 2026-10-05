package scalafim.atlas.io

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import locus4s.data.{Field, VectorField}
import scalafim.atlas.*
import scalafim.image.SampleSpaces

class ParcelMetricFilesSuite extends munit.FunSuite:
  test("UTF-8 file round trip retains target ownership and refuses replacement"):
    val directory = Files.createTempDirectory("scalafim-parcel-metric-")
    val path = directory.resolve("metric.json")
    val resavedPath = directory.resolve("restored.json")
    try
      val atlas = VolumeAtlas.fromLabelVolume(AtlasRef.volume("files", "Toy", SpaceId.Custom, SpaceId.Custom),
        RegionIndex(Vector(AtlasRegionMetadata.fromStrings(RegionId(7), "Parcel"))),
        scalafim.atlas.AtlasTestImages.labelVolume(SampleSpaces(Vector(1, 1, 1)), Array(7)))
      val r = atlas.realization
      val values = VectorField.tabulate(r.parcelDomain)(_ => -1.25)
      val schema = right(ParcelMetricSchema.from("β response", Some("%"), Map("description" -> "café")))
      right(ParcelMetricFiles.write(r)(path, values, schema))
      val original = Files.readString(path, StandardCharsets.UTF_8)
      val restored = right(ParcelMetricFiles.read(r)(path))
      val owned: Field[r.P, Double] = restored.values
      assert(owned.space.sameRuntimeOwnerAs(r.parcelDomain))
      assertEqualsDouble(owned.toVector.head, -1.25, 0.0)
      assertEquals(restored.schema.name, "β response")
      assertEquals(restored.schema.attributes.toMap("description"), "café")
      right(ParcelMetricFiles.write(r)(resavedPath, restored))
      assertEquals(right(ParcelMetricFiles.read(r)(resavedPath)).origin.provenance, restored.origin.provenance)
      assert(ParcelMetricFiles.write(r)(path, VectorField.tabulate(r.parcelDomain)(_ => 99.0), schema).isLeft)
      assertEquals(Files.readString(path, StandardCharsets.UTF_8), original)
      assert(ParcelMetricFiles.read(r)(directory.resolve("missing.json")).isLeft)
      Files.writeString(path, "broken", StandardCharsets.UTF_8)
      assert(ParcelMetricFiles.read(r)(path).swap.toOption.get.isInstanceOf[ParcelMetricFileError.Codec])
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(resavedPath)
      Files.deleteIfExists(directory)

  private def right[A](result: Either[?, A]): A = result.fold(e => fail(e.toString), identity)
