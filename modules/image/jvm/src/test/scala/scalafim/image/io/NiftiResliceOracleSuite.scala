package scalafim.image.io

import scalafim.image.*

import java.nio.file.Paths
import scala.io.Source

class NiftiResliceOracleSuite extends munit.FunSuite:

  private val WorldTolerance = 2e-6
  private val LinearTolerance = 2e-5

  private final case class ExpectedPixel(
    plane: AnatomicalPlane,
    convention: LeftRightConvention,
    dimensions: SliceDimensions,
    pixel: PixelCoord,
    world: WorldPoint,
    nearest: Double,
    linear: Double
  )

  private final case class Fixture(
    generator: String,
    axisCodes: Vector[String],
    cursor: WorldPoint,
    spacing: PixelSpacing,
    pixels: Vector[ExpectedPixel]
  )

  private def resourcePath(name: String) =
    val resource = Option(getClass.getResource(s"/scalafim/image/io/$name"))
      .getOrElse(fail(s"missing test resource $name"))
    Paths.get(resource.toURI)

  private def fixture: Fixture =
    val source = Source.fromResource("scalafim/image/io/nibabel-oblique-slices.tsv")
    val lines =
      try source.getLines().toVector
      finally source.close()

    def metadata(name: String): Vector[String] =
      lines.find(_.startsWith(s"# $name\t"))
        .getOrElse(fail(s"missing $name fixture metadata"))
        .split("\t", -1)
        .drop(1)
        .toVector

    val cursorFields = metadata("cursor_world").map(_.toDouble)
    val spacingFields = metadata("pixel_spacing").map(_.toDouble)
    val records = lines.filterNot(_.startsWith("#")).drop(1).map { line =>
      val fields = line.split("\t", -1)
      ExpectedPixel(
        AnatomicalPlane.valueOf(fields(0)),
        LeftRightConvention.valueOf(fields(1)),
        SliceDimensions(fields(2).toInt, fields(3).toInt),
        PixelCoord(fields(4).toInt, fields(5).toInt),
        WorldPoint(fields(6).toDouble, fields(7).toDouble, fields(8).toDouble),
        fields(9).toDouble,
        fields(10).toDouble
      )
    }
    Fixture(
      metadata("generator").head,
      metadata("source_axis_codes"),
      WorldPoint(cursorFields(0), cursorFields(1), cursorFields(2)),
      PixelSpacing(spacingFields(0), spacingFields(1)),
      records
    )

  private def assertWorld(actual: WorldPoint, expected: WorldPoint, clue: String): Unit =
    assertEqualsDouble(actual.x, expected.x, WorldTolerance, clue = clue)
    assertEqualsDouble(actual.y, expected.y, WorldTolerance, clue = clue)
    assertEqualsDouble(actual.z, expected.z, WorldTolerance, clue = clue)

  test("nibabel oblique fixture retains its independent spatial metadata") {
    val expected = fixture
    val path = resourcePath("nibabel-oblique.nii")
    val header = Nifti.readHeader(path)
    val volume = Nifti.readVol(path)

    assertEquals(expected.generator, "nibabel-5.2.1")
    assertEquals(expected.axisCodes, Vector("A", "L", "I"))
    assertEquals(header.qformCode, 1)
    assertEquals(header.sformCode, 2)
    assert(header.qform.nonEmpty)
    assert(header.sform.nonEmpty)
    assertEquals(header.preferredAffine, header.sform)
    assertEquals(volume.space.dims, Vector(4, 5, 6))
    assertEquals(volume.space, header.space)
  }

  test("file-to-reslice pixels match nibabel in every plane and display convention") {
    val expected = fixture
    val volume = Nifti.readVol(resourcePath("nibabel-oblique.nii"))
    val groups = expected.pixels.groupBy(pixel => pixel.plane -> pixel.convention)

    assertEquals(groups.size, AnatomicalPlane.values.length * LeftRightConvention.values.length)
    var interiorNearest = 0
    groups.foreach { case ((plane, convention), pixels) =>
      val dimensions = pixels.head.dimensions
      assertEquals(pixels.length, dimensions.pixelCount)
      val grid = SliceGrid.covering(
        volume.volumeSpace,
        SlicePlane.canonical(plane, expected.cursor, convention),
        expected.spacing
      )
      assertEquals(grid.dimensions, dimensions, clue = s"plane=$plane convention=$convention")
      val plan = SlicePlan.make(volume.volumeSpace, grid)
      val nearest = plan.sample(volume, SliceSampling.Nearest(-1.0)).toOption.get
      val linear = plan.sample(volume, SliceSampling.Linear(-1.0)).toOption.get

      pixels.foreach { pixel =>
        val clue = s"plane=$plane convention=$convention pixel=${pixel.pixel}"
        assertWorld(grid.worldAt(pixel.pixel).toOption.get, pixel.world, clue)
        assertEqualsDouble(nearest(pixel.pixel.column, pixel.pixel.row), pixel.nearest, 0.0, clue = clue)
        assertEqualsDouble(linear(pixel.pixel.column, pixel.pixel.row), pixel.linear, LinearTolerance, clue = clue)
        if pixel.nearest != -1.0 then interiorNearest += 1
      }
    }
    assert(interiorNearest >= 40, clue = s"fixture should contain many independently located landmarks; got $interiorNearest")
  }
