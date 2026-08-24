package scalafim.image.io

import scalafim.image.SampleSpaces.*

import image4s.ImageMetadata
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.nifti.NiftiDatatype
import image4s.nifti.NiftiTemporalUnit
import image4s.nifti.NiftiWriteOptions
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import scalafim.image.*

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path, Paths}
import scala.io.Source

final class ImageLibraryParitySuite extends munit.FunSuite:
  private val Tolerance = 1e-6

  private final case class OracleRow(
      x: Int,
      y: Int,
      z: Int,
      time: Int,
      niftiOrdinal: Int,
      ravelOrdinal: Int,
      value: Double,
      world: WorldPoint
  )

  private final case class Oracle(
      fixtureVersion: String,
      generator: String,
      source: String,
      shape: Vector[Int],
      axisCodes: Vector[String],
      affine: Affine[D3],
      temporalSpacingSeconds: Double,
      rows: Vector[OracleRow]
  )

  private def resourcePath(name: String): Path =
    val resource = Option(getClass.getResource(s"/scalafim/image/io/$name"))
      .getOrElse(fail(s"missing test resource $name"))
    Paths.get(resource.toURI)

  private def readOracle(name: String): Oracle =
    val source = Source.fromResource(s"scalafim/image/io/$name")
    val lines =
      try source.getLines().toVector
      finally source.close()

    def metadata(key: String): Vector[String] =
      lines
        .find(_.startsWith(s"# $key\t"))
        .getOrElse(fail(s"missing '$key' metadata in $name"))
        .split("\t", -1)
        .drop(1)
        .toVector

    val affineValues = metadata("affine_row_major").map(_.toDouble)
    val records = lines.filterNot(_.startsWith("#")).drop(1).map: line =>
      val fields = line.split("\t", -1)
      OracleRow(
        x = fields(0).toInt,
        y = fields(1).toInt,
        z = fields(2).toInt,
        time = fields(3).toInt,
        niftiOrdinal = fields(4).toInt,
        ravelOrdinal = fields(5).toInt,
        value = fields(6).toDouble,
        world = WorldPoint(fields(7).toDouble, fields(8).toDouble, fields(9).toDouble)
      )

    Oracle(
      fixtureVersion = metadata("fixture_version").head,
      generator = metadata("generator").head,
      source = metadata("source").head,
      shape = metadata("shape").map(_.toInt),
      axisCodes = metadata("axis_codes"),
      affine = Affine.fromRowMajor[D3](affineValues).toOption.get,
      temporalSpacingSeconds = metadata("temporal_spacing_seconds").head.toDouble,
      rows = records
    )

  private def assertMatrix(actual: Affine[?], expected: Affine[?]): Unit =
    assertEquals(actual.matrix.rows, expected.matrix.rows)
    assertEquals(actual.matrix.cols, expected.matrix.cols)
    actual.rowMajor.zip(expected.rowMajor).zipWithIndex.foreach:
      case ((observed, target), index) =>
        assertEqualsDouble(observed, target, Tolerance, clue = s"affine element $index")

  private def assertWorld(actual: WorldPoint, expected: WorldPoint, clue: String): Unit =
    assertEqualsDouble(actual.x, expected.x, Tolerance, clue = clue)
    assertEqualsDouble(actual.y, expected.y, Tolerance, clue = clue)
    assertEqualsDouble(actual.z, expected.z, Tolerance, clue = clue)

  test("nibabel and neuroim2 independently agree on 4D coordinates and affine") {
    val nibabel = readOracle("nibabel-basic-4d.tsv")
    val neuroim2 = readOracle("neuroim2-basic-4d.tsv")

    assertEquals(nibabel.fixtureVersion, "image-library-parity-v1")
    assertEquals(neuroim2.fixtureVersion, nibabel.fixtureVersion)
    assertEquals(nibabel.generator, "nibabel-5.2.1")
    assertEquals(neuroim2.generator, "neuroim2-0.19.0")
    assertEquals(nibabel.source, "nibabel-neuroim2-basic-4d.nii")
    assertEquals(neuroim2.source, nibabel.source)
    assertEquals(nibabel.shape, Vector(2, 3, 4, 3))
    assertEquals(neuroim2.shape, nibabel.shape)
    assertEquals(nibabel.axisCodes, Vector("A", "L", "S"))
    assertEquals(neuroim2.axisCodes, nibabel.axisCodes)
    assertMatrix(neuroim2.affine, nibabel.affine)
    assertEqualsDouble(neuroim2.temporalSpacingSeconds, nibabel.temporalSpacingSeconds, 0.0)
    assertEquals(nibabel.rows.length, nibabel.shape.product)
    assertEquals(neuroim2.rows.length, nibabel.rows.length)

    var distinctOrderings = 0
    nibabel.rows.zip(neuroim2.rows).foreach: (python, r) =>
      val coordinates = Vector(python.x, python.y, python.z, python.time)
      assertEquals(Vector(r.x, r.y, r.z, r.time), coordinates)
      assertEquals(r.niftiOrdinal, python.niftiOrdinal)
      assertEquals(r.ravelOrdinal, python.ravelOrdinal)
      assertEqualsDouble(r.value, python.value, 0.0, clue = s"coordinates=$coordinates")
      assertWorld(r.world, python.world, s"coordinates=$coordinates")

      val expectedNifti =
        python.x + 2 * (python.y + 3 * (python.z + 4 * python.time))
      val expectedRavel =
        python.time + 3 * (python.z + 4 * (python.y + 3 * python.x))
      assertEquals(python.niftiOrdinal, expectedNifti)
      assertEquals(python.ravelOrdinal, expectedRavel)
      if expectedNifti != expectedRavel then distinctOrderings += 1

    assert(
      distinctOrderings >= 60,
      clue = s"fixture must distinguish NIfTI/neuroim2 and Ravel ordinals; got $distinctOrderings"
    )
  }

  test("native NIfTI ingress translates first-axis-fastest storage to canonical Ravel") {
    val nibabel = readOracle("nibabel-basic-4d.tsv")
    val neuroim2 = readOracle("neuroim2-basic-4d.tsv")
    val decoded = Nifti
      .readSeries(resourcePath(nibabel.source))
      .fold(error => fail(error.message), identity)
    val header = NiftiHeader.fromNative(decoded.header)
    val series = decoded.image

    assertEquals(series.data.shape, Shape(2, 3, 4, 3))
    assert(series.data.isCanonicalLayout)
    assert(series.data.isWholeBuffer)
    assert(series.wholeCanonical.isRight)
    assert(series.grid.record.isRight)
    assertEquals(header.qformCode, 1)
    assertEquals(header.sformCode, 2)
    assertMatrix(header.selectedAffine, nibabel.affine)
    assertEqualsDouble(header.pixdim(3), nibabel.temporalSpacingSeconds, Tolerance)

    nibabel.rows.zip(neuroim2.rows).foreach: (python, r) =>
      val coordinates = Vector(python.x, python.y, python.z, python.time)
      val observed = series(python.x, python.y, python.z, python.time)
      assertEqualsDouble(observed, python.value, 0.0, clue = s"nibabel coordinates=$coordinates")
      assertEqualsDouble(observed, r.value, 0.0, clue = s"neuroim2 coordinates=$coordinates")
      assertEquals(
        series.gridToIndex(python.x, python.y, python.z, python.time),
        python.ravelOrdinal
      )
      assertEquals(series.indexToGrid(python.ravelOrdinal), coordinates)
      assertEqualsDouble(series.valueAtCanonicalOrdinal(python.ravelOrdinal), python.value, 0.0)
      val world = series.grid.voxelToWorld(
        VoxelPoint(python.x.toDouble, python.y.toDouble, python.z.toDouble)
      ).fold(error => fail(error.message), identity)
      assertWorld(world, python.world, s"coordinates=$coordinates")
      val roundTrip = series.grid.worldToVoxel(world).fold(error => fail(error.message), identity)
      assertEqualsDouble(roundTrip.x, python.x.toDouble, Tolerance)
      assertEqualsDouble(roundTrip.y, python.y.toDouble, Tolerance)
      assertEqualsDouble(roundTrip.z, python.z.toDouble, Tolerance)

    val expectedTimeSeries = nibabel.rows
      .filter(row => row.x == 1 && row.y == 2 && row.z == 3)
      .sortBy(_.time)
      .map(_.value)
    assertEquals(series.timeSeries(1, 2, 3).iterator.toVector, expectedTimeSeries)
  }

  test("native NIfTI writer emits oracle first-axis-fastest payload and geometry") {
    val oracle = readOracle("nibabel-basic-4d.tsv")
    val sampleSpace = SampleSpaces.requireD3(
      SampleSpaces(
        dims = oracle.shape.take(3),
        affine = Some(oracle.affine)
      ).addDim(ProviderAxes.time(oracle.shape(3)))
    ).fold(error => fail(error.message), identity)
    val data = NDArray.tabulate[Double](2, 3, 4, 3): (x, y, z, time) =>
      0.25 + 1000.0 * time + 100.0 * x + 10.0 * y + z
    val series = NeuroSeries
      .continuous(sampleSpace, data, ImageMetadata.named("image-library-parity"))
      .map(SomeNeuroSeries.eraseSpace)
      .fold(error => fail(error.message), identity)
    val options = NiftiWriteOptions
      .create(
        datatype = NiftiDatatype.Float32,
        slope = 1.0,
        intercept = 0.0,
        nonSpatialPixelDimensions = Vector(oracle.temporalSpacingSeconds),
        temporalUnit = NiftiTemporalUnit.Second
      )
      .fold(error => fail(error.message), identity)
    val path = Files.createTempFile("scalafim-image-library-parity-", ".nii")

    try
      Nifti.writeSeries(path, series, options).fold(error => fail(error.message), identity)
      val bytes = Files.readAllBytes(path)
      val buffer = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
      assertEquals(buffer.getInt(0), 348)
      assertEquals(buffer.getShort(40).toInt, 4)
      assertEquals(Vector.tabulate(4)(axis => buffer.getShort(42 + 2 * axis).toInt), oracle.shape)
      assertEquals(buffer.getShort(70).toInt, NiftiDatatype.Float32.code)
      assertEquals(buffer.getShort(72).toInt, 32)
      assertEqualsDouble(buffer.getFloat(92).toDouble, oracle.temporalSpacingSeconds, Tolerance)
      assert(buffer.getShort(254).toInt > 0, clue = "writer must emit an sform")

      var row = 0
      while row < 3 do
        var column = 0
        while column < 4 do
          val offset = 280 + row * 16 + column * 4
          assertEqualsDouble(
            buffer.getFloat(offset).toDouble,
            oracle.affine.matrix(row, column),
            Tolerance,
            clue = s"sform($row,$column)"
          )
          column += 1
        row += 1

      val payloadOffset = buffer.getFloat(108).toInt
      val byNiftiOrdinal = oracle.rows.sortBy(_.niftiOrdinal)
      byNiftiOrdinal.zipWithIndex.foreach: (expected, ordinal) =>
        assertEquals(expected.niftiOrdinal, ordinal)
        assertEqualsDouble(
          buffer.getFloat(payloadOffset + 4 * ordinal).toDouble,
          expected.value,
          0.0,
          clue = s"NIfTI payload ordinal $ordinal"
        )
    finally Files.deleteIfExists(path)
  }
