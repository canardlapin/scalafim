package scalafim.image.io

import image4s.ImageMetadata
import image4s.nifti.NiftiAffinePolicy
import image4s.nifti.NiftiDatatype
import image4s.nifti.NiftiError
import image4s.nifti.NiftiIoLimits
import image4s.nifti.NiftiIoStrategy
import image4s.nifti.NiftiScalarStored
import image4s.nifti.NiftiWriteOptions
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import scalafim.image.*

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
import java.nio.file.Path

class NiftiSuite extends munit.FunSuite:

  private final case class QForm(
      b: Double,
      c: Double,
      d: Double,
      offset: Vector[Double],
      qfac: Double
  )

  private def assertMatrix(actual: DMat, expected: DMat, tolerance: Double = 1e-6): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var i = 0
    while i < actual.data.length do
      assertEqualsDouble(actual.data(i), expected.data(i), tolerance)
      i += 1

  private def writeFixture(
      path: Path,
      dims: Vector[Int],
      spacing: Vector[Double],
      qform: Option[QForm],
      sform: Option[DMat],
      values: Vector[Double],
      datatype: NiftiDatatype = NiftiDatatype.Float64,
      slope: Double = 1.0,
      intercept: Double = 0.0
  ): Path =
    require(dims.length == 3 || dims.length == 4)
    require(spacing.length == 3)
    require(values.length == dims.product)
    val bytesPerValue = datatype.bitsPerValue / 8
    val bytes = Array.ofDim[Byte](352 + values.length * bytesPerValue)
    val bb = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(0, 348)
    bb.putShort(40, dims.length.toShort)
    var axis = 0
    while axis < 7 do
      bb.putShort(42 + axis * 2, (if axis < dims.length then dims(axis) else 1).toShort)
      axis += 1
    bb.putShort(70, datatype.code.toShort)
    bb.putShort(72, datatype.bitsPerValue.toShort)
    bb.putFloat(76, qform.fold(1.0)(_.qfac).toFloat)
    axis = 0
    while axis < 3 do
      bb.putFloat(80 + axis * 4, spacing(axis).toFloat)
      axis += 1
    bb.putFloat(108, 352.0f)
    bb.putFloat(112, slope.toFloat)
    bb.putFloat(116, intercept.toFloat)

    qform.foreach { q =>
      bb.putShort(252, 1.toShort)
      bb.putFloat(256, q.b.toFloat)
      bb.putFloat(260, q.c.toFloat)
      bb.putFloat(264, q.d.toFloat)
      bb.putFloat(268, q.offset(0).toFloat)
      bb.putFloat(272, q.offset(1).toFloat)
      bb.putFloat(276, q.offset(2).toFloat)
    }

    sform.foreach { affine =>
      bb.putShort(254, 2.toShort)
      var column = 0
      while column < 4 do
        bb.putFloat(280 + column * 4, affine(0, column).toFloat)
        bb.putFloat(296 + column * 4, affine(1, column).toFloat)
        bb.putFloat(312 + column * 4, affine(2, column).toFloat)
        column += 1
    }

    bb.put(344, 'n'.toByte)
    bb.put(345, '+'.toByte)
    bb.put(346, '1'.toByte)
    var i = 0
    while i < values.length do
      val offset = 352 + i * bytesPerValue
      datatype match
        case NiftiDatatype.UInt8   => bb.put(offset, values(i).toInt.toByte)
        case NiftiDatatype.Int16   => bb.putShort(offset, values(i).toShort)
        case NiftiDatatype.Int32   => bb.putInt(offset, values(i).toInt)
        case NiftiDatatype.Float32 => bb.putFloat(offset, values(i).toFloat)
        case NiftiDatatype.Float64 => bb.putDouble(offset, values(i))
      i += 1
    Files.write(path, bytes)
    path

  test("native series round-trip asymmetric 2x3x5x7 coordinates through nii and gzip") {
    val dir = Files.createTempDirectory("scalafim-nifti-suite")
    val sourceSpace =
      NeuroSpace.requireD3(
        NeuroSpace(Vector(2, 3, 5)).addDim(7, Some(Axis.Time))
      ).toOption.get
    val data =
      NDArray.tabulate[Double](2, 3, 5, 7): (x, y, z, time) =>
        x * 1000.0 + y * 100.0 + z * 10.0 + time
    val source =
      NeuroSeries
        .continuous(sourceSpace, data, ImageMetadata.named("asymmetric-series"))
        .toOption
        .map(SomeNeuroSeries.eraseSpace)
        .get
    val options = NiftiWriteOptions.forDatatype(NiftiDatatype.Float32)

    Vector("series.nii", "series.nii.gz").foreach: name =>
      val path = dir.resolve(name)
      assert(Nifti.writeSeries(path, source, options).isRight)
      assertEquals(Nifti.ioStrategy(path), NiftiIoStrategy.BoundedStreaming)
      val loaded = Nifti.readSeries(path).toOption.get.image
      assertEquals(loaded.data.shape, Shape(2, 3, 5, 7))
      var x = 0
      while x < 2 do
        var y = 0
        while y < 3 do
          var z = 0
          while z < 5 do
            var time = 0
            while time < 7 do
              val expected = x * 1000.0 + y * 100.0 + z * 10.0 + time
              assertEqualsDouble(loaded(x, y, z, time), expected, 1e-6)
              time += 1
            z += 1
          y += 1
        x += 1
  }

  test("native volume round-trips asymmetric 2x3x5 coordinates") {
    val dir = Files.createTempDirectory("scalafim-nifti-suite")
    val path = dir.resolve("volume.nii")
    val sourceSpace =
      NeuroSpace.requireSpatialD3(NeuroSpace(Vector(2, 3, 5))).toOption.get
    val data =
      NDArray.tabulate[Double](2, 3, 5): (x, y, z) =>
        x * 100.0 + y * 10.0 + z
    val source =
      NeuroVolume
        .continuous(sourceSpace, data, ImageMetadata.named("asymmetric-volume"))
        .toOption
        .map(SomeNeuroVolume.eraseSpace)
        .get

    assert(Nifti.writeVolume(path, source).isRight)
    val loaded = Nifti.readVolume(path).toOption.get.image

    assertEquals(loaded.data.shape, Shape(2, 3, 5))
    var x = 0
    while x < 2 do
      var y = 0
      while y < 3 do
        var z = 0
        while z < 5 do
          assertEqualsDouble(
            loaded(x, y, z),
            x * 100.0 + y * 10.0 + z,
            1e-12
          )
          z += 1
        y += 1
      x += 1
  }

  test("stored UInt8 retains one native Ravel owner and explicit affine scaling") {
    val dir = Files.createTempDirectory("scalafim-nifti-stored")
    val path = dir.resolve("stored-uint8.nii")
    val fileValues =
      (for
        z <- 0 until 5
        y <- 0 until 3
        x <- 0 until 2
      yield (x + 2 * (y + 3 * z)).toDouble).toVector
    writeFixture(
      path,
      dims = Vector(2, 3, 5),
      spacing = Vector(1.0, 1.0, 1.0),
      qform = None,
      sform = None,
      values = fileValues,
      datatype = NiftiDatatype.UInt8,
      slope = 2.0,
      intercept = 1.0
    )

    val decoded = Nifti.readStored(path).toOption.get
    assertEquals(decoded.header.slope, 2.0)
    assertEquals(decoded.header.intercept, 1.0)
    decoded.image match
      case NiftiScalarStored.UInt8(encoded) =>
        val canonicalRaw =
          (for
            x <- 0 until 2
            y <- 0 until 3
            z <- 0 until 5
          yield x + 2 * (y + 3 * z)).toVector
        assertEquals(
          encoded.data.elementsIterator.map(_.toInt).toVector,
          canonicalRaw
        )
        assertEquals(encoded.valueAt(Vector(1, 2, 4)), Right(59.0))
        assert(encoded.fingerprint.contains("primitive-to-double-affine:v1:uint8"))
      case other =>
        fail(s"expected UInt8 encoded storage, got $other")
  }

  test("qform-only headers reconstruct quaternion rotation, spacing, qfac, and offset") {
    val dir = Files.createTempDirectory("scalafim-nifti-qform")
    val path = dir.resolve("qform-only.nii")
    val halfSqrt = math.sqrt(0.5)
    writeFixture(
      path,
      dims = Vector(1, 1, 1),
      spacing = Vector(2.0, 3.0, 4.0),
      qform = Some(QForm(0.0, 0.0, halfSqrt, Vector(10.0, 20.0, 30.0), qfac = -1.0)),
      sform = None,
      values = Vector(7.0)
    )

    val header = Nifti.readHeader(path).toOption.get
    val expected = DMat.fromRows(
      Vector(
        Vector(0.0, -3.0, 0.0, 10.0),
        Vector(2.0, 0.0, 0.0, 20.0),
        Vector(0.0, 0.0, -4.0, 30.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

    assertEquals(header.qformCode, 1)
    assertEquals(header.sformCode, 0)
    assertMatrix(header.qform.getOrElse(fail("expected qform")), expected)
    assertMatrix(header.space.trans, expected)
  }

  test("sform remains the preferred affine when both qform and sform are present") {
    val dir = Files.createTempDirectory("scalafim-nifti-sform-precedence")
    val path = dir.resolve("both-forms.nii")
    val sform = DMat.fromRows(
      Vector(
        Vector(-2.0, 0.0, 0.0, 40.0),
        Vector(0.0, 3.0, 0.0, 50.0),
        Vector(0.0, 0.0, 4.0, 60.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )
    writeFixture(
      path,
      dims = Vector(1, 1, 1),
      spacing = Vector(2.0, 3.0, 4.0),
      qform = Some(QForm(0.0, 0.0, 0.0, Vector(10.0, 20.0, 30.0), qfac = 1.0)),
      sform = Some(sform),
      values = Vector(1.0)
    )

    val header = Nifti.readHeader(path).toOption.get
    assert(header.qform.nonEmpty)
    assert(header.sform.nonEmpty)
    assertMatrix(header.preferredAffine.getOrElse(fail("expected preferred affine")), sform)
    assertMatrix(header.space.trans, sform)
  }

  test("affine conflicts, unsupported encodings, and resource limits fail with typed errors") {
    val dir = Files.createTempDirectory("scalafim-nifti-policy")
    val conflictPath = dir.resolve("affine-conflict.nii")
    val shifted = DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 30.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )
    writeFixture(
      conflictPath,
      dims = Vector(2, 3, 5),
      spacing = Vector(1.0, 1.0, 1.0),
      qform = Some(QForm(0.0, 0.0, 0.0, Vector(0.0, 0.0, 0.0), 1.0)),
      sform = Some(shifted),
      values = Vector.fill(30)(1.0)
    )

    Nifti.readVolume(
      conflictPath,
      Nifti.ReadOptions.default.copy(
        affinePolicy = NiftiAffinePolicy.RequireAgreement(1e-6)
      )
    ) match
      case Left(NiftiError.AffineFormsDisagree(30.0, 1e-6)) => ()
      case other => fail(s"expected typed affine disagreement, got $other")

    val limited = Nifti.ReadOptions.default.copy(
      ioLimits = NiftiIoLimits.default.copy(maximumPayloadBytes = 200L)
    )
    Nifti.readStored(conflictPath, limited) match
      case Left(NiftiError.PayloadResourceLimitExceeded(_, 240L, 200L)) => ()
      case other => fail(s"expected typed payload limit error, got $other")

    val unsupportedPath = dir.resolve("unsupported.nii")
    writeFixture(
      unsupportedPath,
      dims = Vector(1, 1, 1),
      spacing = Vector(1.0, 1.0, 1.0),
      qform = None,
      sform = None,
      values = Vector(1.0)
    )
    val unsupportedBytes = Files.readAllBytes(unsupportedPath)
    val unsupportedHeader =
      ByteBuffer.wrap(unsupportedBytes).order(ByteOrder.LITTLE_ENDIAN)
    unsupportedHeader.putShort(70, 128.toShort)
    unsupportedHeader.putShort(72, 24.toShort)
    Files.write(unsupportedPath, unsupportedBytes)
    Nifti.readStored(unsupportedPath) match
      case Left(NiftiError.UnsupportedDatatype(128, 24)) => ()
      case other => fail(s"expected typed unsupported datatype error, got $other")
  }

  test("singleton-4D volume reads preserve qform geometry after dropping time") {
    val dir = Files.createTempDirectory("scalafim-nifti-singleton-4d")
    val path = dir.resolve("singleton-4d.nii")
    writeFixture(
      path,
      dims = Vector(2, 1, 1, 1),
      spacing = Vector(2.0, 3.0, 4.0),
      qform = Some(QForm(0.0, 0.0, 0.0, Vector(10.0, 20.0, 30.0), qfac = 1.0)),
      sform = None,
      values = Vector(2.0, 5.0)
    )

    val loaded = Nifti.readVolume(path).toOption.get.image
    val expected = DMat.fromRows(
      Vector(
        Vector(2.0, 0.0, 0.0, 10.0),
        Vector(0.0, 3.0, 0.0, 20.0),
        Vector(0.0, 0.0, 4.0, 30.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

    assertEquals(loaded.sampleSpace.logicalShape.size, 3)
    assertMatrix(
      DMat.fromRowMajorOwned(
        4,
        4,
        loaded.sampleSpace.grid.indexToFrame.rowMajor.toArray
      ),
      expected
    )
    assertEqualsDouble(loaded(0, 0, 0), 2.0, 1e-12)
    assertEqualsDouble(loaded(1, 0, 0), 5.0, 1e-12)
  }
