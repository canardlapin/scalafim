package scalafim.image.io

import scalafim.image.{Axis, DMat, NeuroSpace, NeuroVec, NeuroVol, PrimitiveBuffers}

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
      values: Vector[Double]
  ): Path =
    require(dims.length == 3 || dims.length == 4)
    require(spacing.length == 3)
    require(values.length == dims.product)
    val bytes = Array.ofDim[Byte](352 + values.length * 8)
    val bb = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(0, 348)
    bb.putShort(40, dims.length.toShort)
    var axis = 0
    while axis < 7 do
      bb.putShort(42 + axis * 2, (if axis < dims.length then dims(axis) else 1).toShort)
      axis += 1
    bb.putShort(70, 64.toShort)
    bb.putShort(72, 64.toShort)
    bb.putFloat(76, qform.fold(1.0)(_.qfac).toFloat)
    axis = 0
    while axis < 3 do
      bb.putFloat(80 + axis * 4, spacing(axis).toFloat)
      axis += 1
    bb.putFloat(108, 352.0f)
    bb.putFloat(112, 1.0f)

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
      bb.putDouble(352 + i * 8, values(i))
      i += 1
    Files.write(path, bytes)
    path

  test("writeVec round-trips a 4D double NIfTI through the lightweight reader") {
    val dir = Files.createTempDirectory("scalafim-nifti-suite")
    val path = dir.resolve("maps.nii")
    val data = PrimitiveBuffers.fromArray(Array(1.0, 2.0, 3.0, 4.0))
    val space = NeuroSpace(Vector(2, 1, 1)).addDim(2, Some(Axis.Time))
    val vec = NeuroVec.fromLinear(data, space, "maps")

    Nifti.writeVec(path, vec)
    val loaded = Nifti.readVec(path)

    assertEquals(loaded.space.dims.take(4), Vector(2, 1, 1, 2))
    assertEqualsDouble(loaded.linear(0), 1.0, 1e-12)
    assertEqualsDouble(loaded.linear(1), 2.0, 1e-12)
    assertEqualsDouble(loaded.linear(2), 3.0, 1e-12)
    assertEqualsDouble(loaded.linear(3), 4.0, 1e-12)
  }

  test("writeVol round-trips a 3D double NIfTI through the lightweight reader") {
    val dir = Files.createTempDirectory("scalafim-nifti-suite")
    val path = dir.resolve("volume.nii")
    val data = PrimitiveBuffers.fromArray(Array(5.0, 6.0, 7.0, 8.0))
    val space = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear(data, space, "volume")

    Nifti.writeVol(path, vol)
    val loaded = Nifti.readVol(path)

    assertEquals(loaded.space.dims.take(3), Vector(2, 2, 1))
    assertEqualsDouble(loaded.linear(0), 5.0, 1e-12)
    assertEqualsDouble(loaded.linear(1), 6.0, 1e-12)
    assertEqualsDouble(loaded.linear(2), 7.0, 1e-12)
    assertEqualsDouble(loaded.linear(3), 8.0, 1e-12)
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

    val header = Nifti.readHeader(path)
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

    val header = Nifti.readHeader(path)
    assert(header.qform.nonEmpty)
    assert(header.sform.nonEmpty)
    assertMatrix(header.preferredAffine.getOrElse(fail("expected preferred affine")), sform)
    assertMatrix(header.space.trans, sform)
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

    val loaded = Nifti.readVol(path)
    val expected = DMat.fromRows(
      Vector(
        Vector(2.0, 0.0, 0.0, 10.0),
        Vector(0.0, 3.0, 0.0, 20.0),
        Vector(0.0, 0.0, 4.0, 30.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

    assertEquals(loaded.space.ndim, 3)
    assertMatrix(loaded.space.trans, expected)
    assertEqualsDouble(loaded.linear(0), 2.0, 1e-12)
    assertEqualsDouble(loaded.linear(1), 5.0, 1e-12)
  }
