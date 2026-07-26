package scalafim.image.io

import scalafim.image.{Affine, DMat}

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}

class ItkAffineSuite extends munit.FunSuite:

  private val parameters = Vector(
    1.1, 0.1, 0.0,
    -0.2, 0.9, 0.05,
    0.0, 0.03, 1.2,
    3.0, -4.0, 5.0
  )
  private val center = Vector(10.0, 20.0, 30.0)
  private val expectedLps = DMat.fromRows(
    Vector(
      Vector(1.1, 0.1, 0.0, 0.0),
      Vector(-0.2, 0.9, 0.05, -1.5),
      Vector(0.0, 0.03, 1.2, -1.6),
      Vector(0.0, 0.0, 0.0, 1.0)
    )
  )

  test("MATLAB v4 ITK affine reconstructs the centered stored LPS transform") {
    val path = Files.createTempDirectory("scalafim-itk-mat").resolve("affine.mat")
    writeMatlabV4(path, ByteOrder.LITTLE_ENDIAN, precision = 0)

    val transform = read(path)

    assertMatrix(transform.storedLps.matrix, expectedLps, 1e-12)
    assertEquals(transform.parameters, parameters)
    assertEquals(transform.fixedParameters, center)
  }

  test("big-endian float MATLAB v4 and ITK text decode to the same affine") {
    val dir = Files.createTempDirectory("scalafim-itk-formats")
    val binary = writeMatlabV4(dir.resolve("affine.mat"), ByteOrder.BIG_ENDIAN, precision = 1)
    val text = dir.resolve("affine.txt")
    val content =
      s"""#Insight Transform File V1.0
         |Transform: AffineTransform_double_3_3
         |Parameters: ${parameters.mkString(" ")}
         |FixedParameters: ${center.mkString(" ")}
         |""".stripMargin
    Files.writeString(text, content, StandardCharsets.UTF_8)

    assertMatrix(read(binary).storedLps.matrix, read(text).storedLps.matrix, 2e-6)
  }

  test("RAS conversion, ANTs pullback adapter, and inverse agree pointwise") {
    val path = Files.createTempDirectory("scalafim-itk-direction").resolve("affine.mat")
    writeMatlabV4(path, ByteOrder.LITTLE_ENDIAN, precision = 0)
    val transform = read(path)
    val rasPoint = Vector(-2.0, -3.0, 4.0)
    val lpsPoint = Vector(2.0, 3.0, 4.0)
    val expectedLpsTarget = Affine.applyAffine(transform.storedLps.matrix, lpsPoint)
    val expectedRasTarget = Vector(-expectedLpsTarget(0), -expectedLpsTarget(1), expectedLpsTarget(2))
    val rasTarget = Affine.applyAffine(transform.storedRas.matrix, rasPoint)

    assertVector(rasTarget, expectedRasTarget, 1e-12)
    assertMatrix(transform.antsPullbackRas.matrix, transform.storedRas.matrix, 0.0)
    assertVector(Affine.applyAffine(transform.inverseRas.matrix, rasTarget), rasPoint, 1e-11)
  }

  test("truncated and unsupported MATLAB v4 records fail with typed errors") {
    val dir = Files.createTempDirectory("scalafim-itk-errors")
    val truncated = dir.resolve("truncated.mat")
    Files.write(truncated, Array.fill[Byte](12)(0))
    assert(ItkAffine.read(truncated).isLeft)

    val unsupported = dir.resolve("unsupported.mat")
    writeMatlabV4(unsupported, ByteOrder.LITTLE_ENDIAN, precision = 6)
    ItkAffine.read(unsupported) match
      case Left(ItkAffineReadError.UnsupportedMatlabV4Precision(_, 6)) => ()
      case other => fail(s"expected typed unsupported-precision error, got $other")
  }

  private def read(path: Path): ItkAffineTransform =
    ItkAffine.read(path).fold(error => fail(error.message), identity)

  private def writeMatlabV4(path: Path, order: ByteOrder, precision: Int): Path =
    val parameterName = "AffineTransform_double_3_3\u0000".getBytes(StandardCharsets.US_ASCII)
    val fixedName = "fixed\u0000".getBytes(StandardCharsets.US_ASCII)
    val bytesPerValue = if precision == 0 then 8 else 4
    val bytes = Array.ofDim[Byte](
      20 + parameterName.length + parameters.length * bytesPerValue +
        20 + fixedName.length + center.length * bytesPerValue
    )
    val buffer = ByteBuffer.wrap(bytes).order(order)
    putVariable(buffer, parameterName, parameters, order, precision)
    putVariable(buffer, fixedName, center, order, precision)
    Files.write(path, bytes)
    path

  private def putVariable(
      buffer: ByteBuffer,
      name: Array[Byte],
      values: Vector[Double],
      order: ByteOrder,
      precision: Int
  ): Unit =
    val machine = if order == ByteOrder.LITTLE_ENDIAN then 0 else 1
    buffer.putInt(machine * 1000 + precision * 10)
    buffer.putInt(values.length)
    buffer.putInt(1)
    buffer.putInt(0)
    buffer.putInt(name.length)
    buffer.put(name)
    values.foreach { value =>
      if precision == 0 then buffer.putDouble(value)
      else if precision == 1 then buffer.putFloat(value.toFloat)
      else buffer.putInt(value.toInt)
    }

  private def assertMatrix(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var index = 0
    while index < actual.data.length do
      assertEqualsDouble(actual.data(index), expected.data(index), tolerance)
      index += 1

  private def assertVector(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < actual.length do
      assertEqualsDouble(actual(index), expected(index), tolerance)
      index += 1
