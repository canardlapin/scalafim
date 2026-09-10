package scalafim.image.io

import image4s.{Axis, AxisKind, AxisUnit, NonSpatialAxes, SampleSpace}
import image4s.geometry.{Affine, CoordinateConvention, D3, Frame, Grid, LengthUnit}
import image4s.nifti.{
  NiftiCoordinateSystem,
  NiftiDatatype,
  NiftiError,
  NiftiExtension,
  NiftiTemporalUnit,
  NiftiWriteOptions
}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}

class NiftiIncrementalOutputSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  private def withPath[A](run: Path => A): A =
    val dir = Files.createTempDirectory("scalafim-incremental-")
    val path = dir.resolve("staging.nii")
    try run(path)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(dir)

  private def space(axes: NonSpatialAxes = NonSpatialAxes.empty) =
    val frame = right(Frame.named[D3]("scanner", LengthUnit.Millimeter, CoordinateConvention.RAS))
    val affine = right(
      Affine.fromRowMajor[D3](
        Vector(2.0, 0.25, 0.0, -12.0, 0.0, 3.0, 0.0, 8.0, 0.0, 0.0, 4.0, -5.0, 0.0, 0.0, 0.0, 1.0)
      )
    )
    SampleSpace.create(right(Grid.in(frame)(Vector(2, 3, 5), affine)), axes)

  test("produced FIR blocks retain owners and round-trip asymmetric spatial coordinates"):
    withPath: path =>
      val time = right(Axis.regular("response time", AxisKind.Time, 4, 0.0, 0.75, AxisUnit.Seconds))
      val axes = right(NonSpatialAxes.from(Vector(time)))
      val sampling = space(axes)
      val options = right(
        NiftiWriteOptions.create(
          datatype = NiftiDatatype.Float64,
          slope = 1.0,
          intercept = 0.0,
          nonSpatialPixelDimensions = Vector(0.75),
          temporalUnit = NiftiTemporalUnit.Second,
          coordinateSystem = NiftiCoordinateSystem.AlignedAnatomical
        )
      )
      right(Nifti.withScalarWriter(path, sampling, options): writer =>
        assert(writer.grid eq sampling.grid)
        assert(writer.grid.frame eq sampling.grid.frame)
        assert(writer.axes eq axes)
        // Deliberately out of order. Spatial x is fastest; native Ravel t is fastest.
        Vector(3, 1, 0, 2).foreach: t =>
          Vector(15, 0).foreach: first =>
            val values = Array.tabulate(15): i =>
              val n = first + i
              val x = n % 2
              val y = (n / 2) % 3
              val z = n / 6
              x * 1000.0 + y * 100.0 + z * 10.0 + t
            right(writer.writeSpatialSpan(Vector(t), first.toLong, values))
        Right(()))
      val bytes = ByteBuffer.wrap(Files.readAllBytes(path)).order(ByteOrder.LITTLE_ENDIAN)
      assertEquals(bytes.getShort(254).toInt, 2)
      assertEquals(bytes.get(123).toInt & 63, 10) // millimeters and seconds
      assertEquals(bytes.getFloat(92), 0.75f)
      assertEquals(bytes.getFloat(284), 0.25f)
      assertEquals(bytes.getFloat(292), -12.0f)
      val offset = bytes.getFloat(108).toInt
      val loaded = right(Nifti.readSeries(path)).image
      for x <- 0 until 2; y <- 0 until 3; z <- 0 until 5; t <- 0 until 4 do
        val expected = x * 1000.0 + y * 100.0 + z * 10.0 + t
        val physicalIndex = x + 2 * (y + 3 * (z + 5 * t))
        assertEquals(bytes.getDouble(offset + 8 * physicalIndex), expected)
        assertEquals(loaded(x, y, z, t), expected)

  test("sparse selected maps preserve caller extensions and never reinterpret Ravel offsets"):
    withPath: path =>
      val maps = right(Axis.ordinal("estimate", AxisKind.Batch, 2))
      val sampling = space(right(NonSpatialAxes.from(Vector(maps))))
      val manifest = "{\"maps\":[\"condition-A\",\"condition-B\"],\"units\":\"signal\"}".getBytes("UTF-8").toVector
      val extension = right(NiftiExtension.create(6, manifest))
      val options = right(NiftiWriteOptions.create(NiftiDatatype.Int16, 2.0, 10.0))
      right(Nifti.withScalarWriter(path, sampling, options, Vector(extension)): writer =>
        writer.writeSpatialBlock(Vector(1), Array(29L, 1L, 12L), Array(30.0, 14.0, 18.0)))
      val bytes = ByteBuffer.wrap(Files.readAllBytes(path)).order(ByteOrder.LITTLE_ENDIAN)
      val offset = bytes.getFloat(108).toInt
      for i <- 0 until 60 do
        val expected = Map(59 -> 10, 31 -> 2, 42 -> 4).getOrElse(i, 0)
        assertEquals(bytes.getShort(offset + 2 * i).toInt, expected)
      assertEquals(bytes.getInt(356), 6)
      assertEquals(Files.readAllBytes(path).slice(360, 360 + manifest.size).toVector, manifest)

  test("existing output is preserved and callback exceptions close the provider resource"):
    withPath: path =>
      val sampling = space()
      Files.write(path, Array[Byte](7, 8, 9))
      assert(Nifti.openScalarWriter(path, sampling).isLeft)
      assertEquals(Files.readAllBytes(path).toVector, Vector[Byte](7, 8, 9))
      Files.delete(path)
      var writeAfterScope: () => Either[NiftiError, Unit] = () => fail("callback was not entered")
      val failure = new IllegalStateException("fit interrupted")
      val thrown = intercept[IllegalStateException]:
        Nifti.withScalarWriter(path, sampling): writer =>
          writeAfterScope = () => writer.writeSpatialSpan(Vector.empty, 0L, Array(1.0))
          right(writeAfterScope())
          throw failure
      assert(thrown eq failure)
      assertEquals(writeAfterScope(), Left(NiftiError.OutputClosed))
      assert(Files.exists(path)) // caller still owns cleanup and scientific completeness
