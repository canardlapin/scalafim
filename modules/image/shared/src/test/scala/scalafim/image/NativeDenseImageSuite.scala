package scalafim.image

import image4s.Axis
import image4s.AxisKind
import image4s.Continuous
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import ravel.CanonicalArray.*
import ravel.DType.given
import ravel.NDArray

class NativeDenseImageSuite extends munit.FunSuite:
  private val frame =
    right(Frame.named[D3]("native-dense-image-suite"))

  private val grid =
    right(
      Grid.in(frame)(
        Vector(2, 3, 4),
        Affine.identity[D3]
      )
    )

  private val volumeSpace =
    SampleSpace.create(grid, NonSpatialAxes.empty)

  private val timeAxis =
    right(Axis.create("time", 5, AxisKind.Time))

  private val timeAxes =
    right(NonSpatialAxes.from(Vector(timeAxis)))

  private val seriesSpace =
    SampleSpace.create(grid, timeAxes)

  private val volumeData =
    NDArray.tabulate[Double](2, 3, 4): (x, y, z) =>
      100.0 * x + 10.0 * y + z

  private val seriesData =
    NDArray.tabulate[Double](2, 3, 4, 5): (x, y, z, time) =>
      1000.0 * x + 100.0 * y + 10.0 * z + time

  test("NeuroVolume is the exact Sampled object and Ravel owner"):
    val sampled =
      right(Sampled.continuous(volumeSpace, volumeData))
    val volume = NeuroVolume.fromSampled(sampled)
    val semantic: SomeScalarVolume[Double] = volume
    val agnostic: AnyNeuroVolume[Double] = volume

    assert(volume.sampled eq sampled)
    assert(semantic.asInstanceOf[AnyRef] eq sampled)
    assert(agnostic.asInstanceOf[AnyRef] eq sampled)
    assert(volume.data eq volumeData)
    assertEqualsDouble(volume(1, 2, 3), 123.0, 0.0)

    val direct =
      right(NeuroVolume.continuous(volumeSpace, volumeData))
    assert(direct.data eq volumeData)

  test("whole-canonical access uses Ravel last-axis-fastest order"):
    val volume =
      right(NeuroVolume.continuous(volumeSpace, volumeData))
    val canonical = right(volume.wholeCanonical)

    assertEqualsDouble(canonical.readLinear(0), 0.0, 0.0)
    assertEqualsDouble(canonical.readLinear(1), 1.0, 0.0)
    assertEqualsDouble(canonical.readLinear(4), 10.0, 0.0)
    assertEqualsDouble(canonical.readLinear(12), 100.0, 0.0)
    assertEqualsDouble(canonical.readLinear(23), 123.0, 0.0)

  test("canonical Scala arrays cross an explicitly copying boundary"):
    val volumeInput = Array.tabulate(24)(_.toDouble)
    val volume =
      right(
        NeuroVolume.copyContinuousFromCanonicalArray(
          volumeSpace,
          volumeInput
        )
      )
    volumeInput(0) = -1.0

    assertEqualsDouble(volume(0, 0, 0), 0.0, 0.0)
    assertEqualsDouble(volume(0, 0, 1), 1.0, 0.0)
    assertEqualsDouble(volume(1, 0, 0), 12.0, 0.0)

    val seriesInput = Array.tabulate(120)(_.toDouble)
    val series =
      right(
        NeuroSeries.copyContinuousFromCanonicalArray(
          seriesSpace,
          seriesInput
        )
      )
    seriesInput(0) = -1.0

    assertEqualsDouble(series(0, 0, 0, 0), 0.0, 0.0)
    assertEqualsDouble(series(0, 0, 0, 1), 1.0, 0.0)
    assertEqualsDouble(series(1, 0, 0, 0), 60.0, 0.0)

    NeuroVolume.copyContinuousFromCanonicalArray(
      volumeSpace,
      Array(1.0)
    ) match
      case Left(NativeImageError.CanonicalArraySizeMismatch(24, 1)) => ()
      case other => fail(s"expected a canonical-size error, found $other")

  test("crop, flip, stride, and singleton planes remain immutable views"):
    val volume =
      right(NeuroVolume.continuous(volumeSpace, volumeData))

    val cropped = right(volume.cropVolume(Vector(1, 1, 1), Vector(1, 2, 3)))
    assertEquals(cropped.grid.shape, Vector(1, 2, 3))
    assertEqualsDouble(cropped.data(0, 0, 0), 111.0, 0.0)
    assert(!cropped.data.isWholeBuffer)

    val flipped = right(volume.flipVolume(0))
    assertEqualsDouble(flipped.data(0, 2, 3), 123.0, 0.0)
    assert(!flipped.data.isCanonicalLayout)

    val strided = right(volume.strideVolume(Vector(1, 2, 2)))
    assertEquals(strided.grid.shape, Vector(2, 2, 2))
    assertEqualsDouble(strided.data(1, 1, 1), 122.0, 0.0)
    assert(!strided.data.isCanonicalLayout)

    val plane = right(volume.plane(axis = 2, index = 1))
    assertEquals(plane.grid.shape, Vector(2, 3, 1))
    assertEqualsDouble(plane.data(1, 2, 0), 121.0, 0.0)
    assert(!plane.data.isWholeBuffer)

  test("explicit materialization creates a distinct whole-canonical owner"):
    val volume =
      right(NeuroVolume.continuous(volumeSpace, volumeData))
    val flipped = right(volume.flipVolume(0))
    val materialized =
      NeuroVolume.fromSampled(flipped.materializedCopy)

    assert(!(materialized.data eq flipped.data))
    assert(materialized.data.isCanonicalLayout)
    assert(materialized.data.isWholeBuffer)
    assertEqualsDouble(materialized(0, 2, 3), 123.0, 0.0)

  test("NeuroSeries certifies one Time axis without a wrapper"):
    val sampled =
      right(Sampled.continuous(seriesSpace, seriesData))
    val series = right(NeuroSeries.fromSampled(sampled))
    val semantic: SomeScalarSeries[Double] = series
    val agnostic: AnyNeuroSeries[Double] = series

    assert(series.sampled eq sampled)
    assert(semantic.asInstanceOf[AnyRef] eq sampled)
    assert(agnostic.asInstanceOf[AnyRef] eq sampled)
    assert(series.data eq seriesData)
    assertEqualsDouble(series(1, 2, 3, 4), 1234.0, 0.0)

  test("NeuroSeries rejects a non-Time fourth axis"):
    val channel = right(Axis.create("channel", 5, AxisKind.Channel))
    val axes = right(NonSpatialAxes.from(Vector(channel)))
    val channelSpace = SampleSpace.create(grid, axes)
    val sampled = right(Sampled.continuous(channelSpace, seriesData))

    NeuroSeries.fromSampled(sampled) match
      case Left(NativeImageError.ExpectedSingleTimeAxis(actual)) =>
        assertEquals(actual, Vector(AxisKind.Channel))
      case other =>
        fail(s"expected a Time-axis error, found $other")

  test("canonical series reshapes to voxel-by-time without copying values"):
    val series =
      right(NeuroSeries.continuous(seriesSpace, seriesData))
    val matrix = right(series.voxelTimeMatrix)

    assertEquals(shapeOf(matrix), Vector(24, 5))
    assert(matrix.isCanonicalLayout)
    assert(matrix.isWholeBuffer)
    assertEqualsDouble(matrix(0, 4), 4.0, 0.0)
    assertEqualsDouble(matrix(23, 4), 1234.0, 0.0)

  test("time selection and spatial series transforms preserve view layouts"):
    val series =
      right(NeuroSeries.continuous(seriesSpace, seriesData))

    val time = right(series.volumeAt(3))
    assertEqualsDouble(time.data(1, 2, 3), 1233.0, 0.0)
    assert(!time.data.isCanonicalLayout)

    val cropped = right(series.cropSeries(Vector(1, 1, 1), Vector(1, 2, 3)))
    assertEqualsDouble(cropped.data(0, 0, 0, 4), 1114.0, 0.0)
    assert(!cropped.data.isWholeBuffer)

    val flipped = right(series.flipSeries(2))
    assertEqualsDouble(flipped.data(1, 2, 0, 4), 1234.0, 0.0)
    assert(!flipped.data.isCanonicalLayout)

    val strided = right(series.strideSeries(Vector(1, 2, 2)))
    assertEquals(strided.grid.shape, Vector(2, 2, 2))
    assertEqualsDouble(strided.data(1, 1, 1, 4), 1224.0, 0.0)
    assert(!strided.data.isCanonicalLayout)

  test("noncanonical series requires an explicit materialization before reshape"):
    val series =
      right(NeuroSeries.continuous(seriesSpace, seriesData))
    val flipped = right(series.flipSeries(0))

    assert(flipped.data.isCanonicalLayout == false)
    val checked =
      SomeNeuroSeries
        .fromSampled(flipped)
        .fold(error => fail(error.message), identity)
    assert(checked.data.isCanonicalLayout == false)

  private def shapeOf[A, R <: ravel.AnyRank](
      array: NDArray[A, R]
  ): Vector[Int] =
    Vector.tabulate(array.shape.rank)(array.shape.apply)

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
