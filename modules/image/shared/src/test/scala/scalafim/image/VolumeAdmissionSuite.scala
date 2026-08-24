package scalafim.image

import image4s.Axis as ImageAxis
import image4s.AxisKind
import image4s.Continuous
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import ravel.DType.given
import ravel.NDArray

class VolumeAdmissionSuite extends munit.FunSuite:
  private val frame =
    right(Frame.named[D3]("volume-admission-suite"))

  private val grid =
    right(
      Grid.in(frame)(
        Vector(2, 3, 4),
        Affine.identity[D3]
      )
    )

  private val spatialSpace =
    SampleSpace.create(grid, NonSpatialAxes.empty)

  private val data =
    NDArray.tabulate[Double](2, 3, 4): (x, y, z) =>
      100.0 * x + 10.0 * y + z

  private val canonical =
    Array.tabulate(24)(_.toDouble)

  private val customKind =
    right(AxisKind.custom("phase-cycle"))

  Vector(
    AxisKind.Time,
    AxisKind.Channel,
    AxisKind.Echo,
    customKind
  ).foreach: kind =>
    test(s"volume admission rejects a ${kind.id} axis"):
      val space = withAxis(kind)

      assertRejected(
        SomeScalarVolume.fromRavel(data, space),
        kind,
        "Ravel admission"
      )
      assertRejected(
        SomeNeuroVolume.copyFromCanonicalArray[Double, Continuous](
          canonical,
          space
        ),
        kind,
        "copying admission"
      )

  test("spatial D3 admission retains the exact sample-space object"):
    val volume =
      right(SomeScalarVolume.fromRavel(data, spatialSpace))

    assert(
      volume.sampled.sampleSpace.asInstanceOf[AnyRef] eq spatialSpace,
      clue = "safe admission must retain the exact spatial SampleSpace"
    )

  test("an explicit provider spatial projection is admissible and retained"):
    val source = withAxis(AxisKind.Time)
    val projected = source.spatialOnly
    val volume =
      right(SomeScalarVolume.fromRavel(data, projected))

    assert(
      !(projected.asInstanceOf[AnyRef] eq source),
      clue = "the test must exercise an explicit projection"
    )
    assert(
      volume.sampled.sampleSpace.asInstanceOf[AnyRef] eq projected,
      clue = "admission must retain the caller's explicitly projected space"
    )

  test("the spatial-part helper remains an explicit projection boundary"):
    val source = withAxis(AxisKind.Echo)
    val projected = right(SampleSpaces.requireSpatialD3(source))

    assert(projected.nonSpatialAxes.values.isEmpty)
    assert(projected.grid.asInstanceOf[AnyRef] eq source.grid)
    assert(!(projected.asInstanceOf[AnyRef] eq source))

  private def withAxis(kind: AxisKind) =
    val axis = right(ImageAxis.create(kind.id, 5, kind))
    val axes = right(NonSpatialAxes.from(Vector(axis)))
    SampleSpace.create(grid, axes)

  private def assertRejected[A](
      result: Either[NeuroImageError, A],
      expected: AxisKind,
      boundary: String
  ): Unit =
    result match
      case Left(
            NeuroImageError.Space(
              SampleSpaceError.UnexpectedNonSpatialAxes(actual)
            )
          ) =>
        assertEquals(actual, Vector(expected), clue = boundary)
      case other =>
        fail(s"$boundary should reject ${expected.id}; found $other")

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
