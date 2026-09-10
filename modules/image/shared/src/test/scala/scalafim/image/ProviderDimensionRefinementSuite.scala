package scalafim.image

import image4s.{Axis, AxisKind, AxisUnit, NonSpatialAxes, SampleSpace, SomeSampleSpace}
import image4s.geometry.{Affine, CoordinateConvention, D2, D3, Frame, Grid, LengthUnit}

class ProviderDimensionRefinementSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test("dynamic D3 admission retains exact provider geometry, physical declarations and FIR axis"):
    val frame = right(Frame.named[D3]("scanner", LengthUnit.Meter, CoordinateConvention.LPS))
    val grid = right(Grid.in(frame)(Vector(2, 3, 5), Affine.identity[D3]))
    val time = right(Axis.regular("response time", AxisKind.Time, 4, -1.0, 0.75, AxisUnit.Seconds))
    val axes = right(NonSpatialAxes.from(Vector(time)))
    val space: SomeSampleSpace = SampleSpace.create(grid, axes)
    val admitted = right(SampleSpaces.requireD3(space))
    assert(admitted eq space)
    assert(admitted.grid eq grid)
    assert(admitted.grid.frame eq frame)
    assert(admitted.nonSpatialAxes eq axes)
    assertEquals(admitted.grid.frame.unit, LengthUnit.Meter)
    assertEquals(admitted.grid.frame.convention, CoordinateConvention.LPS)
    assertEquals(
      SampleSpaces.requireVolumeD3(space),
      Left(SampleSpaceError.UnexpectedNonSpatialAxes(Vector(AxisKind.Time)))
    )

  test("D2 refusal preserves the ScalaFIM error and equal-looking D3 frames stay distinct"):
    val slice = right(Frame.named[D2]("slice"))
    val space2 = SampleSpace.create(right(Grid.in(slice)(Vector(2, 3), Affine.identity[D2])), NonSpatialAxes.empty)
    assertEquals(SampleSpaces.requireD3(space2), Left(SampleSpaceError.ExpectedDimensionality("D3 sample space", 3, 2)))
    def volume() =
      val frame = right(Frame.named[D3]("scanner", convention = CoordinateConvention.RAS))
      SampleSpace.create(right(Grid.in(frame)(Vector(2, 3, 5), Affine.identity[D3])), NonSpatialAxes.empty)
    val first = right(SampleSpaces.requireD3(volume()))
    val second = right(SampleSpaces.requireD3(volume()))
    assert(!first.grid.frame.sameRuntimeOwnerAs(second.grid.frame))
