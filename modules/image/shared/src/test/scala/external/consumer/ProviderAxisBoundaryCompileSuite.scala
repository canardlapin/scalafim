package external.consumer

import scala.compiletime.testing.typeCheckErrors

final class ProviderAxisBoundaryCompileSuite extends munit.FunSuite:

  test("the deleted ScalaFIM Axis and AxisSet types cannot be imported"):
    val axisErrors = typeCheckErrors("import scalafim.image.Axis")
    val axisSetErrors = typeCheckErrors("import scalafim.image.AxisSet")

    assert(axisErrors.nonEmpty)
    assert(axisSetErrors.nonEmpty)

  test("time-axis construction uses image4s Axis directly"):
    val errors = typeCheckErrors(
      """
import image4s.Axis
import image4s.AxisKind
import scalafim.image.SampleSpaces

val time = Axis.ordinal("time", AxisKind.Time, 5).toOption.get
val volume = SampleSpaces(Vector(2, 2, 2))
val space = volume.typed.appendNonSpatial(time).toOption.get
val retained: image4s.Axis = space.nonSpatialAxes.values.head
"""
    )

    assertEquals(errors, Nil)
