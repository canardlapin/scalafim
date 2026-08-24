package external.consumer

import scala.compiletime.testing.typeCheckErrors

class SamplingGeometryBoundaryCompileSuite extends munit.FunSuite:

  test("external consumers cannot bypass checked volume-geometry admission"):
    val applyErrors = typeCheckErrors(
      """
        import scalafim.image.SampleSpaces
        import scalafim.spatial.SamplingGeometry

        val admitted =
          SamplingGeometry
            .volume(SampleSpaces(Vector(2, 2, 1)))
            .toOption
            .get

        admitted match
          case SamplingGeometry.Volume(space, mask) =>
            SamplingGeometry.Volume(space, mask)
          case other =>
            other
      """
    )
    val constructorErrors = typeCheckErrors(
      """
        import scalafim.image.SampleSpaces
        import scalafim.spatial.SamplingGeometry

        val admitted =
          SamplingGeometry
            .volume(SampleSpaces(Vector(2, 2, 1)))
            .toOption
            .get

        admitted match
          case SamplingGeometry.Volume(space, mask) =>
            new SamplingGeometry.Volume(space, mask)
          case other =>
            other
      """
    )

    assert(applyErrors.nonEmpty)
    assert(constructorErrors.nonEmpty)
