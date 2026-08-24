package external.consumer

import scala.compiletime.testing.typeCheckErrors

final class ProviderSpaceBoundaryCompileSuite extends munit.FunSuite:

  test("deleted VolumeSpace and SeriesSpace names cannot be imported"):
    val volumeErrors = typeCheckErrors("import scalafim.image.VolumeSpace")
    val seriesErrors = typeCheckErrors("import scalafim.image.SeriesSpace")

    assert(volumeErrors.nonEmpty)
    assert(seriesErrors.nonEmpty)

  test("deleted space unsafe constructors cannot be called"):
    val volumeErrors = typeCheckErrors(
      """
import scalafim.image.*

def forge(space: SomeSampleSpace) = VolumeSpace.unsafe(space)
"""
    )
    val seriesErrors = typeCheckErrors(
      """
import scalafim.image.*

def forge(space: SomeSampleSpace) = SeriesSpace.unsafe(space)
"""
    )

    assert(volumeErrors.nonEmpty)
    assert(seriesErrors.nonEmpty)

  test("external consumers construct canonical provider Grid and SampleSpace values"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*

def geometryOnly[F <: Frame[D3]](grid: Grid[F, D3]): Grid[F, D3] =
  grid

def volumeSampling[F <: Frame[D3]](
    grid: Grid[F, D3]
): SampleSpace[F, D3] =
  SampleSpace.create(grid, NonSpatialAxes.empty)

def seriesSampling[F <: Frame[D3]](
    grid: Grid[F, D3],
    time: Axis
): Either[ImageError, SampleSpace[F, D3]] =
  NonSpatialAxes
    .from(Vector(time))
    .map(axes => SampleSpace.create(grid, axes))
"""
    )

    assertEquals(errors, Nil)
