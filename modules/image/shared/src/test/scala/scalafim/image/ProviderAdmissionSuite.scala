package scalafim.image

import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import image4s.locus.GridDomain
import locus4s.DomainRegistry
import locus4s.data.Field
import ravel.DType.given
import ravel.NDArray

class ProviderAdmissionSuite extends munit.FunSuite:

  test("image4s-locus restores a grid domain and exposes a locus Field"):
    val frame =
      right(
        Frame.persistentNamed[D3](
          right(FrameId.parse("scalafim-provider-admission-frame")),
          "ScalaFIM provider admission frame",
          LengthUnit.Millimeter,
          CoordinateConvention.RAS
        )
      )
    val grid =
      right(
        Grid.createPersistent(
          right(GridId.parse("scalafim-provider-admission-grid")),
          frame
        )(
          Vector(2, 1, 1),
          Affine.identity[D3]
        )
      )
    val registered =
      right(GridDomain.register(grid, "admission voxels", DomainRegistry.empty))
    val restored =
      right(
        GridDomain.restore(
          registered.value.record,
          grid,
          registered.registry
        )
      )
    val image =
      right(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](2, 1, 1)((i, _, _) => 10.0 * (i + 1))
        )
      )
    val spatialField = right(restored.value.spatialField(image))
    val field: Field[restored.S, Double] = spatialField
    val second = right(restored.value.space.index(1))

    assert(
      restored.value.space.sameRuntimeOwnerAs(registered.value.space)
    )
    assert(spatialField.sourceData eq image.data)
    assertEquals(field(second), 20.0)

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
