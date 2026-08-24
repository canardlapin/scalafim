package scalafim.image

import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit

final class SearchlightCenterSuite extends munit.FunSuite:

  test("equal centers hash equally across restored views of one grid owner"):
    val frame =
      right(
        Frame.persistentNamed[D3](
          right(FrameId.parse("searchlight-center-frame")),
          "Searchlight center frame",
          LengthUnit.Millimeter,
          CoordinateConvention.RAS
        )
      )
    val original =
      right(
        Grid.createPersistent(
          right(GridId.parse("searchlight-center-grid")),
          frame
        )(
          Vector(2, 2, 1),
          Affine.identity[D3]
        )
      )
    val registry = right(Grid.Registry.empty.register(original))
    val restored =
      right(
        Grid.restore(
          right(original.record),
          frame,
          registry
        )
      ).grid
    val foreign =
      right(
        Grid.createPersistent(
          right(GridId.parse("searchlight-center-foreign-grid")),
          frame
        )(
          Vector(2, 2, 1),
          Affine.identity[D3]
        )
      )
    val voxel = VoxelCoord(1, 0, 0)
    val first = right(SearchlightCenter.make(original, voxel))
    val sameOwner = right(SearchlightCenter.make(restored, voxel))
    val foreignOwner = right(SearchlightCenter.make(foreign, voxel))

    assert(!original.eq(restored))
    assert(original.sameRuntimeOwnerAs(restored))
    assertEquals(first, sameOwner)
    assertEquals(first.hashCode(), sameOwner.hashCode())
    assertNotEquals(first, foreignOwner)

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
