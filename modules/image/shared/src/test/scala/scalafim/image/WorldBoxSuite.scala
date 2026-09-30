package scalafim.image

import image4s.geometry.Affine
import image4s.geometry.D3
import scala.compiletime.testing.typeCheckErrors
import scalafim.image.world.Spaces

class WorldBoxSuite extends munit.FunSuite:

  private def box(minimum: WorldPoint, maximum: WorldPoint): WorldBox[Spaces.Mni2009c] =
    WorldBox.fromExtrema[Spaces.Mni2009c](Spaces.MNI152NLin2009cAsym, minimum, maximum).fold(error => fail(error.message), identity)

  test("extrema, centre, diagonal and containment"):
    val b = box(WorldPoint(0.0, 0.0, 0.0), WorldPoint(2.0, 4.0, 4.0))
    assertEquals(b.center.toWorldPoint, WorldPoint(1.0, 2.0, 2.0))
    assertEqualsDouble(b.diagonal, 6.0, 1e-12)
    assert(b.contains(b.center))
    assert(b.frame eq Spaces.MNI152NLin2009cAsym)
    assert(WorldBox.fromExtrema[Spaces.Mni2009c](Spaces.MNI152NLin2009cAsym, WorldPoint(1.0, 0.0, 0.0), WorldPoint(0.0, 1.0, 1.0)).isLeft)

  test("union and enclosing agree"):
    val a = box(WorldPoint(0.0, 0.0, 0.0), WorldPoint(1.0, 1.0, 1.0))
    val b = box(WorldPoint(-1.0, 2.0, 0.5), WorldPoint(0.5, 3.0, 2.0))
    val united = a.union(b)
    assertEquals(united, box(WorldPoint(-1.0, 0.0, 0.0), WorldPoint(1.0, 3.0, 2.0)))
    assertEquals(WorldBox.enclosing(Vector(a.min, a.max, b.min, b.max)), Some(united))
    assertEquals(WorldBox.enclosing(Vector.empty[image4s.geometry.Point[Spaces.Mni2009c, D3]]), None)

  test("a grid's bounds enclose its voxel centres in its own frame"):
    val affine =
      ProviderSpaces.affine(
        Vector(
          Vector(-2.0, 0.0, 0.0, 10.0),
          Vector(0.0, 3.0, 0.0, 20.0),
          Vector(0.0, 0.0, 4.0, 30.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val grid = GridSpec.in(Spaces.MNI152NLin2009cAsym)(SpatialDims(3, 4, 5), affine).fold(error => fail(error.message), identity)
    val bounds: WorldBox[Spaces.Mni2009c] = grid.bounds
    assertEquals(bounds.min.toWorldPoint, WorldPoint(6.0, 20.0, 30.0))
    assertEquals(bounds.max.toWorldPoint, WorldPoint(10.0, 29.0, 46.0))

  test("boxes in different frames cannot be combined"):
    val errors = typeCheckErrors(
      """
        import scalafim.image.WorldBox
        import scalafim.image.world.Spaces
        val mniBox: WorldBox[Spaces.Mni2009c] = ???
        val fsBox: WorldBox[Spaces.FsAverage] = ???
        mniBox.union(fsBox)
      """
    )
    assert(errors.nonEmpty && errors.forall(_.message.contains("Required")), clue = errors.toString)
