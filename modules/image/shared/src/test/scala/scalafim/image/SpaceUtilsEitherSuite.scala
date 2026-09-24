package scalafim.image

import image4s.geometry.{Affine, D3}
import scalafim.image.world.WorldSpace

class SpaceUtilsEitherSuite extends munit.FunSuite:
  private val oblique = Affine
    .fromRowMajor[D3](Vector(0.95, -0.2, 0.05, -80.0, 0.21, 0.97, -0.03, -110.0, -0.04, 0.02, 2.5, -40.0, 0, 0, 0, 1))
    .fold(e => fail(e.message), identity)

  test("aligned-space construction reports invalid input as typed errors"):
    assert(SpaceUtils.alignedSpace(Vector(4, 0, 3), oblique, None).isLeft)
    assert(SpaceUtils.alignedSpace(Vector(4, 5, 3), oblique, Some(Vector(1.0, -2.0, 1.0))).isLeft)
    assert(SpaceUtils.alignedSpace(Vector(4, 5, 3), oblique, Some(Vector(1.0, 2.0))).isLeft)
    assert(SpaceUtils.alignedSpace(Vector(4, 5, 3), oblique, Some(Vector(2.0))).isRight)

  test("deoblique keeps the input's world space and rejects conflicting options"):
    val mni = WorldSpace.template("MNI152NLin2009cAsym").fold(e => fail(e.message), identity)
    val space = SampleSpaces
      .make(Vector(6, 7, 5), affine = Some(oblique))
      .flatMap(SampleSpaces.inWorld(_, mni))
      .fold(e => fail(e.message), identity)
    val deobliqued = Deoblique.targetEither(space, None, Some(2.0)).fold(e => fail(e.message), identity)
    assertEquals(SampleSpaces.worldOf(deobliqued), Right(mni))
    assert(Deoblique.targetEither(space, Some(space), Some(2.0)).isLeft)
    assert(Deoblique.targetEither(space, None, Some(-1.0)).isLeft)
