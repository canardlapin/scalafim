package scalafim.fmri.motion

import scalafim.fmri.motion.fixtures.VolreggerFixtures

class RigidPoseSuite extends munit.FunSuite:

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assertEqualsDouble(actual, expected, tol)

  test("identity pose converts to the provider identity affine") {
    val matrix = RigidPose.identity.toAffine.matrix
    var r = 0
    while r < 4 do
      var c = 0
      while c < 4 do
        val expected = if r == c then 1.0 else 0.0
        assertClose(matrix(r, c), expected)
        c += 1
      r += 1
  }

  test("pose affine uses volregger ZYX rotation convention") {
    val matrix = VolreggerFixtures.translationAndYaw.toAffine.matrix
    assertClose(matrix(0, 0), 0.0)
    assertClose(matrix(0, 1), -1.0)
    assertClose(matrix(0, 2), 0.0)
    assertClose(matrix(0, 3), 1.0)
    assertClose(matrix(1, 0), 1.0)
    assertClose(matrix(1, 1), 0.0)
    assertClose(matrix(1, 2), 0.0)
    assertClose(matrix(1, 3), 2.0)
    assertClose(matrix(2, 0), 0.0)
    assertClose(matrix(2, 1), 0.0)
    assertClose(matrix(2, 2), 1.0)
    assertClose(matrix(2, 3), 3.0)
  }

  test("provider affine roundtrip preserves a finite rigid pose") {
    val pose = RigidPose.unsafe(0.25, -0.5, 1.5, 0.1, -0.2, 0.3)
    val roundtrip = RigidPose.fromAffine(pose.toAffine).fold(err => fail(err.message), identity)
    assertClose(roundtrip.tx, pose.tx)
    assertClose(roundtrip.ty, pose.ty)
    assertClose(roundtrip.tz, pose.tz)
    assertClose(roundtrip.rx, pose.rx)
    assertClose(roundtrip.ry, pose.ry)
    assertClose(roundtrip.rz, pose.rz)
  }

  test("rigid pose stores typed translation and ZYX Euler rotation components") {
    val translation = Translation3Mm.make(1.0, -2.0, 3.5).fold(err => fail(err.message), identity)
    val rotation = EulerZYXRad.make(0.1, -0.2, 2.0 * math.Pi + 0.3).fold(err => fail(err.message), identity)
    val pose = RigidPose.make(translation, rotation)

    assertEqualsDouble(pose.translation.tx, 1.0, 1e-12)
    assertEqualsDouble(pose.translation.ty, -2.0, 1e-12)
    assertEqualsDouble(pose.translation.tz, 3.5, 1e-12)
    assertEqualsDouble(pose.rotation.rx, 0.1, 1e-12)
    assertEqualsDouble(pose.rotation.ry, -0.2, 1e-12)
    assertEqualsDouble(pose.rotation.rz, 0.3, 1e-12)
    assertEqualsDouble(pose.tx, 1.0, 1e-12)
    assertEqualsDouble(pose.rz, 0.3, 1e-12)
  }

  test("inverse composes back to identity") {
    val pose = RigidPose.unsafe(0.25, -0.5, 1.5, 0.1, -0.2, 0.3)
    val inv = pose.inverse.fold(err => fail(err.message), identity)
    val composed = pose.andThen(inv).fold(err => fail(err.message), identity)
    assertClose(composed.tx, 0.0)
    assertClose(composed.ty, 0.0)
    assertClose(composed.tz, 0.0)
    assertClose(composed.rx, 0.0)
    assertClose(composed.ry, 0.0)
    assertClose(composed.rz, 0.0)
  }

  test("andThen follows provider application order") {
    val translate = RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    val rotate = RigidPose.unsafe(0.0, 0.0, 0.0, 0.0, 0.0, math.Pi / 2.0)
    val composed = translate.andThen(rotate).fold(err => fail(err.message), identity)
    val point = composed.toAffine(Vector(0.0, 0.0, 0.0)).toOption.get

    assertClose(point(0), 0.0)
    assertClose(point(1), 1.0)
    assertClose(point(2), 0.0)
  }

  test("non-finite pose is rejected by smart constructor") {
    assert(RigidPose.make(Double.NaN, 0.0, 0.0, 0.0, 0.0, 0.0).isLeft)
  }
