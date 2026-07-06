package scalafim.fmri.motion

import scalafim.fmri.motion.fixtures.VolreggerFixtures

class RigidPoseSuite extends munit.FunSuite:

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assertEqualsDouble(actual, expected, tol)

  test("identity pose converts to identity matrix") {
    val matrix = RigidPose.identity.toMatrix
    var r = 0
    while r < 4 do
      var c = 0
      while c < 4 do
        val expected = if r == c then 1.0 else 0.0
        assertClose(matrix(r, c), expected)
        c += 1
      r += 1
  }

  test("pose to matrix uses volregger ZYX rotation convention") {
    val matrix = VolreggerFixtures.translationAndYaw.toMatrix
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

  test("matrix roundtrip preserves a finite rigid pose") {
    val pose = RigidPose.unsafe(0.25, -0.5, 1.5, 0.1, -0.2, 0.3)
    val roundtrip = RigidPose.fromMatrix(pose.toMatrix).fold(err => fail(err.message), identity)
    assertClose(roundtrip.tx, pose.tx)
    assertClose(roundtrip.ty, pose.ty)
    assertClose(roundtrip.tz, pose.tz)
    assertClose(roundtrip.rx, pose.rx)
    assertClose(roundtrip.ry, pose.ry)
    assertClose(roundtrip.rz, pose.rz)
  }

  test("inverse composes back to identity") {
    val pose = RigidPose.unsafe(0.25, -0.5, 1.5, 0.1, -0.2, 0.3)
    val inv = pose.inverse.fold(err => fail(err.message), identity)
    val composed = pose.compose(inv).fold(err => fail(err.message), identity)
    assertClose(composed.tx, 0.0)
    assertClose(composed.ty, 0.0)
    assertClose(composed.tz, 0.0)
    assertClose(composed.rx, 0.0)
    assertClose(composed.ry, 0.0)
    assertClose(composed.rz, 0.0)
  }

  test("non-finite pose is rejected by smart constructor") {
    assert(RigidPose.make(Double.NaN, 0.0, 0.0, 0.0, 0.0, 0.0).isLeft)
  }
