package scalafim.fmri.motion

import scalafim.image.{Axis, NeuroSpace, NeuroVec, NArrayUtil}

class MotionApplierSuite extends munit.FunSuite:

  private def line(values: Vector[Double], nVolumes: Int = 1): NeuroVec[Double] =
    val data = NArrayUtil.tabulate[Double](values.length)(values)
    NeuroVec.fromLinear(
      data,
      NeuroSpace(Vector(values.length / nVolumes, 1, 1)).addDim(nVolumes, Some(Axis.Time)),
      "line"
    )

  private def assertSameValues(actual: NeuroVec[Double], expected: Vector[Double], tol: Double = 1e-12): Unit =
    assertEquals(actual.values.data.length, expected.length)
    var i = 0
    while i < expected.length do
      assertEqualsDouble(actual.values.data(i), expected(i), tol)
      i += 1

  test("identity motion application is one-pass and preserves data") {
    val run = line(Vector(0.0, 1.0, 2.0, 3.0, 10.0, 11.0, 12.0, 13.0), nVolumes = 2)
    val trace = MotionTrace.identity(2).fold(err => fail(err.message), identity)
    val corrected = MotionApplier.apply(run, trace).fold(err => fail(err.message), identity)
    assertEquals(corrected.space, run.space)
    assertEquals(corrected.label, run.label)
    assertSameValues(corrected, run.values.data.toVector)
  }

  test("positive moving-to-fixed translation pulls fixed voxels from lower source indices") {
    val run = line(Vector(0.0, 1.0, 2.0, 3.0))
    val trace = MotionTrace.unsafe(Vector(RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)))
    val corrected = MotionApplier.apply(run, trace).fold(err => fail(err.message), identity)
    assertSameValues(corrected, Vector(0.0, 0.0, 1.0, 2.0))
  }

  test("negative translation with clamp padding samples high edge repeatedly") {
    val run = line(Vector(0.0, 1.0, 2.0, 3.0))
    val trace = MotionTrace.unsafe(Vector(RigidPose.unsafe(-1.0, 0.0, 0.0, 0.0, 0.0, 0.0)))
    val corrected = MotionApplier.apply(run, trace).fold(err => fail(err.message), identity)
    assertSameValues(corrected, Vector(1.0, 2.0, 3.0, 3.0))
  }

  test("zero padding returns zero outside the source field") {
    val run = line(Vector(0.0, 1.0, 2.0, 3.0))
    val trace = MotionTrace.unsafe(Vector(RigidPose.unsafe(-1.0, 0.0, 0.0, 0.0, 0.0, 0.0)))
    val control = ApplyControl.make(padMode = PadMode.Zero).fold(err => fail(err.message), identity)
    val corrected = MotionApplier.apply(run, trace, control).fold(err => fail(err.message), identity)
    assertSameValues(corrected, Vector(1.0, 2.0, 3.0, 0.0))
  }

  test("motion application validates trace length") {
    val run = line(Vector(0.0, 1.0, 2.0, 3.0), nVolumes = 1)
    val trace = MotionTrace.identity(2).fold(err => fail(err.message), identity)
    assert(MotionApplier.apply(run, trace).isLeft)
  }
