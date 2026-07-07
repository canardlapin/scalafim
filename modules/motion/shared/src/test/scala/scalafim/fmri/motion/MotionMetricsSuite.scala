package scalafim.fmri.motion

import scalafim.fmri.motion.fixtures.VolreggerFixtures
import scalafim.image.{Axis, NeuroSpace, NeuroVec, NeuroVol, NArrayUtil}

class MotionMetricsSuite extends munit.FunSuite:

  private def vec1x1x1(values: Vector[Double]): NeuroVec[Double] =
    val data = NArrayUtil.tabulate[Double](values.length)(values)
    NeuroVec.fromLinear(data, NeuroSpace(Vector(1, 1, 1)).addDim(values.length, Some(Axis.Time)), "dvars-fixture")

  private def maskAll(space: NeuroSpace): NeuroVol[Boolean] =
    NeuroVol.fromLinear(NArrayUtil.fillConst[Boolean](space.spatialDims.product, true), space.spatialSpace, "mask")

  test("framewise displacement matches volregger convention") {
    val trace = MotionTrace.unsafe(VolreggerFixtures.fdTrace)
    val fd = MotionMetrics.framewiseDisplacement(trace)
    val pairs = MotionMetrics.framewiseDisplacementPairs(trace)
    assertEquals(fd.length, VolreggerFixtures.fdExpected.length)
    fd.zip(VolreggerFixtures.fdExpected).foreach { case (actual, expected) =>
      assertEqualsDouble(actual, expected, 1e-12)
    }
    assertEquals(pairs.length, VolreggerFixtures.fdExpected.length - 1)
    assertEquals(pairs.head.pair.previous.value, 0)
    assertEquals(pairs.head.pair.current.value, 1)
    assertEqualsDouble(pairs.head.millimeters, VolreggerFixtures.fdExpected(1), 1e-12)
  }

  test("DVARS computes temporal RMS differences") {
    val run = vec1x1x1(VolreggerFixtures.dvarsRunValues)
    val dv = MotionMetrics.dvars(run).fold(err => fail(err.message), identity)
    val pairs = MotionMetrics.dvarsPairs(run).fold(err => fail(err.message), identity)
    assert(dv.head.isNaN)
    assertEqualsDouble(dv(1), VolreggerFixtures.dvarsExpected(1), 1e-12)
    assertEqualsDouble(dv(2), VolreggerFixtures.dvarsExpected(2), 1e-12)
    assertEquals(pairs.length, 2)
    assertEquals(pairs.head.pair, FramePair.unsafe(0, 1))
    assertEqualsDouble(pairs.head.rms, VolreggerFixtures.dvarsExpected(1), 1e-12)
  }

  test("robust DVARS clips values above three times the finite median") {
    val run = vec1x1x1(VolreggerFixtures.robustDvarsRunValues)
    val dv = MotionMetrics.dvars(run, robust = true).fold(err => fail(err.message), identity)
    assert(dv.head.isNaN)
    assertEqualsDouble(dv(1), VolreggerFixtures.robustDvarsExpected(1), 1e-12)
    assertEqualsDouble(dv(2), VolreggerFixtures.robustDvarsExpected(2), 1e-12)
    assertEqualsDouble(dv(3), VolreggerFixtures.robustDvarsExpected(3), 1e-12)
  }

  test("DVARS policy replaces robust Boolean for typed pair metrics") {
    val run = vec1x1x1(VolreggerFixtures.robustDvarsRunValues)
    val pairs = MotionMetrics.dvarsPairs(run, None, DvarsPolicy.RobustClip3xMedian).fold(err => fail(err.message), identity)
    assertEqualsDouble(pairs(2).rms, VolreggerFixtures.robustDvarsExpected(3), 1e-12)
  }

  test("transform displacement for pure translation equals translation length") {
    val pose = RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    assertEqualsDouble(MotionMetrics.transformDisplacement(pose), 1.0, 1e-12)
  }

  test("masked displacement summary uses spatial mask coordinates") {
    val space = NeuroSpace(Vector(2, 1, 1))
    val mask = maskAll(space)
    val pose = RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    val summary = MotionMetrics.maskedDisplacementSummary(mask, pose).fold(err => fail(err.message), identity)
    assertEqualsDouble(summary.median, 1.0, 1e-12)
    assertEqualsDouble(summary.p95, 1.0, 1e-12)
    assertEqualsDouble(summary.max, 1.0, 1e-12)
  }

  test("mask shape mismatch is a typed error") {
    val run = vec1x1x1(Vector(1.0, 2.0))
    val badMask = maskAll(NeuroSpace(Vector(2, 1, 1)))
    assert(MotionMetrics.dvars(run, Some(badMask)).isLeft)
  }
