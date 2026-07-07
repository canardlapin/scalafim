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

  private def volume(values: Vector[Double], dims: Vector[Int], nVolumes: Int): NeuroVec[Double] =
    val data = NArrayUtil.tabulate[Double](values.length)(values)
    NeuroVec.fromLinear(data, NeuroSpace(dims).addDim(nVolumes, Some(Axis.Time)), "volume")

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

  test("all-zero slice timing is byte-identical to volume application") {
    val run = volume(
      Vector(
        0.0, 1.0, 2.0,
        10.0, 11.0, 12.0,
        100.0, 101.0, 102.0,
        110.0, 111.0, 112.0
      ),
      dims = Vector(3, 1, 2),
      nVolumes = 2
    )
    val trace =
      MotionTrace.unsafe(
        Vector(
          RigidPose.identity,
          RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        )
      )
    val zeroTiming = AcquisitionTiming.Slice(SliceTiming.unsafe(Vector(0.0, 0.0)))
    val timedControl = ApplyControl.make(acquisitionTiming = zeroTiming).fold(err => fail(err.message), identity)

    val volumeCorrected = MotionApplier.apply(run, trace).fold(err => fail(err.message), identity)
    val timedCorrected = MotionApplier.apply(run, trace, timedControl).fold(err => fail(err.message), identity)

    assertSameValues(timedCorrected, volumeCorrected.values.data.toVector)
  }

  test("nonzero slice timing interpolates packet poses during final application") {
    val run = volume(
      Vector(
        0.0, 1.0, 2.0,
        10.0, 11.0, 12.0,
        100.0, 101.0, 102.0,
        110.0, 111.0, 112.0
      ),
      dims = Vector(3, 1, 2),
      nVolumes = 2
    )
    val trace =
      MotionTrace.unsafe(
        Vector(
          RigidPose.identity,
          RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        )
      )
    val sliceTiming = AcquisitionTiming.Slice(SliceTiming.unsafe(Vector(0.0, 1.0)))
    val timedControl = ApplyControl.make(acquisitionTiming = sliceTiming).fold(err => fail(err.message), identity)

    val volumeCorrected = MotionApplier.apply(run, trace).fold(err => fail(err.message), identity)
    val timedCorrected = MotionApplier.apply(run, trace, timedControl).fold(err => fail(err.message), identity)

    assertEqualsDouble(volumeCorrected.values.data(3), 10.0, 1e-12)
    assertEqualsDouble(timedCorrected.values.data(3), 10.0, 1e-12)
    assertEqualsDouble(volumeCorrected.values.data(4), 11.0, 1e-12)
    assertEqualsDouble(timedCorrected.values.data(4), 10.0, 1e-12)
    assertEqualsDouble(volumeCorrected.values.data(5), 12.0, 1e-12)
    assertEqualsDouble(timedCorrected.values.data(5), 11.0, 1e-12)
  }

  test("motion application validates trace length") {
    val run = line(Vector(0.0, 1.0, 2.0, 3.0), nVolumes = 1)
    val trace = MotionTrace.identity(2).fold(err => fail(err.message), identity)
    assert(MotionApplier.apply(run, trace).isLeft)
  }

  test("motion application validates acquisition timing against z dimension") {
    val run = volume(Vector(0.0, 1.0, 2.0, 3.0), dims = Vector(1, 1, 2), nVolumes = 2)
    val trace = MotionTrace.identity(2).fold(err => fail(err.message), identity)
    val badTiming = AcquisitionTiming.Slice(SliceTiming.unsafe(Vector(0.0)))
    val control = ApplyControl.make(acquisitionTiming = badTiming).fold(err => fail(err.message), identity)
    assert(MotionApplier.apply(run, trace, control).isLeft)
  }
