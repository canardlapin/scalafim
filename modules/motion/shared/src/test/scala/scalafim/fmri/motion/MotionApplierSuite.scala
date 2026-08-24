package scalafim.fmri.motion

import scalafim.image.*
import scalafim.image.SampleSpaces.*

class MotionApplierSuite extends munit.FunSuite:

  private def line(values: Vector[Double], nVolumes: Int = 1): SomeScalarSeries[Double] =
    val data = PrimitiveBuffers.tabulate[Double](values.length)(values)
    SomeScalarSeries.unsafeCopyFromCanonicalArray(
      data,
      SampleSpaces(Vector(values.length / nVolumes, 1, 1)).addDim(ProviderAxes.time(nVolumes)),
      "line"
    )

  private def volume(values: Vector[Double], dims: Vector[Int], nVolumes: Int): SomeScalarSeries[Double] =
    val data = PrimitiveBuffers.tabulate[Double](values.length)(values)
    SomeScalarSeries.unsafeCopyFromCanonicalArray(data, SampleSpaces(dims).addDim(ProviderAxes.time(nVolumes)), "volume")

  private def assertSameValues(actual: SomeScalarSeries[Double], expected: Vector[Double], tol: Double = 1e-12): Unit =
    val actualValues = actual.copyToCanonicalArray
    assertEquals(actualValues.length, expected.length)
    var i = 0
    while i < expected.length do
      assertEqualsDouble(actualValues(i), expected(i), tol)
      i += 1

  test("identity motion application is one-pass and preserves data") {
    val run = line(Vector(0.0, 1.0, 2.0, 3.0, 10.0, 11.0, 12.0, 13.0), nVolumes = 2)
    val trace = MotionTrace.identity(2).fold(err => fail(err.message), identity)
    val corrected = MotionApplier.apply(run, trace).fold(err => fail(err.message), identity)
    assertEquals(corrected.space, run.space)
    assertEquals(corrected.label, run.label)
    assertSameValues(corrected, run.copyToCanonicalArray.toVector)
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
        0.0, 100.0, 10.0, 110.0,
        1.0, 101.0, 11.0, 111.0,
        2.0, 102.0, 12.0, 112.0
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

    assertSameValues(timedCorrected, volumeCorrected.copyToCanonicalArray.toVector)
  }

  test("nonzero slice timing interpolates packet poses during final application") {
    val run = volume(
      Vector(
        0.0, 100.0, 10.0, 110.0,
        1.0, 101.0, 11.0, 111.0,
        2.0, 102.0, 12.0, 112.0
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

    assertEqualsDouble(volumeCorrected(0, 0, 1, 0), 10.0, 1e-12)
    assertEqualsDouble(timedCorrected(0, 0, 1, 0), 10.0, 1e-12)
    assertEqualsDouble(volumeCorrected(1, 0, 1, 0), 11.0, 1e-12)
    assertEqualsDouble(timedCorrected(1, 0, 1, 0), 10.0, 1e-12)
    assertEqualsDouble(volumeCorrected(2, 0, 1, 0), 12.0, 1e-12)
    assertEqualsDouble(timedCorrected(2, 0, 1, 0), 11.0, 1e-12)
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
