package scalafim.dataset.zarr

import scalafim.archive.zarr.{AcquisitionTiming, TimeUnits}

class CanonicalBoldSamplingSuite extends munit.FunSuite:

  test("regular seconds preserve zero and nonzero origins") {
    val zero =
      refine(AcquisitionTiming.Regular(0.0, 0.8, 600L, TimeUnits.Second))
    assertEquals(zero.blockLens, Vector(600))
    assertEquals(zero.tr.map(_.value), Vector(0.8))
    assertEquals(zero.startTime.map(_.value), Vector(0.0))

    val nonzero =
      refine(AcquisitionTiming.Regular(0.4, 0.8, 600L, TimeUnits.Second))
    assertEquals(nonzero.startTime.map(_.value), Vector(0.4))
  }

  test("regular milliseconds normalize before validation") {
    val frame =
      refine(AcquisitionTiming.Regular(400.0, 800.0, 10L, TimeUnits.Millisecond))

    assertEquals(frame.tr.map(_.value), Vector(0.8))
    assertEquals(frame.startTime.map(_.value), Vector(0.4))
  }

  test("default precision remains valid for very short TRs") {
    val frame =
      refine(AcquisitionTiming.Regular(0.0, 0.001, 10L, TimeUnits.Second))

    assertEqualsDouble(frame.precision.value, 0.0001, 1e-15)
  }

  test("explicit timing, negative origin, overflow, and invalid precision are typed errors") {
    assert(CanonicalBoldSampling
      .refine(
        AcquisitionTiming.Explicit(Vector(0.0, 0.8), TimeUnits.Second),
        SamplingPrecisionPolicy.Default
      )
      .left
      .exists(_.message.contains("explicit NeuroArchive timing")))

    assert(CanonicalBoldSampling
      .refine(
        AcquisitionTiming.Regular(-0.1, 0.8, 10L, TimeUnits.Second),
        SamplingPrecisionPolicy.Default
      )
      .left
      .exists(_.message.contains("non-negative")))

    assert(CanonicalBoldSampling
      .refine(
        AcquisitionTiming.Regular(0.0, 0.8, Int.MaxValue.toLong + 1L, TimeUnits.Second),
        SamplingPrecisionPolicy.Default
      )
      .left
      .exists(_.message.contains("2147483647")))

    assert(CanonicalBoldSampling
      .refine(
        AcquisitionTiming.Regular(0.0, 0.8, 10L, TimeUnits.Second),
        SamplingPrecisionPolicy.Fixed(0.8)
      )
      .left
      .exists(_.message.contains("modeling precision")))
  }

  private def refine(timing: AcquisitionTiming) =
    CanonicalBoldSampling
      .refine(timing, SamplingPrecisionPolicy.Default)
      .fold(error => fail(error.message), identity)
