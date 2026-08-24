package scalafim.fmri.motion

import scalafim.image.*
import scalafim.image.SampleSpaces.*

class MotionQcSuite extends munit.FunSuite:

  private def run(values: Vector[Double]): SomeScalarSeries[Double] =
    SomeScalarSeries.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.tabulate[Double](values.length)(values),
      SampleSpaces(Vector(1, 1, 1)).addDim(ProviderAxes.time(values.length)),
      "qc-fixture"
    )

  test("MotionQc combines FD, DVARS, cost drop, and censor flags") {
    val x = run(Vector(0.0, 1.0, 3.0))
    val trace =
      MotionTrace.unsafe(
        Vector(
          RigidPose.identity,
          RigidPose.unsafe(0.6, 0.0, 0.0, 0.0, 0.0, 0.0),
          RigidPose.unsafe(0.6, 0.0, 0.0, 0.0, 0.0, 0.0)
        )
      )
    val qc =
      MotionQc.from(
        run = x,
        trace = trace,
        costInit = Some(Vector(1.0, 1.0, 1.0)),
        costFinal = Some(Vector(0.5, 1.2, 0.8))
      ).fold(err => fail(err.message), identity)

    assertEqualsDouble(qc.fd(1), 0.6, 1e-12)
    assert(qc.dvars.head.isNaN)
    assertEqualsDouble(qc.dvars(1), 1.0, 1e-12)
    assertEqualsDouble(qc.dvars(2), 2.0, 1e-12)
    assertEquals(qc.costDrop, Vector(0.5, -0.19999999999999996, 0.19999999999999996))
    assertEquals(qc.motionSpike, Vector(false, true, false))
    assertEquals(qc.dvarsSpike, Vector(false, false, false))
    assertEquals(qc.fitFailure, Vector(false, true, false))
    assertEquals(qc.censorSuggest, Vector(false, true, false))
    assertEquals(qc.fdPairs.length, 2)
    assertEquals(qc.dvarsPairs.head.pair, FramePair.unsafe(0, 1))
    assert(!qc.fitCostTrace.isMissing)
  }

  test("MotionQc records missing fit-cost traces explicitly") {
    val x = run(Vector(0.0, 1.0, 3.0))
    val trace = MotionTrace.identity(3).fold(err => fail(err.message), identity)
    val qc = MotionQc.from(x, trace).fold(err => fail(err.message), identity)

    assert(qc.fitCostTrace.isMissing)
    assert(qc.costDrop.forall(_.isNaN))
    assertEquals(qc.fitFailure, Vector(false, false, false))
  }

  test("MotionQc rejects incomplete fit-cost traces") {
    val x = run(Vector(0.0, 1.0))
    val trace = MotionTrace.identity(2).fold(err => fail(err.message), identity)
    MotionQc.from(x, trace, costInit = Some(Vector(1.0, 1.0))) match
      case Left(MotionError.IncompleteFitCostTrace("costFinal")) => ()
      case other => fail(s"expected incomplete fit-cost trace, got $other")
  }

  test("MotionQc records packet correction magnitudes when supplied") {
    val x = run(Vector(0.0, 1.0, 3.0))
    val trace = MotionTrace.identity(3).fold(err => fail(err.message), identity)
    val qc =
      MotionQc
        .from(x, trace, packetCorrectionMagnitude = Some(Vector(0.0, 0.25, 0.5)))
        .fold(err => fail(err.message), identity)

    assertEquals(qc.packetCorrectionMagnitude, Some(Vector(0.0, 0.25, 0.5)))
    assert(MotionQc.from(x, trace, packetCorrectionMagnitude = Some(Vector(0.0, 0.25))).isLeft)
    assert(MotionQc.from(x, trace, packetCorrectionMagnitude = Some(Vector(0.0, Double.NaN, 0.5))).isLeft)
  }

  test("MotionQc applies typed thresholds and censor policy") {
    val x = run(Vector(0.0, 1.0, 3.0))
    val trace =
      MotionTrace.unsafe(
        Vector(
          RigidPose.identity,
          RigidPose.unsafe(0.6, 0.0, 0.0, 0.0, 0.0, 0.0),
          RigidPose.unsafe(0.6, 0.0, 0.0, 0.0, 0.0, 0.0)
        )
      )
    val policy =
      MotionQcPolicy(
        thresholds = MotionQcThresholds.unsafe(
          fdSpikeMm = 0.7,
          dvarsSpikeRms = Some(1.5),
          fitCostIncreaseTolerance = 0.25
        ),
        dvarsPolicy = DvarsPolicy.Raw,
        censorPolicy = CensorPolicy.MotionFitOrDvars
      )
    val qc =
      MotionQc
        .from(
          run = x,
          trace = trace,
          costInit = Some(Vector(1.0, 1.0, 1.0)),
          costFinal = Some(Vector(0.5, 1.2, 0.8)),
          policy = policy
        )
        .fold(err => fail(err.message), identity)

    assertEquals(qc.motionSpike, Vector(false, false, false))
    assertEquals(qc.fitFailure, Vector(false, false, false))
    assertEquals(qc.dvarsSpike, Vector(false, false, true))
    assertEquals(qc.censorSuggest, Vector(false, false, true))
  }

  test("MotionQc validates trace length") {
    val x = run(Vector(0.0, 1.0))
    val trace = MotionTrace.identity(1).fold(err => fail(err.message), identity)
    assert(MotionQc.from(x, trace).isLeft)
  }
