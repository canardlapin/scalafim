package scalafim.fmri.motion

import scalafim.image.{Axis, NeuroSpace, NeuroVec, NArrayUtil}

class MotionQcSuite extends munit.FunSuite:

  private def run(values: Vector[Double]): NeuroVec[Double] =
    NeuroVec.fromLinear(
      NArrayUtil.tabulate[Double](values.length)(values),
      NeuroSpace(Vector(1, 1, 1)).addDim(values.length, Some(Axis.Time)),
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
    assertEquals(qc.fitFailure, Vector(false, true, false))
    assertEquals(qc.censorSuggest, Vector(false, true, false))
  }

  test("MotionQc validates trace length") {
    val x = run(Vector(0.0, 1.0))
    val trace = MotionTrace.identity(1).fold(err => fail(err.message), identity)
    assert(MotionQc.from(x, trace).isLeft)
  }
