package scalafim.fmri.motion.fixtures

import scalafim.fmri.motion.RigidPose

object VolreggerFixtures:
  val translationAndYaw: RigidPose =
    RigidPose.unsafe(1.0, 2.0, 3.0, 0.0, 0.0, math.Pi / 2.0)

  val fdTrace: Vector[RigidPose] =
    Vector(
      RigidPose.identity,
      RigidPose.unsafe(0.5, 0.0, 0.0, 0.01, 0.0, 0.0),
      RigidPose.unsafe(0.5, -0.25, 0.0, 0.01, 0.02, 0.0)
    )

  val fdExpected: Vector[Double] =
    Vector(0.0, 1.0, 1.25)

  val dvarsRunValues: Vector[Double] =
    Vector(1.0, 2.0, 5.0)

  val dvarsExpected: Vector[Double] =
    Vector(Double.NaN, 1.0, 3.0)

  val robustDvarsRunValues: Vector[Double] =
    Vector(0.0, 1.0, 11.0, 14.0)

  val robustDvarsExpected: Vector[Double] =
    Vector(Double.NaN, 1.0, 9.0, 3.0)

  val estimatorDims: Vector[Int] =
    Vector(7, 5, 5)

  val estimatorTranslationX: Double =
    1.0

  val estimatorRotationZ: Double =
    0.18

  val estimatorCaptureRotationZ: Double =
    0.35
