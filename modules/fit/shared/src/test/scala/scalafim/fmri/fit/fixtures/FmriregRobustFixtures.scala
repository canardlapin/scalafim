package scalafim.fmri.fit.fixtures

import scalafim.linalg.DoubleMatrix

object FmriregRobustFixtures:
  val design: DoubleMatrix =
    DoubleMatrix.fromRows(
      (0 until 8).map(i => Vector(i.toDouble, 1.0)).toVector
    )

  val volumeSpikeResponse: DoubleMatrix =
    DoubleMatrix.fromRows(
      (0 until 8).map { i =>
        val x = i.toDouble
        val base = Vector(
          1.0 + 2.0 * x,
          5.0 - 1.5 * x,
          -2.0 + 0.75 * x
        )
        if i == 7 then
          Vector(base(0) + 30.0, base(1) - 28.0, base(2) + 26.0)
        else base
      }.toVector
    )

  val huberRunCoefficients: DoubleMatrix =
    DoubleMatrix.fromRows(
      Vector(
        Vector(2.4220066, -1.8938728, 1.1157391),
        Vector(0.1561713, 5.7875734, -2.7313182)
      )
    )

  val huberRunWeights: Vector[Double] =
    Vector(
      1.0,
      1.0,
      1.0,
      1.0,
      1.0,
      1.0,
      0.9969395,
      0.1057796
    )

  val huberRunScaleComponents: Vector[Double] =
    Vector(1.944244, 1.944244, 1.944244)

  val huberVoxelScaleComponents: Vector[Double] =
    Vector(2.083118, 1.944244, 1.805369)
