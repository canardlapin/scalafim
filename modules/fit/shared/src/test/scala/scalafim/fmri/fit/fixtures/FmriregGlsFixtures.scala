package scalafim.fmri.fit.fixtures

import scalafim.fmri.fit.GaleTestSyntax.*

import gale.linalg.DMat

object FmriregGlsFixtures:
  val rho: Double = 0.35
  val exactFirst: Boolean = true
  val censoredTimepoints: Vector[Int] = Vector(2)
  val residualDegreesOfFreedom: Int = 6

  val design: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(-2.0, 1.0),
        Vector(-1.0, 1.0),
        Vector(0.0, 1.0),
        Vector(1.0, 1.0),
        Vector(2.0, 1.0),
        Vector(-1.0, 1.0),
        Vector(0.0, 1.0),
        Vector(1.0, 1.0)
      )
    )

  val response: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(1.2, -1.5),
        Vector(0.1, -0.2),
        Vector(1.7, 0.4),
        Vector(2.5, 1.8),
        Vector(4.9, 2.0),
        Vector(-0.4, 0.6),
        Vector(0.8, 1.1),
        Vector(2.2, 1.7)
      )
    )

  val whitenedDesign: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(-1.873499399519519, 0.9367496997597597),
        Vector(-0.3, 0.65),
        Vector(0.35, 0.65),
        Vector(0.9367496997597597, 0.9367496997597597),
        Vector(1.65, 0.65),
        Vector(-1.7, 0.65),
        Vector(0.35, 0.65),
        Vector(1.0, 0.65)
      )
    )

  val whitenedResponse: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(1.124099639711712, -1.40512454963964),
        Vector(-0.32, 0.325),
        Vector(1.665, 0.47),
        Vector(2.3418742493994, 1.686149459567568),
        Vector(4.025, 1.37),
        Vector(-2.115, -0.09999999999999998),
        Vector(0.9400000000000001, 0.8900000000000001),
        Vector(1.92, 1.315)
      )
    )

  val coefficients: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(1.169144243493604, 0.73542126157918),
        Vector(1.683333333333334, 0.7083333333333333)
      )
    )

  val residualVariance: Vector[Double] =
    Vector(1.202744422082536, 0.2115509078015482)

  val normalizedCovariance: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(0.08822232024702252, 1.407491535032746e-17),
        Vector(1.407491535032746e-17, 0.2331002331002331)
      )
    )

  val rss: Vector[Double] =
    Vector(7.216466532495218, 1.269305446809289)
