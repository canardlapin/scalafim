package scalafim.fmri.mvpa.fit

/** Values emitted by
  * `tools/r-parity/generate_signed_cross_run_rayleigh_fixtures.R` using only
  * dense base-R products, `solve`, and `eigen`.
  */
object SignedCrossRunRayleighReferenceFixtures:
  val design: Vector[Vector[Double]] = Vector(
    Vector(1.0, -1.0, -1.0),
    Vector(1.0, -1.0, -0.7),
    Vector(1.0, 1.0, -0.4),
    Vector(1.0, 1.0, -0.1),
    Vector(1.0, -1.0, 0.1),
    Vector(1.0, -1.0, 0.4),
    Vector(1.0, 1.0, 0.7),
    Vector(1.0, 1.0, 1.0)
  )

  private val baseResponse: Vector[Vector[Double]] = Vector(
    Vector(-0.9, 0.1, -0.15, -0.2),
    Vector(-1.14, 0.5, -0.09, -0.5),
    Vector(0.91, -0.85, 0.21, 0.5),
    Vector(0.67, -0.55, 0.47, 0.2),
    Vector(-1.02, 0.75, -0.47, -0.1),
    Vector(-0.56, 0.65, -0.56, -0.4),
    Vector(0.99, -0.2, 0.49, 0.6),
    Vector(1.05, -0.4, 0.1, 0.3)
  )

  val responses: Vector[Vector[Vector[Double]]] =
    Vector.tabulate(3): run =>
      baseResponse.zipWithIndex.map: (row, time) =>
        row.zipWithIndex.map: (value, feature) =>
          value + 0.03 * run.toDouble * ((time + feature) % 3 - 1).toDouble

  val numerators: Vector[Double] = Vector(
    44.827071413711394,
    49.464895255976160,
    59.345143799340228
  )

  val denominators: Vector[Double] = Vector(
    1.00000000000000022,
    1.00000000000000000,
    0.99999999999999978
  )

  val statistics: Vector[Double] = Vector(
    44.827071413711387,
    49.464895255976160,
    59.345143799340242
  )

  val ridgeAmounts: Vector[Double] = Vector(
    0.019927673076923128,
    0.019041750000000024,
    0.017741134615384645
  )

  val directions: Vector[Vector[Double]] = Vector(
    Vector(0.56219170617404057, -1.2185480977666330, 1.3635296930516532, 0.42837192250117234),
    Vector(0.56818516494885818, -1.3282093346404058, 1.4204033608695501, 0.41746141694427075),
    Vector(-0.43692055608417513, 1.6852612877628246, -1.5855305007490343, -0.44687884789775284)
  )

  val meanStatistic: Double = 51.212370156342594
