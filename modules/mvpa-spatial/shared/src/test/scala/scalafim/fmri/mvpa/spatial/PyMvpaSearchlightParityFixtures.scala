package scalafim.fmri.mvpa.spatial

import scalafim.fmri.mvpa.PatternMatrix

/** Constants generated without ScalaFIM by `tools/mvpa/generate_pymvpa_searchlight_reference.py`.
  */
object PyMvpaSearchlightParityFixtures:
  val sourceRevision: String =
    "f699189b5b7e7a1bcaaf6f0a19aa077d8879b422"

  val sourceVersion: String =
    "2.6.5.dev1"

  val jsonSha256: String =
    "07ebd4de37ae675895352c4763643c96b72b929c80c1e6027c8e7aea41645ee0"

  val tolerance: Double = 1e-12

  val labels: Vector[String] =
    Vector("zero", "zero", "zero", "one", "one", "one")

  private def value(sample: Int, feature: Int): Double =
    val target = if sample < 3 then 0 else 1
    val base =
      ((sample + 1) * 13 +
        (feature + 1) * 7 +
        ((sample + 1) * (feature + 3)) % 11) / 20.0
    val interaction = target * ((feature % 7) - 3) * 0.35
    base + interaction

  private def patterns(features: Int): PatternMatrix =
    PatternMatrix.fromRows:
      Vector.tabulate(6): sample =>
        Vector.tabulate(features): feature =>
          value(sample, feature)

  object Volume:
    val dims: Vector[Int] =
      Vector(4, 3, 2)

    val affineRowMajor: Vector[Double] =
      Vector(
        2.0, 1.0, 0.0, 10.0, 0.0, 3.0, 1.0, -7.0, 0.0, 0.0, 4.0, 5.0, 0.0, 0.0, 0.0, 1.0
      )

    val radiusMillimeters: Double =
      math.sqrt(10.0)

    val supportOrdinals: Vector[Int] =
      Vector(0, 1, 2, 3, 4, 5, 7, 8, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 22, 23)

    val centerOrdinals: Vector[Int] =
      Vector(0, 5, 8, 13, 18, 23)

    val neighborhoods: Vector[(Int, Vector[Int])] =
      Vector(
        0 -> Vector(0, 2),
        5 -> Vector(3, 5, 11),
        8 -> Vector(2, 4, 8, 10, 12, 14),
        13 -> Vector(7, 13, 15, 19),
        18 -> Vector(12, 14, 18, 20),
        23 -> Vector(23)
      )

    val localMeanContrast: Vector[Double] =
      Vector(
        1.2083333333333335, 2.2944444444444443, 1.7944444444444452, 1.9041666666666677, 2.220833333333333,
        1.6500000000000021
      )

    val patternMatrix: PatternMatrix =
      patterns(dims.product)

  object Surface:
    val vertices: Vector[Vector[Double]] =
      Vector(
        Vector(0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(2.0, 0.0, 0.0),
        Vector(0.1, 0.0, 0.1),
        Vector(0.0, 1.0, 0.0),
        Vector(1.0, 1.0, 0.0),
        Vector(2.0, 1.0, 0.0),
        Vector(0.1, 1.0, 0.1)
      )

    val faces: Vector[(Int, Int, Int)] =
      Vector(
        (0, 1, 4),
        (1, 5, 4),
        (1, 2, 5),
        (2, 6, 5),
        (2, 3, 6),
        (3, 7, 6)
      )

    val radius: Double =
      1.0

    val centerOrdinals: Vector[Int] =
      Vector(0, 2, 3, 7)

    val geodesicNeighborhoods: Vector[(Int, Vector[Int])] =
      Vector(
        0 -> Vector(0, 1, 4),
        2 -> Vector(1, 2, 6),
        3 -> Vector(3, 7),
        7 -> Vector(3, 7)
      )

    val chordNeighborhoods: Vector[(Int, Vector[Int])] =
      Vector(
        0 -> Vector(0, 1, 3, 4),
        2 -> Vector(1, 2, 6),
        3 -> Vector(0, 1, 3, 7),
        7 -> Vector(3, 4, 5, 7)
      )

    val localMeanContrast: Vector[Double] =
      Vector(
        1.4499999999999997,
        1.9333333333333331,
        1.341666666666666,
        1.341666666666666
      )

    val patternMatrix: PatternMatrix =
      patterns(vertices.length)
