package scalafim.connectivity.fixtures

object AriadneCoreFixtures:
  /** Contractual Ariadne semantics for ScalaFIM: numeric edge/matrix meaning and
    * vectorization order. R S3 classes, lists, and plotting surfaces are not
    * contractual.
    */
  val notes: Vector[String] =
    Vector(
      "conn_set upper-triangle order is preserved only through AriadneCompatible order",
      "conn_rect_set as.vector order is preserved only through AriadneCompatible order",
      "weighted_cor normalizes finite non-negative frame weights before centering",
      "cor_lw shrinks off-diagonal correlations toward the identity target",
      "R S3/list classes and plotting surfaces are not ScalaFIM contracts"
    )

  val weightedInputRows: Vector[Vector[Double]] =
    Vector(
      Vector(1.0, 2.0, 3.0),
      Vector(2.0, 1.0, 4.0),
      Vector(3.0, 4.0, 2.0),
      Vector(4.0, 3.0, 1.0),
      Vector(5.0, 5.0, 5.0)
    )

  val frameWeights: Vector[Double] =
    Vector(1.0, 2.0, 0.0, 3.0, 4.0)

  val weightedCorrelation: Vector[Vector[Double]] =
    Vector(
      Vector(1.0, 0.9041625741926010, 0.2176437222929923),
      Vector(0.9041625741926009, 1.0, 0.4128155653769297),
      Vector(0.2176437222929923, 0.4128155653769298, 1.0)
    )

  val diagonalShrinkageCorrelation: Vector[Vector[Double]] =
    Vector(
      Vector(1.0, 0.6635507871665350, 0.1597253274703743),
      Vector(0.6635507871665349, 1.0, 0.3029588938748859),
      Vector(0.1597253274703743, 0.3029588938748860, 1.0)
    )
