package scalafim.fmri.mvpa

import gale.linalg.DMat

object MvpaParityFixtures:
  object Rdm:
    val patterns: DMat =
      GaleTestMatrix.fromRows(
        Vector(
          Vector(0.0, 0.0),
          Vector(3.0, 4.0),
          Vector(1.0, 1.0)
        )
      )

    val squaredEuclidean: Vector[Double] =
      Vector(25.0, 2.0, 13.0)

    val squaredEuclideanNormalized: Vector[Double] =
      Vector(12.5, 1.0, 6.5)

    val euclidean: Vector[Double] =
      Vector(5.0, math.sqrt(2.0), math.sqrt(13.0))

  object Correlation:
    val patterns: DMat =
      GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0, 3.0),
          Vector(1.0, 2.0, 3.0),
          Vector(3.0, 2.0, 1.0)
        )
      )

    val distances: Vector[Double] =
      Vector(0.0, 2.0, 2.0)

  object Crossnobis:
    val means: PartitionMeans =
      PartitionMeans.unsafe(
        conditions = 2,
        features = 2,
        folds = 2,
        data = Array(
          1.0, 0.0,
          3.0, 0.0,
          1.0, 0.0,
          5.0, 0.0
        )
      )

    val rawDistance: Double =
      8.0

    val normalizedDistance: Double =
      4.0

  object Rsa:
    val items: Vector[String] =
      Vector("a", "b", "c", "d")

    val observed: RdmVector =
      RdmVector.unsafe(4, Vector(3.0, 3.0, 6.0, 8.0, 9.0, 13.0))

    val target: RdmVector =
      RdmVector.unsafe(4, Vector(1.0, -10.0, -9.0, -12.0, -19.0, -14.0))

    val trendControlReversed: RdmModel =
      RdmModel.unsafe(
        "trend",
        Vector("d", "c", "b", "a"),
        RdmVector.unsafe(4, Vector(6.0, 5.0, 3.0, 4.0, 2.0, 1.0))
      )

  object Classifiers:
    val centroidPatterns: PatternMatrix =
      PatternMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(2.0, 0.0),
          Vector(0.0, 2.0)
        )
      )

    val centroidResponse: Response =
      Response.categorical(Vector("a", "b", "a", "b")).toOption.get

    val centroidHighProbability: Double =
      1.0 / (1.0 + math.exp(-2.0))

    val swiftPatterns: PatternMatrix =
      PatternMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(1.0, 0.0),
          Vector(0.0, 1.0)
        )
      )

    val swiftResponse: Response =
      Response.categorical(Vector("a", "b", "a", "b")).toOption.get

    val swiftHighProbability: Double =
      1.0 / (1.0 + math.exp(-1.0))

    val ridgePatterns: PatternMatrix =
      PatternMatrix.fromRows(
        Vector(
          Vector(0.0),
          Vector(2.0),
          Vector(4.0),
          Vector(6.0)
        )
      )

    val ridgeResponse: Response =
      Response.categorical(Vector("a", "a", "b", "b")).toOption.get

    val ridgeHighProbability: Double =
      1.0 / (1.0 + math.exp(-2.4))
