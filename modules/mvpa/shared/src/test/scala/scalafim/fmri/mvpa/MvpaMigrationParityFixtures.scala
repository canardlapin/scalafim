package scalafim.fmri.mvpa

import gale.linalg.DMat

/** Constants generated independently by `tools/mvpa/generate_migration_parity.R`.
  */
object MvpaMigrationParityFixtures:
  val sourceRevision: String =
    "528c302e454697055bc9af31c9a6eca684f019e3"

  val jsonSha256: String =
    "053e50710a241e6eacee4e532bf0d398d02a84536bacf5bf5db6ca444144d04d"

  val tolerance: Double = 1e-12

  object SwiftFit:
    val trainingPatterns: PatternMatrix =
      PatternMatrix.fromRows(
        Vector(
          Vector(2.0, 0.0, 10.0),
          Vector(0.0, 2.0, 8.0),
          Vector(3.0, 1.0, 9.0),
          Vector(-1.0, 4.0, 7.0),
          Vector(1.0, 3.0, 11.0),
          Vector(4.0, -2.0, 6.0),
          Vector(0.0, 5.0, 12.0)
        )
      )

    val trainingResponse: Response =
      Response
        .categorical(Vector("zeta", "alpha", "zeta", "beta", "alpha", "zeta", "beta"))
        .toOption
        .get

    val testPatterns: PatternMatrix =
      PatternMatrix.fromRows(
        Vector(
          Vector(2.0, 1.0, 8.5),
          Vector(-0.5, 4.5, 11.0),
          Vector(3.5, -1.0, 6.5),
          Vector(1.0, 2.5, 10.0)
        )
      )

    val classes: Vector[String] =
      Vector("zeta", "alpha", "beta")

    val counts: Vector[Int] =
      Vector(3, 2, 2)

    val priors: Vector[Double] =
      Vector(
        0.42857142857142855,
        0.2857142857142857,
        0.2857142857142857
      )

    val zscoreCenter: Vector[Double] =
      Vector(
        1.2857142857142858,
        1.8571428571428572,
        9.0
      )

    val zscoreSampleSd: Vector[Double] =
      Vector(
        1.7994708216848745,
        2.410295378065479,
        2.1602468994692869
      )

    val scaledCentroids: DMat =
      GaleTestMatrix.fromRows(
        Vector(
          Vector(0.95266102324493385, -0.90879989664763949, -0.30860669992418382),
          Vector(-0.43663630232059469, 0.26671301314658985, 0.23145502494313785),
          Vector(-0.99235523254680591, 1.0964868318248695, 0.23145502494313785)
        )
      )

    val scaledTest: DMat =
      GaleTestMatrix.fromRows(
        Vector(
          Vector(0.39694209301872235, -0.35561735086211982, -0.23145502494313785),
          Vector(-0.99235523254680602, 1.0964868318248693, 0.92582009977255142),
          Vector(1.2305204883580396, -1.1853911695403994, -1.1572751247156892),
          Vector(-0.158776837207489, 0.26671301314658985, 0.46291004988627571)
        )
      )

    val scores: DMat =
      GaleTestMatrix.fromRows(
        Vector(
          Vector(-0.98889220304229875, -1.7321807602022148, -3.210483556730662),
          Vector(-3.9892366030974031, -0.47041088831903521, 0.028263171468814186),
          Vector(0.84503809038654887, -2.5317485381330593, -5.1618112189430878),
          Vector(-2.2981634525256984, -1.1628358091070226, -1.8159216336007544)
        )
      )

    val probabilities: DMat =
      GaleTestMatrix.fromRows(
        Vector(
          Vector(0.63131955332372924, 0.3002224202666115, 0.068458026409659312),
          Vector(0.01107336402471612, 0.37366822393903626, 0.61525841203624765),
          Vector(0.96467470222404206, 0.032950430293574355, 0.0023748674823836006),
          Vector(0.17446238828428492, 0.5429606350004802, 0.28257697671523491)
        )
      )

    val predicted: Vector[String] =
      Vector("zeta", "beta", "zeta", "alpha")

  object SwiftCrossValidation:
    val patterns: PatternMatrix =
      PatternMatrix.fromRows(
        Vector(
          Vector(2.0, 1.0, 0.0),
          Vector(-2.0, -1.0, 0.0),
          Vector(-1.5, -1.0, 1.0),
          Vector(1.5, 1.0, -1.0),
          Vector(0.8, 0.5, 0.0),
          Vector(2.2, 0.7, 1.0),
          Vector(-2.0, -0.4, -1.0),
          Vector(-0.7, -0.2, 0.5),
          Vector(-1.8, -1.2, 0.0)
        )
      )

    val labels: Vector[String] =
      Vector("b", "a", "a", "b", "a", "b", "a", "b", "a")

    val response: Response =
      Response.categorical(labels).toOption.get

    val blocks: Vector[Int] =
      Vector(0, 0, 1, 1, 1, 2, 2, 2, 2)

    val classes: Vector[String] =
      Vector("b", "a")

    val probabilities: DMat =
      GaleTestMatrix.fromRows(
        Vector(
          Vector(0.93360327508833352, 0.066396724911666483),
          Vector(0.041840832422621453, 0.95815916757737851),
          Vector(0.36120276152878011, 0.63879723847121994),
          Vector(0.81713721780146087, 0.1828627821985391),
          Vector(0.87686005915306098, 0.12313994084693908),
          Vector(0.46949030787242568, 0.53050969212757426),
          Vector(0.14408805126964574, 0.85591194873035437),
          Vector(0.050354240185370026, 0.94964575981463006),
          Vector(0.011793534313393831, 0.98820646568660619)
        )
      )

    val predicted: Vector[String] =
      Vector("b", "a", "a", "b", "b", "a", "a", "a", "a")

    val correct: Int = 6
    val testedSamples: Int = 9
    val pooledAccuracy: Double = 2.0 / 3.0
    val unweightedMeanFoldAccuracy: Double = 13.0 / 18.0

  object IdentityMetricRdm:
    val means: PartitionMeans =
      PartitionMeans.unsafe(
        conditions = 3,
        features = 3,
        folds = 3,
        data = Array(
          0.0, 0.0, 0.0, -1.0, 0.0, -1.0, 0.0, -2.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, -1.0, 0.0, -1.0, -1.0, 0.0, 0.0, 0.0,
          -0.5, 0.0, 1.0, 0.0, -3.0, 1.0
        )
      )

    val conditionNames: Vector[String] =
      Vector("a", "b", "c")

    val pairOrder: Vector[(String, String)] =
      Vector(("b", "a"), ("c", "a"), ("c", "b"))

    val orderedPartitionPairs: Vector[(Int, Int)] =
      Vector((0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1))

    val raw: Vector[Double] =
      Vector(-2.0 / 3.0, 10.0 / 3.0, 10.0 / 3.0)

    val featureNormalized: Vector[Double] =
      Vector(-2.0 / 9.0, 10.0 / 9.0, 10.0 / 9.0)
