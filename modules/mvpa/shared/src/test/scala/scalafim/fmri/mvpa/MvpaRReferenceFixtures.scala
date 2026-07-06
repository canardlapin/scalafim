package scalafim.fmri.mvpa

import scalafim.linalg.DoubleMatrix

object MvpaRReferenceFixtures:
  final case class NaiveXdecCase(
      roiId: Int,
      label: String,
      featureIndices: Vector[Int],
      expectedClasses: Vector[String],
      expectedPredicted: Vector[String],
      expectedAccuracy: Double,
      expectedProbabilities: DoubleMatrix
  )

  object FeatureRsa:
    val items: Vector[String] =
      Vector("item_0", "item_1", "item_2", "item_3", "item_4", "item_5")

    val featureNames: Vector[String] =
      Vector("shape", "color", "motion")

    val lambda: Double =
      0.75

    val featureRows: DoubleMatrix =
      DoubleMatrix.fromRows(
        Vector(
          Vector(0.0, 1.0, 0.2),
          Vector(1.0, 0.0, -0.1),
          Vector(0.5, 1.5, 0.7),
          Vector(2.0, 0.2, 0.4),
          Vector(1.2, 2.4, -0.3),
          Vector(2.5, 1.1, 1.0)
        )
      )

    val patternRows: DoubleMatrix =
      DoubleMatrix.fromRows(
        Vector(
          Vector(0.76, 0.82, 0.98, 1.95),
          Vector(2.67, -0.27, -0.62, 2.34),
          Vector(1.79, 1.29, 1.14, 2.66),
          Vector(4.71, -0.08, -1.19, 3.45),
          Vector(1.8, 3.33, 0.53, 1.5),
          Vector(5.64, 0.93, -0.84, 4.16)
        )
      )

    val searchlightPatternIndices: Vector[Int] =
      Vector(0, 2, 3)

    object EncodeRegional:
      val predicted: DoubleMatrix =
        DoubleMatrix.fromRows(
          Vector(
            Vector(1.3654728019231466, 1.2239737378960658, 0.8868764273637605, 2.13063836654292),
            Vector(3.016896591222865, 0.38815249604505586, -0.4227203076382656, 2.603839223575439),
            Vector(2.2770376687235117, 1.2852714793967881, 0.5632339052767827, 2.6334197467166232),
            Vector(4.255314360103228, 0.1460622859096521, -0.8387269834038726, 3.2901963635567824),
            Vector(1.1064511905129211, 0.7508330615156926, 0.8389147010541362, 2.0736324483912805),
            Vector(4.90373975435582, 0.6678318787161285, -0.5291292502585163, 3.8972090243414463)
          )
        )

      val observed: DoubleMatrix =
        DoubleMatrix.fromRows(
          Vector(
            Vector(0.76, 0.82, 0.98, 1.95),
            Vector(2.67, -0.27, -0.62, 2.34),
            Vector(1.79, 1.29, 1.14, 2.66),
            Vector(4.71, -0.08, -1.19, 3.45),
            Vector(1.8, 3.33, 0.53, 1.5),
            Vector(5.64, 0.93, -0.84, 4.16)
          )
        )

      val metrics: MetricVector =
        MetricVector(
          "PatternCorrelation" -> 0.751714402488899,
          "PatternDiscrimination" -> 0.09070978621631454,
          "PatternRankPercentile" -> 0.6,
          "RdmCorrelation" -> 0.6785714285714286,
          "TargetCorrelation" -> 0.9239785936604978,
          "Mse" -> 0.43935829205598487,
          "RSquared" -> 0.8504879759744588,
          "MeanTargetwiseCorrelation" -> 0.8261347296932826,
          "Observations" -> 6.0,
          "TargetColumns" -> 4.0,
          "RidgeLambda" -> 0.75
        )

    object DecodeRegional:
      val predicted: DoubleMatrix =
        DoubleMatrix.fromRows(
          Vector(
            Vector(0.45228628473213517, 1.3938333643070311, 0.27220499552721644),
            Vector(1.2874509708674042, 0.6213412817134125, 0.27037367160358944),
            Vector(0.6130754120120836, 1.3903949992963147, 0.35224704561602427),
            Vector(1.9440403769036676, 0.3610062971727266, 0.5231392236281435),
            Vector(-0.23708540620011376, 2.022765984703965, 0.778663093469407),
            Vector(2.1770386835136444, 0.851259478734854, 0.8874595954982221)
          )
        )

      val observed: DoubleMatrix =
        DoubleMatrix.fromRows(
          Vector(
            Vector(0.0, 1.0, 0.2),
            Vector(1.0, 0.0, -0.1),
            Vector(0.5, 1.5, 0.7),
            Vector(2.0, 0.2, 0.4),
            Vector(1.2, 2.4, -0.3),
            Vector(2.5, 1.1, 1.0)
          )
        )

      val metrics: MetricVector =
        MetricVector(
          "PatternCorrelation" -> 0.8844519613446294,
          "PatternDiscrimination" -> 0.7499815971210615,
          "PatternRankPercentile" -> 0.8666666666666667,
          "RdmCorrelation" -> 0.6881764634233478,
          "TargetCorrelation" -> 0.7808188877889917,
          "Mse" -> 0.2616986280415614,
          "RSquared" -> 0.6069607588862658,
          "MeanTargetwiseCorrelation" -> 0.6352744424416389,
          "Observations" -> 6.0,
          "TargetColumns" -> 3.0,
          "RidgeLambda" -> 0.75
        )

    object EncodeSearchlight:
      val predicted: DoubleMatrix =
        DoubleMatrix.fromRows(
          Vector(
            Vector(1.3654728019231466, 0.8868764273637605, 2.13063836654292),
            Vector(3.016896591222865, -0.4227203076382656, 2.603839223575439),
            Vector(2.2770376687235117, 0.5632339052767827, 2.6334197467166232),
            Vector(4.255314360103228, -0.8387269834038726, 3.2901963635567824),
            Vector(1.1064511905129211, 0.8389147010541362, 2.0736324483912805),
            Vector(4.90373975435582, -0.5291292502585163, 3.8972090243414463)
          )
        )

      val observed: DoubleMatrix =
        DoubleMatrix.fromRows(
          Vector(
            Vector(0.76, 0.98, 1.95),
            Vector(2.67, -0.62, 2.34),
            Vector(1.79, 1.14, 2.66),
            Vector(4.71, -1.19, 3.45),
            Vector(1.8, 0.53, 1.5),
            Vector(5.64, -0.84, 4.16)
          )
        )

      val metrics: MetricVector =
        MetricVector(
          "PatternCorrelation" -> 0.8712010757780226,
          "PatternDiscrimination" -> 0.10075223932575061,
          "PatternRankPercentile" -> 0.6333333333333333,
          "RdmCorrelation" -> 0.03214285714285714,
          "TargetCorrelation" -> 0.9767339338090325,
          "Mse" -> 0.17645993244320896,
          "RSquared" -> 0.9460881577469456,
          "MeanTargetwiseCorrelation" -> 0.9650346313490276,
          "Observations" -> 6.0,
          "TargetColumns" -> 3.0,
          "RidgeLambda" -> 0.75
        )

  object NaiveXdec:
    val sourceRows: DoubleMatrix =
      DoubleMatrix.fromRows(
        Vector(
          Vector(2.0, 1.0, -1.0, 0.0, 0.6),
          Vector(-1.2, 2.1, 0.8, -0.5, 1.0),
          Vector(0.1, -1.0, 2.2, 1.0, -0.7),
          Vector(2.3, 0.9, -0.8, 0.2, 0.4),
          Vector(-0.9, 2.4, 1.1, -0.4, 0.8),
          Vector(-0.1, -0.8, 2.0, 1.2, -0.5)
        )
      )

    val targetRows: DoubleMatrix =
      DoubleMatrix.fromRows(
        Vector(
          Vector(0.0, -1.3, 2.8, 1.4, -0.8),
          Vector(2.8, 1.4, -1.3, 0.1, 0.7),
          Vector(-1.4, 2.8, 1.5, -0.7, 1.1),
          Vector(0.2, -1.1, 2.6, 1.1, -0.6),
          Vector(-1.2, 2.6, 1.2, -0.3, 0.9),
          Vector(2.6, 1.1, -1.2, 0.2, 0.5)
        )
      )

    val sourceLabels: Vector[String] =
      Vector("a", "b", "c", "a", "b", "c")

    val targetLabels: Vector[String] =
      Vector("c", "a", "b", "c", "b", "a")

    val regionalCases: Vector[NaiveXdecCase] =
      Vector(
        NaiveXdecCase(
          roiId = 101,
          label = "regional_front",
          featureIndices = Vector(0, 1, 2),
          expectedClasses = Vector("a", "b", "c"),
          expectedPredicted = Vector("c", "a", "b", "c", "b", "a"),
          expectedAccuracy = 1.0,
          expectedProbabilities = DoubleMatrix.fromRows(
            Vector(
              Vector(0.11722663730035712, 0.2055694795891601, 0.6772038831104829),
              Vector(0.7139665467777573, 0.1681179141383179, 0.11791553908392469),
              Vector(0.13375582157283064, 0.6459209659634877, 0.2203232124636816),
              Vector(0.12092972044663232, 0.1991283569054043, 0.6799419226479633),
              Vector(0.141822146686901, 0.6509983952007941, 0.20717945811230498),
              Vector(0.717313762481801, 0.15992882745781736, 0.1227574100603816)
            )
          )
        ),
        NaiveXdecCase(
          roiId = 102,
          label = "regional_back",
          featureIndices = Vector(2, 3, 4),
          expectedClasses = Vector("a", "b", "c"),
          expectedPredicted = Vector("c", "a", "b", "c", "b", "a"),
          expectedAccuracy = 1.0,
          expectedProbabilities = DoubleMatrix.fromRows(
            Vector(
              Vector(0.09792952958434892, 0.2260014833090925, 0.6760689871065586),
              Vector(0.6992676325702212, 0.19925395635059912, 0.10147841107917988),
              Vector(0.15255338252855177, 0.6140423109357394, 0.23340430653570887),
              Vector(0.09373684461685719, 0.24404105198945203, 0.6622221033936908),
              Vector(0.14996758447239592, 0.6122713003258309, 0.2377611152017732),
              Vector(0.7102998752197895, 0.18001116886690738, 0.10968895591330294)
            )
          )
        )
      )

    val searchlightCases: Vector[NaiveXdecCase] =
      Vector(
        NaiveXdecCase(
          roiId = 201,
          label = "sl_left",
          featureIndices = Vector(0, 1, 4),
          expectedClasses = Vector("a", "b", "c"),
          expectedPredicted = Vector("c", "a", "b", "c", "b", "a"),
          expectedAccuracy = 1.0,
          expectedProbabilities = DoubleMatrix.fromRows(
            Vector(
              Vector(0.4171817519945964, 0.06959922371999121, 0.5132190242854124),
              Vector(0.5033770055451968, 0.089438956198967, 0.4071840382558362),
              Vector(0.12962993859547803, 0.7663212950704329, 0.10404876633408916),
              Vector(0.41718175199459634, 0.06959922371999121, 0.5132190242854124),
              Vector(0.13326967060630246, 0.7625617575578215, 0.10416857183587616),
              Vector(0.49896864312017075, 0.0854412078103411, 0.415590149069488)
            )
          )
        ),
        NaiveXdecCase(
          roiId = 202,
          label = "sl_mid",
          featureIndices = Vector(1, 2, 3),
          expectedClasses = Vector("a", "b", "c"),
          expectedPredicted = Vector("c", "a", "b", "c", "b", "a"),
          expectedAccuracy = 1.0,
          expectedProbabilities = DoubleMatrix.fromRows(
            Vector(
              Vector(0.10406725280176837, 0.14662198888930567, 0.7493107583089259),
              Vector(0.5804375125742458, 0.3392127574777426, 0.08034972994801153),
              Vector(0.2963341433520461, 0.5782754795506185, 0.12539037709733547),
              Vector(0.10215072211563471, 0.15457388422716728, 0.7432753936571981),
              Vector(0.3238367825828405, 0.5662621314097471, 0.10990108600741236),
              Vector(0.596661181833248, 0.31825859077916413, 0.0850802273875877)
            )
          )
        ),
        NaiveXdecCase(
          roiId = 203,
          label = "sl_right",
          featureIndices = Vector(0, 3, 4),
          expectedClasses = Vector("a", "b", "c"),
          expectedPredicted = Vector("c", "a", "b", "c", "b", "a"),
          expectedAccuracy = 1.0,
          expectedProbabilities = DoubleMatrix.fromRows(
            Vector(
              Vector(0.17849293555756526, 0.14349017544226908, 0.6780168890001657),
              Vector(0.6848171598823188, 0.14148656717754446, 0.17369627294013668),
              Vector(0.14622237240846592, 0.706453015899698, 0.14732461169183614),
              Vector(0.2001485869007281, 0.1297912341236448, 0.6700601789756271),
              Vector(0.12882103059508598, 0.700852681052237, 0.17032628835267707),
              Vector(0.6800319811049913, 0.13031378325048312, 0.1896542356445256)
            )
          )
        )
      )
