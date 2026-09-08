package scalafim.fmri.mvpa

import gale.linalg.DMat

object CrossDomainClassificationReferenceFixtures:
  final case class MeasurementCase(
      measurementOrdinal: Int,
      label: String,
      featureOrdinals: Vector[Int],
      expectedClasses: Vector[String],
      expectedPredicted: Vector[String],
      expectedAccuracy: Double,
      expectedDecisionScores: DMat
  )

  object CorrelationCentroid:
    val sourceRows: DMat = GaleTestMatrix.fromRows(
      Vector(
        Vector(2, 1, -1, 0.0, 0.59999999999999998),
        Vector(-1.2, 2.1000000000000001, 0.80000000000000004, -0.5, 1),
        Vector(0.10000000000000001, -1, 2.2000000000000002, 1, -0.69999999999999996),
        Vector(2.2999999999999998, 0.90000000000000002, -0.80000000000000004, 0.20000000000000001, 0.40000000000000002),
        Vector(-0.90000000000000002, 2.3999999999999999, 1.1000000000000001, -0.40000000000000002, 0.80000000000000004),
        Vector(-0.10000000000000001, -0.80000000000000004, 2, 1.2, -0.5)
      )
    )
    val targetRows: DMat = GaleTestMatrix.fromRows(
      Vector(
        Vector(0.0, -1.3, 2.7999999999999998, 1.3999999999999999, -0.80000000000000004),
        Vector(2.7999999999999998, 1.3999999999999999, -1.3, 0.10000000000000001, 0.69999999999999996),
        Vector(-1.3999999999999999, 2.7999999999999998, 1.5, -0.69999999999999996, 1.1000000000000001),
        Vector(0.20000000000000001, -1.1000000000000001, 2.6000000000000001, 1.1000000000000001, -0.59999999999999998),
        Vector(-1.2, 2.6000000000000001, 1.2, -0.29999999999999999, 0.90000000000000002),
        Vector(2.6000000000000001, 1.1000000000000001, -1.2, 0.20000000000000001, 0.5)
      )
    )
    val sourceLabels: Vector[String] = Vector("a", "b", "c", "a", "b", "c")
    val targetLabels: Vector[String] = Vector("c", "a", "b", "c", "b", "a")
    val regionalCases: Vector[MeasurementCase] = Vector(
      MeasurementCase(
        measurementOrdinal = 101,
        label = "regional_front",
        featureOrdinals = Vector(0, 1, 2),
        expectedClasses = Vector("a", "b", "c"),
        expectedPredicted = Vector("c", "a", "b", "c", "b", "a"),
        expectedAccuracy = 1,
        expectedDecisionScores = GaleTestMatrix.fromRows(
          Vector(
            Vector(-0.75403987754090995, -0.19236493259538318, 0.999823374730756),
            Vector(0.99828169494515584, -0.44788880996560754, -0.80258581533987827),
            Vector(-0.57913574405687041, 0.99552549792428824, -0.080056038890832529),
            Vector(-0.72842034806994493, -0.22968027579858319, 0.9983774844833484),
            Vector(-0.52435153841720294, 0.99958185419693302, -0.14533995779328246),
            Vector(0.99999891599884339, -0.5007855455772583, -0.76530430151365092)
          )
        )
      ),
      MeasurementCase(
        measurementOrdinal = 102,
        label = "regional_back",
        featureOrdinals = Vector(2, 3, 4),
        expectedClasses = Vector("a", "b", "c"),
        expectedPredicted = Vector("c", "a", "b", "c", "b", "a"),
        expectedAccuracy = 1,
        expectedDecisionScores = GaleTestMatrix.fromRows(
          Vector(
            Vector(-0.93226700516501171, -0.095973576730020563, 0.99977998347346242),
            Vector(0.99987827886267366, -0.25557509561808311, -0.93030919217410502),
            Vector(-0.40233634341649577, 0.99021290851320043, 0.022921243413509994),
            Vector(-0.96143242388204653, -0.0045872977994996667, 0.99367724848473826),
            Vector(-0.41931393468876721, 0.98744238355544978, 0.041533350056064927),
            Vector(0.99339926779878285, -0.37926907430658729, -0.87463928567664939)
          )
        )
      )
    )
    val searchlightCases: Vector[MeasurementCase] = Vector(
      MeasurementCase(
        measurementOrdinal = 201,
        label = "sl_left",
        featureOrdinals = Vector(0, 1, 4),
        expectedClasses = Vector("a", "b", "c"),
        expectedPredicted = Vector("c", "a", "b", "c", "b", "a"),
        expectedAccuracy = 1,
        expectedDecisionScores = GaleTestMatrix.fromRows(
          Vector(
            Vector(0.79115676942363522, -0.99961179968986424, 0.9983374884595827),
            Vector(0.99778842338933726, -0.72999464103782785, 0.78571428571428559),
            Vector(-0.77692997190225521, 0.99998779023625861, -0.9967540405260592),
            Vector(0.79115676942363511, -0.99961179968986424, 0.9983374884595827),
            Vector(-0.7452718657831271, 0.99903695897526945, -0.99163606977944929),
            Vector(0.99990086740991735, -0.76481387545962942, 0.81705716910288306)
          )
        )
      ),
      MeasurementCase(
        measurementOrdinal = 202,
        label = "sl_mid",
        featureOrdinals = Vector(1, 2, 3),
        expectedClasses = Vector("a", "b", "c"),
        expectedPredicted = Vector("c", "a", "b", "c", "b", "a"),
        expectedAccuracy = 1,
        expectedDecisionScores = GaleTestMatrix.fromRows(
          Vector(
            Vector(-0.97415755813652982, -0.63133713920942613, 0.99995888524184018),
            Vector(0.99967748942006207, 0.46252285191956122, -0.9777159305474028),
            Vector(0.32350920056794819, 0.99207188360423604, -0.53654659215204892),
            Vector(-0.98792257547484508, -0.57369976879070894, 0.99669466111277971),
            Vector(0.44118144424992467, 0.99999891353730708, -0.63947744401477336),
            Vector(0.99695330542194038, 0.36846811863620493, -0.95080144928311938)
          )
        )
      ),
      MeasurementCase(
        measurementOrdinal = 203,
        label = "sl_right",
        featureOrdinals = Vector(0, 3, 4),
        expectedClasses = Vector("a", "b", "c"),
        expectedPredicted = Vector("c", "a", "b", "c", "b", "a"),
        expectedAccuracy = 1,
        expectedDecisionScores = GaleTestMatrix.fromRows(
          Vector(
            Vector(-0.33469550404642934, -0.55297795873564537, 0.99992766988526205),
            Vector(0.99960493149287377, -0.57734217029053114, -0.37223873512210709),
            Vector(-0.57558592270248621, 0.99954221293364387, -0.56807608936492027),
            Vector(-0.21730626876840262, -0.65043902570750978, 0.99100123373724824),
            Vector(-0.70313722211069829, 0.99073640606140456, -0.42384536275427098),
            Vector(0.99754089796264644, -0.65465367070797709, -0.27939632824530852)
          )
        )
      )
    )
