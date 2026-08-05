package scalafim.fmri.fit.fixtures

import gale.linalg.DMat

object ReducedRankGlsFmriregFixtures:
  val design: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(0.0, 1.0),
        Vector(1.0, 1.0),
        Vector(2.0, 1.0),
        Vector(3.0, 1.0)
      )
    )

  val response: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, 1.0),
        Vector(5.0, 0.0),
        Vector(7.0, -1.0)
      )
    )

  val rankOneCoefficients: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(2.0242950394631682, -0.05187092023486416),
        Vector(0.9481290797651353, -0.024295039463167797)
      )
    )

  val rankOneResidualVariance: Vector[Double] =
    Vector(0.0019516910050603785, 2.9724241192283034)

  val partitionedDesign: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(-2.0, 1.0, 1.0),
        Vector(-1.0, 0.0, 1.0),
        Vector(0.0, 1.0, 1.0),
        Vector(1.0, 0.0, 1.0),
        Vector(2.0, 1.0, 1.0),
        Vector(3.0, 0.0, 1.0)
      )
    )

  val partitionedResponse: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(-1.0, 1.5, 0.2),
        Vector(0.2, 1.0, -0.3),
        Vector(1.4, 0.2, 0.6),
        Vector(2.1, -0.4, 0.4),
        Vector(3.7, -1.2, 1.1),
        Vector(4.2, -1.6, 0.7)
      )
    )

  val partitionedRankOneCoefficients: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0915447077229612, -0.6657306061640562, 0.205559387688436),
        Vector(0.35552850667361396, -0.21683601833421076, 0.06695302686233233),
        Vector(1.0431300594683794, 0.35794997891580027, 0.31374379272461594)
      )
    )

  val partitionedRankOneResidualVariance: Vector[Double] =
    Vector(0.04574532589819886, 0.0049726557073170715, 0.14336927507943342)

  val partitionedTaskNormalizedCovariance: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(0.0625, 0.0625),
        Vector(0.0625, 0.729166666666667)
      )
    )

  val partitionedConditionalVariance: Vector[Double] =
    Vector(0.0231829962663243, 0.00862349976272567, 0.000822169078932675)

  val partitionedConditionalStandardErrors: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(0.0380649085989349, 0.0232157001869501, 0.00716837271863651),
        Vector(0.130016414774679, 0.0792967122709855, 0.0244846541066388)
      )
    )

  val partitionedTaskATStatistics: Vector[Double] =
    Vector(28.6758788579753, -28.6758788579753, 28.6758788579753)

  val partitionedTaskFStatistics: Vector[Double] =
    Vector(428.678353895757, 428.678353895757, 428.678353895757)

  val bootstrapReplicates: Int = 32
  val bootstrapBlockSize: Int = 2
  val bootstrapSeed: Int = 7

  val partitionedBootstrapStandardErrors: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(0.00503664668473836, 0.00627288757758244, 0.0211909495163694),
        Vector(0.118163263369014, 0.0730441448785384, 0.0210854527577203)
      )
    )

  val partitionedBootstrapTaskATStatistics: Vector[Double] =
    Vector(216.72052380217, -106.128254002701, 9.70033870023838)

  val partitionedBootstrapCovariance: Vector[DMat] =
    Vector(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(2.53678098268859e-05, -0.000251358678467534),
          Vector(-0.000251358678467534, 0.013962556810015)
        )
      ),
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(3.93491185609881e-05, 0.000215951410537452),
          Vector(0.000215951410537452, 0.00533544710103691)
        )
      ),
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(0.000449056341405317, 0.000217678449635068),
          Vector(0.000217678449635068, 0.000444596317998054)
        )
      )
    )
