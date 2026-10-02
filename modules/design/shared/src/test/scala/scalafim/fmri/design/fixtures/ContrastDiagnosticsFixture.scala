package scalafim.fmri.design.fixtures

object ContrastDiagnosticsFixture:
  /** Independent NumPy audit receipt, extracted from the durable model-studio
    * bundle's m2b/m3/m4b method dumps on 2026-10-02. These values are fixtures
    * for the general OLS contracts, not application-specific runtime defaults. */
  object DurableAudit:
    val dmsFixedEffectsSeOverSigma: Vector[Double] = Vector(
      0.16045351706916153,
      0.14210577265657767,
      0.2366842355592583,
      0.06783927395296015,
      0.4989567421909018
    )
    val fWorstDirectionSeOverSigma: Vector[Double] = Vector(
      0.053641418674500875,
      0.2449450735728347,
      0.7022252037959982
    )
    val dmsResidualDf: Vector[Int] = Vector(183, 183)
    val duplicateAliasResidualUpperBound: Double = 8e-13

  // [intercept, u, u, w], with u and w orthogonal and centred.
  val duplicateDesign: Array[Double] = Array(
    1.0, 0.5, 0.5, 0.5,
    1.0, -0.5, -0.5, -0.5,
    1.0, 0.5, 0.5, -0.5,
    1.0, -0.5, -0.5, 0.5,
    1.0, 0.0, 0.0, 0.0
  )

  // First run has task exactly equal to its nuisance column. The second has
  // one unit-norm task direction orthogonal to the intercept nuisance.
  val singularTaskRun: Array[Double] = Array(
    1.0, 1.0,
    0.0, 0.0,
    0.0, 0.0,
    0.0, 0.0
  )
  val informativeTaskRun: Array[Double] = Array(
    1.0, 0.0,
    0.0, 1.0,
    0.0, 0.0,
    0.0, 0.0
  )
