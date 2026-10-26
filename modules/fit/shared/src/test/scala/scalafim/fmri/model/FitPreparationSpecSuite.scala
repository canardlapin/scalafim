package scalafim.fmri.model

import gale.linalg.Matrix

class FitPreparationSpecSuite extends munit.FunSuite:
  test("typed preparation preserves normalized AR initialization and robust policy"):
    val ar = AutocorrelationConfig.unsafe(order = 1, initialization = Some(ArInitialization.ExactAr1))
    val gls = FitStrategy.GeneralizedLeastSquares(ar)
    assertEquals(gls.preparation.autocorrelation.initialization, None)
    assertEquals(gls.arConfig, Some(ar))
    val robust = FitStrategy.RobustLeastSquares(
      RobustConfig.unsafe(RobustOptions(RobustPsi.Huber())),
      autocorrelation = RobustAutocorrelation.Reestimate(ar)
    )
    assertEquals(robust.preparation.robust.reestimateAutocorrelation, true)

  test("withControls preserves all exact strategy variants"):
    val strategies = Vector[FitStrategy](
      FitStrategy.OrdinaryLeastSquares(),
      FitStrategy.RunwiseLeastSquares(),
      FitStrategy.SeparateRunsThenFixedEffects(),
      FitStrategy.GeneralizedLeastSquares(),
      FitStrategy.RunwiseGeneralizedLeastSquares(),
      FitStrategy.RobustLeastSquares(RobustConfig.unsafe(RobustOptions(RobustPsi.Huber()))),
      FitStrategy.LeastSquaresSeparate(),
      FitStrategy.LatentSketch(),
      FitStrategy.ReducedRankGls()
    )
    strategies.foreach: strategy =>
      val controls = strategy.fitControls.copy(missingData = MissingDataPolicy.ExcludeVoxel)
      assertEquals(strategy.withControls(controls).fitControls, controls)
      assertEquals(strategy.withControls(strategy.fitControls), strategy)
      assertEquals(strategy.withControls(controls).getClass, strategy.getClass)
      assertEquals(strategy.withControls(controls).coefficientScope, strategy.coefficientScope)

  test("typed nuisance fixed regularization rejects invalid values"):
    val matrix = NuisanceMatrix.unsafe(Matrix.dense(1, 1, Vector(1.0)))
    val invalid = ModelNuisanceProjection.MatrixProjection(matrix, Regularization.Fixed(Double.NaN))
    assert(invalid.validateFor(1).isLeft)
