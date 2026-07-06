package scalafim.fmri.model

enum FitEngine:
  case OrdinaryLeastSquares
  case LeastSquaresSeparate
  case RunwiseLeastSquares
  case GeneralizedLeastSquares
  case RobustLeastSquares
  case LatentSketch
  case ReducedRankGls

final case class FitPlan(
    model: FmriModel,
    engine: FitEngine = FitEngine.OrdinaryLeastSquares,
    config: FitConfig = FitConfig()
):
  def nTimepoints: Int = model.nTimepoints
  def nPredictors: Int = model.nPredictors
  def nVoxels: Int = model.dataset.shape.spatialSize

  def summary: FitSummary =
    FitSummary(
      engine = engine,
      timepoints = nTimepoints,
      predictors = nPredictors,
      voxels = nVoxels,
      robust = config.robust.psi != RobustPsi.Disabled,
      autocorrelated = config.autocorrelation.structure != ArStructure.Iid
    )

final case class FitSummary(
    engine: FitEngine,
    timepoints: Int,
    predictors: Int,
    voxels: Int,
    robust: Boolean,
    autocorrelated: Boolean
)
