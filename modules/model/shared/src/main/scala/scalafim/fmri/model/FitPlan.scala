package scalafim.fmri.model

enum FitEngine:
  case OrdinaryLeastSquares
  case LeastSquaresSeparate
  case RunwiseLeastSquares
  case GeneralizedLeastSquares
  case RobustLeastSquares
  case LatentSketch
  case ReducedRankGls

final class FitPlan private (
    val model: FmriModel,
    val strategy: FitStrategy
):
  def engine: FitEngine = strategy.engine
  def config: FitConfig = strategy.config

  def nTimepoints: Int = model.nTimepoints
  def nPredictors: Int = model.nPredictors
  def nVoxels: Int = model.dataset.shape.spatialSize

  def summary: FitSummary =
    FitSummary(
      engine = engine,
      timepoints = nTimepoints,
      predictors = nPredictors,
      voxels = nVoxels,
      robust = strategy match
        case FitStrategy.RobustLeastSquares(_, _) => true
        case _ => false,
      autocorrelated = strategy match
        case FitStrategy.GeneralizedLeastSquares(_, _) => true
        case FitStrategy.ReducedRankGls(_, _) => true
        case _ => false
    )

  def copy(
      model: FmriModel = this.model,
      strategy: FitStrategy = this.strategy
  ): FitPlan =
    FitPlan(model, strategy)

  override def equals(other: Any): Boolean =
    other match
      case that: FitPlan => model == that.model && strategy == that.strategy
      case _ => false

  override def hashCode(): Int =
    31 * model.hashCode() + strategy.hashCode()

  override def toString: String =
    s"FitPlan(model=$model, strategy=$strategy)"

object FitPlan:
  def make(
      model: FmriModel,
      strategy: FitStrategy = FitStrategy.Default
  ): Either[ModelError, FitPlan] =
    strategy.validateFor(model).map(_ => new FitPlan(model, strategy))

  def makeLegacy(
      model: FmriModel,
      engine: FitEngine,
      config: FitConfig = FitConfig()
  ): Either[ModelError, FitPlan] =
    FitStrategy.fromLegacy(engine, config).flatMap(make(model, _))

  def apply(model: FmriModel): FitPlan =
    apply(model, FitStrategy.Default)

  def apply(
      model: FmriModel,
      strategy: FitStrategy
  ): FitPlan =
    make(model, strategy).fold(error => throw new IllegalArgumentException(error.message), identity)

  def apply(
      model: FmriModel,
      engine: FitEngine
  ): FitPlan =
    apply(model, engine, FitConfig())

  def apply(
      model: FmriModel,
      engine: FitEngine,
      config: FitConfig
  ): FitPlan =
    makeLegacy(model, engine, config).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class FitSummary(
    engine: FitEngine,
    timepoints: Int,
    predictors: Int,
    voxels: Int,
    robust: Boolean,
    autocorrelated: Boolean
)
