package scalafim.fmri.model

import gale.linalg.DMat
import scalafim.fmri.design.TermId

import scala.util.control.NonFatal

enum MissingDataPolicy:
  /** Reject the selected response when any value is NA, NaN, or infinite. */
  case Error

  /** Exclude a response column as a whole when any selected value is NA, NaN,
    * or infinite. Healthy columns remain fit together in one finite block.
    */
  case ExcludeVoxel

  /** Fit each response column on its own finite selected rows. Voxels sharing
    * the same observed-row mask are grouped into one numerical block; result
    * inference remains partitioned by observation pattern.
    */
  case OmitRowsPerVoxel

  /** Compatibility alias for the original whole-voxel exclusion behavior.
    * New code should use [[ExcludeVoxel]].
    */
  case Propagate

enum ScaleScope:
  case Run, Global, Voxel

enum RobustPsi:
  case Disabled
  case Huber(k: Double = 1.345)
  case Bisquare(c: Double = 4.685)

enum ArStructure:
  case Iid
  case Ar(order: Int)

/** How a fixed temporal weight vector is aligned to a fit selection. */
enum FixedWeightAlignment:
  /** The vector is indexed by the model's full zero-based timepoint axis. */
  case FullSeries
  /** The vector is already aligned, in order, to the selected response rows. */
  case SelectedRows

/** The population over which DVARS and the resulting weights are normalized. */
enum DvarsWeightScope:
  /** Normalize independently within each run, never across a run boundary. */
  case WithinRun
  /** Normalize once over the selected rows, while still resetting derivatives at run and censor gaps. */
  case AcrossSelection

final class VolumeWeightThreshold private (val value: Double):
  require(value > 0.0 && value.isFinite, "volume-weight threshold must be positive and finite")

  override def equals(other: Any): Boolean =
    other match
      case that: VolumeWeightThreshold => value == that.value
      case _                           => false

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString

object VolumeWeightThreshold:
  val Default: VolumeWeightThreshold = unsafe(1.5)

  def apply(value: Double): Either[ModelError, VolumeWeightThreshold] =
    if value > 0.0 && value.isFinite then Right(new VolumeWeightThreshold(value))
    else Left(ModelError.InvalidParameter("volume-weight threshold", "must be positive and finite"))

  def unsafe(value: Double): VolumeWeightThreshold =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final class SoftThresholdSteepness private (val value: Double):
  require(value > 0.0 && value.isFinite, "soft-threshold steepness must be positive and finite")

  override def equals(other: Any): Boolean =
    other match
      case that: SoftThresholdSteepness => value == that.value
      case _                            => false

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString

object SoftThresholdSteepness:
  val Default: SoftThresholdSteepness = unsafe(2.0)

  def apply(value: Double): Either[ModelError, SoftThresholdSteepness] =
    if value > 0.0 && value.isFinite then Right(new SoftThresholdSteepness(value))
    else Left(ModelError.InvalidParameter("soft-threshold steepness", "must be positive and finite"))

  def unsafe(value: Double): SoftThresholdSteepness =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

/** A DVARS-to-weight function whose parameters exist only where they are meaningful. */
enum DvarsWeightFunction:
  case InverseSquared
  case SoftThreshold(
      threshold: VolumeWeightThreshold = VolumeWeightThreshold.Default,
      steepness: SoftThresholdSteepness = SoftThresholdSteepness.Default
  )
  case TukeyBisquare(threshold: VolumeWeightThreshold = VolumeWeightThreshold.Default)

/** A complete, executable response-derived volume-weight estimator. */
final case class DvarsWeightEstimator(
    function: DvarsWeightFunction = DvarsWeightFunction.InverseSquared,
    scope: DvarsWeightScope = DvarsWeightScope.WithinRun
)

enum VolumeWeighting:
  case Disabled
  case Estimated(estimator: DvarsWeightEstimator)
  case Fixed(
      weights: Vector[Double],
      alignment: FixedWeightAlignment = FixedWeightAlignment.FullSeries
  )

enum Regularization:
  case Auto, Gcv
  case Fixed(lambda: Double)

enum NuisanceProjection:
  case Disabled
  case MatrixProjection(matrix: DMat, lambda: Regularization = Regularization.Auto)

final case class RobustOptions(
    psi: RobustPsi = RobustPsi.Disabled,
    maxIterations: Int = 2,
    scaleScope: ScaleScope = ScaleScope.Run,
    reestimateAutocorrelation: Boolean = false
):
  require(maxIterations >= 1, "robust maxIterations must be at least 1")
  psi match
    case RobustPsi.Huber(k)     => require(k > 0.0 && k.isFinite, "Huber k must be positive and finite")
    case RobustPsi.Bisquare(c)  => require(c > 0.0 && c.isFinite, "Bisquare c must be positive and finite")
    case RobustPsi.Disabled    => ()

object RobustOptions:
  def make(
      psi: RobustPsi = RobustPsi.Disabled,
      maxIterations: Int = 2,
      scaleScope: ScaleScope = ScaleScope.Run,
      reestimateAutocorrelation: Boolean = false
  ): Either[ModelError, RobustOptions] =
    catchModelError(RobustOptions(psi, maxIterations, scaleScope, reestimateAutocorrelation))

final case class ArOptions(
    structure: ArStructure = ArStructure.Iid,
    iterations: Int = 1,
    global: Boolean = false,
    voxelwise: Boolean = false,
    exactFirst: Boolean = true,
    censoredTimepoints: Vector[Int] = Vector.empty,
    rho: Option[Double] = None,
    phi: Option[Vector[Double]] = None
):
  require(iterations >= 0, "AR iterations must be non-negative")
  require(censoredTimepoints.forall(_ >= 0), "censored timepoints must be non-negative")
  require(rho.isEmpty || phi.isEmpty, "use either AR rho or AR phi, not both")
  rho.foreach(r => require(r.isFinite && math.abs(r) < 1.0, "AR rho must be finite and satisfy abs(rho) < 1"))
  structure match
    case ArStructure.Ar(order) =>
      require(order >= 1, "AR order must be at least 1")
      rho.foreach(_ => require(order == 1, "AR rho is only valid for AR(1)"))
      phi.foreach { coefficients =>
        require(coefficients.length == order, s"AR phi length must match AR($order)")
        require(coefficients.forall(_.isFinite), "AR phi coefficients must be finite")
      }
    case ArStructure.Iid =>
      require(rho.isEmpty && phi.isEmpty, "iid autocorrelation cannot carry AR coefficients")

object ArOptions:
  def make(
      structure: ArStructure = ArStructure.Iid,
      iterations: Int = 1,
      global: Boolean = false,
      voxelwise: Boolean = false,
      exactFirst: Boolean = true,
      censoredTimepoints: Vector[Int] = Vector.empty,
      rho: Option[Double] = None,
      phi: Option[Vector[Double]] = None
  ): Either[ModelError, ArOptions] =
    catchModelError(ArOptions(structure, iterations, global, voxelwise, exactFirst, censoredTimepoints, rho, phi))

final case class LssConfig(
    trialTerm: Option[String] = None,
    eps: Double = 1e-12,
    rankTol: Double = 1e-7
):
  trialTerm.foreach(term => require(term.trim.nonEmpty, "LSS trial term must be non-empty"))
  require(eps > 0.0 && eps.isFinite, "LSS eps must be positive and finite")
  require(rankTol >= 0.0 && rankTol.isFinite, "LSS rankTol must be non-negative and finite")

object LssConfig:
  def make(
      trialTerm: Option[String] = None,
      eps: Double = 1e-12,
      rankTol: Double = 1e-7
  ): Either[ModelError, LssConfig] =
    catchModelError(LssConfig(trialTerm, eps, rankTol))

final case class FitConfig(
    robust: RobustOptions = RobustOptions(),
    autocorrelation: ArOptions = ArOptions(),
    volumeWeighting: VolumeWeighting = VolumeWeighting.Disabled,
    nuisanceProjection: NuisanceProjection = NuisanceProjection.Disabled,
    missingData: MissingDataPolicy = MissingDataPolicy.Error,
    lss: LssConfig = LssConfig()
):
  volumeWeighting match
    case VolumeWeighting.Fixed(weights, _) =>
      require(weights.nonEmpty, "fixed volume weights must be non-empty")
      require(weights.forall(w => w >= 0.0 && w.isFinite), "fixed volume weights must be non-negative and finite")
    case VolumeWeighting.Disabled | VolumeWeighting.Estimated(_) => ()

  nuisanceProjection match
    case NuisanceProjection.MatrixProjection(matrix, regularization) =>
      require(matrix.rows > 0 && matrix.cols > 0, "nuisance matrix must be non-empty")
      regularization match
        case Regularization.Fixed(lambda) => require(lambda >= 0.0 && lambda.isFinite, "lambda must be non-negative and finite")
        case Regularization.Auto | Regularization.Gcv => ()
    case NuisanceProjection.Disabled => ()

object FitConfig:
  def make(
      robust: RobustOptions = RobustOptions(),
      autocorrelation: ArOptions = ArOptions(),
      volumeWeighting: VolumeWeighting = VolumeWeighting.Disabled,
      nuisanceProjection: NuisanceProjection = NuisanceProjection.Disabled,
      missingData: MissingDataPolicy = MissingDataPolicy.Error,
      lss: LssConfig = LssConfig()
  ): Either[ModelError, FitConfig] =
    catchModelError(FitConfig(robust, autocorrelation, volumeWeighting, nuisanceProjection, missingData, lss))

final class ArOrder private (val value: Int):
  require(value >= 1, "AR order must be at least 1")

  override def equals(other: Any): Boolean =
    other match
      case that: ArOrder => value == that.value
      case _ => false

  override def hashCode(): Int =
    value.hashCode()

  override def toString: String =
    value.toString

object ArOrder:
  val One: ArOrder = unsafe(1)

  def apply(value: Int): Either[ModelError, ArOrder] =
    if value >= 1 then Right(new ArOrder(value))
    else Left(ModelError.InvalidParameter("AR order", "must be at least 1"))

  def unsafe(value: Int): ArOrder =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final class CensoredTimepoints private (val values: Vector[Int]):
  require(values.forall(_ >= 0), "censored timepoints must be non-negative")

  override def equals(other: Any): Boolean =
    other match
      case that: CensoredTimepoints => values == that.values
      case _ => false

  override def hashCode(): Int =
    values.hashCode()

  def validateWithin(nTimepoints: Int): Either[ModelError, Unit] =
    val seen = scala.collection.mutable.HashSet.empty[Int]
    var i = 0
    while i < values.length do
      val value = values(i)
      if value >= nTimepoints then return Left(ModelError.TimepointOutOfBounds("censored", value, nTimepoints))
      if seen.contains(value) then return Left(ModelError.InvalidParameter("censored timepoints", s"duplicate timepoint $value"))
      seen += value
      i += 1
    Right(())

object CensoredTimepoints:
  val Empty: CensoredTimepoints = unsafe(Vector.empty)

  def apply(values: Vector[Int]): Either[ModelError, CensoredTimepoints] =
    if values.exists(_ < 0) then Left(ModelError.InvalidParameter("censored timepoints", "must be non-negative"))
    else Right(new CensoredTimepoints(values))

  def unsafe(values: Vector[Int]): CensoredTimepoints =
    apply(values).fold(error => throw new IllegalArgumentException(error.message), identity)

enum ArCoefficientSpec:
  case Estimate
  case Rho(value: Double)
  case Phi(values: Vector[Double])

  def toLegacy: (Option[Double], Option[Vector[Double]]) =
    this match
      case Estimate => None -> None
      case Rho(value) => Some(value) -> None
      case Phi(values) => None -> Some(values)

object ArCoefficientSpec:
  def rho(value: Double): Either[ModelError, ArCoefficientSpec] =
    if value.isFinite && math.abs(value) < 1.0 then Right(Rho(value))
    else Left(ModelError.InvalidParameter("AR rho", "must be finite and satisfy abs(rho) < 1"))

  def phi(values: Vector[Double], order: ArOrder): Either[ModelError, ArCoefficientSpec] =
    if values.length != order.value then Left(ModelError.InvalidParameter("AR phi", s"length must match AR(${order.value})"))
    else if values.exists(value => !value.isFinite) then Left(ModelError.InvalidParameter("AR phi", "coefficients must be finite"))
    else Right(Phi(values))

final class AutocorrelationConfig private (
    val order: ArOrder,
    val iterations: Int,
    val global: Boolean,
    val voxelwise: Boolean,
    val exactFirst: Boolean,
    val censoredTimepoints: CensoredTimepoints,
    val coefficients: ArCoefficientSpec
):
  require(iterations >= 0, "AR iterations must be non-negative")

  override def equals(other: Any): Boolean =
    other match
      case that: AutocorrelationConfig =>
        order == that.order &&
          iterations == that.iterations &&
          global == that.global &&
          voxelwise == that.voxelwise &&
          exactFirst == that.exactFirst &&
          censoredTimepoints == that.censoredTimepoints &&
          coefficients == that.coefficients
      case _ => false

  override def hashCode(): Int =
    (((((31 * order.hashCode() + iterations.hashCode()) * 31 + global.hashCode()) * 31 + voxelwise.hashCode()) * 31 + exactFirst.hashCode()) * 31 +
      censoredTimepoints.hashCode()) * 31 + coefficients.hashCode()

  def toLegacy: ArOptions =
    val (rho, phi) = coefficients.toLegacy
    ArOptions(
      structure = ArStructure.Ar(order.value),
      iterations = iterations,
      global = global,
      voxelwise = voxelwise,
      exactFirst = exactFirst,
      censoredTimepoints = censoredTimepoints.values,
      rho = rho,
      phi = phi
    )

  def validateFor(nTimepoints: Int): Either[ModelError, Unit] =
    censoredTimepoints.validateWithin(nTimepoints)

object AutocorrelationConfig:
  val Default: AutocorrelationConfig =
    unsafe(order = 1)

  def apply(
      order: Int = 1,
      iterations: Int = 1,
      global: Boolean = false,
      voxelwise: Boolean = false,
      exactFirst: Boolean = true,
      censoredTimepoints: Vector[Int] = Vector.empty,
      coefficients: ArCoefficientSpec = ArCoefficientSpec.Estimate
  ): Either[ModelError, AutocorrelationConfig] =
    for
      arOrder <- ArOrder(order)
      censored <- CensoredTimepoints(censoredTimepoints)
      checkedCoefficients <- validateCoefficients(arOrder, iterations, coefficients)
      config <- make(arOrder, iterations, global, voxelwise, exactFirst, censored, checkedCoefficients)
    yield config

  def fromLegacy(options: ArOptions): Either[ModelError, AutocorrelationConfig] =
    options.structure match
      case ArStructure.Iid =>
        Left(ModelError.InvalidFitConfig(FitEngine.GeneralizedLeastSquares, "autocorrelation strategy requires AR(p), not iid"))
      case ArStructure.Ar(order) =>
        val coefficients =
          options.rho match
            case Some(rho) => ArCoefficientSpec.Rho(rho)
            case None =>
              options.phi match
                case Some(phi) => ArCoefficientSpec.Phi(phi)
                case None => ArCoefficientSpec.Estimate
        apply(
          order = order,
          iterations = options.iterations,
          global = options.global,
          voxelwise = options.voxelwise,
          exactFirst = options.exactFirst,
          censoredTimepoints = options.censoredTimepoints,
          coefficients = coefficients
        )

  def unsafe(
      order: Int = 1,
      iterations: Int = 1,
      global: Boolean = false,
      voxelwise: Boolean = false,
      exactFirst: Boolean = true,
      censoredTimepoints: Vector[Int] = Vector.empty,
      coefficients: ArCoefficientSpec = ArCoefficientSpec.Estimate
  ): AutocorrelationConfig =
    apply(order, iterations, global, voxelwise, exactFirst, censoredTimepoints, coefficients)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def make(
      order: ArOrder,
      iterations: Int,
      global: Boolean,
      voxelwise: Boolean,
      exactFirst: Boolean,
      censoredTimepoints: CensoredTimepoints,
      coefficients: ArCoefficientSpec
  ): Either[ModelError, AutocorrelationConfig] =
    if iterations < 0 then Left(ModelError.InvalidParameter("AR iterations", "must be non-negative"))
    else if global && voxelwise then Left(ModelError.InvalidParameter("AR pooling", "global and voxelwise estimation cannot both be enabled"))
    else if voxelwise && coefficients != ArCoefficientSpec.Estimate then
      Left(ModelError.InvalidParameter("AR voxelwise estimation", "requires estimated coefficients"))
    else if iterations > 1 && coefficients != ArCoefficientSpec.Estimate then
      Left(ModelError.InvalidParameter("AR iterations", "fixed coefficients cannot be re-estimated"))
    else Right(new AutocorrelationConfig(order, iterations, global, voxelwise, exactFirst, censoredTimepoints, coefficients))

  private def validateCoefficients(
      order: ArOrder,
      iterations: Int,
      coefficients: ArCoefficientSpec
  ): Either[ModelError, ArCoefficientSpec] =
    coefficients match
      case ArCoefficientSpec.Estimate =>
        if iterations >= 1 then Right(coefficients)
        else Left(ModelError.InvalidParameter("AR coefficients", "estimated AR needs at least one iteration"))
      case ArCoefficientSpec.Rho(value) =>
        if order.value != 1 then Left(ModelError.InvalidParameter("AR rho", "is only valid for AR(1)"))
        else ArCoefficientSpec.rho(value)
      case ArCoefficientSpec.Phi(values) =>
        ArCoefficientSpec.phi(values, order)

final class RobustConfig private (val options: RobustOptions):
  def toLegacy: RobustOptions = options

  override def equals(other: Any): Boolean =
    other match
      case that: RobustConfig => options == that.options
      case _ => false

  override def hashCode(): Int =
    options.hashCode()

object RobustConfig:
  def apply(options: RobustOptions): Either[ModelError, RobustConfig] =
    options.psi match
      case RobustPsi.Disabled =>
        Left(ModelError.InvalidFitConfig(FitEngine.RobustLeastSquares, "robust strategy requires a robust psi"))
      case _ =>
        Right(new RobustConfig(options))

  def huber(
      k: Double = 1.345,
      maxIterations: Int = 2,
      scaleScope: ScaleScope = ScaleScope.Run,
      reestimateAutocorrelation: Boolean = false
  ): Either[ModelError, RobustConfig] =
    RobustOptions.make(RobustPsi.Huber(k), maxIterations, scaleScope, reestimateAutocorrelation).flatMap(apply)

  def unsafe(options: RobustOptions): RobustConfig =
    apply(options).fold(error => throw new IllegalArgumentException(error.message), identity)

enum RobustAutocorrelation:
  case Disabled
  case Reestimate(autocorrelation: AutocorrelationConfig)

  def toLegacy: ArOptions =
    this match
      case Disabled => ArOptions()
      case Reestimate(autocorrelation) => autocorrelation.toLegacy

  def reestimates: Boolean =
    this match
      case Disabled => false
      case Reestimate(_) => true

  def validateFor(nTimepoints: Int): Either[ModelError, Unit] =
    this match
      case Disabled =>
        Right(())
      case Reestimate(autocorrelation) =>
        for
          _ <- autocorrelation.validateFor(nTimepoints)
          _ <- RobustAutocorrelation.validateReestimate(autocorrelation)
        yield ()

object RobustAutocorrelation:
  def fromLegacy(
      robust: RobustOptions,
      autocorrelation: ArOptions
  ): Either[ModelError, RobustAutocorrelation] =
    if robust.reestimateAutocorrelation then
      AutocorrelationConfig
        .fromLegacy(autocorrelation)
        .left
        .map {
          case ModelError.InvalidFitConfig(_, detail) => ModelError.InvalidFitConfig(FitEngine.RobustLeastSquares, detail)
          case other => other
        }
        .flatMap { config =>
          validateReestimate(config).map(_ => Reestimate(config))
        }
    else if autocorrelation == ArOptions() then Right(Disabled)
    else Left(ModelError.InvalidFitConfig(FitEngine.RobustLeastSquares, "AR options require robust re-estimation"))

  private[model] def validateReestimate(autocorrelation: AutocorrelationConfig): Either[ModelError, Unit] =
    if autocorrelation.voxelwise then
      Left(ModelError.InvalidFitConfig(FitEngine.RobustLeastSquares, "robust AR re-estimation currently requires shared or run-pooled AR"))
    else
      autocorrelation.coefficients match
        case ArCoefficientSpec.Estimate =>
          Right(())
        case _ =>
          Left(ModelError.InvalidFitConfig(FitEngine.RobustLeastSquares, "robust AR re-estimation requires estimated coefficients"))

final class TimepointWeights private (val values: Vector[Double]):
  require(values.nonEmpty, "timepoint weights must be non-empty")
  require(values.forall(w => w >= 0.0 && w.isFinite), "timepoint weights must be non-negative and finite")

  override def equals(other: Any): Boolean =
    other match
      case that: TimepointWeights => values == that.values
      case _ => false

  override def hashCode(): Int =
    values.hashCode()

  def validateFullSeriesLength(nTimepoints: Int): Either[ModelError, Unit] =
    if values.length == nTimepoints then Right(())
    else Left(ModelError.VectorLengthMismatch("fixed volume weights", nTimepoints, values.length))

object TimepointWeights:
  def apply(values: Vector[Double]): Either[ModelError, TimepointWeights] =
    if values.isEmpty then Left(ModelError.InvalidParameter("fixed volume weights", "must be non-empty"))
    else if values.exists(w => w < 0.0 || !w.isFinite) then
      Left(ModelError.InvalidParameter("fixed volume weights", "must be non-negative and finite"))
    else Right(new TimepointWeights(values))

  def unsafe(values: Vector[Double]): TimepointWeights =
    apply(values).fold(error => throw new IllegalArgumentException(error.message), identity)

enum ModelVolumeWeighting:
  case Disabled
  case Estimated(estimator: DvarsWeightEstimator)
  case Fixed(
      weights: TimepointWeights,
      alignment: FixedWeightAlignment = FixedWeightAlignment.FullSeries
  )

  def toLegacy: VolumeWeighting =
    this match
      case Disabled => VolumeWeighting.Disabled
      case Estimated(estimator) => VolumeWeighting.Estimated(estimator)
      case Fixed(weights, alignment) => VolumeWeighting.Fixed(weights.values, alignment)

  def validateFor(nTimepoints: Int): Either[ModelError, Unit] =
    this match
      case Fixed(weights, FixedWeightAlignment.FullSeries) =>
        weights.validateFullSeriesLength(nTimepoints)
      case Fixed(_, FixedWeightAlignment.SelectedRows) | Disabled | Estimated(_) =>
        Right(())

object ModelVolumeWeighting:
  def fromLegacy(value: VolumeWeighting): Either[ModelError, ModelVolumeWeighting] =
    value match
      case VolumeWeighting.Disabled =>
        Right(Disabled)
      case VolumeWeighting.Estimated(estimator) =>
        Right(Estimated(estimator))
      case VolumeWeighting.Fixed(weights, alignment) =>
        TimepointWeights(weights).map(Fixed(_, alignment))

  def fixed(
      weights: Vector[Double],
      alignment: FixedWeightAlignment = FixedWeightAlignment.FullSeries
  ): Either[ModelError, ModelVolumeWeighting] =
    TimepointWeights(weights).map(Fixed(_, alignment))

  def estimatedDvars(
      function: DvarsWeightFunction = DvarsWeightFunction.InverseSquared,
      scope: DvarsWeightScope = DvarsWeightScope.WithinRun
  ): ModelVolumeWeighting =
    Estimated(DvarsWeightEstimator(function, scope))

final class NuisanceMatrix private (val matrix: DMat):
  require(matrix.rows > 0 && matrix.cols > 0, "nuisance matrix must be non-empty")

  override def equals(other: Any): Boolean =
    other match
      case that: NuisanceMatrix => matrix == that.matrix
      case _ => false

  override def hashCode(): Int =
    matrix.hashCode()

  def validateRows(nTimepoints: Int): Either[ModelError, Unit] =
    if matrix.rows == nTimepoints then Right(())
    else Left(ModelError.MatrixRowMismatch("nuisance matrix", nTimepoints, matrix.rows))

object NuisanceMatrix:
  def apply(matrix: DMat): Either[ModelError, NuisanceMatrix] =
    if matrix.rows > 0 && matrix.cols > 0 then Right(new NuisanceMatrix(matrix))
    else Left(ModelError.InvalidParameter("nuisance matrix", "must be non-empty"))

  def unsafe(matrix: DMat): NuisanceMatrix =
    apply(matrix).fold(error => throw new IllegalArgumentException(error.message), identity)

enum ModelNuisanceProjection:
  case Disabled
  case MatrixProjection(matrix: NuisanceMatrix, lambda: Regularization)

  def toLegacy: NuisanceProjection =
    this match
      case Disabled => NuisanceProjection.Disabled
      case MatrixProjection(matrix, lambda) => NuisanceProjection.MatrixProjection(matrix.matrix, lambda)

  def validateFor(nTimepoints: Int): Either[ModelError, Unit] =
    this match
      case Disabled => Right(())
      case MatrixProjection(matrix, _) => matrix.validateRows(nTimepoints)

object ModelNuisanceProjection:
  def fromLegacy(value: NuisanceProjection): Either[ModelError, ModelNuisanceProjection] =
    value match
      case NuisanceProjection.Disabled =>
        Right(Disabled)
      case NuisanceProjection.MatrixProjection(matrix, lambda) =>
        validateRegularization(lambda).flatMap(_ => NuisanceMatrix(matrix).map(MatrixProjection(_, lambda)))

  private def validateRegularization(lambda: Regularization): Either[ModelError, Unit] =
    lambda match
      case Regularization.Fixed(value) if value < 0.0 || !value.isFinite =>
        Left(ModelError.InvalidParameter("lambda", "must be non-negative and finite"))
      case _ =>
        Right(())

final case class FitControls(
    volumeWeighting: ModelVolumeWeighting = ModelVolumeWeighting.Disabled,
    nuisanceProjection: ModelNuisanceProjection = ModelNuisanceProjection.Disabled,
    missingData: MissingDataPolicy = MissingDataPolicy.Error
):
  def validateFor(nTimepoints: Int): Either[ModelError, Unit] =
    for
      _ <- volumeWeighting.validateFor(nTimepoints)
      _ <- nuisanceProjection.validateFor(nTimepoints)
    yield ()

  def toLegacyConfig(
      robust: RobustOptions = RobustOptions(),
      autocorrelation: ArOptions = ArOptions(),
      lss: LssConfig = LssConfig()
  ): FitConfig =
    FitConfig(
      robust = robust,
      autocorrelation = autocorrelation,
      volumeWeighting = volumeWeighting.toLegacy,
      nuisanceProjection = nuisanceProjection.toLegacy,
      missingData = missingData,
      lss = lss
    )

object FitControls:
  def fromLegacy(config: FitConfig): Either[ModelError, FitControls] =
    for
      volume <- ModelVolumeWeighting.fromLegacy(config.volumeWeighting)
      nuisance <- ModelNuisanceProjection.fromLegacy(config.nuisanceProjection)
    yield FitControls(volume, nuisance, config.missingData)

final class LssStrategyConfig private (
    val trialTerm: Option[TermId],
    val eps: Double,
    val rankTol: Double
):
  require(eps > 0.0 && eps.isFinite, "LSS eps must be positive and finite")
  require(rankTol >= 0.0 && rankTol.isFinite, "LSS rankTol must be non-negative and finite")

  override def equals(other: Any): Boolean =
    other match
      case that: LssStrategyConfig => trialTerm == that.trialTerm && eps == that.eps && rankTol == that.rankTol
      case _ => false

  override def hashCode(): Int =
    (31 * trialTerm.hashCode() + eps.hashCode()) * 31 + rankTol.hashCode()

  def toLegacy: LssConfig =
    LssConfig(trialTerm = trialTerm.map(_.value), eps = eps, rankTol = rankTol)

  def validateFor(model: FmriModel): Either[ModelError, Unit] =
    trialTerm match
      case None => Right(())
      case Some(term) =>
        val name = term.value
        if model.eventModel.termKeys.contains(name) then Right(())
        else Left(ModelError.UnknownLssTrialTerm(name, model.eventModel.termKeys))

object LssStrategyConfig:
  val Default: LssStrategyConfig = unsafe()

  def apply(
      trialTerm: Option[String] = None,
      eps: Double = 1e-12,
      rankTol: Double = 1e-7
  ): Either[ModelError, LssStrategyConfig] =
    for
      term <- bindTrialTerm(trialTerm)
      config <- make(term, eps, rankTol)
    yield config

  def fromLegacy(config: LssConfig): Either[ModelError, LssStrategyConfig] =
    apply(config.trialTerm, config.eps, config.rankTol)

  def unsafe(
      trialTerm: Option[String] = None,
      eps: Double = 1e-12,
      rankTol: Double = 1e-7
  ): LssStrategyConfig =
    apply(trialTerm, eps, rankTol).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def bindTrialTerm(value: Option[String]): Either[ModelError, Option[TermId]] =
    value match
      case None => Right(None)
      case Some(term) =>
        TermId(term)
          .left
          .map(error => ModelError.InvalidId("term", term, error.message))
          .map(Some(_))

  private def make(
      trialTerm: Option[TermId],
      eps: Double,
      rankTol: Double
  ): Either[ModelError, LssStrategyConfig] =
    if eps <= 0.0 || !eps.isFinite then Left(ModelError.InvalidParameter("LSS eps", "must be positive and finite"))
    else if rankTol < 0.0 || !rankTol.isFinite then Left(ModelError.InvalidParameter("LSS rankTol", "must be non-negative and finite"))
    else Right(new LssStrategyConfig(trialTerm, eps, rankTol))

final class LowRankComponentCount private (val value: Int):
  require(value >= 1, "low-rank component count must be positive")

  override def equals(other: Any): Boolean =
    other match
      case that: LowRankComponentCount => value == that.value
      case _ => false

  override def hashCode(): Int =
    value.hashCode()

  override def toString: String =
    value.toString

object LowRankComponentCount:
  def apply(value: Int): Either[ModelError, LowRankComponentCount] =
    if value >= 1 then Right(new LowRankComponentCount(value))
    else Left(ModelError.InvalidParameter("low-rank component count", "must be positive"))

  def unsafe(value: Int): LowRankComponentCount =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

enum LowRankComponentSpec:
  case Full
  case Fixed(count: LowRankComponentCount)

  def validateFor(label: String, maximum: Int): Either[ModelError, Unit] =
    val maxComponents = math.max(0, maximum)
    this match
      case Full =>
        if maxComponents >= 1 then Right(())
        else Left(ModelError.InvalidParameter(label, "requires at least one estimable component"))
      case Fixed(count) =>
        if maxComponents < 1 then Left(ModelError.InvalidParameter(label, "requires at least one estimable component"))
        else if count.value <= maxComponents then Right(())
        else Left(ModelError.InvalidParameter(label, s"requested ${count.value} components but maximum is $maxComponents"))

  def isFull(maximum: Int): Boolean =
    this match
      case Full => true
      case Fixed(count) => count.value == maximum

object LowRankComponentSpec:
  def fixed(value: Int): Either[ModelError, LowRankComponentSpec] =
    LowRankComponentCount(value).map(Fixed.apply)

  def unsafeFixed(value: Int): LowRankComponentSpec =
    fixed(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final class ReducedRankEnergyRetention private (val value: Double):
  require(value > 0.0 && value <= 1.0 && value.isFinite, "energy retention must be in (0, 1]")

  override def equals(other: Any): Boolean =
    other match
      case that: ReducedRankEnergyRetention => value == that.value
      case _ => false

  override def hashCode(): Int =
    value.hashCode()

  override def toString: String =
    value.toString

object ReducedRankEnergyRetention:
  def apply(value: Double): Either[ModelError, ReducedRankEnergyRetention] =
    if value > 0.0 && value <= 1.0 && value.isFinite then Right(new ReducedRankEnergyRetention(value))
    else Left(ModelError.InvalidParameter("reduced-rank energy retention", "must be in (0, 1]"))

  def unsafe(value: Double): ReducedRankEnergyRetention =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final class ReducedRankResidualBudget private (val value: Double):
  require(value >= 0.0 && value.isFinite, "residual RSS budget must be non-negative and finite")

  override def equals(other: Any): Boolean =
    other match
      case that: ReducedRankResidualBudget => value == that.value
      case _ => false

  override def hashCode(): Int =
    value.hashCode()

  override def toString: String =
    value.toString

object ReducedRankResidualBudget:
  def apply(value: Double): Either[ModelError, ReducedRankResidualBudget] =
    if value >= 0.0 && value.isFinite then Right(new ReducedRankResidualBudget(value))
    else Left(ModelError.InvalidParameter("reduced-rank residual RSS budget", "must be non-negative and finite"))

  def unsafe(value: Double): ReducedRankResidualBudget =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

enum ReducedRankComponentSpec:
  case Full
  case Fixed(count: LowRankComponentCount)
  case EnergyRetained(keep: ReducedRankEnergyRetention)
  case ResidualSumsOfSquaresBudget(budget: ReducedRankResidualBudget)

  def validateFor(label: String, maximum: Int): Either[ModelError, Unit] =
    val maxComponents = math.max(0, maximum)
    this match
      case Full =>
        if maxComponents >= 1 then Right(())
        else Left(ModelError.InvalidParameter(label, "requires at least one estimable component"))
      case Fixed(count) =>
        if maxComponents < 1 then Left(ModelError.InvalidParameter(label, "requires at least one estimable component"))
        else if count.value <= maxComponents then Right(())
        else Left(ModelError.InvalidParameter(label, s"requested ${count.value} components but maximum is $maxComponents"))
      case EnergyRetained(_) | ResidualSumsOfSquaresBudget(_) =>
        if maxComponents >= 1 then Right(())
        else Left(ModelError.InvalidParameter(label, "requires at least one estimable component"))

  def isFull(maximum: Int): Boolean =
    this match
      case Full => true
      case Fixed(count) => count.value == maximum
      case EnergyRetained(_) | ResidualSumsOfSquaresBudget(_) => false

object ReducedRankComponentSpec:
  def fixed(value: Int): Either[ModelError, ReducedRankComponentSpec] =
    LowRankComponentCount(value).map(Fixed.apply)

  def unsafeFixed(value: Int): ReducedRankComponentSpec =
    fixed(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  def energyRetained(value: Double): Either[ModelError, ReducedRankComponentSpec] =
    ReducedRankEnergyRetention(value).map(EnergyRetained.apply)

  def unsafeEnergyRetained(value: Double): ReducedRankComponentSpec =
    energyRetained(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  def residualSumsOfSquaresBudget(value: Double): Either[ModelError, ReducedRankComponentSpec] =
    ReducedRankResidualBudget(value).map(ResidualSumsOfSquaresBudget.apply)

  def unsafeResidualSumsOfSquaresBudget(value: Double): ReducedRankComponentSpec =
    residualSumsOfSquaresBudget(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final class ReducedRankBootstrapReplicates private (val value: Int):
  require(value >= 2, "reduced-rank bootstrap requires at least two replicates")

  override def equals(other: Any): Boolean =
    other match
      case that: ReducedRankBootstrapReplicates => value == that.value
      case _                                    => false

  override def hashCode(): Int = value.hashCode()

object ReducedRankBootstrapReplicates:
  def apply(value: Int): Either[ModelError, ReducedRankBootstrapReplicates] =
    if value >= 2 then Right(new ReducedRankBootstrapReplicates(value))
    else Left(ModelError.InvalidParameter("reduced-rank bootstrap replicates", "must be at least two"))

  def unsafe(value: Int): ReducedRankBootstrapReplicates =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final class ReducedRankBootstrapBlockSize private (val value: Int):
  require(value >= 1, "reduced-rank bootstrap block size must be positive")

  override def equals(other: Any): Boolean =
    other match
      case that: ReducedRankBootstrapBlockSize => value == that.value
      case _                                   => false

  override def hashCode(): Int = value.hashCode()

object ReducedRankBootstrapBlockSize:
  def apply(value: Int): Either[ModelError, ReducedRankBootstrapBlockSize] =
    if value >= 1 then Right(new ReducedRankBootstrapBlockSize(value))
    else Left(ModelError.InvalidParameter("reduced-rank bootstrap block size", "must be positive"))

  def unsafe(value: Int): ReducedRankBootstrapBlockSize =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final class ReducedRankBootstrapSeed private (val value: Int):
  require(value >= 0, "reduced-rank bootstrap seed must be non-negative")

  override def equals(other: Any): Boolean =
    other match
      case that: ReducedRankBootstrapSeed => value == that.value
      case _                              => false

  override def hashCode(): Int = value.hashCode()

object ReducedRankBootstrapSeed:
  def apply(value: Int): Either[ModelError, ReducedRankBootstrapSeed] =
    if value >= 0 then Right(new ReducedRankBootstrapSeed(value))
    else Left(ModelError.InvalidParameter("reduced-rank bootstrap seed", "must be non-negative"))

  def unsafe(value: Int): ReducedRankBootstrapSeed =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class ReducedRankBootstrapConfig private (
    replicates: ReducedRankBootstrapReplicates,
    blockSize: ReducedRankBootstrapBlockSize,
    seed: ReducedRankBootstrapSeed
):
  def validateFor(nTimepoints: Int): Either[ModelError, Unit] =
    if blockSize.value <= nTimepoints then Right(())
    else Left(ModelError.InvalidParameter(
      "reduced-rank bootstrap block size",
      s"requested ${blockSize.value} rows but model has $nTimepoints timepoints"
    ))

object ReducedRankBootstrapConfig:
  val Default: ReducedRankBootstrapConfig = unsafe()

  def apply(
      replicates: Int = 200,
      blockSize: Int = 1,
      seed: Int = 0
  ): Either[ModelError, ReducedRankBootstrapConfig] =
    for
      checkedReplicates <- ReducedRankBootstrapReplicates(replicates)
      checkedBlockSize <- ReducedRankBootstrapBlockSize(blockSize)
      checkedSeed <- ReducedRankBootstrapSeed(seed)
    yield new ReducedRankBootstrapConfig(checkedReplicates, checkedBlockSize, checkedSeed)

  def unsafe(
      replicates: Int = 200,
      blockSize: Int = 1,
      seed: Int = 0
  ): ReducedRankBootstrapConfig =
    apply(replicates, blockSize, seed).fold(error => throw new IllegalArgumentException(error.message), identity)

enum ReducedRankInferencePolicy:
  case Conditional
  case Bootstrap(config: ReducedRankBootstrapConfig)

  def validateFor(nTimepoints: Int): Either[ModelError, Unit] =
    this match
      case Conditional       => Right(())
      case Bootstrap(config) => config.validateFor(nTimepoints)

enum LatentSketchMethod:
  case IdentityResponse
  case ContiguousVoxelAveraging
  case PrincipalComponents

  def label: String =
    this match
      case IdentityResponse          => "identity response"
      case ContiguousVoxelAveraging  => "contiguous voxel averaging"
      case PrincipalComponents       => "principal components"

final class LatentSketchConfig private (
    val components: LowRankComponentSpec,
    val method: LatentSketchMethod
):
  override def equals(other: Any): Boolean =
    other match
      case that: LatentSketchConfig => components == that.components && method == that.method
      case _ => false

  override def hashCode(): Int =
    31 * components.hashCode() + method.hashCode()

  def validateFor(model: FmriModel): Either[ModelError, Unit] =
    val maximum =
      method match
        case LatentSketchMethod.PrincipalComponents =>
          math.min(model.nTimepoints, model.dataset.shape.spatialSize)
        case LatentSketchMethod.IdentityResponse | LatentSketchMethod.ContiguousVoxelAveraging =>
          model.dataset.shape.spatialSize
    for
      _ <- components.validateFor("latent sketch components", maximum)
      _ <- validateMethod(maximum)
    yield ()

  def isFullRankFor(model: FmriModel): Boolean =
    components.isFull(model.dataset.shape.spatialSize)

  private def validateMethod(maximum: Int): Either[ModelError, Unit] =
    method match
      case LatentSketchMethod.IdentityResponse if !components.isFull(maximum) =>
        Left(ModelError.InvalidParameter(
          "latent sketch method",
          "identity response requires full component coverage; use contiguous voxel averaging for compressed sketches"
        ))
      case LatentSketchMethod.PrincipalComponents =>
        components match
          case LowRankComponentSpec.Fixed(_) =>
            Right(())
          case LowRankComponentSpec.Full =>
            Left(ModelError.InvalidParameter(
              "latent sketch method",
              "principal components require an explicit fixed component count"
            ))
      case _ =>
        Right(())

object LatentSketchConfig:
  val Default: LatentSketchConfig = unsafe()

  def apply(
      components: LowRankComponentSpec = LowRankComponentSpec.Full,
      method: LatentSketchMethod = LatentSketchMethod.IdentityResponse
  ): Either[ModelError, LatentSketchConfig] =
    Right(new LatentSketchConfig(components, method))

  def unsafe(
      components: LowRankComponentSpec = LowRankComponentSpec.Full,
      method: LatentSketchMethod = LatentSketchMethod.IdentityResponse
  ): LatentSketchConfig =
    apply(components, method).fold(error => throw new IllegalArgumentException(error.message), identity)

final class ReducedRankGlsConfig private (
    val components: ReducedRankComponentSpec,
    val autocorrelation: AutocorrelationConfig,
    val inference: ReducedRankInferencePolicy
):
  override def equals(other: Any): Boolean =
    other match
      case that: ReducedRankGlsConfig =>
        components == that.components && autocorrelation == that.autocorrelation && inference == that.inference
      case _ => false

  override def hashCode(): Int =
    (31 * components.hashCode() + autocorrelation.hashCode()) * 31 + inference.hashCode()

  def validateFor(model: FmriModel): Either[ModelError, Unit] =
    val maximum = math.min(model.eventModel.columnNames.length, model.dataset.shape.spatialSize)
    for
      _ <- components.validateFor("reduced-rank GLS components", maximum)
      _ <- autocorrelation.validateFor(model.nTimepoints)
      _ <- inference.validateFor(model.nTimepoints)
    yield ()

  def isFullRankFor(model: FmriModel): Boolean =
    components.isFull(math.min(model.eventModel.columnNames.length, model.dataset.shape.spatialSize))

  def toLegacyConfig(controls: FitControls): FitConfig =
    controls.toLegacyConfig(autocorrelation = autocorrelation.toLegacy)

object ReducedRankGlsConfig:
  def apply(
      components: ReducedRankComponentSpec = ReducedRankComponentSpec.Full,
      autocorrelation: AutocorrelationConfig = AutocorrelationConfig.Default,
      inference: ReducedRankInferencePolicy = ReducedRankInferencePolicy.Conditional
  ): Either[ModelError, ReducedRankGlsConfig] =
    Right(new ReducedRankGlsConfig(components, autocorrelation, inference))

  def unsafe(
      components: ReducedRankComponentSpec = ReducedRankComponentSpec.Full,
      autocorrelation: AutocorrelationConfig = AutocorrelationConfig.Default,
      inference: ReducedRankInferencePolicy = ReducedRankInferencePolicy.Conditional
  ): ReducedRankGlsConfig =
    apply(components, autocorrelation, inference).fold(error => throw new IllegalArgumentException(error.message), identity)

enum FitStrategy:
  case OrdinaryLeastSquares(controls: FitControls = FitControls())
  case RunwiseLeastSquares(controls: FitControls = FitControls())
  case SeparateRunsThenFixedEffects(controls: FitControls = FitControls())
  case GeneralizedLeastSquares(
      autocorrelation: AutocorrelationConfig = AutocorrelationConfig.Default,
      controls: FitControls = FitControls()
  )
  case RobustLeastSquares(
      robust: RobustConfig,
      controls: FitControls = FitControls(),
      autocorrelation: RobustAutocorrelation = RobustAutocorrelation.Disabled
  )
  case LeastSquaresSeparate(
      lss: LssStrategyConfig = LssStrategyConfig.Default,
      controls: FitControls = FitControls()
  )
  case LatentSketch(
      sketch: LatentSketchConfig = LatentSketchConfig.Default,
      controls: FitControls = FitControls()
  )
  case ReducedRankGls(
      lowRank: ReducedRankGlsConfig = ReducedRankGlsConfig.unsafe(),
      controls: FitControls = FitControls()
  )

  def engine: FitEngine =
    this match
      case OrdinaryLeastSquares(_) => FitEngine.OrdinaryLeastSquares
      case RunwiseLeastSquares(_) => FitEngine.RunwiseLeastSquares
      case SeparateRunsThenFixedEffects(_) => FitEngine.FixedEffects
      case GeneralizedLeastSquares(_, _) => FitEngine.GeneralizedLeastSquares
      case RobustLeastSquares(_, _, _) => FitEngine.RobustLeastSquares
      case LeastSquaresSeparate(_, _) => FitEngine.LeastSquaresSeparate
      case LatentSketch(_, _) => FitEngine.LatentSketch
      case ReducedRankGls(_, _) => FitEngine.ReducedRankGls

  def config: FitConfig =
    this match
      case OrdinaryLeastSquares(controls) =>
        controls.toLegacyConfig()
      case RunwiseLeastSquares(controls) =>
        controls.toLegacyConfig()
      case SeparateRunsThenFixedEffects(controls) =>
        controls.toLegacyConfig()
      case GeneralizedLeastSquares(autocorrelation, controls) =>
        controls.toLegacyConfig(autocorrelation = autocorrelation.toLegacy)
      case RobustLeastSquares(robust, controls, autocorrelation) =>
        controls.toLegacyConfig(
          robust = robust.toLegacy.copy(reestimateAutocorrelation = autocorrelation.reestimates),
          autocorrelation = autocorrelation.toLegacy
        )
      case LeastSquaresSeparate(lss, controls) =>
        controls.toLegacyConfig(lss = lss.toLegacy)
      case LatentSketch(_, controls) =>
        controls.toLegacyConfig()
      case ReducedRankGls(lowRank, controls) =>
        lowRank.toLegacyConfig(controls)

  /** The coefficient estimand exposed by this strategy. */
  def coefficientScope: CoefficientScope =
    this match
      case RunwiseLeastSquares(_) => CoefficientScope.RunSpecific
      case SeparateRunsThenFixedEffects(_) => CoefficientScope.SeparateRunsThenFixedEffects
      case _                      => CoefficientScope.SharedAcrossRuns

  def validateFor(model: FmriModel): Either[ModelError, Unit] =
    val nTimepoints = model.nTimepoints
    this match
      case OrdinaryLeastSquares(controls) =>
        FitStrategy.validateControls(engine, controls, nTimepoints, allowVolumeWeighting = true)
      case RunwiseLeastSquares(controls) =>
        FitStrategy.validateControls(engine, controls, nTimepoints, allowVolumeWeighting = false)
      case SeparateRunsThenFixedEffects(controls) =>
        FitStrategy.validateControls(engine, controls, nTimepoints, allowVolumeWeighting = false)
      case GeneralizedLeastSquares(autocorrelation, controls) =>
        for
          _ <- FitStrategy.validateControls(engine, controls, nTimepoints, allowVolumeWeighting = false)
          _ <- autocorrelation.validateFor(nTimepoints)
        yield ()
      case RobustLeastSquares(robust, controls, autocorrelation) =>
        for
          _ <- FitStrategy.validateControls(engine, controls, nTimepoints, allowVolumeWeighting = false)
          _ <- FitStrategy.validateRobustAutocorrelation(robust, autocorrelation, nTimepoints)
        yield ()
      case LeastSquaresSeparate(lss, controls) =>
        for
          _ <- FitStrategy.validateControls(engine, controls, nTimepoints, allowVolumeWeighting = false)
          _ <- lss.validateFor(model)
        yield ()
      case LatentSketch(sketch, controls) =>
        for
          _ <- FitStrategy.validateControls(engine, controls, nTimepoints, allowVolumeWeighting = false)
          _ <- sketch.validateFor(model)
        yield ()
      case ReducedRankGls(lowRank, controls) =>
        for
          _ <- FitStrategy.validateControls(engine, controls, nTimepoints, allowVolumeWeighting = false)
          _ <- lowRank.validateFor(model)
        yield ()

object FitStrategy:
  val Default: FitStrategy =
    OrdinaryLeastSquares()

  def fromLegacy(engine: FitEngine, config: FitConfig = FitConfig()): Either[ModelError, FitStrategy] =
    FitControls.fromLegacy(config).flatMap { controls =>
      engine match
        case FitEngine.OrdinaryLeastSquares =>
          for
            _ <- requireNoRobust(engine, config)
            _ <- requireNoAutocorrelation(engine, config)
            _ <- requireDefaultLss(engine, config)
          yield FitStrategy.OrdinaryLeastSquares(controls)

        case FitEngine.RunwiseLeastSquares =>
          for
            _ <- requireNoRobust(engine, config)
            _ <- requireNoAutocorrelation(engine, config)
            _ <- requireDefaultLss(engine, config)
          yield FitStrategy.RunwiseLeastSquares(controls)

        case FitEngine.FixedEffects =>
          for
            _ <- requireNoRobust(engine, config)
            _ <- requireNoAutocorrelation(engine, config)
            _ <- requireDefaultLss(engine, config)
          yield FitStrategy.SeparateRunsThenFixedEffects(controls)

        case FitEngine.GeneralizedLeastSquares =>
          for
            _ <- requireNoRobust(engine, config)
            _ <- requireDefaultLss(engine, config)
            ar <- AutocorrelationConfig.fromLegacy(config.autocorrelation).left.map {
              case ModelError.InvalidFitConfig(_, detail) => ModelError.InvalidFitConfig(engine, detail)
              case other => other
            }
          yield FitStrategy.GeneralizedLeastSquares(ar, controls)

        case FitEngine.RobustLeastSquares =>
          for
            _ <- requireDefaultLss(engine, config)
            robust <- RobustConfig(config.robust)
            autocorrelation <- RobustAutocorrelation.fromLegacy(config.robust, config.autocorrelation)
          yield FitStrategy.RobustLeastSquares(robust, controls, autocorrelation)

        case FitEngine.LeastSquaresSeparate =>
          for
            _ <- requireNoRobust(engine, config)
            _ <- requireNoAutocorrelation(engine, config)
            lss <- LssStrategyConfig.fromLegacy(config.lss)
          yield FitStrategy.LeastSquaresSeparate(lss, controls)

        case FitEngine.LatentSketch =>
          for
            _ <- requireNoRobust(engine, config)
            _ <- requireNoAutocorrelation(engine, config)
            _ <- requireDefaultLss(engine, config)
          yield FitStrategy.LatentSketch(controls = controls)

        case FitEngine.ReducedRankGls =>
          for
            _ <- requireNoRobust(engine, config)
            _ <- requireDefaultLss(engine, config)
            ar <- AutocorrelationConfig.fromLegacy(config.autocorrelation).left.map {
              case ModelError.InvalidFitConfig(_, detail) => ModelError.InvalidFitConfig(engine, detail)
              case other => other
            }
            lowRank <- ReducedRankGlsConfig(autocorrelation = ar)
          yield FitStrategy.ReducedRankGls(lowRank, controls)
    }

  def unsafeFromLegacy(engine: FitEngine, config: FitConfig = FitConfig()): FitStrategy =
    fromLegacy(engine, config).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def validateControls(
      engine: FitEngine,
      controls: FitControls,
      nTimepoints: Int,
      allowVolumeWeighting: Boolean
  ): Either[ModelError, Unit] =
    for
      _ <- controls.validateFor(nTimepoints)
      _ <-
        if allowVolumeWeighting || controls.volumeWeighting == ModelVolumeWeighting.Disabled then Right(())
        else
          Left(ModelError.InvalidFitConfig(
            engine,
            "volume weighting is currently executable only for ordinary least squares"
          ))
    yield ()

  private def requireNoRobust(engine: FitEngine, config: FitConfig): Either[ModelError, Unit] =
    if config.robust == RobustOptions() then Right(())
    else Left(ModelError.InvalidFitConfig(engine, "robust options belong only to RobustLeastSquares"))

  private def requireNoAutocorrelation(engine: FitEngine, config: FitConfig): Either[ModelError, Unit] =
    if config.autocorrelation == ArOptions() then Right(())
    else Left(ModelError.InvalidFitConfig(engine, "AR options belong only to GLS strategies"))

  private def requireDefaultLss(engine: FitEngine, config: FitConfig): Either[ModelError, Unit] =
    if config.lss == LssConfig() then Right(())
    else Left(ModelError.InvalidFitConfig(engine, "LSS options belong only to LeastSquaresSeparate"))

  private def validateRobustAutocorrelation(
      robust: RobustConfig,
      autocorrelation: RobustAutocorrelation,
      nTimepoints: Int
  ): Either[ModelError, Unit] =
    autocorrelation match
      case RobustAutocorrelation.Disabled if robust.toLegacy.reestimateAutocorrelation =>
        Left(ModelError.InvalidFitConfig(FitEngine.RobustLeastSquares, "robust re-estimation requires an explicit AR policy"))
      case _ =>
        autocorrelation.validateFor(nTimepoints)

private def catchModelError[A](value: => A): Either[ModelError, A] =
  try Right(value)
  catch case NonFatal(t) => Left(ModelError.fromThrowable(t))
