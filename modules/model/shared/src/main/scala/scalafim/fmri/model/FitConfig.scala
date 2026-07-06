package scalafim.fmri.model

import scalafim.linalg.DoubleMatrix

enum MissingDataPolicy:
  case Error, Propagate

enum ScaleScope:
  case Run, Global, Voxel

enum RobustPsi:
  case Disabled
  case Huber(k: Double = 1.345)
  case Bisquare(c: Double = 4.685)

enum ArStructure:
  case Iid
  case Ar(order: Int)

enum VolumeWeighting:
  case Disabled
  case Estimated(method: VolumeWeighting.Method, threshold: Double)
  case Fixed(weights: Vector[Double])

object VolumeWeighting:
  enum Method:
    case InverseSquared, SoftThreshold, Tukey

enum Regularization:
  case Auto, Gcv
  case Fixed(lambda: Double)

enum NuisanceProjection:
  case Disabled
  case MatrixProjection(matrix: DoubleMatrix, lambda: Regularization = Regularization.Auto)

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

final case class LssConfig(
    trialTerm: Option[String] = None,
    eps: Double = 1e-12,
    rankTol: Double = 1e-7
):
  trialTerm.foreach(term => require(term.trim.nonEmpty, "LSS trial term must be non-empty"))
  require(eps > 0.0 && eps.isFinite, "LSS eps must be positive and finite")
  require(rankTol >= 0.0 && rankTol.isFinite, "LSS rankTol must be non-negative and finite")

final case class FitConfig(
    robust: RobustOptions = RobustOptions(),
    autocorrelation: ArOptions = ArOptions(),
    volumeWeighting: VolumeWeighting = VolumeWeighting.Disabled,
    nuisanceProjection: NuisanceProjection = NuisanceProjection.Disabled,
    missingData: MissingDataPolicy = MissingDataPolicy.Error,
    lss: LssConfig = LssConfig()
):
  volumeWeighting match
    case VolumeWeighting.Estimated(_, threshold) =>
      require(threshold > 0.0 && threshold.isFinite, "volume-weight threshold must be positive and finite")
    case VolumeWeighting.Fixed(weights) =>
      require(weights.nonEmpty, "fixed volume weights must be non-empty")
      require(weights.forall(w => w >= 0.0 && w.isFinite), "fixed volume weights must be non-negative and finite")
    case VolumeWeighting.Disabled => ()

  nuisanceProjection match
    case NuisanceProjection.MatrixProjection(matrix, regularization) =>
      require(matrix.rows > 0 && matrix.cols > 0, "nuisance matrix must be non-empty")
      regularization match
        case Regularization.Fixed(lambda) => require(lambda >= 0.0 && lambda.isFinite, "lambda must be non-negative and finite")
        case Regularization.Auto | Regularization.Gcv => ()
    case NuisanceProjection.Disabled => ()
