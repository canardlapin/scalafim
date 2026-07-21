package scalafim.multivar

opaque type Ridge = Double

object Ridge:
  def apply(value: Double): Either[MultivarError, Ridge] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(MultivarError.InvalidTolerance("ridge", value))

  private[multivar] def unsafe(value: Double): Ridge =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (ridge: Ridge)
    inline def value: Double = ridge

final case class CcaRegularization(x: Ridge, y: Ridge)

object CcaRegularization:
  val default: CcaRegularization =
    CcaRegularization(Ridge.unsafe(1e-8), Ridge.unsafe(1e-8))

  def symmetric(value: Double): Either[MultivarError, CcaRegularization] =
    for ridge <- Ridge(value)
    yield CcaRegularization(ridge, ridge)

  def asymmetric(x: Double, y: Double): Either[MultivarError, CcaRegularization] =
    for
      xRidge <- Ridge(x)
      yRidge <- Ridge(y)
    yield CcaRegularization(xRidge, yRidge)

/** Direction of a directed paired regression.
  *
  * Single-case by design: only x-to-y fitting is implemented, and an unsupported
  * y-to-x case would be representable-but-rejected. The enum is kept (rather than
  * dropping the parameter) so the fitted direction stays explicit in method values
  * and plans, and a future y-to-x estimator is an additive case.
  */
enum RegressionDirection:
  case XToY

  def label: String =
    this match
      case XToY => "x-to-y"

/** Regularization for directed paired regression.
  *
  * `Ridge` follows the covariance-scale convention shared with CCA: the typed value
  * `lambda` penalizes the covariance-scaled Gram, so the coefficient solves
  * `(X'X/(n-1) + lambda I)^-1 (X'Y/(n-1))` — equivalently
  * `(X'X + (n-1) lambda I)^-1 X'Y` — and `lambda` is comparable across sample sizes
  * and with `CcaRegularization`.
  */
enum RegressionRegularization:
  case Ols
  case Ridge(value: scalafim.multivar.Ridge)

  def label: String =
    this match
      case Ols      => "ols"
      case RegressionRegularization.Ridge(_) => "ridge"

object RegressionRegularization:
  def ridge(value: Double): Either[MultivarError, RegressionRegularization] =
    scalafim.multivar.Ridge(value).map(RegressionRegularization.Ridge(_))

enum PairedLatentMethod:
  case Plsc
  case Cca(regularization: CcaRegularization)
  case ReducedRankRegression(
      direction: RegressionDirection,
      regularization: RegressionRegularization
  )

  def label: String =
    this match
      case Plsc                             => "plsc"
      case Cca(_)                           => "cca"
      case ReducedRankRegression(_, _)      => "rrr"
