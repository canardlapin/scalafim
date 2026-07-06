package scalafim.fmri.ar

final case class ArmaCoefficients(
    phi: Vector[Double],
    theta: Vector[Double] = Vector.empty
):
  require(phi.forall(_.isFinite), "AR coefficients must be finite")
  require(theta.forall(_.isFinite), "MA coefficients must be finite")

  def arOrder: Int = phi.length
  def maOrder: Int = theta.length
  def isIid: Boolean = phi.isEmpty && theta.isEmpty
  def isAr1: Boolean = phi.length == 1 && theta.isEmpty

  def exactAr1FirstScale: Either[ArError, Double] =
    if !isAr1 then Right(1.0)
    else
      val rho = phi.head
      if math.abs(rho) >= 1.0 then Left(ArError.InvalidExactFirstAr1(rho))
      else Right(math.sqrt(1.0 - rho * rho))

object ArmaCoefficients:
  val Iid: ArmaCoefficients = ArmaCoefficients(Vector.empty)

  def ar(phi: Double*): ArmaCoefficients =
    ArmaCoefficients(phi.toVector)

  def arma(phi: Seq[Double], theta: Seq[Double]): ArmaCoefficients =
    ArmaCoefficients(phi.toVector, theta.toVector)
