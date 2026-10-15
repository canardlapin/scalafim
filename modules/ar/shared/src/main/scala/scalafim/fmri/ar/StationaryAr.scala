package scalafim.fmri.ar

import gale.linalg.{CholeskyOptions, DMat, Matrix, TriangularSolve, Vec}

/** Stationary moments and the exact initial whitening factor of a causal Gaussian AR(p) process with unit innovation
  * variance.
  *
  * `initial` is `L^-1`, where `L L' = Gamma_p` is the covariance of `p` consecutive values. Applying it to the first
  * `p` values of a contiguous stretch, and the AR recursion to every later value, gives `W Sigma W' = I` exactly.
  * Its leading `k x k` block is the exact factor for a stretch of only `k < p` values. The truncated restart used by
  * [[WhiteningTransform]] (`ExactAr1` scales the first value; higher orders leave it and its successors partially
  * filtered) agrees with this factor only for AR(1).
  */
final case class StationaryArFactor private (phi: Vector[Double], autocovariance: Vector[Double], initial: DMat):
  def order: Int = phi.length

object StationaryArFactor:

  def apply(phi: Vector[Double]): Either[ArError, StationaryArFactor] =
    if phi.isEmpty then Right(new StationaryArFactor(phi, Vector(1.0), Matrix.zeros(0, 0)))
    else
      for
        _ <- Pacf.validateStationary(phi)
        gamma <- autocovariance(phi, phi.length)
        initial <- initialFactor(gamma, phi.length)
      yield new StationaryArFactor(phi, gamma, initial)

  /** Autocovariances at lags `0..maxLag` for unit innovation variance, from the Yule-Walker system for lags `0..p`
    * and the AR recursion beyond.
    */
  def autocovariance(phi: Vector[Double], maxLag: Int): Either[ArError, Vector[Double]] =
    val p = phi.length
    if maxLag < 0 then Left(ArError.InvalidArLag(maxLag))
    else if p == 0 then Right(Vector.tabulate(maxLag + 1)(lag => if lag == 0 then 1.0 else 0.0))
    else
      Pacf.validateStationary(phi).flatMap { _ =>
        val system = Matrix.tabulate(p + 1, p + 1) { (lag, j) =>
          var value = if lag == j then 1.0 else 0.0
          var k = 1
          while k <= p do
            if math.abs(lag - k) == j then value -= phi(k - 1)
            k += 1
          value
        }
        val rhs = Vec.tabulate(p + 1)(lag => if lag == 0 then 1.0 else 0.0)
        system.lu
          .flatMap(_.solve(rhs))
          .left
          .map(error => ArError.StationarityCheckFailed(s"stationary autocovariance solve failed: $error"))
          .map { solved =>
            val out = new Array[Double](math.max(maxLag, p) + 1)
            var lag = 0
            while lag <= p do
              out(lag) = solved(lag)
              lag += 1
            while lag < out.length do
              var value = 0.0
              var k = 1
              while k <= p do
                value += phi(k - 1) * out(lag - k)
                k += 1
              out(lag) = value
              lag += 1
            out.take(maxLag + 1).toVector
          }
      }

  private def initialFactor(gamma: Vector[Double], p: Int): Either[ArError, DMat] =
    val covariance = Matrix.tabulate(p, p)((i, j) => gamma(math.abs(i - j)))
    covariance
      .cholesky(CholeskyOptions())
      .left
      .map(error => ArError.StationarityCheckFailed(s"stationary covariance is not positive definite: $error"))
      .flatMap { factor =>
        val columns = (0 until p).toVector.map { column =>
          TriangularSolve
            .lower(factor.lower, Vec.tabulate(p)(i => if i == column then 1.0 else 0.0))
            .left
            .map(error => ArError.StationarityCheckFailed(s"initial factor solve failed: $error"))
        }
        columns.collectFirst { case Left(error) => error }.toLeft {
          val solved = columns.collect { case Right(value) => value }
          Matrix.tabulate(p, p)((row, column) => solved(column)(row))
        }
      }
