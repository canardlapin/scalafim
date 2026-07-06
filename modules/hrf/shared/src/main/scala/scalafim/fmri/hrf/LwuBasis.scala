package scalafim.fmri.hrf

import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.linalg.Mat

final case class LwuParams(tau: Double, sigma: Double, rho: Double)

object LwuBasis:

  def apply(
      theta0: LwuParams,
      times: Seq[Seconds],
      normalizePrimary: Boolean = false,
      delta: Double = 1e-4
  ): Mat =
    val n = times.length
    val out = new Array[Double](n * 4)
    var i = 0
    var maxAbs = 0.0
    while i < n do
      val t = times(i)
      val h0 = HrfFunctions.lwu(t, theta0.tau, theta0.sigma, theta0.rho, HrfFunctions.LwuNormalize.None)
      out(i * 4) = h0
      val a = math.abs(h0)
      if a > maxAbs then maxAbs = a
      i += 1

    val scale = if normalizePrimary && maxAbs > 1e-10 then maxAbs else 1.0

    i = 0
    while i < n do
      val t = times(i)
      val tauP = theta0.tau + delta
      val tauM = theta0.tau - delta
      val sigP = theta0.sigma + delta
      val sigM = theta0.sigma - delta
      val rhoP = theta0.rho + delta
      val rhoM = theta0.rho - delta

      val dTau =
        (HrfFunctions.lwu(t, tauP, theta0.sigma, theta0.rho) -
          HrfFunctions.lwu(t, tauM, theta0.sigma, theta0.rho)) / (2.0 * delta)
      val dSigma =
        (HrfFunctions.lwu(t, theta0.tau, sigP, theta0.rho) -
          HrfFunctions.lwu(t, theta0.tau, sigM, theta0.rho)) / (2.0 * delta)
      val dRho =
        (HrfFunctions.lwu(t, theta0.tau, theta0.sigma, rhoP) -
          HrfFunctions.lwu(t, theta0.tau, theta0.sigma, rhoM)) / (2.0 * delta)

      out(i * 4) /= scale
      out(i * 4 + 1) = dTau
      out(i * 4 + 2) = dSigma
      out(i * 4 + 3) = dRho
      i += 1

    Mat.unsafe(n, 4, out)
