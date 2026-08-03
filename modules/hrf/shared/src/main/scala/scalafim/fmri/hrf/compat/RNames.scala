package scalafim.fmri.hrf.compat

import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.{Design, SamplingFrame}
import scalafim.fmri.hrf.HrfCombinators.*
import scalafim.fmri.hrf.regressor.{NeuralInput, Regressor, RegressorSet}
import scalafim.fmri.hrf.linalg.Mat
import scala.annotation.targetName

object r:

  // ---- HRF constructors / objects ----
  def hrf_gamma(t: Double = 0.0, shape: Double = 6.0, rate: Double = 1.0): Hrf = Hrfs.gamma(shape, rate)
  def hrf_gaussian(mean: Double = 6.0, sd: Double = 2.0): Hrf = Hrfs.gaussian(mean, sd)
  def hrf_spmg1(P1: Double = 5.0, P2: Double = 15.0, A1: Double = 0.0833): Hrf = Hrfs.spmg1(P1, P2, A1)
  def hrf_mexhat(mean: Double = 6.0, sd: Double = 2.0): Hrf = Hrfs.mexhat(mean, sd)
  def hrf_inv_logit(mu1: Double = 6.0, s1: Double = 1.0, mu2: Double = 16.0, s2: Double = 1.0, lag: Double = 0.0): Hrf =
    Hrfs.invLogit(mu1, s1, mu2, s2, lag.s)
  def hrf_half_cosine(h1: Double = 1.0, h2: Double = 5.0, h3: Double = 7.0, h4: Double = 7.0, f1: Double = 0.0, f2: Double = 0.0): Hrf =
    Hrfs.halfCosine(h1.s, h2.s, h3.s, h4.s, f1, f2)
  def hrf_fourier(span: Double = 24.0, nbasis: Int = 5): Hrf = Hrfs.fourier(nbasis, span.s)
  def hrf_daguerre_generator(nbasis: Int = 3, scale: Double = 4.0): Hrf = Hrfs.daguerre(nbasis, scale)
  def hrf_bspline_generator(nbasis: Int = 5, span: Double = 24.0): Hrf = Hrfs.bspline(nbasis, span.s)
  def hrf_tent_generator(nbasis: Int = 5, span: Double = 24.0): Hrf = Hrfs.tent(nbasis, span.s)
  def hrf_fir_generator(nbasis: Int = 12, span: Double = 24.0): Hrf = Hrfs.fir(nbasis, span.s)
  def hrf_lwu(tau: Double = 6.0, sigma: Double = 2.5, rho: Double = 0.35, normalize: String = "none"): Hrf =
    val norm = normalize.toLowerCase match
      case "height" => HrfFunctions.LwuNormalize.Height
      case _        => HrfFunctions.LwuNormalize.None
    Hrfs.lwu(tau, sigma, rho, norm)
  def hrf_boxcar(width: Double, amplitude: Double = 1.0, normalize: Boolean = false): Hrf =
    Hrfs.boxcar(width.s, amplitude, normalize)
  def hrf_weighted(weights: Seq[Double], width: Double = 0.0, times: Seq[Double] = Seq.empty, method: String = "constant", normalize: Boolean = false): Hrf =
    val meth = method.toLowerCase match
      case "linear" => Hrfs.WeightedMethod.Linear
      case _        => Hrfs.WeightedMethod.Constant
    val tOpt: Option[Vector[Seconds]] =
      if times.nonEmpty then Some(times.map(Seconds(_)).toVector) else None
    val wOpt: Option[Seconds] = if tOpt.isEmpty then Some(Seconds(width)) else None
    Hrfs.weighted(weights.toVector, wOpt, tOpt, meth, normalize)

  val HRF_GAMMA: Hrf = Hrfs.Gamma
  val HRF_GAUSSIAN: Hrf = Hrfs.Gaussian
  val HRF_SPMG1: Hrf = Hrfs.SPMG1
  val HRF_SPMG2: Hrf = Hrfs.SPMG2
  val HRF_SPMG3: Hrf = Hrfs.SPMG3
  val HRF_BSPLINE: Hrf = Hrfs.bspline()
  val HRF_FIR: Hrf = Hrfs.fir()

  def bind_basis(hrfs: Hrf*): Hrf = HrfCombinators.bindBasis(hrfs)

  def gen_hrf(
      hrf: Hrf,
      lag: Double = 0.0,
      width: Double = 0.0,
      precision: Double = 0.1,
      half_life: Double = Double.PositiveInfinity,
      summate: Boolean = true,
      normalize: Boolean = false,
      name: Option[String] = None,
      span: Option[Double] = None
  ): Hrf =
    HrfCombinators.gen(
      base = hrf,
      lag = lag.s,
      width = width.s,
      precision = precision.s,
      halfLife = half_life,
      summate = summate,
      normalize = normalize,
      name = name,
      span = span.map(_.s)
    )

  def lag_hrf(hrf: Hrf, lag: Double): Hrf = hrf.lag(lag.s)
  def block_hrf(hrf: Hrf, width: Double, precision: Double = 0.1, half_life: Double = Double.PositiveInfinity, summate: Boolean = true, normalize: Boolean = false): Hrf =
    hrf.block(width.s, precision.s, half_life, summate, normalize)
  def normalise_hrf(hrf: Hrf): Hrf = hrf.normalize(0.1.s)
  def hrf_from_coefficients(hrf: Hrf, h: Seq[Double], name: Option[String] = None): Hrf =
    hrf.withCoefficients(h.toArray, name)

  def empirical_hrf(t: Seq[Double], y: Seq[Double], name: String = "empirical_hrf"): Hrf =
    Hrfs.empirical(t.map(_.s), y, name)

  def gen_empirical_hrf(t: Seq[Double], y: Seq[Double], name: String = "empirical_hrf"): Hrf =
    empirical_hrf(t, y, name)

  def as_hrf(
      f: Double => Double,
      name: String = "custom",
      nbasis: Int = 1,
      span: Double = 24.0
  ): Hrf =
    require(nbasis == 1, "scalar as_hrf requires nbasis=1")
    Hrf.scalar(name, span = span.s)(t => f(t.value))

  def as_hrf_multi(
      f: Double => Array[Double],
      nbasis: Int,
      name: String = "custom",
      span: Double = 24.0
  ): Hrf =
    Hrf.multi(name, nbasis = nbasis, span = span.s)(t => f(t.value))

  def deriv(hrf: Hrf, t: Seq[Double]): Mat =
    Deriv.doubles(hrf, t)

  def hrf_basis_lwu(theta0: Seq[Double], t: Seq[Double], normalize_primary: String = "none"): Mat =
    require(theta0.length == 3, "`theta0` must have length 3")
    val norm = normalize_primary.toLowerCase == "height"
    LwuBasis(LwuParams(theta0(0), theta0(1), theta0(2)), t.map(Lag(_)), norm)

  def list_available_hrfs(): Vector[String] = Registry.listAvailable

  def make_hrf(name: String, lag: Double = 0.0, nbasis: Int = 1): Hrf =
    Registry.get(name, nbasis = nbasis, lag = lag.s)

  def nbasis(hrf: Hrf): Int = hrf.nbasis
  def penalty_matrix(hrf: Hrf, order: Int = 2, shrink_deriv: Double = 2.0): Mat =
    Penalty.penaltyMatrix(hrf, order, shrink_deriv)
  def reconstruction_matrix(hrf: Hrf, times: Seq[Double]): Mat =
    Reconstruction.matrix(hrf, times)
  def hrf_toeplitz(hrf: Hrf, time: Seq[Double], len: Int): Mat =
    Toeplitz.matrix(hrf, time, len)

  // ---- Sampling frame ----
  def sampling_frame(blocklens: Seq[Int], TR: Seq[Double], start_time: Seq[Double] = Seq.empty, precision: Double = 0.1): SamplingFrame =
    SamplingFrame(blocklens, TR, start_time, precision)
  def samples(sframe: SamplingFrame, global: Boolean = false): Vector[Double] =
    sframe.samples(global = global).map(_.value)
  def global_onsets(sframe: SamplingFrame, onsets: Seq[Double], blockids: Seq[Int]): Vector[Double] =
    sframe.globalOnsets(onsets.map(Seconds(_)), blockids).map(_.value)

  // ---- Regressors ----
  def regressor(
      onsets: Seq[Double],
      hrf: Hrf = HRF_SPMG1,
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Double = 40.0,
      summate: Boolean = true
  ): Regressor =
    Regressor(onsets, hrf, duration, amplitude, Some(span), summate)

  def regressor_set(
      onsets: Seq[Double],
      fac: Seq[String],
      hrf: Hrf = HRF_SPMG1,
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Double = 40.0,
      summate: Boolean = true
  ): RegressorSet =
    RegressorSet(onsets, fac, hrf, duration, amplitude, Some(span), summate)

  def evaluate(reg: Regressor, grid: Seq[Double], precision: Double = 0.33, method: String = "conv"): Mat =
    val m = method.toLowerCase match
      case "fft"  => Regressor.EvalMethod.FFT
      case "loop" => Regressor.EvalMethod.Loop
      case _      => Regressor.EvalMethod.Conv
    Regressor.evaluate(reg, grid, precision, m)

  def evaluate(hrf: Hrf, grid: Seq[Double]): Mat =
    hrf.evalDoubles(grid)

  def neural_input(reg: Regressor, start: Double = 0.0, end: Option[Double] = None, resolution: Double = 0.33): (Array[Double], Array[Double]) =
    NeuralInput(reg, start, end, resolution)

  def regressor_design(
      onsets: Seq[Double],
      fac: Seq[String],
      block: Seq[Int],
      sframe: SamplingFrame,
      hrf: Hrf = HRF_SPMG1,
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Double = 40.0,
      precision: Double = 0.33,
      method: String = "conv",
      summate: Boolean = true
  ): Mat =
    val m = method.toLowerCase match
      case "fft"  => Regressor.EvalMethod.FFT
      case "loop" => Regressor.EvalMethod.Loop
      case _      => Regressor.EvalMethod.Conv
    Design.regressorDesign(onsets, fac, block, sframe, hrf, duration, amplitude, span, precision, m, summate)
