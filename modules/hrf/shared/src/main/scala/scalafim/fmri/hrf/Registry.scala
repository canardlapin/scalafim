package scalafim.fmri.hrf

import scalafim.fmri.hrf.{Hrf, Seconds, s}

object Registry:

  private val names = Vector(
    "spmg1", "spmg2", "spmg3",
    "gamma", "gaussian", "lwu", "mexhat", "inv_logit", "half_cosine",
    "fir", "bspline", "tent", "fourier", "daguerre", "sine",
    "boxcar", "weighted"
  )

  def listAvailable: Vector[String] = names

  def get(
      name: String,
      nbasis: Int = 1,
      span: Seconds = 24.s,
      lag: Seconds = 0.0.s,
      width: Seconds = 0.0.s,
      precision: Seconds = 0.1.s,
      summate: Boolean = true,
      normalize: Boolean = false
  ): Hrf =
    val base =
      name.toLowerCase match
        case "spmg1" => Hrfs.spmg1(span = span)
        case "spmg2" => Hrfs.SPMG2
        case "spmg3" => Hrfs.SPMG3
        case "gamma" | "gam" => Hrfs.gamma(span = span)
        case "gaussian" => Hrfs.gaussian(span = span)
        case "lwu" => Hrfs.lwu(span = span)
        case "mexhat" => Hrfs.mexhat(span = span)
        case "inv_logit" => Hrfs.invLogit(span = span)
        case "half_cosine" => Hrfs.halfCosine()
        case "fir" => Hrfs.fir(nBasis = nbasis, span = span)
        case "bspline" | "bs" => Hrfs.bspline(nBasis = nbasis, span = span)
        case "tent" => Hrfs.tent(nBasis = nbasis, span = span)
        case "fourier" => Hrfs.fourier(nBasis = nbasis, span = span)
        case "daguerre" => Hrfs.daguerre(nBasis = nbasis, span = span)
        case "sine" => Hrfs.sine(nBasis = nbasis, span = span)
        case other => throw new IllegalArgumentException(s"Unknown HRF '$other'")

    HrfCombinators.gen(
      base = base,
      lag = lag,
      width = width,
      precision = precision,
      summate = summate,
      normalize = normalize
    )
