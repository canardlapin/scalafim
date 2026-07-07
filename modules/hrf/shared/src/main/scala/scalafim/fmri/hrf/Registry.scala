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

  def getEither(
      name: String,
      nbasis: Int = 1,
      span: Seconds = 24.s,
      lag: Seconds = 0.0.s,
      width: Seconds = 0.0.s,
      precision: Seconds = 0.1.s,
      summate: Boolean = true,
      normalize: Boolean = false
  ): Either[HrfSpecError, Hrf] =
    HrfSpec
      .fromName(
        name = name,
        nbasis = nbasis,
        span = span,
        lag = lag,
        width = width,
        precision = precision,
        summate = summate,
        normalize = normalize
      )
      .flatMap(_.toLegacyHrf)

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
    getEither(
      name = name,
      nbasis = nbasis,
      span = span,
      lag = lag,
      width = width,
      precision = precision,
      summate = summate,
      normalize = normalize
    ).fold(err => throw new IllegalArgumentException(err.message), identity)
