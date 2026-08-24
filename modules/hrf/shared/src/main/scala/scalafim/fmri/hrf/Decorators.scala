package scalafim.fmri.hrf

import scalafim.fmri.hrf.{Hrf, Seconds, s}
import scalafim.fmri.hrf.linalg.{Mat, Vec}

object HrfCombinators:

  /** Contract `hrf` with `coeffs` without checking basis identity.
    *
    * The reconstructed kernel keeps the source basis in its descriptor
    * (`HrfParams.Coefficients`), so provenance survives the contraction.
    */
  private[hrf] def withCoefficientsUnchecked(
      hrf: Hrf,
      coeffs: Array[Double],
      name: Option[String]
  ): Hrf =
    val nm = name.getOrElse(s"${hrf.name}_from_coef")
    val weights = coeffs.clone
    val descriptor =
      HrfDescriptor.derived(
        name = nm,
        nbasis = 1,
        span = hrf.span,
        params = HrfParams.Coefficients(hrf.name, weights.toVector),
        components = Vector(hrf.descriptor)
      )
    Hrf.of(nm, nbasis = 1, span = hrf.span, descriptor = Some(descriptor), support = hrf.support) { t =>
      val v = hrf(t).data
      var acc = 0.0
      var i = 0
      while i < weights.length do
        acc += v(i) * weights(i)
        i += 1
      Vec.unsafe(Array(acc))
    }

  def bindBasis(hrfs: Seq[Hrf], name: Option[String] = None): Hrf =
    require(hrfs.nonEmpty, "bindBasis requires at least one HRF")
    if hrfs.length == 1 then hrfs.head
    else
      val nb = hrfs.map(_.nbasis).sum
      val sp = hrfs.map(_.span).max
      val nm = name.getOrElse(hrfs.map(_.name).mkString(" + "))
      val descriptor = HrfDescriptor.composite(nm, hrfs.toVector.map(_.descriptor), sp)
      val support = Support.union(hrfs.map(_.support))
      Hrf.of(nm, nbasis = nb, span = sp, descriptor = Some(descriptor), support = support) { t =>
        val out = new Array[Double](nb)
        var offset = 0
        var i = 0
        while i < hrfs.length do
          val v = hrfs(i)(t).data
          System.arraycopy(v, 0, out, offset, v.length)
          offset += v.length
          i += 1
        Vec.unsafe(out)
      }

  extension (hrf: Hrf)

    def lag(lag: Seconds): Hrf =
      require(lag.value.isFinite, "`lag` must be finite")
      if lag.value == 0.0 then hrf
      else
        val newSpan = if lag.value > 0.0 then hrf.span + lag else hrf.span
        val nm = s"${hrf.name}_lag(${lag.value})"
        val descriptor = hrf.descriptor.derived(nm, span = newSpan)
        Hrf.of(
          nm,
          nbasis = hrf.nbasis,
          span = newSpan,
          descriptor = Some(descriptor),
          support = hrf.support.shifted(lag)
        ) { t =>
          hrf(t.rewound(lag))
        }

    def normalize(dt: Seconds = 0.1.s): Hrf =
      normalizeWithTransform(dt).basis

    /** Normalize once on the fixed reference grid declared by `mode`.
      *
      * Unlike the legacy resolution-taking overload, this operation reports
      * unusable scales explicitly and makes the normalization convention part
      * of the call's type rather than a string or boolean flag.
      */
    def normalize(mode: HrfNormalization): Either[HrfNormalizationError, Hrf] =
      normalizeWithTransform(mode).map(_.basis)

    /** Peak-normalize each column, returning the gauge change that did it.
      *
      * Rescaling a basis does not change the space it spans, but it does change
      * the units of every coefficient and contrast attached to it, and it
      * invalidates any quadratic penalty. Returning the [[BasisTransform]] is
      * what lets a caller transport those instead of silently keeping stale
      * ones — see `Hrf.penaltyMatrixFor`.
      *
      * The scales are computed over the kernel's own `[0, span]` at resolution
      * `dt`, so they do not depend on where the caller later samples.
      */
    def normalizeWithTransform(dt: Seconds = 0.1.s): TransformedBasis =
      require(dt.value.isFinite && dt.value > 0.0, "`dt` must be finite and > 0")
      val nSamples = math.ceil(hrf.span.value / dt.value).toInt + 1
      val maxAbs = Array.fill(hrf.nbasis)(0.0)
      var i = 0
      while i < nSamples do
        val t = Lag(i * dt.value)
        val v = hrf(t).data
        var j = 0
        while j < hrf.nbasis do
          val a = math.abs(v(j))
          if a > maxAbs(j) then maxAbs(j) = a
          j += 1
        i += 1
      val scales = maxAbs.map(m => if m > 1e-10 then m else 1.0)
      val nm = s"${hrf.name}_norm"
      val descriptor = hrf.descriptor.derived(nm, span = hrf.span)
      val normalized =
        Hrf.of(nm, nbasis = hrf.nbasis, span = hrf.span, descriptor = Some(descriptor), support = hrf.support) { t =>
          val v = hrf(t).data
          Vec.unsafe(v.zip(scales).map(_ / _))
        }
      TransformedBasis(normalized, BasisTransform.Diagonal(scales.toVector))

    /** Fixed-reference normalization together with its gauge transform. */
    def normalizeWithTransform(
        mode: HrfNormalization
    ): Either[HrfNormalizationError, TransformedBasis] =
      HrfNormalizer.withTransform(hrf, mode)

    def block(
        width: Seconds,
        precision: Seconds = 0.1.s,
        halfLife: Double = Double.PositiveInfinity,
        summate: Boolean = true,
        normalize: Boolean = false,
        integration: Integration = Integration.Exact
    ): Hrf =
      require(width.value.isFinite, "`width` must be finite")
      require(precision.value.isFinite && precision.value > 0.0, "`precision` must be finite and > 0")
      require(!halfLife.isNaN, "`halfLife` must be finite or infinite")
      if halfLife.isFinite then require(halfLife > 0.0, "`halfLife` must be > 0")
      if width.value <= 0.0 then hrf
      else
        val dt = precision.value
        // Trapezoid quadrature: the blocked kernel is the integral
        // `∫₀^w h(t-u) e^{-ln2 · u / halfLife} du`, so it converges as `dt`
        // shrinks rather than scaling with `w / dt`.
        val (offsets, boxWeights) = Quadrature.boxOffsets(width.value, dt)
        val weights = if summate then boxWeights else Quadrature.normalized(boxWeights)
        val nOffsets = offsets.length
        val decays =
          if halfLife.isInfinite then Array.fill(nOffsets)(1.0)
          else offsets.map(o => math.exp(-math.log(2.0) * o / halfLife))

        // Without decay the blocked kernel is exactly the box response, so it
        // can go through the same primitive the evaluator uses. With a finite
        // half-life the integrand is `h(t-u)·2^{-u/halfLife}`, which is not a
        // box response at all, so that case stays with quadrature.
        val pulse =
          Pulse
            .fromSummate(width, summate)
            .fold(err => throw new IllegalArgumentException(err.message), identity)

        def rawAt(t: Lag): Array[Double] =
          if halfLife.isInfinite then PulseResponse.at(pulse, hrf, t, precision, integration).data
          else
            val out = Array.fill(hrf.nbasis)(0.0)
            var i = 0
            while i < nOffsets do
              val w = weights(i) * decays(i)
              if w != 0.0 then
                val v = hrf(t - Lag.unsafe(offsets(i))).data
                var j = 0
                while j < hrf.nbasis do
                  out(j) += v(j) * w
                  j += 1
              i += 1
            out

        val newSpan = hrf.span + width

        val scales =
          if !normalize then Array.fill(hrf.nbasis)(1.0)
          else
            val nSamples = math.ceil(newSpan.value / dt).toInt + 1
            val maxAbs = Array.fill(hrf.nbasis)(0.0)
            var k = 0
            while k < nSamples do
              val tt = Lag(k * dt)
              val v = rawAt(tt)
              var j = 0
              while j < hrf.nbasis do
                val a = math.abs(v(j))
                if a > maxAbs(j) then maxAbs(j) = a
                j += 1
              k += 1
            maxAbs.map(m => if m > 1e-10 then m else 1.0)

        val nm = s"${hrf.name}_block(w=${width.value})"
        val descriptor = hrf.descriptor.derived(nm, span = newSpan)
        Hrf.of(
          nm,
          nbasis = hrf.nbasis,
          span = newSpan,
          descriptor = Some(descriptor),
          support = hrf.support.widened(width)
        ) { t =>
          val raw = rawAt(t)
          Vec.unsafe(raw.zip(scales).map(_ / _))
        }

    /** Contract this basis with a coefficient vector.
      *
      * Only the length is checked, so coefficients fitted against a different
      * basis of the same width are accepted silently. Prefer
      * [[ResponseBasis.reconstruct]], where the basis identity is carried in
      * the type.
      */
    def withCoefficients(coeffs: Array[Double], name: Option[String] = None): Hrf =
      require(coeffs.length == hrf.nbasis, s"length(coeffs) must equal nbasis (${hrf.nbasis})")
      withCoefficientsUnchecked(hrf, coeffs, name)

    def evaluate(
        grid: Seq[Lag],
        amplitude: Double = 1.0,
        duration: Seconds = 0.0.s,
        precision: Seconds = 0.2.s,
        summate: Boolean = true,
        normalize: Boolean = false,
        integration: Integration = Integration.Exact
    ): Mat =
      Evaluate(hrf, grid, amplitude, duration, precision, summate, normalize, integration)

    def evaluateDoubles(
        grid: Seq[Double],
        amplitude: Double = 1.0,
        duration: Double = 0.0,
        precision: Double = 0.2,
        summate: Boolean = true,
        normalize: Boolean = false,
        integration: Integration = Integration.Exact
    ): Mat =
      Evaluate.doubles(hrf, grid, amplitude, duration, precision, summate, normalize, integration)

    def deriv(lags: Seq[Lag], eps: Seconds = 1e-4.s): Mat =
      Deriv(hrf, lags, eps)

    def derivDoubles(times: Seq[Double], eps: Double = 1e-4): Mat =
      Deriv.doubles(hrf, times, eps)

    def penaltyMatrix(order: Int = 2, shrinkDeriv: Double = 2.0): Mat =
      Penalty.penaltyMatrix(hrf, order, shrinkDeriv)

    def reconstructionMatrix(times: Seq[Double]): Mat =
      Reconstruction.matrix(hrf, times)

    def toeplitz(time: Seq[Double], len: Int): Mat =
      Toeplitz.matrix(hrf, time, len)

  def gen(
      base: Hrf,
      lag: Seconds = 0.0.s,
      width: Seconds = 0.0.s,
      precision: Seconds = 0.1.s,
      halfLife: Double = Double.PositiveInfinity,
      summate: Boolean = true,
      normalize: Boolean = false,
      name: Option[String] = None,
      span: Option[Seconds] = None,
      normalization: HrfNormalization = HrfNormalization.None
  ): Hrf =
    require(lag.value.isFinite, "`lag` must be finite")
    require(width.value.isFinite, "`width` must be finite")
    require(precision.value.isFinite && precision.value > 0.0, "`precision` must be finite and > 0")
    require(!halfLife.isNaN, "`halfLife` must be finite or infinite")
    if halfLife.isFinite then require(halfLife > 0.0, "`halfLife` must be > 0")
    genEither(base, lag, width, precision, halfLife, summate, normalize, name, span, normalization)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private[hrf] def genEither(
      base: Hrf,
      lag: Seconds,
      width: Seconds,
      precision: Seconds,
      halfLife: Double,
      summate: Boolean,
      normalize: Boolean,
      name: Option[String],
      span: Option[Seconds],
      normalization: HrfNormalization
  ): Either[HrfNormalizationError, Hrf] =
    if normalize && normalization != HrfNormalization.None then
      Left(HrfNormalizationError.ConflictingModes)
    else
      val withBlock = if width.value > 0.0 then base.block(width, precision, halfLife, summate, normalize = false) else base
      val withLag = if lag.value != 0.0 then withBlock.lag(lag) else withBlock
      val normalized =
        if normalize then Right(withLag.normalize(precision))
        else withLag.normalize(normalization)
      normalized.map { withNorm =>
        val renamed =
          name match
            case Some(nm) =>
              val descriptor = withNorm.descriptor.copy(family = HrfFamily.Derived(nm))
              Hrf.of(
                nm,
                nbasis = withNorm.nbasis,
                span = withNorm.span,
                descriptor = Some(descriptor),
                support = withNorm.support
              ) { t => withNorm(t) }
            case None => withNorm
        span match
          case Some(sp) =>
            // An explicit `span` overrides the computational horizon only. It
            // does not claim the kernel is zero past `sp`, so a compact support
            // is narrowed to `sp` and an unbounded one stays unbounded.
            val narrowed =
              renamed.support match
                case Support.Compact(horizon) => Support.Compact(if horizon.value <= sp.value then horizon else sp)
                case Support.Unbounded        => Support.Unbounded
            Hrf.of(
              renamed.name,
              nbasis = renamed.nbasis,
              span = sp,
              descriptor = Some(renamed.descriptor.withSpan(sp)),
              support = narrowed
            ) { t => renamed(t) }
          case None => renamed
      }
