package scalafim.fmri.hrf

import scalafim.fmri.hrf.{Hrf, Seconds, s}
import scalafim.fmri.hrf.linalg.{Mat, Vec}

object HrfCombinators:

  def bindBasis(hrfs: Seq[Hrf], name: Option[String] = None): Hrf =
    require(hrfs.nonEmpty, "bindBasis requires at least one HRF")
    if hrfs.length == 1 then hrfs.head
    else
      val nb = hrfs.map(_.nbasis).sum
      val sp = hrfs.map(_.span).max
      val nm = name.getOrElse(hrfs.map(_.name).mkString(" + "))
      val descriptor = HrfDescriptor.composite(nm, hrfs.toVector.map(_.descriptor), sp)
      Hrf.of(nm, nbasis = nb, span = sp, descriptor = Some(descriptor)) { t =>
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
        Hrf.of(nm, nbasis = hrf.nbasis, span = newSpan, descriptor = Some(descriptor)) { t =>
          hrf(Seconds(t.value - lag.value))
        }

    def normalize(dt: Seconds = 0.1.s): Hrf =
      require(dt.value.isFinite && dt.value > 0.0, "`dt` must be finite and > 0")
      val nSamples = math.ceil(hrf.span.value / dt.value).toInt + 1
      val maxAbs = Array.fill(hrf.nbasis)(0.0)
      var i = 0
      while i < nSamples do
        val t = Seconds(i * dt.value)
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
      Hrf.of(nm, nbasis = hrf.nbasis, span = hrf.span, descriptor = Some(descriptor)) { t =>
        val v = hrf(t).data
        Vec.unsafe(v.zip(scales).map(_ / _))
      }

    def block(
        width: Seconds,
        precision: Seconds = 0.1.s,
        halfLife: Double = Double.PositiveInfinity,
        summate: Boolean = true,
        normalize: Boolean = false
    ): Hrf =
      require(width.value.isFinite, "`width` must be finite")
      require(precision.value.isFinite && precision.value > 0.0, "`precision` must be finite and > 0")
      require(!halfLife.isNaN, "`halfLife` must be finite or infinite")
      if halfLife.isFinite then require(halfLife > 0.0, "`halfLife` must be > 0")
      if width.value <= 0.0 then hrf
      else
        val dt = precision.value
        val nOffsets = math.floor(width.value / dt).toInt + 1
        val offsets = Array.tabulate(nOffsets)(i => i * dt)
        val decays =
          if halfLife.isInfinite then Array.fill(nOffsets)(1.0)
          else offsets.map(o => math.exp(-math.log(2.0) * o / halfLife))

        def rawAt(t: Seconds): Array[Double] =
          val out = Array.fill(hrf.nbasis)(0.0)
          var i = 0
          while i < nOffsets do
            val shifted = Seconds(t.value - offsets(i))
            val v = hrf(shifted).data
            val d = decays(i)
            if summate || hrf.nbasis > 1 then
              var j = 0
              while j < hrf.nbasis do
                out(j) += v(j) * d
                j += 1
            else
              val vv = v(0) * d
              if vv > out(0) then out(0) = vv
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
              val tt = Seconds(k * dt)
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
        Hrf.of(nm, nbasis = hrf.nbasis, span = newSpan, descriptor = Some(descriptor)) { t =>
          val raw = rawAt(t)
          Vec.unsafe(raw.zip(scales).map(_ / _))
        }

    def withCoefficients(coeffs: Array[Double], name: Option[String] = None): Hrf =
      require(coeffs.length == hrf.nbasis, s"length(coeffs) must equal nbasis (${hrf.nbasis})")
      val nm = name.getOrElse(s"${hrf.name}_from_coef")
      val descriptor =
        HrfDescriptor.derived(
          name = nm,
          nbasis = 1,
          span = hrf.span,
          params = HrfParams.Coefficients(hrf.name, coeffs.toVector),
          components = Vector(hrf.descriptor)
        )
      Hrf.of(nm, nbasis = 1, span = hrf.span, descriptor = Some(descriptor)) { t =>
        val v = hrf(t).data
        var acc = 0.0
        var i = 0
        while i < coeffs.length do
          acc += v(i) * coeffs(i)
          i += 1
        Vec.unsafe(Array(acc))
      }

    def evaluate(
        grid: Seq[Seconds],
        amplitude: Double = 1.0,
        duration: Seconds = 0.0.s,
        precision: Seconds = 0.2.s,
        summate: Boolean = true,
        normalize: Boolean = false
    ): Mat =
      Evaluate(hrf, grid, amplitude, duration, precision, summate, normalize)

    def evaluateDoubles(
        grid: Seq[Double],
        amplitude: Double = 1.0,
        duration: Double = 0.0,
        precision: Double = 0.2,
        summate: Boolean = true,
        normalize: Boolean = false
    ): Mat =
      Evaluate.doubles(hrf, grid, amplitude, duration, precision, summate, normalize)

    def deriv(times: Seq[Seconds], eps: Seconds = 1e-4.s): Mat =
      Deriv(hrf, times, eps)

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
      span: Option[Seconds] = None
  ): Hrf =
    require(lag.value.isFinite, "`lag` must be finite")
    require(width.value.isFinite, "`width` must be finite")
    require(precision.value.isFinite && precision.value > 0.0, "`precision` must be finite and > 0")
    require(!halfLife.isNaN, "`halfLife` must be finite or infinite")
    if halfLife.isFinite then require(halfLife > 0.0, "`halfLife` must be > 0")
    val withBlock = if width.value > 0.0 then base.block(width, precision, halfLife, summate, normalize = false) else base
    val withLag = if lag.value != 0.0 then withBlock.lag(lag) else withBlock
    val withNorm = if normalize then withLag.normalize(precision) else withLag
    val renamed =
      name match
        case Some(nm) =>
          val descriptor = withNorm.descriptor.copy(family = HrfFamily.Derived(nm))
          Hrf.of(nm, nbasis = withNorm.nbasis, span = withNorm.span, descriptor = Some(descriptor)) { t => withNorm(t) }
        case None => withNorm
    span match
      case Some(sp) =>
        Hrf.of(renamed.name, nbasis = renamed.nbasis, span = sp, descriptor = Some(renamed.descriptor.withSpan(sp))) { t => renamed(t) }
      case None     => renamed
