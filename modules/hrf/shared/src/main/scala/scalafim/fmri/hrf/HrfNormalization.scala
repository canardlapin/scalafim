package scalafim.fmri.hrf

import scalafim.fmri.hrf.linalg.Vec

/** Fixed HRF scaling conventions.
  *
  * Every non-identity mode computes its scale once on a declared reference
  * grid. Later evaluation points therefore cannot change coefficient units.
  * The canonical modes apply the first basis column's scale to every column;
  * only [[UnitPeakPerBasis]] changes their relative scale.
  */
enum HrfNormalization(val label: String):
  case None extends HrfNormalization("none")
  case Spm extends HrfNormalization("spm")
  case UnitPeak extends HrfNormalization("unit_peak")
  case UnitIntegral extends HrfNormalization("unit_integral")
  case UnitPeakPerBasis extends HrfNormalization("unit_peak_per_basis")

object HrfNormalization:
  def fromString(value: String): Either[HrfNormalizationError, HrfNormalization] =
    val normalized = value.trim.toLowerCase
    HrfNormalization.values.find(_.label == normalized) match
      case scala.Some(mode) => Right(mode)
      case scala.None       => Left(HrfNormalizationError.UnknownMode(value, HrfNormalization.values.toVector.map(_.label)))

enum HrfNormalizationError:
  case UnknownMode(value: String, available: Vector[String])
  case ConflictingModes
  case InvalidReferenceSpan(hrfName: String, span: Seconds)
  case BasisWidthMismatch(hrfName: String, expected: Int, actual: Int)
  case NonFiniteReferenceValue(
      hrfName: String,
      mode: HrfNormalization,
      sampleIndex: Int,
      basisIndex: Int,
      value: Double
  )
  case UnusableFactor(
      hrfName: String,
      mode: HrfNormalization,
      basisIndex: Int,
      value: Double
  )

  def message: String =
    this match
      case UnknownMode(value, available) =>
        s"Unknown HRF normalization '$value' (available: ${available.mkString(", ")})"
      case ConflictingModes =>
        "Use either legacy per-basis normalization or a fixed normalization mode, not both"
      case InvalidReferenceSpan(hrfName, span) =>
        s"HRF '$hrfName' has an invalid normalization reference span ${span.value}"
      case BasisWidthMismatch(hrfName, expected, actual) =>
        s"HRF '$hrfName' returned $actual basis values during normalization, expected $expected"
      case NonFiniteReferenceValue(hrfName, mode, sampleIndex, basisIndex, value) =>
        s"HRF '$hrfName' returned non-finite value $value for ${mode.label} normalization at sample ${sampleIndex + 1}, basis ${basisIndex + 1}"
      case UnusableFactor(hrfName, mode, basisIndex, value) =>
        s"HRF '$hrfName' has unusable ${mode.label} normalization factor $value for basis ${basisIndex + 1}"

private[hrf] object HrfNormalizer:

  private val FixedSamplesPerSecond = 50.0
  private val SpmReferenceEnd = 32.0
  private val SpmReferenceSamples = 1600
  private val MinimumNormalFactor = 2.2250738585072014e-308

  def withTransform(
      hrf: Hrf,
      mode: HrfNormalization
  ): Either[HrfNormalizationError, TransformedBasis] =
    mode match
      case HrfNormalization.None =>
        Right(TransformedBasis(hrf, BasisTransform.Identity(hrf.nbasis)))
      case _ =>
        scales(hrf, mode).map { scale =>
          val name = s"${hrf.name}[norm=${mode.label}]"
          val descriptor = hrf.descriptor.derived(name, span = hrf.span)
          val normalized =
            Hrf.of(
              name,
              nbasis = hrf.nbasis,
              span = hrf.span,
              descriptor = Some(descriptor),
              support = hrf.support
            ) { lag =>
              val raw = hrf(lag).data
              val out = new Array[Double](hrf.nbasis)
              var column = 0
              while column < hrf.nbasis do
                out(column) = raw(column) / scale(column)
                column += 1
              Vec.unsafe(out)
            }
          TransformedBasis(normalized, BasisTransform.Diagonal(scale.toVector))
        }

  private def scales(
      hrf: Hrf,
      mode: HrfNormalization
  ): Either[HrfNormalizationError, Array[Double]] =
    val span = hrf.span.value
    if !span.isFinite || span < 0.0 then
      Left(HrfNormalizationError.InvalidReferenceSpan(hrf.name, hrf.span))
    else
      val (sampleCount, end) =
        mode match
          case HrfNormalization.Spm =>
            (SpmReferenceSamples, SpmReferenceEnd)
          case _ =>
            (math.max(math.round(span * FixedSamplesPerSecond).toInt + 1, 2), span)
      val step = end / (sampleCount - 1).toDouble
      referenceFactors(hrf, mode, sampleCount, step)

  private def referenceFactors(
      hrf: Hrf,
      mode: HrfNormalization,
      sampleCount: Int,
      step: Double
  ): Either[HrfNormalizationError, Array[Double]] =
    val perBasis = mode == HrfNormalization.UnitPeakPerBasis
    val factors = Array.fill(if perBasis then hrf.nbasis else 1)(0.0)
    var previousCanonical = 0.0
    var sample = 0
    while sample < sampleCount do
      val values = hrf(Lag(sample.toDouble * step)).data
      if values.length != hrf.nbasis then
        return Left(HrfNormalizationError.BasisWidthMismatch(hrf.name, hrf.nbasis, values.length))

      val columnsToInspect = if perBasis then hrf.nbasis else 1
      var column = 0
      while column < columnsToInspect do
        val value = values(column)
        if !value.isFinite then
          return Left(HrfNormalizationError.NonFiniteReferenceValue(hrf.name, mode, sample, column, value))
        column += 1

      mode match
        case HrfNormalization.Spm =>
          factors(0) += values(0)
        case HrfNormalization.UnitPeak =>
          factors(0) = math.max(factors(0), math.abs(values(0)))
        case HrfNormalization.UnitIntegral =>
          if sample > 0 then factors(0) += step * (previousCanonical + values(0)) / 2.0
          previousCanonical = values(0)
        case HrfNormalization.UnitPeakPerBasis =>
          column = 0
          while column < hrf.nbasis do
            factors(column) = math.max(factors(column), math.abs(values(column)))
            column += 1
        case HrfNormalization.None =>
          ()
      sample += 1

    if perBasis then
      var column = 0
      while column < factors.length do
        if factors(column) == 0.0 then factors(column) = 1.0
        column += 1
      Right(factors)
    else
      val factor = factors(0)
      if !factor.isFinite || math.abs(factor) < MinimumNormalFactor then
        Left(HrfNormalizationError.UnusableFactor(hrf.name, mode, 0, factor))
      else Right(Array.fill(hrf.nbasis)(factor))
