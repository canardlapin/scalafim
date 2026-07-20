package scalafim.latent

import scalafim.linalg.DoubleMatrix

enum DctNorm(val metadataValue: String):
  case Ortho extends DctNorm("ortho")
  case None extends DctNorm("none")

opaque type RidgePenalty = Double

object RidgePenalty:
  val Zero: RidgePenalty = 0.0

  def apply(value: Double): Either[LatentError, RidgePenalty] =
    if value < 0.0 || !value.isFinite then Left(LatentError.InvalidParameter("ridge", value))
    else Right(value)

  def unsafe(value: Double): RidgePenalty =
    require(value >= 0.0 && value.isFinite, "ridge penalty must be finite and non-negative")
    value

  extension (penalty: RidgePenalty)
    inline def value: Double = penalty
    def metadataValue: String =
      if penalty == 0.0 then "0.0" else penalty.toString

final class DctSpec private (
    val timepoints: Int,
    val components: Int,
    val norm: DctNorm
):
  def copy(
      timepoints: Int = this.timepoints,
      components: Int = this.components,
      norm: DctNorm = this.norm
  ): Either[LatentError, DctSpec] =
    DctSpec(timepoints, components, norm)

  def withComponents(value: Int): Either[LatentError, DctSpec] =
    copy(components = value)

  def withNorm(value: DctNorm): DctSpec =
    new DctSpec(timepoints, components, value)

  override def equals(other: Any): Boolean =
    other match
      case that: DctSpec =>
        timepoints == that.timepoints &&
          components == that.components &&
          norm == that.norm
      case _ =>
        false

  override def hashCode(): Int =
    (timepoints, components, norm).##

  override def toString: String =
    s"DctSpec(timepoints=$timepoints, components=$components, norm=${norm.metadataValue})"

object DctSpec:
  def apply(
      timepoints: Int,
      components: Int,
      norm: DctNorm = DctNorm.Ortho
  ): Either[LatentError, DctSpec] =
    if timepoints <= 0 then Left(LatentError.NonPositiveDimension("DCT timepoints", timepoints))
    else if components <= 0 then Left(LatentError.NonPositiveDimension("DCT components", components))
    else if components > timepoints then Left(LatentError.DimensionMismatch("DCT components", timepoints, components))
    else Right(new DctSpec(timepoints, components, norm))

  def unsafe(
      timepoints: Int,
      components: Int,
      norm: DctNorm = DctNorm.Ortho
  ): DctSpec =
    apply(timepoints, components, norm).fold(error => throw new IllegalArgumentException(error.message), identity)

  def full(
      timepoints: Int,
      norm: DctNorm = DctNorm.Ortho
  ): Either[LatentError, DctSpec] =
    apply(timepoints, timepoints, norm)

object DctBasis:
  def build(spec: DctSpec): Either[LatentError, DoubleMatrix] =
    buildUnchecked(spec.timepoints, spec.components, spec.norm)

  def build(
      timepoints: Int,
      components: Int,
      norm: DctNorm = DctNorm.Ortho
  ): Either[LatentError, DoubleMatrix] =
    DctSpec(timepoints, components, norm).flatMap(build)

  def full(
      timepoints: Int,
      norm: DctNorm = DctNorm.Ortho
  ): Either[LatentError, DoubleMatrix] =
    build(timepoints, timepoints, norm)

  private def buildUnchecked(
      timepoints: Int,
      components: Int,
      norm: DctNorm
  ): Either[LatentError, DoubleMatrix] =
    val scales = new Array[Double](components)
    var component = 0
    while component < components do
      scales(component) =
        norm match
          case DctNorm.Ortho =>
            if component == 0 then 1.0 / math.sqrt(timepoints.toDouble)
            else math.sqrt(2.0 / timepoints.toDouble)
          case DctNorm.None =>
            1.0
      component += 1

    val out = new Array[Double](timepoints * components)
    var time = 0
    while time < timepoints do
      component = 0
      while component < components do
        val angle =
          math.Pi * (time.toDouble + 0.5) * component.toDouble / timepoints.toDouble
        out(time * components + component) = math.cos(angle) * scales(component)
        component += 1
      time += 1

    Right(DoubleMatrix.unsafe(timepoints, components, out))
