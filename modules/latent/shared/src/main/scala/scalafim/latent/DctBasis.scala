package scalafim.latent

import scalafim.linalg.DoubleMatrix

enum DctNorm(val metadataValue: String):
  case Ortho extends DctNorm("ortho")
  case None extends DctNorm("none")

object DctBasis:
  def build(
      timepoints: Int,
      components: Int,
      norm: DctNorm = DctNorm.Ortho
  ): Either[LatentError, DoubleMatrix] =
    if timepoints <= 0 then Left(LatentError.NonPositiveDimension("timepoints", timepoints))
    else if components <= 0 then Left(LatentError.NonPositiveDimension("DCT components", components))
    else if components > timepoints then Left(LatentError.DimensionMismatch("DCT components", timepoints, components))
    else
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

  def full(
      timepoints: Int,
      norm: DctNorm = DctNorm.Ortho
  ): Either[LatentError, DoubleMatrix] =
    build(timepoints, timepoints, norm)
