package scalafim.surface.view

import intaglio.*

enum SurfaceCurvatureDisplayError:
  case InvalidBrightness(value: Double)
  case InvalidContrast(value: Double)
  case InvalidSmoothness(value: Double)

/** Explicit anatomy contrast. Positive curvature is dark by default, matching
  * SurfViewJS. The older curvatureUnderlay factory retains its original policy.
  */
final case class SurfaceCurvatureDisplay private (
  brightness: Double,
  contrast: Double,
  smoothness: Double,
  polarity: SurfaceLayer.SulcalPolarity
):
  /** Inspectable mapping of the original curvature values, with saturated tails.
    * Extra knots retain the clipping points when contrast leaves the gamut.
    */
  def mapping: ScalarMapping =
    val sign = if polarity == SurfaceLayer.SulcalPolarity.PositiveIsSulcal then -1.0 else 1.0
    val crossing = if contrast == 0 then Vector.empty else
      Vector((0.0 - brightness) / (sign * contrast) + .5, (1.0 - brightness) / (sign * contrast) + .5)
        .filter(t => t > 0 && t < 1)
    val stops = (Vector(0.0, .5, 1.0) ++ crossing).distinct.sorted.map: t =>
      val value = (brightness + sign * contrast * (t - .5)).max(0).min(1)
      val channel = math.round(value * 255).toInt
      t -> Rgba32.unsafe(channel, channel, channel)
    ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-smoothness / 2, smoothness / 2),
      ScalarRamp.make(stops).toOption.get))

object SurfaceCurvatureDisplay:
  def make(brightness: Double = .5, contrast: Double = .5, smoothness: Double = 1.0,
      polarity: SurfaceLayer.SulcalPolarity = SurfaceLayer.SulcalPolarity.PositiveIsSulcal): Either[SurfaceCurvatureDisplayError, SurfaceCurvatureDisplay] =
    if !brightness.isFinite || brightness < 0 || brightness > 1 then Left(SurfaceCurvatureDisplayError.InvalidBrightness(brightness))
    else if !contrast.isFinite || contrast < 0 then Left(SurfaceCurvatureDisplayError.InvalidContrast(contrast))
    else if !smoothness.isFinite || smoothness <= 0 || smoothness / 2 == 0 then Left(SurfaceCurvatureDisplayError.InvalidSmoothness(smoothness))
    else Right(new SurfaceCurvatureDisplay(brightness, contrast, smoothness, polarity))

  val Default: SurfaceCurvatureDisplay = make().toOption.get
