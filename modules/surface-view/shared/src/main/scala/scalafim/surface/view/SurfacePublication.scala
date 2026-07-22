package scalafim.surface.view

import scalafim.graphics.*

enum SurfacePublicationPreset(
  val width: Int,
  val height: Int,
  val fontPoints: Double,
  val marginNpc: Double
):
  case ManuscriptSingleColumn extends SurfacePublicationPreset(1200, 1200, 18.0, 0.035)
  case ManuscriptDoubleColumn extends SurfacePublicationPreset(2400, 1400, 22.0, 0.025)
  case Poster extends SurfacePublicationPreset(3200, 2400, 30.0, 0.02)

enum SurfaceOrientationMark(val label: String):
  case LeftLateral extends SurfaceOrientationMark("L · lateral")
  case LeftMedial extends SurfaceOrientationMark("L · medial")
  case RightLateral extends SurfaceOrientationMark("R · lateral")
  case RightMedial extends SurfaceOrientationMark("R · medial")
  case Anterior extends SurfaceOrientationMark("anterior")
  case Posterior extends SurfaceOrientationMark("posterior")
  case Dorsal extends SurfaceOrientationMark("dorsal")
  case Ventral extends SurfaceOrientationMark("ventral")
  case Bilateral extends SurfaceOrientationMark("L                                  R")

final case class SurfaceLegendItem private (label: String, color: Rgba32)

object SurfaceLegendItem:
  def make(label: String, color: Rgba32): Either[SurfaceViewError, SurfaceLegendItem] =
    val normalized = label.trim
    if normalized.isEmpty then Left(SurfaceViewError.InvalidPublication("legend labels must be non-empty"))
    else Right(new SurfaceLegendItem(normalized, color))

  def unsafe(label: String, color: Rgba32): SurfaceLegendItem =
    make(label, color).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class SurfacePublicationSpec(
  preset: SurfacePublicationPreset,
  title: Option[String],
  orientation: SurfaceOrientationMark,
  legend: Vector[SurfaceLegendItem] = Vector.empty,
  background: Rgba32 = Rgba32.unsafe(255, 255, 255)
):
  require(title.forall(_.trim.nonEmpty), "publication title must be non-empty when present")

final case class SurfacePublicationReceipt(
  preset: SurfacePublicationPreset,
  width: Int,
  height: Int,
  cameraKey: String,
  layerKeys: Vector[SurfaceResourceKey],
  timepoint: Int,
  chromeGrobs: Int,
  orientation: SurfaceOrientationMark,
  title: Option[String],
  legendLabels: Vector[String],
  background: Rgba32
)

object SurfacePublication:
  def decorate(
    plan: SurfaceRenderPlan,
    spec: SurfacePublicationSpec
  ): (SurfaceRenderPlan, SurfacePublicationReceipt) =
    val chrome = publicationChrome(spec)
    val decorated = plan.copy(chrome = plan.chrome ++ chrome)
    val receipt = SurfacePublicationReceipt(
      spec.preset,
      spec.preset.width,
      spec.preset.height,
      plan.receipt.cameraKey,
      plan.receipt.layerKeys,
      plan.receipt.timepoint,
      decorated.chrome.size,
      spec.orientation,
      spec.title.map(_.trim),
      spec.legend.map(_.label),
      spec.background
    )
    (decorated, receipt)

  /** Place a rendered surface below the same vector chrome used by live
    * viewers. A graphics backend can then produce SVG, Canvas, Java2D, or
    * JavaFX output without a second surface-specific annotation system.
    */
  def compose(image: RasterImage, decoratedPlan: SurfaceRenderPlan): Scene =
    val imageGrob = Grob.imageUnsafe(
      image,
      Point.npcUnsafe(0.0, 0.0),
      Size.npcUnsafe(1.0, 1.0),
      anchor = Anchor.BottomLeft,
      interpolation = RasterInterpolation.Smooth,
      name = Some(GraphicsName.unsafe("surface-publication-image"))
    )
    Scene(Vector(imageGrob)) ++ decoratedPlan.chrome

  private def publicationChrome(spec: SurfacePublicationSpec): Scene =
    val textParams = GraphicParams.unsafe(
      stroke = Some(Rgba.Black),
      fontSize = Length.pointsUnsafe(spec.preset.fontPoints)
    )
    val grobs = Vector.newBuilder[Grob]
    spec.title.foreach: title =>
      grobs += Grob.textUnsafe(
        title.trim,
        Point.npcUnsafe(0.5, 1.0 - spec.preset.marginNpc),
        anchor = Anchor(HJust.Center, VJust.Top),
        gp = textParams,
        name = Some(GraphicsName.unsafe("surface-publication-title"))
      )
    grobs += Grob.textUnsafe(
      spec.orientation.label,
      Point.npcUnsafe(0.5, spec.preset.marginNpc),
      anchor = Anchor(HJust.Center, VJust.Bottom),
      gp = textParams,
      name = Some(GraphicsName.unsafe("surface-orientation"))
    )
    val swatchSize = 0.018
    var index = 0
    while index < spec.legend.length do
      val entry = spec.legend(index)
      val y = 1.0 - spec.preset.marginNpc - (index + 1).toDouble * 0.04
      val fill = Rgba.unsafe(entry.color.red, entry.color.green, entry.color.blue, entry.color.alpha.toDouble / 255.0)
      grobs += Grob.rectUnsafe(
        Point.npcUnsafe(1.0 - spec.preset.marginNpc - 0.12, y),
        Size.npcUnsafe(swatchSize, swatchSize),
        gp = GraphicParams.unsafe(stroke = Some(Rgba.Black), fill = Some(fill)),
        name = Some(GraphicsName.unsafe(s"surface-legend-swatch-$index"))
      )
      grobs += Grob.textUnsafe(
        entry.label,
        Point.npcUnsafe(1.0 - spec.preset.marginNpc - 0.095, y),
        anchor = Anchor(HJust.Left, VJust.Center),
        gp = textParams,
        name = Some(GraphicsName.unsafe(s"surface-legend-label-$index"))
      )
      index += 1
    Scene(grobs.result())
