package scalafim.surface.view

import intaglio.*

enum SurfaceLegendPublicationError:
  case Scene(error: SurfaceViewError)
  case Binding(error: SurfaceLegendError)
  case Drawing(error: LegendError)
  case InsufficientSpace(requiredHeightPt: Double, availableHeightPt: Double)
  case ImageDimensions(expected: RasterDimensions, actual: RasterDimensions)

  def message: String = this match
    case Scene(error) => error.message
    case Binding(error) => error.message
    case Drawing(error) => error.message
    case InsufficientSpace(required, available) => s"publication needs $required points of legend height; $available available"
    case ImageDimensions(expected, actual) => s"surface image dimensions $actual differ from the reserved $expected"

/** Coordinates in physical points from the page's top-left corner. */
final case class SurfacePublicationFrame(leftPt: Double, topPt: Double, widthPt: Double, heightPt: Double)

final case class SurfacePublicationLegendReceipt(
  canonicalKey: String,
  sources: Vector[SurfaceLegendSource],
  frame: SurfacePublicationFrame
)

final case class SurfaceLegendPublicationReceipt(
  publication: SurfacePublicationReceipt,
  surface: SurfaceRenderReceipt,
  surfaceFrame: SurfacePublicationFrame,
  legends: Vector[SurfacePublicationLegendReceipt]
)

final case class SurfaceLegendPublicationArtifact(scene: Scene, receipt: SurfaceLegendPublicationReceipt)

/** Render exactly the supplied plan at the supplied dimensions and background.
  * Backends own pixel generation; the publication layer owns composition.
  */
final case class SurfacePublicationRenderInput(plan: SurfaceRenderPlan, dimensions: RasterDimensions, background: Rgba32)

final class SurfaceLegendPublication private (
  val input: SurfacePublicationRenderInput,
  val receipt: SurfaceLegendPublicationReceipt,
  private val chrome: Scene
):
  /** A callback binds rendering to the prepared scene instead of accepting an
    * unrelated cached image. The returned dimensions are checked before export.
    */
  def renderWith[E](render: SurfacePublicationRenderInput => Either[E, RasterImage])
      : Either[E | SurfaceLegendPublicationError, SurfaceLegendPublicationArtifact] =
    render(input).flatMap: image =>
      if image.dimensions != input.dimensions then
        Left(SurfaceLegendPublicationError.ImageDimensions(input.dimensions, image.dimensions))
      else
        val frame = receipt.surfaceFrame
        val pageHeight = receipt.publication.height * 0.75
        val imageGrob = Grob.imageUnsafe(image,
          SurfaceLegendPublication.point(frame.leftPt, pageHeight - frame.topPt - frame.heightPt),
          SurfaceLegendPublication.size(frame.widthPt, frame.heightPt), anchor = Anchor.BottomLeft,
          interpolation = RasterInterpolation.Nearest, name = Some(GraphicsName.unsafe("publication-surface")))
        // The prepared image uses one pixel per output pixel; no resampling.
        Right(SurfaceLegendPublicationArtifact(Scene(Vector(chrome.grobs.head, imageGrob) ++ chrome.grobs.tail), receipt))

object SurfaceLegendPublication:
  private final case class Card(scene: Scene, widthPt: Double, heightPt: Double)
  private[view] def point(x: Double, y: Double): Point = Point(LengthExpr(Length.pointsUnsafe(x)), LengthExpr(Length.pointsUnsafe(y)))
  private[view] def size(width: Double, height: Double): Size = Size.fromExtents(ExtentExpr.pointsUnsafe(width), ExtentExpr.pointsUnsafe(height))

  /** Reserve a right-hand legend column before requesting any surface pixels.
    * Existing `SurfacePublication.decorate` callers keep their original output.
    * Legacy swatches supplied here become an explicit manual card.
    */
  def prepare(model: SurfaceViewerModel, state: SurfaceViewerState, spec: SurfacePublicationSpec,
    requests: Vector[SurfaceLegendRequest], metrics: TextMetrics = TextMetrics.estimate)
      : Either[SurfaceLegendPublicationError, SurfaceLegendPublication] =
    val preset = spec.preset
    val width = preset.width * 0.75
    val height = preset.height * 0.75
    val margin = math.min(width, height) * preset.marginNpc
    val gap = preset.fontPoints
    val contentWidth = width - margin * 2
    val allRequests = requests ++ Option.when(spec.legend.nonEmpty)(
      SurfaceLegendRequest.Manual(LegendTitle.make("Legend").toOption.get, spec.legend))
    val columnWidth = if allRequests.isEmpty then 0.0 else contentWidth * 0.36
    val font = preset.fontPoints * 0.65
    val scalarStyle = ScalarLegendStyle(widthPt = math.max(columnWidth, 200), fontPt = font,
      barHeightPt = font * 1.6, marginPt = font * 0.5, gapPt = font * 0.5, metrics = metrics)
    val swatchStyle = SwatchLegendStyle(widthPt = math.max(columnWidth, 200), fontPt = font,
      swatchPt = font * 1.6, marginPt = font * 0.5, gapPt = font * 0.5, metrics = metrics)
    for
      plan <- SurfaceCompiler.compile(model, state).left.map(SurfaceLegendPublicationError.Scene.apply)
      legends <- traverse(allRequests)(request => SurfaceLegend.bind(request, model, state).left.map(SurfaceLegendPublicationError.Binding.apply))
      title <- LegendTextLayout.wrap(spec.title.getOrElse(""), contentWidth, preset.fontPoints, metrics = metrics)
        .left.map(SurfaceLegendPublicationError.Drawing.apply)
      orientation <- LegendTextLayout.wrap(spec.orientation.label, contentWidth, preset.fontPoints, metrics = metrics)
        .left.map(SurfaceLegendPublicationError.Drawing.apply)
      cards <- traverse(legends)(legend => draw(legend, scalarStyle, swatchStyle))
      top = math.ceil((margin + title.heightPt + (if title.lines.isEmpty then 0 else gap)) / 0.75) * 0.75
      bottom = height - margin - orientation.heightPt - gap
      available = bottom - top
      required = cards.map(_.heightPt).sum + math.max(0, cards.length - 1) * gap
      _ <- if available >= 0.75 && required <= available then Right(())
        else Left(SurfaceLegendPublicationError.InsufficientSpace(required, available))
    yield
      val surfaceWidth = math.floor((contentWidth - columnWidth - (if cards.isEmpty then 0 else gap)) / 0.75).toInt
      val surfaceHeight = math.floor(available / 0.75).toInt
      val dimensions = RasterDimensions.unsafe(surfaceWidth, surfaceHeight)
      val frame = SurfacePublicationFrame(margin, top, surfaceWidth * 0.75, surfaceHeight * 0.75)
      val grobs = Vector.newBuilder[Grob]
      val background = Rgba.unsafe(spec.background.red, spec.background.green, spec.background.blue, spec.background.alpha.toDouble / 255)
      grobs += Grob.rectUnsafe(Point.npcUnsafe(0, 0), Size.npcUnsafe(1, 1), anchor = Anchor.BottomLeft,
        gp = GraphicParams.unsafe(stroke = None, fill = Some(background)), name = Some(GraphicsName.unsafe("publication-background")))
      val textGp = GraphicParams.unsafe(stroke = Some(Rgba.Black), fontSize = Length.pointsUnsafe(preset.fontPoints), fontFamily = Some("sans-serif"))
      def text(layout: LegendTextLayout, start: Double, name: String): Unit =
        layout.lines.zipWithIndex.foreach: (line, i) =>
          grobs += Grob.textUnsafe(line, point(width / 2, height - start - i * layout.lineHeightPt),
            anchor = Anchor(HJust.Center, VJust.Top), gp = textGp, name = Some(GraphicsName.unsafe(s"$name-$i")))
      text(title, margin, "publication-title")
      text(orientation, height - margin - orientation.heightPt, "publication-orientation")
      var nextTop = top
      val legendReceipts = cards.zip(legends).zipWithIndex.map: (pair, index) =>
        val (card, legend) = pair
        val cardFrame = SurfacePublicationFrame(width - margin - card.widthPt, nextTop, card.widthPt, card.heightPt)
        grobs += Grob.group(card.scene.grobs, viewport = Some(Viewport.unsafe(
          origin = point(cardFrame.leftPt, height - nextTop - card.heightPt), size = size(card.widthPt, card.heightPt), clip = Clip.Off)),
          name = Some(GraphicsName.unsafe(s"publication-legend-$index")))
        nextTop += card.heightPt + gap
        SurfacePublicationLegendReceipt(legend.canonicalKey, legend.sources, cardFrame)
      val chrome = Scene(grobs.result())
      val publicationReceipt = SurfacePublicationReceipt(preset, preset.width, preset.height, plan.receipt.cameraKey,
        plan.receipt.layerKeys, plan.receipt.timepoint, chrome.size, spec.orientation, spec.title.map(_.trim),
        legends.flatMap(labels), spec.background)
      new SurfaceLegendPublication(SurfacePublicationRenderInput(plan, dimensions, spec.background),
        SurfaceLegendPublicationReceipt(publicationReceipt, plan.receipt, frame, legendReceipts), chrome)

  private def labels(legend: SurfaceLegend): Vector[String] = legend.content match
    case SurfaceLegendContent.Continuous(value) => Vector(value.title.text) ++ value.ticks.map(_.label)
    case SurfaceLegendContent.Split(value) => Vector(value.title.text) ++ value.ticks.map(_.label)
    case SurfaceLegendContent.Categorical(title, entries, fallback) => Vector(title.text) ++ entries.map(_.label) :+ fallback.label
    case SurfaceLegendContent.Manual(title, entries) => Vector(title.text) ++ entries.map(_.label)

  private def draw(legend: SurfaceLegend, scalarStyle: ScalarLegendStyle, swatchStyle: SwatchLegendStyle)
      : Either[SurfaceLegendPublicationError, Card] =
    val drawing = legend.content match
      case SurfaceLegendContent.Continuous(calibration) =>
        ScalarLegendDrawing.draw(calibration, scalarStyle, legend.notes.filterNot(calibration.notes.contains))
          .map(value => Card(value.scene, value.widthPt, value.heightPt))
      case SurfaceLegendContent.Split(calibration) =>
        ScalarLegendDrawing.draw(calibration, scalarStyle, legend.notes.filterNot(calibration.notes.contains))
          .map(value => Card(value.scene, value.widthPt, value.heightPt))
      case SurfaceLegendContent.Categorical(title, entries, fallback) =>
        SwatchLegendDrawing.draw(title, entries.map(e => e.label -> e.color) :+ (fallback.label -> fallback.color), legend.notes, swatchStyle)
          .map(value => Card(value.scene, value.widthPt, value.heightPt))
      case SurfaceLegendContent.Manual(title, entries) =>
        SwatchLegendDrawing.draw(title, entries.map(e => e.label -> e.color), legend.notes, swatchStyle)
          .map(value => Card(value.scene, value.widthPt, value.heightPt))
    drawing.left.map(SurfaceLegendPublicationError.Drawing.apply)

  private def traverse[A, B](values: Vector[A])(f: A => Either[SurfaceLegendPublicationError, B])
      : Either[SurfaceLegendPublicationError, Vector[B]] =
    values.foldLeft[Either[SurfaceLegendPublicationError, Vector[B]]](Right(Vector.empty)): (result, value) =>
      for previous <- result; next <- f(value) yield previous :+ next
