package scalafim.surface.view

import intaglio.*

enum SurfaceFigureOutputScale(val factor: Int):
  case One extends SurfaceFigureOutputScale(1)
  case Two extends SurfaceFigureOutputScale(2)

enum SurfaceFigurePublicationError:
  case InvalidSpecification(reason: String)
  case Composition(error: SurfaceCompositionError)
  case Binding(error: SurfaceLegendError)
  case Drawing(error: SurfaceLegendPublicationError)
  case InsufficientSpace(requiredHeightPt: Double, availableHeightPt: Double)
  case ImageDimensions(expected: RasterDimensions, actual: RasterDimensions)

  def message: String = this match
    case InvalidSpecification(reason) => reason
    case Composition(error) => error.toString
    case Binding(error) => error.message
    case Drawing(error) => error.message
    case InsufficientSpace(required, available) => s"figure needs $required points of legend height; $available available"
    case ImageDimensions(expected, actual) => s"surface image dimensions $actual differ from the requested $expected"

/** A fixed 1200 by 900 logical-pixel page. Its physical point geometry never
  * changes with output scale, so vector type and panel positions remain stable.
  */
final case class SurfaceFigureSpec private (
  title: Option[String],
  outputScale: SurfaceFigureOutputScale,
  panelLabels: Map[SurfaceViewId, String]
):
  val background: Rgba32 = Rgba32.unsafe(0, 0, 0, 0)

object SurfaceFigureSpec:
  def make(title: Option[String] = None, outputScale: SurfaceFigureOutputScale = SurfaceFigureOutputScale.One,
      panelLabels: Map[SurfaceViewId, String] = Map.empty)
      : Either[SurfaceFigurePublicationError, SurfaceFigureSpec] =
    if title.exists(_.trim.isEmpty) then Left(SurfaceFigurePublicationError.InvalidSpecification("figure title must be non-empty when present"))
    else if panelLabels.values.exists(_.trim.isEmpty) then Left(SurfaceFigurePublicationError.InvalidSpecification("panel labels must be non-empty"))
    else Right(new SurfaceFigureSpec(title.map(_.trim), outputScale, panelLabels.view.mapValues(_.trim).toMap))

final case class SurfaceFigureRenderInput(
  plan: SurfaceCompositionPlan,
  dimensions: RasterDimensions,
  background: Rgba32,
  pixelScale: Double
):
  val pixelsPerInch: Int = (96 * pixelScale).toInt

final case class SurfaceFigureLegendReceipt(canonicalKey: String, sources: Vector[SurfaceLegendSource], frame: SurfacePublicationFrame)

final case class SurfaceFigurePublicationReceipt(
  logicalWidth: Int,
  logicalHeight: Int,
  dimensions: RasterDimensions,
  pixelsPerInch: Int,
  physicalWidthPt: Double,
  physicalHeightPt: Double,
  surfaceFrame: SurfacePublicationFrame,
  composition: SurfaceCompositionReceipt,
  legends: Vector[SurfaceFigureLegendReceipt]
)

final case class SurfaceFigurePublicationArtifact(scene: Scene, receipt: SurfaceFigurePublicationReceipt)

final class SurfaceFigurePublication private (
  val input: SurfaceFigureRenderInput,
  val receipt: SurfaceFigurePublicationReceipt,
  private val chrome: Scene
):
  def renderWith[E](render: SurfaceFigureRenderInput => Either[E, RasterImage])
      : Either[E | SurfaceFigurePublicationError, SurfaceFigurePublicationArtifact] =
    render(input).flatMap: image =>
      if image.dimensions != input.dimensions then
        Left(SurfaceFigurePublicationError.ImageDimensions(input.dimensions, image.dimensions))
      else
        val imageGrob = Grob.imageUnsafe(image, SurfaceLegendPublication.point(0, 0),
          SurfaceLegendPublication.size(receipt.physicalWidthPt, receipt.physicalHeightPt), anchor = Anchor.BottomLeft,
          interpolation = RasterInterpolation.Nearest, name = Some(GraphicsName.unsafe("surface-figure-image")))
        Right(SurfaceFigurePublicationArtifact(Scene(Vector(imageGrob) ++ chrome.grobs), receipt))

object SurfaceFigurePublication:
  val LogicalWidth = 1200
  val LogicalHeight = 900
  private val PointsPerLogicalPixel = 72.0 / 96.0
  private val WidthPt = LogicalWidth * PointsPerLogicalPixel
  private val HeightPt = LogicalHeight * PointsPerLogicalPixel
  private val MarginPt = 14.0
  private val LegendWidthPt = 180.0
  private val GapPt = 8.0
  private val TitleFontPt = 16.0
  private val PanelFontPt = 11.0
  private val PanelLabelHeightPt = PanelFontPt + GapPt

  def prepare[F <: image4s.geometry.Frame[image4s.geometry.D3]](
    model: SurfaceViewerModel,
    state: SurfaceViewerState,
    navigation: SurfaceCompositionState[F],
    spec: SurfaceFigureSpec,
    legendRequests: Vector[SurfaceLegendRequest],
    metrics: TextMetrics = TextMetrics.estimate
  ): Either[SurfaceFigurePublicationError, SurfaceFigurePublication] =
    if navigation.views.exists(_.colorPipeline != state.colorPipeline) then
      Left(SurfaceFigurePublicationError.Composition(SurfaceCompositionError.InvalidPresentation(
        "composition view color pipelines must match the figure state")))
    else
      for
        _ <- if spec.panelLabels.keySet.subsetOf(navigation.views.map(_.id).toSet) then Right(())
          else Left(SurfaceFigurePublicationError.InvalidSpecification("panel labels refer to views outside the composition"))
        plan <- SurfaceCompositionCompiler.compile(model, SurfaceCompositionPresentation.fromViewerState(state), navigation.views)
          .left.map(SurfaceFigurePublicationError.Composition.apply)
        legends <- traverse(legendRequests)(request => SurfaceLegend.bind(request, model, state, plan)
          .left.map(SurfaceFigurePublicationError.Binding.apply))
        cards <- traverse(legends)(legend => SurfaceLegendPublication.draw(legend,
          ScalarLegendStyle(widthPt = LegendWidthPt, fontPt = PanelFontPt, barHeightPt = 14, marginPt = 5, gapPt = 4, metrics = metrics),
          SwatchLegendStyle(widthPt = LegendWidthPt, fontPt = PanelFontPt, swatchPt = 12, marginPt = 5, gapPt = 4, metrics = metrics))
          .left.map(SurfaceFigurePublicationError.Drawing.apply))
        title <- spec.title match
          case None => Right(None)
          case Some(value) => LegendTextLayout.wrap(value, WidthPt - MarginPt * 2, TitleFontPt, metrics = metrics)
            .left.map(error => SurfaceFigurePublicationError.Drawing(SurfaceLegendPublicationError.Drawing(error))).map(Some(_))
        publication <- layout(spec, plan, legends, cards, title,
          navigation.views.map(view => view.id -> view.camera.aspectRatio.value).toMap)
      yield publication

  private def layout(spec: SurfaceFigureSpec, plan: SurfaceCompositionPlan, legends: Vector[SurfaceLegend],
      cards: Vector[SurfaceLegendPublication.Card], title: Option[LegendTextLayout], aspects: Map[SurfaceViewId, Double])
      : Either[SurfaceFigurePublicationError, SurfaceFigurePublication] =
    val titleHeight = title.fold(0.0)(_.heightPt + GapPt)
    val legendColumn = if cards.isEmpty then 0.0 else LegendWidthPt + GapPt
    val surfaceFrame = SurfacePublicationFrame(MarginPt, MarginPt + titleHeight,
      WidthPt - MarginPt * 2 - legendColumn, HeightPt - MarginPt * 2 - titleHeight)
    val available = surfaceFrame.heightPt
    val required = cards.map(_.heightPt).sum + math.max(0, cards.length - 1) * GapPt
    if required > available then Left(SurfaceFigurePublicationError.InsufficientSpace(required, available))
    else
      reframe(plan, surfaceFrame, aspects).flatMap: reframed =>
        val grobs = Vector.newBuilder[Grob]
        val transparent = Rgba.unsafe(0, 0, 0, 0)
        grobs += Grob.rectUnsafe(SurfaceLegendPublication.point(0, 0), SurfaceLegendPublication.size(WidthPt, HeightPt),
          anchor = Anchor.BottomLeft, gp = GraphicParams.unsafe(stroke = None, fill = Some(transparent)),
          name = Some(GraphicsName.unsafe("surface-figure-transparent-background")))
        val titleGp = GraphicParams.unsafe(stroke = Some(Rgba.Black), fontSize = Length.pointsUnsafe(TitleFontPt), fontFamily = Some("sans-serif"))
        title.foreach: layout =>
          layout.lines.zipWithIndex.foreach: (line, index) =>
            grobs += Grob.textUnsafe(line, SurfaceLegendPublication.point(WidthPt / 2,
              HeightPt - MarginPt - index * layout.lineHeightPt), anchor = Anchor(HJust.Center, VJust.Top), gp = titleGp,
              name = Some(GraphicsName.unsafe(s"surface-figure-title-$index")))
        val panelGp = GraphicParams.unsafe(stroke = Some(Rgba.Black), fontSize = Length.pointsUnsafe(PanelFontPt), fontFamily = Some("sans-serif"))
        plan.views.foreach: view =>
          val cellLeft = surfaceFrame.leftPt + view.viewport.x * surfaceFrame.widthPt
          val cellTop = surfaceFrame.topPt + view.viewport.y * surfaceFrame.heightPt
          val label = spec.panelLabels.getOrElse(view.id, view.id.value)
          grobs += Grob.textUnsafe(label, SurfaceLegendPublication.point(cellLeft + GapPt, HeightPt - cellTop - GapPt),
            anchor = Anchor(HJust.Left, VJust.Top), gp = panelGp, name = Some(GraphicsName.unsafe(s"surface-figure-panel-${view.id.value}")))
        var nextTop = surfaceFrame.topPt
        val legendReceipts = cards.zip(legends).zipWithIndex.map: (pair, index) =>
          val (card, legend) = pair
          val frame = SurfacePublicationFrame(WidthPt - MarginPt - card.widthPt, nextTop, card.widthPt, card.heightPt)
          grobs += Grob.group(card.scene.grobs, viewport = Some(Viewport.unsafe(
            origin = SurfaceLegendPublication.point(frame.leftPt, HeightPt - frame.topPt - frame.heightPt),
            size = SurfaceLegendPublication.size(frame.widthPt, frame.heightPt), clip = Clip.Off)),
            name = Some(GraphicsName.unsafe(s"surface-figure-legend-$index")))
          nextTop += card.heightPt + GapPt
          SurfaceFigureLegendReceipt(legend.canonicalKey, legend.sources, frame)
        val dimensions = RasterDimensions.unsafe(LogicalWidth * spec.outputScale.factor, LogicalHeight * spec.outputScale.factor)
        val input = SurfaceFigureRenderInput(reframed, dimensions, spec.background, spec.outputScale.factor.toDouble)
        val receipt = SurfaceFigurePublicationReceipt(LogicalWidth, LogicalHeight, dimensions, input.pixelsPerInch,
          WidthPt, HeightPt, surfaceFrame, reframed.receipt, legendReceipts)
        Right(new SurfaceFigurePublication(input, receipt, Scene(grobs.result())))

  private def reframe(plan: SurfaceCompositionPlan, frame: SurfacePublicationFrame, aspects: Map[SurfaceViewId, Double])
      : Either[SurfaceFigurePublicationError, SurfaceCompositionPlan] =
    val views = Vector.newBuilder[SurfaceCompositionView]
    var index = 0
    while index < plan.views.length do
      val view = plan.views(index)
      val cellTop = frame.topPt + view.viewport.y * frame.heightPt
      val cellHeight = view.viewport.height * frame.heightPt
      val availableHeight = cellHeight - PanelLabelHeightPt
      val cellLeft = frame.leftPt + view.viewport.x * frame.widthPt
      val cellWidth = view.viewport.width * frame.widthPt
      if availableHeight <= 0 then return Left(SurfaceFigurePublicationError.InvalidSpecification(
        s"view ${view.id.value} is too short for its panel label"))
      val aspect = aspects.get(view.id) match
        case Some(value) => value
        case None => return Left(SurfaceFigurePublicationError.InvalidSpecification(
          s"view ${view.id.value} has no navigation camera aspect"))
      val (displayLeft, displayTop, displayWidth, displayHeight) =
        if cellWidth / availableHeight > aspect then
          val width = availableHeight * aspect
          (cellLeft + (cellWidth - width) / 2, cellTop + PanelLabelHeightPt, width, availableHeight)
        else
          val height = cellWidth / aspect
          (cellLeft, cellTop + PanelLabelHeightPt + (availableHeight - height) / 2, cellWidth, height)
      val viewport = SurfaceViewport(
        displayLeft / WidthPt,
        displayTop / HeightPt,
        displayWidth / WidthPt,
        displayHeight / HeightPt
      )
      views += view.copy(viewport = viewport)
      index += 1
    SurfaceCompositionPlan.make(views.result(), plan.resources).left.map(SurfaceFigurePublicationError.Composition.apply)

  private def traverse[A, B](values: Vector[A])(f: A => Either[SurfaceFigurePublicationError, B])
      : Either[SurfaceFigurePublicationError, Vector[B]] =
    val result = Vector.newBuilder[B]
    var index = 0
    while index < values.length do
      f(values(index)) match
        case Left(error) => return Left(error)
        case Right(value) => result += value
      index += 1
    Right(result.result())
