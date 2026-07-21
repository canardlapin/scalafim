package scalafim.graphics

/** Text-extent capability used by the layout solver to size axis strips and
  * legend columns. The portable default is a conservative estimate; platform
  * backends may provide real font metrics without changing the solver
  * contract.
  */
trait TextMetrics:
  def widthPt(text: String, fontSizePt: Double): Double
  def heightPt(fontSizePt: Double): Double

object TextMetrics:
  /** Conservative portable estimator: wide-glyph average advance and a
    * standard line height, so allocated regions err toward whitespace rather
    * than overlap.
    */
  val estimate: TextMetrics =
    new TextMetrics:
      override def widthPt(text: String, fontSizePt: Double): Double =
        text.length.toDouble * fontSizePt * 0.62

      override def heightPt(fontSizePt: Double): Double =
        fontSizePt * 1.25

/** Placement and sizing defaults for solved plot layouts. All sizes are in
  * points; the solver converts them to npc fractions of the reference
  * device.
  */
final case class LayoutPolicy(
    metrics: TextMetrics = TextMetrics.estimate,
    referenceDevice: DeviceContext = DeviceContext.unsafe(640.0, 480.0),
    outerMarginPt: Double = 10.0,
    tickLengthPt: Double = 4.0,
    tickLabelGapPt: Double = 4.0,
    axisFontPt: Double = 10.0,
    axisTitleFontPt: Double = 11.0,
    axisTitleGapPt: Double = 6.0,
    plotTitleFontPt: Double = 16.0,
    plotSubtitleFontPt: Double = 12.0,
    plotLabelGapPt: Double = 4.0,
    legendFontPt: Double = 10.0,
    legendKeyPt: Double = 10.0,
    legendGapPt: Double = 10.0,
    legendPaddingPt: Double = 6.0
):
  require(outerMarginPt >= 0.0 && outerMarginPt.isFinite, "`outerMarginPt` must be finite and >= 0")
  require(tickLengthPt >= 0.0 && tickLengthPt.isFinite, "`tickLengthPt` must be finite and >= 0")
  require(tickLabelGapPt >= 0.0 && tickLabelGapPt.isFinite, "`tickLabelGapPt` must be finite and >= 0")
  require(axisFontPt > 0.0 && axisFontPt.isFinite, "`axisFontPt` must be finite and > 0")
  require(axisTitleFontPt > 0.0 && axisTitleFontPt.isFinite, "`axisTitleFontPt` must be finite and > 0")
  require(axisTitleGapPt >= 0.0 && axisTitleGapPt.isFinite, "`axisTitleGapPt` must be finite and >= 0")
  require(plotTitleFontPt > 0.0 && plotTitleFontPt.isFinite, "`plotTitleFontPt` must be finite and > 0")
  require(plotSubtitleFontPt > 0.0 && plotSubtitleFontPt.isFinite, "`plotSubtitleFontPt` must be finite and > 0")
  require(plotLabelGapPt >= 0.0 && plotLabelGapPt.isFinite, "`plotLabelGapPt` must be finite and >= 0")
  require(legendFontPt > 0.0 && legendFontPt.isFinite, "`legendFontPt` must be finite and > 0")
  require(legendKeyPt >= 0.0 && legendKeyPt.isFinite, "`legendKeyPt` must be finite and >= 0")
  require(legendGapPt >= 0.0 && legendGapPt.isFinite, "`legendGapPt` must be finite and >= 0")
  require(legendPaddingPt >= 0.0 && legendPaddingPt.isFinite, "`legendPaddingPt` must be finite and >= 0")

/** Stable names for solver-allocated regions. */
object PlotRegion:
  val Panel: GraphicsName = GraphicsName.unsafe("plot-panel")
  val AxisBottom: GraphicsName = GraphicsName.unsafe("axis-bottom")
  val AxisLeft: GraphicsName = GraphicsName.unsafe("axis-left")
  val AxisTop: GraphicsName = GraphicsName.unsafe("axis-top")
  val AxisRight: GraphicsName = GraphicsName.unsafe("axis-right")
  val Legend: GraphicsName = GraphicsName.unsafe("legend-region")
  val Title: GraphicsName = GraphicsName.unsafe("plot-title")
  val Subtitle: GraphicsName = GraphicsName.unsafe("plot-subtitle")
  val PanelBackground: GraphicsName = GraphicsName.unsafe("plot-panel-background")
  val PanelGridX: GraphicsName = GraphicsName.unsafe("plot-panel-grid-x")
  val PanelGridY: GraphicsName = GraphicsName.unsafe("plot-panel-grid-y")

/** What the solver must make room for. */
final case class AxisRequest(labels: Vector[String], title: Option[String] = None)

final case class PlotLayoutRequest(
    axes: Map[AxisSide, AxisRequest] = Map.empty,
    legend: Option[LegendRequest] = None,
    labels: PlotLabels = PlotLabels(),
    panelAspect: Option[CoordinateRatio] = None
)

final case class LegendRequest(title: Option[String], labels: Vector[String])

/** Solved plot regions, all as npc frames of the whole plot area. */
final case class PlotFrames(
    panel: PanelFrame,
    axes: Map[AxisSide, PanelFrame],
    legend: Option[PanelFrame],
    title: Option[PanelFrame],
    subtitle: Option[PanelFrame]
):
  def legendViewport(clip: Clip = Clip.Off): Option[Viewport] =
    legend.map { frame =>
      Viewport.unsafe(
        origin = frame.origin,
        size = frame.size,
        clip = clip
      )
    }

  def titleViewport: Option[Viewport] =
    title.map(frameViewport)

  def subtitleViewport: Option[Viewport] =
    subtitle.map(frameViewport)

  private def frameViewport(frame: PanelFrame): Viewport =
    Viewport.unsafe(origin = frame.origin, size = frame.size, clip = Clip.Off)

/** Allocates panel, axis-strip, and legend regions from declared extents and
  * estimated text sizes. Deterministic and portable: no font access, no
  * platform calls.
  */
object PlotLayoutSolver:
  def solve(
      policy: LayoutPolicy,
      request: PlotLayoutRequest
  ): Either[GraphicsError, PlotFrames] =
    val device = policy.referenceDevice
    val pxPerPt = device.pxPerUnit(LengthUnit.Point).getOrElse(96.0 / 72.0)
    def npcX(pt: Double): Double = pt * pxPerPt / device.width
    def npcY(pt: Double): Double = pt * pxPerPt / device.height

    val marginX = npcX(policy.outerMarginPt)
    val marginY = npcY(policy.outerMarginPt)

    def titleExtent(request: AxisRequest): Double =
      request.title.fold(0.0)(_ => policy.axisTitleGapPt + policy.metrics.heightPt(policy.axisTitleFontPt))
    def axisStripPtY(request: AxisRequest): Double =
      policy.tickLengthPt + policy.tickLabelGapPt + policy.metrics.heightPt(policy.axisFontPt) + titleExtent(request)
    def axisStripPtX(request: AxisRequest): Double =
      val labelWidth = request.labels.foldLeft(0.0)((acc, label) => math.max(acc, policy.metrics.widthPt(label, policy.axisFontPt)))
      policy.tickLengthPt + policy.tickLabelGapPt + labelWidth + titleExtent(request)

    val bottom = request.axes.get(AxisSide.Bottom).map(axis => npcY(axisStripPtY(axis))).getOrElse(0.0)
    val top = request.axes.get(AxisSide.Top).map(axis => npcY(axisStripPtY(axis))).getOrElse(0.0)
    val left = request.axes.get(AxisSide.Left).map(axis => npcX(axisStripPtX(axis))).getOrElse(0.0)
    val right = request.axes.get(AxisSide.Right).map(axis => npcX(axisStripPtX(axis))).getOrElse(0.0)

    val titleHeight = request.labels.title.map(_ => npcY(policy.metrics.heightPt(policy.plotTitleFontPt)))
    val subtitleHeight = request.labels.subtitle.map(_ => npcY(policy.metrics.heightPt(policy.plotSubtitleFontPt)))
    val betweenLabels = if titleHeight.nonEmpty && subtitleHeight.nonEmpty then npcY(policy.plotLabelGapPt) else 0.0
    val belowLabels = if titleHeight.nonEmpty || subtitleHeight.nonEmpty then npcY(policy.plotLabelGapPt) else 0.0
    val headerHeight = titleHeight.getOrElse(0.0) + subtitleHeight.getOrElse(0.0) + betweenLabels + belowLabels

    val legendWidth = request.legend.map { legend =>
      val labelPt = legend.labels.foldLeft(0.0) { (acc, label) =>
        math.max(acc, policy.metrics.widthPt(label, policy.legendFontPt))
      }
      val titlePt = legend.title.fold(0.0)(title => policy.metrics.widthPt(title, policy.legendFontPt))
      val entryPt = policy.legendKeyPt + policy.legendGapPt / 2.0 + labelPt
      npcX(policy.legendPaddingPt * 2.0 + math.max(entryPt, titlePt))
    }
    val legendGap = legendWidth.fold(0.0)(_ => npcX(policy.legendGapPt))

    val availableX0 = marginX + left
    val availableX1 = 1.0 - marginX - right - legendGap - legendWidth.getOrElse(0.0)
    val availableY0 = marginY + bottom
    val availableY1 = 1.0 - marginY - top - headerHeight

    if availableX1 <= availableX0 then Left(GraphicsError.LayoutOverflow("panel width"))
    else if availableY1 <= availableY0 then Left(GraphicsError.LayoutOverflow("panel height"))
    else
      val availableW = availableX1 - availableX0
      val availableH = availableY1 - availableY0
      val (panelX0, panelY0, panelW, panelH) =
        request.panelAspect match
          case None =>
            (availableX0, availableY0, availableW, availableH)
          case Some(aspect) =>
            val targetNpcAspect = aspect.toDouble * device.width / device.height
            if availableH / availableW > targetNpcAspect then
              val height = availableW * targetNpcAspect
              (availableX0, availableY0 + (availableH - height) / 2.0, availableW, height)
            else
              val width = availableH / targetNpcAspect
              (availableX0 + (availableW - width) / 2.0, availableY0, width, availableH)
      for
        panel <- PanelFrame.npc(panelX0, panelY0, panelW, panelH)
        axes <- axisFrames(request, panelX0, panelY0, panelW, panelH, bottom, top, left, right)
        legend <- legendWidth match
          case Some(width) =>
            PanelFrame.npc(availableX1 + right + legendGap, panelY0, width, panelH).map(Some(_))
          case None =>
            Right(None)
        subtitle <- subtitleHeight match
          case Some(height) =>
            PanelFrame.npc(panelX0, availableY1 + top + belowLabels, panelW, height).map(Some(_))
          case None => Right(None)
        title <- titleHeight match
          case Some(height) =>
            val y = availableY1 + top + belowLabels + subtitleHeight.getOrElse(0.0) + betweenLabels
            PanelFrame.npc(panelX0, y, panelW, height).map(Some(_))
          case None => Right(None)
      yield PlotFrames(panel, axes, legend, title, subtitle)

  private def axisFrames(
      request: PlotLayoutRequest,
      panelX: Double,
      panelY: Double,
      panelW: Double,
      panelH: Double,
      bottom: Double,
      top: Double,
      left: Double,
      right: Double
  ): Either[GraphicsError, Map[AxisSide, PanelFrame]] =
    val out = Map.newBuilder[AxisSide, PanelFrame]
    var result: Either[GraphicsError, Unit] = Right(())
    def add(side: AxisSide, x: Double, y: Double, w: Double, h: Double): Unit =
      if result.isRight then
        result = PanelFrame.npc(x, y, w, h).map { frame =>
          out += side -> frame
          ()
        }
    if request.axes.contains(AxisSide.Bottom) then add(AxisSide.Bottom, panelX, panelY - bottom, panelW, bottom)
    if request.axes.contains(AxisSide.Top) then add(AxisSide.Top, panelX, panelY + panelH, panelW, top)
    if request.axes.contains(AxisSide.Left) then add(AxisSide.Left, panelX - left, panelY, left, panelH)
    if request.axes.contains(AxisSide.Right) then add(AxisSide.Right, panelX + panelW, panelY, right, panelH)
    result.map(_ => out.result())
