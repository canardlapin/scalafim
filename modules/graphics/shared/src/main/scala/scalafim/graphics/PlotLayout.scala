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
    legendFontPt: Double = 10.0,
    legendKeyPt: Double = 10.0,
    legendGapPt: Double = 10.0,
    legendPaddingPt: Double = 6.0
):
  require(outerMarginPt >= 0.0 && outerMarginPt.isFinite, "`outerMarginPt` must be finite and >= 0")
  require(tickLengthPt >= 0.0 && tickLengthPt.isFinite, "`tickLengthPt` must be finite and >= 0")
  require(tickLabelGapPt >= 0.0 && tickLabelGapPt.isFinite, "`tickLabelGapPt` must be finite and >= 0")
  require(axisFontPt > 0.0 && axisFontPt.isFinite, "`axisFontPt` must be finite and > 0")
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

/** What the solver must make room for. */
final case class PlotLayoutRequest(
    axes: Map[AxisSide, Vector[String]] = Map.empty,
    legend: Option[LegendRequest] = None
)

final case class LegendRequest(title: Option[String], labels: Vector[String])

/** Solved plot regions, all as npc frames of the whole plot area. */
final case class PlotFrames(
    panel: PanelFrame,
    axes: Map[AxisSide, PanelFrame],
    legend: Option[PanelFrame]
):
  def legendViewport(clip: Clip = Clip.Off): Option[Viewport] =
    legend.map { frame =>
      Viewport.unsafe(
        origin = frame.origin,
        size = frame.size,
        clip = clip
      )
    }

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

    val axisStripPtY = policy.tickLengthPt + policy.tickLabelGapPt + policy.metrics.heightPt(policy.axisFontPt)
    def axisStripPtX(labels: Vector[String]): Double =
      val labelWidth = labels.foldLeft(0.0)((acc, label) => math.max(acc, policy.metrics.widthPt(label, policy.axisFontPt)))
      policy.tickLengthPt + policy.tickLabelGapPt + labelWidth

    val bottom = if request.axes.contains(AxisSide.Bottom) then npcY(axisStripPtY) else 0.0
    val top = if request.axes.contains(AxisSide.Top) then npcY(axisStripPtY) else 0.0
    val left = request.axes.get(AxisSide.Left).map(labels => npcX(axisStripPtX(labels))).getOrElse(0.0)
    val right = request.axes.get(AxisSide.Right).map(labels => npcX(axisStripPtX(labels))).getOrElse(0.0)

    val legendWidth = request.legend.map { legend =>
      val labelPt = legend.labels.foldLeft(0.0) { (acc, label) =>
        math.max(acc, policy.metrics.widthPt(label, policy.legendFontPt))
      }
      val titlePt = legend.title.fold(0.0)(title => policy.metrics.widthPt(title, policy.legendFontPt))
      val entryPt = policy.legendKeyPt + policy.legendGapPt / 2.0 + labelPt
      npcX(policy.legendPaddingPt * 2.0 + math.max(entryPt, titlePt))
    }
    val legendGap = legendWidth.fold(0.0)(_ => npcX(policy.legendGapPt))

    val panelX0 = marginX + left
    val panelX1 = 1.0 - marginX - right - legendGap - legendWidth.getOrElse(0.0)
    val panelY0 = marginY + bottom
    val panelY1 = 1.0 - marginY - top

    if panelX1 <= panelX0 then Left(GraphicsError.LayoutOverflow("panel width"))
    else if panelY1 <= panelY0 then Left(GraphicsError.LayoutOverflow("panel height"))
    else
      val panelW = panelX1 - panelX0
      val panelH = panelY1 - panelY0
      for
        panel <- PanelFrame.npc(panelX0, panelY0, panelW, panelH)
        axes <- axisFrames(request, panelX0, panelY0, panelW, panelH, marginX, marginY, bottom, top, left, right)
        legend <- legendWidth match
          case Some(width) =>
            PanelFrame.npc(panelX1 + right + legendGap, panelY0, width, panelH).map(Some(_))
          case None =>
            Right(None)
      yield PlotFrames(panel, axes, legend)

  private def axisFrames(
      request: PlotLayoutRequest,
      panelX: Double,
      panelY: Double,
      panelW: Double,
      panelH: Double,
      marginX: Double,
      marginY: Double,
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
    if request.axes.contains(AxisSide.Bottom) then add(AxisSide.Bottom, panelX, marginY, panelW, bottom)
    if request.axes.contains(AxisSide.Top) then add(AxisSide.Top, panelX, panelY + panelH, panelW, top)
    if request.axes.contains(AxisSide.Left) then add(AxisSide.Left, marginX, panelY, left, panelH)
    if request.axes.contains(AxisSide.Right) then add(AxisSide.Right, panelX + panelW, panelY, right, panelH)
    result.map(_ => out.result())
