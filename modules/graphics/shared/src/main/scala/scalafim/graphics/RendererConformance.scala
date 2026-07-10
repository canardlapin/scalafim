package scalafim.graphics

/** Behavioral family a conformance case exercises. */
enum ConformanceGroup:
  case Primitive
  case Layout
  case Guide
  case CompiledPlot

enum RenderPrimitiveKind:
  case Disc
  case Polyline
  case Polygon
  case Rectangle
  case Text
  case Image

/** Backend-neutral facts that must be observable in renderer output. Unlike a
  * marker-only smoke check, these requirements pin primitive choice, styles,
  * text placement, and group effects without prescribing an output format.
  */
enum RenderRequirement:
  case Primitive(name: GraphicsName, kind: RenderPrimitiveKind)
  case Group(name: GraphicsName, clipped: Boolean, rotated: Boolean)
  case Style(
      name: GraphicsName,
      stroke: Option[Rgba],
      fill: Option[Rgba],
      lineWidth: Double,
      lineType: LineType,
      alpha: Double
  )
  case Text(name: GraphicsName, horizontal: HJust, vertical: VJust, rotated: Boolean)
  case Image(
      name: GraphicsName,
      dimensions: RasterDimensions,
      interpolation: RasterInterpolation,
      alpha: Double
  )

  def description: String =
    this match
      case Primitive(name, kind) =>
        s"primitive '${name.value}' as $kind"
      case Group(name, clipped, rotated) =>
        s"group '${name.value}' with clipped=$clipped and rotated=$rotated"
      case Style(name, _, _, lineWidth, lineType, alpha) =>
        s"style '${name.value}' with lineWidth=$lineWidth, lineType=$lineType, alpha=$alpha"
      case Text(name, horizontal, vertical, rotated) =>
        s"text '${name.value}' with anchor=($horizontal,$vertical) and rotated=$rotated"
      case Image(name, dimensions, interpolation, alpha) =>
        s"image '${name.value}' with ${dimensions.width}x${dimensions.height} pixels, interpolation=$interpolation, alpha=$alpha"

/** One renderer conformance case: a scene, the family it exercises, and the
  * named grobs whose markers must survive into backend output.
  */
final case class ConformanceCase(
    name: GraphicsName,
    group: ConformanceGroup,
    scene: Scene,
    markers: Vector[GraphicsName],
    requirements: Vector[RenderRequirement] = Vector.empty
)

/** Adapter a backend implements to run the conformance contract. `Out` must
  * have value equality (used for the determinism check).
  */
trait RendererHarness[Out]:
  def render(scene: Scene): Either[String, Out]
  def containsMarker(out: Out, name: GraphicsName): Boolean
  def satisfies(out: Out, requirement: RenderRequirement): Boolean =
    false

  /** Backend-specific well-formedness check on the rendered output; return a
    * problem description to fail the case.
    */
  def validate(out: Out): Option[String] =
    None

/** The renderer conformance contract: canonical scenes grouped by
  * primitive/layout/guide/compiled-plot behavior, plus a portable checker
  * that any backend runs without encoding plot semantics.
  */
object RendererConformance:
  final case class Violation(caseName: String, group: ConformanceGroup, problem: String)

  /** Run every conformance case through a backend harness. An empty result
    * means the backend renders each case successfully, deterministically,
    * with every marker present and its own validation passing.
    */
  def check[Out](harness: RendererHarness[Out]): Either[GraphicsError, Vector[Violation]] =
    cases.map { all =>
      all.flatMap(conformanceCase => checkCase(harness, conformanceCase))
    }

  private def checkCase[Out](
      harness: RendererHarness[Out],
      conformanceCase: ConformanceCase
  ): Vector[Violation] =
    def violation(problem: String): Violation =
      Violation(conformanceCase.name.value, conformanceCase.group, problem)
    harness.render(conformanceCase.scene) match
      case Left(error) =>
        Vector(violation(s"render failed: $error"))
      case Right(first) =>
        harness.render(conformanceCase.scene) match
          case Left(error) =>
            Vector(violation(s"second render failed: $error"))
          case Right(second) =>
            val determinism =
              if first == second then Vector.empty
              else Vector(violation("rendering is not deterministic"))
            val markers = conformanceCase.markers.filterNot(harness.containsMarker(first, _)).map { missing =>
              violation(s"missing marker '${missing.value}'")
            }
            val requirements = conformanceCase.requirements.filterNot(harness.satisfies(first, _)).map { missing =>
              violation(s"missing semantic requirement: ${missing.description}")
            }
            val validation = harness.validate(first).map(violation).toVector
            determinism ++ markers ++ requirements ++ validation

  def cases: Either[GraphicsError, Vector[ConformanceCase]] =
    for
      point <- pointCase
      line <- lineCase
      shapes <- shapeCase
      rectAndCircle <- rectCircleCase
      text <- textCase
      image <- imageCase
      clipped <- clippedViewportCase
      rotated <- rotatedViewportCase
      clippedAndRotated <- clippedRotatedViewportCase
      rasterOriented <- yDownViewportCase
      axis <- axisCase
      legend <- legendCase
      scaled <- scaledPlotCase
      solved <- solvedPlotCase
    yield Vector(
      point,
      line,
      shapes,
      rectAndCircle,
      text,
      image,
      clipped,
      rotated,
      clippedAndRotated,
      rasterOriented,
      axis,
      legend,
      scaled,
      solved
    )

  def group(group: ConformanceGroup): Either[GraphicsError, Vector[ConformanceCase]] =
    cases.map(_.filter(_.group == group))

  // --- Primitive cases -----------------------------------------------------

  def pointCase: Either[GraphicsError, ConformanceCase] =
    Grob
      .points(
        Vector(Point.npcUnsafe(0.25, 0.25), Point.npcUnsafe(0.75, 0.75)),
        size = ExtentExpr.pointsUnsafe(4.0),
        gp = GraphicParams.unsafe(stroke = Some(Rgba.Black), fill = Some(Rgba.unsafe(40, 80, 120))),
        name = Some(GraphicsName.unsafe("conformance-point"))
      )
      .map { grob =>
        ConformanceCase(
          GraphicsName.unsafe("point"),
          ConformanceGroup.Primitive,
          Scene(Vector(grob)),
          Vector(GraphicsName.unsafe("conformance-point")),
          Vector(
            RenderRequirement.Primitive(GraphicsName.unsafe("conformance-point"), RenderPrimitiveKind.Disc)
          )
        )
      }

  def lineCase: Either[GraphicsError, ConformanceCase] =
    Grob
      .lines(
        Vector(
          Point.npcUnsafe(0.1, 0.1),
          Point.npcUnsafe(0.5, 0.75),
          Point.npcUnsafe(0.9, 0.25)
        ),
        gp = GraphicParams.unsafe(stroke = Some(Rgba.unsafe(25, 75, 125)), lineWidth = 1.5, lineType = LineType.Dashed),
        name = Some(GraphicsName.unsafe("conformance-line"))
      )
      .map { grob =>
        ConformanceCase(
          GraphicsName.unsafe("line"),
          ConformanceGroup.Primitive,
          Scene(Vector(grob)),
          Vector(GraphicsName.unsafe("conformance-line")),
          Vector(
            RenderRequirement.Primitive(GraphicsName.unsafe("conformance-line"), RenderPrimitiveKind.Polyline),
            RenderRequirement.Style(
              GraphicsName.unsafe("conformance-line"),
              Some(Rgba.unsafe(25, 75, 125)),
              None,
              1.5,
              LineType.Dashed,
              1.0
            )
          )
        )
      }

  def shapeCase: Either[GraphicsError, ConformanceCase] =
    for
      square <- Grob.points(
        Vector(Point.npcUnsafe(0.25, 0.5)),
        size = ExtentExpr.pointsUnsafe(6.0),
        shape = PointShape.Square,
        name = Some(GraphicsName.unsafe("conformance-square"))
      )
      triangle <- Grob.points(
        Vector(Point.npcUnsafe(0.5, 0.5)),
        size = ExtentExpr.pointsUnsafe(6.0),
        shape = PointShape.Triangle,
        name = Some(GraphicsName.unsafe("conformance-triangle"))
      )
      cross <- Grob.points(
        Vector(Point.npcUnsafe(0.75, 0.5)),
        size = ExtentExpr.pointsUnsafe(6.0),
        shape = PointShape.Cross,
        name = Some(GraphicsName.unsafe("conformance-cross"))
      )
    yield ConformanceCase(
      GraphicsName.unsafe("shapes"),
      ConformanceGroup.Primitive,
      Scene(Vector(square, triangle, cross)),
      Vector(
        GraphicsName.unsafe("conformance-square"),
        GraphicsName.unsafe("conformance-triangle"),
        GraphicsName.unsafe("conformance-cross")
      )
    )

  def rectCircleCase: Either[GraphicsError, ConformanceCase] =
    for
      rect <- Grob.rect(
        Point.npcUnsafe(0.5, 0.5),
        Size.npcUnsafe(0.25, 0.4),
        anchor = Anchor.Center,
        gp = GraphicParams.unsafe(fill = Some(Rgba.unsafe(10, 20, 30)), alpha = 0.75),
        name = Some(GraphicsName.unsafe("conformance-rect"))
      )
      circle <- Grob.circle(
        Point.npcUnsafe(0.25, 0.75),
        ExtentExpr.pointsUnsafe(3.0),
        gp = GraphicParams.unsafe(stroke = Some(Rgba.unsafe(200, 0, 0))),
        name = Some(GraphicsName.unsafe("conformance-circle"))
      )
    yield ConformanceCase(
      GraphicsName.unsafe("rect-circle"),
      ConformanceGroup.Primitive,
      Scene(Vector(rect, circle)),
      Vector(GraphicsName.unsafe("conformance-rect"), GraphicsName.unsafe("conformance-circle"))
    )

  def textCase: Either[GraphicsError, ConformanceCase] =
    Grob
      .text(
        "A&B <label>",
        Point.npcUnsafe(0.5, 0.75),
        anchor = Anchor(HJust.Left, VJust.Top),
        rotationDegrees = 30.0,
        gp = GraphicParams.unsafe(stroke = None, fill = Some(Rgba.Black), fontSize = Length.pointsUnsafe(9.0)),
        name = Some(GraphicsName.unsafe("conformance-text"))
      )
      .map { grob =>
        ConformanceCase(
          GraphicsName.unsafe("text"),
          ConformanceGroup.Primitive,
          Scene(Vector(grob)),
          Vector(GraphicsName.unsafe("conformance-text")),
          Vector(
            RenderRequirement.Primitive(GraphicsName.unsafe("conformance-text"), RenderPrimitiveKind.Text),
            RenderRequirement.Text(
              GraphicsName.unsafe("conformance-text"),
              HJust.Left,
              VJust.Top,
              rotated = true
            )
          )
        )
      }

  def imageCase: Either[GraphicsError, ConformanceCase] =
    val dimensions = RasterDimensions.unsafe(2, 2)
    val raster = RasterImage.unsafePacked(
      dimensions,
      Vector(
        Rgba32.unsafe(220, 30, 30),
        Rgba32.unsafe(30, 200, 60, 160),
        Rgba32.unsafe(40, 80, 220),
        Rgba32.unsafe(245, 210, 40, 0)
      )
    )
    val name = GraphicsName.unsafe("conformance-image")
    Grob
      .image(
        raster,
        Point.npcUnsafe(0.5, 0.5),
        Size.npcUnsafe(0.5, 0.5),
        interpolation = RasterInterpolation.Nearest,
        alpha = 0.8,
        name = Some(name)
      )
      .map { grob =>
        ConformanceCase(
          GraphicsName.unsafe("image"),
          ConformanceGroup.Primitive,
          Scene(Vector(grob)),
          Vector(name),
          Vector(
            RenderRequirement.Primitive(name, RenderPrimitiveKind.Image),
            RenderRequirement.Image(name, dimensions, RasterInterpolation.Nearest, alpha = 0.8)
          )
        )
      }

  // --- Layout cases --------------------------------------------------------

  def clippedViewportCase: Either[GraphicsError, ConformanceCase] =
    val viewport = Viewport.unsafe(
      origin = Point.npcUnsafe(0.1, 0.2),
      size = Size.npcUnsafe(0.6, 0.5),
      xScale = Interval.unsafe(-1.0, 1.0),
      yScale = Interval.unsafe(0.0, 10.0),
      clip = Clip.On
    )
    Grob
      .lines(
        Vector(Point.nativeUnsafe(-1.0, 0.0), Point.nativeUnsafe(1.0, 10.0)),
        viewport = Some(viewport),
        name = Some(GraphicsName.unsafe("conformance-clip"))
      )
      .map { grob =>
        ConformanceCase(
          GraphicsName.unsafe("clipped-viewport"),
          ConformanceGroup.Layout,
          Scene(Vector(grob)),
          Vector(GraphicsName.unsafe("conformance-clip")),
          Vector(
            RenderRequirement.Group(GraphicsName.unsafe("conformance-clip"), clipped = true, rotated = false)
          )
        )
      }

  def rotatedViewportCase: Either[GraphicsError, ConformanceCase] =
    val viewport = Viewport.unsafe(
      origin = Point.npcUnsafe(0.2, 0.2),
      size = Size.npcUnsafe(0.5, 0.5),
      clip = Clip.Off,
      angleDegrees = 15.0
    )
    Grob
      .lines(
        Vector(Point.npcUnsafe(0.0, 0.0), Point.npcUnsafe(1.0, 1.0)),
        viewport = Some(viewport),
        name = Some(GraphicsName.unsafe("conformance-rotation"))
      )
      .map { grob =>
        ConformanceCase(
          GraphicsName.unsafe("rotated-viewport"),
          ConformanceGroup.Layout,
          Scene(Vector(grob)),
          Vector(GraphicsName.unsafe("conformance-rotation")),
          Vector(
            RenderRequirement.Group(GraphicsName.unsafe("conformance-rotation"), clipped = false, rotated = true)
          )
        )
      }

  def clippedRotatedViewportCase: Either[GraphicsError, ConformanceCase] =
    val viewport = Viewport.unsafe(
      origin = Point.npcUnsafe(0.15, 0.15),
      size = Size.npcUnsafe(0.6, 0.6),
      clip = Clip.On,
      angleDegrees = 22.5
    )
    val style = GraphicParams.unsafe(
      stroke = Some(Rgba.unsafe(80, 40, 160)),
      lineWidth = 2.0,
      lineType = LineType.Dotted,
      alpha = 0.6
    )
    Grob
      .lines(
        Vector(Point.npcUnsafe(-0.1, 0.2), Point.npcUnsafe(1.1, 0.8)),
        gp = style,
        viewport = Some(viewport),
        name = Some(GraphicsName.unsafe("conformance-clip-rotation"))
      )
      .map { grob =>
        val name = GraphicsName.unsafe("conformance-clip-rotation")
        ConformanceCase(
          GraphicsName.unsafe("clipped-rotated-viewport"),
          ConformanceGroup.Layout,
          Scene(Vector(grob)),
          Vector(name),
          Vector(
            RenderRequirement.Group(name, clipped = true, rotated = true),
            RenderRequirement.Primitive(name, RenderPrimitiveKind.Polyline),
            RenderRequirement.Style(
              name,
              Some(Rgba.unsafe(80, 40, 160)),
              None,
              2.0,
              LineType.Dotted,
              0.6
            )
          )
        )
      }

  def yDownViewportCase: Either[GraphicsError, ConformanceCase] =
    val viewport = Viewport.unsafe(
      origin = Point.npcUnsafe(0.1, 0.1),
      size = Size.npcUnsafe(0.8, 0.8),
      yScale = Interval.unsafe(0.0, 4.0),
      clip = Clip.Off,
      yDirection = YDirection.Down
    )
    Grob
      .rect(
        Point.nativeUnsafe(0.5, 1.0),
        Size.npcUnsafe(0.5, 0.25),
        anchor = Anchor(HJust.Left, VJust.Top),
        viewport = Some(viewport),
        name = Some(GraphicsName.unsafe("conformance-ydown"))
      )
      .map { grob =>
        ConformanceCase(
          GraphicsName.unsafe("ydown-viewport"),
          ConformanceGroup.Layout,
          Scene(Vector(grob)),
          Vector(GraphicsName.unsafe("conformance-ydown"))
        )
      }

  // --- Guide cases ---------------------------------------------------------

  def axisCase: Either[GraphicsError, ConformanceCase] =
    val layout =
      PanelLayout(
        PanelFrame.npcUnsafe(0.1, 0.15, 0.8, 0.75),
        xScale = Interval.unsafe(0.0, 10.0),
        yScale = Interval.unsafe(-1.0, 1.0),
        clip = Clip.Off
      )
    for
      bottom <- GuideSpec.lower(
        GuideSpec.Axis(
          AxisSide.Bottom,
          breaks = Breaks.countUnsafe(3),
          name = Some(GraphicsName.unsafe("conformance-x-axis"))
        ),
        layout
      )
      left <- GuideSpec.lower(
        GuideSpec.Axis(
          AxisSide.Left,
          breaks = Breaks.countUnsafe(3),
          name = Some(GraphicsName.unsafe("conformance-y-axis"))
        ),
        layout
      )
    yield ConformanceCase(
      GraphicsName.unsafe("axes"),
      ConformanceGroup.Guide,
      Scene(Vector(bottom.grob, left.grob)),
      Vector(GraphicsName.unsafe("conformance-x-axis"), GraphicsName.unsafe("conformance-y-axis"))
    )

  def legendCase: Either[GraphicsError, ConformanceCase] =
    val layout =
      PanelLayout(
        PanelFrame.npcUnsafe(0.1, 0.1, 0.7, 0.8),
        xScale = Interval.unsafe(0.0, 1.0),
        yScale = Interval.unsafe(0.0, 1.0)
      )
    GuideSpec
      .lower(
        GuideSpec.Legend(
          title = Some("condition"),
          entries = Vector(
            LegendEntry.colorUnsafe("A", Rgba.unsafe(40, 80, 120)),
            LegendEntry.colorUnsafe("B", Rgba.unsafe(210, 120, 40))
          ),
          name = Some(GraphicsName.unsafe("conformance-legend"))
        ),
        layout
      )
      .map { guide =>
        ConformanceCase(
          GraphicsName.unsafe("legend"),
          ConformanceGroup.Guide,
          Scene(Vector(guide.grob)),
          Vector(GraphicsName.unsafe("conformance-legend"))
        )
      }

  // --- Compiled plot cases -------------------------------------------------

  private final case class Observation(x: Double, y: Double, condition: String)

  private val observations =
    Vector(
      Observation(0.0, 1.0, "A"),
      Observation(1.0, 2.0, "B"),
      Observation(2.0, 3.0, "A")
    )

  private def conditionScale: Either[GraphicsError, DiscreteScale[Rgba]] =
    DiscreteDomain.ordered(Vector("A", "B")).flatMap { domain =>
      DiscreteScale(
        "condition",
        domain,
        DiscretePalette.valuesUnsafe(Vector(Rgba.unsafe(40, 80, 120), Rgba.unsafe(210, 120, 40)))
      )
    }

  def scaledPlotCase: Either[GraphicsError, ConformanceCase] =
    for
      xScale <- ContinuousScale.train("x-position", observations.map(_.x), Palette.numeric)
      yScale <- ContinuousScale.train("y-position", observations.map(_.y), Palette.numeric)
      colorScale <- conditionScale
      plot <- Plot(observations)
        .withScale(ScaleBinding[Observation, Double, Double](Aesthetic.X, _.x, xScale))
        .flatMap(_.withScale(ScaleBinding[Observation, Double, Double](Aesthetic.Y, _.y, yScale)))
        .flatMap(_.withScale(ScaleBinding[Observation, String, Rgba](Aesthetic.Color, _.condition, colorScale)))
        .flatMap(_.addLayer(Layer.point[Observation](_.x, _.y)))
      scene <- PlotCompiler.compile(
        plot,
        PlotCompilerOptions(
          layout = Some(
            PanelLayout(
              PanelFrame.npcUnsafe(0.12, 0.12, 0.68, 0.72),
              xScale = Interval.unsafe(-0.05, 1.05),
              yScale = Interval.unsafe(-0.05, 1.05),
              clip = Clip.On
            )
          ),
          guides = GuidePolicy.Explicit(
            Vector(
              GuideSpec.Axis(
                AxisSide.Bottom,
                ticks = Some(
                  Vector(
                    AxisTick.unsafe(0.0, "0"),
                    AxisTick.unsafe(0.5, "0.5"),
                    AxisTick.unsafe(1.0, "1")
                  )
                ),
                tickLength = Some(ExtentExpr.nativeUnsafe(0.05)),
                labelOffset = Some(ExtentExpr.nativeUnsafe(0.1)),
                name = Some(GraphicsName.unsafe("scaled-x-axis"))
              ),
              GuideSpec.Legend(
                title = Some("condition"),
                entries = Vector(
                  LegendEntry.colorUnsafe("A", Rgba.unsafe(40, 80, 120)),
                  LegendEntry.colorUnsafe("B", Rgba.unsafe(210, 120, 40))
                ),
                origin = Point.npcUnsafe(0.84, 0.82),
                name = Some(GraphicsName.unsafe("condition-legend"))
              )
            )
          )
        )
      )
    yield ConformanceCase(
      GraphicsName.unsafe("scaled-plot"),
      ConformanceGroup.CompiledPlot,
      scene,
      Vector(
        GraphicsName.unsafe("plot-panel"),
        GraphicsName.unsafe("scaled-x-axis"),
        GraphicsName.unsafe("condition-legend")
      )
    )

  def solvedPlotCase: Either[GraphicsError, ConformanceCase] =
    for
      colorScale <- conditionScale
      plot <- Plot(observations)
        .withScale(ScaleBinding[Observation, String, Rgba](Aesthetic.Color, _.condition, colorScale))
        .flatMap(_.addLayer(Layer.point[Observation](_.x, _.y)))
      scene <- PlotCompiler.compile(
        plot,
        PlotCompilerOptions(
          policy = Some(LayoutPolicy()),
          guides = GuidePolicy.Derived()
        )
      )
    yield ConformanceCase(
      GraphicsName.unsafe("solved-plot"),
      ConformanceGroup.CompiledPlot,
      scene,
      Vector(
        GraphicsName.unsafe("plot-panel"),
        GraphicsName.unsafe("x-axis"),
        GraphicsName.unsafe("y-axis"),
        GraphicsName.unsafe("condition-legend")
      )
    )
