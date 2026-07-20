package scalafim.graphics

/** How the compiler determines the plot's guides.
  *
  *   - [[GuidePolicy.NoGuides]] — no guides; a layout is optional.
  *   - [[GuidePolicy.Explicit]] — exactly the given specs.
  *   - [[GuidePolicy.Derived]] — routine axes and legends derive from the
  *     layers' trained scales (or from the panel ranges when a position is
  *     unscaled). An explicit override axis suppresses the derived axis on
  *     the same side, and any explicit legend suppresses derived legends;
  *     overrides are always included.
  */
enum GuidePolicy:
  case NoGuides
  case Explicit(specs: Vector[GuideSpec])
  case Derived(overrides: Vector[GuideSpec] = Vector.empty, deriveLegends: Boolean = true)

  def requiresLayout: Boolean =
    this match
      case NoGuides        => false
      case Explicit(specs) => specs.nonEmpty
      case Derived(_, _)   => true

/** Padding applied to compiler-derived panel ranges after scale training and
  * guide derivation. `multiplicative` is a fraction of the trained width;
  * `additive` is in panel-native units. Degenerate ranges use `zeroWidth` as
  * their reference width so a single point still receives visible framing.
  */
final case class RangeExpansion private (
    multiplicative: Double,
    additive: Double,
    zeroWidth: Double
):
  def expand(interval: Interval): Either[GraphicsError, Interval] =
    if isNone then Right(interval)
    else
      val referenceWidth = if interval.width == 0.0 then zeroWidth else interval.width
      val padding = referenceWidth * multiplicative + additive
      Interval(interval.lower - padding, interval.upper + padding)

  def isNone: Boolean =
    multiplicative == 0.0 && additive == 0.0

object RangeExpansion:
  val default: RangeExpansion =
    new RangeExpansion(multiplicative = 0.05, additive = 0.0, zeroWidth = 1.0)

  val none: RangeExpansion =
    new RangeExpansion(multiplicative = 0.0, additive = 0.0, zeroWidth = 1.0)

  def apply(
      multiplicative: Double,
      additive: Double = 0.0,
      zeroWidth: Double = 1.0
  ): Either[GraphicsError, RangeExpansion] =
    if !multiplicative.isFinite || multiplicative < 0.0
      || !additive.isFinite || additive < 0.0
      || !zeroWidth.isFinite || zeroWidth <= 0.0
    then Left(GraphicsError.InvalidRangeExpansion(multiplicative, additive, zeroWidth))
    else Right(new RangeExpansion(multiplicative, additive, zeroWidth))

  def unsafe(
      multiplicative: Double,
      additive: Double = 0.0,
      zeroWidth: Double = 1.0
  ): RangeExpansion =
    apply(multiplicative, additive, zeroWidth).orThrow

final case class PlotCompilerOptions(
    layout: Option[PanelLayout] = None,
    frame: Option[PanelFrame] = None,
    policy: Option[LayoutPolicy] = None,
    margins: PanelMargins = PanelMargins.none,
    expansion: RangeExpansion = RangeExpansion.default,
    guides: GuidePolicy = GuidePolicy.NoGuides
)

object PlotCompilerOptions:
  val default: PlotCompilerOptions =
    PlotCompilerOptions()

final case class TrainedPlot[Row](
    layers: Vector[ResolvedLayer[Row]],
    layout: Option[PanelLayout],
    guides: Vector[ResolvedGuide],
    scaleRegistry: PlotScaleRegistry
):
  def scene: Scene =
    val layerGrobs = layers.flatMap(_.grobs)
    val panelGrobs =
      layout match
        case None =>
          layerGrobs
        case Some(panel) =>
          Vector(
            Grob.group(
              layerGrobs,
              viewport = Some(panel.viewport),
              name = Some(GraphicsName.unsafe("plot-panel"))
            )
          )
    Scene(panelGrobs ++ guides.map(_.grob))

  def droppedRows: Vector[DroppedRow[Row]] =
    layers.flatMap(_.droppedRows)

  def scaleDeclarations: Vector[ScaleDeclaration] =
    layers.flatMap(_.scaleDeclarations)

  def trainedScales: Vector[TrainedScale] =
    scaleRegistry.scales

final case class ResolvedLayer[Row](
    layerIndex: Int,
    geom: Geom,
    stat: Stat,
    dataSize: Int,
    mapping: AesSpec[Row],
    scaleDeclarations: Vector[ScaleDeclaration],
    trainedScales: Vector[TrainedScale],
    rows: Vector[ResolvedRow[Row]],
    droppedRows: Vector[DroppedRow[Row]],
    grobs: Vector[Grob]
)

final case class ScaleDeclaration(
    layerIndex: Int,
    aesthetic: String,
    scaleName: GraphicsName,
    kind: ScaleKind
)

final case class TrainedScale(
    aesthetic: String,
    descriptor: ScaleDescriptor,
    scale: Scale[?, ?]
)

final case class ResolvedRow[Row](
    rowIndex: Int,
    source: Row,
    x: Double,
    y: Double,
    point: Point,
    label: Option[String],
    group: Option[String],
    gp: GraphicParams,
    size: ExtentExpr
)

final case class DroppedRow[Row](
    layerIndex: Int,
    rowIndex: Int,
    source: Row,
    reason: PlotDropReason
)

enum PlotDropReason:
  case MissingAesthetic(aesthetic: String)
  case MissingPosition
  case MissingLabel
  case NonFinitePosition(x: Double, y: Double)
  case TransformDomain(aesthetic: String, transform: String, value: Double)
  case ScaleOutOfDomain(aesthetic: String, scale: String, value: String)
  case InvalidAesthetic(aesthetic: String, value: String)

/** Facade over the compiler phases: mapping resolution, plot-wide scale
  * training, row evaluation, geom lowering, and guide resolution. Each phase
  * lives in [[CompilerPhases]] and is independently testable.
  */
object PlotCompiler:
  def compile[Row](
      plot: Plot[Row],
      options: PlotCompilerOptions = PlotCompilerOptions.default
  ): Either[GraphicsError, Scene] =
    resolve(plot, options).map(_.scene)

  def resolve[Row](
      plot: Plot[Row],
      options: PlotCompilerOptions = PlotCompilerOptions.default
  ): Either[GraphicsError, TrainedPlot[Row]] =
    for
      plans <- MappingPhase.plan(plot)
      scales <- ScalePhase.train(plans)
      layers <- resolveLayers(scales.plans)
      ranges <- LayoutPhase.panelRangesFor(options, layers)
      specs <- GuidePhase.specs(
        options.guides,
        scales.registry,
        ranges,
        relativeLegend = options.policy.nonEmpty
      )
      resolution <- LayoutPhase.assemble(plot.coord, options, ranges, specs)
      guides <- GuidePhase.lower(
        resolution.layout,
        resolution.frames,
        specs,
        options.policy.getOrElse(LayoutPolicy())
      )
    yield TrainedPlot(layers, resolution.layout, guides, scales.registry)

  private def resolveLayers[Row](
      plans: Vector[LayerPlan[Row]]
  ): Either[GraphicsError, Vector[ResolvedLayer[Row]]] =
    val out = Vector.newBuilder[ResolvedLayer[Row]]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < plans.length && result.isRight do
      result = resolveLayer(plans(idx)).map { layer =>
        out += layer
        ()
      }
      idx += 1
    result.map(_ => out.result())

  private def resolveLayer[Row](plan: LayerPlan[Row]): Either[GraphicsError, ResolvedLayer[Row]] =
    val registry = ScalePhase.registry(plan)
    RowPhase.resolve(plan).flatMap { case (rows, droppedRows) =>
      GeomPhase.lower(plan.layer.geom, rows).map { grobs =>
        ResolvedLayer(
          layerIndex = plan.layerIndex,
          geom = plan.layer.geom,
          stat = plan.layer.stat,
          dataSize = plan.data.length,
          mapping = plan.mapping,
          scaleDeclarations = registry.declarations(plan.layerIndex),
          trainedScales = registry.trained,
          rows = rows,
          droppedRows = droppedRows,
          grobs = grobs
        )
      }
    }
