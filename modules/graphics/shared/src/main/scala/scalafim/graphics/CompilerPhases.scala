package scalafim.graphics

/** Output of the mapping-resolution phase: one plan per layer with effective
  * data, effective mapping, and its normalized aesthetic environment.
  */
private[graphics] final case class LayerPlan[Row](
    layerIndex: Int,
    layer: Layer[Row],
    data: Vector[Row],
    mapping: AesSpec[Row],
    env: AesEnv[Row]
)

/** A layer after its statistical transform. Every stat emits the same typed
  * row envelope, so scale training remains plot-wide even when layers have
  * different statistics.
  */
private[graphics] final case class StatPlan[Row](
    source: LayerPlan[Row],
    frame: StatFrame[Row],
    mapping: AesSpec[StatRow[Row]],
    env: AesEnv[StatRow[Row]]
):
  def layerIndex: Int = source.layerIndex
  def layer: Layer[Row] = source.layer
  def data: Vector[StatRow[Row]] = frame.rows

/** Phase 1 — mapping resolution: merge layer and plot mappings, validate the
  * input contract, and reject unsupported geoms before any row is evaluated.
  */
private[graphics] object MappingPhase:
  def plan[Row](plot: Plot[Row]): Either[GraphicsError, Vector[LayerPlan[Row]]] =
    val out = Vector.newBuilder[LayerPlan[Row]]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < plot.layers.length && result.isRight do
      result = planLayer(plot, plot.layers(idx), idx).map { plan =>
        out += plan
        ()
      }
      idx += 1
    result.map(_ => out.result())

  def planLayer[Row](plot: Plot[Row], layer: Layer[Row], layerIndex: Int): Either[GraphicsError, LayerPlan[Row]] =
    if !isSupported(layer.geom) then Left(GraphicsError.UnsupportedGeom(layer.geom.label))
    else
      val mapping = layer.effectiveMapping(plot.mapping)
      Layer.validate(layer, mapping).map { _ =>
        LayerPlan(layerIndex, layer, layer.effectiveData(plot.data), mapping, mapping.env)
      }

  private def isSupported(geom: Geom): Boolean =
    geom match
      case Geom.Point | Geom.Line | Geom.Text | Geom.Bar => true
      case Geom.Rect                                     => false

/** Phase 2 — statistical transformation. Identity only lifts the source
  * mapping into a stat row. Count aggregates by its typed key, creates count
  * and proportion fields, and owns the discrete x scale plus computed y.
  */
private[graphics] object StatPhase:
  def transform[Row](plans: Vector[LayerPlan[Row]]): Either[GraphicsError, Vector[StatPlan[Row]]] =
    val out = Vector.newBuilder[StatPlan[Row]]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < plans.length && result.isRight do
      result = transform(plans(idx)).map { plan =>
        out += plan
        ()
      }
      idx += 1
    result.map(_ => out.result())

  def transform[Row](plan: LayerPlan[Row]): Either[GraphicsError, StatPlan[Row]] =
    plan.layer.stat match
      case Stat.Identity =>
        val rows = plan.data.map(row => StatRow(row, Vector(row), None, ComputedValues.empty))
        val frame = StatFrame(rows, Set.empty)
        val mapping = plan.mapping.contramap[StatRow[Row]](_.source)
        Right(StatPlan(plan, frame, mapping, mapping.env))
      case count: Stat.Count[?] =>
        countFrame(plan, count.asInstanceOf[Stat.Count[Row]])

  private def countFrame[Row](
      plan: LayerPlan[Row],
      stat: Stat.Count[Row]
  ): Either[GraphicsError, StatPlan[Row]] =
    if plan.data.isEmpty then
      val mapping = countMapping[Row](stat)
      mapping.map { resolved =>
        StatPlan(
          plan,
          StatFrame(Vector.empty, Set(ComputedAesthetic.Count, ComputedAesthetic.Proportion)),
          resolved,
          resolved.env
        )
      }
    else
      val keys = plan.data.map(stat.x)
      val order = stat.order.arrange(keys)
      val groups = scala.collection.mutable.HashMap.empty[String, scala.collection.mutable.ArrayBuffer[Row]]
      plan.data.zip(keys).foreach { case (row, key) =>
        groups.getOrElseUpdate(key, scala.collection.mutable.ArrayBuffer.empty) += row
      }
      val rows = order.map { key =>
        val members = groups(key).toVector
        StatRow(
          source = members.head,
          members = members,
          category = Some(key),
          computed = ComputedValues.counted(members.length, plan.data.length)
        )
      }
      countMapping[Row](stat).map { mapping =>
        StatPlan(
          plan,
          StatFrame(rows, Set(ComputedAesthetic.Count, ComputedAesthetic.Proportion)),
          mapping,
          mapping.env
        )
      }

  private def countMapping[Row](
      stat: Stat.Count[Row]
  ): Either[GraphicsError, AesSpec[StatRow[Row]]] =
    DiscreteScale(stat.scaleName.value, DiscreteDomain.empty, DiscretePalette.indices).map { scale =>
      AesSpec[StatRow[Row]](
        x = Some(AesValue.scaled(_.category.getOrElse(""), scale)),
        y = Some(AesValue.direct(_.computed.get(ComputedAesthetic.Count).getOrElse(0.0)))
      )
    }

/** Output of plot-wide scale training: every layer plan is rebound to the same
  * trained scale for each aesthetic, and the plot registry contains one entry
  * per aesthetic.
  */
private[graphics] final case class ScaleResolution[Row](
    plans: Vector[StatPlan[Row]],
    registry: PlotScaleRegistry
)

/** Phase 3 — plot-wide scale training. All observations from all layers using
  * an aesthetic train one shared scale before any row is mapped. Distinct
  * scale declarations for the same aesthetic are rejected instead of silently
  * placing independently normalized layers on one axis.
  */
private[graphics] object ScalePhase:
  private final case class Contribution[Row](
      layerIndex: Int,
      rows: Vector[StatRow[Row]],
      entry: RegisteredScale[StatRow[Row]]
  )

  def train[Row](plans: Vector[StatPlan[Row]]): Either[GraphicsError, ScaleResolution[Row]] =
    val initial = ScaleResolution(plans, PlotScaleRegistry.empty)
    Aesthetic.values.foldLeft[Either[GraphicsError, ScaleResolution[Row]]](Right(initial)) {
      (result, aesthetic) => result.flatMap(trainAesthetic(_, aesthetic))
    }

  def registry[Row](plan: StatPlan[Row]): ScaleRegistry[StatRow[Row]] =
    ScaleRegistry.fromEnv(plan.env)

  private def trainAesthetic[Row](
      resolution: ScaleResolution[Row],
      aesthetic: Aesthetic[?]
  ): Either[GraphicsError, ScaleResolution[Row]] =
    val contributions = resolution.plans.flatMap { plan =>
      plan.env.scaledEntry(aesthetic).map(Contribution(plan.layerIndex, plan.data, _))
    }
    contributions.headOption match
      case None =>
        Right(resolution)
      case Some(first) =>
        contributions.find(contribution => !first.entry.sharesDeclaration(contribution.entry)) match
          case Some(conflicting) =>
            Left(
              GraphicsError.ConflictingPlotScales(
                aesthetic.label,
                first.layerIndex,
                first.entry.descriptor.name.value,
                conflicting.layerIndex,
                conflicting.entry.descriptor.name.value
              )
            )
          case None =>
            val observations = contributions.flatMap(contribution => contribution.entry.observations(contribution.rows))
            for
              trained <- first.entry.trainPlotWide(observations)
              plans <- rebind(resolution.plans, aesthetic, observations)
            yield
              ScaleResolution(
                plans,
                PlotScaleRegistry.from(resolution.registry.scales :+ trained.trained)
              )

  private def rebind[Row](
      plans: Vector[StatPlan[Row]],
      aesthetic: Aesthetic[?],
      observations: Vector[ScaleObservation]
  ): Either[GraphicsError, Vector[StatPlan[Row]]] =
    val out = Vector.newBuilder[StatPlan[Row]]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < plans.length && result.isRight do
      val plan = plans(idx)
      plan.env.scaledEntry(aesthetic) match
        case None =>
          out += plan
        case Some(entry) =>
          result = entry.trainPlotWide(observations).map { trained =>
            val env = trained.install(plan.env)
            out += plan.copy(mapping = AesSpec.fromEnv(env), env = env)
            ()
          }
      idx += 1
    result.map(_ => out.result())

/** Phase 4 — row evaluation: map each stat row through the aesthetic
  * environment, keeping typed drop diagnostics for rows a renderer must skip.
  */
private[graphics] object RowPhase:
  def resolve[Row](
      plan: StatPlan[Row],
      theme: Theme = Theme.default
  ): Either[GraphicsError, (Vector[ResolvedRow[Row]], Vector[DroppedRow[Row]])] =
    val rows = Vector.newBuilder[ResolvedRow[Row]]
    val dropped = Vector.newBuilder[DroppedRow[Row]]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < plan.data.length && result.isRight do
      val source = plan.data(idx)
      resolveRow(idx, source, plan.layer, plan.env, theme) match
        case RowResolution.Resolved(row) =>
          rows += row
        case RowResolution.Dropped(reason) =>
          dropped += DroppedRow(plan.layerIndex, idx, source.source, reason)
        case RowResolution.Failed(error) =>
          result = Left(error)
      idx += 1
    result.map(_ => (rows.result(), dropped.result()))

  private def resolveRow[Row](
      rowIndex: Int,
      source: StatRow[Row],
      layer: Layer[Row],
      env: AesEnv[StatRow[Row]],
      theme: Theme
  ): RowResolution[Row] =
    val resolved =
      for
        x <- requiredAes(Aesthetic.X, env.get(Aesthetic.X), source)
        y <- requiredAes(Aesthetic.Y, env.get(Aesthetic.Y), source)
        _ <- finitePosition(x, y)
        text <- labelValue(layer.geom, env, source)
        group <- optionalAes(Aesthetic.Group, env.get(Aesthetic.Group), source)
        gp <- rowGraphicParams(source, env, layer.params.getOrElse(theme.geom))
        size <- rowSize(source, env, theme.pointSizePt)
      yield
        ResolvedRow(
          rowIndex = rowIndex,
          source = source.source,
          x = x,
          y = y,
          point = Point.nativeUnsafe(x, y),
          label = if layer.geom == Geom.Text then Some(text) else None,
          group = group,
          gp = gp,
          size = size
        )
    resolved match
      case Right(row)   => RowResolution.Resolved(row)
      case Left(reason) => RowResolution.Dropped(reason)

  private def rowGraphicParams[Row](
      row: StatRow[Row],
      env: AesEnv[StatRow[Row]],
      base: GraphicParams
  ): Either[PlotDropReason, GraphicParams] =
    for
      stroke <- optionalAes(Aesthetic.Color, env.get(Aesthetic.Color), row)
      fill <- optionalAes(Aesthetic.Fill, env.get(Aesthetic.Fill), row)
      alpha <- optionalAes(Aesthetic.Alpha, env.get(Aesthetic.Alpha), row)
      gp <- GraphicParams
        .checked(
          stroke = stroke.orElse(base.stroke),
          fill = fill.orElse(base.fill),
          lineWidth = base.lineWidth,
          lineType = base.lineType,
          lineCap = base.lineCap,
          lineJoin = base.lineJoin,
          alpha = alpha.getOrElse(base.alpha),
          fontFamily = base.fontFamily,
          fontSize = base.fontSize
        )
        .left
        .map(error => PlotDropReason.InvalidAesthetic("gp", error.message))
    yield gp

  private def rowSize[Row](
      row: StatRow[Row],
      env: AesEnv[StatRow[Row]],
      defaultSizePt: Double
  ): Either[PlotDropReason, ExtentExpr] =
    optionalAes(Aesthetic.Size, env.get(Aesthetic.Size), row).flatMap {
      case None =>
        Right(ExtentExpr.pointsUnsafe(defaultSizePt))
      case Some(size) =>
        ExtentExpr
          .points(size)
          .left
          .map(error => PlotDropReason.InvalidAesthetic("size", error.message))
    }

  private def labelValue[Row](
      geom: Geom,
      env: AesEnv[StatRow[Row]],
      row: StatRow[Row]
  ): Either[PlotDropReason, String] =
    geom match
      case Geom.Text =>
        requiredAes(Aesthetic.Label, env.get(Aesthetic.Label), row)
      case _ =>
        Right("")

  private def finitePosition(x: Double, y: Double): Either[PlotDropReason, Unit] =
    if x.isFinite && y.isFinite then Right(())
    else Left(PlotDropReason.NonFinitePosition(x, y))

  private def requiredAes[Row, A](
      aesthetic: Aesthetic[A],
      value: Option[AesValue[Row, A]],
      row: Row
  ): Either[PlotDropReason, A] =
    value match
      case None      => Left(PlotDropReason.MissingAesthetic(aesthetic.label))
      case Some(aes) => evalAes(aesthetic, aes, row)

  private def optionalAes[Row, A](
      aesthetic: Aesthetic[A],
      value: Option[AesValue[Row, A]],
      row: Row
  ): Either[PlotDropReason, Option[A]] =
    value match
      case None      => Right(None)
      case Some(aes) => evalAes(aesthetic, aes, row).map(Some(_))

  private def evalAes[Row, A](
      aesthetic: Aesthetic[A],
      value: AesValue[Row, A],
      row: Row
  ): Either[PlotDropReason, A] =
    value match
      case AesValue.Direct(f) =>
        Right(f(row))
      case AesValue.Constant(v) =>
        Right(v)
      case scaled: AesValue.Scaled[Row, ?, A] =>
        scaled
          .scale
          .mapValueResult(scaled.value(row))
          .left
          .map(toDropReason(aesthetic, _))

  private def toDropReason[A](
      aesthetic: Aesthetic[A],
      failure: ScaleMapFailure
  ): PlotDropReason =
    failure match
      case ScaleMapFailure.TransformDomain(transform, value) =>
        PlotDropReason.TransformDomain(aesthetic.label, transform, value)
      case ScaleMapFailure.OutOfDomain(scale, value) =>
        PlotDropReason.ScaleOutOfDomain(aesthetic.label, scale, value)

  private enum RowResolution[Row]:
    case Resolved(row: ResolvedRow[Row])
    case Dropped(reason: PlotDropReason)
    case Failed(error: GraphicsError)

/** Phase 5 — geom lowering: turn resolved rows into grobs. Lowering is
  * group-aware: layers honoring the group aesthetic lower to one grob per
  * group carrying that group's graphic params.
  */
private[graphics] object GeomPhase:
  def lower[Row](
      geom: Geom,
      rows: Vector[ResolvedRow[Row]]
  ): Either[GraphicsError, Vector[Grob]] =
    geom match
      case Geom.Point =>
        pointGrobs(rows)
      case Geom.Line =>
        lineGrobs(rows)
      case Geom.Text =>
        textGrobs(rows)
      case Geom.Bar =>
        barGrobs(rows)
      case Geom.Rect =>
        Left(GraphicsError.UnsupportedGeom(Geom.Rect.label))

  private def pointGrobs[Row](rows: Vector[ResolvedRow[Row]]): Either[GraphicsError, Vector[Grob]] =
    val out = Vector.newBuilder[Grob]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < rows.length && result.isRight do
      val row = rows(idx)
      result = Grob.points(Vector(row.point), size = row.size, gp = row.gp).map { grob =>
        out += grob
        ()
      }
      idx += 1
    result.map(_ => out.result())

  private def lineGrobs[Row](rows: Vector[ResolvedRow[Row]]): Either[GraphicsError, Vector[Grob]] =
    val groups = groupInOrder(rows)
    val out = Vector.newBuilder[Grob]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < groups.length && result.isRight do
      val group = groups(idx)
      if group.length >= 2 then
        result = Grob.lines(group.map(_.point), gp = group.head.gp).map { grob =>
          out += grob
          ()
        }
      idx += 1
    result.map(_ => out.result())

  private def textGrobs[Row](rows: Vector[ResolvedRow[Row]]): Either[GraphicsError, Vector[Grob]] =
    val out = Vector.newBuilder[Grob]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < rows.length && result.isRight do
      val row = rows(idx)
      result = Grob.text(row.label.getOrElse(""), row.point, gp = row.gp).map { grob =>
        out += grob
        ()
      }
      idx += 1
    result.map(_ => out.result())

  private def barGrobs[Row](rows: Vector[ResolvedRow[Row]]): Either[GraphicsError, Vector[Grob]] =
    val out = Vector.newBuilder[Grob]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < rows.length && result.isRight do
      val row = rows(idx)
      val height = math.abs(row.y)
      val centerY = math.min(0.0, row.y) + height / 2.0
      result = Grob
        .rect(
          center = Point.nativeUnsafe(row.x, centerY),
          size = Size.fromExtents(ExtentExpr.nativeUnsafe(0.9), ExtentExpr.nativeUnsafe(height)),
          gp = row.gp,
          name = Some(GraphicsName.unsafe(s"stat-count-bar-$idx"))
        )
        .map { grob =>
          out += grob
          ()
        }
      idx += 1
    result.map(_ => out.result())

  /** Partition rows by their group value, preserving first-encounter order of
    * groups and row order within each group.
    */
  private def groupInOrder[Row](rows: Vector[ResolvedRow[Row]]): Vector[Vector[ResolvedRow[Row]]] =
    if rows.forall(_.group.isEmpty) then
      if rows.isEmpty then Vector.empty else Vector(rows)
    else
      val order = Vector.newBuilder[Option[String]]
      val buckets = scala.collection.mutable.HashMap.empty[Option[String], scala.collection.mutable.ArrayBuffer[ResolvedRow[Row]]]
      rows.foreach { row =>
        val bucket = buckets.getOrElseUpdate(
          row.group, {
            order += row.group
            scala.collection.mutable.ArrayBuffer.empty[ResolvedRow[Row]]
          }
        )
        bucket += row
      }
      order.result().map(key => buckets(key).toVector)

/** Phase 6 — layout resolution: use the explicit panel layout when given,
  * or derive one from an explicit frame plus panel data ranges computed from
  * the layers' position scales (mapped space is the unit interval) or their
  * resolved row values when a position is unscaled.
  */
private[graphics] object LayoutPhase:
  final case class LayoutResolution(layout: Option[PanelLayout], frames: Option[PlotFrames])

  /** Panel data ranges when any layout source (explicit layout, frame, or
    * solver policy) is in play; `None` when the plot compiles layout-free.
    */
  def panelRangesFor[Row](
      options: PlotCompilerOptions,
      layers: Vector[ResolvedLayer[Row]]
  ): Either[GraphicsError, Option[(Interval, Interval)]] =
    options.layout match
      case Some(layout) =>
        Right(Some((layout.xScale, layout.yScale)))
      case None if options.frame.nonEmpty || options.policy.nonEmpty =>
        panelRanges(layers).map(Some(_))
      case None =>
        Right(None)

  def assemble(
      coord: Coord,
      options: PlotCompilerOptions,
      ranges: Option[(Interval, Interval)],
      specs: Vector[GuideSpec],
      labels: PlotLabels
  ): Either[GraphicsError, LayoutResolution] =
    val clip = coordClip(coord)
    (options.layout, options.frame, options.policy, ranges) match
      case (Some(layout), _, _, _) =>
        Right(LayoutResolution(Some(layout.withClip(clip)), None))
      case (None, Some(frame), _, Some((xRange, yRange))) =>
        expandedRanges(options.expansion, xRange, yRange).map { case (expandedX, expandedY) =>
          LayoutResolution(Some(PanelLayout(frame, expandedX, expandedY, options.margins, clip)), None)
        }
      case (None, None, Some(policy), Some((xRange, yRange))) =>
        for
          frames <- PlotLayoutSolver.solve(policy, layoutRequest(specs, xRange, yRange, labels))
          expanded <- expandedRanges(options.expansion, xRange, yRange)
        yield
          val (expandedX, expandedY) = expanded
          LayoutResolution(
            Some(PanelLayout(frames.panel, expandedX, expandedY, options.margins, clip)),
            Some(frames)
          )
      case _ =>
        if options.guides.requiresLayout then Left(GraphicsError.MissingLayout("guides"))
        else Right(LayoutResolution(None, None))

  private def expandedRanges(
      expansion: RangeExpansion,
      xRange: Interval,
      yRange: Interval
  ): Either[GraphicsError, (Interval, Interval)] =
    for
      x <- expansion.expand(xRange)
      y <- expansion.expand(yRange)
    yield (x, y)

  private def layoutRequest(
      specs: Vector[GuideSpec],
      xRange: Interval,
      yRange: Interval,
      labels: PlotLabels
  ): PlotLayoutRequest =
    val axes = specs.collect { case axis: GuideSpec.Axis =>
      val range = if axis.side.isHorizontal then xRange else yRange
      axis.side -> AxisRequest(axisLabels(axis, range), axis.title)
    }.toMap
    val legends = specs.collect { case legend: GuideSpec.Legend => legend }
    val legend =
      if legends.isEmpty then None
      else
        Some(
          LegendRequest(
            legends.head.title,
            legends.flatMap(_.entries.map(_.label)) ++ legends.drop(1).flatMap(_.title)
          )
        )
    PlotLayoutRequest(axes, legend, labels)

  private def axisLabels(axis: GuideSpec.Axis, range: Interval): Vector[String] =
    axis.ticks match
      case Some(ticks) =>
        ticks.map(_.label)
      case None =>
        Axis.ticks(range, axis.breaks, axis.labeler).map(_.map(_.label)).getOrElse(Vector.empty)

  def panelRanges[Row](
      layers: Vector[ResolvedLayer[Row]]
  ): Either[GraphicsError, (Interval, Interval)] =
    for
      xRange <- positionRange(layers, Aesthetic.X.label, _.x)
      yRange <- positionRange(layers, Aesthetic.Y.label, _.y)
    yield (xRange, yRange)

  /** Union of the position ranges contributed by each layer. Scaled layers
    * live in mapped unit space (trained with the unit interval plus their
    * actual mapped rows, so an `OobPolicy.Keep` overflow widens the panel
    * rather than silently clipping); unscaled layers contribute raw row
    * values. Mixing the two across layers is incoherent — mapped and raw
    * coordinates share no unit — and is a typed error.
    */
  private def positionRange[Row](
      layers: Vector[ResolvedLayer[Row]],
      aesthetic: String,
      value: ResolvedRow[Row] => Double
  ): Either[GraphicsError, Interval] =
    var sawScaled = false
    var sawUnscaledData = false
    var range = ContinuousRange.empty
    layers.foreach { layer =>
      val values = layer.rows.iterator.map(value).toVector
      layer.trainedScales.find(_.aesthetic == aesthetic) match
        case Some(scale) =>
          sawScaled = true
          if scale.descriptor.kind == ScaleKind.Continuous then
            range = range.train(Vector(0.0, 1.0))
          range = range.train(values)
        case None =>
          if layer.rows.nonEmpty then sawUnscaledData = true
          range = range.train(values)
      if layer.geom == Geom.Bar then
        if aesthetic == Aesthetic.X.label then
          range = range.train(values.iterator.flatMap(x => Iterator(x - 0.45, x + 0.45)))
        else if aesthetic == Aesthetic.Y.label then
          range = range.train(Iterator.single(0.0))
    }
    if sawScaled && sawUnscaledData then Left(GraphicsError.MixedPositionScaling(aesthetic))
    else range.requireTrained

  private def coordClip(coord: Coord): Clip =
    coord match
      case Coord.Cartesian(clip) => clip

/** Structural plot text lowers into solver-owned regions before any backend
  * sees the scene. Axis titles remain guide children; title and subtitle are
  * top-level text grobs in dedicated viewports.
  */
private[graphics] object PlotLabelPhase:
  def lower(
      labels: PlotLabels,
      frames: Option[PlotFrames],
      theme: PlotTextTheme
  ): Either[GraphicsError, Vector[Grob]] =
    val needsHeader = labels.title.nonEmpty || labels.subtitle.nonEmpty
    if !needsHeader then Right(Vector.empty)
    else
      frames match
        case None => Left(GraphicsError.MissingLayout("plot title"))
        case Some(solved) =>
          val out = Vector.newBuilder[Grob]
          for
            _ <- addLabel(
              labels.title,
              solved.titleViewport,
              theme.title,
              PlotRegion.Title,
              out
            )
            _ <- addLabel(
              labels.subtitle,
              solved.subtitleViewport,
              theme.subtitle,
              PlotRegion.Subtitle,
              out
            )
          yield out.result()

  private def addLabel(
      text: Option[String],
      viewport: Option[Viewport],
      gp: GraphicParams,
      name: GraphicsName,
      out: scala.collection.mutable.Builder[Grob, Vector[Grob]]
  ): Either[GraphicsError, Unit] =
    text match
      case None => Right(())
      case Some(label) =>
        viewport match
          case None => Left(GraphicsError.MissingLayout(name.value))
          case Some(frame) =>
            Grob
              .text(
                label,
                Point.npcUnsafe(0.0, 0.5),
                anchor = Anchor(HJust.Left, VJust.Center),
                gp = gp,
                viewport = Some(frame),
                name = Some(name)
              )
              .map { grob =>
                out += grob
                ()
              }

/** Phase 7 — guide resolution: determine guide specs from the policy (deriving
  * routine axes and legends from trained scales) and lower them against the
  * panel layout.
  */
private[graphics] object GuidePhase:
  def specs(
      policy: GuidePolicy,
      plotScales: PlotScaleRegistry,
      ranges: Option[(Interval, Interval)],
      relativeLegend: Boolean,
      labels: PlotLabels
  ): Either[GraphicsError, Vector[GuideSpec]] =
    policy match
      case GuidePolicy.NoGuides =>
        Right(Vector.empty)
      case GuidePolicy.Explicit(explicit) =>
        if explicit.isEmpty then Right(Vector.empty)
        else
          ranges match
            case Some((xRange, yRange)) => materializeAxisTicks(explicit, xRange, yRange)
            case None                   => Left(GraphicsError.MissingLayout("guides"))
      case GuidePolicy.Derived(overrides, deriveLegends) =>
        ranges match
          case None =>
            Left(GraphicsError.MissingLayout("guides"))
          case Some((xRange, yRange)) =>
            derived(plotScales, xRange, yRange, overrides, deriveLegends, relativeLegend, labels)

  private def derived(
      plotScales: PlotScaleRegistry,
      xRange: Interval,
      yRange: Interval,
      overrides: Vector[GuideSpec],
      deriveLegends: Boolean,
      relativeLegend: Boolean,
      labels: PlotLabels
  ): Either[GraphicsError, Vector[GuideSpec]] =
    val overriddenSides = overrides.collect { case axis: GuideSpec.Axis => axis.side }.toSet
    val hasLegendOverride = overrides.exists {
      case _: GuideSpec.Legend => true
      case _                   => false
    }
    for
      resolvedOverrides <- materializeAxisTicks(overrides, xRange, yRange)
      xAxis <-
        if overriddenSides.contains(AxisSide.Bottom) then Right(None)
        else positionAxis(plotScales, Aesthetic.X, AxisSide.Bottom, xRange, labels.x)
      yAxis <-
        if overriddenSides.contains(AxisSide.Left) then Right(None)
        else positionAxis(plotScales, Aesthetic.Y, AxisSide.Left, yRange, labels.y)
      legends <-
        if hasLegendOverride || !deriveLegends then Right(Vector.empty)
        else discreteLegends(plotScales, relativeLegend)
    yield Vector(xAxis, yAxis).flatten ++ resolvedOverrides ++ legends

  /** Resolve caller-supplied break policies against the unexpanded data
    * ranges. Panel padding is a view concern and must not leak into tick values
    * or labels when the guides are lowered later against the expanded layout.
    */
  private def materializeAxisTicks(
      specs: Vector[GuideSpec],
      xRange: Interval,
      yRange: Interval
  ): Either[GraphicsError, Vector[GuideSpec]] =
    val out = Vector.newBuilder[GuideSpec]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < specs.length && result.isRight do
      specs(idx) match
        case axis: GuideSpec.Axis if axis.ticks.isEmpty =>
          val range = if axis.side.isHorizontal then xRange else yRange
          result = Axis.ticks(range, axis.breaks, axis.labeler).map { ticks =>
            out += axis.copy(ticks = Some(ticks))
            ()
          }
        case spec =>
          out += spec
      idx += 1
    result.map(_ => out.result())

  /** Derive an axis for a position aesthetic. A trained continuous scale
    * provides breaks and labels in the raw data domain, positioned in mapped
    * unit space; an unscaled position takes default breaks over the panel
    * range. Both carry explicit ticks so the layout solver can size strips
    * from the actual labels.
    */
  private def positionAxis(
      plotScales: PlotScaleRegistry,
      aesthetic: Aesthetic[?],
      side: AxisSide,
      range: Interval,
      requestedTitle: Option[String]
  ): Either[GraphicsError, Option[GuideSpec.Axis]] =
    val name = GraphicsName.unsafe(s"${aesthetic.label}-axis")
    plotScales.forAesthetic(aesthetic) match
      case Some(trained) =>
        trained.scale match
          case continuous: ContinuousScale[?] =>
            scaledTicks(continuous).map { ticks =>
              Some(
                GuideSpec.Axis(
                  side,
                  ticks = Some(ticks),
                  title = requestedTitle.orElse(Some(continuous.name.value)),
                  name = Some(name)
                )
              )
            }
          case discrete: DiscreteScale[?] =>
            discretePositionTicks(discrete) match
              case Some(ticks) =>
                Right(
                  Some(
                    GuideSpec.Axis(
                      side,
                      ticks = Some(ticks),
                      title = requestedTitle.orElse(Some(discrete.name.value)),
                      name = Some(name)
                    )
                  )
                )
              case None =>
                defaultTicks(side, range, name, requestedTitle.orElse(Some(aesthetic.label)))
          case _ =>
            defaultTicks(side, range, name, requestedTitle.orElse(Some(aesthetic.label)))
      case None =>
        defaultTicks(side, range, name, requestedTitle.orElse(Some(aesthetic.label)))

  private def defaultTicks(
      side: AxisSide,
      range: Interval,
      name: GraphicsName,
      title: Option[String]
  ): Either[GraphicsError, Option[GuideSpec.Axis]] =
    Axis.ticks(range, Breaks.default, Labeler.default).map { ticks =>
      Some(GuideSpec.Axis(side, ticks = Some(ticks), title = title, name = Some(name)))
    }

  private def discretePositionTicks(scale: DiscreteScale[?]): Option[Vector[AxisTick]] =
    val out = Vector.newBuilder[AxisTick]
    var idx = 0
    var valid = true
    while idx < scale.domain.levels.length && valid do
      val level = scale.domain.levels(idx)
      scale.mapValue(level) match
        case Some(position: Double) =>
          AxisTick(position, level) match
            case Right(tick) => out += tick
            case Left(_)     => valid = false
        case _ =>
          valid = false
      idx += 1
    if valid then Some(out.result()) else None

  /** Ticks for a trained continuous scale: break values come from the scale's
    * transform in the raw data domain; positions are the mapped unit-space
    * coordinates the rows were resolved into.
    */
  private def scaledTicks(scale: ContinuousScale[?]): Either[GraphicsError, Vector[AxisTick]] =
    val breaks = scale.breaks
    val labels = scale.labels
    if labels.length != breaks.length then
      Left(GraphicsError.AxisLabelCountMismatch(breaks.length, labels.length))
    else
      val out = Vector.newBuilder[AxisTick]
      var idx = 0
      var result: Either[GraphicsError, Unit] = Right(())
      while idx < breaks.length && result.isRight do
        result = scale.transform.transform(breaks(idx)).flatMap { transformed =>
          AxisTick(scale.transformedDomain.rescale(transformed), labels(idx)).map { tick =>
            out += tick
            ()
          }
        }
        idx += 1
      result.map(_ => out.result())

  /** One legend per distinct discrete color/fill scale, with entries drawn
    * from the scale's own palette.
    */
  /** One legend per distinct discrete color/fill scale, stacked downward
    * from the top of the legend region so multiple legends never overprint.
    */
  private def discreteLegends(
      plotScales: PlotScaleRegistry,
      relative: Boolean
  ): Either[GraphicsError, Vector[GuideSpec]] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    val out = Vector.newBuilder[GuideSpec]
    var result: Either[GraphicsError, Unit] = Right(())
    val originX = if relative then 0.08 else 0.82
    var nextY = if relative then 0.92 else 0.88
    plotScales.scales.foreach { trained =>
      if result.isRight
        && (trained.aesthetic == Aesthetic.Color.label || trained.aesthetic == Aesthetic.Fill.label)
        && seen.add(trained.descriptor.name.value)
      then
        trained.scale match
          case discrete: DiscreteScale[?] =>
            result = legendFor(discrete, Point.npcUnsafe(originX, nextY)).map { legend =>
              legend.foreach { spec =>
                out += spec
                nextY -= (spec.entries.length + 1).toDouble * 0.055 + 0.04
              }
              ()
            }
          case _ =>
            ()
    }
    result.map(_ => out.result())

  private def legendFor(
      scale: DiscreteScale[?],
      origin: Point
  ): Either[GraphicsError, Option[GuideSpec.Legend]] =
    val entries = Vector.newBuilder[LegendEntry]
    var colorable = true
    var result: Either[GraphicsError, Unit] = Right(())
    scale.domain.levels.foreach { level =>
      if result.isRight && colorable then
        scale.mapValue(level) match
          case Some(color: Rgba) =>
            result = LegendEntry.color(level, color).map { entry =>
              entries += entry
              ()
            }
          case _ =>
            colorable = false
    }
    result.map { _ =>
      val resolved = entries.result()
      if !colorable || resolved.isEmpty then None
      else
        Some(
          GuideSpec.Legend(
            title = Some(scale.name.value),
            entries = resolved,
            origin = origin,
            name = Some(GraphicsName.unsafe(s"${scale.name.value}-legend"))
          )
        )
    }

  def lower(
      layout: Option[PanelLayout],
      frames: Option[PlotFrames],
      specs: Vector[GuideSpec],
      policy: LayoutPolicy = LayoutPolicy(),
      theme: Theme = Theme.default
  ): Either[GraphicsError, Vector[ResolvedGuide]] =
    if specs.isEmpty then Right(Vector.empty)
    else
      layout match
        case None =>
          Left(GraphicsError.MissingLayout("guides"))
        case Some(panel) =>
          val legendViewport = frames.flatMap(_.legendViewport())
          val out = Vector.newBuilder[ResolvedGuide]
          var idx = 0
          var result: Either[GraphicsError, Unit] = Right(())
          while idx < specs.length && result.isRight do
            result = GuideSpec.lower(specs(idx), panel, legendViewport, policy, theme).map { guide =>
              out += guide
              ()
            }
            idx += 1
          result.map(_ => out.result())

/** Panel decoration is ordinary renderer-neutral geometry. It is lowered
  * after guide derivation so grid lines use the same tick positions as axes,
  * and inserted before layer marks so data remains visually authoritative.
  */
private[graphics] object PanelPhase:
  def lower(
      layout: Option[PanelLayout],
      specs: Vector[GuideSpec],
      theme: PanelTheme
  ): Either[GraphicsError, Vector[Grob]] =
    layout match
      case None => Right(Vector.empty)
      case Some(panel) =>
        val out = Vector.newBuilder[Grob]
        theme.background.foreach { gp =>
          out += Grob.rectUnsafe(
            center = Point.npcUnsafe(0.5, 0.5),
            size = Size.npcUnsafe(1.0, 1.0),
            gp = gp,
            name = Some(PlotRegion.PanelBackground)
          )
        }
        theme.grid match
          case None => Right(out.result())
          case Some(gp) =>
            val xValues = tickValues(specs, horizontal = true).filter(panel.xScale.contains)
            val yValues = tickValues(specs, horizontal = false).filter(panel.yScale.contains)
            if xValues.nonEmpty then
              out += Grob.segments(
                xValues.map(x => Point.nativeUnsafe(x, panel.yScale.lower) -> Point.nativeUnsafe(x, panel.yScale.upper)),
                gp = gp,
                name = Some(PlotRegion.PanelGridX)
              ).orThrow
            if yValues.nonEmpty then
              out += Grob.segments(
                yValues.map(y => Point.nativeUnsafe(panel.xScale.lower, y) -> Point.nativeUnsafe(panel.xScale.upper, y)),
                gp = gp,
                name = Some(PlotRegion.PanelGridY)
              ).orThrow
            Right(out.result())

  private def tickValues(specs: Vector[GuideSpec], horizontal: Boolean): Vector[Double] =
    specs.iterator.collect {
      case axis: GuideSpec.Axis if axis.side.isHorizontal == horizontal =>
        axis.ticks.getOrElse(Vector.empty).map(_.value)
    }.flatten.toVector.distinct
