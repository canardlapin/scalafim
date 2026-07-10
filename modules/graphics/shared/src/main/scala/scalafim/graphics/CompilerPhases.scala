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

/** Phase 1 — mapping resolution: merge layer and plot mappings, validate the
  * geom's required aesthetics, and reject unsupported stats and geoms before
  * any row is evaluated.
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
    if layer.stat != Stat.Identity then Left(GraphicsError.UnsupportedStat(layer.stat.toString))
    else if !isSupported(layer.geom) then Left(GraphicsError.UnsupportedGeom(layer.geom.label))
    else
      val mapping = layer.effectiveMapping(plot.mapping)
      Layer.validate(layer.geom, mapping).map { _ =>
        LayerPlan(layerIndex, layer, layer.effectiveData(plot.data), mapping, mapping.env)
      }

  private def isSupported(geom: Geom): Boolean =
    geom match
      case Geom.Point | Geom.Line | Geom.Text => true
      case Geom.Rect                          => false

/** Phase 2 — scale resolution: register each scaled binding once per layer. */
private[graphics] object ScalePhase:
  def registry[Row](plan: LayerPlan[Row]): ScaleRegistry[Row] =
    ScaleRegistry.fromEnv(plan.env)

/** Phase 3 — row evaluation: map each data row through the aesthetic
  * environment, keeping typed drop diagnostics for rows a renderer must skip.
  */
private[graphics] object RowPhase:
  def resolve[Row](
      plan: LayerPlan[Row]
  ): Either[GraphicsError, (Vector[ResolvedRow[Row]], Vector[DroppedRow[Row]])] =
    val rows = Vector.newBuilder[ResolvedRow[Row]]
    val dropped = Vector.newBuilder[DroppedRow[Row]]
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < plan.data.length && result.isRight do
      val source = plan.data(idx)
      resolveRow(idx, source, plan.layer, plan.env) match
        case RowResolution.Resolved(row) =>
          rows += row
        case RowResolution.Dropped(reason) =>
          dropped += DroppedRow(plan.layerIndex, idx, source, reason)
        case RowResolution.Failed(error) =>
          result = Left(error)
      idx += 1
    result.map(_ => (rows.result(), dropped.result()))

  private def resolveRow[Row](
      rowIndex: Int,
      source: Row,
      layer: Layer[Row],
      env: AesEnv[Row]
  ): RowResolution[Row] =
    val resolved =
      for
        x <- requiredAes(Aesthetic.X, env.get(Aesthetic.X), source)
        y <- requiredAes(Aesthetic.Y, env.get(Aesthetic.Y), source)
        _ <- finitePosition(x, y)
        text <- labelValue(layer.geom, env, source)
        group <- optionalAes(Aesthetic.Group, env.get(Aesthetic.Group), source)
        gp <- rowGraphicParams(source, env, layer.params)
        size <- rowSize(source, env)
      yield
        ResolvedRow(
          rowIndex = rowIndex,
          source = source,
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
      row: Row,
      env: AesEnv[Row],
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
          alpha = alpha.getOrElse(base.alpha),
          fontFamily = base.fontFamily,
          fontSize = base.fontSize
        )
        .left
        .map(error => PlotDropReason.InvalidAesthetic("gp", error.message))
    yield gp

  private def rowSize[Row](row: Row, env: AesEnv[Row]): Either[PlotDropReason, ExtentExpr] =
    optionalAes(Aesthetic.Size, env.get(Aesthetic.Size), row).flatMap {
      case None =>
        Right(ExtentExpr.pointsUnsafe(4.0))
      case Some(size) =>
        ExtentExpr
          .points(size)
          .left
          .map(error => PlotDropReason.InvalidAesthetic("size", error.message))
    }

  private def labelValue[Row](
      geom: Geom,
      env: AesEnv[Row],
      row: Row
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

/** Phase 4 — geom lowering: turn resolved rows into grobs. Lowering is
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

/** Phase 5 — layout resolution: use the explicit panel layout when given,
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
      specs: Vector[GuideSpec]
  ): Either[GraphicsError, LayoutResolution] =
    val clip = coordClip(coord)
    (options.layout, options.frame, options.policy, ranges) match
      case (Some(layout), _, _, _) =>
        Right(LayoutResolution(Some(layout.withClip(clip)), None))
      case (None, Some(frame), _, Some((xRange, yRange))) =>
        Right(LayoutResolution(Some(PanelLayout(frame, xRange, yRange, options.margins, clip)), None))
      case (None, None, Some(policy), Some((xRange, yRange))) =>
        PlotLayoutSolver.solve(policy, layoutRequest(specs, xRange, yRange)).map { frames =>
          LayoutResolution(
            Some(PanelLayout(frames.panel, xRange, yRange, options.margins, clip)),
            Some(frames)
          )
        }
      case _ =>
        if options.guides.requiresLayout then Left(GraphicsError.MissingLayout("guides"))
        else Right(LayoutResolution(None, None))

  private def layoutRequest(
      specs: Vector[GuideSpec],
      xRange: Interval,
      yRange: Interval
  ): PlotLayoutRequest =
    val axes = specs.collect { case axis: GuideSpec.Axis =>
      val range = if axis.side.isHorizontal then xRange else yRange
      axis.side -> axisLabels(axis, range)
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
    PlotLayoutRequest(axes, legend)

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
      if layer.trainedScales.exists(_.aesthetic == aesthetic) then
        sawScaled = true
        range = range.train(Vector(0.0, 1.0)).train(layer.rows.iterator.map(value))
      else
        if layer.rows.nonEmpty then sawUnscaledData = true
        range = range.train(layer.rows.iterator.map(value))
    }
    if sawScaled && sawUnscaledData then Left(GraphicsError.MixedPositionScaling(aesthetic))
    else range.requireTrained

  private def coordClip(coord: Coord): Clip =
    coord match
      case Coord.Cartesian(clip) => clip

/** Phase 6 — guide resolution: determine guide specs from the policy (deriving
  * routine axes and legends from trained scales) and lower them against the
  * panel layout.
  */
private[graphics] object GuidePhase:
  def specs[Row](
      policy: GuidePolicy,
      layers: Vector[ResolvedLayer[Row]],
      ranges: Option[(Interval, Interval)],
      relativeLegend: Boolean
  ): Either[GraphicsError, Vector[GuideSpec]] =
    policy match
      case GuidePolicy.NoGuides =>
        Right(Vector.empty)
      case GuidePolicy.Explicit(explicit) =>
        Right(explicit)
      case GuidePolicy.Derived(overrides, deriveLegends) =>
        ranges match
          case None =>
            Left(GraphicsError.MissingLayout("guides"))
          case Some((xRange, yRange)) =>
            derived(layers, xRange, yRange, overrides, deriveLegends, relativeLegend)

  private def derived[Row](
      layers: Vector[ResolvedLayer[Row]],
      xRange: Interval,
      yRange: Interval,
      overrides: Vector[GuideSpec],
      deriveLegends: Boolean,
      relativeLegend: Boolean
  ): Either[GraphicsError, Vector[GuideSpec]] =
    val overriddenSides = overrides.collect { case axis: GuideSpec.Axis => axis.side }.toSet
    val hasLegendOverride = overrides.exists {
      case _: GuideSpec.Legend => true
      case _                   => false
    }
    for
      xAxis <-
        if overriddenSides.contains(AxisSide.Bottom) then Right(None)
        else positionAxis(layers, Aesthetic.X.label, AxisSide.Bottom, xRange)
      yAxis <-
        if overriddenSides.contains(AxisSide.Left) then Right(None)
        else positionAxis(layers, Aesthetic.Y.label, AxisSide.Left, yRange)
      legends <-
        if hasLegendOverride || !deriveLegends then Right(Vector.empty)
        else discreteLegends(layers, relativeLegend)
    yield Vector(xAxis, yAxis).flatten ++ overrides ++ legends

  /** Derive an axis for a position aesthetic. A trained continuous scale
    * provides breaks and labels in the raw data domain, positioned in mapped
    * unit space; an unscaled position takes default breaks over the panel
    * range. Both carry explicit ticks so the layout solver can size strips
    * from the actual labels.
    */
  private def positionAxis[Row](
      layers: Vector[ResolvedLayer[Row]],
      aesthetic: String,
      side: AxisSide,
      range: Interval
  ): Either[GraphicsError, Option[GuideSpec.Axis]] =
    val name = GraphicsName.unsafe(s"$aesthetic-axis")
    firstScale(layers, aesthetic) match
      case Some(trained) =>
        trained.scale match
          case continuous: ContinuousScale[?] =>
            scaledTicks(continuous).map { ticks =>
              Some(GuideSpec.Axis(side, ticks = Some(ticks), name = Some(name)))
            }
          case _ =>
            defaultTicks(side, range, name)
      case None =>
        defaultTicks(side, range, name)

  private def defaultTicks(
      side: AxisSide,
      range: Interval,
      name: GraphicsName
  ): Either[GraphicsError, Option[GuideSpec.Axis]] =
    Axis.ticks(range, Breaks.default, Labeler.default).map { ticks =>
      Some(GuideSpec.Axis(side, ticks = Some(ticks), name = Some(name)))
    }

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
  private def discreteLegends[Row](
      layers: Vector[ResolvedLayer[Row]],
      relative: Boolean
  ): Either[GraphicsError, Vector[GuideSpec]] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    val out = Vector.newBuilder[GuideSpec]
    var result: Either[GraphicsError, Unit] = Right(())
    val originX = if relative then 0.08 else 0.82
    var nextY = if relative then 0.92 else 0.88
    layers.foreach { layer =>
      layer.trainedScales.foreach { trained =>
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

  private def firstScale[Row](
      layers: Vector[ResolvedLayer[Row]],
      aesthetic: String
  ): Option[TrainedScale] =
    layers.iterator.flatMap(_.trainedScales).find(_.aesthetic == aesthetic)

  def lower(
      layout: Option[PanelLayout],
      frames: Option[PlotFrames],
      specs: Vector[GuideSpec],
      policy: LayoutPolicy = LayoutPolicy()
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
            result = GuideSpec.lower(specs(idx), panel, legendViewport, policy).map { guide =>
              out += guide
              ()
            }
            idx += 1
          result.map(_ => out.result())
