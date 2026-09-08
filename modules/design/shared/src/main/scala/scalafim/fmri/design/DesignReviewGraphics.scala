package scalafim.fmri.design

import intaglio.*

/** Renderer-neutral recipes. Recompile at the actual target dimensions. */
object DesignReviewGraphics:
  private final case class Cell(time: Double, column: Double, scaled: Double)
  private final case class PointValue(time: Double, value: Double)
  private val blue = Rgba.unsafe(63, 108, 143)
  private val paper = Rgba.unsafe(248, 249, 247)
  private val warm = Rgba.unsafe(181, 96, 55)
  private val diverging: Palette[Rgba] = t =>
    if t <= 0.5 then Palette.gradient(blue,paper)(t * 2) else Palette.gradient(paper,warm)((t - 0.5) * 2)

  private val textStyle = GraphicParams.unsafe(stroke=None,fill=Some(Rgba.unsafe(65,82,76)),fontSize=Length.pointsUnsafe(8))
  private val reviewTheme = Theme.minimal.copy(axis=Theme.minimal.axis.copy(text=textStyle,title=textStyle),
    legend=Theme.minimal.legend.copy(text=textStyle,title=textStyle))

  private def options(width: Double, height: Double, y: GuideSpec.Axis, legend: Boolean): PlotCompilerOptions =
    PlotCompilerOptions(policy = Some(LayoutPolicy(referenceDevice = DeviceContext.unsafe(width,height),
      outerMarginPt = 6, axisFontPt = 8, axisTitleFontPt = 8, colorbarHeightPt = math.max(24,math.min(90,(height-100) / 2)))),
      theme = reviewTheme,
      guides = GuidePolicy.Derived(overrides = Vector(GuideSpec.Axis(AxisSide.Bottom,title=Some("Run time (s)")),y), deriveLegends = legend))

  def matrixScene(review: DesignReview, firstColumn: Int = 0, columnCount: Int = 24,
      width: Double = 800, height: Double = 350, maximumCells: Int = 200000): Either[GraphicsError, Scene] =
    matrixPlot(review,firstColumn,columnCount,width,height,maximumCells).map(_.scene)

  def matrixPlot(review: DesignReview, firstColumn: Int = 0, columnCount: Int = 24,
      width: Double = 800, height: Double = 350, maximumCells: Int = 200000): Either[GraphicsError, TrainedPlot] =
    val indices = (firstColumn until math.min(review.columns.size, firstColumn + columnCount)).toVector
    if firstColumn < 0 || columnCount < 1 || indices.isEmpty then Left(GraphicsError.EmptyGeometry("design columns"))
    else if review.scans.size.toLong * indices.size > maximumCells then Left(GraphicsError.EmptyGeometry("design exceeds review cell budget; select fewer columns"))
    else
      val cells = indices.zipWithIndex.flatMap { (column, local) =>
        review.scans.indices.map(row => Cell(review.scans(row).time.value, -(local + 1).toDouble, review.scaled(row,column)))
      }
      val ticks = indices.zipWithIndex.map { (column, local) =>
        AxisTick.unsafe(-(local + 1).toDouble, s"${column + 1}  ${review.columns(column).label.take(28)}")
      }
      for
        fill <- ContinuousScale.fixed("Scaled", Vector(-1.0,1.0), diverging)
        program <- plot(cells).aes(_.time,_.column)
          .encode(Aesthetic.Fill, _.scaled, fill)
          .geomTile(_ => review.repetitionTime.value, _ => 1.0, params = Some(GraphicParams.unsafe(stroke=None)))
          .axisTitles("Run time (s)", "")
          .build
        scene <- PlotCompiler.resolve(program.plot.withDescription(
          s"Raw design divided by each column's maximum absolute value over all original scans; all-zero columns remain zero. Source=${review.fingerprint.value}; run=${review.run.oneBased}; scope=${review.scope}. " +
          s"Rows=${review.scans.map(s => s"${s.source.oneBased}:${s.time.value}:${s.retained}").mkString(",")}. " +
          indices.map(c => s"column=${c+1}; label=${review.columns(c).label}; id=${review.columns(c).id.value}; source-index=${review.sourceColumnIndices(c)}; peak=${review.peaks(c)}; raw=${review.scans.indices.map(r => review.raw(r,c)).mkString(",")}").mkString("\n")),
          options(width,height,GuideSpec.Axis(AxisSide.Left,ticks=Some(ticks)),legend=height >= 220))
      yield scene

  def traceScene(scans: Vector[ReviewScan], series: Vector[ReviewSeries],
      width: Double = 800, height: Double = 180): Either[GraphicsError, Scene] =
    tracePlot(scans,series,width,height).map(_.scene)

  def tracePlot(scans: Vector[ReviewScan], series: Vector[ReviewSeries],
      width: Double = 800, height: Double = 180, alignWith: Option[PanelLayout] = None): Either[GraphicsError, TrainedPlot] =
    if scans.isEmpty || series.isEmpty || series.exists(_.values.size != scans.size) then
      Left(GraphicsError.EmptyGeometry("trace values must match original scan rows"))
    else
      val segments = series.zipWithIndex.flatMap { (s, index) =>
        val parts = Vector.newBuilder[Vector[PointValue]]
        var current = Vector.empty[PointValue]
        scans.zip(s.values).foreach { (scan, value) =>
          value match
            case Some(v) => current :+= PointValue(scan.time.value,v)
            case None =>
              if current.nonEmpty then parts += current
              current = Vector.empty
        }
        if current.nonEmpty then parts += current
        parts.result().map(points => (points,index))
      }
      if segments.isEmpty then Left(GraphicsError.EmptyGeometry("no observed trace samples"))
      else
        val colors = Vector(blue,warm,Rgba.unsafe(67,137,109),Rgba.unsafe(136,95,150))
        val base = Plot(segments.flatMap(_._1)).withAxisTitles("Run time (s)",series.map(_.units).distinct.mkString(" / "))
          .withDescription(series.map(s => s"${s.name}; design-column=${s.designColumn}; units=${s.units}; values=${s.values.map(_.fold("missing")(_.toString)).mkString(",")}").mkString("\n") +
            "\nOriginal source scans=" + scans.map(s => s"${s.run.oneBased}:${s.source.oneBased}:${s.time.value}:${s.retained}").mkString(","))
        val layered = segments.foldLeft[Either[GraphicsError,Plot[PointValue]]](Right(base)) { (acc,entry) =>
          val (points,index) = entry
          val gp = Some(GraphicParams.unsafe(stroke=Some(colors(index % colors.size)),lineWidth=1.3))
          acc.flatMap(_.addLayer(if points.size == 1 then Layer.point[PointValue](_.time,_.value,data=Some(points),params=gp)
            else Layer.line[PointValue](_.time,_.value,data=Some(points),params=gp)))
        }
        layered.flatMap(p => aligned(p, options(width,height,GuideSpec.Axis(AxisSide.Left,breaks=Breaks.prettyUnsafe(if height < 140 then 2 else 4),title=if height < 140 then None else Some(series.map(_.units).distinct.mkString(" / "))),legend=false),alignWith))

  /** Effective model onsets and durations with source event-row provenance. */
  def eventTimelinePlot(review: DesignReview, width: Double = 800, height: Double = 180, alignWith: Option[PanelLayout] = None): Either[GraphicsError,TrainedPlot] =
    val events = review.schema.audit.sourceEvents.filter(_.blockId == review.run.oneBased-1)
      .distinctBy(e => (e.sourceRow,e.onset,e.duration))
    if events.isEmpty then Left(GraphicsError.EmptyGeometry("no source event timing in the compiled model"))
    else
      final case class EventSegment(start: Double,end: Double,y: Double,top: Double)
      val marks = events.map(e => EventSegment(e.onset.value,e.onset.value+e.duration.value,
        if e.duration.value == 0 then 0.0 else 0.5,if e.duration.value == 0 then 1.0 else 0.5))
      for
        time <- ContinuousScale.fixed("time",review.scans.map(_.time.value) ++ marks.flatMap(e => Vector(e.start,e.end)),Palette.numeric)
        program <- plot(marks).aes(_.start,_.y)
          .geomSegment(_.end,_.top,params=Some(GraphicParams.unsafe(stroke=Some(blue),lineWidth=1.3))).build
        positioned <- program.plot.encode(Aesthetic.X,_.start,time)
        trained <- aligned(positioned.withDescription("Effective model event timing; " + events.map(_.canonical).mkString("\n")),
          options(width,height,GuideSpec.Axis(AxisSide.Left,ticks=Some(Vector.empty)),legend=false),alignWith)
      yield trained

  /** Share the exact horizontal frame and acquisition domain across linked panels. */
  private def aligned[A](plot: Plot[A],options: PlotCompilerOptions,reference: Option[PanelLayout]): Either[GraphicsError,TrainedPlot] =
    PlotCompiler.resolve(plot,options).flatMap { natural =>
      (reference,natural.layout) match
        case (Some(target),Some(current)) =>
          val frame = current.frame.copy(origin=current.frame.origin.copy(x=target.frame.origin.x),
            size=Size.fromExtents(target.frame.size.width,current.frame.size.height))
          PlotCompiler.resolve(plot,options.copy(layout=Some(current.copy(frame=frame,xScale=target.xScale))))
        case _ => Right(natural)
    }
