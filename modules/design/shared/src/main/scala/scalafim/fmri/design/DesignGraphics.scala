package scalafim.fmri.design

import scalafim.fmri.design.event.EventModel
import scalafim.graphics.*

enum DesignGraphicsError:
  case Export(error: DesignExportError)
  case Graphics(error: GraphicsError)

  def message: String =
    this match
      case Export(error)   => error.message
      case Graphics(error) => error.message

final case class EventPlotGraphicsOptions(
    layoutPolicy: LayoutPolicy = LayoutPolicy(),
    clip: Clip = Clip.On,
    xBreaks: Breaks = Breaks.default,
    yBreaks: Breaks = Breaks.default,
    showLegend: Boolean = true,
    colors: Vector[Rgba] = DesignGraphics.defaultColors
)

object DesignGraphics:
  val defaultColors: Vector[Rgba] =
    Vector(
      Rgba.unsafe(40, 80, 120),
      Rgba.unsafe(210, 120, 40),
      Rgba.unsafe(70, 145, 85),
      Rgba.unsafe(140, 90, 170),
      Rgba.unsafe(190, 65, 80),
      Rgba.unsafe(70, 150, 165)
    )

  def eventPlot(
      data: EventPlotData,
      options: EventPlotGraphicsOptions = EventPlotGraphicsOptions()
  ): Either[GraphicsError, Plot[TracePoint]] =
    eventProgram(data, options).map(_.plot)

  def eventScene(
      data: EventPlotData,
      options: EventPlotGraphicsOptions = EventPlotGraphicsOptions()
  ): Either[GraphicsError, Scene] =
    eventProgram(data, options).flatMap(_.scene)

  def eventModelScene(
      model: EventModel,
      termName: Option[String] = None,
      options: EventPlotGraphicsOptions = EventPlotGraphicsOptions(),
      columnSelector: DesignColumnSelector = DesignColumnSelector.All
  ): Either[DesignGraphicsError, Scene] =
    DesignExports
      .eventPlotDataEither(model, termName = termName, columnSelector = columnSelector)
      .left
      .map(DesignGraphicsError.Export(_))
      .flatMap(data => eventScene(data, options).left.map(DesignGraphicsError.Graphics(_)))

  private def regressorDomain(data: EventPlotData): Either[GraphicsError, DiscreteDomain] =
    val declared =
      if data.regressors.nonEmpty then data.regressors
      else data.points.map(_.regressor).distinct
    for
      initial <- DiscreteDomain.ordered(declared)
      domain <- initial.train(data.points.map(_.regressor))
    yield domain

  private def eventProgram(
      data: EventPlotData,
      options: EventPlotGraphicsOptions
  ): Either[GraphicsError, PlotProgram[TracePoint]] =
    if data.points.isEmpty then Left(GraphicsError.EmptyGeometry("event plot"))
    else if options.colors.isEmpty then Left(GraphicsError.EmptyPalette)
    else
      for
        domain <- regressorDomain(data)
        program <- addTraceLayers(
          plot(data.points)
            .aes(_.time, _.response)
            .scaleColorDiscrete(
              _.regressor,
              levels = domain.levels,
              colors = options.colors,
              name = "regressor"
            )
            .coord(Coord.Cartesian(options.clip))
            .compilerOptions(
              PlotCompilerOptions(
                policy = Some(options.layoutPolicy),
                guides = GuidePolicy.Derived(
                  overrides = axisOverrides(options),
                  deriveLegends = options.showLegend
                )
              )
            ),
          data.points,
          domain.levels
        ).build
      yield program

  private def addTraceLayers(
      builder: PlotBuilder[TracePoint, PlotPosition.XY[TracePoint]],
      points: Vector[TracePoint],
      regressors: Vector[String]
  ): PlotBuilder[TracePoint, PlotPosition.XY[TracePoint]] =
    regressors.foldLeft(builder) { (current, regressor) =>
      val trace = points.filter(_.regressor == regressor)
      if trace.isEmpty then current
      else if trace.length < 2 then current.geomPoint(data = Some(trace))
      else current.geomLine(data = Some(trace))
    }

  private def axisOverrides(options: EventPlotGraphicsOptions): Vector[GuideSpec] =
    Vector(
      GuideSpec.Axis(AxisSide.Bottom, breaks = options.xBreaks, name = Some(GraphicsName.unsafe("time-axis"))),
      GuideSpec.Axis(AxisSide.Left, breaks = options.yBreaks, name = Some(GraphicsName.unsafe("response-axis")))
    )
