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
    if data.points.isEmpty then Left(GraphicsError.EmptyGeometry("event plot"))
    else if options.colors.isEmpty then Left(GraphicsError.EmptyPalette)
    else
      for
        domain <- regressorDomain(data)
        colorScale <- DiscreteScale(
          "regressor",
          domain,
          DiscretePalette.valuesUnsafe(options.colors)
        )
        plot <- Plot(data.points)
          .withCoord(Coord.Cartesian(options.clip))
          .withScale(ScaleBinding[TracePoint, String, Rgba](Aesthetic.Color, _.regressor, colorScale))
        withLayers <- addTraceLayers(plot, data.points, domain.levels)
      yield withLayers

  def eventScene(
      data: EventPlotData,
      options: EventPlotGraphicsOptions = EventPlotGraphicsOptions()
  ): Either[GraphicsError, Scene] =
    for
      plot <- eventPlot(data, options)
      scene <- PlotCompiler.compile(
        plot,
        PlotCompilerOptions(
          policy = Some(options.layoutPolicy),
          guides = GuidePolicy.Derived(
            overrides = axisOverrides(options),
            deriveLegends = options.showLegend
          )
        )
      )
    yield scene

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

  private def addTraceLayers(
      plot: Plot[TracePoint],
      points: Vector[TracePoint],
      regressors: Vector[String]
  ): Either[GraphicsError, Plot[TracePoint]] =
    var out = plot
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < regressors.length && result.isRight do
      val regressor = regressors(idx)
      val trace = points.filter(_.regressor == regressor)
      if trace.nonEmpty then
        val layer =
          if trace.length < 2 then Layer.point[TracePoint](_.time, _.response, data = Some(trace))
          else Layer.line[TracePoint](_.time, _.response, data = Some(trace))
        result = out.addLayer(layer).map { next =>
          out = next
          ()
        }
      idx += 1
    result.map(_ => out)

  private def axisOverrides(options: EventPlotGraphicsOptions): Vector[GuideSpec] =
    Vector(
      GuideSpec.Axis(AxisSide.Bottom, breaks = options.xBreaks, name = Some(GraphicsName.unsafe("time-axis"))),
      GuideSpec.Axis(AxisSide.Left, breaks = options.yBreaks, name = Some(GraphicsName.unsafe("response-axis")))
    )
