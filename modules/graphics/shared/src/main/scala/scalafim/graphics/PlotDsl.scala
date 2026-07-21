package scalafim.graphics

import scala.annotation.implicitNotFound

/** Position mapping states carried by [[PlotBuilder]]. They make geom
  * prerequisites visible to the Scala compiler without exposing compiler
  * phases or requiring a macro-based syntax layer.
  */
sealed trait PlotPosition[Row]

object PlotPosition:
  final case class Empty[Row]() extends PlotPosition[Row]

  sealed trait WithX[Row] extends PlotPosition[Row]:
    def x: Row => Double

  final case class X[Row](x: Row => Double) extends WithX[Row]

  final case class XY[Row](x: Row => Double, y: Row => Double) extends WithX[Row]

@implicitNotFound("This plotting operation requires x. Call .aes(x) or .aes(x, y) first.")
sealed trait HasX[Row, Position <: PlotPosition[Row]]:
  def apply(position: Position): PlotPosition.WithX[Row]

object HasX:
  given x[Row]: HasX[Row, PlotPosition.X[Row]] with
    def apply(position: PlotPosition.X[Row]): PlotPosition.WithX[Row] = position

  given xy[Row]: HasX[Row, PlotPosition.XY[Row]] with
    def apply(position: PlotPosition.XY[Row]): PlotPosition.WithX[Row] = position

@implicitNotFound("This plotting operation requires x and y. Call .aes(x, y) first.")
sealed trait HasXY[Row, Position <: PlotPosition[Row]]:
  def apply(position: Position): PlotPosition.XY[Row]

object HasXY:
  given xy[Row]: HasXY[Row, PlotPosition.XY[Row]] with
    def apply(position: PlotPosition.XY[Row]): PlotPosition.XY[Row] = position

/** An executable, inspectable plotting value.
  *
  * The public DSL stops here: callers can inspect or further compose the
  * renderer-neutral [[Plot]], resolve it to a [[TrainedPlot]], or compile it
  * to a [[Scene]]. Concrete renderers remain separate modules.
  */
final case class PlotProgram[Row] private[graphics] (
    plot: Plot[Row],
    compilerOptions: PlotCompilerOptions
):
  def resolve: Either[GraphicsError, TrainedPlot[Row]] =
    PlotCompiler.resolve(plot, compilerOptions)

  def scene: Either[GraphicsError, Scene] =
    PlotCompiler.compile(plot, compilerOptions)

/** Immutable user-facing plotting builder.
  *
  * Every operation returns another builder. Errors from checked scales,
  * coordinates, and layer validation accumulate in `build` as
  * `GraphicsError`; `resolve` and `scene` retain typed compiler errors. No
  * exception or backend value crosses the API boundary.
  */
final class PlotBuilder[Row, Position <: PlotPosition[Row]] private[graphics] (
    private val data: Vector[Row],
    private val position: Position,
    private val result: Either[GraphicsError, Plot[Row]],
    private val options: PlotCompilerOptions
):

  def aes(x: Row => Double): PlotBuilder[Row, PlotPosition.X[Row]] =
    remap(
      PlotPosition.X(x),
      mapping => mapping.copy(x = Some(AesValue.direct(x)), y = None)
    )

  def aes(x: Row => Double, y: Row => Double): PlotBuilder[Row, PlotPosition.XY[Row]] =
    remap(PlotPosition.XY(x, y), _.withPosition(x, y))

  def aes(
      x: Row => Double,
      y: Row => Double,
      color: Row => Rgba
  ): PlotBuilder[Row, PlotPosition.XY[Row]] =
    remap(PlotPosition.XY(x, y), _.withPosition(x, y).withColor(color))

  def color(value: Row => Rgba): PlotBuilder[Row, Position] =
    mapAesthetics(_.withColor(value))

  def color(value: Rgba): PlotBuilder[Row, Position] =
    mapAesthetics(_.withColor(value))

  def fill(value: Row => Rgba): PlotBuilder[Row, Position] =
    mapAesthetics(_.withFill(value))

  def fill(value: Rgba): PlotBuilder[Row, Position] =
    mapAesthetics(_.withFill(value))

  def alpha(value: Row => Double): PlotBuilder[Row, Position] =
    mapAesthetics(_.withAlpha(value))

  def alpha(value: Double): PlotBuilder[Row, Position] =
    mapAesthetics(_.withAlpha(value))

  def size(value: Row => Double): PlotBuilder[Row, Position] =
    mapAesthetics(_.withSize(value))

  def size(value: Double): PlotBuilder[Row, Position] =
    mapAesthetics(_.withSize(value))

  def group(value: Row => String): PlotBuilder[Row, Position] =
    mapAesthetics(_.withGroup(value))

  def scaleXContinuous(
      name: String = "x",
      transform: Transform = Transform.identity,
      oob: OobPolicy = OobPolicy.Censor
  )(using ev: HasX[Row, Position]): PlotBuilder[Row, Position] =
    val x = ev(position).x
    bindContinuous(Aesthetic.X, x, name, Palette.numeric, transform, oob)

  def scaleYContinuous(
      name: String = "y",
      transform: Transform = Transform.identity,
      oob: OobPolicy = OobPolicy.Censor
  )(using ev: HasXY[Row, Position]): PlotBuilder[Row, Position] =
    val y = ev(position).y
    bindContinuous(Aesthetic.Y, y, name, Palette.numeric, transform, oob)

  def scaleColorDiscrete(
      value: Row => String,
      levels: Vector[String] = Vector.empty,
      colors: Vector[Rgba] = options.theme.palettes.discrete,
      name: String = "color"
  ): PlotBuilder[Row, Position] =
    bindDiscrete(Aesthetic.Color, value, levels, colors, name)

  def scaleFillDiscrete(
      value: Row => String,
      levels: Vector[String] = Vector.empty,
      colors: Vector[Rgba] = options.theme.palettes.discrete,
      name: String = "fill"
  ): PlotBuilder[Row, Position] =
    bindDiscrete(Aesthetic.Fill, value, levels, colors, name)

  def geomPoint(
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using HasXY[Row, Position]): PlotBuilder[Row, Position] =
    addInheritedGeom(Geom.Point, data, params)

  def geomLine(
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using HasXY[Row, Position]): PlotBuilder[Row, Position] =
    addInheritedGeom(Geom.Line, data, params)

  def geomText(
      label: Row => String,
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using HasXY[Row, Position]): PlotBuilder[Row, Position] =
    addLayer(
      Layer.fromMapping(
        Geom.Text,
        AesSpec.empty[Row].withLabel(label),
        data = data,
        inheritMapping = true,
        params = params
      )
    )

  def geomHistogram(
      bins: HistogramBins = HistogramBins.default,
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using ev: HasX[Row, Position]): PlotBuilder[Row, Position] =
    addLayer(Right(Layer.histogram(ev(position).x, data, bins, params)))

  def geomDensity(
      config: DensityConfig = DensityConfig.default,
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using ev: HasX[Row, Position]): PlotBuilder[Row, Position] =
    addLayer(Right(Layer.density(ev(position).x, data, config, params)))

  def geomSummary(
      interval: SummaryInterval = SummaryInterval.StandardError,
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using ev: HasXY[Row, Position]): PlotBuilder[Row, Position] =
    val xy = ev(position)
    addLayer(Right(Layer.summary(xy.x, xy.y, data, interval, params)))

  def geomArea(
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using ev: HasXY[Row, Position]): PlotBuilder[Row, Position] =
    val xy = ev(position)
    addLayer(Right(Layer.area(xy.x, xy.y, data, resultMapping, params)))

  def geomRibbon(
      yMin: Row => Double,
      yMax: Row => Double,
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using ev: HasX[Row, Position]): PlotBuilder[Row, Position] =
    addLayer(Right(Layer.ribbon(ev(position).x, yMin, yMax, data, resultMapping, params)))

  def geomErrorBar(
      yMin: Row => Double,
      yMax: Row => Double,
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using ev: HasX[Row, Position]): PlotBuilder[Row, Position] =
    addLayer(Right(Layer.errorBar(ev(position).x, yMin, yMax, data, resultMapping, params)))

  def geomSegment(
      xEnd: Row => Double,
      yEnd: Row => Double,
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using ev: HasXY[Row, Position]): PlotBuilder[Row, Position] =
    val xy = ev(position)
    addLayer(Right(Layer.segment(xy.x, xy.y, xEnd, yEnd, data, resultMapping, params)))

  def geomTile(
      width: Row => Double,
      height: Row => Double,
      data: Option[Vector[Row]] = None,
      params: Option[GraphicParams] = None
  )(using ev: HasXY[Row, Position]): PlotBuilder[Row, Position] =
    val xy = ev(position)
    addLayer(Right(Layer.tile(xy.x, xy.y, width, height, data, resultMapping, params)))

  def hline(y: Double, params: Option[GraphicParams] = None): PlotBuilder[Row, Position] =
    addLayer(Right(Layer.hline(y, data = Some(data), params = params)))

  def vline(x: Double, params: Option[GraphicParams] = None): PlotBuilder[Row, Position] =
    addLayer(Right(Layer.vline(x, data = Some(data), params = params)))

  def coord(coord: Coord): PlotBuilder[Row, Position] =
    updatePlot(_.withCoord(coord))

  def coordFixed(ratio: Double = 1.0, clip: Clip = Clip.On): PlotBuilder[Row, Position] =
    updateResult(Coord.fixed(ratio, clip).flatMap(coord => result.map(_.withCoord(coord))))

  def labels(value: PlotLabels): PlotBuilder[Row, Position] =
    updatePlot(_.withLabels(value))

  def title(value: String): PlotBuilder[Row, Position] =
    updatePlot(_.withTitle(value))

  def subtitle(value: String): PlotBuilder[Row, Position] =
    updatePlot(_.withSubtitle(value))

  def axisTitles(x: String, y: String): PlotBuilder[Row, Position] =
    updatePlot(_.withAxisTitles(x, y))

  def theme(value: Theme): PlotBuilder[Row, Position] =
    updateOptions(options.copy(theme = value))

  def guides(value: GuidePolicy): PlotBuilder[Row, Position] =
    updateOptions(options.copy(guides = value))

  def compilerOptions(value: PlotCompilerOptions): PlotBuilder[Row, Position] =
    updateOptions(value)

  def build: Either[GraphicsError, PlotProgram[Row]] =
    result.map(PlotProgram(_, options))

  def resolve: Either[GraphicsError, TrainedPlot[Row]] =
    build.flatMap(_.resolve)

  def scene: Either[GraphicsError, Scene] =
    build.flatMap(_.scene)

  private def resultMapping: AesSpec[Row] =
    result.toOption.map(_.mapping).getOrElse(AesSpec.empty)

  private def addInheritedGeom(
      geom: Geom,
      data: Option[Vector[Row]],
      params: Option[GraphicParams]
  ): PlotBuilder[Row, Position] =
    addLayer(Layer.fromMapping(geom, AesSpec.empty, data, inheritMapping = true, params = params))

  private def addLayer(layer: Either[GraphicsError, Layer[Row]]): PlotBuilder[Row, Position] =
    updateResult(for current <- result; next <- layer; plot <- current.addLayer(next) yield plot)

  private def bindContinuous[Out](
      aesthetic: Aesthetic[Out],
      value: Row => Double,
      name: String,
      palette: Palette[Out],
      transform: Transform,
      oob: OobPolicy
  ): PlotBuilder[Row, Position] =
    val next =
      for
        current <- result
        scale <- ContinuousScale.train(name, data.map(value), palette, transform, oob)
        plot <- current.withScale(ScaleBinding(aesthetic, value, scale))
      yield plot
    updateResult(next)

  private def bindDiscrete(
      aesthetic: Aesthetic[Rgba],
      value: Row => String,
      levels: Vector[String],
      colors: Vector[Rgba],
      name: String
  ): PlotBuilder[Row, Position] =
    val declared = if levels.nonEmpty then levels else data.map(value).distinct
    val next =
      for
        current <- result
        domain <- DiscreteDomain.ordered(declared)
        palette <- DiscretePalette.values(colors)
        scale <- DiscreteScale(name, domain, palette)
        plot <- current.withScale(ScaleBinding(aesthetic, value, scale))
      yield plot
    updateResult(next)

  private def mapAesthetics(f: AesSpec[Row] => AesSpec[Row]): PlotBuilder[Row, Position] =
    updateResult(result.flatMap(current => current.withMapping(f(current.mapping))))

  private def remap[Next <: PlotPosition[Row]](
      nextPosition: Next,
      f: AesSpec[Row] => AesSpec[Row]
  ): PlotBuilder[Row, Next] =
    new PlotBuilder(data, nextPosition, result.flatMap(current => current.withMapping(f(current.mapping))), options)

  private def updatePlot(f: Plot[Row] => Plot[Row]): PlotBuilder[Row, Position] =
    updateResult(result.map(f))

  private def updateResult(next: Either[GraphicsError, Plot[Row]]): PlotBuilder[Row, Position] =
    new PlotBuilder(data, position, next, options)

  private def updateOptions(next: PlotCompilerOptions): PlotBuilder[Row, Position] =
    new PlotBuilder(data, position, result, next)

/** Start a renderer-neutral plot program. The default DSL policy derives axes
  * and legends and uses the active theme's layout policy.
  */
def plot[Row](data: IterableOnce[Row]): PlotBuilder[Row, PlotPosition.Empty[Row]] =
  val rows = data.iterator.toVector
  val theme = Theme.default
  new PlotBuilder(
    rows,
    PlotPosition.Empty(),
    Right(Plot(rows)),
    PlotCompilerOptions(
      policy = Some(theme.layoutPolicy),
      guides = GuidePolicy.Derived(),
      theme = theme
    )
  )
