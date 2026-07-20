package scalafim.graphics

sealed trait AesValue[Row, A]:
  def map(row: Row): Option[A]
  def isScaled: Boolean =
    false

  def contramap[Input](f: Input => Row): AesValue[Input, A] =
    this match
      case AesValue.Direct(value)        => AesValue.direct(input => value(f(input)))
      case AesValue.Constant(value)      => AesValue.constant(value)
      case AesValue.Scaled(value, scale) => AesValue.scaled(input => value(f(input)), scale)

object AesValue:
  final case class Direct[Row, A](value: Row => A) extends AesValue[Row, A]:
    override def map(row: Row): Option[A] =
      Some(value(row))

  final case class Constant[Row, A](value: A) extends AesValue[Row, A]:
    override def map(row: Row): Option[A] =
      Some(value)

  final case class Scaled[Row, In, A](value: Row => In, scale: Scale[In, A]) extends AesValue[Row, A]:
    override def map(row: Row): Option[A] =
      scale.mapValue(value(row))

    override def isScaled: Boolean =
      true

  def direct[Row, A](value: Row => A): AesValue[Row, A] =
    Direct(value)

  def constant[Row, A](value: A): AesValue[Row, A] =
    Constant(value)

  def scaled[Row, In, A](value: Row => In, scale: Scale[In, A]): AesValue[Row, A] =
    Scaled(value, scale)

final case class Position2[Row](x: AesValue[Row, Double], y: AesValue[Row, Double]):
  def map(row: Row): Option[(Double, Double)] =
    for
      px <- x.map(row)
      py <- y.map(row)
    yield (px, py)

enum RequiredAesthetic(val aesthetic: Aesthetic[?]):
  case X extends RequiredAesthetic(Aesthetic.X)
  case Y extends RequiredAesthetic(Aesthetic.Y)
  case Label extends RequiredAesthetic(Aesthetic.Label)

  def label: String =
    aesthetic.label

  def isPresent[Row](mapping: AesSpec[Row]): Boolean =
    mapping.env.isBound(aesthetic)

final case class AesSpec[Row](
    x: Option[AesValue[Row, Double]] = None,
    y: Option[AesValue[Row, Double]] = None,
    color: Option[AesValue[Row, Rgba]] = None,
    fill: Option[AesValue[Row, Rgba]] = None,
    alpha: Option[AesValue[Row, Double]] = None,
    size: Option[AesValue[Row, Double]] = None,
    label: Option[AesValue[Row, String]] = None,
    group: Option[AesValue[Row, String]] = None
):
  def contramap[Input](f: Input => Row): AesSpec[Input] =
    AesSpec(
      x = x.map(_.contramap(f)),
      y = y.map(_.contramap(f)),
      color = color.map(_.contramap(f)),
      fill = fill.map(_.contramap(f)),
      alpha = alpha.map(_.contramap(f)),
      size = size.map(_.contramap(f)),
      label = label.map(_.contramap(f)),
      group = group.map(_.contramap(f))
    )

  def position: Option[Position2[Row]] =
    for
      px <- x
      py <- y
    yield Position2(px, py)

  def withPosition(x: Row => Double, y: Row => Double): AesSpec[Row] =
    copy(x = Some(AesValue.direct(x)), y = Some(AesValue.direct(y)))

  def withColor(f: Row => Rgba): AesSpec[Row] =
    copy(color = Some(AesValue.direct(f)))

  def withColor(value: Rgba): AesSpec[Row] =
    copy(color = Some(AesValue.constant(value)))

  def withFill(f: Row => Rgba): AesSpec[Row] =
    copy(fill = Some(AesValue.direct(f)))

  def withFill(value: Rgba): AesSpec[Row] =
    copy(fill = Some(AesValue.constant(value)))

  def withAlpha(f: Row => Double): AesSpec[Row] =
    copy(alpha = Some(AesValue.direct(f)))

  def withAlpha(value: Double): AesSpec[Row] =
    copy(alpha = Some(AesValue.constant(value)))

  def withSize(f: Row => Double): AesSpec[Row] =
    copy(size = Some(AesValue.direct(f)))

  def withSize(value: Double): AesSpec[Row] =
    copy(size = Some(AesValue.constant(value)))

  def withLabel(f: Row => String): AesSpec[Row] =
    copy(label = Some(AesValue.direct(f)))

  def withLabel(value: String): AesSpec[Row] =
    copy(label = Some(AesValue.constant(value)))

  def withGroup(f: Row => String): AesSpec[Row] =
    copy(group = Some(AesValue.direct(f)))

  def withGroup(value: String): AesSpec[Row] =
    copy(group = Some(AesValue.constant(value)))

  /** Normalize to the typed aesthetic environment. */
  def env: AesEnv[Row] =
    var out = AesEnv.empty[Row]
    x.foreach(value => out = out.updated(Aesthetic.X, value))
    y.foreach(value => out = out.updated(Aesthetic.Y, value))
    color.foreach(value => out = out.updated(Aesthetic.Color, value))
    fill.foreach(value => out = out.updated(Aesthetic.Fill, value))
    alpha.foreach(value => out = out.updated(Aesthetic.Alpha, value))
    size.foreach(value => out = out.updated(Aesthetic.Size, value))
    label.foreach(value => out = out.updated(Aesthetic.Label, value))
    group.foreach(value => out = out.updated(Aesthetic.Group, value))
    out

  def bindScale[In, A](binding: ScaleBinding[Row, In, A]): Either[GraphicsError, AesSpec[Row]] =
    env.bind(binding).map(AesSpec.fromEnv)

  def inherit(parent: AesSpec[Row]): AesSpec[Row] =
    AesSpec.fromEnv(env.inherit(parent.env))

object AesSpec:
  def empty[Row]: AesSpec[Row] =
    AesSpec()

  def fromEnv[Row](env: AesEnv[Row]): AesSpec[Row] =
    AesSpec(
      x = env.get(Aesthetic.X),
      y = env.get(Aesthetic.Y),
      color = env.get(Aesthetic.Color),
      fill = env.get(Aesthetic.Fill),
      alpha = env.get(Aesthetic.Alpha),
      size = env.get(Aesthetic.Size),
      label = env.get(Aesthetic.Label),
      group = env.get(Aesthetic.Group)
    )

enum Geom(val label: String):
  case Point extends Geom("point")
  case Line extends Geom("line")
  case Text extends Geom("text")
  case Rect extends Geom("rect")
  case Bar extends Geom("bar")

  def requiredAesthetics: Vector[RequiredAesthetic] =
    this match
      case Point => Vector(RequiredAesthetic.X, RequiredAesthetic.Y)
      case Line  => Vector(RequiredAesthetic.X, RequiredAesthetic.Y)
      case Text  => Vector(RequiredAesthetic.X, RequiredAesthetic.Y, RequiredAesthetic.Label)
      case Rect | Bar => Vector(RequiredAesthetic.X, RequiredAesthetic.Y)

enum Coord:
  case Cartesian(clip: Clip = Clip.On)

final case class Layer[Row] private (
    geom: Geom,
    stat: Stat[Row],
    data: Option[Vector[Row]],
    mapping: AesSpec[Row],
    inheritMapping: Boolean,
    params: Option[GraphicParams]
):
  def effectiveMapping(plotMapping: AesSpec[Row]): AesSpec[Row] =
    if inheritMapping then mapping.inherit(plotMapping) else mapping

  def effectiveData(plotData: Vector[Row]): Vector[Row] =
    data.getOrElse(plotData)

object Layer:
  def point[Row](
      x: Row => Double,
      y: Row => Double,
      data: Option[Vector[Row]] = None,
      mapping: AesSpec[Row] = AesSpec.empty[Row],
      inheritMapping: Boolean = true,
      params: Option[GraphicParams] = None
  ): Layer[Row] =
    Layer(Geom.Point, Stat.Identity, data, mapping.withPosition(x, y), inheritMapping, params)

  def line[Row](
      x: Row => Double,
      y: Row => Double,
      data: Option[Vector[Row]] = None,
      mapping: AesSpec[Row] = AesSpec.empty[Row],
      inheritMapping: Boolean = true,
      params: Option[GraphicParams] = None
  ): Layer[Row] =
    Layer(Geom.Line, Stat.Identity, data, mapping.withPosition(x, y), inheritMapping, params)

  def text[Row](
      x: Row => Double,
      y: Row => Double,
      label: Row => String,
      data: Option[Vector[Row]] = None,
      mapping: AesSpec[Row] = AesSpec.empty[Row],
      inheritMapping: Boolean = true,
      params: Option[GraphicParams] = None
  ): Layer[Row] =
    Layer(Geom.Text, Stat.Identity, data, mapping.withPosition(x, y).withLabel(label), inheritMapping, params)

  /** Count observations by a discrete key and lower the computed result as
    * bars. Position aesthetics belong to the statistic, so this constructor
    * deliberately does not accept raw `x` or `y` mappings.
    */
  def count[Row](
      x: Row => String,
      data: Option[Vector[Row]] = None,
      order: CountOrder = CountOrder.Encountered,
      scaleName: GraphicsName = GraphicsName.unsafe("x"),
      params: Option[GraphicParams] = None
  ): Layer[Row] =
    Layer(
      Geom.Bar,
      Stat.Count(x, order, scaleName),
      data,
      AesSpec.empty[Row],
      inheritMapping = false,
      params
    )

  def histogram[Row](
      x: Row => Double,
      data: Option[Vector[Row]] = None,
      bins: HistogramBins = HistogramBins.default,
      params: Option[GraphicParams] = None
  ): Layer[Row] =
    Layer(Geom.Bar, Stat.Bin(x, bins), data, AesSpec.empty[Row], inheritMapping = false, params)

  def summary[Row](
      x: Row => Double,
      y: Row => Double,
      data: Option[Vector[Row]] = None,
      interval: SummaryInterval = SummaryInterval.StandardError,
      params: Option[GraphicParams] = None
  ): Layer[Row] =
    Layer(Geom.Point, Stat.Summary(x, y, interval), data, AesSpec.empty[Row], inheritMapping = false, params)

  def density[Row](
      x: Row => Double,
      data: Option[Vector[Row]] = None,
      config: DensityConfig = DensityConfig.default,
      params: Option[GraphicParams] = None
  ): Layer[Row] =
    Layer(Geom.Line, Stat.Density(x, config), data, AesSpec.empty[Row], inheritMapping = false, params)

  def fromMapping[Row](
      geom: Geom,
      mapping: AesSpec[Row],
      data: Option[Vector[Row]] = None,
      inheritMapping: Boolean = true,
      stat: Stat[Row] = Stat.Identity,
      params: Option[GraphicParams] = None
  ): Either[GraphicsError, Layer[Row]] =
    val layer = Layer(geom, stat, data, mapping, inheritMapping, params)
    if inheritMapping then Right(layer)
    else validate(layer, mapping).map(_ => layer)

  private[graphics] def validate[Row](layer: Layer[Row], mapping: AesSpec[Row]): Either[GraphicsError, Unit] =
    layer.stat match
      case Stat.Identity =>
        validate(layer.geom, mapping)
      case _: Stat.Count[?] =>
        validateComputedStat(layer, mapping, Geom.Bar)
      case _: Stat.Bin[?] =>
        validateComputedStat(layer, mapping, Geom.Bar)
      case _: Stat.Summary[?] =>
        validateComputedStat(layer, mapping, Geom.Point)
      case _: Stat.Density[?] =>
        validateComputedStat(layer, mapping, Geom.Line)

  private def validateComputedStat[Row](
      layer: Layer[Row],
      mapping: AesSpec[Row],
      expectedGeom: Geom
  ): Either[GraphicsError, Unit] =
    if layer.geom != expectedGeom then Left(GraphicsError.InvalidStatGeom(layer.stat.label, layer.geom.label))
    else if mapping.x.nonEmpty then Left(GraphicsError.StatAestheticConflict(layer.stat.label, Aesthetic.X.label))
    else if mapping.y.nonEmpty then Left(GraphicsError.StatAestheticConflict(layer.stat.label, Aesthetic.Y.label))
    else
      mapping.env.bound.headOption match
        case Some(aesthetic) => Left(GraphicsError.UnsupportedStatAesthetic(layer.stat.label, aesthetic.label))
        case None            => Right(())

  private[graphics] def validate[Row](geom: Geom, mapping: AesSpec[Row]): Either[GraphicsError, Unit] =
    geom.requiredAesthetics.find(required => !required.isPresent(mapping)) match
      case Some(aesthetic) => Left(GraphicsError.MissingAesthetic(geom.label, aesthetic.label))
      case None            => Right(())

final case class PlotLabels(
    title: Option[String] = None,
    subtitle: Option[String] = None,
    x: Option[String] = None,
    y: Option[String] = None
):
  def isEmpty: Boolean =
    title.isEmpty && subtitle.isEmpty && x.isEmpty && y.isEmpty

final case class Plot[Row] private (
    data: Vector[Row],
    mapping: AesSpec[Row],
    layers: Vector[Layer[Row]],
    coord: Coord,
    labels: PlotLabels
):
  def addLayer(layer: Layer[Row]): Either[GraphicsError, Plot[Row]] =
    Layer.validate(layer, layer.effectiveMapping(mapping)).map(_ => copy(layers = layers :+ layer))

  def withMapping(mapping: AesSpec[Row]): Either[GraphicsError, Plot[Row]] =
    validateLayers(mapping).map(_ => copy(mapping = mapping))

  def withScale[In, A](binding: ScaleBinding[Row, In, A]): Either[GraphicsError, Plot[Row]] =
    mapping.bindScale(binding).flatMap(withMapping)

  def withCoord(coord: Coord): Plot[Row] =
    copy(coord = coord)

  def withLabels(labels: PlotLabels): Plot[Row] =
    copy(labels = labels)

  def withTitle(title: String): Plot[Row] =
    copy(labels = labels.copy(title = Some(title)))

  def withSubtitle(subtitle: String): Plot[Row] =
    copy(labels = labels.copy(subtitle = Some(subtitle)))

  def withAxisTitles(x: String, y: String): Plot[Row] =
    copy(labels = labels.copy(x = Some(x), y = Some(y)))

  def layerData(layer: Layer[Row]): Vector[Row] =
    layer.effectiveData(data)

  def layerMapping(layer: Layer[Row]): AesSpec[Row] =
    layer.effectiveMapping(mapping)

  private def validateLayers(plotMapping: AesSpec[Row]): Either[GraphicsError, Unit] =
    var idx = 0
    var result: Either[GraphicsError, Unit] = Right(())
    while idx < layers.length && result.isRight do
      val layer = layers(idx)
      result = Layer.validate(layer, layer.effectiveMapping(plotMapping))
      idx += 1
    result

object Plot:
  def apply[Row](data: Vector[Row]): Plot[Row] =
    Plot(data, AesSpec.empty, Vector.empty, Coord.Cartesian(), PlotLabels())
