package scalafim.graphics

final case class Position2[Row](x: Row => Double, y: Row => Double):
  def apply(row: Row): (Double, Double) =
    (x(row), y(row))

final case class AesSpec[Row](
    position: Option[Position2[Row]] = None,
    color: Option[Row => Rgba] = None,
    fill: Option[Row => Rgba] = None,
    alpha: Option[Row => Double] = None,
    size: Option[Row => Double] = None,
    label: Option[Row => String] = None,
    group: Option[Row => String] = None
):
  def withPosition(x: Row => Double, y: Row => Double): AesSpec[Row] =
    copy(position = Some(Position2(x, y)))

  def withColor(f: Row => Rgba): AesSpec[Row] =
    copy(color = Some(f))

  def withFill(f: Row => Rgba): AesSpec[Row] =
    copy(fill = Some(f))

  def withAlpha(f: Row => Double): AesSpec[Row] =
    copy(alpha = Some(f))

  def withSize(f: Row => Double): AesSpec[Row] =
    copy(size = Some(f))

  def withLabel(f: Row => String): AesSpec[Row] =
    copy(label = Some(f))

  def withGroup(f: Row => String): AesSpec[Row] =
    copy(group = Some(f))

  def inherit(parent: AesSpec[Row]): AesSpec[Row] =
    AesSpec(
      position = position.orElse(parent.position),
      color = color.orElse(parent.color),
      fill = fill.orElse(parent.fill),
      alpha = alpha.orElse(parent.alpha),
      size = size.orElse(parent.size),
      label = label.orElse(parent.label),
      group = group.orElse(parent.group)
    )

object AesSpec:
  def empty[Row]: AesSpec[Row] =
    AesSpec()

enum Geom(val label: String):
  case Point extends Geom("point")
  case Line extends Geom("line")
  case Text extends Geom("text")
  case Rect extends Geom("rect")

  def requiredAesthetics: Vector[String] =
    this match
      case Point => Vector("x", "y")
      case Line  => Vector("x", "y")
      case Text  => Vector("x", "y", "label")
      case Rect  => Vector("x", "y")

enum Stat:
  case Identity
  case Count

enum Coord:
  case Cartesian(clip: Clip = Clip.On)

final case class Layer[Row] private (
    geom: Geom,
    stat: Stat,
    data: Option[Vector[Row]],
    mapping: AesSpec[Row],
    inheritMapping: Boolean,
    params: GraphicParams
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
      params: GraphicParams = GraphicParams.unsafe()
  ): Layer[Row] =
    Layer(Geom.Point, Stat.Identity, data, mapping.withPosition(x, y), inheritMapping, params)

  def line[Row](
      x: Row => Double,
      y: Row => Double,
      data: Option[Vector[Row]] = None,
      mapping: AesSpec[Row] = AesSpec.empty[Row],
      inheritMapping: Boolean = true,
      params: GraphicParams = GraphicParams.unsafe()
  ): Layer[Row] =
    Layer(Geom.Line, Stat.Identity, data, mapping.withPosition(x, y), inheritMapping, params)

  def text[Row](
      x: Row => Double,
      y: Row => Double,
      label: Row => String,
      data: Option[Vector[Row]] = None,
      mapping: AesSpec[Row] = AesSpec.empty[Row],
      inheritMapping: Boolean = true,
      params: GraphicParams = GraphicParams.unsafe()
  ): Layer[Row] =
    Layer(Geom.Text, Stat.Identity, data, mapping.withPosition(x, y).withLabel(label), inheritMapping, params)

  def fromMapping[Row](
      geom: Geom,
      mapping: AesSpec[Row],
      data: Option[Vector[Row]] = None,
      inheritMapping: Boolean = true,
      stat: Stat = Stat.Identity,
      params: GraphicParams = GraphicParams.unsafe()
  ): Either[GraphicsError, Layer[Row]] =
    Right(Layer(geom, stat, data, mapping, inheritMapping, params))

  private[graphics] def validate[Row](geom: Geom, mapping: AesSpec[Row]): Either[GraphicsError, Unit] =
    val missing =
      geom.requiredAesthetics.find {
        case "x"     => mapping.position.isEmpty
        case "y"     => mapping.position.isEmpty
        case "label" => mapping.label.isEmpty
        case _       => false
      }
    missing match
      case Some(aesthetic) => Left(GraphicsError.MissingAesthetic(geom.label, aesthetic))
      case None            => Right(())

final case class Plot[Row] private (
    data: Vector[Row],
    mapping: AesSpec[Row],
    layers: Vector[Layer[Row]],
    scales: Vector[ScaleBinding[Row, ?, ?]],
    coord: Coord
):
  def addLayer(layer: Layer[Row]): Either[GraphicsError, Plot[Row]] =
    Layer.validate(layer.geom, layer.effectiveMapping(mapping)).map(_ => copy(layers = layers :+ layer))

  def withMapping(mapping: AesSpec[Row]): Plot[Row] =
    copy(mapping = mapping)

  def withScale[In, A](binding: ScaleBinding[Row, In, A]): Either[GraphicsError, Plot[Row]] =
    if scales.exists(_.aesthetic.label == binding.aesthetic.label) then
      Left(GraphicsError.DuplicateScale(binding.aesthetic.label))
    else Right(copy(scales = scales :+ binding))

  def withCoord(coord: Coord): Plot[Row] =
    copy(coord = coord)

  def layerData(layer: Layer[Row]): Vector[Row] =
    layer.effectiveData(data)

  def layerMapping(layer: Layer[Row]): AesSpec[Row] =
    layer.effectiveMapping(mapping)

object Plot:
  def apply[Row](data: Vector[Row]): Plot[Row] =
    Plot(data, AesSpec.empty, Vector.empty, Vector.empty, Coord.Cartesian())
