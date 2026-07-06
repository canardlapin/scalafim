package scalafim.surface.gifti

enum GiftiIntent(val code: String):
  case PointSet extends GiftiIntent("NIFTI_INTENT_POINTSET")
  case Triangle extends GiftiIntent("NIFTI_INTENT_TRIANGLE")
  case Label extends GiftiIntent("NIFTI_INTENT_LABEL")
  case NodeIndex extends GiftiIntent("NIFTI_INTENT_NODE_INDEX")
  case VectorData extends GiftiIntent("NIFTI_INTENT_VECTOR")
  case Shape extends GiftiIntent("NIFTI_INTENT_SHAPE")
  case TimeSeries extends GiftiIntent("NIFTI_INTENT_TIME_SERIES")
  case RgbVector extends GiftiIntent("NIFTI_INTENT_RGB_VECTOR")
  case RgbaVector extends GiftiIntent("NIFTI_INTENT_RGBA_VECTOR")
  case GenMatrix extends GiftiIntent("NIFTI_INTENT_GENMATRIX")
  case NoneIntent extends GiftiIntent("NIFTI_INTENT_NONE")
  case Other(value: String) extends GiftiIntent(value)

object GiftiIntent:
  def fromAttribute(value: String): GiftiIntent =
    val raw = value.trim
    raw.toUpperCase match
      case PointSet.code => PointSet
      case Triangle.code => Triangle
      case Label.code => Label
      case NodeIndex.code => NodeIndex
      case VectorData.code => VectorData
      case Shape.code => Shape
      case TimeSeries.code => TimeSeries
      case RgbVector.code => RgbVector
      case RgbaVector.code => RgbaVector
      case GenMatrix.code => GenMatrix
      case "" | NoneIntent.code => NoneIntent
      case _ => Other(raw)

enum GiftiEncoding(val code: String):
  case Ascii extends GiftiEncoding("ASCII")
  case Base64Binary extends GiftiEncoding("Base64Binary")
  case GZipBase64Binary extends GiftiEncoding("GZipBase64Binary")
  case ExternalFileBinary extends GiftiEncoding("ExternalFileBinary")
  case Other(value: String) extends GiftiEncoding(value)

object GiftiEncoding:
  def fromAttribute(value: Option[String]): GiftiEncoding =
    value.map(_.trim).filter(_.nonEmpty) match
      case None => Ascii
      case Some(raw) =>
        raw.toLowerCase match
          case "ascii" => Ascii
          case "base64binary" => Base64Binary
          case "gzipbase64binary" => GZipBase64Binary
          case "externalfilebinary" => ExternalFileBinary
          case _ => Other(raw)

enum GiftiDataType(val code: String):
  case UInt8 extends GiftiDataType("NIFTI_TYPE_UINT8")
  case Int32 extends GiftiDataType("NIFTI_TYPE_INT32")
  case Float32 extends GiftiDataType("NIFTI_TYPE_FLOAT32")
  case Other(value: String) extends GiftiDataType(value)

object GiftiDataType:
  def fromAttribute(value: String): GiftiDataType =
    val raw = value.trim
    raw.toUpperCase match
      case UInt8.code => UInt8
      case Int32.code => Int32
      case Float32.code => Float32
      case _ => Other(raw)

enum GiftiEndian(val code: String):
  case Little extends GiftiEndian("LittleEndian")
  case Big extends GiftiEndian("BigEndian")
  case Other(value: String) extends GiftiEndian(value)

object GiftiEndian:
  def fromAttribute(value: Option[String]): GiftiEndian =
    value.map(_.trim).filter(_.nonEmpty) match
      case None => Little
      case Some(raw) =>
        raw.toLowerCase match
          case "littleendian" => Little
          case "bigendian" => Big
          case _ => Other(raw)

enum GiftiArrayOrder(val code: String):
  case RowMajor extends GiftiArrayOrder("RowMajorOrder")
  case ColumnMajor extends GiftiArrayOrder("ColumnMajorOrder")
  case Other(value: String) extends GiftiArrayOrder(value)

object GiftiArrayOrder:
  def fromAttribute(value: Option[String]): GiftiArrayOrder =
    value.map(_.trim).filter(_.nonEmpty) match
      case None => RowMajor
      case Some(raw) =>
        raw.toLowerCase match
          case "rowmajororder" => RowMajor
          case "columnmajororder" => ColumnMajor
          case _ => Other(raw)

final case class GiftiTransform(
  dataSpace: Option[String],
  transformedSpace: Option[String],
  matrixData: Vector[Double]
):
  require(matrixData.length == 16, "GIFTI transform matrix must contain 16 values")
  require(matrixData.forall(_.isFinite), "GIFTI transform matrix values must be finite")

final case class GiftiLabel(
  key: Int,
  name: String,
  red: Option[Double] = None,
  green: Option[Double] = None,
  blue: Option[Double] = None,
  alpha: Option[Double] = None
):
  require(name.trim.nonEmpty, "GIFTI label name must be non-empty")
  Vector(red, green, blue, alpha).flatten.foreach(v => require(v.isFinite, "GIFTI label color values must be finite"))

  def colorHex: Option[String] =
    for
      r <- red
      g <- green
      b <- blue
    yield f"#${toByte(r)}%02x${toByte(g)}%02x${toByte(b)}%02x"

  private def toByte(value: Double): Int =
    math.max(0, math.min(255, math.round(value * 255.0).toInt))

final case class GiftiDataArray(
  intent: GiftiIntent,
  dataType: GiftiDataType,
  encoding: GiftiEncoding,
  endian: GiftiEndian,
  arrayOrder: GiftiArrayOrder,
  dims: Vector[Int],
  metadata: Map[String, String],
  transforms: Vector[GiftiTransform],
  dataText: String,
  externalFileName: Option[String] = None,
  externalFileOffset: Option[Long] = None
):
  require(dims.nonEmpty, "GIFTI data array dimensions must be non-empty")
  require(dims.forall(_ > 0), "GIFTI data array dimensions must be positive")
  require(metadata.keys.forall(_.trim.nonEmpty), "GIFTI metadata keys must be non-empty")
  externalFileName.foreach(name => require(name.trim.nonEmpty, "GIFTI external file name must be non-empty"))
  externalFileOffset.foreach(offset => require(offset >= 0L, "GIFTI external file offset must be non-negative"))

  def rank: Int =
    dims.length

  def elementCount: Int =
    dims.product

  def rows: Int =
    dims.head

  def columns: Int =
    dims.lift(1).getOrElse(1)

  def isTriple: Boolean =
    rank == 2 && columns == 3

  def isQuad: Boolean =
    rank == 2 && columns == 4

  def isPointSet: Boolean =
    intent == GiftiIntent.PointSet

  def isTriangle: Boolean =
    intent == GiftiIntent.Triangle

  def isLabel: Boolean =
    intent == GiftiIntent.Label

  def isNodeIndex: Boolean =
    intent == GiftiIntent.NodeIndex

final case class GiftiDocument(
  attributes: Map[String, String],
  metadata: Map[String, String],
  labelTable: Vector[GiftiLabel],
  dataArrays: Vector[GiftiDataArray]
):
  require(dataArrays.nonEmpty, "GIFTI document must contain at least one data array")
  require(attributes.keys.forall(_.trim.nonEmpty), "GIFTI document attributes must have non-empty names")
  require(metadata.keys.forall(_.trim.nonEmpty), "GIFTI metadata keys must be non-empty")
  require(labelTable.map(_.key).distinct.length == labelTable.length, "GIFTI label keys must be unique")

  def pointSet: Option[GiftiDataArray] =
    dataArrays.find(_.isPointSet)

  def triangles: Option[GiftiDataArray] =
    dataArrays.find(_.isTriangle)

  def labels: Option[GiftiDataArray] =
    dataArrays.find(_.isLabel)

  def nodeIndices: Option[GiftiDataArray] =
    dataArrays.find(_.isNodeIndex)

  def normals: Option[GiftiDataArray] =
    dataArrays.find(_.intent == GiftiIntent.VectorData)

  def colors: Option[GiftiDataArray] =
    dataArrays.find(array => array.intent == GiftiIntent.RgbVector || array.intent == GiftiIntent.RgbaVector)

enum GiftiError:
  case Xml(reason: String)
  case MissingDataArray(intent: GiftiIntent)
  case InvalidDocument(reason: String)
  case InvalidDataArray(reason: String)
  case UnsupportedEncoding(encoding: GiftiEncoding)
  case UnsupportedEndian(endian: GiftiEndian)
  case UnsupportedDataType(dataType: GiftiDataType)
  case DecodeFailure(reason: String)

  def message: String =
    this match
      case Xml(msg) => s"GIFTI XML parse failed: $msg"
      case MissingDataArray(intent) => s"GIFTI document is missing ${intent.code} DataArray"
      case InvalidDocument(msg) => s"invalid GIFTI document: $msg"
      case InvalidDataArray(msg) => s"invalid GIFTI DataArray: $msg"
      case UnsupportedEncoding(encoding) => s"unsupported GIFTI encoding: ${encoding.code}"
      case UnsupportedEndian(endian) => s"unsupported GIFTI endian: ${endian.code}"
      case UnsupportedDataType(dataType) => s"unsupported GIFTI data type: ${dataType.code}"
      case DecodeFailure(msg) => s"GIFTI data decode failed: $msg"
