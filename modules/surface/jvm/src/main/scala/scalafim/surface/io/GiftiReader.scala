package scalafim.surface.io

import scalafim.surface.gifti.*

import org.w3c.dom.Element
import org.w3c.dom.NamedNodeMap
import org.w3c.dom.Node

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Base64
import java.util.zip.GZIPInputStream
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory
import scala.util.control.NonFatal

object GiftiReader:

  def read(path: Path): Either[GiftiError, GiftiDocument] =
    val input = open(path)
    try parse(input)
    finally input.close()

  def parseString(xml: String): Either[GiftiError, GiftiDocument] =
    val bytes = xml.getBytes(StandardCharsets.UTF_8)
    parse(new ByteArrayInputStream(bytes))

  def doublePayload(array: GiftiDataArray): Either[GiftiError, GiftiPayload[Double]] =
    doubleValues(array).flatMap(values => GiftiPayload.from(array, values.toVector))

  def doubleMatrix(array: GiftiDataArray): Either[GiftiError, GiftiMatrix[Double]] =
    doublePayload(array).flatMap(GiftiPayload.requireMatrix)

  def intPayload(array: GiftiDataArray): Either[GiftiError, GiftiPayload[Int]] =
    intValues(array).flatMap(values => GiftiPayload.from(array, values.toVector))

  def intVector(array: GiftiDataArray): Either[GiftiError, GiftiVector[Int]] =
    intPayload(array).flatMap(GiftiPayload.requireVector)

  def intMatrix(array: GiftiDataArray): Either[GiftiError, GiftiMatrix[Int]] =
    intPayload(array).flatMap(GiftiPayload.requireMatrix)

  def bytePayload(array: GiftiDataArray): Either[GiftiError, GiftiPayload[Int]] =
    byteValues(array).flatMap(values => GiftiPayload.from(array, values.toVector))

  def doubleData(array: GiftiDataArray): Either[GiftiError, Array[Double]] =
    doublePayload(array).map(_.values.toArray)

  def intData(array: GiftiDataArray): Either[GiftiError, Array[Int]] =
    intPayload(array).map(_.values.toArray)

  def byteData(array: GiftiDataArray): Either[GiftiError, Array[Int]] =
    bytePayload(array).map(_.values.toArray)

  private def doubleValues(array: GiftiDataArray): Either[GiftiError, Array[Double]] =
    array.encoding match
      case GiftiEncoding.Ascii =>
        parseAscii(array).map(_.map(_.toDouble))
      case _ =>
        for
          bytes <- rawBinary(array)
          values <- decodeDoubles(array, bytes)
          checked <- validateCount(array, values)
        yield checked

  private def intValues(array: GiftiDataArray): Either[GiftiError, Array[Int]] =
    array.encoding match
      case GiftiEncoding.Ascii =>
        parseAscii(array).flatMap { values =>
          val out = Array.ofDim[Int](values.length)
          var error: GiftiError | Null = null
          var i = 0
          while i < values.length && error == null do
            val value = values(i)
            if !value.isWhole then
              error = GiftiError.DecodeFailure(s"${array.intent.code} contains non-integer ASCII value $value")
            else out(i) = value.toInt
            i += 1
          error match
            case null => validateCount(array, out)
            case e => Left(e)
        }
      case _ =>
        for
          bytes <- rawBinary(array)
          values <- decodeInts(array, bytes)
          checked <- validateCount(array, values)
        yield checked

  private def byteValues(array: GiftiDataArray): Either[GiftiError, Array[Int]] =
    array.dataType match
      case GiftiDataType.UInt8 =>
        array.encoding match
          case GiftiEncoding.Ascii =>
            intValues(array)
          case _ =>
            rawBinary(array).map(bytes => bytes.map(_ & 0xff)).flatMap(validateCount(array, _))
      case other =>
        Left(GiftiError.UnsupportedDataType(other))

  private def parse(input: InputStream): Either[GiftiError, GiftiDocument] =
    try
      val factory = DocumentBuilderFactory.newInstance()
      factory.setNamespaceAware(false)
      factory.setExpandEntityReferences(false)
      factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
      val builder = factory.newDocumentBuilder()
      val doc = builder.parse(input)
      val root = doc.getDocumentElement
      if root == null || root.getTagName != "GIFTI" then
        Left(GiftiError.InvalidDocument("root element must be GIFTI"))
      else parseRoot(root)
    catch
      case NonFatal(error) =>
        Left(GiftiError.Xml(error.getMessage))

  private def open(path: Path): InputStream =
    val input = Files.newInputStream(path)
    if path.getFileName.toString.endsWith(".gz") then new GZIPInputStream(input) else input

  private def parseRoot(root: Element): Either[GiftiError, GiftiDocument] =
    for
      arrays <- sequence(children(root, "DataArray").map(parseDataArray))
      labels <- sequence(children(root, "LabelTable").flatMap(table => children(table, "Label")).map(parseLabel))
      document <- buildDocument(root, labels, arrays)
    yield document

  private def parseDataArray(element: Element): Either[GiftiError, GiftiDataArray] =
    val attrs = attributes(element)
    for
      intentText <- required(attrs, "Intent", "DataArray Intent")
      dataTypeText <- required(attrs, "DataType", "DataArray DataType")
      dimensionality <- intAttr(attrs, "Dimensionality", "DataArray Dimensionality")
      dims <- dimensions(attrs, dimensionality)
      transforms <- sequence(children(element, "CoordinateSystemTransformMatrix").map(parseTransform))
      externalOffset <- optionalLongAttr(attrs, "ExternalFileOffset", "DataArray ExternalFileOffset")
      text <- dataText(element)
      array <- buildDataArray(
        intent = GiftiIntent.fromAttribute(intentText),
        dataType = GiftiDataType.fromAttribute(dataTypeText),
        encoding = GiftiEncoding.fromAttribute(attrs.get("Encoding")),
        endian = GiftiEndian.fromAttribute(attrs.get("Endian")),
        arrayOrder = GiftiArrayOrder.fromAttribute(attrs.get("ArrayIndexingOrder")),
        dims = dims,
        metadata = metadata(element),
        transforms = transforms,
        dataText = text,
        externalFileName = attrs.get("ExternalFileName").filter(_.trim.nonEmpty),
        externalFileOffset = externalOffset
      )
    yield array

  private def parseTransform(element: Element): Either[GiftiError, GiftiTransform] =
    for
      matrixText <- childText(element, "MatrixData").toRight(GiftiError.InvalidDataArray("CoordinateSystemTransformMatrix requires MatrixData"))
      values <- parseDoubles(matrixText, "CoordinateSystemTransformMatrix")
      transform <-
        if values.length == 16 then
          Right(
            GiftiTransform(
              dataSpace = childText(element, "DataSpace"),
              transformedSpace = childText(element, "TransformedSpace"),
              matrixData = values.toVector
            )
          )
        else Left(GiftiError.InvalidDataArray("CoordinateSystemTransformMatrix must contain 16 values"))
    yield transform

  private def parseLabel(element: Element): Either[GiftiError, GiftiLabel] =
    val attrs = attributes(element)
    val keyText = attrs.get("Key").orElse(attrs.get("Index"))
    for
      keyRaw <- keyText.toRight(GiftiError.InvalidDocument("Label requires Key or Index"))
      key <- parseDocumentInt(keyRaw, "Label Key")
      red <- optionalDoubleAttr(attrs, "Red", "Label Red")
      green <- optionalDoubleAttr(attrs, "Green", "Label Green")
      blue <- optionalDoubleAttr(attrs, "Blue", "Label Blue")
      alpha <- optionalDoubleAttr(attrs, "Alpha", "Label Alpha")
      label <- buildLabel(
        key = key,
        name = element.getTextContent.trim,
        red = red,
        green = green,
        blue = blue,
        alpha = alpha
      )
    yield label

  private def buildDocument(root: Element, labels: Vector[GiftiLabel], arrays: Vector[GiftiDataArray]): Either[GiftiError, GiftiDocument] =
    try Right(GiftiDocument(attributes(root), metadata(root), labels, arrays))
    catch case error: IllegalArgumentException => Left(GiftiError.InvalidDocument(cleanRequirement(error.getMessage)))

  private def buildDataArray(
    intent: GiftiIntent,
    dataType: GiftiDataType,
    encoding: GiftiEncoding,
    endian: GiftiEndian,
    arrayOrder: GiftiArrayOrder,
    dims: Vector[Int],
    metadata: Map[String, String],
    transforms: Vector[GiftiTransform],
    dataText: String,
    externalFileName: Option[String],
    externalFileOffset: Option[Long]
  ): Either[GiftiError, GiftiDataArray] =
    try
      Right(
        GiftiDataArray(
          intent = intent,
          dataType = dataType,
          encoding = encoding,
          endian = endian,
          arrayOrder = arrayOrder,
          dims = dims,
          metadata = metadata,
          transforms = transforms,
          dataText = dataText,
          externalFileName = externalFileName,
          externalFileOffset = externalFileOffset
        )
      )
    catch case error: IllegalArgumentException => Left(GiftiError.InvalidDataArray(cleanRequirement(error.getMessage)))

  private def buildLabel(
    key: Int,
    name: String,
    red: Option[Double],
    green: Option[Double],
    blue: Option[Double],
    alpha: Option[Double]
  ): Either[GiftiError, GiftiLabel] =
    try Right(GiftiLabel(key, name, red, green, blue, alpha))
    catch case error: IllegalArgumentException => Left(GiftiError.InvalidDocument(cleanRequirement(error.getMessage)))

  private def metadata(parent: Element): Map[String, String] =
    children(parent, "MetaData")
      .flatMap(table => children(table, "MD"))
      .flatMap { md =>
        for
          name <- childText(md, "Name").map(_.trim).filter(_.nonEmpty)
          value <- childText(md, "Value")
        yield name -> value.trim
      }
      .toMap

  private def dataText(element: Element): Either[GiftiError, String] =
    childText(element, "Data").toRight(GiftiError.InvalidDataArray("DataArray requires Data"))

  private def dimensions(attrs: Map[String, String], dimensionality: Int): Either[GiftiError, Vector[Int]] =
    if dimensionality <= 0 then Left(GiftiError.InvalidDataArray("DataArray Dimensionality must be positive"))
    else
      sequence(
        Vector.tabulate(dimensionality) { dim =>
          intAttr(attrs, s"Dim$dim", s"DataArray Dim$dim")
        }
      ).flatMap { dims =>
        if dims.forall(_ > 0) then Right(dims)
        else Left(GiftiError.InvalidDataArray("DataArray dimensions must be positive"))
      }

  private def parseAscii(array: GiftiDataArray): Either[GiftiError, Array[Double]] =
    array.dataType match
      case GiftiDataType.Float32 | GiftiDataType.Int32 | GiftiDataType.UInt8 =>
        parseDoubles(array.dataText, array.intent.code).flatMap(validateCount(array, _))
      case other =>
        Left(GiftiError.UnsupportedDataType(other))

  private def parseDoubles(text: String, label: String): Either[GiftiError, Array[Double]] =
    try Right(splitFields(text).map(_.toDouble))
    catch
      case _: NumberFormatException =>
        Left(GiftiError.DecodeFailure(s"$label contains non-numeric ASCII data"))

  private def rawBinary(array: GiftiDataArray): Either[GiftiError, Array[Byte]] =
    array.encoding match
      case GiftiEncoding.Base64Binary =>
        decodeBase64(array.dataText)
      case GiftiEncoding.GZipBase64Binary =>
        decodeBase64(array.dataText).flatMap(gunzip)
      case GiftiEncoding.ExternalFileBinary =>
        Left(GiftiError.UnsupportedEncoding(array.encoding))
      case GiftiEncoding.Other(_) =>
        Left(GiftiError.UnsupportedEncoding(array.encoding))
      case GiftiEncoding.Ascii =>
        Left(GiftiError.UnsupportedEncoding(array.encoding))

  private def decodeDoubles(array: GiftiDataArray, bytes: Array[Byte]): Either[GiftiError, Array[Double]] =
    array.dataType match
      case GiftiDataType.Float32 =>
        withBuffer(array, bytes, 4) { buffer =>
          val out = Array.ofDim[Double](bytes.length / 4)
          var i = 0
          while i < out.length do
            out(i) = buffer.getFloat().toDouble
            i += 1
          out
        }
      case GiftiDataType.Int32 =>
        withBuffer(array, bytes, 4) { buffer =>
          val out = Array.ofDim[Double](bytes.length / 4)
          var i = 0
          while i < out.length do
            out(i) = buffer.getInt().toDouble
            i += 1
          out
        }
      case GiftiDataType.UInt8 =>
        Right(bytes.map(byte => (byte & 0xff).toDouble))
      case other =>
        Left(GiftiError.UnsupportedDataType(other))

  private def decodeInts(array: GiftiDataArray, bytes: Array[Byte]): Either[GiftiError, Array[Int]] =
    array.dataType match
      case GiftiDataType.Int32 =>
        withBuffer(array, bytes, 4) { buffer =>
          val out = Array.ofDim[Int](bytes.length / 4)
          var i = 0
          while i < out.length do
            out(i) = buffer.getInt()
            i += 1
          out
        }
      case GiftiDataType.UInt8 =>
        Right(bytes.map(_ & 0xff))
      case other =>
        Left(GiftiError.UnsupportedDataType(other))

  private def decodeBase64(text: String): Either[GiftiError, Array[Byte]] =
    try Right(Base64.getMimeDecoder.decode(text))
    catch
      case NonFatal(error) =>
        Left(GiftiError.DecodeFailure(s"invalid Base64 payload: ${error.getMessage}"))

  private def gunzip(bytes: Array[Byte]): Either[GiftiError, Array[Byte]] =
    try
      val input = new GZIPInputStream(new ByteArrayInputStream(bytes))
      val output = new ByteArrayOutputStream()
      val buffer = Array.ofDim[Byte](8192)
      var n = input.read(buffer)
      while n >= 0 do
        output.write(buffer, 0, n)
        n = input.read(buffer)
      input.close()
      Right(output.toByteArray)
    catch
      case NonFatal(error) =>
        Left(GiftiError.DecodeFailure(s"invalid GZip payload: ${error.getMessage}"))

  private def withBuffer[A](array: GiftiDataArray, bytes: Array[Byte], bytesPerValue: Int)(decode: ByteBuffer => A): Either[GiftiError, A] =
    if bytes.length % bytesPerValue != 0 then
      Left(GiftiError.DecodeFailure(s"${array.intent.code} byte length ${bytes.length} is not divisible by $bytesPerValue"))
    else byteOrder(array.endian).map(order => decode(ByteBuffer.wrap(bytes).order(order)))

  private def byteOrder(endian: GiftiEndian): Either[GiftiError, ByteOrder] =
    endian match
      case GiftiEndian.Little => Right(ByteOrder.LITTLE_ENDIAN)
      case GiftiEndian.Big => Right(ByteOrder.BIG_ENDIAN)
      case other => Left(GiftiError.UnsupportedEndian(other))

  private def validateCount[A](array: GiftiDataArray, values: Array[A]): Either[GiftiError, Array[A]] =
    if values.length == array.elementCount then Right(values)
    else
      Left(
        GiftiError.DecodeFailure(
          s"${array.intent.code} decoded ${values.length} values but dimensions require ${array.elementCount}"
        )
      )

  private def required(attrs: Map[String, String], name: String, label: String): Either[GiftiError, String] =
    attrs.get(name).filter(_.trim.nonEmpty).toRight(GiftiError.InvalidDataArray(s"$label is required"))

  private def intAttr(attrs: Map[String, String], name: String, label: String): Either[GiftiError, Int] =
    attrs.get(name).toRight(GiftiError.InvalidDataArray(s"$label is required")).flatMap { value =>
      parseInt(value, label)
    }

  private def optionalLongAttr(attrs: Map[String, String], name: String, label: String): Either[GiftiError, Option[Long]] =
    attrs.get(name).map(_.trim).filter(_.nonEmpty) match
      case None => Right(None)
      case Some(value) =>
        try
          val parsed = value.toLong
          if parsed >= 0L then Right(Some(parsed))
          else Left(GiftiError.InvalidDataArray(s"$label must be non-negative"))
        catch case _: NumberFormatException => Left(GiftiError.InvalidDataArray(s"$label must be an integer"))

  private def optionalDoubleAttr(attrs: Map[String, String], name: String, label: String): Either[GiftiError, Option[Double]] =
    attrs.get(name).map(_.trim).filter(_.nonEmpty) match
      case None => Right(None)
      case Some(value) =>
        try
          val parsed = value.toDouble
          if parsed.isFinite then Right(Some(parsed))
          else Left(GiftiError.InvalidDocument(s"$label must be finite"))
        catch case _: NumberFormatException => Left(GiftiError.InvalidDocument(s"$label must be numeric"))

  private def parseInt(value: String, label: String): Either[GiftiError, Int] =
    try Right(value.trim.toInt)
    catch case _: NumberFormatException => Left(GiftiError.InvalidDataArray(s"$label must be an integer"))

  private def parseDocumentInt(value: String, label: String): Either[GiftiError, Int] =
    try Right(value.trim.toInt)
    catch case _: NumberFormatException => Left(GiftiError.InvalidDocument(s"$label must be an integer"))

  private def cleanRequirement(message: String): String =
    Option(message)
      .getOrElse("requirement failed")
      .stripPrefix("requirement failed: ")

  private def childText(parent: Element, name: String): Option[String] =
    children(parent, name).headOption.map(_.getTextContent)

  private def children(parent: Element, name: String): Vector[Element] =
    val out = Vector.newBuilder[Element]
    var child = parent.getFirstChild
    while child != null do
      if child.getNodeType == Node.ELEMENT_NODE && child.getNodeName == name then
        out += child.asInstanceOf[Element]
      child = child.getNextSibling
    out.result()

  private def attributes(element: Element): Map[String, String] =
    val nodes: NamedNodeMap = element.getAttributes
    Vector
      .tabulate(nodes.getLength)(i => nodes.item(i))
      .map(node => node.getNodeName -> node.getNodeValue)
      .toMap

  private def splitFields(text: String): Array[String] =
    text.trim.split("\\s+").filter(_.nonEmpty)

  private def sequence[A](values: Vector[Either[GiftiError, A]]): Either[GiftiError, Vector[A]] =
    val out = Vector.newBuilder[A]
    val iterator = values.iterator
    while iterator.hasNext do
      iterator.next() match
        case Right(value) => out += value
        case Left(error) => return Left(error)
    Right(out.result())
