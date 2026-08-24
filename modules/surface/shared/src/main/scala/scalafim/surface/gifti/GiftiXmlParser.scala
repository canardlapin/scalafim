package scalafim.surface.gifti

import scala.io.Source
import scala.util.control.NonFatal
import scala.xml.{Elem, Node}
import scala.xml.parsing.ConstructingParser

private[surface] object GiftiXmlParser:
  val MaximumXmlCharacters: Int = 64 * 1024 * 1024

  def parseString(xml: String): Either[GiftiError, GiftiDocument] =
    if xml.length > MaximumXmlCharacters then
      Left(GiftiError.InvalidDocument(s"GIFTI XML input exceeds the $MaximumXmlCharacters character limit"))
    else if containsEntityDeclaration(xml) then
      Left(GiftiError.InvalidDocument("GIFTI XML entity declarations are unsupported"))
    else
      val source = Source.fromString(xml)
      try parseDocument(source)
      finally source.close()

  def parse(source: Source): Either[GiftiError, GiftiDocument] =
    val xml = source.take(MaximumXmlCharacters + 1).mkString
    parseString(xml)

  private def parseDocument(source: Source): Either[GiftiError, GiftiDocument] =
    try
      // ConstructingParser is implemented in Scala and therefore gives the
      // JVM and Scala.js the same parser. It is non-validating, so an external
      // GIFTI DTD declaration is recorded but never fetched. Override the
      // inherited JVM-oriented external-source loader so it cannot enter the
      // Scala.js linker graph even though non-validating parsing never calls it.
      SafeConstructingParser(source).initialize.document().docElem match
        case root: Elem if root.label == "GIFTI" => parseRoot(root)
        case _ => Left(GiftiError.InvalidDocument("root element must be GIFTI"))
    catch
      case NonFatal(error) =>
        Left(GiftiError.Xml(Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))

  private def containsEntityDeclaration(xml: String): Boolean =
    val declaration = "<!ENTITY"
    var index = 0
    while index <= xml.length - declaration.length do
      if xml.regionMatches(true, index, declaration, 0, declaration.length) then
        return true
      index += 1
    false

  private final class SafeConstructingParser(source: Source)
      extends ConstructingParser(source, preserveWS = true):

    override def externalSource(systemId: String): Source =
      Source.fromString("")

  private object SafeConstructingParser:

    def apply(source: Source): SafeConstructingParser =
      new SafeConstructingParser(source)

  private def parseRoot(root: Elem): Either[GiftiError, GiftiDocument] =
    for
      arrays <- sequence(children(root, "DataArray").map(parseDataArray))
      labels <- sequence(children(root, "LabelTable").flatMap(table => children(table, "Label")).map(parseLabel))
      document <- buildDocument(root, labels, arrays)
    yield document

  private def parseDataArray(element: Elem): Either[GiftiError, GiftiDataArray] =
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

  private def parseTransform(element: Elem): Either[GiftiError, GiftiTransform] =
    for
      matrixText <- childText(element, "MatrixData")
        .toRight(GiftiError.InvalidDataArray("CoordinateSystemTransformMatrix requires MatrixData"))
      values <- parseDoubles(matrixText, "CoordinateSystemTransformMatrix")
      transform <-
        if values.length == 16 then
          buildTransform(
            dataSpace = childText(element, "DataSpace"),
            transformedSpace = childText(element, "TransformedSpace"),
            matrixData = values.toVector
          )
        else Left(GiftiError.InvalidDataArray("CoordinateSystemTransformMatrix must contain 16 values"))
    yield transform

  private def parseLabel(element: Elem): Either[GiftiError, GiftiLabel] =
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
        name = element.text.trim,
        red = red,
        green = green,
        blue = blue,
        alpha = alpha
      )
    yield label

  private def buildDocument(
    root: Elem,
    labels: Vector[GiftiLabel],
    arrays: Vector[GiftiDataArray]
  ): Either[GiftiError, GiftiDocument] =
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

  private def buildTransform(
    dataSpace: Option[String],
    transformedSpace: Option[String],
    matrixData: Vector[Double]
  ): Either[GiftiError, GiftiTransform] =
    try Right(GiftiTransform(dataSpace, transformedSpace, matrixData))
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

  private def metadata(parent: Elem): Map[String, String] =
    children(parent, "MetaData")
      .flatMap(table => children(table, "MD"))
      .flatMap { md =>
        for
          name <- childText(md, "Name").map(_.trim).filter(_.nonEmpty)
          value <- childText(md, "Value")
        yield name -> value.trim
      }
      .toMap

  private def dataText(element: Elem): Either[GiftiError, String] =
    childText(element, "Data").toRight(GiftiError.InvalidDataArray("DataArray requires Data"))

  private def dimensions(
    attrs: Map[String, String],
    dimensionality: Int
  ): Either[GiftiError, Vector[Int]] =
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

  private def parseDoubles(text: String, label: String): Either[GiftiError, Array[Double]] =
    try Right(splitFields(text).map(_.toDouble))
    catch case _: NumberFormatException => Left(GiftiError.DecodeFailure(s"$label contains non-numeric ASCII data"))

  private def required(
    attrs: Map[String, String],
    name: String,
    label: String
  ): Either[GiftiError, String] =
    attrs.get(name).filter(_.trim.nonEmpty).toRight(GiftiError.InvalidDataArray(s"$label is required"))

  private def intAttr(
    attrs: Map[String, String],
    name: String,
    label: String
  ): Either[GiftiError, Int] =
    attrs.get(name).toRight(GiftiError.InvalidDataArray(s"$label is required")).flatMap { value =>
      parseInt(value, label)
    }

  private def optionalLongAttr(
    attrs: Map[String, String],
    name: String,
    label: String
  ): Either[GiftiError, Option[Long]] =
    attrs.get(name).map(_.trim).filter(_.nonEmpty) match
      case None => Right(None)
      case Some(value) =>
        try
          val parsed = value.toLong
          if parsed >= 0L then Right(Some(parsed))
          else Left(GiftiError.InvalidDataArray(s"$label must be non-negative"))
        catch case _: NumberFormatException => Left(GiftiError.InvalidDataArray(s"$label must be an integer"))

  private def optionalDoubleAttr(
    attrs: Map[String, String],
    name: String,
    label: String
  ): Either[GiftiError, Option[Double]] =
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

  private def childText(parent: Elem, name: String): Option[String] =
    children(parent, name).headOption.map(_.text)

  private def children(parent: Node, name: String): Vector[Elem] =
    parent.child.collect { case element: Elem if element.label == name => element }.toVector

  private def attributes(element: Elem): Map[String, String] =
    element.attributes.asAttrMap

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
