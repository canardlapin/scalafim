package scalafim.transform.itk

import scalafim.transform.{NumberText, TransformCodec, TransformFormat, TransformIoError, TransformSource}

/** `#Insight Transform File V1.0` text: `.txt`, `.tfm`, and ANTs text `.mat`. */
object ItkTextCodec extends TransformCodec[ItkTransformFile]:
  val format: TransformFormat = TransformFormat.ItkText

  private val Magic = "#Insight Transform File"

  def decode(source: TransformSource): Either[TransformIoError, ItkTransformFile] =
    source match
      case TransformSource.Text(text) => parse(text)
      case TransformSource.Binary(bytes) =>
        parse(String(IArray.genericWrapArray(bytes).toArray, "UTF-8"))

  private def parse(text: String): Either[TransformIoError, ItkTransformFile] =
    val lines = text.linesIterator.map(_.trim).toVector
    if !lines.headOption.exists(_.startsWith(Magic)) then Left(TransformIoError.WrongSource(format, "an '#Insight Transform File' header"))
    else
      // Each transform block starts at a "Transform:" line; Parameters/FixedParameters follow within the block.
      val starts = lines.indices.filter(i => lines(i).startsWith("Transform:")).toVector
      if starts.isEmpty then Left(TransformIoError.Malformed("ITK text", "no Transform: entries"))
      else
        val blocks = starts.zip(starts.drop(1) :+ lines.size)
        val entries = blocks.map: (from, until) =>
          val block = lines.slice(from, until)
          val typeName = block.head.stripPrefix("Transform:").trim
          for
            parameters <- numbers(block, "Parameters:")
            fixed      <- numbers(block, "FixedParameters:")
          yield ItkEntry(typeName, parameters, fixed)
        entries
          .foldLeft[Either[TransformIoError, Vector[ItkEntry]]](Right(Vector.empty)): (acc, next) =>
            acc.flatMap(done => next.map(done :+ _))
          .map(ItkTransformFile(_))

  private def numbers(block: Vector[String], key: String): Either[TransformIoError, Vector[Double]] =
    block.find(_.startsWith(key)) match
      case None => Right(Vector.empty)
      case Some(line) =>
        val tokens = line.stripPrefix(key).trim.split("\\s+").toVector.filter(_.nonEmpty)
        val parsed = tokens.flatMap(_.toDoubleOption)
        if parsed.size == tokens.size then Right(parsed)
        else Left(TransformIoError.Malformed("ITK text", s"non-numeric value in '$line'"))

  def encode(file: ItkTransformFile): Either[TransformIoError, TransformSource] =
    if file.entries.isEmpty then Left(TransformIoError.Malformed("ITK text", "no transforms to write"))
    else
      val out = new StringBuilder("#Insight Transform File V1.0\n")
      file.entries.zipWithIndex.foreach: (entry, index) =>
        out.append(s"#Transform $index\n")
        out.append(s"Transform: ${entry.typeName}\n")
        if !entry.isComposite then
          out.append(s"Parameters: ${entry.parameters.map(NumberText.format).mkString(" ")}\n")
          out.append(s"FixedParameters: ${entry.fixedParameters.map(NumberText.format).mkString(" ")}\n")
      Right(TransformSource.Text(out.toString))

/** ITK's MATLAB v4 binary transform file (MatlabTransformIO): one variable named after the transform type and one
  * named `fixed`.
  */
object ItkMatlabCodec extends TransformCodec[ItkTransformFile]:
  val format: TransformFormat = TransformFormat.ItkMatlab

  private final case class Variable(name: String, values: Vector[Double])

  def decode(source: TransformSource): Either[TransformIoError, ItkTransformFile] =
    source match
      case TransformSource.Text(_) => Left(TransformIoError.WrongSource(format, "binary MATLAB v4 bytes"))
      case TransformSource.Binary(bytes) =>
        byteOrder(bytes).toRight(TransformIoError.WrongSource(format, "a MATLAB v4 matrix header")).flatMap: little =>
          variables(bytes, little).flatMap: vars =>
            val fixed = vars.find(_.name.equalsIgnoreCase("fixed")).map(_.values).getOrElse(Vector.empty)
            vars.filterNot(_.name.equalsIgnoreCase("fixed")) match
              case Vector(transform) => Right(ItkTransformFile(Vector(ItkEntry(transform.name, transform.values, fixed))))
              case other =>
                Left(TransformIoError.Malformed("ITK MATLAB v4", s"expected one transform variable and 'fixed', found ${other.map(_.name).mkString(", ")}"))

  def encode(file: ItkTransformFile): Either[TransformIoError, TransformSource] =
    file.entries match
      case Vector(entry) if !entry.isComposite =>
        val out = Vector.newBuilder[Byte]
        def variable(name: String, values: Vector[Double]): Unit =
          val nameBytes = name.getBytes("US-ASCII") :+ 0.toByte
          Vector(0, values.size, 1, 0, nameBytes.length).foreach(int => out ++= littleEndian32(int))
          out ++= nameBytes
          values.foreach(v => out ++= littleEndian64(java.lang.Double.doubleToLongBits(v)))
        variable(entry.typeName, entry.parameters)
        variable("fixed", entry.fixedParameters)
        Right(TransformSource.Binary(IArray.from(out.result())))
      case _ =>
        Left(TransformIoError.Malformed("ITK MATLAB v4", "MATLAB v4 transform files hold exactly one non-composite transform"))

  private def littleEndian32(v: Int): Vector[Byte] =
    Vector(v.toByte, (v >>> 8).toByte, (v >>> 16).toByte, (v >>> 24).toByte)

  private def littleEndian64(v: Long): Vector[Byte] =
    Vector.tabulate(8)(i => (v >>> (8 * i)).toByte)

  private def int32(b: IArray[Byte], at: Int, little: Boolean): Int =
    if little then (b(at) & 0xff) | ((b(at + 1) & 0xff) << 8) | ((b(at + 2) & 0xff) << 16) | ((b(at + 3) & 0xff) << 24)
    else ((b(at) & 0xff) << 24) | ((b(at + 1) & 0xff) << 16) | ((b(at + 2) & 0xff) << 8) | (b(at + 3) & 0xff)

  private def int64(b: IArray[Byte], at: Int, little: Boolean): Long =
    val first = int32(b, at, little).toLong & 0xffffffffL
    val second = int32(b, at + 4, little).toLong & 0xffffffffL
    if little then (second << 32) | first else (first << 32) | second

  /** MOPT's thousands digit is the machine format: 0 little-endian IEEE, 1 big-endian IEEE. */
  private def byteOrder(b: IArray[Byte]): Option[Boolean] =
    if b.length < 20 then None
    else
      Vector(true, false).find: little =>
        val mopt = int32(b, 0, little)
        mopt >= 0 && mopt / 1000 == (if little then 0 else 1) && int32(b, 4, little) >= 0 && int32(b, 8, little) >= 0 && int32(b, 16, little) > 0 && int32(b, 16, little) <= 4096

  private def variables(b: IArray[Byte], little: Boolean): Either[TransformIoError, Vector[Variable]] =
    val out = Vector.newBuilder[Variable]
    var at = 0
    var failure: Option[TransformIoError] = None
    while failure.isEmpty && at < b.length do
      if b.length - at < 20 then failure = Some(TransformIoError.Malformed("ITK MATLAB v4", "truncated variable header"))
      else
        val mopt = int32(b, at, little)
        val rows = int32(b, at + 4, little)
        val cols = int32(b, at + 8, little)
        val imaginary = int32(b, at + 12, little)
        val nameLength = int32(b, at + 16, little)
        val precision = (mopt / 10) % 10
        val width = precision match
          case 0     => 8
          case 1 | 2 => 4
          case 3 | 4 => 2
          case 5     => 1
          case _     => -1
        val count = rows.toLong * cols.toLong
        if width < 0 || mopt % 10 != 0 || rows < 0 || cols < 0 || count > 1_000_000L || nameLength <= 0 || nameLength > 4096 then
          failure = Some(TransformIoError.Malformed("ITK MATLAB v4", s"unsupported variable header (mopt $mopt, ${rows}x$cols)"))
        else
          val dataStart = at + 20 + nameLength
          val dataBytes = count * width * (if imaginary != 0 then 2 else 1)
          if dataStart + dataBytes > b.length then failure = Some(TransformIoError.Malformed("ITK MATLAB v4", "truncated variable data"))
          else
            val name = (0 until nameLength).map(i => b(at + 20 + i)).takeWhile(_ != 0).map(c => (c & 0xff).toChar).mkString
            val values = Vector.tabulate(count.toInt): i =>
              val o = dataStart + i * width
              precision match
                case 0 => java.lang.Double.longBitsToDouble(int64(b, o, little))
                case 1 => java.lang.Float.intBitsToFloat(int32(b, o, little)).toDouble
                case 2 => int32(b, o, little).toDouble
                case 3 => (if little then ((b(o) & 0xff) | (b(o + 1) << 8)) else ((b(o) << 8) | (b(o + 1) & 0xff))).toShort.toDouble
                case 4 => (if little then ((b(o) & 0xff) | ((b(o + 1) & 0xff) << 8)) else (((b(o) & 0xff) << 8) | (b(o + 1) & 0xff))).toDouble
                case _ => (b(o) & 0xff).toDouble
            out += Variable(name, values)
            at = (dataStart + dataBytes).toInt
    failure.toLeft(out.result())
