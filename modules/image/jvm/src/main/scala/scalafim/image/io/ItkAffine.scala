package scalafim.image.io

import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.GeometryError

import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

enum ItkAffineReadError:
  case ReadFailed(path: Path, reason: String)
  case UnsupportedFormat(path: Path)
  case Malformed(path: Path, reason: String)
  case UnsupportedMatlabV4Precision(path: Path, code: Int)
  case InvalidAffine(path: Path, cause: GeometryError)

  def message: String =
    this match
      case ReadFailed(path, reason) => s"could not read ITK affine $path: $reason"
      case UnsupportedFormat(path) => s"$path is neither an ITK text transform nor a MATLAB v4 affine"
      case Malformed(path, reason) => s"malformed ITK affine $path: $reason"
      case UnsupportedMatlabV4Precision(path, code) =>
        s"unsupported MATLAB v4 precision code $code in ITK affine $path"
      case InvalidAffine(path, cause) =>
        s"invalid ITK affine $path: ${cause.message}"

/** One ITK affine with storage and coordinate conventions named explicitly.
  *
  * The file stores the matrix used by ITK `TransformPoint` in LPS millimetres.
  * Its semantic source and target depend on the calling application. In
  * particular, `antsApplyTransforms` uses the stored matrix as the output-grid
  * to input-image pull map; `antsPullbackRas` names exactly that adapter.
  */
final case class ItkAffineTransform private (
    parameters: Vector[Double],
    fixedParameters: Vector[Double],
    storedLps: Affine[D3],
    storedRas: Affine[D3]
):
  def antsPullbackRas: Affine[D3] =
    storedRas

  lazy val inverseRas: Affine[D3] =
    storedRas.inverse

object ItkAffineTransform:
  private[io] val lpsToRas: Affine[D3] =
    Affine
      .fromRowMajor[D3](
        Vector(
        Vector(-1.0, 0.0, 0.0, 0.0),
        Vector(0.0, -1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
        ).flatten
      )
      .fold(error => throw new IllegalStateException(error.message), identity)

  private[io] def make(
      path: Path,
      parameters: Vector[Double],
      fixedParameters: Vector[Double]
  ): Either[ItkAffineReadError, ItkAffineTransform] =
    if parameters.length < 12 then
      Left(ItkAffineReadError.Malformed(path, s"expected at least 12 parameters, got ${parameters.length}"))
    else if parameters.take(12).exists(value => !value.isFinite) then
      Left(ItkAffineReadError.Malformed(path, "affine parameters must be finite"))
    else
      val center = fixedParameters.padTo(3, 0.0).take(3)
      if center.exists(value => !value.isFinite) then
        Left(ItkAffineReadError.Malformed(path, "fixed parameters must be finite"))
      else
        val rows = Vector.tabulate(3) { row =>
          var column = 0
          var centered = 0.0
          while column < 3 do
            centered += parameters(row * 3 + column) * center(column)
            column += 1
          Vector.tabulate(3)(column => parameters(row * 3 + column)) :+
            (parameters(9 + row) + center(row) - centered)
        } :+ Vector(0.0, 0.0, 0.0, 1.0)
        Affine
          .fromRowMajor[D3](rows.flatten)
          .left
          .map(error => ItkAffineReadError.InvalidAffine(path, error))
          .flatMap: storedLps =>
            for
              fromLps <- lpsToRas
                .andThen(storedLps)
                .left
                .map(error => ItkAffineReadError.InvalidAffine(path, error))
              storedRas <- fromLps
                .andThen(lpsToRas)
                .left
                .map(error => ItkAffineReadError.InvalidAffine(path, error))
            yield new ItkAffineTransform(
              parameters.take(12),
              center,
              storedLps,
              storedRas
            )

object ItkAffine:
  private final case class MatlabVariable(rows: Int, columns: Int, values: Vector[Double])

  private val NumberPattern =
    "[-+]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?".r
  private val TextMagic = "#Insight Transform File"
  private val MaximumElements = 1_000_000L
  private val MaximumNameBytes = 4096

  def read(path: Path): Either[ItkAffineReadError, ItkAffineTransform] =
    try
      val bytes = Files.readAllBytes(path)
      if bytes.isEmpty then Left(ItkAffineReadError.UnsupportedFormat(path))
      else if startsWithTextMagic(bytes) then parseText(path, new String(bytes, StandardCharsets.UTF_8))
      else parseMatlabV4(path, bytes)
    catch
      case NonFatal(error) =>
        Left(ItkAffineReadError.ReadFailed(path, Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))

  private def startsWithTextMagic(bytes: Array[Byte]): Boolean =
    val length = math.min(bytes.length, TextMagic.length)
    new String(bytes, 0, length, StandardCharsets.US_ASCII) == TextMagic.take(length) &&
      bytes.length >= TextMagic.length

  private def parseText(path: Path, text: String): Either[ItkAffineReadError, ItkAffineTransform] =
    val transforms = text.linesIterator.filter(_.trim.startsWith("Transform:")).toVector
    if transforms.length != 1 || !transforms.head.toLowerCase.contains("affinetransform") ||
        !transforms.head.toLowerCase.contains("_3_3") then
      Left(ItkAffineReadError.Malformed(path, "expected exactly one ITK AffineTransform_float/double_3_3"))
    else
      val parameters = keyedNumbers(text, "Parameters:")
      val fixed = keyedNumbers(text, "FixedParameters:")
      ItkAffineTransform.make(path, parameters, fixed)

  private def keyedNumbers(text: String, key: String): Vector[Double] =
    text.linesIterator
      .find(_.trim.startsWith(key))
      .toVector
      .flatMap { line =>
        val start = line.indexOf(key) + key.length
        NumberPattern.findAllIn(line.drop(start)).map(_.toDouble)
      }

  private def parseMatlabV4(
      path: Path,
      bytes: Array[Byte]
  ): Either[ItkAffineReadError, ItkAffineTransform] =
    detectByteOrder(bytes) match
      case None => Left(ItkAffineReadError.UnsupportedFormat(path))
      case Some(order) =>
        parseVariables(path, bytes, order).flatMap { variables =>
          variables.collectFirst {
            case (name, variable) if name.toLowerCase.matches(".*affinetransform_(double|float)_3_3.*") =>
              variable
          } match
            case None => Left(ItkAffineReadError.Malformed(path, "missing AffineTransform_float/double_3_3 variable"))
            case Some(affine) =>
              val fixed = variables.collectFirst {
                case (name, variable) if name.equalsIgnoreCase("fixed") => variable.values
              }.getOrElse(Vector.empty)
              ItkAffineTransform.make(path, affine.values, fixed)
        }

  private def detectByteOrder(bytes: Array[Byte]): Option[ByteOrder] =
    if bytes.length < 20 then None
    else
      val little = plausibleHeader(ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN), ByteOrder.LITTLE_ENDIAN)
      val big = plausibleHeader(ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN), ByteOrder.BIG_ENDIAN)
      if little && !big then Some(ByteOrder.LITTLE_ENDIAN)
      else if big && !little then Some(ByteOrder.BIG_ENDIAN)
      else if little then Some(ByteOrder.LITTLE_ENDIAN)
      else None

  private def plausibleHeader(buffer: ByteBuffer, order: ByteOrder): Boolean =
    val mopt = buffer.getInt(0)
    val rows = buffer.getInt(4)
    val columns = buffer.getInt(8)
    val imaginary = buffer.getInt(12)
    val nameBytes = buffer.getInt(16)
    val machine = mopt / 1000
    val expectedMachine = if order == ByteOrder.LITTLE_ENDIAN then 0 else 1
    val elements = rows.toLong * columns.toLong
    mopt >= 0 && machine == expectedMachine && rows > 0 && columns > 0 && elements <= MaximumElements &&
      (imaginary == 0 || imaginary == 1) && nameBytes > 0 && nameBytes <= MaximumNameBytes &&
      20L + nameBytes.toLong <= buffer.limit().toLong

  private def parseVariables(
      path: Path,
      bytes: Array[Byte],
      order: ByteOrder
  ): Either[ItkAffineReadError, Vector[(String, MatlabVariable)]] =
    val buffer = ByteBuffer.wrap(bytes).order(order)
    val variables = Vector.newBuilder[(String, MatlabVariable)]
    while buffer.hasRemaining do
      if buffer.remaining < 20 then
        return Left(ItkAffineReadError.Malformed(path, "truncated MATLAB v4 variable header"))
      val mopt = buffer.getInt()
      val rows = buffer.getInt()
      val columns = buffer.getInt()
      val imaginary = buffer.getInt()
      val nameBytes = buffer.getInt()
      val elements = rows.toLong * columns.toLong
      val machine = mopt / 1000
      val expectedMachine = if order == ByteOrder.LITTLE_ENDIAN then 0 else 1
      val matrixType = mopt % 10
      if mopt < 0 || machine != expectedMachine || rows <= 0 || columns <= 0 || elements > MaximumElements ||
          (imaginary != 0 && imaginary != 1) || nameBytes <= 0 || nameBytes > MaximumNameBytes || matrixType != 0 then
        return Left(ItkAffineReadError.Malformed(path, "invalid MATLAB v4 variable header"))
      if buffer.remaining < nameBytes then
        return Left(ItkAffineReadError.Malformed(path, "truncated MATLAB v4 variable name"))

      val rawName = Array.ofDim[Byte](nameBytes)
      buffer.get(rawName)
      val terminator = rawName.indexWhere(_ == 0)
      val used = if terminator < 0 then rawName.length else terminator
      val name = new String(rawName, 0, used, StandardCharsets.US_ASCII)
      if name.isEmpty then return Left(ItkAffineReadError.Malformed(path, "empty MATLAB v4 variable name"))

      val precision = (mopt / 10) % 10
      val bytesPerValue =
        precision match
          case 0 => 8
          case 1 | 2 => 4
          case 3 | 4 => 2
          case 5 => 1
          case other => return Left(ItkAffineReadError.UnsupportedMatlabV4Precision(path, other))
      val valueBytes = elements * bytesPerValue.toLong
      val totalBytes = valueBytes * (if imaginary == 0 then 1L else 2L)
      if totalBytes > buffer.remaining.toLong then
        return Left(ItkAffineReadError.Malformed(path, s"truncated MATLAB v4 matrix $name"))

      val values = Vector.newBuilder[Double]
      var index = 0L
      while index < elements do
        val value =
          precision match
            case 0 => buffer.getDouble()
            case 1 => buffer.getFloat().toDouble
            case 2 => buffer.getInt().toDouble
            case 3 => buffer.getShort().toDouble
            case 4 => (buffer.getShort().toInt & 0xffff).toDouble
            case 5 => (buffer.get().toInt & 0xff).toDouble
        values += value
        index += 1
      if imaginary != 0 then buffer.position(buffer.position() + valueBytes.toInt)
      variables += name -> MatlabVariable(rows, columns, values.result())
    Right(variables.result())
