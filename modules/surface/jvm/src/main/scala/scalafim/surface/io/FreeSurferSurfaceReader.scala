package scalafim.surface.io

import scalafim.surface.Hemisphere
import scalafim.surface.SurfaceGeometry
import scalafim.surface.SurfaceKind
import scalafim.surface.TriangleMesh

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object FreeSurferSurfaceReader:

  private val TriangleFileMagic = 16777214

  def read(path: Path): SurfaceGeometry =
    if isAscii(path) then readAscii(path) else readBinary(path)

  def readAscii(path: Path): SurfaceGeometry =
    readAscii(path, inferHemisphere(path), inferKind(path))

  def readAscii(path: Path, hemisphere: Hemisphere, kind: SurfaceKind): SurfaceGeometry =
    val lines = Files.readAllLines(path, StandardCharsets.UTF_8)
    require(lines.size >= 2, "FreeSurfer ASCII surface must contain a header and counts line")

    val counts = splitFields(lines.get(1))
    require(counts.length >= 2, "FreeSurfer ASCII counts line must contain vertex and face counts")

    val vertexCount = parsePositiveInt(counts(0), "FreeSurfer ASCII vertex count")
    val faceCount = parsePositiveInt(counts(1), "FreeSurfer ASCII face count")
    val dataLines = lines.subList(2, lines.size).toArray(Array.empty[String]).filter(_.trim.nonEmpty)
    require(
      dataLines.length >= vertexCount + faceCount,
      "FreeSurfer ASCII surface ended before all vertices and faces were read"
    )

    val coordinates = Array.ofDim[Double](vertexCount * 3)
    var row = 0
    while row < vertexCount do
      val fields = splitFields(dataLines(row))
      require(fields.length >= 3, s"FreeSurfer ASCII vertex row ${row + 1} must contain 3 coordinates")
      var col = 0
      while col < 3 do
        coordinates(row * 3 + col) = parseDouble(fields(col), s"FreeSurfer ASCII vertex row ${row + 1}")
        col += 1
      row += 1

    val faces = Array.ofDim[Int](faceCount * 3)
    row = 0
    while row < faceCount do
      val fields = splitFields(dataLines(vertexCount + row))
      require(fields.length >= 3, s"FreeSurfer ASCII face row ${row + 1} must contain 3 vertex indices")
      var col = 0
      while col < 3 do
        faces(row * 3 + col) = parseInt(fields(col), s"FreeSurfer ASCII face row ${row + 1}")
        col += 1
      row += 1

    SurfaceGeometry(TriangleMesh.fromArrays(coordinates, faces), hemisphere, kind)

  def readBinary(path: Path): SurfaceGeometry =
    readBinary(path, inferHemisphere(path), inferKind(path))

  def readBinary(path: Path, hemisphere: Hemisphere, kind: SurfaceKind): SurfaceGeometry =
    val bytes = Files.readAllBytes(path)
    require(bytes.length >= 3, "File too small to be a valid FreeSurfer binary header")

    val magic = readThreeByteInt(bytes, 0)
    require(magic == TriangleFileMagic, s"Unsupported FreeSurfer binary surface magic: $magic")

    var offset = 3
    offset = skipLine(bytes, offset, "FreeSurfer binary created-by line")
    offset = skipLine(bytes, offset, "FreeSurfer binary info line")
    require(bytes.length - offset >= 8, "Unable to read vertex/face counts from FreeSurfer file")

    val counts = ByteBuffer.wrap(bytes, offset, 8).order(ByteOrder.BIG_ENDIAN)
    val vertexCount = counts.getInt()
    val faceCount = counts.getInt()
    require(vertexCount > 0, "FreeSurfer binary vertex count must be positive")
    require(faceCount > 0, "FreeSurfer binary face count must be positive")
    offset += 8

    val coordinateCount = checkedTriple(vertexCount, "FreeSurfer binary vertex count")
    val faceIndexCount = checkedTriple(faceCount, "FreeSurfer binary face count")
    val coordinateBytes = checkedBytes(coordinateCount, "FreeSurfer binary coordinates")
    val faceBytes = checkedBytes(faceIndexCount, "FreeSurfer binary faces")
    val expected = offset.toLong + coordinateBytes.toLong + faceBytes.toLong
    require(bytes.length >= expected, "FreeSurfer binary surface ended before all vertices and faces were read")

    val buffer = ByteBuffer.wrap(bytes, offset, (coordinateBytes + faceBytes)).order(ByteOrder.BIG_ENDIAN)
    val coordinates = Array.ofDim[Double](coordinateCount)
    var i = 0
    while i < coordinateCount do
      coordinates(i) = buffer.getFloat().toDouble
      i += 1

    val faces = Array.ofDim[Int](faceIndexCount)
    i = 0
    while i < faceIndexCount do
      faces(i) = buffer.getInt()
      i += 1

    SurfaceGeometry(TriangleMesh.fromArrays(coordinates, faces), hemisphere, kind)

  def inferHemisphere(path: Path): Hemisphere =
    val name = path.getFileName.toString
    val lower = name.toLowerCase
    if lower.matches("(^|.*[._-])lh([._-].*|\\..*)") || lower.startsWith("lh.") || lower.contains("hemi-l") then
      Hemisphere.Left
    else if lower.matches("(^|.*[._-])rh([._-].*|\\..*)") || lower.startsWith("rh.") || lower.contains("hemi-r") then
      Hemisphere.Right
    else Hemisphere.Unknown

  def inferKind(path: Path): SurfaceKind =
    val base = path.getFileName.toString
      .stripSuffix(".asc")
      .stripSuffix(".surf.gii")
      .stripSuffix(".gii")
    val tokens = base.split("[._-]").toVector.map(_.trim).filter(_.nonEmpty)
    tokens.reverseIterator.flatMap(knownKind).toSeq.headOption.getOrElse {
      SurfaceKind.Custom(tokens.lastOption.getOrElse("surface"))
    }

  private def isAscii(path: Path): Boolean =
    val name = path.getFileName.toString.toLowerCase
    name.endsWith(".asc") || Files.readAllBytes(path).headOption.contains('#'.toByte)

  private def readThreeByteInt(bytes: Array[Byte], offset: Int): Int =
    ((bytes(offset) & 0xff) << 16) |
      ((bytes(offset + 1) & 0xff) << 8) |
      (bytes(offset + 2) & 0xff)

  private def skipLine(bytes: Array[Byte], start: Int, label: String): Int =
    var i = start
    while i < bytes.length && bytes(i) != '\n'.toByte do i += 1
    require(i < bytes.length, s"Unable to read $label from FreeSurfer file")
    i + 1

  private def splitFields(line: String): Array[String] =
    line.trim.split("\\s+").filter(_.nonEmpty)

  private def knownKind(token: String): Option[SurfaceKind] =
    token.toLowerCase.replaceAll("\\d+$", "") match
      case "white" => Some(SurfaceKind.White)
      case "pial" => Some(SurfaceKind.Pial)
      case "inflated" => Some(SurfaceKind.Inflated)
      case "sphere" | "spherical" => Some(SurfaceKind.Sphere)
      case "smoothwm" | "smooth_wm" | "smooth-wm" => Some(SurfaceKind.SmoothWm)
      case "midthickness" | "mid_thickness" | "mid-thickness" => Some(SurfaceKind.Midthickness)
      case _ => None

  private def parsePositiveInt(value: String, label: String): Int =
    val parsed = parseInt(value, label)
    require(parsed > 0, s"$label must be positive")
    parsed

  private def parseInt(value: String, label: String): Int =
    try value.toInt
    catch case _: NumberFormatException => throw new IllegalArgumentException(s"$label must be an integer")

  private def parseDouble(value: String, label: String): Double =
    try value.toDouble
    catch case _: NumberFormatException => throw new IllegalArgumentException(s"$label must contain numeric coordinates")

  private def checkedTriple(value: Int, label: String): Int =
    require(value <= Int.MaxValue / 3, s"$label is too large")
    value * 3

  private def checkedBytes(count: Int, label: String): Int =
    require(count <= Int.MaxValue / 4, s"$label byte count is too large")
    count * 4
