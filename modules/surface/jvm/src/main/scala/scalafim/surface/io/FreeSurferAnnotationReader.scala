package scalafim.surface.io

import java.io.{DataInputStream, EOFException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal
import scalafim.surface.LabelInfo

/** Decoder for the documented FreeSurfer `.annot` vertex/color-table format.
  * Annotation values are the stored packed colour codes; callers that need
  * stable semantic IDs must map them through the returned colour table.
  */
object FreeSurferAnnotationReader:
  final case class Annotation(vertexAnnotations: Array[Int], table: Vector[LabelInfo])

  def read(path: Path): Either[String, Annotation] =
    try
      val in = new DataInputStream(Files.newInputStream(path))
      try
        val count = in.readInt()
        if count < 0 then Left(s"negative FreeSurfer annotation vertex count $count")
        else
          val labels = Array.fill(count)(0)
          val seen = Array.fill(count)(false)
          var row = 0
          while row < count do
            val vertex = in.readInt()
            if vertex < 0 || vertex >= count then return Left(s"annotation vertex index $vertex outside [0,$count)")
            if seen(vertex) then return Left(s"duplicate annotation vertex index $vertex")
            labels(vertex) = in.readInt()
            seen(vertex) = true
            row += 1
          val hasTable = in.readInt()
          if hasTable == 0 then Right(Annotation(labels, Vector.empty))
          else readTable(in).map(Annotation(labels, _))
      finally in.close()
    catch
      case _: EOFException => Left("truncated FreeSurfer annotation")
      case NonFatal(error) => Left(Option(error.getMessage).getOrElse(error.getClass.getSimpleName))

  private def readTable(in: DataInputStream): Either[String, Vector[LabelInfo]] =
    val versionOrEntries = in.readInt()
    if versionOrEntries > 0 then oldTable(in, versionOrEntries)
    else if versionOrEntries == -2 then newTable(in)
    else Left(s"unsupported FreeSurfer colour-table version $versionOrEntries")

  private def oldTable(in: DataInputStream, entries: Int): Either[String, Vector[LabelInfo]] =
    readCString(in).flatMap: _ =>
      val output = Vector.newBuilder[LabelInfo]
      var index = 0
      var failure = Option.empty[String]
      while index < entries && failure.isEmpty do
        readCString(in) match
          case Left(error) => failure = Some(error)
          case Right(name) =>
            rgba(in) match
              case Left(error) => failure = Some(error)
              case Right(colour) => output += LabelInfo(pack(colour), name, Some(hex(colour)))
        index += 1
      failure.toLeft(output.result())

  private def newTable(in: DataInputStream): Either[String, Vector[LabelInfo]] =
    val maxEntries = in.readInt()
    if maxEntries < 0 then Left(s"negative FreeSurfer colour-table capacity $maxEntries")
    else readCString(in).flatMap: _ =>
      val entries = in.readInt()
      if entries < 0 || entries > maxEntries then Left(s"invalid FreeSurfer colour-table entry count $entries")
      else
        val output = Vector.newBuilder[LabelInfo]
        var row = 0
        val ids = scala.collection.mutable.Set.empty[Int]
        var failure = Option.empty[String]
        while row < entries && failure.isEmpty do
          val id = in.readInt()
          if id < 0 || id >= maxEntries then failure = Some(s"FreeSurfer colour-table entry id $id outside [0,$maxEntries)")
          else if !ids.add(id) then failure = Some(s"duplicate FreeSurfer colour-table entry id $id")
          else readCString(in) match
            case Left(error) => failure = Some(error)
            case Right(name) =>
              rgba(in) match
                case Left(error) => failure = Some(error)
                case Right(colour) => output += LabelInfo(pack(colour), name, Some(hex(colour)))
          row += 1
        failure.toLeft(output.result())

  private def rgba(in: DataInputStream): Either[String, (Int, Int, Int, Int)] =
    val value = (in.readInt(), in.readInt(), in.readInt(), in.readInt())
    if Vector(value._1, value._2, value._3, value._4).forall(channel => channel >= 0 && channel <= 255) then Right(value)
    else Left(s"FreeSurfer RGBA channels must be in [0,255], found ${value.productIterator.mkString(",")}")

  private def pack(value: (Int, Int, Int, Int)): Int =
    value._1 | (value._2 << 8) | (value._3 << 16)

  private def hex(value: (Int, Int, Int, Int)): String =
    f"#${value._1}%02x${value._2}%02x${value._3}%02x"

  private def readCString(in: DataInputStream): Either[String, String] =
    val size = in.readInt()
    if size <= 0 || size > 1024 * 1024 then Left(s"invalid FreeSurfer string size $size")
    else
      val bytes = Array.ofDim[Byte](size)
      in.readFully(bytes)
      val value = String(bytes, StandardCharsets.UTF_8).stripSuffix("\u0000")
      if value.nonEmpty then Right(value) else Left("FreeSurfer colour-table name is empty")
