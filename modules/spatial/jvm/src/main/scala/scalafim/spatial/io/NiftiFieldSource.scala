package scalafim.spatial.io

import scalafim.image.GridCompatibility
import scalafim.image.io.{Nifti, NiftiHeader}
import scalafim.spatial.*

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.READ
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import scala.util.Using
import scala.util.control.NonFatal

final case class NiftiFieldSourceStats(
  validationCalls: Long,
  readCalls: Long,
  headerReads: Long,
  channelOpens: Long,
  readWindows: Long,
  bytesRead: Long
)

private final case class NiftiFileStamp(size: Long, modifiedMillis: Long):
  def fingerprint: String =
    s"size=$size,mtime=$modifiedMillis"

private final case class ValidatedNiftiState(
  stamp: NiftiFileStamp,
  header: NiftiHeader
)

private final case class NiftiReadWindow(
  startVoxel: Int,
  voxelCount: Int,
  outputRows: Vector[Int]
):
  def byteCount(bytesPerValue: Int): Int =
    Math.multiplyExact(voxelCount, bytesPerValue)

final class NiftiFieldSource private (
  val path: Path,
  override val descriptor: FieldSourceDescriptor
) extends FieldSource:
  private var baseline = Option.empty[NiftiFileStamp]
  private var cachedState = Option.empty[ValidatedNiftiState]
  private var validationCount = 0L
  private var readCount = 0L
  private var headerReadCount = 0L
  private var channelOpenCount = 0L
  private var windowReadCount = 0L
  private var byteReadCount = 0L

  override def validate(): Either[SpatialError, Unit] =
    this.synchronized {
      validationCount += 1L
      resolveState().map(_ => ())
    }

  override def read(request: FieldSourceRequest): Either[SpatialError, FieldSourceBlock] =
    this.synchronized {
      readCount += 1L
      for
        checkedRequest <- FieldSourceRequest.make(descriptor, request.sourceRows, request.observations)
        state <- resolveState()
        block <- readBlock(state.header, checkedRequest)
      yield block
    }

  def stats: NiftiFieldSourceStats =
    this.synchronized {
      NiftiFieldSourceStats(
        validationCalls = validationCount,
        readCalls = readCount,
        headerReads = headerReadCount,
        channelOpens = channelOpenCount,
        readWindows = windowReadCount,
        bytesRead = byteReadCount
      )
    }

  private def resolveState(): Either[SpatialError, ValidatedNiftiState] =
    currentStamp().flatMap { stamp =>
      baseline match
        case Some(expected) if expected != stamp =>
          Left(SpatialError.FieldSourceStale(descriptor.id, expected.fingerprint, stamp.fingerprint))
        case _ =>
          cachedState match
            case Some(state) if state.stamp == stamp =>
              Right(state)
            case _ =>
              readAndValidateHeader(stamp).map { state =>
                if baseline.isEmpty then baseline = Some(stamp)
                cachedState = Some(state)
                state
              }
    }

  private def currentStamp(): Either[SpatialError, NiftiFileStamp] =
    try
      if !Files.isRegularFile(path) then
        Left(SpatialError.FieldSourceUnavailable(descriptor.id, s"NIfTI file does not exist: $path"))
      else
        val attributes = Files.readAttributes(path, classOf[BasicFileAttributes])
        Right(NiftiFileStamp(attributes.size(), attributes.lastModifiedTime().toMillis))
    catch
      case NonFatal(error) =>
        Left(SpatialError.FieldSourceUnavailable(descriptor.id, detail(error)))

  private def readAndValidateHeader(stamp: NiftiFileStamp): Either[SpatialError, ValidatedNiftiState] =
    try
      headerReadCount += 1L
      Nifti
        .readHeader(path)
        .left
        .map(error => SpatialError.FieldSourceReadFailed(descriptor.id, error.message))
        .flatMap: header =>
          validateHeader(header).map(_ => ValidatedNiftiState(stamp, header))
    catch
      case NonFatal(error) =>
        Left(SpatialError.FieldSourceReadFailed(descriptor.id, detail(error)))

  private def validateHeader(header: NiftiHeader): Either[SpatialError, Unit] =
    val bytesPerValue = header.bitpix / 8
    val supported =
      (header.datatype == 2 && bytesPerValue == 1) ||
        (header.datatype == 4 && bytesPerValue == 2) ||
        (header.datatype == 8 && bytesPerValue == 4) ||
        (header.datatype == 16 && bytesPerValue == 4) ||
        (header.datatype == 64 && bytesPerValue == 8)
    if header.dims.length < 3 || header.dims.length > 4 then
      Left(SpatialError.FieldSourceReadFailed(descriptor.id, s"expected a 3D or 4D NIfTI, got ${header.dims.mkString("x")}"))
    else if !supported then
      Left(SpatialError.FieldSourceReadFailed(descriptor.id, s"unsupported NIfTI datatype ${header.datatype} with bitpix ${header.bitpix}"))
    else if header.voxOffset < 0 then
      Left(SpatialError.FieldSourceReadFailed(descriptor.id, s"negative vox_offset ${header.voxOffset}"))
    else
      val actualRows = header.dims.take(3).product
      val actualObservations = if header.dims.length == 4 then header.dims(3) else 1
      if actualRows != descriptor.rows then
        Left(SpatialError.FieldSourceShapeMismatch(descriptor.id, descriptor.rows, actualRows))
      else if actualObservations != descriptor.observations then
        Left(SpatialError.FieldObservationMismatch(descriptor.observations, actualObservations))
      else
        descriptor.geometry match
          case SamplingGeometry.Volume(space, _)
              if GridCompatibility.spatial(space, header.space).isRight =>
            Right(())
          case SamplingGeometry.Volume(_, _) =>
            Left(SpatialError.FieldSourceGeometryMismatch(descriptor.id))
          case _ =>
            Left(SpatialError.FieldSourceGeometryMismatch(descriptor.id))

  private def readBlock(
    header: NiftiHeader,
    request: FieldSourceRequest
  ): Either[SpatialError, FieldSourceBlock] =
    val valueCount = request.rows.toLong * request.columns.toLong
    if valueCount > Int.MaxValue.toLong then
      Left(SpatialError.FieldSourceReadFailed(descriptor.id, s"requested block has $valueCount values"))
    else if request.rows == 0 then
      FieldSourceBlock.make(
        descriptor.id,
        request,
        GaleSpatialSupport.unsafeOwnedMatrix(0, request.columns, Array.emptyDoubleArray)
      )
    else
      try
        val bytesPerValue = header.bitpix / 8
        val windows = readWindows(request.sourceRows, header.dims.take(3))
        val maximumBytes = windows.map(_.byteCount(bytesPerValue)).max
        val buffer = ByteBuffer.allocateDirect(maximumBytes).order(header.byteOrder)
        val values = new Array[Double](valueCount.toInt)
        val slope = if header.slope == 0.0 then 1.0 else header.slope

        Using.resource(FileChannel.open(path, READ)) { channel =>
          channelOpenCount += 1L
          var outputColumn = 0
          while outputColumn < request.observations.length do
            val observation = request.observations(outputColumn)
            val frameElement = Math.multiplyExact(observation.toLong, descriptor.rows.toLong)
            var windowIndex = 0
            while windowIndex < windows.length do
              val window = windows(windowIndex)
              val firstElement = Math.addExact(frameElement, window.startVoxel.toLong)
              val byteOffset = Math.multiplyExact(firstElement, bytesPerValue.toLong)
              val position = Math.addExact(header.voxOffset.toLong, byteOffset)
              val bytes = window.byteCount(bytesPerValue)
              buffer.clear()
              buffer.limit(bytes)
              readFully(channel, buffer, position)
              windowReadCount += 1L
              byteReadCount += bytes.toLong

              var voxelOffset = 0
              while voxelOffset < window.voxelCount do
                val outputRow = window.outputRows(voxelOffset)
                values(outputRow * request.columns + outputColumn) =
                  decodeAt(buffer, header.datatype, voxelOffset * bytesPerValue) * slope + header.intercept
                voxelOffset += 1
              windowIndex += 1
            outputColumn += 1
        }
        FieldSourceBlock.make(
          descriptor.id,
          request,
          GaleSpatialSupport.unsafeOwnedMatrix(request.rows, request.columns, values)
        )
      catch
        case NonFatal(error) =>
          Left(SpatialError.FieldSourceReadFailed(descriptor.id, detail(error)))

  private def readWindows(
    sourceRows: Vector[Int],
    spatialDims: Vector[Int]
  ): Vector[NiftiReadWindow] =
    val nx = spatialDims(0)
    val ny = spatialDims(1)
    val nz = spatialDims(2)
    val indexed =
      sourceRows.zipWithIndex.map { case (canonicalOrdinal, outputRow) =>
        val z = canonicalOrdinal % nz
        val xy = canonicalOrdinal / nz
        val y = xy % ny
        val x = xy / ny
        val niftiOrdinal = x + nx * (y + ny * z)
        niftiOrdinal -> outputRow
      }.sortBy(_._1)
    val windows = Vector.newBuilder[NiftiReadWindow]
    var start = indexed.head._1
    var previous = start
    var outputs = Vector(indexed.head._2)
    var i = 1
    while i < indexed.length do
      val (voxel, outputRow) = indexed(i)
      if voxel == previous + 1 then
        previous = voxel
        outputs = outputs :+ outputRow
      else
        windows += NiftiReadWindow(start, previous - start + 1, outputs)
        start = voxel
        previous = voxel
        outputs = Vector(outputRow)
      i += 1
    windows += NiftiReadWindow(start, previous - start + 1, outputs)
    windows.result()

  private def readFully(channel: FileChannel, buffer: ByteBuffer, position: Long): Unit =
    var offset = 0L
    while buffer.hasRemaining do
      val count = channel.read(buffer, position + offset)
      if count < 0 then throw new IllegalArgumentException(s"unexpected EOF at byte ${position + offset}")
      if count == 0 then throw new IllegalArgumentException(s"unable to read byte ${position + offset}")
      offset += count.toLong

  private def decodeAt(buffer: ByteBuffer, datatype: Int, offset: Int): Double =
    datatype match
      case 2  => (buffer.get(offset) & 0xff).toDouble
      case 4  => buffer.getShort(offset).toDouble
      case 8  => buffer.getInt(offset).toDouble
      case 16 => buffer.getFloat(offset).toDouble
      case 64 => buffer.getDouble(offset)
      case other => throw new UnsupportedOperationException(s"unsupported NIfTI datatype $other")

  private def detail(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

object NiftiFieldSource:
  /**
   * Builds a source descriptor without touching the filesystem. The first terminal
   * validation pins the backing file revision; later changes are reported as stale.
   */
  def prepare(
    path: Path,
    domain: Domain,
    observations: Int,
    label: String = ""
  ): Either[SpatialError, NiftiFieldSource] =
    val normalized = path.toAbsolutePath.normalize()
    if normalized.toString.endsWith(".gz") then
      Left(
        SpatialError.FieldSourceUnavailable(
          FieldSourceId.unsafe(s"nifti:$normalized"),
          "compressed NIfTI requires an explicitly staged uncompressed file"
        )
      )
    else
      for
        sourceId <- FieldSourceId(s"nifti:$normalized")
        revision <- FieldSourceRevision(s"first-read-snapshot:$normalized")
        descriptor <- FieldSourceDescriptor.make(
          sourceId,
          revision,
          if label.trim.isEmpty then normalized.getFileName.toString else label,
          domain.id,
          domain.geometry,
          domain.nElements,
          observations
        )
      yield new NiftiFieldSource(normalized, descriptor)
