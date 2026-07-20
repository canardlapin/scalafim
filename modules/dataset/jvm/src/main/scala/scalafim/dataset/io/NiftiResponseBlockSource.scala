package scalafim.dataset.io

import scalafim.dataset.*
import scalafim.image.{NArrayUtil, NeuroSpace}
import scalafim.image.io.{Nifti, NiftiHeader}

import java.io.BufferedInputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.StandardOpenOption.READ
import java.nio.file.{FileAlreadyExistsException, Files, Path}
import java.security.MessageDigest
import java.util.zip.GZIPInputStream
import scala.util.Using
import scala.util.control.NonFatal

final class NiftiStagingCache private (val root: Path):
  def stage(path: Path): Either[DatasetError, Path] =
    val normalized = path.toAbsolutePath.normalize()
    if !normalized.toString.endsWith(".gz") then Right(normalized)
    else this.synchronized(stageCompressed(normalized))

  private def stageCompressed(path: Path): Either[DatasetError, Path] =
    if !Files.isRegularFile(path) then
      Left(DatasetError.StorageFailure(s"compressed NIfTI does not exist: $path"))
    else
      var temporary = Option.empty[Path]
      try
        Files.createDirectories(root)
        val target = root.resolve(s"${metadataKey(path)}.nii")
        if Files.isRegularFile(target) && Files.size(target) >= 352L then Right(target)
        else
          Files.deleteIfExists(target)
          val tmp = Files.createTempFile(root, ".nifti-stage-", ".partial")
          temporary = Some(tmp)
          Using.Manager { use =>
            val rawInput = use(Files.newInputStream(path))
            val bufferedInput = use(new BufferedInputStream(rawInput))
            val input = use(new GZIPInputStream(bufferedInput))
            val output = use(Files.newOutputStream(tmp))
            input.transferTo(output)
          }.fold(error => throw error, _ => ())

          if Files.size(tmp) < 352L then
            Left(DatasetError.StorageFailure(s"staged NIfTI is smaller than its header: $path"))
          else
            try Files.move(tmp, target, ATOMIC_MOVE)
            catch
              case _: FileAlreadyExistsException => Files.deleteIfExists(tmp)
              case _: java.nio.file.AtomicMoveNotSupportedException =>
                try Files.move(tmp, target)
                catch case _: FileAlreadyExistsException => Files.deleteIfExists(tmp)
            temporary = None
            Right(target)
      catch
        case NonFatal(error) =>
          Left(DatasetError.StorageFailure(s"failed to stage compressed NIfTI '$path': ${error.getMessage}"))
      finally
        temporary.foreach(path => Files.deleteIfExists(path))

  private def metadataKey(path: Path): String =
    val attributes = Files.readAttributes(path, classOf[java.nio.file.attribute.BasicFileAttributes])
    val value = s"${path.toString}\u0000${attributes.size()}\u0000${attributes.lastModifiedTime().toMillis}"
    val digest = MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8))
    digest.iterator.map(byte => f"${byte & 0xff}%02x").mkString

object NiftiStagingCache:
  def make(root: Path): Either[DatasetError, NiftiStagingCache] =
    val normalized = root.toAbsolutePath.normalize()
    try
      Files.createDirectories(normalized)
      if Files.isDirectory(normalized) then Right(new NiftiStagingCache(normalized))
      else Left(DatasetError.StorageFailure(s"NIfTI staging root is not a directory: $normalized"))
    catch
      case NonFatal(error) =>
        Left(DatasetError.StorageFailure(s"cannot create NIfTI staging root '$normalized': ${error.getMessage}"))

  def unsafe(root: Path): NiftiStagingCache =
    make(root).fold(error => throw new IllegalArgumentException(error.message), identity)

final class NiftiResponseBlockSource private (
    val sourcePath: Path,
    val dataPath: Path,
    val header: NiftiHeader,
    val shape: DatasetShape,
    val voxelDomain: VoxelDomain,
    val metadata: DatasetMetadata
) extends ResponseBlockSource:
  val wasStaged: Boolean =
    sourcePath != dataPath

  protected[dataset] def readResolved(
      selection: ResolvedDataSelection
  ): Either[DatasetError, FmriSeries] =
    val rows = selection.nTimepoints
    val columns = selection.nVoxels
    val bytesPerValue = header.bitpix / 8
    val valueCount = rows.toLong * columns.toLong
    if valueCount > Int.MaxValue.toLong then
      Left(DatasetError.StorageFailure(s"requested NIfTI block has $valueCount values and exceeds the supported array size"))
    else
      NiftiReadPlan.make(selection.voxels, bytesPerValue).flatMap { plan =>
        val values = NArrayUtil.ofSize[Double](valueCount.toInt)
        val buffer = ByteBuffer.allocateDirect(plan.maxBufferBytes).order(header.byteOrder)
        val slope = if header.slope == 0.0 then 1.0 else header.slope

        try
          Using.resource(FileChannel.open(dataPath, READ)) { open =>
            var row = 0
            while row < rows do
              val timepoint = selection.timepoints(row)
              val frameElement = Math.multiplyExact(timepoint.toLong, shape.spatialSize.toLong)
              var windowIndex = 0
              while windowIndex < plan.windows.length do
                val window = plan.windows(windowIndex)
                val firstElement = Math.addExact(frameElement, window.startVoxel.toLong)
                val byteOffset = Math.multiplyExact(firstElement, bytesPerValue.toLong)
                val position = Math.addExact(header.voxOffset.toLong, byteOffset)
                buffer.clear()
                buffer.limit(window.byteCount)
                readFully(open, buffer, position)

                var selectedIndex = 0
                while selectedIndex < window.selectedVoxels do
                  val scalarOffset = window.voxelOffsets(selectedIndex) * bytesPerValue
                  val outputColumn = window.outputColumns(selectedIndex)
                  values(row * columns + outputColumn) =
                    decodeAt(buffer, header.datatype, scalarOffset) * slope + header.intercept
                  selectedIndex += 1
                windowIndex += 1
              row += 1

            FmriSeries.make(
              data = matrixFromRowMajor(rows, columns, values),
              voxelIndices = selection.voxelIndexValues,
              timepoints = selection.timepointIndices,
              shape = shape,
              metadata = metadata
            )
          }
        catch
          case NonFatal(error) =>
            Left(DatasetError.StorageFailure(s"failed NIfTI block read from '$dataPath': ${error.getMessage}"))
      }

  private def readFully(channel: FileChannel, buffer: ByteBuffer, position: Long): Unit =
    var offset = 0L
    while buffer.hasRemaining do
      val read = channel.read(buffer, position + offset)
      if read < 0 then throw new IllegalArgumentException(s"unexpected EOF at byte ${position + offset}")
      if read == 0 then throw new IllegalArgumentException(s"unable to read byte ${position + offset}")
      offset += read.toLong

  private def decodeAt(buffer: ByteBuffer, datatype: Int, offset: Int): Double =
    datatype match
      case 2  => (buffer.get(offset) & 0xff).toDouble
      case 4  => buffer.getShort(offset).toDouble
      case 8  => buffer.getInt(offset).toDouble
      case 16 => buffer.getFloat(offset).toDouble
      case 64 => buffer.getDouble(offset)
      case other => throw new UnsupportedOperationException(s"unsupported NIfTI datatype $other")

object NiftiResponseBlockSource:
  def open(
      path: Path,
      staging: Option[NiftiStagingCache] = None,
      voxelDomain: Option[VoxelDomain] = None,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, NiftiResponseBlockSource] =
    val normalized = path.toAbsolutePath.normalize()
    val resolved =
      if normalized.toString.endsWith(".gz") then
        staging match
          case Some(cache) => cache.stage(normalized)
          case None => Left(DatasetError.StorageFailure(s"compressed NIfTI requires an explicit staging cache: $normalized"))
      else Right(normalized)

    resolved.flatMap { dataPath =>
      readHeader(dataPath).flatMap { header =>
        validateHeader(header).flatMap { case (space, timepoints) =>
          DatasetShape.make(space, timepoints).flatMap { shape =>
            val domain = voxelDomain.getOrElse(VoxelDomain.fullUnsafe(shape))
            if domain.spatialSize != shape.spatialSize then
              Left(DatasetError.ShapeMismatch(s"voxel domain size ${domain.spatialSize} does not match NIfTI spatial size ${shape.spatialSize}"))
            else Right(new NiftiResponseBlockSource(normalized, dataPath, header, shape, domain, metadata))
          }
        }
      }
    }

  private def readHeader(path: Path): Either[DatasetError, NiftiHeader] =
    try Right(Nifti.readHeader(path))
    catch
      case NonFatal(error) => Left(DatasetError.StorageFailure(s"failed to read NIfTI header '$path': ${error.getMessage}"))

  private def validateHeader(header: NiftiHeader): Either[DatasetError, (NeuroSpace, Int)] =
    val bytesPerValue = header.bitpix / 8
    val supported =
      (header.datatype == 2 && bytesPerValue == 1) ||
        (header.datatype == 4 && bytesPerValue == 2) ||
        (header.datatype == 8 && bytesPerValue == 4) ||
        (header.datatype == 16 && bytesPerValue == 4) ||
        (header.datatype == 64 && bytesPerValue == 8)
    if header.dims.length < 3 || header.dims.length > 4 then
      Left(DatasetError.StorageFailure(s"NIfTI response must be 3D or 4D, got dimensions ${header.dims.mkString("x")}"))
    else if !supported then
      Left(DatasetError.StorageFailure(s"unsupported NIfTI datatype ${header.datatype} with bitpix ${header.bitpix}"))
    else if header.voxOffset < 0 then
      Left(DatasetError.StorageFailure(s"NIfTI vox_offset must be non-negative, got ${header.voxOffset}"))
    else
      try
        val timepoints = if header.dims.length == 4 then header.dims(3) else 1
        val space = header.space.spatialSpace
        Right(space -> timepoints)
      catch
        case NonFatal(error) => Left(DatasetError.StorageFailure(s"invalid NIfTI geometry: ${error.getMessage}"))
