package scalafim.dataset.zarr

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption.READ
import java.security.MessageDigest
import scala.util.Using
import scala.util.control.NonFatal
import scalafim.archive.zarr.*
import scalafim.dataset.DatasetError
import scalafim.dataset.io.NiftiStagingCache
import scalafim.image.DMat
import scalafim.image.io.{Nifti, NiftiHeader}
import zarr4s.*

final case class NiftiImportResult(
    manifest: NeuroArchiveManifest,
    descriptor: ArrayDescriptor,
    publication: PublicationReceipt
)

object NiftiCanonicalImporter:
  def publish(
      source: Path,
      bidsRelativePath: String,
      target: Path,
      chunkShape: Shape,
      staging: Option[NiftiStagingCache] = None
  ): Either[NeuroArchiveZarrError, NiftiImportResult] =
    publishWith(source, bidsRelativePath, target, ImportLayout.Direct(chunkShape), staging)

  def publishSharded(
      source: Path,
      bidsRelativePath: String,
      target: Path,
      profile: CanonicalChunkProfile = CanonicalChunkProfile.balancedV01,
      staging: Option[NiftiStagingCache] = None
  ): Either[NeuroArchiveZarrError, NiftiImportResult] =
    publishWith(source, bidsRelativePath, target, ImportLayout.Sharded(profile), staging)

  private def publishWith(
      source: Path,
      bidsRelativePath: String,
      target: Path,
      layout: ImportLayout,
      staging: Option[NiftiStagingCache]
  ): Either[NeuroArchiveZarrError, NiftiImportResult] =
    val absolute = source.toAbsolutePath.normalize()
    for
      bids <- BidsAcquisition.fromRelativePath(bidsRelativePath)
        .left.map(NeuroArchiveZarrError.PublicationFailure.apply)
      dataPath <- stage(absolute, staging)
      header <- readHeader(dataPath)
      scalar <- NiftiScalar.fromHeader(header)
      shape <- canonicalShape(header)
      descriptor <- compileDescriptor(shape, layout, scalar)
      sourceHash <- fileHash(absolute)
      logicalHash <- logicalHash(dataPath, header, scalar, shape)
      manifest <- makeManifest(bids, bidsRelativePath, header, shape, scalar, sourceHash, logicalHash, dataPath)
      receipt <- JvmNeuroArchivePublisher.create(
        target,
        descriptor,
        manifest,
        new NiftiChunkProvider(dataPath, header, descriptor, scalar)
      )
    yield NiftiImportResult(manifest, descriptor, receipt)

  private def stage(
      source: Path,
      staging: Option[NiftiStagingCache]
  ): Either[NeuroArchiveZarrError, Path] =
    if source.toString.endsWith(".gz") then staging match
      case None => Left(NeuroArchiveZarrError.PublicationFailure("compressed NIfTI import requires an explicit staging cache"))
      case Some(cache) => cache.stage(source).left.map(error => NeuroArchiveZarrError.PublicationFailure(error.message))
    else Right(source)

  private def readHeader(path: Path): Either[NeuroArchiveZarrError, NiftiHeader] =
    try Right(Nifti.readHeader(path))
    catch case NonFatal(error) => Left(NeuroArchiveZarrError.PublicationFailure(error.getMessage))

  private def canonicalShape(header: NiftiHeader): Either[NeuroArchiveZarrError, Shape] =
    if header.dims.length < 3 || header.dims.length > 4 then
      Left(NeuroArchiveZarrError.ManifestArrayMismatch("NIfTI input must be 3D or 4D"))
    else
      val time = if header.dims.length == 4 then header.dims(3).toLong else 1L
      Shape(time, header.dims(2).toLong, header.dims(1).toLong, header.dims(0).toLong)
        .left.map(NeuroArchiveZarrError.Kernel.apply)

  private def compileDescriptor(
      shape: Shape,
      layout: ImportLayout,
      scalar: NiftiScalar
  ): Either[NeuroArchiveZarrError, ArrayDescriptor] =
    val fill = scalar match
      case NiftiScalar.Float32 | NiftiScalar.Float64 => "0.0"
      case _ => "0"
    val codecs = layout match
      case ImportLayout.Direct(_) =>
        """[{"name":"bytes","configuration":{"endian":"little"}},{"name":"gzip","configuration":{"level":1}},{"name":"crc32c"}]"""
      case ImportLayout.Sharded(profile) =>
        val inner = profile.innerChunkShape.toVector.mkString("[", ",", "]")
        s"""[{"name":"sharding_indexed","configuration":{"chunk_shape":$inner,"codecs":[{"name":"bytes","configuration":{"endian":"little"}},{"name":"gzip","configuration":{"level":1}},{"name":"crc32c"}],"index_codecs":[{"name":"bytes","configuration":{"endian":"little"}},{"name":"crc32c"}],"index_location":"start"}}]"""
    val outerShape = layout match
      case ImportLayout.Direct(chunkShape) => chunkShape
      case ImportLayout.Sharded(profile) => profile.shardShape
    if outerShape.rank.toInt != 4 then
      return Left(NeuroArchiveZarrError.ManifestArrayMismatch("import chunk shape must have rank four"))
    val metadata =
      s"""{"zarr_format":3,"node_type":"array","shape":${shape.toVector.mkString("[", ",", "]")},"data_type":"${scalar.zarrName}","chunk_grid":{"name":"regular","configuration":{"chunk_shape":${outerShape.toVector.mkString("[", ",", "]")}}},"chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},"fill_value":$fill,"codecs":$codecs,"dimension_names":["t","z","y","x"],"attributes":{"neuroarchive_profile":"${NeuroArchiveManifest.profileId}"},"storage_transformers":[]}"""
    ZarrMetadata.parse(metadata).left.map(NeuroArchiveZarrError.Kernel.apply).flatMap:
      case ZarrNodeMetadata.Group(_) => Left(NeuroArchiveZarrError.ManifestArrayMismatch("generated canonical metadata is not an array"))
      case ZarrNodeMetadata.Array(array) =>
        ArrayDescriptor.compile(array).left.map(NeuroArchiveZarrError.Kernel.apply)

  private def makeManifest(
      bids: BidsAcquisition,
      relativePath: String,
      header: NiftiHeader,
      shape: Shape,
      scalar: NiftiScalar,
      sourceHash: Sha256Digest,
      logicalHash: Sha256Digest,
      dataPath: Path
  ): Either[NeuroArchiveZarrError, NeuroArchiveManifest] =
    for
      source <- SourceArtifact(relativePath, sourceHash)
      calibration <- ScalarCalibration(
        scalar.zarrName,
        if header.slope == 0.0 then 1.0 else header.slope,
        header.intercept
      )
      affine <- Affine4x4(selectedAffine(header).toRows.flatten)
      spatial <- Shape(shape.axis(1), shape.axis(2), shape.axis(3))
        .left.map(NeuroArchiveZarrError.Kernel.apply)
      geometry <- VoxelGeometry(spatial, affine)
      repetition <- repetitionTimeSeconds(dataPath, header.byteOrder)
      revision = ContentRevision.unsafe(Sha256.digestUtf8(
        s"${bids.acquisitionId.value}\u0000${sourceHash.value}\u0000${logicalHash.value}\u0000${header.slope}\u0000${header.intercept}\u0000${affine.toVector.mkString(",")}"
      ).value)
      manifest <- NeuroArchiveManifest(
        bids.acquisitionId,
        PayloadId.unsafe(s"canonical-${sourceHash.value.take(16)}"),
        revision,
        source,
        shape,
        calibration,
        geometry,
        AcquisitionTiming.Regular(0.0, repetition, shape.axis(0), TimeUnits.Second),
        logicalHash
      )
    yield manifest

  private def selectedAffine(header: NiftiHeader): DMat = header.preferredAffine.getOrElse:
    DMat.fromRows(Vector(
      Vector(header.pixdim(0), 0.0, 0.0, 0.0),
      Vector(0.0, header.pixdim(1), 0.0, 0.0),
      Vector(0.0, 0.0, header.pixdim(2), 0.0),
      Vector(0.0, 0.0, 0.0, 1.0)
    ))

  private def repetitionTimeSeconds(
      path: Path,
      order: ByteOrder
  ): Either[NeuroArchiveZarrError, Double] =
    try
      // pixdim[4] begins at byte 92 and xyzt_units is byte 123.
      val buffer = ByteBuffer.allocate(32).order(order)
      Using.resource(FileChannel.open(path, READ)): channel =>
        var position = 0L
        while buffer.hasRemaining do
          val count = channel.read(buffer, 92L + position)
          if count <= 0 then throw IllegalArgumentException("truncated NIfTI timing metadata")
          position += count.toLong
      val raw = math.abs(buffer.getFloat(0).toDouble)
      val value = if raw > 0.0 && raw.isFinite then raw else 1.0
      val temporalUnits = buffer.get(31).toInt & 0x38
      temporalUnits match
        case 0 | 8 => Right(value)
        case 16 => Right(value / 1000.0)
        case 24 => Right(value / 1000000.0)
        case found => Left(NeuroArchiveZarrError.PublicationFailure(
          s"unsupported NIfTI temporal unit code $found for a BOLD acquisition"
        ))
    catch case NonFatal(error) => Left(NeuroArchiveZarrError.PublicationFailure(error.getMessage))

  private def fileHash(path: Path): Either[NeuroArchiveZarrError, Sha256Digest] =
    try
      val digest = MessageDigest.getInstance("SHA-256")
      Using.resource(Files.newInputStream(path)): input =>
        val buffer = new Array[Byte](1024 * 1024)
        var count = input.read(buffer)
        while count >= 0 do
          if count > 0 then digest.update(buffer, 0, count)
          count = input.read(buffer)
      Right(Sha256Digest.unsafe(hex(digest.digest())))
    catch case NonFatal(error) => Left(NeuroArchiveZarrError.PublicationFailure(error.getMessage))

  private def logicalHash(
      path: Path,
      header: NiftiHeader,
      scalar: NiftiScalar,
      shape: Shape
  ): Either[NeuroArchiveZarrError, Sha256Digest] =
    try
      val digest = MessageDigest.getInstance("SHA-256")
      digest.update(s"neuroarchive-logical-payload-v1\u0000${scalar.zarrName}\u0000${shape.toVector.mkString(",")}\u0000".getBytes(StandardCharsets.UTF_8))
      val count = shape.elementCount.fold(error => throw IllegalArgumentException(error.message), identity)
      val bytes = new Array[Byte](scalar.byteWidth)
      val buffer = ByteBuffer.wrap(bytes)
      Using.resource(FileChannel.open(path, READ)): channel =>
        var element = 0L
        while element < count do
          buffer.clear()
          var read = 0L
          while buffer.hasRemaining do
            val found = channel.read(buffer, header.voxOffset.toLong + element * scalar.byteWidth + read)
            if found < 0 then throw IllegalArgumentException("truncated NIfTI payload")
            read += found.toLong
          if header.byteOrder == ByteOrder.LITTLE_ENDIAN || scalar.byteWidth == 1 then digest.update(bytes)
          else
            var index = bytes.length - 1
            while index >= 0 do
              digest.update(bytes(index))
              index -= 1
          element += 1L
      Right(Sha256Digest.unsafe(hex(digest.digest())))
    catch case NonFatal(error) => Left(NeuroArchiveZarrError.PublicationFailure(error.getMessage))

  private def hex(bytes: Array[Byte]): String =
    bytes.iterator.map(byte => f"${byte & 0xff}%02x").mkString

private enum NiftiScalar(val code: Int, val bitpix: Int, val zarrName: String):
  case UInt8 extends NiftiScalar(2, 8, "uint8")
  case Int16 extends NiftiScalar(4, 16, "int16")
  case Int32 extends NiftiScalar(8, 32, "int32")
  case Float32 extends NiftiScalar(16, 32, "float32")
  case Float64 extends NiftiScalar(64, 64, "float64")

  def byteWidth: Int = bitpix / 8

private object NiftiScalar:
  def fromHeader(header: NiftiHeader): Either[NeuroArchiveZarrError, NiftiScalar] =
    NiftiScalar.values.find(value => value.code == header.datatype && value.bitpix == header.bitpix)
      .toRight(NeuroArchiveZarrError.PublicationFailure(
        s"unsupported NIfTI datatype ${header.datatype} with bitpix ${header.bitpix}"
      ))

private enum ImportLayout:
  case Direct(chunkShape: Shape)
  case Sharded(profile: CanonicalChunkProfile)

private final class NiftiChunkProvider(
    path: Path,
    header: NiftiHeader,
    descriptor: ArrayDescriptor,
    scalar: NiftiScalar
) extends ChunkProvider:
  private val sourceChunkShape = descriptor.layout match
    case PhysicalLayout.Direct(_) => descriptor.grid.chunkShape
    case PhysicalLayout.Sharded(sharded, _, _, _, _) => sharded.innerChunkShape

  def chunk(coordinate: ChunkCoordinate, storedShape: Shape): Either[ZarrError, ChunkPayload] =
    try
      val count = storedShape.elementCount.fold(error => throw IllegalArgumentException(error.message), identity).toInt
      val block = scalar match
        case NiftiScalar.UInt8 => PrimitiveBlock.UInt8(OwnedBytes.copyOf(readBytes(coordinate, storedShape, count)))
        case NiftiScalar.Int16 => PrimitiveBlock.Int16(OwnedShorts.copyOf(readShorts(coordinate, storedShape, count)))
        case NiftiScalar.Int32 => PrimitiveBlock.Int32(OwnedInts.copyOf(readInts(coordinate, storedShape, count)))
        case NiftiScalar.Float32 => PrimitiveBlock.Float32(OwnedFloats.copyOf(readFloats(coordinate, storedShape, count)))
        case NiftiScalar.Float64 => PrimitiveBlock.Float64(OwnedDoubles.copyOf(readDoubles(coordinate, storedShape, count)))
      Right(ChunkPayload.Values(block))
    catch case NonFatal(error) => Left(ZarrError.WriteFailure(error.getMessage))

  private def readBytes(coordinate: ChunkCoordinate, shape: Shape, count: Int): Array[Byte] =
    val out = new Array[Byte](count)
    read(coordinate, shape)((offset, buffer) => out(offset) = buffer.get(0))
    out

  private def readShorts(coordinate: ChunkCoordinate, shape: Shape, count: Int): Array[Short] =
    val out = new Array[Short](count)
    read(coordinate, shape)((offset, buffer) => out(offset) = buffer.getShort(0))
    out

  private def readInts(coordinate: ChunkCoordinate, shape: Shape, count: Int): Array[Int] =
    val out = new Array[Int](count)
    read(coordinate, shape)((offset, buffer) => out(offset) = buffer.getInt(0))
    out

  private def readFloats(coordinate: ChunkCoordinate, shape: Shape, count: Int): Array[Float] =
    val out = new Array[Float](count)
    read(coordinate, shape)((offset, buffer) => out(offset) = buffer.getFloat(0))
    out

  private def readDoubles(coordinate: ChunkCoordinate, shape: Shape, count: Int): Array[Double] =
    val out = new Array[Double](count)
    read(coordinate, shape)((offset, buffer) => out(offset) = buffer.getDouble(0))
    out

  private def read(
      coordinate: ChunkCoordinate,
      storedShape: Shape
  )(put: (Int, ByteBuffer) => Unit): Unit =
    val cursor = new Array[Long](4)
    val scalarBytes = ByteBuffer.allocate(scalar.byteWidth).order(header.byteOrder)
    Using.resource(FileChannel.open(path, READ)): channel =>
      val count = storedShape.elementCount.fold(error => throw IllegalArgumentException(error.message), identity)
      var output = 0
      while output < count do
        var inside = true
        var linear = 0L
        var axis = 0
        while axis < 4 do
          val global = coordinate.axis(axis) * sourceChunkShape.axis(axis) + cursor(axis)
          if global >= descriptor.shape.axis(axis) then inside = false
          linear = linear * descriptor.shape.axis(axis) + global
          axis += 1
        if inside then
          scalarBytes.clear()
          var read = 0L
          while scalarBytes.hasRemaining do
            val found = channel.read(scalarBytes, header.voxOffset.toLong + linear * scalar.byteWidth + read)
            if found < 0 then throw IllegalArgumentException("truncated NIfTI payload")
            read += found.toLong
          put(output, scalarBytes)
        advance(cursor, storedShape)
        output += 1

  private def advance(cursor: Array[Long], shape: Shape): Unit =
    var axis = cursor.length - 1
    var advanced = false
    while axis >= 0 && !advanced do
      cursor(axis) += 1L
      if cursor(axis) < shape.axis(axis) then advanced = true
      else
        cursor(axis) = 0L
        axis -= 1
