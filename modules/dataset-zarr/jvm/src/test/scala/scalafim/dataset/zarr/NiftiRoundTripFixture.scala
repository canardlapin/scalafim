package scalafim.dataset.zarr

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.Files
import java.nio.file.Path
import scalafim.archive.zarr.*
import zarr4s.*

object NiftiRoundTripFixture:
  val relativePath = "sub-01/func/sub-01_task-rest_run-01_bold.nii"

  final case class Result(
      source: Path,
      revision: Path,
      exportRoot: Path,
      opened: OpenedCanonicalBold,
      imported: NiftiImportResult
  )

  def create(root: Path): Result =
    Files.createDirectories(root)
    val source = writeSource(root.resolve("source.nii"))
    val revision = root.resolve("revision.zarr")
    val chunkShape = Shape(1L, 1L, 2L, 2L).fold(error => throw IllegalArgumentException(error.message), identity)
    val imported = NiftiCanonicalImporter.publish(
      source,
      relativePath,
      revision,
      chunkShape
    ).fold(error => throw IllegalStateException(error.message), identity)
    val store = JvmFileStore.open(revision).fold(error => throw IllegalStateException(error), identity)
    val opened = NeuroArchiveZarr.openCanonical(store, runtime = JvmCodecRuntime.portable)
      .fold(error => throw IllegalStateException(error.message), identity)
    val exportRoot = root.resolve("bids-export")
    val acquisition = BidsAcquisition.fromRelativePath(relativePath)
      .fold(error => throw IllegalArgumentException(error), identity)
    BidsNiftiExporter.exportRevision(opened, exportRoot, acquisition)
      .fold(error => throw IllegalStateException(error.message), identity)
    Result(source, revision, exportRoot, opened, imported)

  def writeSource(
      path: Path,
      repetitionTime: Float = 1.5f,
      xyztUnits: Byte = 10.toByte
  ): Path =
    val values = Vector.tabulate(24)(_.toShort)
    val bytes = new Array[Byte](352 + values.length * 2)
    val buffer = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(0, 348)
    buffer.putShort(40, 4.toShort)
    buffer.putShort(42, 3.toShort)
    buffer.putShort(44, 2.toShort)
    buffer.putShort(46, 2.toShort)
    buffer.putShort(48, 2.toShort)
    var dimension = 4
    while dimension < 7 do
      buffer.putShort(42 + dimension * 2, 1.toShort)
      dimension += 1
    buffer.putShort(70, 4.toShort)
    buffer.putShort(72, 16.toShort)
    buffer.putFloat(76, 1.0f)
    buffer.putFloat(80, 2.0f)
    buffer.putFloat(84, 2.0f)
    buffer.putFloat(88, 2.5f)
    buffer.putFloat(92, repetitionTime)
    buffer.putFloat(108, 352.0f)
    buffer.putFloat(112, 0.25f)
    buffer.putFloat(116, -2.0f)
    buffer.put(123, xyztUnits)
    buffer.putShort(254, 1.toShort)
    val affine = Vector(
      Vector(2.0f, 0.0f, 0.0f, -10.0f),
      Vector(0.0f, 2.0f, 0.0f, -12.0f),
      Vector(0.0f, 0.0f, 2.5f, -8.0f)
    )
    var row = 0
    while row < 3 do
      var column = 0
      while column < 4 do
        buffer.putFloat(280 + row * 16 + column * 4, affine(row)(column))
        column += 1
      row += 1
    buffer.put(344, 'n'.toByte)
    buffer.put(345, '+'.toByte)
    buffer.put(346, '1'.toByte)
    var index = 0
    while index < values.length do
      buffer.putShort(352 + index * 2, values(index))
      index += 1
    Files.write(path, bytes)
    path

object NiftiRoundTripFixtureMain:
  def main(arguments: Array[String]): Unit =
    require(arguments.length == 1, "expected output root")
    NiftiRoundTripFixture.create(Path.of(arguments(0)))

object NiftiShardedFixtureMain:
  def main(arguments: Array[String]): Unit =
    require(arguments.length == 1, "expected output root")
    val root = Path.of(arguments(0))
    Files.createDirectories(root)
    val source = NiftiRoundTripFixture.writeSource(root.resolve("source.nii"))
    val inner = Shape(1L, 1L, 2L, 2L)
      .fold(error => throw IllegalArgumentException(error.message), identity)
    val shard = Shape(2L, 2L, 2L, 4L)
      .fold(error => throw IllegalArgumentException(error.message), identity)
    val profile = CanonicalChunkProfile("oracle-sharded", inner, shard)
      .fold(error => throw IllegalArgumentException(error.message), identity)
    NiftiCanonicalImporter.publishSharded(
      source,
      NiftiRoundTripFixture.relativePath,
      root.resolve("revision.zarr"),
      profile
    ).fold(error => throw IllegalStateException(error.message), identity)
