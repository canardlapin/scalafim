package scalafim.dataset.zarr

import java.nio.file.Files
import java.nio.file.Path
import scalafim.archive.zarr.*
import zarr4s.*

object NiftiOracleBatchMain:
  private val scalarNames = Vector("uint8", "int16", "int32", "float32", "float64")

  def main(arguments: Array[String]): Unit =
    require(arguments.length == 2, "expected input and output roots")
    val input = Path.of(arguments(0))
    val output = Path.of(arguments(1))
    Files.createDirectories(output)
    val inner = Shape(1L, 1L, 2L, 2L)
      .fold(error => throw IllegalArgumentException(error.message), identity)
    val shard = Shape(2L, 2L, 2L, 4L)
      .fold(error => throw IllegalArgumentException(error.message), identity)
    val profile = CanonicalChunkProfile("oracle-sharded", inner, shard)
      .fold(error => throw IllegalArgumentException(error.message), identity)
    scalarNames.foreach: scalar =>
      val relative = s"sub-01/func/sub-01_task-rest_acq-${scalar}_bold.nii"
      val revision = output.resolve(s"$scalar.zarr")
      NiftiCanonicalImporter.publishSharded(
        input.resolve(s"$scalar.nii"),
        relative,
        revision,
        profile
      ).fold(error => throw IllegalStateException(error.message), identity)
      val store = JvmFileStore.open(revision).fold(error => throw IllegalStateException(error), identity)
      val opened = NeuroArchiveZarr.openCanonical(store, runtime = JvmCodecRuntime.portable)
        .fold(error => throw IllegalStateException(error.message), identity)
      val acquisition = BidsAcquisition.fromRelativePath(relative)
        .fold(error => throw IllegalArgumentException(error), identity)
      BidsNiftiExporter.exportRevision(opened, output.resolve(s"$scalar-bids"), acquisition)
        .fold(error => throw IllegalStateException(error.message), identity)
