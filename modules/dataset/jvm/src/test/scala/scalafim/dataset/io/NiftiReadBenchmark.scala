package scalafim.dataset.io

import scalafim.dataset.{DataSelection, TimepointSelection, VoxelIndex, VoxelSelection}

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.{CREATE, READ, TRUNCATE_EXISTING, WRITE}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Opt-in representative benchmark for the positional NIfTI block reader.
  *
  * Run with:
  * `sbt "datasetJVM/Test/runMain scalafim.dataset.io.NiftiReadBenchmark"`
  */
object NiftiReadBenchmark:
  private val Tolerance = 1e-12
  private val SpatialSize = 60000
  private val SelectedVoxels = 50000
  private val Timepoints = 200
  private val BytesPerValue = 8

  def main(args: Array[String]): Unit =
    val root = Files.createTempDirectory("scalafim-nifti-read-benchmark-")
    try
      val path = root.resolve("representative.nii")
      writeFixture(path)
      val voxels = Vector.tabulate(SelectedVoxels)(index => index + index / 100)
      val plan = NiftiReadPlan.make(voxels, BytesPerValue).fold(error => throw new IllegalStateException(error.message), identity)
      val stats = plan.stats(Timepoints)
      val selection = DataSelection(
        time = TimepointSelection.All,
        voxels = VoxelSelection.Indices(voxels.map(VoxelIndex.unsafe))
      )
      val source = NiftiResponseBlockSource.open(path).fold(error => throw new IllegalStateException(error.message), identity)

      val started = System.nanoTime()
      val block = source.readBlock(selection).fold(error => throw new IllegalStateException(error.message), identity)
      val elapsedSeconds = (System.nanoTime() - started).toDouble / 1e9
      val throughputMiB = stats.plannedBytes.toDouble / (1024.0 * 1024.0) / elapsedSeconds

      require(block.data.rows == Timepoints && block.data.cols == SelectedVoxels)
      require(math.abs(block.data(0, 0)) <= Tolerance)
      val lastVoxel = voxels.last
      val expectedLast = (Timepoints - 1).toDouble * SpatialSize.toDouble + lastVoxel.toDouble
      require(math.abs(block.data(Timepoints - 1, SelectedVoxels - 1) - expectedLast) <= Tolerance)

      println(
        Vector(
          s"timepoints=$Timepoints",
          s"selectedVoxels=$SelectedVoxels",
          s"windowsPerTimepoint=${stats.windowsPerTimepoint}",
          s"plannedReads=${stats.plannedReads}",
          s"plannedBytes=${stats.plannedBytes}",
          s"maxBufferBytes=${stats.maxBufferBytes}",
          f"elapsedSeconds=$elapsedSeconds%.3f",
          f"throughputMiBPerSecond=$throughputMiB%.1f"
        ).mkString("NIfTI read benchmark: ", " ", "")
      )
    finally
      val files = Files.walk(root)
      try files.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)
      finally files.close()

  private def writeFixture(path: Path): Unit =
    val header = ByteBuffer.allocate(352).order(ByteOrder.LITTLE_ENDIAN)
    header.putInt(0, 348)
    header.putShort(40, 4.toShort)
    header.putShort(42, 100.toShort)
    header.putShort(44, 100.toShort)
    header.putShort(46, 6.toShort)
    header.putShort(48, Timepoints.toShort)
    header.putShort(70, 64.toShort)
    header.putShort(72, 64.toShort)
    header.putFloat(80, 1.0f)
    header.putFloat(84, 1.0f)
    header.putFloat(88, 1.0f)
    header.putFloat(108, 352.0f)
    header.putFloat(112, 1.0f)
    val magic = "n+1".getBytes(StandardCharsets.US_ASCII)
    header.put(344, magic(0))
    header.put(345, magic(1))
    header.put(346, magic(2))
    header.position(0)

    val channel = FileChannel.open(path, CREATE, TRUNCATE_EXISTING, WRITE, READ)
    try
      writeFully(channel, header)
      val chunk = ByteBuffer.allocateDirect(1024 * 1024).order(ByteOrder.LITTLE_ENDIAN)
      val values = SpatialSize.toLong * Timepoints.toLong
      var element = 0L
      while element < values do
        chunk.clear()
        while chunk.remaining() >= BytesPerValue && element < values do
          chunk.putDouble(element.toDouble)
          element += 1L
        chunk.flip()
        writeFully(channel, chunk)
    finally channel.close()

  private def writeFully(channel: FileChannel, buffer: ByteBuffer): Unit =
    while buffer.hasRemaining do channel.write(buffer)
