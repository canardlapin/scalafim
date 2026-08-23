package scalafim.image.io

import java.lang.management.ManagementFactory
import java.nio.file.Files

import com.sun.management.ThreadMXBean
import image4s.ImageMetadata
import image4s.nifti.NiftiDatatype
import image4s.nifti.NiftiIoStrategy
import image4s.nifti.NiftiWriteOptions
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import scalafim.image.Axis
import scalafim.image.NeuroSeries
import scalafim.image.NeuroSpace
import scalafim.image.SomeNeuroSeries

final class NativeNiftiAllocationSuite extends FunSuite:
  @volatile private var retained: AnyRef = new Object

  test("native NIfTI adapters retain one scaled Ravel destination with bounded streaming"):
    val spatialShape = Vector(64, 48, 24)
    val timePoints = 8
    val valueCount = spatialShape.product * timePoints
    val sourceSpace =
      NeuroSpace
        .requireD3(
          NeuroSpace(spatialShape).addDim(timePoints, Some(Axis.Time))
        )
        .toOption
        .get
    val data =
      NDArray.tabulate[Double](64, 48, 24, 8): (x, y, z, time) =>
        x.toDouble + y.toDouble * 0.1 + z.toDouble * 0.01 + time.toDouble * 0.001
    val source =
      NeuroSeries
        .continuous(
          sourceSpace,
          data,
          ImageMetadata.named("native-nifti-allocation")
        )
        .toOption
        .map(SomeNeuroSeries.eraseSpace)
        .get
    val options = NiftiWriteOptions.forDatatype(NiftiDatatype.Float32)
    val directory = Files.createTempDirectory("scalafim-native-nifti-")
    val cases =
      Vector(
        "nii" -> directory.resolve("series.nii"),
        "nii.gz" -> directory.resolve("series.nii.gz")
      )

    cases.foreach: (name, path) =>
      assert(Nifti.writeSeries(path, source, options).isRight)
      assertEquals(Nifti.ioStrategy(path), NiftiIoStrategy.BoundedStreaming)

      val read = () =>
        retained = Nifti.readSeries(path).toOption.get.image.data
      Vector.fill(3)(read())
      val readAllocated = medianAllocation(read())
      val readNanos = medianElapsed(read())
      val outputBytes = valueCount.toLong * 8L
      val readLimit = outputBytes + 1024L * 1024L
      assert(
        readAllocated <= readLimit,
        s"$name read allocated $readAllocated B for a $outputBytes B Ravel output; " +
          s"limit=$readLimit"
      )
      receipt("read-scaled-double", name, valueCount, readAllocated, readNanos)

      val write = () =>
        retained = Nifti.writeSeries(path, source, options).toOption.get
      Vector.fill(3)(write())
      val writeAllocated = medianAllocation(write())
      val writeNanos = medianElapsed(write())
      val writeLimit = valueCount.toLong * 4L + 1024L * 1024L
      assert(
        writeAllocated <= writeLimit,
        s"$name write allocated $writeAllocated B for $valueCount values; " +
          s"limit=$writeLimit"
      )
      receipt("write-float32", name, valueCount, writeAllocated, writeNanos)

  private def medianAllocation(body: => Unit): Long =
    Vector.fill(7)(allocatedBytes(body)).sorted.apply(3)

  private def medianElapsed(body: => Unit): Long =
    Vector.fill(7)(elapsedNanos(body)).sorted.apply(3)

  @scala.annotation.nowarn("cat=deprecation")
  private def allocatedBytes(body: => Unit): Long =
    val bean =
      ManagementFactory.getThreadMXBean match
        case value: ThreadMXBean if value.isThreadAllocatedMemorySupported =>
          if !value.isThreadAllocatedMemoryEnabled then
            value.setThreadAllocatedMemoryEnabled(true)
          value
        case _ =>
          fail("this JVM does not expose per-thread allocation accounting")
    val threadId = Thread.currentThread().getId()
    val before = bean.getThreadAllocatedBytes(threadId)
    body
    val after = bean.getThreadAllocatedBytes(threadId)
    assert(retained ne null)
    after - before

  private def elapsedNanos(body: => Unit): Long =
    val before = System.nanoTime()
    body
    System.nanoTime() - before

  private def receipt(
      operation: String,
      format: String,
      values: Int,
      allocatedBytes: Long,
      elapsedNanos: Long
  ): Unit =
    println(
      s"SCALAFIM-NIFTI JVM receipt: operation=$operation, format=$format, " +
        s"values=$values, allocated=$allocatedBytes B, elapsed=$elapsedNanos ns, " +
        s"java=${sys.props.getOrElse("java.version", "unknown")}, " +
        s"scala=${util.Properties.versionNumberString}"
    )
