package scalafim.image

import java.lang.management.ManagementFactory

/** Standalone diagnostic until the isolated JMH configuration can be added.
  *
  * Run with:
  *
  * `sbt 'imageJVM/Test/runMain scalafim.image.HalfFlowImageKernelProbe'`
  */
object HalfFlowImageKernelProbe:

  private final case class Measurement(
      medianNanos: Long,
      p95Nanos: Long,
      medianAllocatedBytes: Long,
      checksum: Double
  )

  private val allocationBean: Option[com.sun.management.ThreadMXBean] =
    ManagementFactory.getThreadMXBean match
      case bean: com.sun.management.ThreadMXBean =>
        if bean.isThreadAllocatedMemorySupported then
          if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
          Some(bean)
        else None
      case _ => None

  def main(args: Array[String]): Unit =
    val side = if args.nonEmpty then args(0).toInt else 48
    val warmups = if args.length >= 2 then args(1).toInt else 4
    val samples = if args.length >= 3 then args(2).toInt else 9
    require(side >= 4, "side must be at least four")
    require(warmups >= 1, "warmups must be positive")
    require(samples >= 3, "samples must be at least three")

    val grid = GridSpec.identity(Vector(side, side, side))
    val left = coordinateField(grid) { (x, y, z) =>
      (x + 0.25, y + 0.125, z + 0.375)
    }
    val right = coordinateField(grid) { (x, y, z) =>
      (
        1.01 * x + 0.015 * y + 0.1,
        0.99 * y + 0.01 * z - 0.2,
        1.005 * z + 0.02 * x + 0.05
      )
    }
    val points =
      Vector.tabulate(grid.nVoxels) { i =>
        Vector(
          left.values.data(i),
          left.values.data(i + grid.nVoxels),
          left.values.data(i + 2 * grid.nVoxels)
        )
      }
    val referencePlan =
      DenseFieldInterpolationPlan.make(grid, points, Resample.Method.Linear)
        .fold(err => throw new IllegalArgumentException(err.message), plan => plan)
    val destination = NArrayUtil.ofSize[Double](grid.nVoxels * 3)
    val destinationValid = NArrayUtil.ofSize[Boolean](grid.nVoxels)
    val rightSampler = DenseFieldSampler(right.grid)
    DenseFieldKernels.composePullInto(
      left,
      right,
      destination,
      destinationValid,
      rightSampler,
      FieldValidity.All,
      FieldValidity.All
    )
    var referenceResult = Vector.empty[Vector[Double]]

    val preparedReference = measure(warmups, samples) {
      referenceResult =
        referencePlan.sample(right.values, DenseFieldOutside.QueryPoint)
          .fold(err => throw new IllegalArgumentException(err.message), values => values)
      checksumReference(referenceResult, destinationValid)
    }
    val dynamicReference = measure(warmups, samples) {
      val plan =
        DenseFieldInterpolationPlan.make(grid, points, Resample.Method.Linear)
          .fold(err => throw new IllegalArgumentException(err.message), value => value)
      referenceResult =
        plan.sample(right.values, DenseFieldOutside.QueryPoint)
          .fold(err => throw new IllegalArgumentException(err.message), values => values)
      checksumReference(referenceResult, destinationValid)
    }
    val primitive = measure(warmups, samples) {
      DenseFieldKernels.composePullInto(
        left,
        right,
        destination,
        destinationValid,
        rightSampler,
        FieldValidity.All,
        FieldValidity.All
      )
      checksumPrimitive(destination, destinationValid, grid.nVoxels)
    }

    val maxError = maximumValidError(referenceResult, destination, destinationValid, grid.nVoxels)
    val validCount = countValid(destinationValid)
    val dynamicSpeedup = dynamicReference.medianNanos.toDouble / primitive.medianNanos.toDouble
    val preparedSpeedup = preparedReference.medianNanos.toDouble / primitive.medianNanos.toDouble
    val dynamicAllocationRatio =
      if primitive.medianAllocatedBytes > 0 then
        dynamicReference.medianAllocatedBytes.toDouble / primitive.medianAllocatedBytes.toDouble
      else Double.PositiveInfinity
    val preparedAllocationRatio =
      if primitive.medianAllocatedBytes > 0 then
        preparedReference.medianAllocatedBytes.toDouble / primitive.medianAllocatedBytes.toDouble
      else Double.PositiveInfinity

    println("HALF_FLOW_IMAGE_KERNEL_PROBE_JSON_BEGIN")
    println("{")
    println("  \"schema\": \"scalafim-half-flow-image-kernel-probe-v1\",")
    println(s"  \"jvm\": \"${escape(System.getProperty("java.vm.name"))} ${escape(System.getProperty("java.version"))}\",")
    println(s"  \"os\": \"${escape(System.getProperty("os.name"))} ${escape(System.getProperty("os.arch"))}\",")
    println(s"  \"side\": $side,")
    println(s"  \"voxels\": ${grid.nVoxels},")
    println(s"  \"warmups\": $warmups,")
    println(s"  \"samples\": $samples,")
    println(s"  \"validCount\": $validCount,")
    println(s"  \"maximumValidError\": $maxError,")
    println(s"  \"dynamicSpeedupMedian\": $dynamicSpeedup,")
    println(s"  \"preparedSpeedupMedian\": $preparedSpeedup,")
    println(s"  \"dynamicAllocationReductionMedian\": $dynamicAllocationRatio,")
    println(s"  \"preparedAllocationReductionMedian\": $preparedAllocationRatio,")
    printMeasurement("referencePlanAndSample", dynamicReference, trailingComma = true)
    printMeasurement("referencePreparedSample", preparedReference, trailingComma = true)
    printMeasurement("primitiveComposeInto", primitive, trailingComma = false)
    println("}")
    println("HALF_FLOW_IMAGE_KERNEL_PROBE_JSON_END")

  private def coordinateField(
      grid: GridSpec
  )(
      f: (Double, Double, Double) => (Double, Double, Double)
  ): DenseVectorField =
    val values = NArrayUtil.ofSize[Double](grid.nVoxels * 3)
    var z = 0
    while z < grid.shape.z do
      var y = 0
      while y < grid.shape.y do
        var x = 0
        while x < grid.shape.x do
          val i = x + y * grid.shape.x + z * grid.shape.x * grid.shape.y
          val mapped = f(x.toDouble, y.toDouble, z.toDouble)
          values(i) = mapped._1
          values(i + grid.nVoxels) = mapped._2
          values(i + 2 * grid.nVoxels) = mapped._3
          x += 1
        y += 1
      z += 1
    DenseVectorField(
      grid,
      NDArray(values, grid.dims :+ 3),
      DenseVectorFieldKind.SourceCoordinates
    )

  private def measure(
      warmups: Int,
      samples: Int
  )(
      operation: => Double
  ): Measurement =
    var checksum = 0.0
    var i = 0
    while i < warmups do
      checksum = operation
      i += 1

    val nanos = Array.ofDim[Long](samples)
    val allocated = Array.ofDim[Long](samples)
    i = 0
    while i < samples do
      val allocatedBefore = currentAllocatedBytes()
      val start = System.nanoTime()
      checksum = operation
      nanos(i) = System.nanoTime() - start
      val allocatedAfter = currentAllocatedBytes()
      allocated(i) =
        if allocatedBefore >= 0L && allocatedAfter >= allocatedBefore then allocatedAfter - allocatedBefore
        else -1L
      i += 1

    val sortedNanos = nanos.sorted
    val measuredAllocations = allocated.filter(_ >= 0L).sorted
    Measurement(
      medianNanos = percentile(sortedNanos, 0.5),
      p95Nanos = percentile(sortedNanos, 0.95),
      medianAllocatedBytes =
        if measuredAllocations.isEmpty then -1L else percentile(measuredAllocations, 0.5),
      checksum = checksum
    )

  private def checksumReference(
      values: Vector[Vector[Double]],
      valid: narr.NArray[Boolean]
  ): Double =
    var sum = 0.0
    var i = 0
    while i < values.length do
      if valid(i) then
        val point = values(i)
        val weight = (i % 17 + 1).toDouble
        sum += weight * (point(0) + 0.5 * point(1) + 0.25 * point(2))
      i += 1
    sum

  private def checksumPrimitive(
      values: narr.NArray[Double],
      valid: narr.NArray[Boolean],
      n: Int
  ): Double =
    var sum = 0.0
    var i = 0
    while i < n do
      if valid(i) then
        val weight = (i % 17 + 1).toDouble
        sum += weight * (values(i) + 0.5 * values(i + n) + 0.25 * values(i + 2 * n))
      i += 1
    sum

  private def maximumValidError(
      reference: Vector[Vector[Double]],
      actual: narr.NArray[Double],
      valid: narr.NArray[Boolean],
      n: Int
  ): Double =
    var maximum = 0.0
    var i = 0
    while i < n do
      if valid(i) then
        maximum = math.max(maximum, math.abs(actual(i) - reference(i)(0)))
        maximum = math.max(maximum, math.abs(actual(i + n) - reference(i)(1)))
        maximum = math.max(maximum, math.abs(actual(i + 2 * n) - reference(i)(2)))
      i += 1
    maximum

  private def countValid(valid: narr.NArray[Boolean]): Int =
    var count = 0
    var i = 0
    while i < valid.length do
      if valid(i) then count += 1
      i += 1
    count

  private def currentAllocatedBytes(): Long =
    allocationBean match
      case Some(bean) => bean.getCurrentThreadAllocatedBytes
      case None => -1L

  private def percentile(sorted: Array[Long], probability: Double): Long =
    val index = math.min(sorted.length - 1, math.ceil(probability * sorted.length.toDouble).toInt - 1)
    sorted(math.max(0, index))

  private def printMeasurement(
      name: String,
      measurement: Measurement,
      trailingComma: Boolean
  ): Unit =
    println(s"  \"$name\": {")
    println(s"    \"medianNanos\": ${measurement.medianNanos},")
    println(s"    \"p95Nanos\": ${measurement.p95Nanos},")
    println(s"    \"medianAllocatedBytes\": ${measurement.medianAllocatedBytes},")
    println(s"    \"checksum\": ${measurement.checksum}")
    println(if trailingComma then "  }," else "  }")

  private def escape(value: String): String =
    value.replace("\\", "\\\\").replace("\"", "\\\"")
