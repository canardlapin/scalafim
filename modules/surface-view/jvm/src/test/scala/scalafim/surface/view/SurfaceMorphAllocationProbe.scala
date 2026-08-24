package scalafim.surface.view

import com.sun.management.ThreadMXBean
import scalafim.surface.*

import java.lang.management.ManagementFactory

final case class SurfaceMorphAllocationReceipt(
  fixture: String,
  vertices: Int,
  faces: Int,
  iterations: Int,
  elapsedNanos: Long,
  allocatedBytes: Long,
  coordinateBytesPerMorph: Long,
  topologyReused: Boolean,
  checksum: Double
):
  def allocatedBytesPerIteration: Double =
    allocatedBytes.toDouble / iterations.toDouble

  def allocationAmplification: Double =
    allocatedBytesPerIteration / coordinateBytesPerMorph.toDouble

  def render: String =
    s"""{"receipt":"surface-morph-allocation-v1","fixture":"$fixture","vertices":$vertices,"faces":$faces,"iterations":$iterations,"elapsedNanos":$elapsedNanos,"allocatedBytes":$allocatedBytes,"allocatedBytesPerIteration":$allocatedBytesPerIteration,"coordinateBytesPerMorph":$coordinateBytesPerMorph,"allocationAmplification":$allocationAmplification,"topologyReused":$topologyReused,"checksum":$checksum}"""

/** Runnable allocation receipt for topology-preserving surface morphs.
  *
  * Construction of the two endpoint meshes is excluded. Each measured morph
  * must allocate only coordinate-realization state and must retain the exact
  * mesh4s topology owner.
  *
  * Run with:
  *
  * `sbt "surfaceViewJVM/Test/runMain scalafim.surface.view.SurfaceMorphAllocationProbe"`
  */
object SurfaceMorphAllocationProbe:
  private final case class Measurement[A](value: A, elapsedNanos: Long, allocatedBytes: Long)
  private final case class Grid(coordinates: Array[Double], faces: Array[Int])

  def main(args: Array[String]): Unit =
    val bean = allocationBean()
    Vector(
      ("generated-fsaverage5-scale", 129, 81, 20, 10),
      ("generated-cortical-scale", 498, 329, 5, 3)
    ).foreach: (name, columns, rows, iterations, warmupIterations) =>
      val receipt = run(name, columns, rows, iterations, warmupIterations, bean)
      require(receipt.topologyReused, s"${receipt.fixture} rebuilt its topology during morphing")
      require(
        receipt.allocationAmplification <= 2.5,
        s"${receipt.fixture} allocated ${receipt.allocationAmplification}x its coordinate payload"
      )
      println(receipt.render)

  private def run(
    name: String,
    columns: Int,
    rows: Int,
    iterations: Int,
    warmupIterations: Int,
    bean: ThreadMXBean
  ): SurfaceMorphAllocationReceipt =
    val grid = gridFixture(columns, rows)
    val fromMesh = TriangleMesh.fromArrays(grid.coordinates, grid.faces)
    val targetCoordinates = grid.coordinates.clone()
    var offset = 0
    while offset < targetCoordinates.length do
      targetCoordinates(offset) += 0.25
      targetCoordinates(offset + 1) -= 0.5
      targetCoordinates(offset + 2) += 1.0
      offset += 3
    val toMesh = fromMesh.withCoordinatesEither(targetCoordinates).toOption.get
    val from = SurfaceGeometry(fromMesh, Hemisphere.Left, SurfaceKind.White)
    val to = SurfaceGeometry(toMesh, Hemisphere.Left, SurfaceKind.Pial)

    var warmup = 0
    while warmup < warmupIterations do
      val fraction = if warmup % 2 == 0 then 0.25 else 0.75
      SurfaceMorph.interpolate(from, to, SurfaceMorphFraction.unsafe(fraction)).toOption.get
      warmup += 1

    val measured = measure(bean):
      var iteration = 0
      var checksum = 0.0
      var topologyReused = true
      while iteration < iterations do
        val fraction = if iteration % 2 == 0 then 0.25 else 0.75
        val geometry =
          SurfaceMorph.interpolate(from, to, SurfaceMorphFraction.unsafe(fraction)).toOption.get
        topologyReused &&= geometry.mesh.topology eq fromMesh.topology
        checksum += geometry.mesh.vertex(VertexId.unsafe(iteration % geometry.vertexCount)).z
        iteration += 1
      (checksum, topologyReused)

    SurfaceMorphAllocationReceipt(
      fixture = name,
      vertices = from.vertexCount,
      faces = from.faceCount,
      iterations = iterations,
      elapsedNanos = measured.elapsedNanos,
      allocatedBytes = measured.allocatedBytes,
      coordinateBytesPerMorph = from.vertexCount.toLong * 3L * java.lang.Double.BYTES.toLong,
      topologyReused = measured.value._2,
      checksum = measured.value._1
    )

  private def gridFixture(columns: Int, rows: Int): Grid =
    val coordinates = new Array[Double](columns * rows * 3)
    var row = 0
    while row < rows do
      var column = 0
      while column < columns do
        val vertex = row * columns + column
        val offset = vertex * 3
        coordinates(offset) = column.toDouble
        coordinates(offset + 1) = row.toDouble
        coordinates(offset + 2) = ((row + column) % 7).toDouble
        column += 1
      row += 1

    val faces = new Array[Int]((columns - 1) * (rows - 1) * 6)
    var offset = 0
    row = 0
    while row < rows - 1 do
      var column = 0
      while column < columns - 1 do
        val lowerLeft = row * columns + column
        val lowerRight = lowerLeft + 1
        val upperLeft = lowerLeft + columns
        val upperRight = upperLeft + 1
        faces(offset) = lowerLeft
        faces(offset + 1) = lowerRight
        faces(offset + 2) = upperLeft
        faces(offset + 3) = lowerRight
        faces(offset + 4) = upperRight
        faces(offset + 5) = upperLeft
        offset += 6
        column += 1
      row += 1
    Grid(coordinates, faces)

  private def allocationBean(): ThreadMXBean =
    ManagementFactory.getThreadMXBean match
      case bean: ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
        if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
        bean
      case _ =>
        throw new IllegalStateException("JVM thread-allocation accounting is unavailable")

  private def measure[A](bean: ThreadMXBean)(body: => A): Measurement[A] =
    val allocatedBefore = bean.getCurrentThreadAllocatedBytes
    val started = System.nanoTime()
    val value = body
    Measurement(
      value,
      System.nanoTime() - started,
      bean.getCurrentThreadAllocatedBytes - allocatedBefore
    )
