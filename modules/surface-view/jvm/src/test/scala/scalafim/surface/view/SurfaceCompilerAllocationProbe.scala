package scalafim.surface.view

import com.sun.management.ThreadMXBean
import scalafim.surface.*

import java.lang.management.ManagementFactory

final case class SurfaceCompilerAllocationReceipt(
  fixture: String,
  vertices: Int,
  faces: Int,
  ingestionElapsedNanos: Long,
  ingestionAllocatedBytes: Long,
  compileIterations: Int,
  compileElapsedNanos: Long,
  compileAllocatedBytes: Long,
  packetReused: Boolean,
  checksum: Double
):
  def render: String =
    s"""{"receipt":"surface-compiler-allocation-v1","fixture":"$fixture","vertices":$vertices,"faces":$faces,"ingestionElapsedNanos":$ingestionElapsedNanos,"ingestionAllocatedBytes":$ingestionAllocatedBytes,"compileIterations":$compileIterations,"compileElapsedNanos":$compileElapsedNanos,"compileAllocatedBytes":$compileAllocatedBytes,"compileAllocatedBytesPerIteration":${compileAllocatedBytes.toDouble / compileIterations.toDouble},"packetReused":$packetReused,"checksum":$checksum}"""

/** Runnable allocation receipt for the mesh4s-backed renderer boundary.
  *
  * Run with:
  *
  * `sbt "surfaceViewJVM/Test/runMain scalafim.surface.view.SurfaceCompilerAllocationProbe"`
  */
object SurfaceCompilerAllocationProbe:
  private final case class Measurement[A](value: A, elapsedNanos: Long, allocatedBytes: Long)
  private final case class Grid(coordinates: Array[Double], faces: Array[Int])

  def main(args: Array[String]): Unit =
    val bean = allocationBean()
    Vector(
      ("generated-fsaverage5-scale", 129, 81, 200),
      ("generated-cortical-scale", 498, 329, 100)
    ).foreach: (name, columns, rows, iterations) =>
      println(run(name, columns, rows, iterations, bean).render)

  private def run(
    name: String,
    columns: Int,
    rows: Int,
    iterations: Int,
    bean: ThreadMXBean
  ): SurfaceCompilerAllocationReceipt =
    val grid = gridFixture(columns, rows)
    val ingestion = measure(bean):
      val mesh = TriangleMesh.fromArrays(grid.coordinates, grid.faces)
      val geometry = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Pial)
      val asset = SurfaceAsset.make(SurfaceId.unsafe(name), geometry).toOption.get
      val model = SurfaceViewerModel.make(Vector(asset), Vector.empty).toOption.get
      (asset, model)
    val (asset, model) = ingestion.value
    val initial = SurfaceViewerState.initial(model)
    val dorsal = SurfaceViewer
      .reduce(model, initial, SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal))
      .toOption
      .get

    var warmup = 0
    while warmup < 40 do
      SurfaceCompiler.compile(model, if warmup % 2 == 0 then initial else dorsal).toOption.get
      warmup += 1

    val before = SurfaceCompiler.compile(model, initial).toOption.get
    val after = SurfaceCompiler.compile(model, dorsal).toOption.get
    val packetReused =
      (before.meshes.head eq after.meshes.head) &&
        (before.meshes.head.positions eq after.meshes.head.positions) &&
        (before.meshes.head.normals eq after.meshes.head.normals) &&
        (before.meshes.head.indices eq after.meshes.head.indices)

    val repeated = measure(bean):
      var iteration = 0
      var checksum = 0.0
      while iteration < iterations do
        val state = if iteration % 2 == 0 then initial else dorsal
        val plan = SurfaceCompiler.compile(model, state).toOption.get
        checksum += plan.camera.directionX + plan.camera.directionZ
        checksum += plan.meshes.head.positions(0).toDouble
        iteration += 1
      checksum

    SurfaceCompilerAllocationReceipt(
      name,
      asset.geometry.vertexCount,
      asset.geometry.faceCount,
      ingestion.elapsedNanos,
      ingestion.allocatedBytes,
      iterations,
      repeated.elapsedNanos,
      repeated.allocatedBytes,
      packetReused,
      repeated.value
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
