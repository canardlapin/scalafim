package scalafim.surface

import scala.scalajs.js

final case class SurfaceTopologyJsReceipt(
  fixture: String,
  vertices: Int,
  faces: Int,
  elapsedMilliseconds: Double,
  observedHeapDeltaBytes: Double,
  observedRssDeltaBytes: Double,
  observedExternalDeltaBytes: Double,
  topologyVertices: Int,
  topologyFaces: Int
):
  def render: String =
    s"""{"receipt":"surface-topology-js-v1","fixture":"$fixture","vertices":$vertices,"faces":$faces,"elapsedMilliseconds":$elapsedMilliseconds,"observedHeapDeltaBytes":$observedHeapDeltaBytes,"observedRssDeltaBytes":$observedRssDeltaBytes,"observedExternalDeltaBytes":$observedExternalDeltaBytes,"topologyVertices":$topologyVertices,"topologyFaces":$topologyFaces}"""

/** Node/Scala.js construction receipt for generated realistic-scale meshes.
  * Pass `fsaverage5` or `cortical` to run one case in a fresh Node process.
  */
object SurfaceTopologyJsProbe:
  private final case class Grid(coordinates: Array[Double], faces: Array[Int])
  private final case class Memory(heap: Double, rss: Double, external: Double)

  def main(args: Array[String]): Unit =
    val selected = args.headOption
      .orElse(environmentCase)
      .getOrElse("all")
    val cases = selected match
      case "fsaverage5" => Vector(("generated-fsaverage5-scale", 129, 81))
      case "cortical"   => Vector(("generated-cortical-scale", 498, 329))
      case "all"        => Vector(
          ("generated-fsaverage5-scale", 129, 81),
          ("generated-cortical-scale", 498, 329)
        )
      case other => throw new IllegalArgumentException(s"unknown fixture '$other'")
    cases.foreach: (name, columns, rows) =>
      println(run(name, columns, rows).render)

  private def run(name: String, columns: Int, rows: Int): SurfaceTopologyJsReceipt =
    val grid = gridFixture(columns, rows)
    val before = memory()
    val started = nowMilliseconds()
    val mesh = TriangleMesh.fromArrays(grid.coordinates, grid.faces)
    val elapsed = nowMilliseconds() - started
    val after = memory()
    SurfaceTopologyJsReceipt(
      name,
      grid.coordinates.length / 3,
      grid.faces.length / 3,
      elapsed,
      math.max(0.0, after.heap - before.heap),
      math.max(0.0, after.rss - before.rss),
      math.max(0.0, after.external - before.external),
      mesh.topology.vertices.size,
      mesh.topology.faces.size
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

  private def nowMilliseconds(): Double =
    js.Dynamic.global.performance.applyDynamic("now")().asInstanceOf[Double]

  private def memory(): Memory =
    val usage = js.Dynamic.global.process.applyDynamic("memoryUsage")()
    Memory(
      usage.selectDynamic("heapUsed").asInstanceOf[Double],
      usage.selectDynamic("rss").asInstanceOf[Double],
      usage.selectDynamic("external").asInstanceOf[Double]
    )

  private def environmentCase: Option[String] =
    val process = js.Dynamic.global.selectDynamic("process")
    if js.isUndefined(process) || process == null then None
    else
      val value = process.selectDynamic("env").selectDynamic("SCALAFIM_SURFACE_TOPOLOGY_CASE")
      if js.isUndefined(value) || value == null then None
      else Some(value.asInstanceOf[String])
