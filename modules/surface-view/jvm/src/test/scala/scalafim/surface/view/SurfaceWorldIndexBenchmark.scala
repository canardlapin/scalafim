package scalafim.surface.view

import scalafim.image.WorldPoint
import scalafim.surface.*
import com.sun.management.ThreadMXBean
import java.lang.management.ManagementFactory

/** Synthetic cardinalities, not anatomical fixtures. Run via Test/runMain. */
object SurfaceWorldIndexBenchmark:
  @volatile private var checksum = 0L
  def main(args: Array[String]): Unit =
    val bean = ManagementFactory.getThreadMXBean match
      case value: ThreadMXBean if value.isThreadAllocatedMemorySupported =>
        if !value.isThreadAllocatedMemoryEnabled then value.setThreadAllocatedMemoryEnabled(true)
        value
      case _ => throw new IllegalStateException("per-thread allocation accounting is unavailable")
    // JDK 14+ API; Thread.threadId() would require JDK 19 and CI pins JDK 17.
    def allocated = bean.getCurrentThreadAllocatedBytes()
    for count <- Vector(10449, 163842) do
      val coordinates = Array.tabulate(count * 3)(i =>
        val v = i / 3
        i % 3 match
          case 0 => math.sin(v * 0.137) * 80.0
          case 1 => math.cos(v * 0.193) * 70.0
          case _ => math.sin(v * 0.071) * 60.0)
      val geometry = SurfaceGeometry(TriangleMesh.fromArrays(coordinates, Array(0, 1, 2)), Hemisphere.Left, SurfaceKind.Pial)
      val id = SurfaceId.unsafe("synthetic")
      val radius = SurfaceLinkRadius.unsafe(Double.MaxValue)
      val queries = Vector.tabulate(1000)(i => WorldPoint(math.sin(i * 0.37) * 90, math.cos(i * 0.29) * 80, math.sin(i * 0.11) * 70))
      def scan(q: WorldPoint): Int =
        var best = 0
        var squared = Double.PositiveInfinity
        var vertex = 0
        while vertex < count do
          val x = coordinates(vertex * 3) - q.x
          val y = coordinates(vertex * 3 + 1) - q.y
          val z = coordinates(vertex * 3 + 2) - q.z
          val d = x * x + y * y + z * z
          if d < squared then
            best = vertex
            squared = d
          vertex += 1
        best
      SurfaceWorldLink.prepare(id, geometry).toOption.get
      val a0 = allocated
      val t0 = System.nanoTime()
      val index = SurfaceWorldLink.prepare(id, geometry).toOption.get
      val buildNs = System.nanoTime() - t0
      val buildBytes = allocated - a0
      queries.take(300).foreach(q =>
        checksum += scan(q)
        checksum += index.nearestVertex(q, radius).toOption.get.vertex.index)
      queries.foreach(q => require(scan(q) == index.nearestVertex(q, radius).toOption.get.vertex.index))
      def measure(f: WorldPoint => Int): (Long, Long) =
        val a = allocated
        val t = System.nanoTime()
        queries.foreach(q => checksum += f(q))
        (System.nanoTime() - t, allocated - a)
      val (scanNs, scanBytes) = measure(scan)
      val (queryNs, queryBytes) = measure(q => index.nearestVertex(q, radius).toOption.get.vertex.index)
      val saving = (scanNs - queryNs).toDouble / queries.size
      val crossover = if saving > 0 then math.ceil(buildNs / saving).toLong else -1L
      println(s"WORLD_INDEX vertices=$count queries=${queries.size} build_ns=$buildNs build_bytes=$buildBytes retained_primitive_bytes=${index.retainedBytes} scan_ns=$scanNs scan_bytes=$scanBytes indexed_ns=$queryNs indexed_bytes=$queryBytes crossover_queries=$crossover checksum=$checksum")
