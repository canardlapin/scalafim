package scalafim.fmri.mvpa

/** Bounded allocation evidence, not a timing benchmark or an automated gate.
  * Run with mvpaJVM/Test/runMain scalafim.fmri.mvpa.RsaAllocationProbe.
  */
object RsaAllocationProbe:
  @volatile private var sink = 0.0

  def main(args: Array[String]): Unit =
    val bean = java.lang.management.ManagementFactory.getThreadMXBean match
      case value: com.sun.management.ThreadMXBean if value.isThreadAllocatedMemorySupported => value
      case _ => throw new IllegalStateException("per-thread allocation accounting is unavailable")
    bean.setThreadAllocatedMemoryEnabled(true)
    // JDK 14+ API; Thread.threadId() would require JDK 19 and CI pins JDK 17.
    def allocated = bean.getCurrentThreadAllocatedBytes()
    for (items, count) <- Vector((5, 2), (12, 4), (20, 8)) do
      val labels = Vector.tabulate(items)(i => s"item-$i")
      val distances = items * (items - 1) / 2
      val controls = Vector.tabulate(count) { column =>
        val values = Vector.tabulate(distances)(row => math.cos(2*math.Pi*(column+1)*row/distances))
        RdmModel.unsafe(s"control-$column", labels, RdmVector.unsafe(items, values))
      }
      val scorer = RdmScorer.PartialPearson.unsafe(controls)
      val x = RdmVector.unsafe(items, Vector.tabulate(distances)(i => math.sin(2*math.Pi*i/distances)))
      val y = RdmVector.unsafe(items, Vector.tabulate(distances)(i => math.sin(2*math.Pi*i/distances) + math.sin(4*math.Pi*i/distances)))
      def run(iterations: Int): Unit =
        var i = 0
        while i < iterations do
          sink = scorer.score(labels, x, y).fold(error => throw new IllegalStateException(error.message), identity)
          i += 1
      run(10000)
      for block <- 0 until 3 do
        val before = allocated
        run(5000)
        val bytes = allocated - before
        require(math.abs(sink - 1/math.sqrt(2.0)) <= 1e-12, s"unexpected partial correlation: $sink")
        println(s"rsa-allocation items=$items distances=$distances controls=$count block=$block bytesPerScore=${bytes.toDouble/5000} score=$sink")
