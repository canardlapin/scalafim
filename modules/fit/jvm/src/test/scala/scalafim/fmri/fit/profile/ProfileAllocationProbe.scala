package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.JetLayout

/** Allocation evidence for the repeated successful reduction path. */
object ProfileAllocationProbe:
  @volatile private var sink = 0.0

  def main(args: Array[String]): Unit =
    val bean = java.lang.management.ManagementFactory.getThreadMXBean match
      case value: com.sun.management.ThreadMXBean if value.isThreadAllocatedMemorySupported => value
      case _ => throw new IllegalStateException("per-thread allocation accounting is unavailable")
    bean.setThreadAllocatedMemoryEnabled(true)
    val thread = Thread.currentThread().threadId()
    val d = 2
    val c = 3
    val comps = JetLayout.components(d)
    val s = new Array[Double](comps)
    val b = new Array[Double](comps * c)
    val g = new Array[Double](comps * c * c)
    val reduction = new ProfileReduction(d, c)
    val out = new ProfileJetBuffer(d, c)
    s(0) = 40.0
    for i <- 0 until c do
      b(i) = i + 1.0
      g(i*c+i) = i + 1.0
    def run(count: Int): Unit =
      var iteration = 0
      while iteration < count do
        if !reduction.reduce(s, b, g, out) then throw new IllegalStateException("unexpected failed reduction")
        sink = out.energy
        iteration += 1
    for sign <- Vector(1.0, -1.0) do
      s(JetLayout.second(d, 0, 0)) = 2.0 * sign
      s(JetLayout.second(d, 1, 1)) = 2.0 * sign
      run(100000)
      for block <- 0 until 3 do
        val before = bean.getThreadAllocatedBytes(thread)
        run(100000)
        val bytes = bean.getThreadAllocatedBytes(thread) - before
        require(math.abs(sink - 34.0) < 1e-12)
        require(out.curvature == (if sign > 0.0 then CurvatureStatus.PositiveDefinite else CurvatureStatus.Indefinite))
        println(s"profile-allocation curvatureSign=$sign block=$block bytesPerReduction=${bytes.toDouble/100000} energy=$sink")
