package scalafim.phrfcmp.exec

import java.util.concurrent.atomic.DoubleAdder

/** Source of JVM process CPU seconds (all JVM threads). Injected so tests can drive a fake clock. */
trait CpuClock:
  def processCpuSeconds(): Double

object CpuClock:
  /** Real process CPU time; falls back to the sum of all live thread CPU times if the platform bean is missing. */
  val system: CpuClock = () =>
    java.lang.management.ManagementFactory.getOperatingSystemMXBean match
      case b: com.sun.management.OperatingSystemMXBean if b.getProcessCpuTime >= 0L => b.getProcessCpuTime / 1e9
      case _ =>
        val bean = java.lang.management.ManagementFactory.getThreadMXBean
        bean.getAllThreadIds.map(id => math.max(0L, bean.getThreadCpuTime(id))).sum / 1e9

/** Cumulative CPU across resumes: prior invocations' total, plus this invocation's JVM process CPU, plus the CPU of
  * child processes reported by units (GLMsingle runs as a Python child).
  */
final class CpuMeter(clock: CpuClock, priorSeconds: Double):
  private val start = clock.processCpuSeconds()
  private val children = new DoubleAdder

  def addChild(seconds: Double): Unit = if seconds > 0.0 then children.add(seconds)
  def childSeconds: Double = children.sum
  def totalSeconds: Double = priorSeconds + math.max(0.0, clock.processCpuSeconds() - start) + children.sum
