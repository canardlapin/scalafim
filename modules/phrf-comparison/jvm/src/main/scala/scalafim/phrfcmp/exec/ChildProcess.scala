package scalafim.phrfcmp.exec

import java.nio.file.Path
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

/** Thread pinning for child processes (design 5.2): one thread, every common BLAS/OpenMP knob at 1. */
object ThreadPinning:
  val Threads: Int = 1
  val Environment: Map[String, String] =
    Map("OMP_NUM_THREADS" -> "1", "MKL_NUM_THREADS" -> "1", "OPENBLAS_NUM_THREADS" -> "1")
  def description: String = s"threads=$Threads;" + Environment.toVector.sorted.map((k, v) => s"$k=$v").mkString(";")

/** Children never see the root pipe variables (format spec section 8); the JVM cannot unset its own environment.
  * Every process the pilot starts goes through [[scrub]], applied last, after any caller-supplied environment
  * (`run.GlmSingleBridge` uses it too). `ChildProcessSuite` scans every spawn site under `exec/` for it.
  */
object ChildEnvironment:
  val Scrubbed: Set[String] = Set(RootIntake.RootVar, RootIntake.AckVar)

  /** Removes the root pipe variables from `pb`'s environment and returns `pb`. */
  def scrub(pb: ProcessBuilder): ProcessBuilder =
    Scrubbed.foreach(k => pb.environment().remove(k): Unit)
    pb

/** Outcome of a supervised child process. `cpuSeconds` is the highest CPU total polled over the child and its
  * descendants (a lower bound on rusage; callers should take the max with any CPU the child reports itself, such
  * as the GLMsingle sidecar `cpu_s`).
  */
final case class ChildResult(
    exitCode: Option[Int],
    timedOut: Boolean,
    stdout: String,
    wallSeconds: Double,
    cpuSeconds: Double
)

/** Runs a child and enforces a hard wall-clock limit by killing it from the JVM. Python's SIGALRM is not a hard
  * limit (it is delivered only between bytecodes), so the timeout lives here: on expiry the whole process tree
  * (descendants first, then the child) is destroyed forcibly and reaped.
  */
object ChildProcess:
  def run(
      command: Seq[String],
      timeoutSeconds: Double,
      workingDir: Option[Path] = None,
      env: Map[String, String] = ThreadPinning.Environment,
      pollMillis: Long = 20L
  ): ChildResult =
    require(timeoutSeconds > 0.0, "timeout must be positive")
    val pb = new ProcessBuilder(command*).redirectErrorStream(true)
    workingDir.foreach(d => pb.directory(d.toFile))
    env.foreach((k, v) => pb.environment().put(k, v))
    ChildEnvironment.scrub(pb) // last, so a caller-supplied `env` cannot re-export the root pipe variables
    val t0 = System.nanoTime()
    val process = pb.start()
    val out = new java.io.ByteArrayOutputStream
    val reader = new Thread(() => { val _ = process.getInputStream.transferTo(out) }, "child-stdout")
    reader.setDaemon(true)
    reader.start()
    var cpu = 0.0
    def poll(): Unit =
      val handles = Iterator(process.toHandle) ++ process.descendants().iterator().asScala
      val total = handles.map(h => h.info().totalCpuDuration().toScala.map(_.toNanos / 1e9).getOrElse(0.0)).sum
      if total > cpu then cpu = total
    val deadline = t0 + (timeoutSeconds * 1e9).toLong
    var timedOut = false
    while process.isAlive && !timedOut do
      poll()
      if System.nanoTime() >= deadline then timedOut = true
      else
        val _ = process.waitFor(pollMillis, TimeUnit.MILLISECONDS)
    if timedOut then
      poll()
      killTree(process)
    val _ = process.waitFor(10, TimeUnit.SECONDS)
    reader.join(5000L)
    val exit = if timedOut || process.isAlive then None else Some(process.exitValue())
    ChildResult(exit, timedOut, new String(out.toByteArray, java.nio.charset.StandardCharsets.UTF_8),
      (System.nanoTime() - t0) / 1e9, cpu)

  /** Destroys descendants (snapshot taken first, so reparenting cannot hide them) and then the process itself. */
  def killTree(process: Process): Unit =
    val descendants = process.descendants().iterator().asScala.toVector
    descendants.foreach(h => h.destroyForcibly(): Unit)
    process.destroyForcibly(): Unit
    descendants.foreach(h => try { val _ = h.onExit().get(5, TimeUnit.SECONDS) } catch case _: Exception => ())
