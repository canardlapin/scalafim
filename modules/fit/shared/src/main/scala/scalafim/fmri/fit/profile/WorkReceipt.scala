package scalafim.fmri.fit.profile

/** Wall time of one named phase, in nanoseconds. */
final case class PhaseTiming(phase: String, nanos: Long):
  def millis: Double = nanos / 1e6

/** A stopwatch that attributes elapsed time to named phases; portable. */
final class PhaseClock:
  private val totals = scala.collection.mutable.LinkedHashMap.empty[String, Long]
  private var current: Option[(String, Long)] = None

  def start(phase: String): Unit =
    stop()
    current = Some((phase, System.nanoTime()))

  def stop(): Unit =
    current.foreach { case (phase, t0) =>
      totals.update(phase, totals.getOrElse(phase, 0L) + (System.nanoTime() - t0))
    }
    current = None

  def timings: Vector[PhaseTiming] =
    stop()
    totals.toVector.map { case (p, n) => PhaseTiming(p, n) }

/** Repeated measurements after warm-up: raw values, median and p95. */
final case class MeasuredRuns(nanos: Vector[Long]):
  require(nanos.nonEmpty, "at least one measured run")
  private val sorted = nanos.sorted
  def median: Double = sorted(sorted.length / 2) / 1e6
  def p95: Double = sorted(math.min(sorted.length - 1, math.ceil(0.95 * sorted.length).toInt - 1).max(0)) / 1e6
  def minMillis: Double = sorted.head / 1e6

object Measure:
  /** Run `body` `warmup` times unmeasured, then `runs` times measured. */
  def repeated[A](warmup: Int, runs: Int)(body: () => A): (A, MeasuredRuns) =
    var i = 0
    while i < warmup do
      body()
      i += 1
    val nanos = Vector.newBuilder[Long]
    var last: Option[A] = None
    i = 0
    while i < runs do
      val t0 = System.nanoTime()
      last = Some(body())
      nanos += System.nanoTime() - t0
      i += 1
    (last.get, MeasuredRuns(nanos.result()))

/** The measurable receipt of one condition-fit pass: attempted and accepted
  * counts, every counted operation, phase timings, and an engine-memory
  * estimate where the platform provides one. Budgets are checked against
  * counters, never against wall time.
  */
final case class WorkReceipt(
    label: String,
    voxels: Long,
    accepted: Long,
    nodeScores: Long,
    jets: Long,
    exactEvaluations: Long,
    newtonSteps: Long,
    fallbacks: Long,
    phases: Vector[PhaseTiming],
    engineBytes: Option[Long],
    notes: Vector[String]):
  def perVoxel(value: Long): Double = if voxels == 0L then 0.0 else value.toDouble / voxels
  def acceptedFraction: Double = if voxels == 0L then 0.0 else accepted.toDouble / voxels
  def phaseMillis(phase: String): Double = phases.find(_.phase == phase).map(_.millis).getOrElse(Double.NaN)
  def totalMillis: Double = phases.map(_.millis).sum

  /** Budget violations by counter; empty when within budget. */
  def violations(budget: DecodeBudget, maxNodeScores: Int): Vector[String] =
    val out = Vector.newBuilder[String]
    if perVoxel(nodeScores) > maxNodeScores + 1e-9 then out += f"node scores ${perVoxel(nodeScores)}%.2f > $maxNodeScores"
    if perVoxel(jets) > budget.maxJets + 1e-9 then out += f"jets ${perVoxel(jets)}%.2f > ${budget.maxJets}"
    if perVoxel(exactEvaluations) > budget.maxExactEvaluations + 1e-9 then out += f"exact evaluations ${perVoxel(exactEvaluations)}%.2f > ${budget.maxExactEvaluations}"
    out.result()

  def render: String =
    val phaseText = phases.map(p => f"${p.phase}=${p.millis}%.1fms").mkString(", ")
    f"[$label] voxels=$voxels accepted=${100 * acceptedFraction}%.1f%% per-voxel: nodes=${perVoxel(nodeScores)}%.2f jets=${perVoxel(jets)}%.2f exact=${perVoxel(exactEvaluations)}%.2f newton=${perVoxel(newtonSteps)}%.2f fallbacks=$fallbacks; phases: $phaseText; engine=${engineBytes.map(b => f"${b / 1048576.0}%.1f MiB").getOrElse("n/a")}${if notes.isEmpty then "" else "; " + notes.mkString("; ")}"

object WorkReceipt:
  def from(label: String, counters: DecoderCounters, accepted: Long, clock: PhaseClock, engineBytes: Option[Long], notes: Vector[String] = Vector.empty): WorkReceipt =
    WorkReceipt(label, counters.voxels, accepted, counters.nodeScores, counters.jets, counters.exactEvaluations, counters.newtonSteps, counters.fallbacks, clock.timings, engineBytes, notes)
