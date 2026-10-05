package scalafim.phrfcmp.exec

/** Round-robin dispatch: index 0 of every cell, then index 1 of every cell, and so on (design 5.2, F9). */
object Dispatch:
  def order(cells: Vector[PilotCell], datasets: Int): Vector[Job] =
    require(datasets >= 0, "datasets must be non-negative")
    (0 until datasets).toVector.flatMap(d => cells.map(c => Job(c.id, d)))

  /** The probe wave (first `probe` datasets of every cell) and the remainder, both in dispatch order. */
  def waves(order: Vector[Job], probe: Int): (Vector[Job], Vector[Job]) = order.partition(_.dataset < probe)

  /** Jobs a resume finishes even past the soft stop (design 5.2: the runner "finishes in-flight datasets"): every
    * incomplete job, in any cell, whose index is below the highest index completed by an earlier invocation. Such a
    * job was dispatched before the interruption (dispatch is round-robin in index order) and abandoned by a crash or
    * a hard stop; without it a cell's completed set has a hole and the kept prefix shrinks (decision D2).
    */
  def mustFinish(order: Vector[Job], completed: Set[Job]): Set[Job] =
    completed.iterator.map(_.dataset).maxOption match
      case None => Set.empty
      case Some(top) => order.iterator.filter(j => j.dataset < top && !completed.contains(j)).toSet

/** Retry cap: at most `maxRetries` retries after the first attempt (design 5.2: 2). Refusals are never retried. */
final case class RetryPolicy(maxRetries: Int = 2):
  require(maxRetries >= 0, "maxRetries must be non-negative")

  /** What to do with an attempt's result given the retries already recorded for the unit. */
  def decide(priorRetries: Int, result: ArmResult): RetryPolicy.Step = result match
    case ArmResult.Done(_) => RetryPolicy.Step.Commit(UnitStatus.Done, "")
    case ArmResult.Refused(code, _) => RetryPolicy.Step.Commit(UnitStatus.Refused, code)
    case ArmResult.Failed(code, _) =>
      if priorRetries < maxRetries then RetryPolicy.Step.Retry(code) else RetryPolicy.Step.Commit(UnitStatus.Failed, code)

object RetryPolicy:
  enum Step:
    case Commit(status: UnitStatus, code: String)
    case Retry(code: String)

/** Cumulative CPU guard: soft stop at 45 core-hours, hard ceiling at 60 (design 5.1, 5.2).
  *
  * The runner checks the hard ceiling before every attempt of every unit, whenever an arm polls
  * `ArmContext.shouldAbort`, and after every commit. An attempt that ends after the ceiling tripped is discarded,
  * not committed. Overshoot bound: CPU past the ceiling is at most `threads` times the CPU of the longest single
  * unit attempt (every in-flight attempt may run to its end when its arm does not poll `shouldAbort`), plus the
  * child CPU those attempts report.
  */
final case class CpuGuard(softCoreHours: Double = 45.0, hardCoreHours: Double = 60.0):
  require(softCoreHours > 0.0 && softCoreHours <= hardCoreHours, "need 0 < soft <= hard")

  def check(totalCpuSeconds: Double): CpuGuard.State =
    val hours = totalCpuSeconds / 3600.0
    if hours >= hardCoreHours then CpuGuard.State.HardStop
    else if hours >= softCoreHours then CpuGuard.State.SoftStop
    else CpuGuard.State.Ok

object CpuGuard:
  enum State:
    case Ok, SoftStop, HardStop

  /** Linear projection of total CPU from the completed jobs, in core-hours. */
  def projectCoreHours(totalCpuSeconds: Double, completedJobs: Int, totalJobs: Int): Double =
    if completedJobs <= 0 then 0.0 else totalCpuSeconds / completedJobs * totalJobs / 3600.0

/** Chi-square quantile and the 80 % UCL factor `sqrt(df / chi2_{0.20, df})` (design 3.3). Pure, JVM and JS. */
object Ucl:
  private val Lanczos = Array(
    676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059,
    12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
  )

  def lnGamma(x: Double): Double =
    if x < 0.5 then math.log(math.Pi / math.abs(math.sin(math.Pi * x))) - lnGamma(1.0 - x)
    else
      val xm = x - 1.0
      var a = 0.99999999999980993
      var i = 0
      while i < 8 do
        a += Lanczos(i) / (xm + i + 1.0)
        i += 1
      val t = xm + 7.5
      0.5 * math.log(2.0 * math.Pi) + (xm + 0.5) * math.log(t) - t + math.log(a)

  /** Regularized lower incomplete gamma P(a, x). */
  def gammaP(a: Double, x: Double): Double =
    if x <= 0.0 then 0.0
    else if x < a + 1.0 then
      var sum = 1.0 / a
      var term = sum
      var n = 1
      while n < 1000 && math.abs(term) > math.abs(sum) * 1e-16 do
        term *= x / (a + n)
        sum += term
        n += 1
      sum * math.exp(-x + a * math.log(x) - lnGamma(a))
    else
      val tiny = 1e-300
      var b = x + 1.0 - a
      var c = 1.0 / tiny
      var d = 1.0 / b
      var h = d
      var i = 1
      var done = false
      while i < 1000 && !done do
        val an = -i * (i - a)
        b += 2.0
        d = an * d + b
        if math.abs(d) < tiny then d = tiny
        c = b + an / c
        if math.abs(c) < tiny then c = tiny
        d = 1.0 / d
        val delta = d * c
        h *= delta
        done = math.abs(delta - 1.0) < 1e-16
        i += 1
      1.0 - math.exp(-x + a * math.log(x) - lnGamma(a)) * h

  def chiSquareCdf(x: Double, df: Int): Double = gammaP(df / 2.0, x / 2.0)

  /** Lower-tail quantile by bisection (monotone cdf, so no failure mode). */
  def chiSquareQuantile(p: Double, df: Int): Double =
    require(p > 0.0 && p < 1.0 && df >= 1, "need 0 < p < 1 and df >= 1")
    var hi = df.toDouble.max(1.0)
    while chiSquareCdf(hi, df) < p do hi *= 2.0
    var lo = 0.0
    var i = 0
    while i < 200 do
      val mid = 0.5 * (lo + hi)
      if chiSquareCdf(mid, df) < p then lo = mid else hi = mid
      i += 1
    0.5 * (lo + hi)

  def df(completedDatasets: Int): Int = completedDatasets - 1

  /** The 80 % UCL multiplier on sigma for `df` degrees of freedom (about 1.18 at 19). */
  def factor(df: Int): Double = math.sqrt(df / chiSquareQuantile(0.20, df))

/** The accepted partial (or full) pilot: every cell keeps the dataset indices `0 until D`. */
final case class PartialDecision(
    D: Int,
    kept: Map[CellId, Vector[Int]],
    dropped: Map[CellId, Vector[Int]],
    df: Int,
    uclFactor: Double
):
  def isFull: Boolean = dropped.values.forall(_.isEmpty)

/** Partial-pilot rule (F9, D2): longest completed prefix, uniform reverse-index drop, never selective, refuse below
  * `MinDatasets`.
  */
object PartialPilot:
  val MinDatasets: Int = 15

  /** Length of the longest completed prefix `0, 1, ..., k-1` of one cell. */
  def prefixLength(completed: Set[Int]): Int = Iterator.from(0).takeWhile(completed.contains).length

  /** `completed` holds, per cell, the dataset indices whose every arm is terminal. `D` is the smallest per-cell
    * longest completed prefix (decision D2): every cell keeps exactly `0 until D` and drops every other completed
    * index, highest first, whatever any result says. A hole (an incomplete index below completed ones) is never kept:
    * the kept set is then a function of the dispatch order alone, the scorer's corpus requires indices `0 until D`
    * in order, and the drop stays "from the highest index down". df and the UCL factor are recomputed from `D`.
    */
  def decide(completed: Map[CellId, Set[Int]], minDatasets: Int = MinDatasets): Either[PilotRefusal, PartialDecision] =
    if completed.isEmpty then Left(PilotRefusal.TooFewDatasets(0, minDatasets))
    else
      val d = completed.values.map(prefixLength).min
      if d < minDatasets || d < 2 then Left(PilotRefusal.TooFewDatasets(d, minDatasets))
      else
        val kept = completed.view.mapValues(_ => (0 until d).toVector).toMap
        val dropped = completed.view.mapValues(_.filter(_ >= d).toVector.sorted.reverse).toMap
        val dof = Ucl.df(d)
        Right(PartialDecision(d, kept, dropped, dof, Ucl.factor(dof)))
