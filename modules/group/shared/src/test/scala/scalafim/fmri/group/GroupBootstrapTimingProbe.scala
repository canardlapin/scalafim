package scalafim.fmri.group

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.SubjectId

/** Opt-in timing probe for bd-01M21BNZR9ZBRAYY9JD5WCQ8KX (bootstrap research
  * declaration v2, section 8). It measures the native cost of one Paule-Mandel
  * tau^2 estimate plus the modified Knapp-Hartung studentized group fit over the
  * declared design cells. It never runs by default: the platform gate must be
  * switched on explicitly (see `GroupBootstrapTimingProbeGate.howToEnable`).
  *
  * This is a measurement only. It runs no pilot, no bootstrap, and asserts
  * nothing about calibration.
  */
class GroupBootstrapTimingProbe extends munit.FunSuite:
  import GroupBootstrapTimingProbe.*

  // A full JVM sweep takes 20-40 s, near munit's 30 s default.
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(2, "h")

  test("PM + mKH per-fit cost over the declared bootstrap design cells (opt-in)"):
    assume(GroupBootstrapTimingProbeGate.enabled, GroupBootstrapTimingProbeGate.howToEnable)
    val warmup = GroupBootstrapTimingProbeGate.warmupFits
    val timed = GroupBootstrapTimingProbeGate.timedFits
    println(s"TIMING_CONFIG,${GroupBootstrapTimingProbeGate.platform},warmup=$warmup,timed=$timed,batch=$BatchSize,columns=$BootstrapColumns,pool=$PoolSize,nu=$FirstLevelDf")
    println(Row.header)
    val rows = cells.flatMap { cell =>
      val measured = measure(cell, warmup, timed)
      measured.foreach(row => println(row.csv))
      measured
    }
    println(s"TIMING_SINK,$sink")
    assertEquals(rows.length, cells.length * Path.values.length)
    assert(rows.forall(row => row.medianNs.isFinite && row.medianNs > 0.0), "every cell must yield a positive finite timing")

object GroupBootstrapTimingProbe:
  val BatchSize = 50
  val BootstrapColumns = 499
  val PoolSize = 512
  val FirstLevelDf = 8
  private val Seed = 2026093001L

  enum Design(val code: String, val contrastTerm: String):
    case Intercept extends Design("I", "intercept")
    case QuarterGroupSmooth extends Design("G", "group")

  enum Variances(val code: String):
    case Flat extends Variances("flat")
    case Spread extends Variances("spread")

  /** One path per costed operation. `DrawGeneration` is the Gaussian and
    * scaled chi-square draw work of one bootstrap replicate, without a fit.
    */
  enum Path(val code: String):
    case PublicSingle extends Path("public-1col")
    case PreparedSingle extends Path("prepared-1col")
    case PublicBatch extends Path("public-499col")
    case DrawGeneration extends Path("draws-only")

  final case class Cell(n: Int, design: Design, variances: Variances, tau2: Double):
    def id: String = s"n$n-D${design.code}-V${variances.code}-T${if tau2 == 0.0 then "0" else "2"}"

  final case class Row(
      platform: String, path: Path, cell: Cell, fits: Int, batches: Int,
      medianNs: Double, p90Ns: Double, meanNs: Double, boundaryFraction: Double
  ):
    def csv: String =
      f"TIMING,$platform,${path.code},${cell.id},${cell.n},${cell.design.code},${cell.variances.code},${cell.tau2}%.1f,$fits,$batches,$medianNs%.0f,$p90Ns%.0f,$meanNs%.0f,$boundaryFraction%.4f"

  object Row:
    val header = "TIMING,platform,path,cell,n,design,variances,tau2,fits,batches,median_ns_per_fit,p90_ns_per_fit,mean_ns_per_fit,tau2_boundary_fraction"

  val cells: Vector[Cell] =
    for
      n <- Vector(8, 20, 80)
      design <- Design.values.toVector
      variances <- Variances.values.toVector
      tau2 <- Vector(0.0, 0.2)
    yield Cell(n, design, variances, tau2)

  @volatile private var sink = 0.0

  private def value[A](result: Either[GroupError, A]): A =
    result.fold(error => throw new IllegalStateException(error.message), identity)

  /** Declaration v2 section 5: quarter-group indicator plus the smooth covariate
    * z_i = (i - .5)/n - .5, with 1-based i; the intercept-only design is a column of ones.
    */
  def designMatrix(cell: Cell): DMat =
    cell.design match
      case Design.Intercept => Matrix.tabulate(cell.n, 1)((_, _) => 1.0)
      case Design.QuarterGroupSmooth =>
        val quarter = cell.n / 4
        Matrix.tabulate(cell.n, 3) { (i, j) =>
          j match
            case 0 => 1.0
            case 1 => if i < quarter then 1.0 else 0.0
            case _ => (i + 0.5) / cell.n - 0.5
        }

  def designTerms(cell: Cell): Vector[String] =
    cell.design match
      case Design.Intercept => Vector("intercept")
      case Design.QuarterGroupSmooth => Vector("intercept", "group", "z")

  /** Declaration v2 section 5: flat sigma^2 = .52; spread sigma_i^2 = .04 * 25^((i-1)/(n-1)). */
  def trueVariances(cell: Cell): Array[Double] =
    Array.tabulate(cell.n) { i =>
      cell.variances match
        case Variances.Flat => 0.52
        case Variances.Spread => 0.04 * math.pow(25.0, i.toDouble / (cell.n - 1))
    }

  /** Model J under the null (beta = 0): y_i = u_i + e_i, v_i = sigma_i^2 chi^2_nu / nu.
    * Writes one replicate into `y` and `v`, which is exactly the per-draw work of
    * the B-plug candidate before its refit.
    */
  def draw(random: scala.util.Random, sigma2: Array[Double], tau2: Double, y: Array[Double], v: Array[Double]): Unit =
    val tau = math.sqrt(tau2)
    var i = 0
    while i < sigma2.length do
      y(i) = tau * random.nextGaussian() + math.sqrt(sigma2(i)) * random.nextGaussian()
      var chi = 0.0
      var k = 0
      while k < FirstLevelDf do
        val z = random.nextGaussian()
        chi += z * z
        k += 1
      v(i) = sigma2(i) * chi / FirstLevelDf
      i += 1

  private final case class Pool(ys: Array[Array[Double]], vs: Array[Array[Double]])

  private def pool(cell: Cell): Pool =
    val random = new scala.util.Random(Seed + cell.id.hashCode.toLong)
    val sigma2 = trueVariances(cell)
    val ys = Array.fill(PoolSize)(new Array[Double](cell.n))
    val vs = Array.fill(PoolSize)(new Array[Double](cell.n))
    var k = 0
    while k < PoolSize do
      draw(random, sigma2, cell.tau2, ys(k), vs(k))
      k += 1
    Pool(ys, vs)

  private def column(values: Array[Double]): DMat = Matrix.tabulate(values.length, 1)((i, _) => values(i))

  /** Everything a bootstrap replicate would do through the public API: wrap the
    * draw, validate data and model, prepare the design, run PM + mKH, and form
    * the studentized contrast statistic and its t(n-p) p-value.
    */
  private final class PublicFitter(cell: Cell):
    private val design = value(GroupDesign.fromMatrix(designMatrix(cell), designTerms(cell)))
    private val subjects = Vector.tabulate(cell.n)(i => SubjectId(s"s$i"))
    private val single = GroupSpace.SampleAxis(1)
    private val batch = GroupSpace.SampleAxis(BootstrapColumns)

    private def fit(space: GroupSpace, effects: DMat, variances: DMat): GroupFit =
      val data = value(GroupData.withVariances(subjects, space, "effect", effects, variances))
      val model = value(GroupModel.mixedEffects(data, design, TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung))
      value(GroupEngine.fit(model)).fit("effect").get

    def statistic(y: Array[Double], v: Array[Double]): Double =
      fit(single, column(y), column(v)).term(cell.design.contrastTerm).get.statistics(0)

    def tau2(y: Array[Double], v: Array[Double]): Double =
      fit(single, column(y), column(v)).heterogeneity.get.tau2(0)

    def batchStatistics(p: Pool, offset: Int): Double =
      val effects = Matrix.tabulate(cell.n, BootstrapColumns)((i, s) => p.ys((offset + s) % PoolSize)(i))
      val variances = Matrix.tabulate(cell.n, BootstrapColumns)((i, s) => p.vs((offset + s) % PoolSize)(i))
      val result = fit(batch, effects, variances).term(cell.design.contrastTerm).get
      var total = 0.0
      var s = 0
      while s < BootstrapColumns do
        total += result.statistics(s)
        s += 1
      total

  /** The package-internal floor: the design is prepared once and only the
    * weighted PM + mKH kernel runs per replicate.
    */
  private final class PreparedFitter(cell: Cell):
    private val prepared = value(GroupGlm.prepare(designMatrix(cell)))
    private val row = designTerms(cell).indexOf(cell.design.contrastTerm)
    private val policy = Some(TauEstimator.PauleMandel -> MetaInference.ModifiedKnappHartung)

    def statistic(y: Array[Double], v: Array[Double]): Double =
      val pieces = value(GroupGlm.meta(prepared, column(y), column(v), policy)).fit
      pieces.coefficients(row, 0) / pieces.standardErrors(row, 0)

  private def quantile(sorted: Array[Double], q: Double): Double =
    sorted(math.max(0, math.ceil(q * sorted.length).toInt - 1))

  private def summarize(
      path: Path, cell: Cell, perFit: Array[Double], fits: Int, boundary: Double
  ): Row =
    val sorted = perFit.sorted
    Row(
      GroupBootstrapTimingProbeGate.platform, path, cell, fits, perFit.length,
      quantile(sorted, 0.5), quantile(sorted, 0.9), perFit.sum / perFit.length, boundary
    )

  /** Batches of `BatchSize` single fits are timed with System.nanoTime; the
    * reported quantiles are over per-batch mean ns/fit.
    */
  private def timeSingles(warmup: Int, timed: Int)(fitOne: Int => Double): Array[Double] =
    var k = 0
    var acc = 0.0
    while k < warmup do
      acc += fitOne(k)
      k += 1
    val batches = math.max(1, timed / BatchSize)
    val perFit = new Array[Double](batches)
    var b = 0
    while b < batches do
      val start = System.nanoTime()
      var j = 0
      while j < BatchSize do
        acc += fitOne(warmup + b * BatchSize + j)
        j += 1
      perFit(b) = (System.nanoTime() - start).toDouble / BatchSize
      b += 1
    sink += acc
    perFit

  def measure(cell: Cell, warmup: Int, timed: Int): Vector[Row] =
    val p = pool(cell)
    val public = new PublicFitter(cell)
    val prepared = new PreparedFitter(cell)
    var boundaryCount = 0
    var k = 0
    while k < PoolSize do
      if public.tau2(p.ys(k), p.vs(k)) == 0.0 then boundaryCount += 1
      k += 1
    val boundary = boundaryCount.toDouble / PoolSize
    val batches = math.max(1, timed / BatchSize)

    val publicTimes = timeSingles(warmup, timed)(k => public.statistic(p.ys(k % PoolSize), p.vs(k % PoolSize)))
    val preparedTimes = timeSingles(warmup, timed)(k => prepared.statistic(p.ys(k % PoolSize), p.vs(k % PoolSize)))

    val sigma2 = trueVariances(cell)
    val random = new scala.util.Random(Seed ^ cell.id.hashCode.toLong)
    val y = new Array[Double](cell.n)
    val v = new Array[Double](cell.n)
    val drawTimes = timeSingles(warmup, timed) { _ =>
      draw(random, sigma2, cell.tau2, y, v)
      y(0) + v(0)
    }

    // One call fits BootstrapColumns replicates as sample columns.
    val calls = math.max(1, math.ceil(timed.toDouble / BootstrapColumns).toInt)
    val warmCalls = math.max(1, math.ceil(warmup.toDouble / BootstrapColumns).toInt)
    var acc = 0.0
    var c = 0
    while c < warmCalls do
      acc += public.batchStatistics(p, c * 7)
      c += 1
    val batchTimes = new Array[Double](calls)
    c = 0
    while c < calls do
      val start = System.nanoTime()
      acc += public.batchStatistics(p, c * 13)
      batchTimes(c) = (System.nanoTime() - start).toDouble / BootstrapColumns
      c += 1
    sink += acc

    Vector(
      summarize(Path.PublicSingle, cell, publicTimes, batches * BatchSize, boundary),
      summarize(Path.PreparedSingle, cell, preparedTimes, batches * BatchSize, boundary),
      summarize(Path.PublicBatch, cell, batchTimes, calls * BootstrapColumns, boundary),
      summarize(Path.DrawGeneration, cell, drawTimes, batches * BatchSize, boundary)
    )
