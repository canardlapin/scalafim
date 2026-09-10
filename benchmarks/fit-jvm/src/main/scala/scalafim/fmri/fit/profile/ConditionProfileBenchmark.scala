package scalafim.fmri.fit.profile

import gale.linalg.DMat
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import scala.compiletime.uninitialized
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, NormalizationRule, ShapePoint}

/** ProfileHrf condition-route phases on the C0 geometry (T = 600, 300 events,
  * C = 3, six nuisance columns, AR(1) whitening), per block of `voxels`
  * responses. Each benchmark reports one phase so the post-solve, the
  * projection and the node scan are measured with honest denominators; the
  * whole per-voxel fit is the sum, and `docs/plans/profile-hrf-cohorts.md`
  * fixes the cohort these numbers are quoted against.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class ConditionProfileBenchmark:

  @Param(Array("256", "2048"))
  var voxels: Int = 0

  @Param(Array("1e-3", "1e-4"))
  var tolerance: String = ""

  private val rows = 600
  private var prep: CompactConditionPreparation = uninitialized
  private var runtime: CompactConditionRuntime = uninitialized
  private var objective: CompactConditionObjective = uninitialized
  private var whitened: Array[Double] = uninitialized
  private var column: Array[Double] = uninitialized
  private var z: Array[Double] = uninitialized
  private var qy: Array[Double] = uninitialized
  private var jet: ProfileJetBuffer = uninitialized
  private var counters: DecoderCounters = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    val family = GaussianFamily.Default
    val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(1.0))
    val step = PositiveSeconds(0.1).fold(e => throw new IllegalArgumentException(e.message), identity)
    val rng = new scala.util.Random(20260910L)
    val onsets = Vector.fill(300)(rng.nextInt((rows - 24) * 10) / 10.0).sorted.map(Seconds(_))
    val conditions = Vector.tabulate(300)(i => Vector("A", "B", "C")(i % 3))
    val term = EventTerm(events = Vector(Event.factor(conditions, "cond")), onsets = onsets, blockIds = Vector.fill(300)(0), termTag = Some("cond"))
    val basis = HrfKernelBasis.compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = tolerance.toDouble, maxRank = 48)).fold(e => throw new IllegalArgumentException(e.message), identity)
    val expanded = ExpandedConditionDesign.lower(term, frame, basis, Seconds(0.1)).fold(e => throw new IllegalArgumentException(e.message), identity)
    val whitening = WhiteningPlan.global(ArmaCoefficients.ar(0.3), Vector(TimeSegment(0, rows, 0)))
    val nuisance = DMat.tabulate(rows, 6)((t, j) =>
      val x = (t.toDouble / (rows - 1)) * 2.0 - 1.0
      j match
        case 0 => 1.0
        case 1 => x
        case 2 => x * x - 1.0 / 3.0
        case k => math.cos(math.Pi * (k - 2) * (t + 0.5) / rows))
    prep = CompactConditionPreparation.prepare(expanded, Some(whitening), Some(nuisance)).fold(e => throw new IllegalArgumentException(e.message), identity)
    val grid = NodeGrid(family.chart, Vector(15, 15))
    val budget = DecodeBudget(coarseStride = 2, maxNewtonSteps = 2, maxJets = 2, maxExactEvaluations = 6, weakSdLimit = Vector(0.5, 1.0))
    runtime = new CompactConditionRuntime(prep, grid, budget, None, 1.0, NormalizationRule.Unnormalised)
    objective = new CompactConditionObjective(prep, grid)
    // synthetic responses through the basis: shape + signed amplitudes + AR(1) noise at SNR 0.5
    val data = new Array[Double](rows * voxels)
    val signal = new Array[Double](rows)
    var v = 0
    while v < voxels do
      val point = ShapePoint.unsafe(Vector(3.5 + 4.0 * rng.nextDouble(), math.log(1.0 + 1.5 * rng.nextDouble())))
      val design = expanded.designAt(point).data
      val beta = Array.fill(3)((if rng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * rng.nextDouble()))
      var sum2 = 0.0
      var t = 0
      while t < rows do
        var acc = 0.0
        var j = 0
        while j < 3 do
          acc += design(t * 3 + j) * beta(j)
          j += 1
        signal(t) = acc
        sum2 += acc * acc
        t += 1
      val sd = math.sqrt(sum2 / rows)
      var noise = rng.nextGaussian() * 2.0 * sd
      t = 0
      while t < rows do
        if t > 0 then noise = 0.3 * noise + rng.nextGaussian() * 2.0 * sd * math.sqrt(1.0 - 0.09)
        data(t * voxels + v) = signal(t) + noise + 10.0 * sd
        t += 1
      v += 1
    whitened = prep.whiten(voxels, data).fold(e => throw new IllegalArgumentException(e.message), identity)
    column = new Array[Double](rows)
    z = new Array[Double](prep.rank)
    qy = new Array[Double](prep.nuisanceRank)
    jet = new ProfileJetBuffer(2, 3)
    counters = new DecoderCounters

  private def loadColumn(v: Int): Unit =
    var t = 0
    while t < rows do
      column(t) = whitened(t * voxels + v)
      t += 1

  /** `[U; qF]' W y` for every voxel in the block. */
  @Benchmark
  def projection(): Double =
    var acc = 0.0
    var v = 0
    while v < voxels do
      loadColumn(v)
      acc += prep.project(column, 0, z, qy)
      v += 1
    acc

  /** Exhaustive node scores over the 225-node bank for every voxel (upper bound on the hierarchical scan). */
  @Benchmark
  def nodeScan(): Double =
    var acc = 0.0
    var v = 0
    while v < voxels do
      loadColumn(v)
      val e = prep.project(column, 0, z, qy)
      objective.pointAt(z, e)
      var node = 0
      while node < objective.grid.count do
        acc += objective.scoreNode(node)
        node += 1
      v += 1
    acc

  /** One full continuous jet per voxel at a fixed interior shape. */
  @Benchmark
  def continuousJet(): Double =
    val coords = Array(5.5, math.log(1.6))
    var acc = 0.0
    var v = 0
    while v < voxels do
      loadColumn(v)
      val e = prep.project(column, 0, z, qy)
      objective.pointAt(z, e)
      objective.jetAt(coords, jet)
      acc += jet.energy
      v += 1
    acc

  /** The complete per-voxel fit: projection, hierarchical scan, decode, readout. */
  @Benchmark
  def fit(): Double =
    var acc = 0.0
    var v = 0
    while v < voxels do
      loadColumn(v)
      acc += runtime.fit(column, 0, counters).residualEnergy
      v += 1
    acc
