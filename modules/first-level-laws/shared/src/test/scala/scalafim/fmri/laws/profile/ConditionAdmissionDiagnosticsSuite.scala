package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, JetLayout, ShapePoint}

/** Frozen Gaussian C0 execution trace; passing records observations only. */
class ConditionAdmissionDiagnosticsSuite extends munit.FunSuite:
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(60, "min")
  private val rows = 600
  private val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(1.0))
  private val precision = Seconds(0.1)
  private val step = PositiveSeconds(0.1).fold(e => fail(e.message), identity)
  private val whitening = WhiteningPlan.global(ArmaCoefficients.ar(0.3), Vector(TimeSegment(0, rows, 0)))
  private val nuisance = DMat.tabulate(rows, 6): (t, j) =>
    val x = 2.0 * t / (rows - 1.0) - 1.0
    j match
      case 0 => 1.0
      case 1 => x
      case 2 => x * x - 1.0 / 3.0
      case k => math.cos(math.Pi * (k - 2) * (t + 0.5) / rows)
  private val schedule =
    val rng = new scala.util.Random(20260910L)
    EventTerm(
      events = Vector(Event.factor(Vector.tabulate(300)(i => Vector("A", "B", "C")(i % 3)), "cond")),
      onsets = Vector.fill(300)(rng.nextInt((rows - 24) * 10) / 10.0).sorted.map(Seconds(_)),
      blockIds = Vector.fill(300)(0),
      termTag = Some("cond")
    )
  private lazy val prep =
    val basis = HrfKernelBasis.compile(KernelBasisSpec(GaussianFamily.Default, step, Vector(26, 21), 1e-3, 48)).fold(e => fail(e.message), identity)
    val expanded = ExpandedConditionDesign.lower(schedule, frame, basis, precision).fold(e => fail(e.message), identity)
    val midpoint = ShapePoint.unsafe(Vector(5.25, math.log(1.55)))
    val admission = ObservedFamilyCertification.admitForCompact(expanded, schedule, frame, precision, Some(whitening), Some(nuisance), Vector(midpoint), ObservedFamilyRequirements(1e-2, 1e8, 1e-6)).fold(e => fail(e.message), identity)
    CompactConditionPreparation.prepare(expanded, admission, Some(whitening), Some(nuisance), schedule, frame, precision).fold(e => fail(e.message), identity)
  private val budget = DecodeBudget(coarseStride = 2, maxNewtonSteps = 6, maxJets = 8, maxExactEvaluations = 2, weakSdLimit = Vector(0.5, 1.0), stationarityStepTolerance = 1e-9)

  private def diagnosticNumber(value: Double): String =
    if value.isNaN then "NaN"
    else if value == Double.PositiveInfinity then "Infinity"
    else if value == Double.NegativeInfinity then "-Infinity"
    else java.lang.Double.toHexString(value)

  private def diagnosticValues(value: Array[Double]): String =
    value.iterator.map(diagnosticNumber).map(token => s"\"${token}\"").mkString("[", ",", "]")

  private def diagnosticTrace(events: Vector[String]): String =
    events.mkString("[", ",", "]")

  private final class Observer(val underlying: ShapeObjective) extends ShapeObjective:
    private val events = scala.collection.mutable.ArrayBuffer.empty[String]
    var nodeScores = 0L
    var nodeJets = 0L
    var jets = 0L
    var exact = 0L
    def grid: NodeGrid = underlying.grid
    def amplitudeCount: Int = underlying.amplitudeCount
    def reset(): Unit =
      events.clear()
      nodeScores = 0L
      nodeJets = 0L
      jets = 0L
      exact = 0L
    def trace: Vector[String] = events.toVector
    private def coordinates(value: Array[Double]): String = diagnosticValues(value)
    private def jet(kind: String, coordinates: String, ok: Boolean, out: ProfileJetBuffer): Unit =
      events += s"{\"kind\":\"$kind\",\"coordinates\":$coordinates,\"ok\":$ok,\"energy\":\"${diagnosticNumber(out.energy)}\",\"gradient\":${diagnosticValues(out.gradient)},\"hessian\":${diagnosticValues(out.hessian)},\"amplitudes\":${diagnosticValues(out.amplitudes)},\"curvature\":\"${out.curvature}\"}"
    def scoreNode(node: Int): Double =
      nodeScores += 1L
      underlying.scoreNode(node)
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      nodeJets += 1L
      val ok = underlying.jetAtNode(node, out)
      jet("nodeJet", s"[\"$node\"]", ok, out)
      ok
    def jetAt(coords: Array[Double], out: ProfileJetBuffer): Boolean =
      jets += 1L
      val ok = underlying.jetAt(coords, out)
      jet("jet", coordinates(coords), ok, out)
      ok
    def energyAt(coords: Array[Double], out: ProfileJetBuffer): Double =
      exact += 1L
      val value = underlying.energyAt(coords, out)
      jet("exact", coordinates(coords), true, out)
      value

  private def cohort(snr: Double, seed: Long, voxels: Int = 200): Array[Double] =
    val rng = new scala.util.Random(seed)
    val data = new Array[Double](rows * voxels)
    val nuis = new Array[Double](rows * 6); nuisance.copyRowMajorTo(nuis)
    val chart = GaussianFamily.Default.chart
    val scale = new Array[Double](GaussianFamily.Default.jetComponents)
    val signal = new Array[Double](rows)
    var voxel = 0
    while voxel < voxels do
      val point = ShapePoint.unsafe(Vector(
        chart.lower(0) + 0.5 + rng.nextDouble() * (chart.width(0) - 1.0),
        chart.lower(1) + 0.2 + rng.nextDouble() * (chart.width(1) - 0.4)
      ))
      val beta = Array.fill(3)((if rng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * rng.nextDouble()))
      GaussianFamily.Default.scaleJetInto(GaussianFamily.Default.libraryNormalization, point, scale)
      val design = schedule.convolve(GaussianFamily.Default.toHrf(point), frame, precision = precision).data.data.map(_ / scale(JetLayout.Value))
      var sum = 0.0; var sum2 = 0.0; var t = 0
      while t < rows do
        var value = 0.0
        var c = 0
        while c < 3 do
          value += design(t * 3 + c) * beta(c)
          c += 1
        signal(t) = value
        sum += value
        sum2 += value * value
        t += 1
      val sd = math.sqrt(math.max(1e-12, sum2 / rows - (sum / rows) * (sum / rows)))
      val innovation = sd / snr * math.sqrt(1.0 - 0.09)
      var noise = rng.nextGaussian() * sd / snr; t = 0
      while t < rows do
        if t > 0 then noise = 0.3 * noise + rng.nextGaussian() * innovation
        var drift = 0.0
        var nuisanceCol = 0
        while nuisanceCol < 6 do
          drift += nuis(t * 6 + nuisanceCol) * (if nuisanceCol == 0 then 10.0 else 0.3) * sd
          nuisanceCol += 1
        data(t * voxels + voxel) = signal(t) + noise + drift
        t += 1
      voxel += 1
    data

  test("Gaussian source cohort terminates and preserves its voxel prefix"):
    val one = cohort(1.0, 101L, voxels = 1)
    val two = cohort(1.0, 101L, voxels = 2)
    assertEquals(one.length, rows)
    var row = 0
    while row < rows do
      assertEqualsDouble(one(row), two(row * 2), 0.0)
      row += 1

  test("diagnostic traces keep hexadecimal values quoted and event objects raw"):
    assertEquals(
      diagnosticValues(Array(1.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)),
      """["0x1.0p0","NaN","Infinity","-Infinity"]"""
    )
    val event = """{"kind":"jet","coordinates":["0x1.0p0"]}"""
    assertEquals(diagnosticTrace(Vector(event)), s"[$event]")

  test("records all frozen Gaussian C0 decoder exits without changing work"):
    for (snr, seed) <- Vector(1.0 -> 101L, 0.5 -> 102L) do
      val whitened = prep.whiten(200, cohort(snr, seed)).fold(e => fail(e.message), identity)
      val base = new CompactConditionObjective(prep, NodeGrid(GaussianFamily.Default.chart, Vector(15, 15)))
      val observed = new Observer(base)
      val decoder = new ShapeDecoder(observed, budget, None, 1.0)
      val z = new Array[Double](prep.rank); val qy = new Array[Double](prep.nuisanceRank)
      val column = new Array[Double](rows)
      val statuses = scala.collection.mutable.Map.empty[DecodeStatus, Int].withDefaultValue(0)
      var voxel = 0
      while voxel < 200 do
        var row = 0
        while row < rows do
          column(row) = whitened(row * 200 + voxel)
          row += 1
        val energy = prep.project(column, 0, z, qy)
        base.pointAt(z.clone(), energy)
        observed.reset()
        val counters = new DecoderCounters
        val result = decoder.decode(counters)
        statuses(result.status) += 1
        if result.status != DecodeStatus.Accepted then
          println(s"{\"family\":\"Gaussian\",\"snr\":$snr,\"voxel\":$voxel,\"status\":\"${result.status}\",\"budgetExit\":\"${result.budgetExit}\",\"jets\":${counters.jets},\"exact\":${counters.exactEvaluations},\"candidates\":${counters.candidateAttempts},\"trace\":${diagnosticTrace(observed.trace)}}")
        voxel += 1
      assertEquals(statuses.values.sum, 200)
      println(s"[diagnostic-statuses] family=Gaussian snr=$snr counts=${statuses.toVector.sortBy(_._1.toString).mkString(",")}")
