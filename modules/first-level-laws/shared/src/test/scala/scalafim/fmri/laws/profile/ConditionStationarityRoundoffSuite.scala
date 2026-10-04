package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, JetLayout, ShapePoint}

/** Fixed development cases exposing the distinction between rounded SSE and residual differences. */
class ConditionStationarityRoundoffSuite extends munit.FunSuite:
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

  private def cohort(snr: Double, seed: Long, voxels: Int): Array[Double] =
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

  test("export fixed development comparison inputs without changing the decoder"):
    val n = 32
    val data = prep.whiten(n, cohort(0.5, 102L, n)).fold(e => fail(e.message), identity)
    val grid = NodeGrid(GaussianFamily.Default.chart, Vector(15, 15))
    val objective = new CompactConditionObjective(prep, grid)
    val basis = prep.basis
    val components = prep.family.jetComponents
    val kernels = new Array[Double](components * basis.fineCount)
    val coefficients = new Array[Double](components * basis.rank)
    val assembly = new CompactConditionJets(prep.rHat, prep.rank, prep.conditions, basis.rank, prep.family.dimension)
    val cases = Vector(
      (9, Vector(7.49004329853202, 0.3926858591878316), Vector(7.490043325179904, 0.3926858791205834)),
      (31, Vector(5.119960070064869, 0.3496555300787389), Vector(5.119960057433809, 0.3496555391439763))
    )
    def array(values: Array[Double]): String = values.mkString("[", ",", "]")
    def emit(value: => String): Unit =
      if ConditionC0QualificationPlatform.executionSourceId.nonEmpty then println(value)
    val phi = Array.tabulate(basis.rank * basis.fineCount)(i => basis.value(i / basis.fineCount, i % basis.fineCount))
    emit(s"""{"kind":"roundoff-basis","basisRank":${basis.rank},"rank":${prep.rank},"lags":${array(basis.lags)},"phi":${array(phi)},"rHat":${array(prep.rHat)}}""")
    for (voxel, previous, candidate) <- cases do
      val column = Array.tabulate(rows)(r => data(r * n + voxel))
      val z = new Array[Double](prep.rank)
      val qy = new Array[Double](prep.nuisanceRank)
      val energy = prep.project(column, 0, z, qy)
      objective.pointAt(z, energy)
      for (name, coordinates) <- Vector("previous" -> previous, "candidate" -> candidate) do
        val out = new ProfileJetBuffer(2, 3)
        assert(objective.jetAt(coordinates.toArray, out))
        basis.coefficientJetInto(ShapePoint.unsafe(coordinates), kernels, coefficients, components)
        assembly.assemble(z, energy, coefficients, components)
        val design = assembly.valueDesign
        emit(s"""{"kind":"roundoff-input","voxel":$voxel,"point":"$name","coordinates":${array(coordinates.toArray)},"rows":${prep.rank},"cols":3,"energy":$energy,"profileEnergy":${out.energy},"response":${array(z)},"design":${array(design)},"amplitudes":${array(out.amplitudes)},"kernel":${array(kernels.take(basis.fineCount))},"coefficients":${array(coefficients.take(basis.rank))},"gradient":${array(out.gradient)},"hessian":${array(out.hessian)}}""")
        assert(out.energy.isFinite)
        if name == "candidate" then assert(out.gradient.forall(g => math.abs(g) < 1e-8))
