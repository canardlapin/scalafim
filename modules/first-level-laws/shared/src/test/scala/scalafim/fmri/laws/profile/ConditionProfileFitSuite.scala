package scalafim.fmri.laws.profile

import gale.linalg.DMat
import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.{Event, EventModel, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, JetLayout, NormalizationRule, ShapePoint}
import scalafim.fmri.model.{FitPlan, FmriModel}
import scalafim.image.SampleSpaces

/** PHRF-12: the condition profile fit composed over a real `FitPlan`, checked
  * against the compact runtime on the same data and against a direct oracle.
  */
class ConditionProfileFitSuite extends munit.FunSuite:

  private val family = GaussianFamily.Default
  private val step = PositiveSeconds(0.1).fold(e => fail(e.message), identity)
  private val precision = Seconds(0.1)
  private val rows = 200
  private val voxels = 20
  private val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(1.0))
  private val rng0 = new scala.util.Random(4040L)
  private val events = 30
  private val onsets = Vector.fill(events)(rng0.nextInt(1800) / 10.0).sorted.map(Seconds(_))
  private val conditions = Vector.tabulate(events)(i => Vector("A", "B", "C")(i % 3))
  private val term = EventTerm(events = Vector(Event.factor(conditions, "cond")), onsets = onsets, blockIds = Vector.fill(events)(0), termTag = Some("cond"))
  private lazy val basis = HrfKernelBasis.compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-4, maxRank = 40)).fold(e => fail(e.message), identity)
  private lazy val convolved = term.convolve(basis.kernel, frame, precision = precision)
  private lazy val eventModel = EventModel.build(Vector(convolved), frame)
  private lazy val baseline = BaselineModel.build(samplingFrame = frame, basis = BaselineBasis.Poly, degree = 2, intercept = Intercept.Global)

  private lazy val truth: (Array[Double], Array[Double], Array[Double]) =
    val rng = new scala.util.Random(99L)
    val tau = Array.fill(voxels)(3.5 + 4.0 * rng.nextDouble())
    val logSd = Array.fill(voxels)(math.log(1.0) + rng.nextDouble() * (math.log(2.5) - math.log(1.0)))
    val beta = Array.fill(voxels * 3)((if rng.nextBoolean() then 1.0 else -1.0) * (0.5 + 1.5 * rng.nextDouble()))
    (tau, logSd, beta)

  /** Time-major rows x voxels data: family signal + white noise at SNR 1 + drift. */
  private lazy val data: Array[Double] =
    val (tau, logSd, beta) = truth
    val rng = new scala.util.Random(7L)
    val out = new Array[Double](rows * voxels)
    val scale = new Array[Double](family.jetComponents)
    var v = 0
    while v < voxels do
      val point = ShapePoint.unsafe(Vector(tau(v), logSd(v)))
      family.scaleJetInto(family.libraryNormalization, point, scale)
      val design = term.convolve(family.toHrf(point), frame, precision = precision).data
      val signal = new Array[Double](rows)
      var sum = 0.0
      var sum2 = 0.0
      var t = 0
      while t < rows do
        var acc = 0.0
        var j = 0
        while j < 3 do
          acc += design.data(t * 3 + j) / scale(JetLayout.Value) * beta(v * 3 + j)
          j += 1
        signal(t) = acc
        sum += acc
        sum2 += acc * acc
        t += 1
      val sd = math.sqrt(math.max(1e-12, sum2 / rows - (sum / rows) * (sum / rows)))
      t = 0
      while t < rows do
        val drift = 10.0 * sd + 0.3 * sd * (t.toDouble / rows - 0.5) + 0.2 * sd * math.pow(t.toDouble / rows - 0.5, 2)
        out(t * voxels + v) = signal(t) + rng.nextGaussian() * sd + drift
        t += 1
      v += 1
    out

  private lazy val dataset: SynchronousFmriDataset =
    val b = DMat.newBuilder(rows, voxels)
    var i = 0
    while i < data.length do
      b.writeLinear(i, data(i))
      i += 1
    FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("phrf12"), b.result(), SampleSpaces(Vector(voxels, 1, 1))), frame)

  private lazy val plan: FitPlan = FitPlan(FmriModel(eventModel, baseline, dataset))

  private def policy(output: OutputRequest): ConditionProfilePolicy =
    val structure = ConditionProfileFit.structureFor(plan, convolved).fold(e => fail(e.message), identity)
    ConditionProfilePolicy(basis, structure, Vector(15, 15), DecodeBudget(coarseStride = 2, maxNewtonSteps = 2, maxJets = 2, maxExactEvaluations = 6, weakSdLimit = Vector(0.5, 1.0)), None, 1.0, output, blockSize = 8)

  private final class CollectingSink extends BlockSink[ConditionProfileBlock, ConditionProfileReceipt]:
    val blocks = Vector.newBuilder[ConditionProfileBlock]
    def accept(block: VoxelBlock, payload: ConditionProfileBlock): Either[String, ConditionProfileReceipt] =
      blocks += payload
      Right(ConditionProfileReceipt(payload.ordinal, payload.results.length, payload.results.count(_.status == DecodeStatus.Accepted)))

  test("the structure maps the plan's task columns by condition and basis"):
    val structure = ConditionProfileFit.structureFor(plan, convolved).fold(e => fail(e.message), identity)
    assertEquals(structure.conditionCount, 3)
    assertEquals(structure.basisSize, basis.rank)

  test("the Gram route agrees with the compact route and the direct oracle, streaming blocks in order"):
    val queries = Vector(SignedQuery.make("A-B", Vector(1.0, -1.0, 0.0), 1e-6).fold(e => fail(e.message), identity))
    val prep = ConditionProfileFit.prepare(plan, policy(OutputRequest.ConditionQueries(queries, NormalizationRule.Unnormalised))).fold(e => fail(e.message), identity)
    val sink = new CollectingSink
    val (receipts, counters) = prep.run(dataset, sink).fold(e => fail(e.message), identity)
    val blocks = sink.blocks.result()
    assertEquals(blocks.map(_.ordinal), blocks.indices.toVector)
    assertEquals(receipts.map(_.voxels).sum, voxels)
    val results = blocks.flatMap(_.results).sortBy(_.voxel)
    assertEquals(results.map(_.voxel), (0 until voxels).toVector)
    assert(counters.perVoxel(counters.jets) <= 2.0 + 1e-9)
    assert(counters.perVoxel(counters.nodeScores) <= 90.0)

    // Compact route on the same data: nuisance = the plan's baseline columns, no whitening.
    val expanded = ExpandedConditionDesign.lower(term, frame, basis, precision).fold(e => fail(e.message), identity)
    val model = plan.model
    val taskNames = convolved.columnNames.toSet
    val nuisanceCols = model.columnNames.indices.filterNot(i => taskNames.contains(model.columnNames(i))).toVector
    val nuisance = DMat.tabulate(rows, nuisanceCols.length)((t, j) => model.designMatrix(t, nuisanceCols(j)))
    val compactPrep = CompactConditionPreparation.prepare(expanded, None, Some(nuisance)).fold(e => fail(e.message), identity)
    val runtime = new CompactConditionRuntime(compactPrep, NodeGrid(family.chart, Vector(15, 15)), prep.policy.budget, None, 1.0, NormalizationRule.Unnormalised)
    val column = new Array[Double](rows)
    val (tau, _, _) = truth
    var accepted = 0
    var v = 0
    while v < voxels do
      var t = 0
      while t < rows do
        column(t) = data(t * voxels + v)
        t += 1
      val compact = runtime.fit(column, 0, new DecoderCounters)
      val gramRoute = results(v)
      assertEqualsDouble(gramRoute.coordinates(0), compact.decode.coordinates(0), 1e-6, s"tau voxel $v")
      assertEqualsDouble(gramRoute.coordinates(1), compact.decode.coordinates(1), 1e-6, s"logSd voxel $v")
      var j = 0
      while j < 3 do
        assertEqualsDouble(gramRoute.amplitudes(j).value, compact.amplitudes(j), 1e-6 * math.max(1.0, math.abs(compact.amplitudes(j))), s"amplitude $j voxel $v")
        j += 1
      assertEqualsDouble(gramRoute.queries.head.value, gramRoute.amplitudes(0).value - gramRoute.amplitudes(1).value, 1e-12)
      if gramRoute.status == DecodeStatus.Accepted then
        accepted += 1
        assert(math.abs(gramRoute.coordinates(0) - tau(v)) < 0.6, s"voxel $v latency ${gramRoute.coordinates(0)} vs truth ${tau(v)}")
      v += 1
    assert(accepted >= 0.8 * voxels, s"accepted $accepted of $voxels")

  test("trial outputs and mismatched structures are refused before any response is read"):
    val trial = ConditionProfileFit.prepare(plan, policy(OutputRequest.TrialAmplitudes(NormalizationRule.Density)))
    assert(trial.isLeft)
    val wrongRank = policy(OutputRequest.ConditionAmplitudes(NormalizationRule.Density)).copy(basis = basis)
    val badStructure = wrongRank.copy(structure = scalafim.fmri.fit.TaskBasisStructure.make(Vector(Vector(plan.structuralColumns.head.id))).fold(e => fail(e.message), identity))
    assert(ConditionProfileFit.prepare(plan, badStructure).isLeft)
    assert(ConditionProfileFit.prepare(plan, policy(OutputRequest.ConditionAmplitudes(NormalizationRule.UnitIntegral))).isLeft)
