package scalafim.examples.workflows

import gale.linalg.DMat
import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.{Event, EventModel, EventTerm}
import scalafim.fmri.design.hrf.HrfKernelBasis
import scalafim.fmri.design.hrf.KernelBasisSpec
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, NormalizationRule, ShapePoint}
import scalafim.fmri.model.{FitPlan, FmriModel}
import scalafim.image.SampleSpaces

/** One voxel's condition-profile output as the workflow reports it. */
final case class ProfileHrfVoxelRow(
    voxel: Int,
    status: DecodeStatus,
    peakLatencySeconds: Double,
    fwhmSeconds: Double,
    amplitudes: Vector[Double],
    contrastAminusB: Double,
    conditionalSdTau: Double)

/** Condition-only ProfileHrf over an ordinary OLS `FitPlan`.
  *
  * The task term is convolved with a certified Gaussian kernel basis, the
  * plan is fitted by the existing engine, and the shape post-solve reads the
  * retained sufficient statistics: one shared shape per voxel, signed
  * condition amplitudes, a signed contrast with a float32 audit, and
  * statuses that keep identification separate from approximation.
  */
object ProfileHrfConditionWorkflows:

  private val rows = 240
  private val voxels = 12
  private val frame = SamplingFrame(blockLens = Seq(rows), tr = Seq(1.0))
  private val precision = Seconds(0.1)
  private val family = GaussianFamily.Default

  private val term: EventTerm =
    val rng = new scala.util.Random(2026L)
    val onsets = Vector.fill(36)(rng.nextInt(2100) / 10.0).sorted.map(Seconds(_))
    val conditions = Vector.tabulate(36)(i => Vector("A", "B", "C")(i % 3))
    EventTerm(events = Vector(Event.factor(conditions, "cond")), onsets = onsets, blockIds = Vector.fill(36)(0), termTag = Some("cond"))

  def basis(): HrfKernelBasis =
    val step = PositiveSeconds(0.1).fold(e => throw new IllegalArgumentException(e.message), identity)
    HrfKernelBasis.compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-3, maxRank = 32))
      .fold(e => throw new IllegalArgumentException(e.message), identity)

  /** Synthetic responses: a Gaussian response per voxel plus drift and noise. */
  private def dataset(): SynchronousFmriDataset =
    val rng = new scala.util.Random(11L)
    val builder = DMat.newBuilder(rows, voxels)
    var v = 0
    while v < voxels do
      val point = ShapePoint.unsafe(Vector(4.0 + 3.0 * rng.nextDouble(), math.log(1.0 + 1.2 * rng.nextDouble())))
      val scale = new Array[Double](family.jetComponents)
      family.scaleJetInto(family.libraryNormalization, point, scale)
      val design = term.convolve(family.toHrf(point), frame, precision = precision).data
      val beta = Array(1.5, -0.8, 0.6).map(_ * (0.7 + 0.6 * rng.nextDouble()))
      var t = 0
      while t < rows do
        var signal = 0.0
        var j = 0
        while j < 3 do
          signal += design.data(t * 3 + j) / scale(0) * beta(j)
          j += 1
        builder.update(t, v, signal + 0.5 * rng.nextGaussian() + 20.0 + 0.4 * (t.toDouble / rows))
        t += 1
      v += 1
    FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("profile-hrf-example"), builder.result(), SampleSpaces(Vector(voxels, 1, 1))), frame)

  def run(): Vector[ProfileHrfVoxelRow] =
    val kernelBasis = basis()
    val convolved = term.convolve(kernelBasis.kernel, frame, precision = precision)
    val eventModel = EventModel.build(Vector(convolved), frame)
    val baseline = BaselineModel.build(samplingFrame = frame, basis = BaselineBasis.Poly, degree = 1, intercept = Intercept.Global)
    val data = dataset()
    val plan = FitPlan(FmriModel(eventModel, baseline, data))
    val structure = ConditionProfileFit.structureFor(plan, convolved).fold(e => throw new IllegalArgumentException(e.message), identity)
    val contrast = SignedQuery.make("A-B", Vector(1.0, -1.0, 0.0), 1e-6).fold(e => throw new IllegalArgumentException(e.message), identity)
    val policy = ConditionProfilePolicy(
      basis = kernelBasis,
      structure = structure,
      nodesPerAxis = Vector(15, 15),
      budget = DecodeBudget(coarseStride = 2, maxNewtonSteps = 2, maxJets = 2, maxExactEvaluations = 6, weakSdLimit = Vector(0.5, 1.0)),
      prior = None,
      noiseVariance = 0.25,
      output = OutputRequest.ConditionQueries(Vector(contrast), NormalizationRule.Unnormalised),
      blockSize = 4
    )
    val prepared = ConditionProfileFit.prepare(plan, policy).fold(e => throw new IllegalArgumentException(e.message), identity)
    val rowsOut = Vector.newBuilder[ProfileHrfVoxelRow]
    val sink = new BlockSink[ConditionProfileBlock, ConditionProfileReceipt]:
      def accept(block: VoxelBlock, payload: ConditionProfileBlock): Either[String, ConditionProfileReceipt] =
        payload.results.foreach { r =>
          rowsOut += ProfileHrfVoxelRow(r.voxel, r.status, r.summaries.peakLatency.value, r.summaries.fwhm.value, r.amplitudes.map(_.value), r.queries.head.value, r.conditionalSd(0))
        }
        Right(ConditionProfileReceipt(payload.ordinal, payload.results.length, payload.results.count(_.status == DecodeStatus.Accepted)))
    prepared.run(data, sink).fold(e => throw new IllegalArgumentException(e.message), identity)
    rowsOut.result().sortBy(_.voxel)

@main def runProfileHrfConditionWorkflow(): Unit =
  val rows = ProfileHrfConditionWorkflows.run()
  println("voxel  status            peak(s)  fwhm(s)  A       B       C       A-B     sd(tau)")
  rows.foreach { r =>
    println(f"${r.voxel}%5d  ${r.status.toString}%-16s ${r.peakLatencySeconds}%7.3f  ${r.fwhmSeconds}%7.3f  ${r.amplitudes(0)}%6.3f  ${r.amplitudes(1)}%6.3f  ${r.amplitudes(2)}%6.3f  ${r.contrastAminusB}%6.3f  ${r.conditionalSdTau}%6.3f")
  }
