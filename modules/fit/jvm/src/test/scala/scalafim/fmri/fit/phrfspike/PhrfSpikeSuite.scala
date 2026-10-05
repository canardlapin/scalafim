package scalafim.fmri.fit.phrfspike

import gale.linalg.{DMat, Matrix, QROptions, QRPivoting}
import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.{ConditionId, FactorId, FactorLevelSet, TrialId}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept, NuisanceCheck}
import scalafim.fmri.design.event.{Event, EventSchedule, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.fit.CanonicalTemporalWhitening
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, NormalizationRule, ShapePoint}
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, ProfileCriterion, ProfileHrfPlan, ProfileTrialDrive}
import scalafim.image.SampleSpaces

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

/** S0 source-qualification spike for the PHRF comparative-evaluation runner.
  *
  * Reads the flat export written by `tools/phrf-comparison/spike/export_flat.py`
  * from a HARNESS-root generator dataset (never a pilot root) and drives the
  * production PHRF executor entry points end to end. Everything here is preview
  * evidence for the S0 receipt, not pilot evidence. Skipped when the export is absent.
  */
class PhrfSpikeSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(20, "min")

  private val root: Path = Paths.get(sys.props.getOrElse("user.dir", ".")).resolve("tools/phrf-comparison/spike")
  private val log = ArrayBuffer.empty[String]
  private def say(line: String): Unit =
    println(line)
    log += line
  private val cpuBean = ManagementFactory.getThreadMXBean
  private def cpuMs(): Double = cpuBean.getCurrentThreadCpuTime / 1e6
  private def timed[A](label: String)(body: => A): (A, Double, Double) =
    val c0 = cpuMs(); val w0 = System.nanoTime()
    val value = body
    val cpu = cpuMs() - c0; val wall = (System.nanoTime() - w0) / 1e6
    say(f"TIMING $label%-48s cpu=$cpu%10.1f ms  wall=$wall%10.1f ms")
    (value, cpu, wall)

  // ---- flat reader -------------------------------------------------------
  private def tokens(file: Path): Array[Array[Double]] =
    Files.readAllLines(file).asScala.iterator.filter(_.nonEmpty)
      .map(_.trim.split("\\s+").map(_.toDouble)).toArray
  private final case class Ds(cell: String, y: Array[Array[Double]], nuisance: Array[Array[Double]],
      runId: Array[Int], sampleTime: Array[Double], onset: Array[Double], cond: Array[Int], evRun: Array[Int],
      trialBeta: Option[Array[Array[Double]]]):
    def voxels: Int = y.length
    def rows: Int = sampleTime.length
    def runLengths: Vector[Int] = runId.toVector.groupBy(identity).toVector.sortBy(_._1).map(_._2.length)
  private def load(cell: String): Option[Ds] =
    val dir = root.resolve(s"flat/${cell}__d0000")
    if !Files.exists(dir.resolve("y.txt")) then None
    else
      def flat(name: String) = tokens(dir.resolve(s"$name.txt")).flatten
      val trial = if Files.exists(dir.resolve("trial_beta.txt")) then Some(tokens(dir.resolve("trial_beta.txt"))) else None
      Some(Ds(cell, tokens(dir.resolve("y.txt")), tokens(dir.resolve("nuisance.txt")),
        flat("run_id").map(_.toInt), flat("sample_time"), flat("ev_onset"), flat("ev_cond").map(_.toInt),
        flat("ev_run").map(_.toInt), trial))

  // ---- FIR pre-fit, pooled AR(1), sigma2 ----------------------------------
  private val firLags = 32
  private def firDesign(ds: Ds): DMat =
    val cols = 3 * firLags + ds.nuisance.head.length
    val x = Array.fill(ds.rows, cols)(0.0)
    var e = 0
    while e < ds.onset.length do
      var t = 0
      while t < ds.rows do
        if ds.runId(t) == ds.evRun(e) then
          val lag = math.floor(ds.sampleTime(t) - ds.onset(e) + 1e-9).toInt
          if lag >= 0 && lag < firLags then x(t)(ds.cond(e) * firLags + lag) += 1.0
        t += 1
      e += 1
    DMat.tabulate(ds.rows, cols)((t, j) => if j < 3 * firLags then x(t)(j) else ds.nuisance(t)(j - 3 * firLags))

  private def projector(x: DMat): (DMat, Int) =
    val qr = x.qr(QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(1e-10)))
    val rank = qr.diagnostics.rank.getOrElse(x.cols)
    (qr.q.slice(0, x.rows, 0, rank), rank)

  private def residuals(x: DMat, y: DMat): (DMat, Int) =
    val (q, rank) = projector(x)
    (y - q * (q.t * y), rank)

  private final case class Prefit(rho: Double, sigma2: Double, rankFir: Int, whitening: WhiteningPlan)

  private def prefit(ds: Ds): Prefit =
    val y = DMat.tabulate(ds.rows, ds.voxels)((t, v) => ds.y(v)(t))
    val x = firDesign(ds)
    val (res, rank) = residuals(x, y)
    var num = 0.0; var den = 0.0
    var t = 1
    while t < ds.rows do
      if ds.runId(t) == ds.runId(t - 1) then
        var v = 0
        while v < ds.voxels do
          num += res(t, v) * res(t - 1, v); den += res(t, v) * res(t, v)
          v += 1
      t += 1
    val rho = num / den
    val segments = segmentsOf(ds)
    val plan = WhiteningPlan.global(ArmaCoefficients.ar(rho), segments, exactFirstAr1 = true)
    // Second stage: refit on the whitened problem; sigma2 = pooled RSS / (n - rank).
    val wy = WhiteningTransform.matrix(plan, y).fold(error => fail(error.toString), identity)
    val wx = WhiteningTransform.matrix(plan, x).fold(error => fail(error.toString), identity)
    val (wres, wrank) = residuals(wx, wy)
    var rss = 0.0
    var i = 0
    while i < wres.rows do
      var v = 0
      while v < wres.cols do { rss += wres(i, v) * wres(i, v); v += 1 }
      i += 1
    val sigma2 = rss / (wres.cols.toDouble * (wres.rows - wrank))
    Prefit(rho, sigma2, rank, plan)

  private def segmentsOf(ds: Ds): Vector[TimeSegment] =
    val lengths = ds.runLengths
    val starts = lengths.scanLeft(0)(_ + _)
    lengths.indices.map(r => TimeSegment(starts(r), starts(r + 1), r)).toVector

  // ---- model assembly -----------------------------------------------------
  private def frameOf(ds: Ds) = SamplingFrame(blockLens = ds.runLengths, tr = ds.runLengths.map(_ => 1.0))

  private def baselineOf(ds: Ds, frame: SamplingFrame): BaselineModel =
    val starts = ds.runLengths.scanLeft(0)(_ + _)
    // Block-diagonal C0 nuisance: perRun columns per run (6), run r owns columns r*perRun until (r+1)*perRun.
    // The Constant baseline term already supplies one intercept per run, so the generator's intercept
    // column (index 0 of each run) is dropped; the spanned column space is identical.
    val perRun = ds.nuisance.head.length / ds.runLengths.length
    val mats = ds.runLengths.indices.map { r =>
      val n = starts(r + 1) - starts(r)
      Mat.unsafe(n, perRun - 1, Array.tabulate(n * (perRun - 1))(k => ds.nuisance(starts(r) + k / (perRun - 1))(r * perRun + 1 + k % (perRun - 1))))
    }
    BaselineModel.build(frame, BaselineBasis.Constant, intercept = Intercept.Runwise,
      nuisanceList = Some(mats), nuisanceCheck = NuisanceCheck.None)

  private def datasetOf(ds: Ds, frame: SamplingFrame, voxels: Int, id: String): FmriDataset =
    val data = Matrix.dense(ds.rows, voxels, (0 until ds.rows).flatMap(t => (0 until voxels).map(v => ds.y(v)(t))))
    FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId(id), data, SampleSpaces(Vector(voxels, 1, 1))), frame).dataset

  private def driveOf(ds: Ds): ProfileTrialDrive =
    val schedule = EventSchedule.fromParts(ds.onset.toVector.map(Seconds(_)), Vector.fill(ds.onset.length)(Seconds(0.0)),
      ds.evRun.toVector).fold(error => fail(error.message), identity)
    val membership = TrialMembership.make(ds.cond.toVector, 3).fold(error => fail(error.message), identity)
    val labels = Vector("c0", "c1", "c2")
    ProfileTrialDrive.make(schedule, membership, labels.map(ConditionId.unsafe),
      Vector.tabulate(ds.onset.length)(i => TrialId.unsafe(s"trial$i")), ds.cond.toVector.map(c => ConditionId.unsafe(labels(c))))
      .fold(error => fail(error.message), identity)

  private lazy val basis: HrfKernelBasis = HrfKernelBasis.compile(KernelBasisSpec(
    GaussianFamily.Default, PositiveSeconds.unsafe(Seconds(0.2)), Vector(26, 21), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)

  private def configOf(rho: Double) =
    FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), global = true, rho = Some(rho)))

  private def checked[A](value: Either[ProfileFitError, A]): A = value.fold(error => fail(error.message), identity)

  private def fitSink(into: ArrayBuffer[ProfileFitBlock]) = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
    def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
      into += payload
      Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))
  private def outSink(into: ArrayBuffer[ProfileTrialOutputBlock]) = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
    def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
      into += payload
      Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))

  private def policy(nodes: Int, block: Int, admission: Option[ObservedFamilyAdmission] = None) =
    ProfileDecodePolicy(Vector(nodes, nodes), DecodeBudget(maxNewtonSteps = 16, maxJets = 20, maxExactEvaluations = 40,
      maxCandidateAttempts = 12, stationarityStepTolerance = 1e-8), prior = None,
      execution = ExecutionBudget(block, 1), observedAdmission = admission)

  private def pearson(a: Seq[Double], b: Seq[Double]): Double =
    val ma = a.sum / a.length; val mb = b.sum / b.length
    val cov = a.zip(b).map((x, y) => (x - ma) * (y - mb)).sum
    cov / math.sqrt(a.map(x => (x - ma) * (x - ma)).sum * b.map(y => (y - mb) * (y - mb)).sum)
  private def median(v: Seq[Double]): Double = v.sorted.apply(v.length / 2)

  override def afterAll(): Unit =
    val out = root.resolve("results"); Files.createDirectories(out)
    val _ = Files.writeString(out.resolve("s0-smoke-log.txt"), log.mkString("\n") + "\n")

  // ---- condition smoke ----------------------------------------------------
  test("S0 condition smoke: C-TX-.5, 40 voxels, Gaussian, global pooled AR(1) from FIR pre-fit"):
    val ds = load("C-TX-.5").getOrElse { assume(false, "flat export absent"); sys.error("unreachable") }
    val voxels = 40
    say(s"COND cell=${ds.cell} rows=${ds.rows} runs=${ds.runLengths} events=${ds.onset.length} voxels(total)=${ds.voxels} fit voxels=$voxels")
    val (pre, _, _) = timed("cond FIR pre-fit + AR(1) + sigma2 (all 200 vox)")(prefit(ds))
    say(f"COND FIR rank=${pre.rankFir} rho_hat=${pre.rho}%.4f (truth 0.3) sigma2_whitened=${pre.sigma2}%.4f")
    val frame = frameOf(ds)
    val baseline = baselineOf(ds, frame)
    val dataset = datasetOf(ds, frame, voxels, "phrf-s0-cond")
    val drive = driveOf(ds)
    val config = configOf(pre.rho)
    val nuisance = DMat.tabulate(ds.rows, baseline.designMatrix.cols)((t, j) => baseline.designMatrix(t, j))
    val levels = FactorLevelSet.unsafe(FactorId.unsafe("condition"), drive.conditionLabels.map(_.value))
    val event = Event.factorWithLevels(drive.conditionForTrial.map(_.value), "condition", levels).fold(e => fail(e.message), identity)
    val term = EventTerm.fromSchedule(Vector(event), drive.schedule, Some("condition")).fold(e => fail(e.message), identity)
    val expanded = ExpandedConditionDesign.lower(term, frame, basis, Seconds(0.2), dropEmpty = false).fold(e => fail(e.message), identity)
    val held = Vector(Vector(5.0, math.log(1.5)), Vector(6.5, math.log(2.0)), Vector(4.0, math.log(1.2))).map(ShapePoint.unsafe)
    val (admissionE, _, _) = timed("cond observed-family admission (cold)")(
      ObservedFamilyCertification.admitForCompact(expanded, term, frame, Seconds(0.2), Some(pre.whitening), Some(nuisance), held,
        ObservedFamilyRequirements(0.5, 1e10, 1e-9)))
    val admission = admissionE.fold(e => fail(e.message), identity)
    say(s"COND admission maxProjectorError=${admission.certificate.maxProjectorError} nuisanceRank=${admission.certificate.nuisanceRank}")
    val plan = ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, config, basis, 0.0, ProfileCriterion.PenalizedProfile(pre.sigma2))
      .fold(e => fail(e.message), identity)
    val pol = policy(9, 20, Some(admission))
    val whitening = CanonicalTemporalWhitening.Shared(pre.whitening)
    val (preparedE, prepCpu1, _) = timed("cond ProfileHrfFit.prepare (cold)")(ProfileHrfFit.prepare(plan, DataSelection.All, whitening, pol))
    val prepared = checked(preparedE)
    val (_, prepCpu2, _) = timed("cond ProfileHrfFit.prepare (warm)")(ProfileHrfFit.prepare(plan, DataSelection.All, whitening, pol))
    val reader = SynchronousFmriDataset.readerFor(dataset).fold(e => fail(e.message), identity)
    val blocks = ArrayBuffer.empty[ProfileFitBlock]
    val (summaryE, runCpu, _) = timed("cond run 40 voxels (first)")(prepared.run(reader, fitSink(blocks)))
    val summary = checked(summaryE)
    val (_, runCpu2, _) = timed("cond run 40 voxels (warm)")(prepared.run(reader, fitSink(ArrayBuffer.empty)))
    val fits = blocks.toVector.flatMap(_.results)
    val accepted = fits.count(_.status == DecodeStatus.Accepted)
    say(s"COND route=${summary.setup.route} delivered=${summary.progress.deliveredVoxels} statuses=${fits.groupBy(_.status).view.mapValues(_.size).toMap}")
    say(s"COND prepared.trialOutputs refused as expected: ${prepared.trialOutputs.left.toOption.map(_.message)}")
    say(f"COND median tau=${median(fits.map(_.coordinates(0)))}%.2f median logSd=${median(fits.map(_.coordinates(1)))}%.2f accepted=$accepted/${fits.length}")
    val truthCoef = tokens(root.resolve(s"flat/${ds.cell}__d0000/cond_coef.txt"))
    val rs = fits.map(f => pearson(f.conditionMeans, truthCoef(f.voxelId).toVector))
    say(f"COND median within-voxel r(condition means, cond_coef) = ${median(rs)}%.3f (preview, sign/scale of means unnormalised)")
    assertEquals(summary.setup.route, "direct-condition-compact")
    assertEquals(fits.length, voxels)
    assert(fits.forall(_.penalizedEnergy.isFinite))
    say(f"COND TIMING_SUMMARY prepare_cold_cpu_ms=$prepCpu1%.1f prepare_warm_cpu_ms=$prepCpu2%.1f run40_cpu_ms=$runCpu%.1f run40_warm_cpu_ms=$runCpu2%.1f")

  // ---- trial ML smoke ------------------------------------------------------
  test("S0 trial ML smoke: T-TX-fast, 40 voxels, TrialRandomEffectsML(sigma2), alpha grid probe"):
    val ds = load("T-TX-fast").getOrElse { assume(false, "flat export absent"); sys.error("unreachable") }
    val voxels = 40
    say(s"TRIAL cell=${ds.cell} rows=${ds.rows} runs=${ds.runLengths} trials=${ds.onset.length} fit voxels=$voxels")
    val (pre, _, _) = timed("trial FIR pre-fit + AR(1) + sigma2 (all 200 vox)")(prefit(ds))
    say(f"TRIAL FIR rank=${pre.rankFir} rho_hat=${pre.rho}%.4f (truth 0.3) sigma2_whitened=${pre.sigma2}%.4f")
    val frame = frameOf(ds)
    val baseline = baselineOf(ds, frame)
    val bm = DMat.tabulate(ds.rows, baseline.designMatrix.cols)((t, j) => baseline.designMatrix(t, j))
    say(s"TRIAL baseline design cols=${baseline.designMatrix.cols} rank=${projector(bm)._2} generator nuisance cols=${ds.nuisance.head.length} rank=${projector(DMat.tabulate(ds.rows, ds.nuisance.head.length)((t, j) => ds.nuisance(t)(j)))._2}")
    val dataset = datasetOf(ds, frame, voxels, "phrf-s0-trial")
    val drive = driveOf(ds)
    val config = configOf(pre.rho)
    val whitening = CanonicalTemporalWhitening.Shared(pre.whitening)
    val reader = SynchronousFmriDataset.readerFor(dataset).fold(e => fail(e.message), identity)
    val pol = policy(9, 20)
    def planAt(alpha: Double) =
      ProfileHrfPlan.fromTrialEvents(dataset, drive, baseline, config, basis, alpha, ProfileCriterion.TrialRandomEffectsML(pre.sigma2))
        .fold(e => fail(e.message), identity)

    // preparation per alpha (b) and cacheability probe (c): the full 9-point grid 2^-6..2^2, refusals recorded not thrown.
    val grid = (-6 to 2).map(k => math.pow(2.0, k)).toVector
    val gridResults = (grid :+ 0.25).map { a =>
      val (r, cpu, _) = timed(s"trial prepare ML alpha=$a")(ProfileHrfFit.prepare(planAt(a), DataSelection.All, whitening, pol))
      say(s"TRIAL alpha=$a prepare -> ${r.fold(e => "REFUSED " + e.getClass.getSimpleName + ": " + e.message, _ => "ok")}")
      (a, r.isRight, cpu)
    }
    val prepTimes = gridResults.filter(_._2).map(_._3)
    say(s"TRIAL prepare cpu ms by successful call = ${prepTimes.map(t => f"$t%.0f").mkString(", ")}")
    val prepared = checked(ProfileHrfFit.prepare(planAt(0.25), DataSelection.All, whitening, pol))
    say(s"TRIAL setup route=${prepared.setup.route} retainedReferenceBytes=${prepared.setup.retainedReferenceBytes} loweringDoubles=${prepared.setup.expandedTrialLoweringDoubles} mlSetup=${prepared.setup.mlSetup.nonEmpty}")

    val blocks = ArrayBuffer.empty[ProfileFitBlock]
    val (summaryE, runCpu, _) = timed("trial ML run 40 voxels, 144 trials (first)")(prepared.run(reader, fitSink(blocks)))
    val summary = checked(summaryE)
    val (_, runCpu2, _) = timed("trial ML run 40 voxels, 144 trials (warm)")(prepared.run(reader, fitSink(ArrayBuffer.empty)))
    val (_, runCpu3, _) = timed("trial ML run 40 voxels, 144 trials (warm 2)")(prepared.run(reader, fitSink(ArrayBuffer.empty)))
    val fits = blocks.toVector.flatMap(_.results)
    say(s"TRIAL route=${summary.setup.route} delivered=${summary.progress.deliveredVoxels} statuses=${fits.groupBy(_.status).view.mapValues(_.size).toMap}")
    say(s"TRIAL ML terminal evidence present on ${fits.count(_.criterionEvidence.nonEmpty)}/${fits.length} voxels; provenance has criterion-form=${summary.provenance.contains("criterion-form=J=E+sigma2*D")}")
    say(f"TRIAL ML throughput (warm 2) = ${runCpu3 / voxels}%.1f core-ms/voxel; first=${runCpu / voxels}%.1f warm=${runCpu2 / voxels}%.1f")

    // public outputs: ExactShape / PreparedBasisResidual and the refusal
    val view = checked(prepared.trialOutputs)
    val request = OutputRequest.TrialAmplitudes(NormalizationRule.Density)
    val outs = ArrayBuffer.empty[ProfileTrialOutputBlock]
    val (outE, outCpu, _) = timed("trial public outputs ExactShape/PreparedBasisResidual")(
      view.run(reader, request, ProfileTrialReadoutMode.ExactShape, outSink(outs)))
    val outSummary = checked(outE)
    val voxelsOut = outs.toVector.flatMap(_.results)
    val emitted = voxelsOut.flatMap(v => v.output match
      case ProfileTrialOutputOutcome.Emitted(_, value) => Some(v.voxelId -> value)
      case _ => None)
    say(s"TRIAL public outputs emitted=${emitted.length}/${voxelsOut.length}; other outcomes=${voxelsOut.map(_.output).filterNot(_.isInstanceOf[ProfileTrialOutputOutcome.Emitted]).take(2)}")
    val trueBeta = ds.trialBeta.get
    val rs = emitted.map((v, value) => pearson(value.trialAmplitudes.get.toVector.map(_.toDouble), trueBeta(v).toVector))
    say(f"TRIAL median within-voxel r(trial amplitudes, truth trial_beta) = ${median(rs)}%.3f over ${rs.length} voxels (preview, scale-free)")
    say(s"TRIAL public provenance: publicExecution=${outSummary.publicExecution.map(_.toString.take(200))}")
    val refusal = view.run(reader, request, ProfileTrialReadoutMode.CorrectedReference, outSink(ArrayBuffer.empty))
    say(s"TRIAL refusal (CorrectedReference, ML): $refusal")
    say(s"TRIAL refusal message: ${refusal.left.toOption.map(_.message)}")
    val alpha0 = ProfileHrfFit.prepare(planAt(0.0), DataSelection.All, whitening, pol)
    say(s"TRIAL alpha=0 + ML criterion: ${alpha0.left.toOption.map(_.message)}")
    assertEquals(summary.setup.route, "trial-banded-ml")
    assert(refusal.left.toOption.exists(_.isInstanceOf[ProfileFitError.TrialOutputMlIntent]))
    assertEquals(fits.length, voxels)
    say(f"TRIAL TIMING_SUMMARY prepare_median_cpu_ms=${median(prepTimes)}%.1f prepare_ok_alphas=${gridResults.count(_._2)}/${gridResults.length} run40_cpu_ms=$runCpu%.1f run40_warm2_cpu_ms=$runCpu3%.1f outputs40_cpu_ms=$outCpu%.1f")
