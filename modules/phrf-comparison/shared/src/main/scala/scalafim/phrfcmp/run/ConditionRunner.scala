package scalafim.phrfcmp.run

import gale.linalg.{DMat, Matrix as GMatrix}

import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.ar.TimeSegment
import scalafim.fmri.design.{ConditionId, FactorId, FactorLevelSet, TrialId}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept, NuisanceCheck}
import scalafim.fmri.design.event.{Event, EventSchedule, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.fit.CanonicalTemporalWhitening
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, ShapePoint}
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ProfileCriterion, ProfileHrfPlan, ProfileTrialDrive}
import scalafim.image.SampleSpaces
import scalafim.phrfcmp.ingest.{CellKind, FitInputs}
import scalafim.phrfcmp.prep.{CommonPrep, FirPrefit, NativeArm, Nuisance, Whiten, WhiteningSpec}
import scalafim.phrfcmp.score.{EResp, ResponseGrid}

/**
  * PHRF condition configuration. The defaults are the S0-pinned values except the lowering grid (design 2.1: Gaussian family, 0.2 s lowering
  * grid, 9x9 decode grid, 16 Newton steps, 20 jets, 40 exact evaluations, no prior, admission 0.5 / 1e10 / 1e-9).
  */
final case class PhrfConditionConfig(
    family: GaussianFamily = GaussianFamily.Default,
    // 0.1 s: the generator puts onsets on a 0.1 s grid. The compact route lowers events onto this grid by linear (hat)
    // splitting of each impulse, which is exact only for onsets on a lowering node. At 0.2 s an odd-0.1 onset costs a
    // systematic 3e-3 of the peak response in E-resp (3e-3 in sd) that the exact-lag native arms do not pay; at 0.1 s
    // it is 3e-6 (see docs/verification/phrf-cmp-s3-20261001.md).
    loweringSeconds: Double = 0.1,
    kernelNodes: Vector[Int] = Vector(26, 21),
    kernelTolerance: Double = 1e-4,
    kernelMaxRank: Int = 40,
    decodeNodes: Int = 9,
    budget: DecodeBudget = DecodeBudget(maxNewtonSteps = 16, maxJets = 20, maxExactEvaluations = 40, maxCandidateAttempts = 12, stationarityStepTolerance = 1e-8),
    requirements: ObservedFamilyRequirements = ObservedFamilyRequirements(0.5, 1e10, 1e-9),
    heldOut: Vector[ShapePoint] =
      Vector(Vector(5.0, math.log(1.5)), Vector(6.5, math.log(2.0)), Vector(4.0, math.log(1.2))).map(ShapePoint.unsafe),
    block: Int = 20
):
  /** Compiled once per configuration instance. */
  lazy val basis: Either[String, HrfKernelBasis] =
    PositiveSeconds.fromSeconds(Seconds(loweringSeconds), "lowering").left.map(_.message).flatMap { p =>
      HrfKernelBasis.compile(KernelBasisSpec(family, p, kernelNodes, tolerance = kernelTolerance, maxRank = kernelMaxRank)).left.map(_.message)
    }

object PhrfConditionConfig:
  /** One shared default instance: its kernel basis (about 1.6 s to compile at the 0.1 s lowering grid) is built once per process. */
  lazy val Default: PhrfConditionConfig = PhrfConditionConfig()

final case class ConditionRunConfig(
    horizonSeconds: Int = 32,
    phrf: PhrfConditionConfig = PhrfConditionConfig.Default
)

object ConditionRunConfig:
  lazy val Default: ConditionRunConfig = ConditionRunConfig()

/** All four arms of one dataset, with the hash they share. */
final case class ConditionRunSet(sharedInputSha256: String, results: Vector[ConditionArmResult])

object ConditionRunner:

  /**
    * The shared-input contract (design 2.0.4 and 2.0.5): the PHRF `FitConfig` phi equals the plan phi bit for bit
    * (and segments, order and initial-condition policy agree), and every native arm of the cell kind is bound to the
    * same whitened arrays, hence the same hash. Returns that hash.
    */
  def sharedInput(prep: CommonPrep): Either[FailureKind, String] =
    WhiteningSpec.agreement(prep.spec).left.map(FailureKind.Prep(_)).flatMap { _ =>
      val hashes = prep.armHashes
      val distinct = hashes.values.toSet
      if distinct.size == 1 && hashes.keySet == NativeArm.forKind(prep.kind).toSet then Right(distinct.head)
      else Left(FailureKind.Inconsistent("native arms do not share one whitened-array hash"))
    }

  /**
    * The inputs handed to an arm must be the ones the preparation was built from: whitening `inputs.y` with the shared
    * plan reproduces `prep.whitened.y` bit for bit. PHRF whitens its raw data internally with that same plan, so this
    * is what makes "all arms fit the same whitened arrays" true for it as well.
    */
  def whitenedMatches(prep: CommonPrep, inputs: FitInputs): Boolean =
    Whiten.series(prep.plan, inputs.y) match
      case Right(w) =>
        val a = w.data
        val b = prep.whitened.y.data
        a.length == b.length && a.indices.forall(i => java.lang.Double.doubleToLongBits(a(i)) == java.lang.Double.doubleToLongBits(b(i)))
      case Left(_) => false

  private def sameBits(a: Array[Double], b: Array[Double]): Boolean =
    a.length == b.length && a.indices.forall(i => java.lang.Double.doubleToLongBits(a(i)) == java.lang.Double.doubleToLongBits(b(i)))

  /**
    * The whole binding of `inputs` to `prep`, bit for bit (no tolerance): S2's exact input fingerprint
    * (`CommonPrep.requireMatches`: SHA-256 over onsets, conditions, durations, sample times, run ids, event runs, nuisance
    * and y), which also sees changes finer than the 1 s FIR bins of the pre-fit; and, kept as independent checks, the
    * response (`whitenedMatches`), the nuisance intercept split, and the whitened pre-fit design. It is the same machine
    * only: it makes no cross-platform claim.
    */
  def inputsBound(prep: CommonPrep, inputs: FitInputs): Boolean =
    prep.requireMatches(inputs).isRight && whitenedMatches(prep, inputs) && {
      val nuisanceOk = Nuisance.dropIntercepts(inputs.nuisance, inputs.runId) match
        case Right(s) => sameBits(s.dropped.data, prep.split.dropped.data) && sameBits(s.runIntercepts.data, prep.split.runIntercepts.data)
        case Left(_)  => false
      nuisanceOk && {
        val designOk = for
          x <- FirPrefit.arDesign(inputs, prep.kind)
          w <- Whiten.columns(prep.plan, x)
        yield sameBits(w.data, prep.whitened.prefitDesign.data)
        designOk.getOrElse(false)
      }
    }

  def runAll(prep: CommonPrep, inputs: FitInputs, config: ConditionRunConfig = ConditionRunConfig.Default): ConditionRunSet =
    val results = ConditionArm.All.map(run(_, prep, inputs, config))
    ConditionRunSet(results.head.inputSha256, results)

  /** Never throws: every failure is a typed per-voxel status. */
  def run(arm: ConditionArm, prep: CommonPrep, inputs: FitInputs, config: ConditionRunConfig = ConditionRunConfig.Default): ConditionArmResult =
    val voxels = inputs.y.rows
    def allFailed(k: FailureKind, hash: String = ""): ConditionArmResult =
      ConditionArmResult(arm, hash, Vector.tabulate(voxels)(v => ConditionVoxel(v, ConditionArmStatus.Failed(k), None, None)), None, None)
    if prep.kind != CellKind.Condition then allFailed(FailureKind.KindMismatch)
    else if prep.runId.length != inputs.runId.length || !prep.runId.sameElements(inputs.runId) || prep.whitened.y.rows != voxels then
      allFailed(FailureKind.Inconsistent("preparation and inputs describe different datasets"))
    else if !inputsBound(prep, inputs) then allFailed(FailureKind.InputsNotPrepared)
    else
      sharedInput(prep) match
        case Left(k) => allFailed(k)
        case Right(hash) =>
          val armInput = prep.armInput(arm.nativeArm)
          if armInput.sha256 != hash then allFailed(FailureKind.Inconsistent("arm input hash differs"), hash)
          else
            ConditionEvents.fromInputs(inputs) match
              case Left(k) => allFailed(k, hash)
              case Right(events) =>
                arm match
                  case ConditionArm.Phrf => Phrf.run(prep, inputs, events, config, hash)
                  case _                 => native(arm, prep, inputs, events, config, hash)

  private def native(arm: ConditionArm, prep: CommonPrep, inputs: FitInputs, events: ConditionEvents, config: ConditionRunConfig, hash: String): ConditionArmResult =
    val voxels = inputs.y.rows
    def failed(k: FailureKind) = ConditionArmResult(arm, hash, Vector.tabulate(voxels)(v => ConditionVoxel(v, ConditionArmStatus.Failed(k), None, None)), None, None)
    val outcome =
      for
        hrf <- ConditionDesigns.designHrf(arm, config.horizonSeconds).toRight(FailureKind.Design("no design kernel"))
        basis <- ConditionDesigns.responseBasis(arm, config.horizonSeconds).toRight(FailureKind.Design("no response basis"))
        grid <- ResponseGrid.standard(config.horizonSeconds).left.map(FailureKind.Response(_))
        x <- ConditionDesigns.build(hrf, events, inputs.sampleTime, prep.segments)
        fit <- NativeConditionFit.fit(x, prep.plan, prep.armInput(arm.nativeArm).arrays)
      yield ConditionArmResult(arm, hash, NativeConditionFit.outcomes(fit, basis, events.conditions, grid), Some(fit.coefficients), None)
    outcome.fold(failed, identity)

  /** The PHRF compact condition route (S0 pinned sequence; admission after rho is known). */
  private object Phrf:

    private def fmt(e: Any): String = e match
      case p: Product => p.productPrefix
      case other      => other.getClass.getSimpleName

    def run(prep: CommonPrep, inputs: FitInputs, events: ConditionEvents, config: ConditionRunConfig, hash: String): ConditionArmResult =
      val voxels = inputs.y.rows
      def all(s: ConditionArmStatus) = ConditionArmResult(ConditionArm.Phrf, hash, Vector.tabulate(voxels)(v => ConditionVoxel(v, s, None, None)), None, None)
      def setup(stage: String, e: Any) = all(ConditionArmStatus.Failed(FailureKind.Setup(stage, fmt(e))))
      def refuse(stage: String, e: Any) = all(ConditionArmStatus.Refused(RefusalKind.DatasetLevel(stage, fmt(e))))
      val c = config.phrf
      val segs = prep.segments
      val t = inputs.sampleTime.length
      val tr = if t > 1 then inputs.sampleTime(1) - inputs.sampleTime(0) else 1.0
      val layoutOk = segs.forall { s =>
        (s.start until s.endExclusive).forall(i => inputs.sampleTime(i) == inputs.sampleTime(s.start) + (i - s.start) * tr)
      }
      if !layoutOk || !(tr > 0.0) then setup("frame", "IrregularSampling")
      else
        val runLengths = segs.map(s => s.endExclusive - s.start)
        // Scan k of a run sits at start + k * tr (generator convention); the library default would be tr / 2.
        val frame = SamplingFrame(blockLens = runLengths, tr = runLengths.map(_ => tr), startTime = segs.map(s => inputs.sampleTime(s.start)))
        val result =
          for
            basis <- c.basis.left.map(m => setup("basis", m))
            baseline <- baselineOf(prep, segs, frame).left.map(m => setup("baseline", m))
            drive <- driveOf(events).left.map(m => setup("drive", m))
            dataset <- datasetOf(inputs, frame).left.map(m => setup("dataset", m))
            nuisance = DMat.tabulate(t, baseline.designMatrix.cols)((r, k) => baseline.designMatrix(r, k))
            levels = FactorLevelSet.unsafe(FactorId.unsafe("condition"), drive.conditionLabels.map(_.value))
            event <- Event.factorWithLevels(drive.conditionForTrial.map(_.value), "condition", levels).left.map(e => setup("event", e))
            term <- EventTerm.fromSchedule(Vector(event), drive.schedule, Some("condition")).left.map(e => setup("term", e))
            expanded <- ExpandedConditionDesign.lower(term, frame, basis, Seconds(c.loweringSeconds), dropEmpty = false).left.map(e => setup("lowering", e))
            admission <- ObservedFamilyCertification
              .admitForCompact(expanded, term, frame, Seconds(c.loweringSeconds), Some(prep.plan), Some(nuisance), c.heldOut, c.requirements)
              .left.map(e => refuse("admission", e))
            plan <- ProfileHrfPlan
              .fromTrialEvents(dataset, drive, baseline, prep.phrfConfig, basis, 0.0, ProfileCriterion.PenalizedProfile(prep.sigma2All.sigma2))
              .left.map(e => refuse("plan", e))
            policy = ProfileDecodePolicy(Vector(c.decodeNodes, c.decodeNodes), c.budget, prior = None,
              execution = ExecutionBudget(c.block, 1), observedAdmission = Some(admission))
            prepared <- ProfileHrfFit.prepare(plan, DataSelection.All, CanonicalTemporalWhitening.Shared(prep.plan), policy).left.map(e => refuse("prepare", e))
            reader <- SynchronousFmriDataset.readerFor(dataset).left.map(e => setup("reader", e))
            blocks = scala.collection.mutable.ArrayBuffer.empty[ProfileFitBlock]
            sink = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
              def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
                blocks += payload
                Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))
            summary <- prepared.run(reader, sink).left.map(e => refuse("run", e))
            grid <- ResponseGrid.standard(config.horizonSeconds).left.map(e => all(ConditionArmStatus.Failed(FailureKind.Response(e))))
          yield
            val byId = blocks.toVector.flatMap(_.results).map(r => r.voxelId -> r).toMap
            val outs = Vector.tabulate(voxels) { v =>
              byId.get(v) match
                case None => ConditionVoxel(v, ConditionArmStatus.Failed(FailureKind.Setup("run", "VoxelNotDelivered")), None, None)
                case Some(r) =>
                  val detail = PhrfVoxelDetail(r.coordinates, r.conditionMeans, r.status, r.penalizedEnergy)
                  if r.status != DecodeStatus.Accepted then
                    ConditionVoxel(v, ConditionArmStatus.Refused(RefusalKind.Decode(r.status)), None, Some(detail))
                  else
                    val norm = r.readout match
                      case ProfileAmplitudeReadout.ConditionMeans(_, n) => Right(n)
                      case _ => Left(FailureKind.Setup("readout", "NotConditionMeans"))
                    norm.flatMap(n => EResp.fromParametric(grid, plan.basis.family, ShapePoint.unsafe(r.coordinates), n, r.conditionMeans).left.map(FailureKind.Response(_))) match
                      case Right(resp) => ConditionVoxel(v, ConditionArmStatus.Estimated, Some(resp), Some(detail))
                      case Left(k)     => ConditionVoxel(v, ConditionArmStatus.Failed(k), None, Some(detail))
            }
            ConditionArmResult(ConditionArm.Phrf, hash, outs, None, Some(summary.setup.route),
              Some(PhrfAdmissionSummary(summary.setup.observedAdmissionFingerprint, admission.certificate.points.length, admission.certificate.maxProjectorError)))
        result.fold(identity, identity)

    /** Constant intercepts per run plus the generator nuisance without its intercepts, as per-run blocks (S0 B8). */
    private def baselineOf(prep: CommonPrep, segs: Vector[TimeSegment], frame: SamplingFrame): Either[String, BaselineModel] =
      val n = prep.baselineNuisance
      val owner = (0 until n.cols).map { j =>
        val runs = segs.filter(s => (s.start until s.endExclusive).exists(i => n(i, j) != 0.0)).map(_.runIndex)
        if runs.length == 1 then Right(runs.head) else Left(s"nuisance column $j is not confined to one run")
      }
      owner.collectFirst { case Left(m) => m } match
        case Some(m) => Left(m)
        case None =>
          val own = owner.map(_.toOption.get)
          val mats = segs.map { s =>
            val cols = (0 until n.cols).filter(j => own(j) == s.runIndex)
            val rows = s.endExclusive - s.start
            Mat.unsafe(rows, cols.length, Array.tabulate(rows * cols.length)(k => n(s.start + k / cols.length, cols(k % cols.length))))
          }
          scala.util.Try(BaselineModel.build(frame, BaselineBasis.Constant, intercept = Intercept.Runwise, nuisanceList = Some(mats), nuisanceCheck = NuisanceCheck.None))
            .toEither.left.map(e => e.getClass.getSimpleName)

    private def driveOf(ev: ConditionEvents): Either[String, ProfileTrialDrive] =
      for
        schedule <- EventSchedule.fromParts(ev.onset.toVector.map(Seconds(_)), ev.duration.toVector.map(Seconds(_)), ev.run.toVector).left.map(e => fmt(e))
        membership <- TrialMembership.make(ev.cond.toVector, ev.conditions).left.map(e => fmt(e))
        labels = Vector.tabulate(ev.conditions)(i => s"c$i")
        drive <- ProfileTrialDrive.make(schedule, membership, labels.map(ConditionId.unsafe),
          Vector.tabulate(ev.onset.length)(i => TrialId.unsafe(s"event$i")), ev.cond.toVector.map(k => ConditionId.unsafe(labels(k)))).left.map(e => fmt(e))
      yield drive

    private def datasetOf(inputs: FitInputs, frame: SamplingFrame): Either[String, FmriDataset] =
      val v = inputs.y.rows
      val t = inputs.y.cols
      try
        val data = GMatrix.dense(t, v, (0 until t).flatMap(r => (0 until v).map(k => inputs.y(k, r))))
        Right(FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("phrf-cmp-condition"), data, SampleSpaces(Vector(v, 1, 1))), frame).dataset)
      catch case e: Exception => Left(e.getClass.getSimpleName)
