package scalafim.fmri.fit.profile

import gale.linalg.DMat
import gale.linalg.verifySymmetric
import gale.spectral.{Eigen, EigenSelection, EigenVectors}
import scalafim.dataset.{DataSelection, DatasetSeriesReader, FmriDataset, ResolvedDataSelection, TimepointSelection, VoxelSelection}
import scalafim.fmri.ar.{InitialConditionPolicy, NoisePooling}
import scalafim.fmri.design.{FactorId, FactorLevelSet}
import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, TrialBasisDesign}
import scalafim.fmri.fit.{CanonicalTemporalWhitening, DesignMatrix, EstimateExecutionOutcome, Gls, PreparedContrastGeometry, ResponsePreparationPlan, RunPartition, TemporalPreparationScope}
import scalafim.fmri.model.{AmplitudeStructure, ArStructure, FitConfig, FitEngine, LssConfig, MissingDataPolicy, NuisanceProjection, ProfileHrfPlan, ProfileHrfSource, RobustOptions, VolumeWeighting}

import scala.util.control.NonFatal

/** The observed-family admission is mandatory for either condition route.
  * The legacy run retains raw backend readout; trialOutputs is explicit.
  */
final case class ProfileDecodePolicy(
    nodesPerAxis: Vector[Int],
    budget: DecodeBudget,
    prior: Option[ShapePrior],
    execution: ExecutionBudget = ExecutionBudget(),
    observedAdmission: Option[ObservedFamilyAdmission] = None,
    trialPreparation: TrialPreparationPolicy = TrialPreparationPolicy())

/** `jets` counts all derivative requests; `firstOrderAttempts` is its search-only subset. */
final case class ProfileDecoderWork(
    voxels: Long,
    nodeScores: Long,
    jets: Long,
    exactEvaluations: Long,
    candidateAttempts: Long,
    terminalVerifications: Long,
    newtonSteps: Long,
    fallbacks: Long,
    firstOrderAttempts: Long = 0L)

object ProfileDecoderWork:
  def from(counters: DecoderCounters): ProfileDecoderWork =
    ProfileDecoderWork(counters.voxels, counters.nodeScores, counters.jets, counters.exactEvaluations,
      counters.candidateAttempts, counters.terminalVerifications, counters.newtonSteps, counters.fallbacks, counters.firstOrderAttempts)

final case class ProfileRunProgress(
    deliveredBlocks: Int,
    deliveredVoxels: Int,
    attemptedVoxels: Int,
    workersUsed: Int,
    decoder: ProfileDecoderWork,
    trial: Option[TrialBandedWorkSnapshot],
    decodeStatuses: Map[DecodeStatus, Long] = Map.empty,
    publicReadout: Option[ProfileTrialOutputProgress] = None,
    publicExecution: Option[ProfileTrialExecutionDeclaration] = None,
    trialMl: Option[TrialMlWork] = None)

enum ProfileMlError:
  case Backend(detail: String)
  case TerminalUnavailable
  case TerminalMismatch(detail: String)

  def message: String = s"trial ML refused: $this"

enum ProfileFitError:
  case Unsupported(detail: String)
  case Preparation(detail: String)
  case TrialMlPreparation(detail: String, bankSetup: TrialBandedSetupReceipt, attempted: TrialMlWork)
  case TrialMlFailure(error: ProfileMlError, progress: ProfileRunProgress)
  case TrialOutputCriterionUnsupported
  case TrialOutputMlIntent(mode: ProfileTrialReadoutMode, evidence: ProfileTrialEvidenceRequest)
  case TrialOutputAdmission(error: ProfileTrialReadoutError)
  case TrialExecutionAdmission(detail: String, execution: ProfileTrialExecutionDeclaration)
  case TrialReadoutFailure(error: ProfileTrialReadoutError, progress: ProfileRunProgress)
  case Dataset(detail: String, progress: ProfileRunProgress)
  case Backend(detail: String, progress: ProfileRunProgress)
  case Cancelled(progress: ProfileRunProgress)
  case SinkRefused(detail: String, progress: ProfileRunProgress)
  case SinkThrew(detail: String, progress: ProfileRunProgress)
  case WorkersStillRunning(original: ExecutionError, receipts: Vector[ProfileFitReceipt],
    setup: ProfileSetupReceipt, provenance: String, termination: ProfileRunTermination)

  def publicExecution: Option[ProfileTrialExecutionDeclaration] =
    this match
      case TrialExecutionAdmission(_, execution) => Some(execution)
      case TrialReadoutFailure(_, progress) => progress.publicExecution
      case TrialMlFailure(_, progress) => progress.publicExecution
      case Dataset(_, progress) => progress.publicExecution
      case Backend(_, progress) => progress.publicExecution
      case Cancelled(progress) => progress.publicExecution
      case SinkRefused(_, progress) => progress.publicExecution
      case SinkThrew(_, progress) => progress.publicExecution
      case WorkersStillRunning(_, _, _, _, termination) => termination.publicExecution
      case Unsupported(_) | Preparation(_) | TrialMlPreparation(_, _, _) | TrialOutputAdmission(_) | TrialOutputCriterionUnsupported | TrialOutputMlIntent(_, _) => None

  def message: String =
    this match
      case TrialExecutionAdmission(detail, _) => "profile trial execution refused: " + detail
      case Unsupported(detail) => s"unsupported profile fit: $detail"
      case Preparation(detail) => s"profile preparation failed: $detail"
      case TrialMlPreparation(detail, _, _) => s"profile ML preparation failed: $detail"
      case TrialMlFailure(error, _) => error.message
      case TrialOutputCriterionUnsupported => "public trial outputs do not support the ML criterion"
      case TrialOutputMlIntent(mode, evidence) => s"native ML public output requires ExactShape/PreparedBasisResidual; requested $mode/$evidence"
      case TrialOutputAdmission(error) => s"profile trial output refused: ${error.message}"
      case TrialReadoutFailure(error, _) => s"profile trial readout failed: ${error.message}"
      case Dataset(detail, _) => s"profile dataset read failed: $detail"
      case Backend(detail, _) => s"profile backend failed: $detail"
      case Cancelled(_) => "profile execution cancelled; previously delivered blocks remain partial"
      case SinkRefused(detail, _) => s"profile sink refused a block: $detail; previously delivered blocks remain partial"
      case SinkThrew(detail, _) => s"profile sink threw: $detail; previously delivered blocks remain partial"
      case WorkersStillRunning(original, _, _, _, _) =>
        s"${original.message}; caller-owned readers remain in use until termination confirms the final failure"

/** The trial case is conditional on the shape chosen from this response. It is
  * not a response-independent TrialReadout or an adaptive-estimator derivative.
  */
enum ProfileAmplitudeReadout:
  case ConditionMeans(values: Vector[Double], normalization: scalafim.fmri.hrf.family.NormalizationRule)
  case AdaptiveTrial(backend: TrialBandedReadout)

/** Frozen-variance ML evidence at the returned coordinates. Raw E and its
  * amplitudes stay in raw; minimization is J. Conditional covariance uses HJ,
  * excluding the shape prior: conditionalCovarianceScale * inverse(curvature).
  */
enum ProfileCriterionEvidence:
  case TrialRandomEffectsML(sigma2: Double, raw: ProfileJet,
    determinant: Double, determinantGradient: Vector[Double], determinantHessian: Vector[Double],
    minimization: ProfileJet, score: Double,
    conditionalCovarianceScale: Double, conditionalCurvature: Vector[Double])

final case class ProfileVoxelResult(
    voxelId: Int,
    coordinates: Vector[Double],
    status: DecodeStatus,
    penalizedEnergy: Double,
    conditionMeans: Vector[Double],
    readout: ProfileAmplitudeReadout,
    conditionalSd: Vector[Double],
    criterionEvidence: Option[ProfileCriterionEvidence] = None)

final case class ProfileFitBlock(ordinal: Int, voxelIds: Vector[Int], results: Vector[ProfileVoxelResult])
final case class ProfileFitReceipt(ordinal: Int, voxelIds: Vector[Int])
final case class ProfileSetupReceipt(
    route: String,
    trial: Option[TrialBandedPreparationReceipt],
    retainedReferenceBytes: Option[Long],
    expandedTrialLoweringDoubles: Option[Long],
    observedAdmissionFingerprint: Option[String],
    bankSetup: Option[TrialBandedSetupReceipt] = None,
    mlSetup: Option[TrialMlWork] = None,
    mlEnergyScratchValuesPerWorker: Option[Int] = None,
    compactComparisonPerWorker: Option[CompactComparisonWorkspaceReceipt] = None)
final case class ProfileRunSummary(
    receipts: Vector[ProfileFitReceipt],
    progress: ProfileRunProgress,
    setup: ProfileSetupReceipt,
    provenance: String):
  def publicExecution: Option[ProfileTrialExecutionDeclaration] = progress.publicExecution

/** Finalizes a failed parallel run only after its owned workers have stopped.
  * The resulting progress is captured once and can be read repeatedly.
  */
final class ProfileRunTermination private[profile] (
    private val workers: ExecutionTermination,
    private val finish: () => ProfileFitError,
    val publicExecution: Option[ProfileTrialExecutionDeclaration] = None):
  private var completed: Option[ProfileFitError] = None

  def isTerminated: Boolean = workers.isTerminated

  def awaitFinal(): ProfileFitError = synchronized {
    workers.awaitStopped()
    completed match
      case Some(value) => value
      case None =>
        val value = finish()
        completed = Some(value)
        value
  }

private enum ProfileBackend:
  case Fixed(prepared: ConditionProfilePreparation)
  case Compact(prepared: CompactConditionPreparation)
  case Trial(prepared: TrialBandedPreparation, criterion: TrialCriterionFacade)

private[profile] enum ProfileWorkFailure:
  case Dataset(detail: String)
  case Backend(detail: String)
  case TrialReadout(error: ProfileTrialReadoutError)
  case TrialMl(error: ProfileMlError)
  case Cancelled

private[profile] final case class ProfileTrialEvaluation(
    decoded: ShapeDecodeResult,
    evidence: Option[ProfileCriterionEvidence],
    exactReadout: () => Either[ProfileWorkFailure, TrialBandedReadout],
    mlReadout: Option[() => Either[ProfileWorkFailure, TrialMlReadoutPayload]] = None,
    mlReadoutWork: () => TrialBandedWorkSnapshot = () => ProfileHrfFit.sumTrialWork(Vector.empty),
    mlMeasurementWork: () => TrialResidualMeasurementWork = () => TrialResidualMeasurementWork(),
    mlScratchValues: Option[(Int, Int)] = None)

private[profile] trait TrialCriterionWorker:
  def evaluate(response: Array[Double], counters: DecoderCounters, observed: DecodeStatus => Unit): Either[ProfileWorkFailure, ProfileTrialEvaluation]
  def numericalWork: TrialBandedWorkSnapshot
  def mlWork: Option[TrialMlWork] = None
  def mlScratchValues: Option[(Int, Int)] = None

private[profile] enum TrialCriterionFacade:
  case Penalized(bank: TrialBandedObjective)
  case Ml(bank: TrialBandedObjective, bundle: TrialBandedMlBackend, sigma2: Double)

  def objectiveBank: TrialBandedObjective = this match
    case Penalized(bank) => bank
    case Ml(bank, _, _) => bank

  def mlSetup: Option[TrialMlWork] = this match
    case Penalized(_) => None
    case Ml(_, bundle, _) => Some(bundle.setupReceipt)

  def newWorker(policy: ProfileDecodePolicy, noiseVariance: Double): TrialCriterionWorker = this match
    case Penalized(bank) => new TrialCriterionWorker:
      private val objective = bank.newWorker()
      private val decoder = new ShapeDecoder(objective, policy.budget, policy.prior, noiseVariance)
      private val buffer = bank.preparation.newResponseBuffer
      def numericalWork: TrialBandedWorkSnapshot = objective.work.snapshot
      def evaluate(response: Array[Double], counters: DecoderCounters, observed: DecodeStatus => Unit): Either[ProfileWorkFailure, ProfileTrialEvaluation] =
        bank.preparation.encodeWhitenedInto(response, 0, buffer)
          .left.map(error => ProfileWorkFailure.Backend(error.message)).map { encoded =>
            objective.pointAt(encoded)
            val decoded = decoder.decode(counters)
            observed(decoded.status)
            ProfileTrialEvaluation(decoded, None,
              () => objective.readout(TrialReadoutFactorMode.ExactShape(decoded.coordinates))
                .left.map(error => ProfileWorkFailure.Backend(error.message)))
          }
    case Ml(_, bundle, sigma2) => new TrialCriterionWorker:
      private val backend = bundle.newWorker()
      private val decoder = TrialMlDecoder.checked(Some(backend), sigma2, policy.budget, policy.prior)
      def numericalWork: TrialBandedWorkSnapshot = backend.legacyWork
      override def mlWork: Option[TrialMlWork] = Some(backend.workerWorkSnapshot)
      override def mlScratchValues: Option[(Int, Int)] =
        Some(backend.readoutScratchValues -> backend.measurementScratchValues)
      def evaluate(response: Array[Double], counters: DecoderCounters, observed: DecodeStatus => Unit): Either[ProfileWorkFailure, ProfileTrialEvaluation] =
        val attempt = decoder.flatMap(_.decode(response, counters).result)
        attempt.left.map(error => ProfileWorkFailure.TrialMl(ProfileMlError.Backend(error.message))).flatMap { result =>
          observed(result.decoded.status)
          result.terminalEvidence match
            case TerminalEvidence.Unavailable => Left(ProfileWorkFailure.TrialMl(ProfileMlError.TerminalUnavailable))
            case TerminalEvidence.Available(jet) =>
              val decoded = result.decoded
              if jet.raw.reference.coordinates != decoded.coordinates || jet.minimizationJet.hessian != decoded.dataHessian then
                Left(ProfileWorkFailure.TrialMl(ProfileMlError.TerminalMismatch("returned coordinates or HJ")))
              else jet.determinant.toRight(ProfileWorkFailure.TrialMl(ProfileMlError.TerminalMismatch("missing determinant"))).map { det =>
                val evidence = ProfileCriterionEvidence.TrialRandomEffectsML(sigma2, jet.raw.jet,
                  det.value, det.gradient, det.hessian, jet.minimizationJet, jet.score, 2.0 * sigma2, decoded.dataHessian)
                var cached: Option[Either[ProfileWorkFailure, TrialBandedReadout]] = None
                var cachedPayload: Option[Either[ProfileWorkFailure, TrialMlReadoutPayload]] = None
                def check(raw: TrialBandedReadout): Either[ProfileWorkFailure, TrialBandedReadout] =
                  def agrees(a: Double, b: Double): Boolean =
                    a.isFinite && b.isFinite && math.abs(a - b) <= 1e-8 * math.max(1.0, math.max(math.abs(a), math.abs(b)))
                  if raw.factorMode != TrialReadoutFactorMode.ExactShape(decoded.coordinates) ||
                      !agrees(raw.penalizedEnergy, jet.raw.jet.energy) ||
                      raw.conditionMeans.length != jet.raw.jet.amplitudes.length ||
                      !raw.conditionMeans.zip(jet.raw.jet.amplitudes).forall((a, b) => agrees(a, b)) then
                    Left(ProfileWorkFailure.TrialMl(ProfileMlError.TerminalMismatch("exact readout raw E or condition coefficients")))
                  else Right(raw)
                def payload(): Either[ProfileWorkFailure, TrialMlReadoutPayload] =
                  backend.validateReadoutEpoch(jet.raw.epoch)
                    .left.map(error => ProfileWorkFailure.TrialMl(ProfileMlError.Backend(error.message))) match
                    case Left(error) => return Left(error)
                    case _ => ()
                  cachedPayload match
                    case Some(value) => value
                    case None =>
                      val value = backend.exactReadoutPayload(decoded.coordinates, jet.raw.epoch)
                        .left.map(error => ProfileWorkFailure.TrialMl(ProfileMlError.Backend(error.message)))
                        .flatMap(value => check(value.raw).map(_ => value))
                      cachedPayload = Some(value)
                      value
                def raw(): Either[ProfileWorkFailure, TrialBandedReadout] =
                  backend.validateReadoutEpoch(jet.raw.epoch)
                    .left.map(error => ProfileWorkFailure.TrialMl(ProfileMlError.Backend(error.message))) match
                    case Left(error) => return Left(error)
                    case _ => ()
                  cached match
                    case Some(value) => value
                    case None =>
                      val value = backend.exactReadout(decoded.coordinates)
                        .left.map(error => ProfileWorkFailure.TrialMl(ProfileMlError.Backend(error.message)))
                        .flatMap(check)
                      cached = Some(value)
                      value
                ProfileTrialEvaluation(decoded, Some(evidence), () => raw(), Some(() => payload()),
                  () => backend.readoutWork, () => backend.measurementWork,
                  Some(backend.readoutScratchValues -> backend.measurementScratchValues))
              }
        }

private[profile] trait ProfilePayload[V, P]:
  def publicOutputs: Boolean = false
  def publicExecution: Option[ProfileTrialExecutionDeclaration] = None
  def condition(voxelId: Int, fit: CompactConditionReadout, normalization: scalafim.fmri.hrf.family.NormalizationRule): V
  def trial(voxelId: Int, evaluation: ProfileTrialEvaluation,
    whitened: Array[Double], work: ProfileTrialOutputWork): Either[ProfileWorkFailure, V]
  def block(ordinal: Int, ids: Vector[Int], values: Vector[V]): P
  def ids(payload: P): Vector[Int]
  def retainedTrialValues(payload: P): Long = 0L

private object LegacyProfilePayload extends ProfilePayload[ProfileVoxelResult, ProfileFitBlock]:
  def condition(voxelId: Int, fit: CompactConditionReadout, normalization: scalafim.fmri.hrf.family.NormalizationRule): ProfileVoxelResult =
    ProfileVoxelResult(voxelId, fit.decode.coordinates, fit.decode.status,
      fit.residualEnergy, fit.amplitudes, ProfileAmplitudeReadout.ConditionMeans(fit.amplitudes,
        normalization), fit.decode.conditionalSd)
  def trial(voxelId: Int, evaluation: ProfileTrialEvaluation,
      whitened: Array[Double], work: ProfileTrialOutputWork): Either[ProfileWorkFailure, ProfileVoxelResult] =
    val decoded = evaluation.decoded
    if !decoded.energy.isFinite || decoded.coordinates.exists(!_.isFinite) then
      Left(ProfileWorkFailure.Backend("nonfinite decoded point; exact conditional readout unavailable"))
    else evaluation.exactReadout()
      .map(readout => ProfileVoxelResult(voxelId, decoded.coordinates, decoded.status,
        readout.penalizedEnergy, readout.conditionMeans, ProfileAmplitudeReadout.AdaptiveTrial(readout),
        decoded.conditionalSd, evaluation.evidence))
  def block(ordinal: Int, ids: Vector[Int], values: Vector[ProfileVoxelResult]): ProfileFitBlock =
    ProfileFitBlock(ordinal, ids, values)
  def ids(payload: ProfileFitBlock): Vector[Int] = payload.voxelIds

private final class ProfileWorkerAbort extends RuntimeException with scala.util.control.NoStackTrace

/** Shared preparation is response-independent. Each execution owns mutable
  * workers and retains only sink receipts, never prior voxel payloads.
  */
final class PreparedProfileHrf private[profile] (
    val plan: ProfileHrfPlan,
    val selection: DataSelection,
    val policy: ProfileDecodePolicy,
    val dataset: FmriDataset,
    val selected: ResolvedDataSelection,
    val setup: ProfileSetupReceipt,
    val provenance: String,
    private val backend: ProfileBackend):

  private[profile] def isTrial: Boolean = backend match
    case ProfileBackend.Trial(_, _) => true
    case _ => false

  private[profile] def ownsTrialBank(preparation: TrialBandedPreparation, bank: TrialBandedObjective): Boolean =
    backend match
      case ProfileBackend.Trial(actual, criterion) => (actual eq preparation) && (criterion.objectiveBank eq bank)
      case _ => false

  lazy val trialOutputs: Either[ProfileFitError, PreparedProfileTrialOutputs] = backend match
    case ProfileBackend.Trial(prepared, criterion) => PreparedProfileTrialOutputs.make(this, prepared, criterion.objectiveBank)
    case _ => Left(ProfileFitError.Unsupported("public trial outputs require the trial backend"))

  def run(
      reader: DatasetSeriesReader,
      sink: BlockSink[ProfileFitBlock, ProfileFitReceipt],
      cancelled: () => Boolean = () => false
  ): Either[ProfileFitError, ProfileRunSummary] =
    backend match
      case ProfileBackend.Fixed(_) => ()
      case _ =>
        return runWithExecutor(Vector(reader), sink, cancelled, parallel = false,
          (n, b, factory, target, stop) => BlockExecutor.runSequential(n, b, factory, target, stop))
    val counters = new DecoderCounters
    var fixedCounters: Option[DecoderCounters] = None
    var deliveredBlocks = 0
    var deliveredVoxels = 0
    var attemptedVoxels = 0
    val receipts = Vector.newBuilder[ProfileFitReceipt]
    def progress: ProfileRunProgress =
      ProfileRunProgress(deliveredBlocks, deliveredVoxels, attemptedVoxels, 1,
        ProfileDecoderWork.from(fixedCounters.getOrElse(counters)), None)
    def checkedCancel: Either[ProfileFitError, Unit] =
      try
        if cancelled() then Left(ProfileFitError.Cancelled(progress)) else Right(())
      catch case NonFatal(error) => Left(ProfileFitError.Backend(s"cancellation callback: $error", progress))
    val sameReader = try ProfileHrfFit.sameDataset(reader.dataset, dataset)
      catch case NonFatal(error) => return Left(ProfileFitError.Dataset(s"reader descriptor threw: $error", progress))
    if !sameReader then
      return Left(ProfileFitError.Dataset("reader descriptor differs from the plan's dataset", progress))
    backend match
      case ProfileBackend.Fixed(prepared) =>
        val worker = new prepared.Worker
        fixedCounters = Some(worker.counters)
        var localFailure: Option[ProfileFitError] = None
        var readerFailure: Option[Throwable] = None
        val guardedReader = new DatasetSeriesReader:
          val dataset: FmriDataset = PreparedProfileHrf.this.dataset
          def seriesEither(selection: DataSelection) =
            try reader.seriesEither(selection)
            catch case NonFatal(error) =>
              readerFailure = Some(error)
              throw error
        val outcome = try prepared.retention.foreachBlock(guardedReader, block =>
          val results = Vector.newBuilder[ProfileVoxelResult]
          var within = 0
          while within < block.voxelIndices.length && localFailure.isEmpty do
            checkedCancel match
              case Left(error) => localFailure = Some(error)
              case Right(_) =>
                try
                  attemptedVoxels += 1
                  val fit = worker.fitRaw(block.voxelIndices(within), block.product.crossProducts.col(within), block.product.responseSquares(within))
                  val means = fit.amplitudes.map(_.value)
                  results += ProfileVoxelResult(fit.voxel, fit.coordinates, fit.status, fit.residualEnergy,
                    means, ProfileAmplitudeReadout.ConditionMeans(means, plan.basis.family.libraryNormalization), fit.conditionalSd)
                catch case NonFatal(error) => localFailure = Some(ProfileFitError.Backend(error.toString, progress))
            within += 1
          localFailure match
            case Some(error) => Left(scalafim.fmri.fit.FitError.InvalidFitAxis("profile fit", error.message))
            case None =>
              val ids = block.voxelIndices
              deliver(VoxelBlock(block.ordinal.value, deliveredVoxels, ids.length), ProfileFitBlock(block.ordinal.value, ids, results.result()), sink, checkedCancel, progress) match
                case Left(error) =>
                  localFailure = Some(error)
                  Left(scalafim.fmri.fit.FitError.InvalidFitAxis("profile fit", error.message))
                case Right(receipt) =>
                  receipts += receipt
                  deliveredBlocks += 1
                  deliveredVoxels += ids.length
                  Right(())
        , () => checkedCancel match
          case Left(error) =>
            localFailure = Some(error)
            true
          case Right(_) => false)
        catch case NonFatal(error) =>
          return Left(localFailure.getOrElse(readerFailure match
            case Some(cause) => ProfileFitError.Dataset(cause.toString, progress)
            case None => ProfileFitError.Backend(error.toString, progress)))
        outcome match
          case Left(error) => Left(localFailure.getOrElse(ProfileFitError.Dataset(error.message, progress)))
          case Right(EstimateExecutionOutcome.Cancelled(_, _)) => Left(localFailure.getOrElse(ProfileFitError.Cancelled(progress)))
          case Right(EstimateExecutionOutcome.Completed(_, _)) =>
            Right(ProfileRunSummary(receipts.result(), progress.copy(decoder = ProfileDecoderWork.from(worker.counters)), setup, provenance))
      case _ => Left(ProfileFitError.Backend("unexpected profile backend", progress))

  /** The platform supplies the executor; all scientific work and failure
    * translation stay shared. Readers are caller-owned and never closed here.
    */
  private[profile] def runWithExecutor(
      readers: Vector[DatasetSeriesReader],
      sink: BlockSink[ProfileFitBlock, ProfileFitReceipt],
      cancelled: () => Boolean,
      parallel: Boolean,
      execute: (Int, ExecutionBudget, () => BlockWorker[ProfileFitBlock],
        BlockSink[ProfileFitBlock, ProfileFitReceipt], () => Boolean) =>
        Either[ExecutionError, ExecutionSummary[ProfileFitReceipt]]
  ): Either[ProfileFitError, ProfileRunSummary] =
    runPayload(readers, sink, cancelled, parallel, LegacyProfilePayload, execute)

  private[profile] def runPayload[V, P](
      readers: Vector[DatasetSeriesReader],
      sink: BlockSink[P, ProfileFitReceipt],
      cancelled: () => Boolean,
      parallel: Boolean,
      payload: ProfilePayload[V, P],
      execute: (Int, ExecutionBudget, () => BlockWorker[P],
        BlockSink[P, ProfileFitReceipt], () => Boolean) =>
        Either[ExecutionError, ExecutionSummary[ProfileFitReceipt]]
  ): Either[ProfileFitError, ProfileRunSummary] =
    val publicExecution = payload.publicExecution
    val runProvenance = publicExecution.fold(provenance)(_.provenance)
    if parallel && !isTrial then
      return Left(ProfileFitError.Unsupported("parallel execution is available for the trial route only"))
    val voxels = selected.voxels
    val blocks = BlockExecutor.blocks(voxels.length, policy.execution.blockSize)
    val expectedReaders = if blocks.isEmpty then 0 else if parallel then math.min(policy.execution.workers, blocks.length) else 1
    val empty = ProfileRunProgress(0, 0, 0, 0,
      ProfileDecoderWork(0, 0, 0, 0, 0, 0, 0, 0), None, publicExecution = publicExecution,
      trialMl = setup.mlSetup.map(_ => TrialMlWork()))
    if readers.length != expectedReaders then
      return Left(ProfileFitError.Dataset(s"expected $expectedReaders caller-owned readers; got ${readers.length}", empty))
    if readers.indices.exists(i => (0 until i).exists(j => readers(i) eq readers(j))) then
      return Left(ProfileFitError.Dataset("each worker needs a distinct reader instance", empty))
    var readerIndex = 0
    while readerIndex < readers.length do
      val matches = try ProfileHrfFit.sameDataset(readers(readerIndex).dataset, dataset)
        catch case NonFatal(error) =>
          return Left(ProfileFitError.Dataset(s"reader $readerIndex descriptor threw: $error", empty))
      if !matches then return Left(ProfileFitError.Dataset(s"reader $readerIndex descriptor differs from the plan's dataset", empty))
      readerIndex += 1

    val lock = new Object
    val states = new Array[ProfileBlockWorker](expectedReaders)
    val failures = scala.collection.mutable.Map.empty[Int, ProfileWorkFailure]
    var allocated = 0
    var callbackFailure: Option[String] = None
    var sinkFailure: Option[(Boolean, String)] = None
    var sinkCancelled = false
    var deliveredBlocks = 0
    var deliveredVoxels = 0
    var emittedTrialValues = 0L
    val completedReceipts = Vector.newBuilder[ProfileFitReceipt]
    val grid = NodeGrid(plan.basis.family.chart, policy.nodesPerAxis)
    val rows = dataset.shape.timepoints

    def stop(): Boolean =
      try cancelled()
      catch case NonFatal(error) =>
        lock.synchronized { callbackFailure = Some(error.toString) }
        true

    final class ProfileBlockWorker(val reader: DatasetSeriesReader) extends BlockWorker[P]:
      val counters = new DecoderCounters
      val compact = backend match
        case ProfileBackend.Compact(prepared) => Some(CompactConditionRuntime.raw(prepared, grid, policy.budget,
          policy.prior, plan.criterion.noiseVariance, plan.basis.family.libraryNormalization))
        case _ => None
      val trial = backend match
        case ProfileBackend.Trial(_, criterion) => Some(criterion.newWorker(policy, plan.criterion.noiseVariance))
        case _ => None
      val statuses = scala.collection.mutable.Map.empty[DecodeStatus, Long]
      var attempted = 0
      val publicWork = new ProfileTrialOutputWork
      // Native ML allocates these worker arrays even if decoding refuses.
      // Record persistent storage at worker creation, before any voxel result.
      if payload.publicOutputs then trial.flatMap(_.mlScratchValues).foreach { (coefficients, measurement) =>
        publicWork.coefficientHighWater = coefficients
        publicWork.measurementScratch = measurement
      }

      def process(block: VoxelBlock): P =
        def abort(error: ProfileWorkFailure): Nothing =
          lock.synchronized { failures.update(block.index, error) }
          throw new ProfileWorkerAbort
        def checkCancel(): Unit =
          if stop() then
            lock.synchronized(callbackFailure) match
              case Some(detail) => abort(ProfileWorkFailure.Backend(s"cancellation callback: $detail"))
              case None => abort(ProfileWorkFailure.Cancelled)
        checkCancel()
        val ids = voxels.slice(block.start, block.end)
        val request = DataSelection(TimepointSelection.All,
          VoxelSelection.Indices(selected.voxelIndexValues.slice(block.start, block.end)))
        val series =
          try reader.seriesEither(request).left.map(error => ProfileWorkFailure.Dataset(error.message))
          catch case NonFatal(error) => Left(ProfileWorkFailure.Dataset(error.toString))
        val value = series.fold(abort, identity)
        if value.shape != dataset.shape || value.timepoints != selected.timepoints || value.voxelIndices != ids ||
            value.nTimepoints != rows || value.nVoxels != ids.length then
          abort(ProfileWorkFailure.Dataset("returned shape or time/voxel axes differ from the requested block"))
        val raw = new Array[Double](rows * ids.length)
        try value.data.copyRowMajorTo(raw)
        catch case NonFatal(error) => abort(ProfileWorkFailure.Dataset(error.toString))
        if raw.exists(!_.isFinite) then abort(ProfileWorkFailure.Dataset("nonfinite response under MissingDataPolicy.Error"))
        val whitened: Either[ProfileWorkFailure, Array[Double]] = backend match
          case ProfileBackend.Compact(prepared) => prepared.whiten(ids.length, raw).left.map(error => ProfileWorkFailure.Backend(error.message))
          case ProfileBackend.Trial(prepared, _) => prepared.whitenResponses(ids.length, raw).left.map(error => ProfileWorkFailure.Backend(error.message))
          case _ => abort(ProfileWorkFailure.Backend("unexpected fixed backend"))
        val matrix = whitened.fold(abort, identity)
        val contiguous = new Array[Double](rows)
        val results = Vector.newBuilder[V]
        var within = 0
        while within < ids.length do
          checkCancel()
          var t = 0
          while t < rows do
            contiguous(t) = matrix(t * ids.length + within)
            t += 1
          attempted += 1
          val result = try
            backend match
              case ProfileBackend.Compact(_) =>
                val fit = compact.get.fitRaw(contiguous, 0, counters)
                statuses.update(fit.decode.status, statuses.getOrElse(fit.decode.status, 0L) + 1L)
                Right(payload.condition(ids(within), fit, plan.basis.family.libraryNormalization))
              case ProfileBackend.Trial(_, _) =>
                trial.get.evaluate(contiguous, counters,
                  status => statuses.update(status, statuses.getOrElse(status, 0L) + 1L)).flatMap { evaluation =>
                  payload.trial(ids(within), evaluation, contiguous, publicWork)
                }
              case _ => Left(ProfileWorkFailure.Backend("unexpected fixed backend"))
          catch case NonFatal(error) => Left(ProfileWorkFailure.Backend(error.toString))
          results += result.fold(abort, identity)
          within += 1
        payload.block(block.index, ids, results.result())

    def progress: ProfileRunProgress =
      val live = states.iterator.take(allocated).toVector
      val decoder = live.foldLeft(ProfileDecoderWork(0, 0, 0, 0, 0, 0, 0, 0)) { (sum, worker) =>
        val c = worker.counters
        ProfileDecoderWork(sum.voxels + c.voxels, sum.nodeScores + c.nodeScores, sum.jets + c.jets,
          sum.exactEvaluations + c.exactEvaluations, sum.candidateAttempts + c.candidateAttempts,
          sum.terminalVerifications + c.terminalVerifications, sum.newtonSteps + c.newtonSteps,
          sum.fallbacks + c.fallbacks, sum.firstOrderAttempts + c.firstOrderAttempts)
      }
      val statuses = live.flatMap(_.statuses).groupMapReduce(_._1)(_._2)(_ + _)
      val snapshots = live.flatMap(worker => worker.trial.map(_.numericalWork).toVector ++
        (if payload.publicOutputs && setup.mlSetup.isEmpty then Vector(worker.publicWork.numerical) else Vector.empty))
      val trialWork = if !isTrial then None else Some(ProfileHrfFit.sumTrialWork(snapshots))
      ProfileRunProgress(deliveredBlocks, deliveredVoxels, live.map(_.attempted).sum, live.length,
        decoder, trialWork, statuses,
        if payload.publicOutputs then Some(ProfileTrialOutputProgress.aggregate(live.map(_.publicWork), emittedTrialValues)) else None,
        publicExecution,
        setup.mlSetup.map(_ => live.flatMap(_.trial.flatMap(_.mlWork)).foldLeft(TrialMlWork())(_ + _)))

    val factory = () => lock.synchronized {
      val slot = allocated
      val worker = new ProfileBlockWorker(readers(slot))
      states(slot) = worker
      allocated += 1
      worker
    }
    val guardedSink = new BlockSink[P, ProfileFitReceipt]:
      def accept(block: VoxelBlock, value: P): Either[String, ProfileFitReceipt] =
        if stop() then
          sinkCancelled = true
          return Left("cancelled before sink delivery")
        val accepted = try sink.accept(block, value)
          catch case NonFatal(error) =>
            sinkFailure = Some((true, error.toString))
            Left(error.toString)
        accepted match
          case Left(detail) =>
            if sinkFailure.isEmpty then sinkFailure = Some((false, detail))
            Left(detail)
          case Right(receipt) if receipt.ordinal != block.index || receipt.voxelIds != payload.ids(value) =>
            sinkFailure = Some((false, "receipt axis differs from delivered block"))
            Left("receipt axis differs from delivered block")
          case Right(receipt) =>
            deliveredBlocks += 1
            deliveredVoxels += block.count
            emittedTrialValues += payload.retainedTrialValues(value)
            completedReceipts += receipt
            Right(receipt)
    def finalFailure(error: ExecutionError): ProfileFitError =
      val p = progress
      error match
        case ExecutionError.InvalidBudget(detail) => publicExecution match
          case Some(declaration) => ProfileFitError.TrialExecutionAdmission(detail, declaration)
          case None => ProfileFitError.Unsupported(detail)
        case ExecutionError.Cancelled(_) => callbackFailure match
          case Some(detail) => ProfileFitError.Backend(s"cancellation callback: $detail", p)
          case None => ProfileFitError.Cancelled(p)
        case ExecutionError.SinkFailed(_, detail) =>
          if sinkCancelled then callbackFailure match
            case Some(reason) => ProfileFitError.Backend(s"cancellation callback: $reason", p)
            case None => ProfileFitError.Cancelled(p)
          else sinkFailure match
            case Some((true, reason)) => ProfileFitError.SinkThrew(reason, p)
            case _ => ProfileFitError.SinkRefused(detail, p)
        case ExecutionError.WorkerFailed(block, detail) =>
          failures.get(block.index) match
            case Some(ProfileWorkFailure.Dataset(reason)) => ProfileFitError.Dataset(reason, p)
            case Some(ProfileWorkFailure.Backend(reason)) => ProfileFitError.Backend(reason, p)
            case Some(ProfileWorkFailure.TrialReadout(error)) => ProfileFitError.TrialReadoutFailure(error, p)
            case Some(ProfileWorkFailure.TrialMl(error)) => ProfileFitError.TrialMlFailure(error, p)
            case Some(ProfileWorkFailure.Cancelled) => ProfileFitError.Cancelled(p)
            case None => ProfileFitError.Backend(detail, p)
        case ExecutionError.WorkersStillRunning(_, _, _) =>
          ProfileFitError.Backend("nested unfinished worker outcome", p)
    val result = execute(voxels.length, policy.execution, factory, guardedSink, stop)
    result match
      case Right(summary) => Right(ProfileRunSummary(summary.receipts, progress, setup, runProvenance))
      case Left(ExecutionError.WorkersStillRunning(original, _, termination)) =>
        Left(ProfileFitError.WorkersStillRunning(original, completedReceipts.result(), setup, runProvenance,
          new ProfileRunTermination(termination, () => finalFailure(original), publicExecution)))
      case Left(error) => Left(finalFailure(error))

  private def deliver(
      block: VoxelBlock,
      payload: ProfileFitBlock,
      sink: BlockSink[ProfileFitBlock, ProfileFitReceipt],
      checkedCancel: => Either[ProfileFitError, Unit],
      progress: => ProfileRunProgress
  ): Either[ProfileFitError, ProfileFitReceipt] =
    checkedCancel.flatMap { _ =>
      try sink.accept(block, payload).left.map(detail => ProfileFitError.SinkRefused(detail, progress))
      catch case NonFatal(error) => Left(ProfileFitError.SinkThrew(error.toString, progress))
    }

object ProfileHrfFit:
  /** Preserve both the successful bank setup and partial ML setup on refusal. */
  private[profile] def mlSetupResult(bank: TrialBandedSetupReceipt, attempt: TrialMlAttempt[TrialBandedMlBackend])
      : Either[ProfileFitError, TrialBandedMlBackend] =
    attempt.result.left.map(error => ProfileFitError.TrialMlPreparation(error.message, bank, attempt.work))

  private[profile] def sumTrialWork(parts: Vector[TrialBandedWorkSnapshot]): TrialBandedWorkSnapshot =
    val attempted = parts.map(_.attempted).foldLeft(TrialBandedAttemptedWorkSnapshot(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)) { (a, b) =>
      TrialBandedAttemptedWorkSnapshot(
        a.referenceAttempts + b.referenceAttempts, a.referenceFailures + b.referenceFailures,
        a.partialReferenceFailures + b.partialReferenceFailures, a.releaseFailures + b.releaseFailures,
        a.factorAttempts + b.factorAttempts, a.factorFailures + b.factorFailures,
        a.solveAttempts + b.solveAttempts, a.solveFailures + b.solveFailures,
        a.rightHandSideAttempts + b.rightHandSideAttempts, a.rightHandSideFailures + b.rightHandSideFailures,
        a.jetAttempts + b.jetAttempts, a.jetFailures + b.jetFailures,
        a.readoutAttempts + b.readoutAttempts, a.readoutFailures + b.readoutFailures,
        a.exactReadoutFactorAttempts + b.exactReadoutFactorAttempts,
        a.exactReadoutFactorFailures + b.exactReadoutFactorFailures,
        a.conditionalReadoutAttempts + b.conditionalReadoutAttempts,
        a.conditionalReadoutFailures + b.conditionalReadoutFailures,
        a.conditionalInverseAttempts + b.conditionalInverseAttempts,
        a.conditionalInverseFailures + b.conditionalInverseFailures,
        a.conditionalCorrectionAttempts + b.conditionalCorrectionAttempts,
        a.conditionalCorrectionFailures + b.conditionalCorrectionFailures,
        a.firstOrderAttempts + b.firstOrderAttempts, a.firstOrderFailures + b.firstOrderFailures,
        a.reconstructedBands + b.reconstructedBands, a.reconstructedBandProducts + b.reconstructedBandProducts)
    }
    parts.foldLeft(TrialBandedWorkSnapshot(0, 0, 0, 0, 0, 0, 0, 0, 0, attempted)) { (a, b) =>
      TrialBandedWorkSnapshot(a.voxels + b.voxels, a.trialBasisScores + b.trialBasisScores,
        a.bankValueEvaluations + b.bankValueEvaluations, a.jetEvaluations + b.jetEvaluations,
        a.amplitudeCorrections + b.amplitudeCorrections, a.bandedSolveCalls + b.bandedSolveCalls,
        a.bandedRightHandSides + b.bandedRightHandSides, a.continuousFactors + b.continuousFactors,
        a.exactReadoutFactors + b.exactReadoutFactors, attempted)
    }

  def prepare(
      plan: ProfileHrfPlan,
      selection: DataSelection,
      whitening: CanonicalTemporalWhitening,
      policy: ProfileDecodePolicy
  ): Either[ProfileFitError, PreparedProfileHrf] =
    if plan.criterion.usesDeterminant then
      (plan.source, plan.amplitudes) match
        case (ProfileHrfSource.TrialEvents(_, _, _, _), AmplitudeStructure.ConditionCenteredTrials(_)) => ()
        case _ => return Left(ProfileFitError.Unsupported("ML requires positive-alpha trial events"))
    val dataset = plan.source match
      case ProfileHrfSource.FixedCondition(fixed, _) => fixed.model.dataset
      case ProfileHrfSource.TrialEvents(source, _, _, _) => source
    for
      budget <- policy.execution.validate.left.map(error => ProfileFitError.Unsupported(error.message))
      _ <- validateGrid(plan, policy)
      selected <- dataset.resolve(selection).left.map(error => ProfileFitError.Unsupported(error.message))
      _ <- if selected.timepoints == (0 until dataset.shape.timepoints).toVector then Right(())
           else Left(ProfileFitError.Unsupported("only the full ordered time frame is implemented"))
      source <- prepareSource(plan, selection, whitening, policy)
      compactComparison <- source._1 match
        case ProfileBackend.Compact(prepared) =>
          CompactComparisonWorkspaceReceipt.estimate(prepared,
            CompactComparisonWorkspaceReceipt.enabled(policy.prior, policy.budget))
            .left.map(error => ProfileFitError.Unsupported(error.message)).map(Some(_))
        case _ => Right(None)
    yield
      val (backend, route) = source
      val setup = backend match
        case ProfileBackend.Trial(prepared, criterion) =>
          val bank = criterion.objectiveBank
          ProfileSetupReceipt(route, Some(prepared.receipt), Some(bank.estimatedSharedBytes),
            Some(prepared.retainedSourceDesignDataValues), None, Some(bank.setupReceipt), criterion.mlSetup,
            criterion match
              case TrialCriterionFacade.Ml(_, bundle, _) => Some(bundle.energyScratchValues)
              case _ => None)
        case ProfileBackend.Compact(_) =>
          ProfileSetupReceipt(route, None, None, None, policy.observedAdmission.map(_.fingerprint),
            compactComparisonPerWorker = compactComparison)
        case _ => ProfileSetupReceipt(route, None, None, None, policy.observedAdmission.map(_.fingerprint))
      val mlIdentity = backend match
        case ProfileBackend.Trial(_, TrialCriterionFacade.Ml(_, bundle, sigma2)) => Some((sigma2, bundle.intrinsicLambda))
        case _ => None
      val boundProvenance = ProfileFitIdentity.canonical(plan, selected, whitening, policy, budget,
        route, backend.isInstanceOf[ProfileBackend.Trial], mlIdentity)
      new PreparedProfileHrf(plan, selection, policy, dataset, selected, setup, boundProvenance, backend)

  private def validateGrid(plan: ProfileHrfPlan, policy: ProfileDecodePolicy): Either[ProfileFitError, Unit] =
    val d = plan.basis.family.dimension
    val sizes = policy.nodesPerAxis
    val count = sizes.foldLeft(1L)((n, size) => n * size)
    if sizes.length != d || sizes.exists(_ < 2) || count > 100000L || count <= 0L then
      Left(ProfileFitError.Unsupported("grid dimensions must match the chart with 2..100000 total nodes"))
    else policy.prior match
      case None => Right(())
      case Some(p) if p.dimension != d || p.mean.exists(!_.isFinite) || p.precision.exists(!_.isFinite) =>
        Left(ProfileFitError.Unsupported("shape prior must have finite chart-dimensional mean and precision"))
      case Some(p) =>
        val precision = DMat.tabulate(d, d)((i, j) => p.precision(i * d + j))
        precision.verifySymmetric(0.0).left.map(_ => ProfileFitError.Unsupported("shape prior precision must be symmetric")).flatMap { _ =>
          Eigen.eigSymmetric(precision, EigenSelection.All, EigenVectors.ValuesOnly)
            .left.map(error => ProfileFitError.Unsupported(s"shape prior spectrum failed: $error"))
            .flatMap(spectrum =>
              if (0 until spectrum.size).forall(i => spectrum.eigenvalues(i) >= 0.0) then Right(())
              else Left(ProfileFitError.Unsupported("shape prior precision must be positive semidefinite")))
        }

  private def prepareSource(
      plan: ProfileHrfPlan,
      selection: DataSelection,
      whitening: CanonicalTemporalWhitening,
      policy: ProfileDecodePolicy
  ): Either[ProfileFitError, (ProfileBackend, String)] =
    plan.source match
      case ProfileHrfSource.FixedCondition(fixed, term) =>
        if fixed.engine != FitEngine.OrdinaryLeastSquares then
          Left(ProfileFitError.Unsupported("fixed condition retention requires ordinary least squares"))
        else if selection != DataSelection.All then
          Left(ProfileFitError.Unsupported("fixed condition retention currently requires DataSelection.All"))
        else
          val admission = policy.observedAdmission.toRight(ProfileFitError.Unsupported("condition routing requires observed-family admission"))
          for
            _ <- validateConfig(fixed.config, whitening, fixed.model.dataset, fixed.model.designMatrix).flatMap(_ =>
              if whitening == CanonicalTemporalWhitening.Iid then Right(())
              else Left(ProfileFitError.Unsupported("fixed condition OLS retention does not support AR whitening")))
            held <- admission
            structure <- ConditionProfileFit.structureFor(fixed, term).left.map(error => ProfileFitError.Preparation(error.message))
            expanded <- ExpandedConditionDesign.lower(term.term, fixed.model.dataset.samplingFrame, plan.basis,
              plan.basis.spec.fineStep.seconds).left.map(error => ProfileFitError.Preparation(error.message))
            nuisance <- fixedNuisance(fixed, structure)
            _ <- held.admits(expanded, term.term, fixed.model.dataset.samplingFrame, plan.basis.spec.fineStep.seconds, None,
              nuisance).left.map(error => ProfileFitError.Preparation(error.message))
            prepared <- ConditionProfileFit.prepareRaw(fixed, ConditionProfilePolicy(plan.basis, structure, policy.nodesPerAxis,
              policy.budget, policy.prior, plan.criterion.noiseVariance,
              OutputRequest.ConditionAmplitudes(plan.basis.family.libraryNormalization), held,
              policy.execution.blockSize)).left.map(error => ProfileFitError.Preparation(error.message))
          yield (ProfileBackend.Fixed(prepared), "fixed-condition-ols")
      case ProfileHrfSource.TrialEvents(dataset, drive, baseline, config) =>
        for
          _ <- validateConfig(config, whitening, dataset, baseline.designMatrix)
          prepared <- plan.amplitudes match
            case AmplitudeStructure.ConditionMeans =>
              for
                held <- policy.observedAdmission.toRight(ProfileFitError.Unsupported("condition routing requires observed-family admission"))
                levels <- FactorLevelSet.from(FactorId.unsafe("condition"), drive.conditionLabels.map(_.value))
                  .left.map(error => ProfileFitError.Preparation(error.message))
                event <- Event.factorWithLevels(drive.conditionForTrial.map(_.value), "condition", levels)
                  .left.map(error => ProfileFitError.Preparation(error.message))
                term <- EventTerm.fromSchedule(Vector(event), drive.schedule, Some("condition"))
                  .left.map(error => ProfileFitError.Preparation(error.message))
                expanded <- ExpandedConditionDesign.lower(term, dataset.samplingFrame, plan.basis, plan.basis.spec.fineStep.seconds,
                  dropEmpty = false).left.map(error => ProfileFitError.Preparation(error.message))
                _ <- if expanded.conditionCount == drive.conditionLabels.length then Right(())
                     else Left(ProfileFitError.Preparation("condition lowering changed the declared condition axis"))
                compact <- CompactConditionPreparation.prepare(expanded, held, whiteningOption(whitening),
                  nuisance(baseline.designMatrix), term, dataset.samplingFrame, plan.basis.spec.fineStep.seconds)
                  .left.map(error => ProfileFitError.Preparation(error.message))
              yield (ProfileBackend.Compact(compact), "direct-condition-compact")
            case AmplitudeStructure.ConditionCenteredTrials(alpha) =>
              for
                expanded <- TrialBasisDesign.lower(drive.schedule.onsets, drive.schedule.blockIds,
                  drive.schedule.durations, drive.membership, dataset.samplingFrame, plan.basis,
                  plan.basis.spec.fineStep.seconds, policy.trialPreparation.lowering)
                  .left.map(error => ProfileFitError.Preparation(error.message))
                trial <- TrialBandedPreparation.prepare(expanded, whiteningOption(whitening),
                  nuisance(baseline.designMatrix), alpha.lambda, policy.trialPreparation.maxRetainedValues)
                  .left.map(error => ProfileFitError.Preparation(error.message))
                bank <- trial.objective(NodeGrid(plan.basis.family.chart, policy.nodesPerAxis))
                  .left.map(error => ProfileFitError.Preparation(error.message))
                criterion <-
                  if !plan.criterion.usesDeterminant then Right(TrialCriterionFacade.Penalized(bank))
                  else
                    val attempt = TrialBandedMlBackend.make(bank)
                    mlSetupResult(bank.setupReceipt, attempt)
                      .flatMap { bundle =>
                        if bundle.intrinsicLambda != alpha.lambda then
                          Left(ProfileFitError.TrialMlPreparation("native owner lambda differs from preparation", bank.setupReceipt, attempt.work))
                        else Right(TrialCriterionFacade.Ml(bank, bundle, plan.criterion.noiseVariance))
                      }
              yield (ProfileBackend.Trial(trial, criterion), if plan.criterion.usesDeterminant then "trial-banded-ml" else "trial-banded")
        yield prepared

  private def validateConfig(
      config: FitConfig,
      whitening: CanonicalTemporalWhitening,
      dataset: FmriDataset,
      actualDesign: scalafim.fmri.hrf.linalg.Mat
  ): Either[ProfileFitError, Unit] =
    if config.missingData != MissingDataPolicy.Error then Left(ProfileFitError.Unsupported("missing data policy must be Error"))
    else if config.volumeWeighting != VolumeWeighting.Disabled then Left(ProfileFitError.Unsupported("temporal volume weights are not implemented"))
    else if config.nuisanceProjection != NuisanceProjection.Disabled then Left(ProfileFitError.Unsupported("matrix nuisance projection is not implemented"))
    else if config.robust != RobustOptions() then Left(ProfileFitError.Unsupported("robust settings are not implemented"))
    else if config.lss != LssConfig() then Left(ProfileFitError.Unsupported("nondefault LSS settings are not implemented"))
    else if config.autocorrelation.censoredTimepoints.nonEmpty then Left(ProfileFitError.Unsupported("censoring is not implemented"))
    else if config.autocorrelation.voxelwise then Left(ProfileFitError.Unsupported("voxelwise AR is not implemented"))
    else if config.autocorrelation.iterations != 1 then
      Left(ProfileFitError.Unsupported("iterated AR settings are not implemented"))
    else if config.autocorrelation.structure != ArStructure.Iid && !config.autocorrelation.global then
      Left(ProfileFitError.Unsupported("fixed shared AR requires global coefficient scope in FitConfig"))
    else if config.autocorrelation.structure != ArStructure.Iid &&
        !whiteningOption(whitening).exists(_.pooling == NoisePooling.Global) then
      Left(ProfileFitError.Unsupported("fixed shared AR requires a global whitening coefficient scope"))
    else if config.autocorrelation.structure == ArStructure.Iid && config.autocorrelation != scalafim.fmri.model.ArOptions() then
      Left(ProfileFitError.Unsupported("nondefault IID autocorrelation settings are not implemented"))
    else if config.autocorrelation.structure != ArStructure.Iid &&
        config.autocorrelation.rho.isEmpty && config.autocorrelation.phi.isEmpty then
      Left(ProfileFitError.Unsupported("estimated AR coefficients are not implemented"))
    else
      val rows = dataset.shape.timepoints
      val partitions = RunPartition.fromSamplingFrame(dataset.samplingFrame, (0 until rows).toVector)
      val expectedSegments = Gls.timeSegments(partitions, Vector.empty).left.map(error => ProfileFitError.Unsupported(error.message))
      expectedSegments.flatMap { segments =>
        val identity = (config.autocorrelation.structure, whitening) match
          case (ArStructure.Iid, CanonicalTemporalWhitening.Iid) => Right(())
          case (ArStructure.Ar(order), CanonicalTemporalWhitening.Shared(plan)) =>
            val requested = config.autocorrelation.phi.getOrElse(config.autocorrelation.rho.toVector)
            if plan.coefficients.length != 1 || plan.coefficients.head.phi != requested ||
                plan.coefficients.head.theta.nonEmpty || plan.arOrder != order ||
                plan.initialCondition != InitialConditionPolicy.fromExactFirstAr1(config.autocorrelation.exactFirst) ||
                plan.segments != segments then
              Left(ProfileFitError.Unsupported("fixed shared AR coefficients, initial condition or segments differ from FitConfig"))
            else Right(())
          case _ => Left(ProfileFitError.Unsupported("whitening does not match FitConfig"))
        identity.flatMap { _ =>
          if actualDesign.rows != rows then Left(ProfileFitError.Preparation("baseline rows differ from dataset time axis"))
          else if actualDesign.cols == 0 then Right(())
          else DesignMatrix.fromMatrix(toDMat(actualDesign)).left.map(error => ProfileFitError.Preparation(error.message)).flatMap { matrix =>
            PreparedContrastGeometry.validatePreparation(ResponsePreparationPlan.fromConfig(config), matrix, partitions,
              TemporalPreparationScope.Fixed, whitening).left.map(error => ProfileFitError.Unsupported(error.message))
          }
        }
      }

  private def whiteningOption(whitening: CanonicalTemporalWhitening): Option[scalafim.fmri.ar.WhiteningPlan] =
    whitening match
      case CanonicalTemporalWhitening.Iid => None
      case CanonicalTemporalWhitening.Shared(plan) => Some(plan)

  private def nuisance(matrix: scalafim.fmri.hrf.linalg.Mat): Option[DMat] =
    if matrix.cols == 0 then None else Some(toDMat(matrix))

  private def fixedNuisance(
      fixed: scalafim.fmri.model.FitPlan,
      structure: scalafim.fmri.fit.TaskBasisStructure
  ): Either[ProfileFitError, Option[DMat]] =
    fixed.model.designSchema.toRight(ProfileFitError.Preparation("fixed condition plan needs a structural schema")).map { schema =>
      val selected = structure.columnIds.toSet
      val columns = schema.coefficientAxis.columnIds.indices.filterNot(i => selected.contains(schema.coefficientAxis.columnIds(i)))
      if columns.isEmpty then None
      else Some(DMat.tabulate(fixed.nTimepoints, columns.length)((r, c) => fixed.model.designMatrix(r, columns(c))))
    }

  private def toDMat(matrix: scalafim.fmri.hrf.linalg.Mat): DMat =
    val builder = DMat.newBuilder(matrix.rows, matrix.cols)
    var i = 0
    while i < matrix.data.length do
      builder.writeLinear(i, matrix.data(i))
      i += 1
    builder.result()

  private[profile] def sameDataset(actual: FmriDataset, expected: FmriDataset): Boolean =
    actual.id == expected.id && actual.shape == expected.shape &&
      actual.voxelDomain.indices == expected.voxelDomain.indices &&
      actual.metadata == expected.metadata && actual.samplingFrame == expected.samplingFrame &&
      actual.events == expected.events && actual.timeAxis == expected.timeAxis
