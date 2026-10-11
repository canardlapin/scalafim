package scalafim.fmri.fit.profile

import scalafim.dataset.DatasetSeriesReader
import scalafim.fmri.design.{RowLayout, ScanIndex}
import scalafim.fmri.hrf.family.NormalizationRule
import scalafim.fmri.model.ProfileHrfSource
import scala.util.control.NonFatal

enum ProfileTrialOutputKind:
  case ConditionAmplitudes, ConditionQueries, TrialAmplitudes, TrialQueries

final case class ProfileTrialExecutionQuery private[profile] (
    label: String, weights: Vector[Double], absoluteTolerance: Double)

/** Checked immutable execution intent, retained even when no payload is delivered.
  * Actual factors and successful operations belong to the separate work receipts.
  * Axis binding includes the exact source/preparation, ordered IDs and physical rows.
  */
final class ProfileTrialExecutionDeclaration private (
    val axis: ProfileTrialAxis,
    val mode: ProfileTrialReadoutMode,
    val normalization: NormalizationRule,
    val evidence: ProfileTrialEvidenceRequest,
    val outputKind: ProfileTrialOutputKind,
    val queries: Vector[ProfileTrialExecutionQuery],
    val preparationProvenance: String,
    val provenance: String):
  val contract: String = "profile-public-trial-output/v1"

  def sameExecution(other: ProfileTrialExecutionDeclaration): Boolean =
    axis.sameBinding(other.axis) && provenance == other.provenance

  override def equals(other: Any): Boolean = other match
    case declaration: ProfileTrialExecutionDeclaration => sameExecution(declaration)
    case _ => false

  override def hashCode(): Int = (axis.preparation, axis.source, provenance).hashCode()

object ProfileTrialExecutionDeclaration:
  private[profile] def make(outputs: PreparedProfileTrialOutputs, request: OutputRequest,
      mode: ProfileTrialReadoutMode, evidence: ProfileTrialEvidenceRequest
  ): Either[ProfileFitError, ProfileTrialExecutionDeclaration] =
    outputs.admit(request, mode, evidence).map: _ =>
      val (kind, queries) = request match
        case OutputRequest.ConditionAmplitudes(_) => (ProfileTrialOutputKind.ConditionAmplitudes, Vector.empty)
        case OutputRequest.TrialAmplitudes(_) => (ProfileTrialOutputKind.TrialAmplitudes, Vector.empty)
        case OutputRequest.ConditionQueries(values, _) =>
          (ProfileTrialOutputKind.ConditionQueries, values.map(q => ProfileTrialExecutionQuery(q.label, q.weights, q.absoluteTolerance)))
        case OutputRequest.TrialQueries(values, _) =>
          (ProfileTrialOutputKind.TrialQueries, values.map(q => ProfileTrialExecutionQuery(q.label, q.weights, q.absoluteTolerance)))
      // Length framing avoids collisions from labels containing separators.
      def framed(values: Vector[String]): String =
        values.map(value => value.length.toString + ":" + value).mkString("[", ",", "]")
      def bits(value: Double): String = java.lang.Double.toHexString(value)
      val axis = outputs.axis
      val physicalAxis = framed(Vector(framed(axis.trialIds.map(_.value)),
        framed(axis.conditionIds.map(_.value)), framed(axis.conditionForTrial.map(_.value)),
        framed(axis.nuisanceColumnIds.map(_.value)), framed(axis.selectedResponseRows.map(_.oneBased.toString))))
      val queryIdentity = framed(queries.map(q =>
        framed(Vector(q.label, framed(q.weights.map(bits)), bits(q.absoluteTolerance)))))
      val preparationProvenance = outputs.prepared.provenance.stripSuffix("|exact-readout=true")
      val criterion = if outputs.nativeMl then
        "|criterion=native-trial-ml|conditional-returned-shape=true|raw-energy=E|optimised=J"
      else "|criterion=penalized-profile|raw-energy=E"
      val provenance = preparationProvenance + "|public-trial-execution/v1|intent-only=true" + criterion + "|mode=" + mode +
        "|normalization=" + request.rule + "|evidence=" + evidence + "|output=" + kind +
        "|public-axis=" + physicalAxis + "|queries=" + queryIdentity
      new ProfileTrialExecutionDeclaration(axis, mode, request.rule, evidence, kind, queries,
        preparationProvenance, provenance)

/** A reference selected by the prepared owner at the actual decoded shape.
  * It cannot be constructed from an arbitrary public node number.
  */
final class ProfileTrialReference private (
    private[profile] val owner: PreparedProfileHrf,
    private[profile] val bank: TrialReferenceBank,
    val axis: ProfileTrialAxis,
    val actualCoordinates: Vector[Double],
    val index: Int,
    val coordinates: Vector[Double])

private object ProfileTrialReference:
  def select(owner: PreparedProfileHrf, bank: TrialReferenceBank, axis: ProfileTrialAxis,
      actual: Vector[Double]): Either[ProfileTrialReadoutError, ProfileTrialReference] =
    val selected = bank match
      case gridded: TrialBandedObjective => gridded.grid.chart.point(actual).map: _ =>
        val grid = gridded.grid
        val indices = new Array[Int](grid.dimension)
        var dimension = 0
        while dimension < indices.length do
          val position = (actual(dimension) - grid.chart.lower(dimension)) / grid.step(dimension)
          val lower = math.max(0, math.min(grid.nodesPerAxis(dimension) - 1, math.floor(position).toInt))
          val upper = math.min(grid.nodesPerAxis(dimension) - 1, lower + 1)
          indices(dimension) = if position - lower > upper - position then upper else lower
          dimension += 1
        grid.indexOf(indices)
      case _ => bank.points.nearestAmong(actual, owner.policy.trialReferences.get.candidates)
    selected.map(index => new ProfileTrialReference(owner, bank, axis, actual, index, bank.points.coordinates(index)))
      .left.map(error => ProfileTrialReadoutError.InvalidShape(error.message))

enum ProfileTrialOutputOutcome:
  case Emitted(reference: ProfileTrialReference, value: ProfileTrialReadoutResult)
  case DecodeRefused(status: DecodeStatus)
  /** Shape selection succeeded, but the explicit empirical readout limit did
    * not. Keep this attempted voxel in its block without emitting coefficients.
    */
  case ReadoutRefused(error: ProfileTrialReadoutError.PreparedResidualExceeded)

/** Selection is the full, raw decoder result, including both Hessians and exit.
  * Conditional coefficients never replace its amplitudes or objective energy.
  */
final case class ProfileTrialOutputVoxel(
    voxelId: Int,
    selection: ShapeDecodeResult,
    output: ProfileTrialOutputOutcome)

final case class ProfileTrialOutputBlock(
    ordinal: Int, voxelIds: Vector[Int], results: Vector[ProfileTrialOutputVoxel])

/** Sum of per-worker scoped wrapper high-water values, not a simultaneous live
  * bound or an engine peak. Result values produced and sink-delivered values
  * count only full public trial vectors. ML measurement scratch (all ten N+F
  * arrays plus coordinate/basis/kernel/condition arrays) is listed separately,
  * including workers whose decode refuses. Completed raw memo vector values
  * are also separate: query-only public results retain zero trial values while
  * the successful worker epoch still retains its raw N-vector. Queries,
  * condition/nuisance vectors, collection overhead, response copies, objective
  * scratch and bank are excluded. These are scoped counts, not live totals.
  */
final case class ProfileTrialOutputStorage(
    workerCoefficientHighWaterValues: Vector[Int],
    workerAdjointRowHighWaterValues: Vector[Int],
    completedRetainedTrialAmplitudeValues: Long,
    emittedTrialAmplitudeValues: Long,
    workerMlMeasurementScratchValues: Vector[Int] = Vector.empty,
    completedMemoizedRawTrialValues: Long = 0L)

/** `numerical` overlaps `ProfileRunProgress.trial`: never add it to that total.
  * The local actions/row visits below describe completed results only; failed
  * operations retain their actual numerical attempted/failure counters instead.
  * Residual-limit refusals occur after a complete numerical solve; their local
  * actions/row visits are counted separately from emitted-result work.
  */
final case class ProfileTrialOutputProgress(
    attempts: Long,
    successes: Long,
    failures: Long,
    decodeRefusals: Long,
    numerical: TrialBandedWorkSnapshot,
    completedNormalActionApplications: Long,
    completedResponseRowsEncoded: Long,
    completedWhiteningForwardRowsVisited: Long,
    completedWhiteningTransposeRowsVisited: Long,
    storage: ProfileTrialOutputStorage,
    residualMeasurementAttempts: Long = 0L,
    residualMeasurementFailures: Long = 0L,
    residualMeasurementNormalActions: Long = 0L,
    residualGateRefusals: Long = 0L,
    residualGateNormalActions: Long = 0L,
    residualGateResponseRowsEncoded: Long = 0L)

private[profile] final class ProfileTrialOutputWork:
  var attempts = 0L
  var successes = 0L
  var failures = 0L
  var decodeRefusals = 0L
  var numerical = ProfileHrfFit.sumTrialWork(Vector.empty)
  var normalActions = 0L
  var rowsEncoded = 0L
  var forwardRows = 0L
  var transposeRows = 0L
  var coefficientHighWater = 0
  var adjointHighWater = 0
  var retainedTrials = 0L
  var measurementAttempts = 0L
  var measurementFailures = 0L
  var measurementActions = 0L
  var measurementScratch = 0
  var memoizedRawTrials = 0L
  var gateRefusals = 0L
  var gateNormalActions = 0L
  var gateRowsEncoded = 0L

  def refusedByResidual(work: ProfileTrialReadoutWork): Unit =
    gateRefusals += 1L
    gateNormalActions += work.normalActionApplications
    gateRowsEncoded += work.responseRowsEncoded

  def completed(work: ProfileTrialReadoutWork): Unit =
    successes += 1
    normalActions += work.normalActionApplications
    rowsEncoded += work.responseRowsEncoded
    forwardRows += work.whiteningForwardRowsVisited
    transposeRows += work.whiteningTransposeRowsVisited
    retainedTrials += work.retainedTrialAmplitudeValues

object ProfileTrialOutputProgress:
  private[profile] def aggregate(parts: Vector[ProfileTrialOutputWork], emitted: Long): ProfileTrialOutputProgress =
    ProfileTrialOutputProgress(parts.map(_.attempts).sum, parts.map(_.successes).sum,
      parts.map(_.failures).sum, parts.map(_.decodeRefusals).sum,
      ProfileHrfFit.sumTrialWork(parts.map(_.numerical)), parts.map(_.normalActions).sum,
      parts.map(_.rowsEncoded).sum, parts.map(_.forwardRows).sum, parts.map(_.transposeRows).sum,
      ProfileTrialOutputStorage(parts.map(_.coefficientHighWater), parts.map(_.adjointHighWater),
        parts.map(_.retainedTrials).sum, emitted, parts.map(_.measurementScratch), parts.map(_.memoizedRawTrials).sum),
      parts.map(_.measurementAttempts).sum, parts.map(_.measurementFailures).sum, parts.map(_.measurementActions).sum,
      parts.map(_.gateRefusals).sum, parts.map(_.gateNormalActions).sum, parts.map(_.gateRowsEncoded).sum)

/** Checked public output over one exact prepared bank and physical axis.
  * PenalizedProfile freezes an ephemeral conditional worker; native ML reuses
  * its pointed decoder worker and memoized exact payload. This is
  * neither an adaptive-estimator Jacobian nor original-equation certification.
  */
final class PreparedProfileTrialOutputs private (
    private[profile] val prepared: PreparedProfileHrf,
    private[profile] val bank: TrialReferenceBank,
    val axis: ProfileTrialAxis):

  private[profile] val nativeMl: Boolean = prepared.plan.criterion.usesDeterminant

  def executionDeclaration(request: OutputRequest, mode: ProfileTrialReadoutMode,
      evidence: ProfileTrialEvidenceRequest = ProfileTrialEvidenceRequest.PreparedBasisResidual
  ): Either[ProfileFitError, ProfileTrialExecutionDeclaration] =
    ProfileTrialExecutionDeclaration.make(this, request, mode, evidence)

  private[profile] def admit(request: OutputRequest, mode: ProfileTrialReadoutMode, evidence: ProfileTrialEvidenceRequest): Either[ProfileFitError, Unit] =
    request.validateForTrial(axis).left.map(error =>
      ProfileFitError.TrialOutputAdmission(ProfileTrialReadoutError.Output(error))).flatMap { _ =>
      if nativeMl && (mode != ProfileTrialReadoutMode.ExactShape || evidence != ProfileTrialEvidenceRequest.PreparedBasisResidual) then
        Left(ProfileFitError.TrialOutputMlIntent(mode, evidence))
      else if !axis.preparation.basis.family.supports(request.rule) then
        Left(ProfileFitError.TrialOutputAdmission(ProfileTrialReadoutError.UnsupportedNormalization(request.rule)))
      else if evidence == ProfileTrialEvidenceRequest.CertifiedOriginalEquations then
        Left(ProfileFitError.TrialOutputAdmission(ProfileTrialReadoutError.CertificateUnavailable))
      else Right(())
    }

  private[profile] def referenceAt(actual: Vector[Double]): Either[ProfileTrialReadoutError, ProfileTrialReference] =
    ProfileTrialReference.select(prepared, bank, axis, actual)

  private[profile] def freeze(reference: ProfileTrialReference, request: OutputRequest,
      mode: ProfileTrialReadoutMode, evidence: ProfileTrialEvidenceRequest): Either[ProfileTrialReadoutError, ProfileTrialReadout] =
    if !(reference.owner eq prepared) || !(reference.bank eq bank) || !reference.axis.sameBinding(axis) then
      Left(ProfileTrialReadoutError.ForeignAxis)
    else ProfileTrialReadout.freeze(bank, axis, reference.actualCoordinates, reference.index, request.rule, mode, evidence)

  def run(
      reader: DatasetSeriesReader,
      request: OutputRequest,
      mode: ProfileTrialReadoutMode,
      sink: BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt],
      cancelled: () => Boolean = () => false,
      evidence: ProfileTrialEvidenceRequest = ProfileTrialEvidenceRequest.PreparedBasisResidual
  ): Either[ProfileFitError, ProfileRunSummary] =
    runWithExecutor(Vector(reader), request, mode, sink, cancelled, evidence, parallel = false,
      (n, budget, factory, target, stop) => BlockExecutor.runSequential(n, budget, factory, target, stop))

  private[profile] def runWithExecutor(
      readers: Vector[DatasetSeriesReader],
      request: OutputRequest,
      mode: ProfileTrialReadoutMode,
      sink: BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt],
      cancelled: () => Boolean,
      evidence: ProfileTrialEvidenceRequest,
      parallel: Boolean,
      execute: (Int, ExecutionBudget, () => BlockWorker[ProfileTrialOutputBlock],
        BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt], () => Boolean) =>
        Either[ExecutionError, ExecutionSummary[ProfileFitReceipt]]
  ): Either[ProfileFitError, ProfileRunSummary] =
    executionDeclaration(request, mode, evidence).flatMap { declaration =>
      val payload = new ProfilePayload[ProfileTrialOutputVoxel, ProfileTrialOutputBlock]:
        override def publicOutputs: Boolean = true
        override val publicExecution: Option[ProfileTrialExecutionDeclaration] = Some(declaration)
        def condition(voxelId: Int, fit: CompactConditionReadout,
            normalization: scalafim.fmri.hrf.family.NormalizationRule): ProfileTrialOutputVoxel =
          throw new IllegalStateException("checked public view requires a trial backend")
        def trial(voxelId: Int, evaluation: ProfileTrialEvaluation,
            whitened: Array[Double], work: ProfileTrialOutputWork): Either[ProfileWorkFailure, ProfileTrialOutputVoxel] =
          val decoded = evaluation.decoded
          if decoded.status != DecodeStatus.Accepted then
            work.decodeRefusals += 1
            Right(ProfileTrialOutputVoxel(voxelId, decoded, ProfileTrialOutputOutcome.DecodeRefused(decoded.status)))
          else
            work.attempts += 1
            var succeeded = false
            try
              val evaluated = referenceAt(decoded.coordinates).flatMap: reference =>
                val result = if nativeMl then
                  for
                    _ <- evaluation.evidence match
                      case Some(_: ProfileCriterionEvidence.TrialRandomEffectsML) => Right(())
                      case _ => Left(ProfileTrialReadoutError.Conditional("missing native ML terminal evidence"))
                    scale <- ProfileTrialReadout.scaleAt(axis, decoded.coordinates, request.rule)
                    value <-
                      work.coefficientHighWater = math.max(work.coefficientHighWater, evaluation.mlScratchValues.fold(0)(_._1))
                      work.measurementScratch = math.max(work.measurementScratch, evaluation.mlScratchValues.fold(0)(_._2))
                      try
                        evaluation.mlReadout.toRight(ProfileTrialReadoutError.Conditional("missing same-worker ML readout"))
                          .flatMap(_().left.map(error => ProfileTrialReadoutError.Conditional(error.toString)))
                          .map: payload =>
                            val receipt = payload.summary.work.copy(
                              bandedSolveAttempts = payload.numerical.attempted.solveAttempts,
                              bandedSolveFailures = payload.numerical.attempted.solveFailures,
                              bandedRightHandSideAttempts = payload.numerical.attempted.rightHandSideAttempts,
                              factorAttempts = payload.numerical.attempted.factorAttempts,
                              continuousFactors = payload.numerical.continuousFactors,
                              exactReadoutFactorAttempts = payload.numerical.attempted.exactReadoutFactorAttempts)
                            work.memoizedRawTrials += payload.raw.trialAmplitudes.length
                            ProfileTrialReadout.assemble(axis, decoded.coordinates, reference.index, mode,
                              request.rule, scale._1, scale._2, request, payload.raw.trialAmplitudes(_), payload.summary,
                              ProfileTrialReadoutWork.from(receipt,
                                if request.isInstanceOf[OutputRequest.TrialAmplitudes] then axis.preparation.trials else 0,
                                payload.coefficientScratchValues, 0))
                      finally
                        work.numerical = ProfileHrfFit.sumTrialWork(Vector(work.numerical, evaluation.mlReadoutWork()))
                        val measurement = evaluation.mlMeasurementWork()
                        work.measurementAttempts += measurement.attempts
                        work.measurementFailures += measurement.failures
                        work.measurementActions += measurement.normalActionApplications
                  yield value
                else
                  for
                    response <- ProfileTrialResponse.make(axis, axis.selectedResponseRows, ProfileTrialResponseDomain.Whitened, whitened)
                    frozen <- freeze(reference, request, mode, evidence)
                    value <-
                      val worker = frozen.newWorker()
                      work.coefficientHighWater = math.max(work.coefficientHighWater, axis.preparation.trials + axis.preparation.nuisanceColumns)
                      work.adjointHighWater = math.max(work.adjointHighWater, axis.preparation.rows)
                      try worker.evaluate(response, request)
                      finally work.numerical = ProfileHrfFit.sumTrialWork(Vector(work.numerical, worker.workSnapshot))
                  yield value
                result.map(value => reference -> value)
              evaluated match
                case Right((reference, value)) =>
                  work.completed(value.work)
                  succeeded = true
                  Right(ProfileTrialOutputVoxel(voxelId, decoded, ProfileTrialOutputOutcome.Emitted(reference, value)))
                case Left(error: ProfileTrialReadoutError.PreparedResidualExceeded) =>
                  work.refusedByResidual(error.work)
                  Right(ProfileTrialOutputVoxel(voxelId, decoded, ProfileTrialOutputOutcome.ReadoutRefused(error)))
                case Left(error) => Left(ProfileWorkFailure.TrialReadout(error))
            catch case NonFatal(error) => Left(ProfileWorkFailure.TrialReadout(ProfileTrialReadoutError.Conditional(error.toString)))
            finally if !succeeded then work.failures += 1
        def block(ordinal: Int, ids: Vector[Int], values: Vector[ProfileTrialOutputVoxel]): ProfileTrialOutputBlock =
          ProfileTrialOutputBlock(ordinal, ids, values)
        def ids(value: ProfileTrialOutputBlock): Vector[Int] = value.voxelIds
        override def retainedTrialValues(value: ProfileTrialOutputBlock): Long =
          value.results.map(_.output match
            case ProfileTrialOutputOutcome.Emitted(_, result) => result.trialAmplitudes.fold(0L)(_.length.toLong)
            case _ => 0L).sum
      prepared.runPayload(readers, sink, cancelled, parallel, payload, execute)
    }

object PreparedProfileTrialOutputs:
  private[profile] def make(owner: PreparedProfileHrf, preparation: TrialBandedPreparation,
      bank: TrialReferenceBank): Either[ProfileFitError, PreparedProfileTrialOutputs] =
    if !(bank.preparation eq preparation) || !owner.ownsTrialBank(preparation, bank) then
      return Left(ProfileFitError.Preparation("public trial bank differs from the supplied preparation"))
    owner.plan.source match
      case ProfileHrfSource.TrialEvents(_, drive, baseline, _) =>
        val matrix = baseline.designMatrix
        val nuisanceIds = baseline.compiledSchema match
          case None if matrix.cols == 0 => Right(Vector.empty)
          case None => Left(ProfileFitError.Preparation("public trial output needs the existing compiled baseline schema"))
          case Some(schema) =>
            schema.validate.left.map(error => ProfileFitError.Preparation(error.message)).flatMap { _ =>
              val retained = schema.matrixValues
              val sameMatrix = retained.rows == matrix.rows && retained.cols == matrix.cols &&
                matrix.data.indices.forall(i => retained(i / matrix.cols, i % matrix.cols) == matrix.data(i))
              if !sameMatrix || schema.rows != RowLayout.fromSamplingFrame(owner.dataset.samplingFrame) ||
                  baseline.samplingFrame != owner.dataset.samplingFrame then
                Left(ProfileFitError.Preparation("compiled baseline matrix or physical row layout is stale"))
              else if matrix.cols == 0 then
                if preparation.nuisanceColumns == 0 then Right(Vector.empty)
                else Left(ProfileFitError.Preparation("empty baseline differs from the actual prepared nuisance columns"))
              else preparation.whitenResponses(matrix.cols, matrix.data)
                .left.map(error => ProfileFitError.Preparation(error.message)).flatMap { whitened =>
                  if !java.util.Arrays.equals(whitened, preparation.whitenedNuisance) then
                    Left(ProfileFitError.Preparation("compiled baseline differs from the actual prepared nuisance columns"))
                  else Right(schema.coefficientAxis.columnIds)
                }
            }
        for
          ids <- nuisanceIds
          axis <- ProfileTrialAxis.make(preparation.source, preparation, drive.trialLabels, drive.conditionLabels,
            drive.conditionForTrial, ids, owner.selected.timepoints.map(i => ScanIndex.unsafeOneBased(i + 1)))
            .left.map(error => ProfileFitError.Preparation(error.message))
        yield new PreparedProfileTrialOutputs(owner, bank, axis)
      case _ => Left(ProfileFitError.Unsupported("public trial outputs require a physical trial source"))
