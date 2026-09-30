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
    outputs.admit(request, evidence).map: _ =>
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
      val provenance = preparationProvenance + "|public-trial-execution/v1|intent-only=true|mode=" + mode +
        "|normalization=" + request.rule + "|evidence=" + evidence + "|output=" + kind +
        "|public-axis=" + physicalAxis + "|queries=" + queryIdentity
      new ProfileTrialExecutionDeclaration(axis, mode, request.rule, evidence, kind, queries,
        preparationProvenance, provenance)

/** A reference selected by the prepared owner at the actual decoded shape.
  * It cannot be constructed from an arbitrary public node number.
  */
final class ProfileTrialReference private (
    private[profile] val owner: PreparedProfileHrf,
    private[profile] val bank: TrialBandedObjective,
    val axis: ProfileTrialAxis,
    val actualCoordinates: Vector[Double],
    val index: Int,
    val coordinates: Vector[Double])

private object ProfileTrialReference:
  def select(owner: PreparedProfileHrf, bank: TrialBandedObjective, axis: ProfileTrialAxis,
      actual: Vector[Double]): Either[ProfileTrialReadoutError, ProfileTrialReference] =
    bank.grid.chart.point(actual).map { _ =>
      val indices = new Array[Int](bank.grid.dimension)
      var dimension = 0
      while dimension < indices.length do
        val position = (actual(dimension) - bank.grid.chart.lower(dimension)) / bank.grid.step(dimension)
        val lower = math.max(0, math.min(bank.grid.nodesPerAxis(dimension) - 1, math.floor(position).toInt))
        val upper = math.min(bank.grid.nodesPerAxis(dimension) - 1, lower + 1)
        // Strict comparison preserves the lower bank index at an exact half step.
        indices(dimension) = if position - lower > upper - position then upper else lower
        dimension += 1
      val index = bank.grid.indexOf(indices)
      new ProfileTrialReference(owner, bank, axis, actual, index, bank.grid.point(index).coordinates)
    }.left.map(error => ProfileTrialReadoutError.InvalidShape(error.message))

enum ProfileTrialOutputOutcome:
  case Emitted(reference: ProfileTrialReference, value: ProfileTrialReadoutResult)
  case DecodeRefused(status: DecodeStatus)

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
  * count only full trial vectors; queries, condition/nuisance vectors, collection
  * overhead, response copies, solver/objective scratch and bank are excluded.
  */
final case class ProfileTrialOutputStorage(
    workerCoefficientHighWaterValues: Vector[Int],
    workerAdjointRowHighWaterValues: Vector[Int],
    completedRetainedTrialAmplitudeValues: Long,
    emittedTrialAmplitudeValues: Long)

/** `numerical` overlaps `ProfileRunProgress.trial`: never add it to that total.
  * The local actions/row visits below describe completed results only; failed
  * operations retain their actual numerical attempted/failure counters instead.
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
    storage: ProfileTrialOutputStorage)

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
        parts.map(_.retainedTrials).sum, emitted))

/** Checked public output over one exact prepared bank and physical axis.
  * Each admitted voxel freezes its own ephemeral conditional worker. This is
  * neither an adaptive-estimator Jacobian nor original-equation certification.
  */
final class PreparedProfileTrialOutputs private (
    private[profile] val prepared: PreparedProfileHrf,
    private[profile] val bank: TrialBandedObjective,
    val axis: ProfileTrialAxis):

  def executionDeclaration(request: OutputRequest, mode: ProfileTrialReadoutMode,
      evidence: ProfileTrialEvidenceRequest = ProfileTrialEvidenceRequest.PreparedBasisResidual
  ): Either[ProfileFitError, ProfileTrialExecutionDeclaration] =
    ProfileTrialExecutionDeclaration.make(this, request, mode, evidence)

  private[profile] def admit(request: OutputRequest, evidence: ProfileTrialEvidenceRequest): Either[ProfileFitError, Unit] =
    request.validateForTrial(axis).left.map(error =>
      ProfileFitError.TrialOutputAdmission(ProfileTrialReadoutError.Output(error))).flatMap { _ =>
      if !axis.preparation.basis.family.supports(request.rule) then
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
        def condition(voxelId: Int, fit: CompactConditionFit,
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
              val evaluated = for
                reference <- referenceAt(decoded.coordinates)
                response <- ProfileTrialResponse.make(axis, axis.selectedResponseRows, ProfileTrialResponseDomain.Whitened, whitened)
                frozen <- freeze(reference, request, mode, evidence)
                value <-
                  val worker = frozen.newWorker()
                  work.coefficientHighWater = math.max(work.coefficientHighWater, axis.preparation.trials + axis.preparation.nuisanceColumns)
                  work.adjointHighWater = math.max(work.adjointHighWater, axis.preparation.rows)
                  try worker.evaluate(response, request)
                  finally work.numerical = ProfileHrfFit.sumTrialWork(Vector(work.numerical, worker.workSnapshot))
              yield
                work.completed(value.work)
                succeeded = true
                ProfileTrialOutputVoxel(voxelId, decoded, ProfileTrialOutputOutcome.Emitted(reference, value))
              evaluated.left.map(ProfileWorkFailure.TrialReadout.apply)
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
      bank: TrialBandedObjective): Either[ProfileFitError, PreparedProfileTrialOutputs] =
    if owner.plan.criterion.usesDeterminant then
      return Left(ProfileFitError.TrialOutputCriterionUnsupported)
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
