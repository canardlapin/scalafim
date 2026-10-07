package scalafim.fmri.fit.profile

import scalafim.fmri.ar.WhiteningTransform
import scalafim.fmri.design.{ColumnId, ConditionId, ScanIndex, TrialId}
import scalafim.fmri.design.hrf.TrialBasisDesign
import scalafim.fmri.hrf.family.{JetLayout, NormalizationRule}

enum ProfileTrialAxisError:
  case ForeignSource
  case TrialCount(expected: Int, actual: Int)
  case ConditionCount(expected: Int, actual: Int)
  case NuisanceCount(expected: Int, actual: Int)
  case RowCount(expected: Int, actual: Int)
  case DuplicateTrialId
  case DuplicateConditionId
  case DuplicateNuisanceId
  case DuplicateResponseRow
  case MembershipMismatch(trial: Int)

  def message: String =
    this match
      case ForeignSource => "trial axes must name the source used by this preparation"
      case TrialCount(expected, actual) => s"$actual trial ids; expected $expected"
      case ConditionCount(expected, actual) => s"$actual condition ids; expected $expected"
      case NuisanceCount(expected, actual) => s"$actual nuisance column ids; expected $expected"
      case RowCount(expected, actual) => s"$actual selected response rows; expected $expected"
      case DuplicateTrialId => "trial ids must be unique"
      case DuplicateConditionId => "condition ids must be unique"
      case DuplicateNuisanceId => "nuisance column ids must be unique"
      case DuplicateResponseRow => "selected response rows must be unique"
      case MembershipMismatch(trial) => s"trial ${trial + 1} has a condition id inconsistent with preparation membership"

/** Physical axes for one expanded trial design and its exact preparation.
  * Trial order is caller order; canonical event row maps remain on `source`.
  * The column and selected-row identities must be supplied by the caller.
  */
final class ProfileTrialAxis private (
    val source: TrialBasisDesign,
    val preparation: TrialBandedPreparation,
    val trialIds: Vector[TrialId],
    val conditionIds: Vector[ConditionId],
    val conditionForTrial: Vector[ConditionId],
    val nuisanceColumnIds: Vector[ColumnId],
    val selectedResponseRows: Vector[ScanIndex]):

  def sameBinding(other: ProfileTrialAxis): Boolean =
    (preparation eq other.preparation) && (source eq other.source) &&
      trialIds == other.trialIds && conditionIds == other.conditionIds &&
      conditionForTrial == other.conditionForTrial &&
      nuisanceColumnIds == other.nuisanceColumnIds &&
      selectedResponseRows == other.selectedResponseRows

object ProfileTrialAxis:
  def make(
      source: TrialBasisDesign,
      preparation: TrialBandedPreparation,
      trialIds: Vector[TrialId],
      conditionIds: Vector[ConditionId],
      conditionForTrial: Vector[ConditionId],
      nuisanceColumnIds: Vector[ColumnId],
      selectedResponseRows: Vector[ScanIndex]
  ): Either[ProfileTrialAxisError, ProfileTrialAxis] =
    if !(source eq preparation.source) then Left(ProfileTrialAxisError.ForeignSource)
    else if trialIds.length != preparation.trials then
      Left(ProfileTrialAxisError.TrialCount(preparation.trials, trialIds.length))
    else if conditionIds.length != preparation.conditions then
      Left(ProfileTrialAxisError.ConditionCount(preparation.conditions, conditionIds.length))
    else if conditionForTrial.length != preparation.trials then
      Left(ProfileTrialAxisError.TrialCount(preparation.trials, conditionForTrial.length))
    else if nuisanceColumnIds.length != preparation.nuisanceColumns then
      Left(ProfileTrialAxisError.NuisanceCount(preparation.nuisanceColumns, nuisanceColumnIds.length))
    else if selectedResponseRows.length != preparation.rows then
      Left(ProfileTrialAxisError.RowCount(preparation.rows, selectedResponseRows.length))
    else if trialIds.distinct.length != trialIds.length then Left(ProfileTrialAxisError.DuplicateTrialId)
    else if conditionIds.distinct.length != conditionIds.length then Left(ProfileTrialAxisError.DuplicateConditionId)
    else if nuisanceColumnIds.distinct.length != nuisanceColumnIds.length then Left(ProfileTrialAxisError.DuplicateNuisanceId)
    else if selectedResponseRows.distinct.length != selectedResponseRows.length then Left(ProfileTrialAxisError.DuplicateResponseRow)
    else
      val mismatched = conditionForTrial.indices.find: i =>
        conditionForTrial(i) != conditionIds(preparation.membership.conditionOfTrial(i))
      mismatched match
        case Some(i) => Left(ProfileTrialAxisError.MembershipMismatch(i))
        case None => Right(new ProfileTrialAxis(source, preparation, trialIds, conditionIds,
          conditionForTrial, nuisanceColumnIds, selectedResponseRows))

enum ProfileTrialResponseDomain:
  case Original
  case Whitened

enum ProfileTrialReadoutError:
  case ForeignAxis
  case ResponseRows
  case ResponseLength(expected: Int, actual: Int)
  case NonFiniteResponse(index: Int)
  case InvalidReferenceNode(node: Int, count: Int)
  case InvalidShape(detail: String)
  case UnsupportedNormalization(rule: NormalizationRule)
  case InvalidNormalizationScale(value: Double)
  case Output(error: OutputError)
  case NormalizationMismatch(expected: NormalizationRule, actual: NormalizationRule)
  case Conditional(detail: String)
  case Whitening(error: TrialBandedError)
  case CertificateUnavailable
  case PreparedResidualExceeded(measured: Double, maximum: Double, work: ProfileTrialReadoutWork)
  case ResponseDependentResidualGate

  def message: String =
    this match
      case ForeignAxis => "response or query belongs to another ordered trial axis"
      case ResponseRows => "selected response-row identities or order do not match the trial axis"
      case ResponseLength(expected, actual) => s"response has $actual rows; expected $expected"
      case NonFiniteResponse(index) => s"response row ${index + 1} is not finite"
      case InvalidReferenceNode(node, count) => s"reference node $node is outside 0 until $count"
      case InvalidShape(detail) => detail
      case UnsupportedNormalization(rule) => s"family does not support ${rule.label} normalization"
      case InvalidNormalizationScale(value) => s"normalization scale or transported lambda is invalid: $value"
      case Output(error) => error.message
      case NormalizationMismatch(expected, actual) =>
        s"request normalization ${actual.label} differs from frozen ${expected.label}"
      case Conditional(detail) => detail
      case Whitening(error) => error.message
      case CertificateUnavailable => "an original-family equation certificate is unavailable"
      case PreparedResidualExceeded(measured, maximum, _) =>
        s"prepared-basis normal residual $measured exceeds the declared native-unit limit $maximum"
      case ResponseDependentResidualGate => "a response-dependent residual gate has no unconditional linear adjoint"

/** A copied response with an explicit physical row declaration. */
final class ProfileTrialResponse private (
    val axis: ProfileTrialAxis,
    val domain: ProfileTrialResponseDomain,
    private[profile] val values: Array[Double])

object ProfileTrialResponse:
  def make(
      axis: ProfileTrialAxis,
      selectedRows: Vector[ScanIndex],
      domain: ProfileTrialResponseDomain,
      values: Array[Double]
  ): Either[ProfileTrialReadoutError, ProfileTrialResponse] =
    if selectedRows != axis.selectedResponseRows then Left(ProfileTrialReadoutError.ResponseRows)
    else if values.length != selectedRows.length then
      Left(ProfileTrialReadoutError.ResponseLength(selectedRows.length, values.length))
    else
      val bad = values.indexWhere(x => !x.isFinite)
      if bad >= 0 then Left(ProfileTrialReadoutError.NonFiniteResponse(bad))
      else Right(new ProfileTrialResponse(axis, domain, values.clone()))

enum ProfileTrialReadoutMode:
  case CorrectedReference
  case ExactShape

enum ProfileTrialEvidenceRequest:
  case PreparedBasisResidual
  case PreparedBasisResidualAtMost(limit: ProfileTrialResidualLimit)
  case CertifiedOriginalEquations

/** Explicit empirical equation tolerance, in native normal-equation coordinates. It is
  * neither a coefficient-error bound nor original-family certification.
  */
final case class ProfileTrialResidualLimit(maximumNorm: Double):
  require(maximumNorm.isFinite && maximumNorm >= 0.0, "residual limit must be finite and nonnegative")

/** Only a directly computed prepared-basis normal residual is available. */
final case class ProfileTrialReadoutEvidence(preparedBasisNormalResidualNorm: Double)

/** Operation counts and scoped storage counts, not total worker or peak memory.
  * `wrapperCoefficientBufferValues` and `alwaysRetainedAdjointRowValues` count
  * the two eagerly allocated wrapper arrays on every operation. They exclude
  * TrialConditionalSolve's ten N+F arrays, coordinates, basis coefficients,
  * kernel scratch and condition arrays; the objective/reduction workspace,
  * encoded response, shared bank/source/basis and transient whitening storage.
  * `retainedTrialAmplitudeValues` and `retainedAdjointOutputValues` count result
  * vector values separately from those reusable arrays; other result values,
  * caller inputs, collection storage and VM/object headers are excluded too.
  */
final case class ProfileTrialReadoutWork(
    referenceInverseAttempts: Long,
    referenceInverseFailures: Long,
    bandedSolveAttempts: Long,
    bandedSolveFailures: Long,
    bandedRightHandSideAttempts: Long,
    factorAttempts: Long,
    continuousFactors: Long,
    exactReadoutFactorAttempts: Long,
    residualCorrections: Int,
    normalActionApplications: Long,
    retainedTrialAmplitudeValues: Int,
    wrapperCoefficientBufferValues: Int,
    alwaysRetainedAdjointRowValues: Int,
    retainedAdjointOutputValues: Int,
    responseRowsEncoded: Int,
    whiteningForwardRowsVisited: Int,
    whiteningTransposeRowsVisited: Int)

object ProfileTrialReadoutWork:
  private[profile] def from(
      receipt: TrialConditionalWorkReceipt,
      retained: Int,
      wrapperCoefficientBufferValues: Int,
      alwaysRetainedAdjointRowValues: Int,
      retainedAdjointOutputValues: Int = 0,
      responseRowsEncoded: Int = 0,
      whiteningForwardRowsVisited: Int = 0
  ): ProfileTrialReadoutWork =
    ProfileTrialReadoutWork(
      receipt.referenceInverseAttempts, receipt.referenceInverseFailures,
      receipt.bandedSolveAttempts, receipt.bandedSolveFailures,
      receipt.bandedRightHandSideAttempts, receipt.factorAttempts,
      receipt.continuousFactors, receipt.exactReadoutFactorAttempts,
      receipt.residualCorrections, receipt.normalActionApplications,
      retained, wrapperCoefficientBufferValues, alwaysRetainedAdjointRowValues,
      retainedAdjointOutputValues, responseRowsEncoded, whiteningForwardRowsVisited,
      whiteningTransposeRowsVisited = 0)

final case class ProfileTrialReadoutResult(
    axis: ProfileTrialAxis,
    actualCoordinates: Vector[Double],
    referenceNode: Int,
    mode: ProfileTrialReadoutMode,
    normalization: NormalizationRule,
    normalizationScale: Double,
    nativeLambda: Double,
    equivalentNormalizedLambda: Double,
    trialAmplitudes: Option[Vector[Double]],
    conditionMeans: Vector[Double],
    nuisanceCoefficients: Vector[Double],
    queries: Vector[QueryValue],
    evidence: ProfileTrialReadoutEvidence,
    work: ProfileTrialReadoutWork)

final case class ProfileTrialAdjointResult(
    axis: ProfileTrialAxis,
    domain: ProfileTrialResponseDomain,
    selectedRows: Vector[ScanIndex],
    values: Vector[Double],
    work: ProfileTrialReadoutWork)

/** One selected, fixed shape and reference over an immutable node bank. The
  * selected shape may come from a response-dependent decoder, but this object
  * is a linear conditional operator only while that shape remains fixed.
  */
final class ProfileTrialReadout private (
    val bank: TrialBandedObjective,
    val axis: ProfileTrialAxis,
    val actualCoordinates: Vector[Double],
    val referenceNode: Int,
    val mode: ProfileTrialReadoutMode,
    val evidenceRequest: ProfileTrialEvidenceRequest,
    val normalization: NormalizationRule,
    val normalizationScale: Double,
    val equivalentNormalizedLambda: Double):

  def nativeLambda: Double = axis.preparation.lambda

  /** Each worker owns all mutable coefficient, response and transpose scratch. */
  final class Worker:
    private val objective = bank.newWorker()
    private val solver = new TrialConditionalSolve(objective)
    private val encoded = axis.preparation.newResponseBuffer
    private val coefficients = new Array[Double](axis.preparation.trials + axis.preparation.nuisanceColumns)
    private val adjointRows = new Array[Double](axis.preparation.rows)

    def workSnapshot: TrialBandedWorkSnapshot = objective.work.snapshot

    def evaluate(
        response: ProfileTrialResponse,
        request: OutputRequest
    ): Either[ProfileTrialReadoutError, ProfileTrialReadoutResult] =
      if !response.axis.sameBinding(axis) then return Left(ProfileTrialReadoutError.ForeignAxis)
      request.validateForTrial(axis) match
        case Left(error) => return Left(ProfileTrialReadoutError.Output(error))
        case Right(_) => ()
      if request.rule != normalization then
        return Left(ProfileTrialReadoutError.NormalizationMismatch(normalization, request.rule))
      var whiteningRows = 0
      val whitened = response.domain match
        case ProfileTrialResponseDomain.Whitened => response.values
        case ProfileTrialResponseDomain.Original =>
          axis.preparation.whitenResponses(1, response.values) match
            case Left(error) => return Left(ProfileTrialReadoutError.Whitening(error))
            case Right(values) =>
              if axis.preparation.whitening.nonEmpty then whiteningRows = axis.preparation.rows
              values
      axis.preparation.encodeWhitenedInto(whitened, 0, encoded) match
        case Left(error) => return Left(ProfileTrialReadoutError.Whitening(error))
        case Right(_) => ()
      objective.pointAt(encoded)
      val conditionalMode = mode match
        case ProfileTrialReadoutMode.CorrectedReference => TrialConditionalMode.CorrectedReference
        case ProfileTrialReadoutMode.ExactShape => TrialConditionalMode.ExactShape
      val summary = solver.solveInto(encoded, referenceNode, actualCoordinates, coefficients,
        mode = conditionalMode) match
        case Left(error) => return Left(ProfileTrialReadoutError.Conditional(error.message))
        case Right(value) => value
      evidenceRequest match
        case ProfileTrialEvidenceRequest.PreparedBasisResidualAtMost(limit)
            if summary.preparedBasisResidualNorm > limit.maximumNorm =>
          return Left(ProfileTrialReadoutError.PreparedResidualExceeded(summary.preparedBasisResidualNorm, limit.maximumNorm,
            ProfileTrialReadoutWork.from(summary.work, 0, coefficients.length, adjointRows.length,
              responseRowsEncoded = axis.preparation.rows, whiteningForwardRowsVisited = whiteningRows)))
        case _ => ()
      Right(ProfileTrialReadout.assemble(axis, actualCoordinates, referenceNode, mode,
        normalization, normalizationScale, equivalentNormalizedLambda, request, coefficients(_), summary,
        ProfileTrialReadoutWork.from(summary.work,
          if request.isInstanceOf[OutputRequest.TrialAmplitudes] then axis.preparation.trials else 0,
          coefficients.length, adjointRows.length, responseRowsEncoded = axis.preparation.rows,
          whiteningForwardRowsVisited = whiteningRows)))

    /** Reverse the frozen map from one normalized signed trial query to the
      * whitened selected-response rows. This is not a decoder derivative.
      */
    def transposeWhitened(query: ProfileTrialSignedQuery): Either[ProfileTrialReadoutError, ProfileTrialAdjointResult] =
      if !query.axis.sameBinding(axis) then return Left(ProfileTrialReadoutError.ForeignAxis)
      evidenceRequest match
        case ProfileTrialEvidenceRequest.PreparedBasisResidualAtMost(_) =>
          return Left(ProfileTrialReadoutError.ResponseDependentResidualGate)
        case _ => ()
      java.util.Arrays.fill(coefficients, 0.0)
      var i = 0
      while i < axis.preparation.trials do
        coefficients(i) = query.weights(i) / normalizationScale
        i += 1
      val conditionalMode = mode match
        case ProfileTrialReadoutMode.CorrectedReference => TrialConditionalMode.CorrectedReference
        case ProfileTrialReadoutMode.ExactShape => TrialConditionalMode.ExactShape
      val receipt = solver.transposeInto(referenceNode, actualCoordinates, coefficients, adjointRows,
        mode = conditionalMode) match
        case Left(error) => return Left(ProfileTrialReadoutError.Conditional(error.message))
        case Right(value) => value
      Right(ProfileTrialAdjointResult(axis, ProfileTrialResponseDomain.Whitened,
        axis.selectedResponseRows, adjointRows.toVector,
        ProfileTrialReadoutWork.from(receipt, 0, coefficients.length, adjointRows.length,
          retainedAdjointOutputValues = adjointRows.length)))

    /** Return original selected-response coordinates with the reviewed AR
      * transpose Wᵀ. W inverse would be a different operator.
      */
    def transposeOriginal(query: ProfileTrialSignedQuery): Either[ProfileTrialReadoutError, ProfileTrialAdjointResult] =
      transposeWhitened(query).flatMap: result =>
        axis.preparation.whitening match
          case None => Right(result.copy(domain = ProfileTrialResponseDomain.Original))
          case Some(plan) =>
            WhiteningTransform.transposeMatrix(plan,
              TrialBandedPreparation.toDMat(axis.preparation.rows, 1, result.values.toArray)) match
              case Left(error) => Left(ProfileTrialReadoutError.Whitening(TrialBandedError.Whitening(error.toString)))
              case Right(transposed) =>
                Right(result.copy(domain = ProfileTrialResponseDomain.Original,
                  values = Vector.tabulate(axis.preparation.rows)(i => transposed(i, 0)),
                  work = result.work.copy(whiteningTransposeRowsVisited = axis.preparation.rows)))

  def newWorker(): Worker = new Worker

object ProfileTrialReadout:
  /** Pure post-solve rendering shared by the ordinary and same-worker ML paths.
    * Nuisance units and residual evidence stay native; only trial/condition
    * values and signed queries use the frozen normalization scale.
    */
  private[profile] def assemble(axis: ProfileTrialAxis, actual: Vector[Double], referenceNode: Int,
      mode: ProfileTrialReadoutMode, normalization: NormalizationRule, scale: Double,
      normalizedLambda: Double, request: OutputRequest, coefficient: Int => Double,
      summary: TrialConditionalSummary, work: ProfileTrialReadoutWork): ProfileTrialReadoutResult =
    val n = axis.preparation.trials
    val means = summary.conditionMeans.map(_ / scale)
    val amplitudes = request match
      case OutputRequest.TrialAmplitudes(_) => Some(Vector.tabulate(n)(i => coefficient(i) / scale))
      case _ => None
    val queries = request match
      case OutputRequest.TrialQueries(items, _) => items.map: query =>
        var value = 0.0
        var i = 0
        while i < n do
          value += query.weights(i) * coefficient(i)
          i += 1
        QueryEvaluation.audit(query.label, value / scale, query.absoluteTolerance)
      case OutputRequest.ConditionQueries(items, _) => QueryEvaluation.evaluate(means, items)
      case _ => Vector.empty
    ProfileTrialReadoutResult(axis, actual, referenceNode, mode, normalization, scale,
      axis.preparation.lambda, normalizedLambda, amplitudes, means, summary.nuisanceCoefficients,
      queries, ProfileTrialReadoutEvidence(summary.preparedBasisResidualNorm), work)

  private[profile] def scaleAt(axis: ProfileTrialAxis, actual: Vector[Double], rule: NormalizationRule)
      : Either[ProfileTrialReadoutError, (Double, Double)] =
    val family = axis.preparation.basis.family
    family.chart.point(actual).left.map(error => ProfileTrialReadoutError.InvalidShape(error.message)).flatMap: point =>
      if !family.supports(rule) then Left(ProfileTrialReadoutError.UnsupportedNormalization(rule))
      else
        val scaleJet = new Array[Double](family.jetComponents)
        family.scaleJetInto(rule, point, scaleJet)
        val scale = scaleJet(JetLayout.Value)
        val lambda = scale * scale * axis.preparation.lambda
        if !scale.isFinite || scale == 0.0 || !lambda.isFinite || lambda <= 0.0 then
          Left(ProfileTrialReadoutError.InvalidNormalizationScale(scale))
        else Right(scale -> lambda)

  def freeze(
      bank: TrialBandedObjective,
      axis: ProfileTrialAxis,
      actualCoordinates: Vector[Double],
      referenceNode: Int,
      normalization: NormalizationRule,
      mode: ProfileTrialReadoutMode,
      evidence: ProfileTrialEvidenceRequest = ProfileTrialEvidenceRequest.PreparedBasisResidual
  ): Either[ProfileTrialReadoutError, ProfileTrialReadout] =
    if !(bank.preparation eq axis.preparation) then Left(ProfileTrialReadoutError.ForeignAxis)
    else if referenceNode < 0 || referenceNode >= bank.grid.count then
      Left(ProfileTrialReadoutError.InvalidReferenceNode(referenceNode, bank.grid.count))
    else if evidence == ProfileTrialEvidenceRequest.CertifiedOriginalEquations then
      Left(ProfileTrialReadoutError.CertificateUnavailable)
    else
      scaleAt(axis, actualCoordinates, normalization).map: (scale, lambda) =>
        new ProfileTrialReadout(bank, axis, actualCoordinates, referenceNode,
          mode, evidence, normalization, scale, lambda)
