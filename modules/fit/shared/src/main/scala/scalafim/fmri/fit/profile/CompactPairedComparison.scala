package scalafim.fmri.fit.profile

import gale.numeric.{PairedResidualComparison, PairedResidualError, PairedResidualWorkspace}

/** Added retained primitive storage per compact worker. The response is owned
  * even when prior-aware decoding keeps the ordinary scalar path.
  */
final case class CompactComparisonWorkspaceReceipt private[profile] (
    enabled: Boolean,
    ownedResponseValues: Long,
    snapshotValues: Long,
    comparisonScratchValues: Long):
  private val maximumCells = Long.MaxValue / 8L
  require(ownedResponseValues > 0L && ownedResponseValues <= Int.MaxValue.toLong, "owned response storage must fit one primitive array")
  require(snapshotValues >= 0L && comparisonScratchValues >= 0L, "comparison storage must be nonnegative")
  require(snapshotValues <= maximumCells - ownedResponseValues &&
    comparisonScratchValues <= maximumCells - ownedResponseValues - snapshotValues, "comparison byte count overflows")
  require(if enabled then snapshotValues > 0L && comparisonScratchValues > 0L
    else snapshotValues == 0L && comparisonScratchValues == 0L, "comparison storage disagrees with its policy")
  def retainedDoubles: Long = ownedResponseValues + snapshotValues + comparisonScratchValues
  def estimatedBytes: Long = retainedDoubles * 8L
  def policyId: String = if enabled then CompactComparisonWorkspaceReceipt.PolicyId else "scalar-only"

object CompactComparisonWorkspaceReceipt:
  val PolicyId: String = "compact-paired-profile/v1"
  private[profile] def enabled(prior: Option[ShapePrior], budget: DecodeBudget): Boolean =
    prior.isEmpty && budget.maxExactEvaluations > 0 && budget.maxNewtonSteps > 0 && budget.maxJets >= 3 &&
      budget.stationarityStepTolerance <= 1e-9

  def estimate(preparation: CompactConditionPreparation, enabled: Boolean): Either[CompactConditionError, CompactComparisonWorkspaceReceipt] =
    val rows = preparation.rank
    val columns = preparation.conditions
    val dimension = preparation.family.dimension
    if dimension < 1 || dimension > 3 then return Left(CompactConditionError.RuntimePreparation("comparison dimension must be 1..3"))
    PairedResidualWorkspace.requiredArrayCells(rows, columns)
      .left.map(error => CompactConditionError.RuntimePreparation(error.message))
      .flatMap: scratch =>
        val snapshots = if enabled then
          2L * rows * columns + 2L * columns + rows + 3L * dimension + dimension.toLong * dimension
        else 0L
        val usedScratch = if enabled then scratch else 0L
        if snapshots > Long.MaxValue / 8L - rows || usedScratch > Long.MaxValue / 8L - rows - snapshots then
          Left(CompactConditionError.RuntimePreparation("comparison byte count overflows"))
        else Right(CompactComparisonWorkspaceReceipt(enabled, rows.toLong, snapshots, usedScratch))

private[profile] enum CompactComparisonFailure:
  case SnapshotMismatch
  case Numerical(cause: PairedResidualError)
  case NotCertified(comparison: PairedResidualComparison)

/** Certified data-criterion descent for a coherent terminal move. The point
  * and response epoch binding is supplied by the closed compact adapter.
  */
final class PairedShapeDecrease private[profile] (
    val previousCoordinates: Vector[Double],
    val candidateCoordinates: Vector[Double],
    val responseEpoch: Long,
    val rows: Int,
    val columns: Int,
    val comparison: PairedResidualComparison):
  require(previousCoordinates.nonEmpty && previousCoordinates.length <= 3 &&
    previousCoordinates.length == candidateCoordinates.length, "comparison points need equal 1..3 dimensions")
  require(previousCoordinates.forall(_.isFinite) && candidateCoordinates.forall(_.isFinite), "comparison coordinates must be finite")
  require(rows > 0 && columns > 0 && rows.toLong * columns <= Int.MaxValue, "comparison design shape must be positive and admitted")
  require(comparison.certifiesProfileDecrease, "paired move needs a certified profile decrease")

private[profile] final case class PaidProfileComparison(exactUsed: Int, proof: Option[PairedShapeDecrease])

/** Resource transaction used after the decoder's full-jet stationarity gate.
  * Only the closed compact capability can enter it. A refused or unresolved
  * mathematical comparison spends the same exact slot as a certified one.
  */
private[profile] object PaidProfileComparison:
  def attempt(objective: PairedProfileObjective, coordinates: Array[Double], jet: ProfileJetBuffer,
      exactUsed: Int, maximum: Int, counters: DecoderCounters): PaidProfileComparison =
    require(exactUsed >= 0 && maximum >= exactUsed, "exact comparison quota state is invalid")
    if exactUsed == maximum || !objective.pairedCandidateAvailable(coordinates, jet) then
      PaidProfileComparison(exactUsed, None)
    else charge(exactUsed, maximum, counters)(objective.comparePairedCandidate(coordinates, jet))

  /** Kept private to the profile implementation; this is a quota transaction,
    * not an objective capability or an alternative certificate issuer.
    */
  private[profile] def charge(exactUsed: Int, maximum: Int, counters: DecoderCounters)(
      comparison: => Either[CompactComparisonFailure, PairedShapeDecrease]): PaidProfileComparison =
    require(exactUsed >= 0 && maximum >= exactUsed, "exact comparison quota state is invalid")
    if exactUsed == maximum then PaidProfileComparison(exactUsed, None)
    else
      counters.exactEvaluations += 1
      counters.pairedComparisons += 1
      val proof = comparison.toOption
      PaidProfileComparison(exactUsed + 1, proof)

/** Owned snapshots, issued only by the final compact objective. */
private[profile] final class CompactPairedState(rows: Int, columns: Int, dimension: Int):
  private val admittedCells = PairedResidualWorkspace.requiredArrayCells(rows, columns)
    .fold(error => throw new IllegalArgumentException(error.message), identity)
  require(dimension >= 1 && dimension <= 3, "paired snapshot dimension must be 1..3")
  val currentDesign: Array[Double] = new Array[Double](rows * columns)
  val previousDesign: Array[Double] = new Array[Double](rows * columns)
  val currentCoefficients: Array[Double] = new Array[Double](columns)
  val previousCoefficients: Array[Double] = new Array[Double](columns)
  val previousResponse: Array[Double] = new Array[Double](rows)
  val currentCoordinates: Array[Double] = new Array[Double](dimension)
  val previousCoordinates: Array[Double] = new Array[Double](dimension)
  val currentGradient: Array[Double] = new Array[Double](dimension)
  val currentHessian: Array[Double] = new Array[Double](dimension * dimension)
  val workspace: PairedResidualWorkspace = PairedResidualWorkspace(rows, columns)
    .fold(error => throw new IllegalArgumentException(error.message), identity)
  require(workspace.arrayCells == admittedCells, "paired workspace differs from its admitted storage")
  var currentEnergy: Double = Double.NaN
  var responseEnergy: Double = Double.NaN
  var currentEpoch: Long = 0L
  var previousEpoch: Long = 0L
  var currentCurvature: CurvatureStatus = CurvatureStatus.GramNotPositiveDefinite
  var currentFullJet = false
  var previousValid = false

  private def same(left: Array[Double], right: Array[Double]): Boolean =
    if left.length != right.length then false
    else
      var index = 0
      while index < left.length do
        if java.lang.Double.doubleToRawLongBits(left(index)) != java.lang.Double.doubleToRawLongBits(right(index)) then return false
        index += 1
      true

  def currentMatches(coordinates: Array[Double], jet: ProfileJetBuffer, epoch: Long): Boolean =
    currentFullJet && currentEpoch == epoch && same(currentCoordinates, coordinates) &&
      same(currentCoefficients, jet.amplitudes) && same(currentGradient, jet.gradient) &&
      same(currentHessian, jet.hessian) && currentCurvature == jet.curvature &&
      java.lang.Double.doubleToRawLongBits(currentEnergy) == java.lang.Double.doubleToRawLongBits(jet.energy)

  def stamp(coordinates: Vector[Double], jet: ProfileJetBuffer, epoch: Long): Unit =
    currentFullJet = coordinates.forall(_.isFinite) && jet.energy.isFinite &&
      jet.amplitudes.forall(_.isFinite) && jet.gradient.forall(_.isFinite) && jet.hessian.forall(_.isFinite)
    if currentFullJet then
      var axis = 0
      while axis < dimension do
        currentCoordinates(axis) = coordinates(axis)
        axis += 1
      System.arraycopy(jet.amplitudes, 0, currentCoefficients, 0, columns)
      System.arraycopy(jet.gradient, 0, currentGradient, 0, dimension)
      System.arraycopy(jet.hessian, 0, currentHessian, 0, dimension * dimension)
      currentEnergy = jet.energy
      currentCurvature = jet.curvature
      currentEpoch = epoch

  def capture(coordinates: Array[Double], jet: ProfileJetBuffer, response: Array[Double], energy: Double, epoch: Long): Unit =
    previousValid = currentMatches(coordinates, jet, epoch)
    if previousValid then
      System.arraycopy(currentDesign, 0, previousDesign, 0, previousDesign.length)
      System.arraycopy(currentCoefficients, 0, previousCoefficients, 0, columns)
      System.arraycopy(currentCoordinates, 0, previousCoordinates, 0, dimension)
      System.arraycopy(response, 0, previousResponse, 0, rows)
      responseEnergy = energy
      previousEpoch = epoch

  def available(coordinates: Array[Double], jet: ProfileJetBuffer, response: Array[Double], energy: Double, epoch: Long): Boolean =
    previousValid && previousEpoch == epoch && currentMatches(coordinates, jet, epoch) &&
      same(previousResponse, response) && java.lang.Double.doubleToRawLongBits(responseEnergy) == java.lang.Double.doubleToRawLongBits(energy)

  def compare(coordinates: Array[Double], jet: ProfileJetBuffer, response: Array[Double], energy: Double, epoch: Long)
      : Either[CompactComparisonFailure, PairedShapeDecrease] =
    if !available(coordinates, jet, response, energy, epoch) then Left(CompactComparisonFailure.SnapshotMismatch)
    else workspace.compare(previousResponse, previousDesign, currentDesign, previousCoefficients, currentCoefficients)
      .left.map(CompactComparisonFailure.Numerical.apply)
      .flatMap: comparison =>
        if comparison.certifiesProfileDecrease then
          Right(new PairedShapeDecrease(previousCoordinates.toVector, currentCoordinates.toVector, epoch, rows, columns, comparison))
        else Left(CompactComparisonFailure.NotCertified(comparison))
