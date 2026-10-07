package scalafim.fmri.fit.profile

import scala.util.control.NonFatal

/** Native coherent ML backend over one TrialBanded worker.
  *
  * Every evaluation builds or reuses ONE reference whose stamped bundle
  * (`TrialAcceptedTrialBand`) supplies the single N-sized factor used for both
  * raw energy E and the constrained determinant D, and mints ONE
  * `CriterionReference` shared by both parts by identity. Node determinant jets
  * are response independent and cached at setup; their helper work is setup
  * work. Continuous value calls return the scalar D of the same value
  * reference, with no hidden determinant derivatives; continuous full calls
  * build one full reference and evaluate the raw jet and the determinant jet
  * from it, then release it.
  *
  * `pointAt` takes one whitened response column of `preparation.rows` values.
  * Work mapping per attempt (the 22-field snapshot on the objective is kept
  * unchanged and separately):
  *  - `referenceAttempts`, `nFactorAttempts/Failures` (one accepted-band
  *    factorisation per reference), and `smallFactorAttempts` (legacy release
  *    and scalar-determinant factors, plus the helper's small B factor);
  *  - `solveAttempts` and `rightHandSideAttempts` from the legacy snapshot plus
  *    the helper;
  *  - `membershipRightHandSides`, which counts both the legacy scalar
  *    determinant and the helper's duplicate membership solve (the duplication
  *    remains until explicitly removed), and `derivativeRightHandSides` (helper);
  *  - `logDetRecursionAttempts` (the helper's log-determinant jets);
  *  - `failures`: refused backend attempts, counted once each. Stage failures
  *    remain in the 22-field snapshot and the helper receipt.
  * Native ML additionally owns 2T+N+C response/residual/coefficient values per
  * active worker. Energy traversal includes criterion and native readout calls;
  * their source, nuisance and recovery counts are separate from factor work.
  * The unused prepared prototype allocates no residual-energy workspace.
  */
private[profile] final class TrialBandedMlBackend private (
    objective: TrialBandedObjective,
    val owner: CriterionOwner,
    nodes: Vector[TrialBandedMlBackend.NodeDeterminant],
    val setupReceipt: TrialMlWork) extends CoherentTrialMlBackend:
  import TrialBandedMlBackend.*

  private val preparation = objective.preparation
  private val dimension = preparation.basis.family.dimension
  val grid: NodeGrid = objective.grid
  val amplitudeCount: Int = objective.amplitudeCount
  private val buffer = new ProfileJetBuffer(dimension, amplitudeCount)
  private val encoded = preparation.newResponseBuffer
  // The prepared prototype needs no response arrays until it is used as a worker.
  private lazy val energy = new TrialMlResidualEnergy(preparation)
  def energyScratchValues: Int = 2 * preparation.rows + preparation.trials + preparation.conditions
  private val measurementSolver = new TrialConditionalSolve(objective)
  private val solvedCoefficients = new Array[Double](preparation.trials + preparation.nuisanceColumns)
  private var readoutMemo: Option[(Vector[Double], Either[TrialMlFailure, TrialBandedReadout])] = None
  private var payloadMemo: Option[Either[TrialMlFailure, TrialMlReadoutPayload]] = None
  private var readoutStart = objective.work.snapshot
  private var readoutEnd = objective.work.snapshot
  def readoutWork: TrialBandedWorkSnapshot =
    if readoutMemo.isEmpty then difference(objective.work.snapshot, objective.work.snapshot)
    else difference(readoutStart, readoutEnd)
  private var measurementStart = TrialResidualMeasurementWork()
  def measurementWork: TrialResidualMeasurementWork =
    val end = measurementSolver.measurementWork
    TrialResidualMeasurementWork(end.attempts - measurementStart.attempts,
      end.failures - measurementStart.failures, end.normalActionApplications - measurementStart.normalActionApplications)
  def readoutScratchValues: Int = solvedCoefficients.length
  def measurementScratchValues: Int = measurementSolver.scratchValues
  private var sequence = 0L
  private var epoch: Option[CriterionEpoch] = None
  private var worker = TrialMlWork()

  def workerWorkSnapshot: TrialMlWork = worker

  /** The unchanged 22-field attempted snapshot of this worker's objective. */
  def legacyWork: TrialBandedWorkSnapshot = objective.work.snapshot

  /** A fresh worker sharing the owner, node cache and setup receipt. */
  def newWorker(): TrialBandedMlBackend = new TrialBandedMlBackend(objective.newWorker(), owner, nodes, setupReceipt)

  private def finish[A](before: Mark, result: Either[TrialMlFailure, A]): TrialMlAttempt[A] =
    val attempted = delta(before, mark(objective.work), refused = result.isLeft) + TrialMlWork(
      residualEnergyEvaluations = energy.evaluations - worker.residualEnergyEvaluations,
      residualEnergyRows = energy.residualRows - worker.residualEnergyRows,
      residualEnergySourceValues = energy.sourceValues - worker.residualEnergySourceValues,
      responseCopyValues = energy.responseCopies - worker.responseCopyValues,
      residualEnergyCoefficientProducts = energy.coefficientProducts - worker.residualEnergyCoefficientProducts,
      residualEnergyNuisanceValues = energy.nuisanceValues - worker.residualEnergyNuisanceValues)
    worker = worker + attempted
    TrialMlAttempt(result, attempted)

  private def criterion[A](result: Either[CriterionJet.Error, A]): Either[TrialMlFailure, A] =
    result.left.map(TrialMlFailure.Criterion.apply)

  def pointAt(response: Array[Double]): TrialMlAttempt[CriterionEpoch] =
    val before = mark(objective.work)
    epoch = None
    energy.invalidate()
    readoutMemo = None
    payloadMemo = None
    measurementStart = measurementSolver.measurementWork
    val result =
      if response.length != preparation.rows then
        Left(TrialMlFailure.Backend(s"response has ${response.length} rows; expected ${preparation.rows}"))
      else if response.exists(!_.isFinite) then Left(TrialMlFailure.Backend("response must be finite"))
      else
        preparation.encodeWhitenedInto(response, 0, encoded).left.map(error => TrialMlFailure.Backend(error.message))
          .flatMap { value =>
            energy.pointAt(response)
            objective.pointAt(value)
            sequence += 1L
            criterion(CriterionEpoch.checked(owner, sequence)).map { next => epoch = Some(next); next }
          }
    finish(before, result)

  private def currentEpoch: Either[TrialMlFailure, CriterionEpoch] =
    epoch.toRight(TrialMlFailure.Backend("pointAt must succeed before evaluation"))

  /** Exact readout of this worker's pointed response. The objective charges
    * readout work to legacyWork; the ML ledger counts criterion attempts only.
    */
  private def sameCoordinates(a: Vector[Double], b: Vector[Double]): Boolean =
    a.length == b.length && a.zip(b).forall((x, y) =>
      java.lang.Double.doubleToLongBits(x) == java.lang.Double.doubleToLongBits(y))

  private[profile] def exactReadout(coordinates: Vector[Double]): Either[TrialMlFailure, TrialBandedReadout] =
    currentEpoch.flatMap: _ =>
      readoutMemo match
        case Some((at, result)) if sameCoordinates(at, coordinates) => result
        case Some(_) => Left(TrialMlFailure.Backend("readout coordinates differ from the memoized terminal"))
        case None =>
          readoutStart = objective.work.snapshot
          val result = try objective.exactSolvedReadoutInto(coordinates, solvedCoefficients, Some(energy))
            .left.map(error => TrialMlFailure.Backend(error.message))
          catch case NonFatal(error) => Left(TrialMlFailure.Backend(error.toString))
          readoutEnd = objective.work.snapshot
          // Readout factors/solves remain in the separate legacy ledger. Energy
          // traversal counts include criterion and readout evaluations exactly once.
          worker = worker + TrialMlWork(
            residualEnergyEvaluations = energy.evaluations - worker.residualEnergyEvaluations,
            residualEnergyRows = energy.residualRows - worker.residualEnergyRows,
            residualEnergySourceValues = energy.sourceValues - worker.residualEnergySourceValues,
            responseCopyValues = energy.responseCopies - worker.responseCopyValues,
            residualEnergyCoefficientProducts = energy.coefficientProducts - worker.residualEnergyCoefficientProducts,
            residualEnergyNuisanceValues = energy.nuisanceValues - worker.residualEnergyNuisanceValues)
          readoutMemo = Some(coordinates -> result)
          result

  private[profile] def validateReadoutEpoch(expected: CriterionEpoch): Either[TrialMlFailure, Unit] =
    currentEpoch.flatMap: current =>
      if (current eq expected) && (expected.owner eq owner) then Right(())
      else Left(TrialMlFailure.Backend("stale or foreign readout epoch"))

  private[profile] def exactReadoutPayload(coordinates: Vector[Double], expectedEpoch: CriterionEpoch)
      : Either[TrialMlFailure, TrialMlReadoutPayload] =
    currentEpoch.flatMap: current =>
      if !(current eq expectedEpoch) || !(expectedEpoch.owner eq owner) then
        Left(TrialMlFailure.Backend("stale or foreign readout epoch"))
      else if readoutMemo.exists(value => !sameCoordinates(value._1, coordinates)) then
        Left(TrialMlFailure.Backend("readout coordinates differ from the memoized terminal"))
      else payloadMemo match
        case Some(result) => result
        case None =>
          val result = exactReadout(coordinates).flatMap: raw =>
            measurementSolver.measureExactSolved(encoded, coordinates, solvedCoefficients)
              .left.map(error => TrialMlFailure.Backend(error.message)).map: summary =>
                TrialMlReadoutPayload(raw, summary, owner, current, coordinates, readoutWork,
                  measurementWork, readoutScratchValues, measurementScratchValues)
          payloadMemo = Some(result)
          result

  private def checkedNode(node: Int): Either[TrialMlFailure, NodeDeterminant] =
    if node < 0 || node >= nodes.length then Left(TrialMlFailure.InvalidPolicy(s"invalid node $node"))
    else Right(nodes(node))

  private def checkedCoordinates(coordinates: Vector[Double]): Either[TrialMlFailure, Unit] =
    if coordinates.length != dimension then
      Left(TrialMlFailure.Criterion(CriterionJet.Error.Length("coordinates", dimension, coordinates.length)))
    else criterion(CriterionJet.validateCoordinates(coordinates))

  private def amplitudes: Vector[Double] = buffer.amplitudes.toVector

  def valueAtNode(node: Int): TrialMlAttempt[CoherentCriterionValue] =
    val before = mark(objective.work)
    val result = for
      current <- currentEpoch
      cached <- checkedNode(node)
      energy <- objective.mlValueAtNode(node, buffer, energy).left.map(error => TrialMlFailure.Backend(error.message))
      _ <- if energy.isFinite then Right(()) else Left(TrialMlFailure.Backend("node raw energy solve refused"))
      raw <- criterion(RawEnergyValue.checked(cached.reference, current, energy, amplitudes, amplitudeCount))
      det <- criterion(DeterminantValue.checked(cached.reference, cached.determinant.value))
      pair <- criterion(CoherentCriterionValue.checked(raw, det))
    yield pair
    finish(before, result)

  def jetAtNode(node: Int): TrialMlAttempt[CoherentCriterionJet] =
    val before = mark(objective.work)
    val result = for
      current <- currentEpoch
      cached <- checkedNode(node)
      completed <- objective.mlJetAtNode(node, buffer, energy).left.map(error => TrialMlFailure.Backend(error.message))
      _ <- if completed then Right(()) else Left(TrialMlFailure.Criterion(CriterionJet.Error.ProfilingRefused))
      raw <- criterion(RawEnergyJet.checked(cached.reference, current, buffer.toJet, amplitudeCount))
      pair <- criterion(CoherentCriterionJet.checked(raw, cached.determinant))
    yield pair
    finish(before, result)

  def valueAt(coordinates: Vector[Double]): TrialMlAttempt[CoherentCriterionValue] =
    val before = mark(objective.work)
    val result = for
      current <- currentEpoch
      _ <- checkedCoordinates(coordinates)
      values <- objective.mlValueAt(coordinates.toArray, buffer, energy).left.map(error => TrialMlFailure.Backend(error.message))
      (energy, determinant) = values
      _ <- if energy.isFinite then Right(()) else Left(TrialMlFailure.Backend("continuous raw energy solve refused"))
      reference <- criterion(CriterionReference.checked(owner, coordinates, CriterionDerivativeOrder.Value))
      raw <- criterion(RawEnergyValue.checked(reference, current, energy, amplitudes, amplitudeCount))
      det <- criterion(DeterminantValue.checked(reference, determinant))
      pair <- criterion(CoherentCriterionValue.checked(raw, det))
    yield pair
    finish(before, result)

  def jetAt(coordinates: Vector[Double]): TrialMlAttempt[CoherentCriterionJet] =
    val before = mark(objective.work)
    val result = for
      current <- currentEpoch
      _ <- checkedCoordinates(coordinates)
      evaluated <- objective.mlJetAt(coordinates.toArray, buffer, energy).left.map(error => TrialMlFailure.Backend(error.message))
      (completed, attempt) = evaluated
      _ <- if completed then Right(()) else Left(TrialMlFailure.Criterion(CriterionJet.Error.ProfilingRefused))
      jet <- attempt.outcome.left.map(error => TrialMlFailure.Backend(error.message))
      reference <- criterion(CriterionReference.checked(owner, coordinates, CriterionDerivativeOrder.Full))
      raw <- criterion(RawEnergyJet.checked(reference, current, buffer.toJet, amplitudeCount))
      det <- criterion(DeterminantJet.checked(reference, jet.value, jet.gradient, jet.hessian))
      pair <- criterion(CoherentCriterionJet.checked(raw, det))
    yield pair
    finish(before, result)

private[profile] final case class TrialMlReadoutPayload(
    raw: TrialBandedReadout, summary: TrialConditionalSummary,
    owner: CriterionOwner, epoch: CriterionEpoch, coordinates: Vector[Double],
    numerical: TrialBandedWorkSnapshot, measurement: TrialResidualMeasurementWork,
    coefficientScratchValues: Int, measurementScratchValues: Int)

private[profile] object TrialBandedMlBackend:
  private def difference(before: TrialBandedWorkSnapshot, after: TrialBandedWorkSnapshot): TrialBandedWorkSnapshot =
    TrialBandedWorkSnapshot(
      after.voxels - before.voxels,
      after.trialBasisScores - before.trialBasisScores,
      after.bankValueEvaluations - before.bankValueEvaluations,
      after.jetEvaluations - before.jetEvaluations,
      after.amplitudeCorrections - before.amplitudeCorrections,
      after.bandedSolveCalls - before.bandedSolveCalls,
      after.bandedRightHandSides - before.bandedRightHandSides,
      after.continuousFactors - before.continuousFactors,
      after.exactReadoutFactors - before.exactReadoutFactors,
      TrialBandedAttemptedWorkSnapshot(
        after.attempted.referenceAttempts - before.attempted.referenceAttempts,
        after.attempted.referenceFailures - before.attempted.referenceFailures,
        after.attempted.partialReferenceFailures - before.attempted.partialReferenceFailures,
        after.attempted.releaseFailures - before.attempted.releaseFailures,
        after.attempted.factorAttempts - before.attempted.factorAttempts,
        after.attempted.factorFailures - before.attempted.factorFailures,
        after.attempted.solveAttempts - before.attempted.solveAttempts,
        after.attempted.solveFailures - before.attempted.solveFailures,
        after.attempted.rightHandSideAttempts - before.attempted.rightHandSideAttempts,
        after.attempted.rightHandSideFailures - before.attempted.rightHandSideFailures,
        after.attempted.jetAttempts - before.attempted.jetAttempts,
        after.attempted.jetFailures - before.attempted.jetFailures,
        after.attempted.readoutAttempts - before.attempted.readoutAttempts,
        after.attempted.readoutFailures - before.attempted.readoutFailures,
        after.attempted.exactReadoutFactorAttempts - before.attempted.exactReadoutFactorAttempts,
        after.attempted.exactReadoutFactorFailures - before.attempted.exactReadoutFactorFailures,
        after.attempted.conditionalReadoutAttempts - before.attempted.conditionalReadoutAttempts,
        after.attempted.conditionalReadoutFailures - before.attempted.conditionalReadoutFailures,
        after.attempted.conditionalInverseAttempts - before.attempted.conditionalInverseAttempts,
        after.attempted.conditionalInverseFailures - before.attempted.conditionalInverseFailures,
        after.attempted.conditionalCorrectionAttempts - before.attempted.conditionalCorrectionAttempts,
        after.attempted.conditionalCorrectionFailures - before.attempted.conditionalCorrectionFailures,
        after.attempted.firstOrderAttempts - before.attempted.firstOrderAttempts,
        after.attempted.firstOrderFailures - before.attempted.firstOrderFailures))

  private[profile] final case class NodeDeterminant(reference: CriterionReference, determinant: DeterminantJet)

  /** Work marks read only from the objective's own counters, which the helper
    * charges where it runs; an attempt's work is always a before/after delta.
    */
  private final case class Mark(
      attempted: TrialBandedAttemptedWorkSnapshot,
      bandFactorAttempts: Long,
      bandFactorFailures: Long,
      membershipRightHandSides: Long,
      helperSolves: Long,
      helperRightHandSides: Long,
      helperMembership: Long,
      helperConditionFactors: Long,
      helperLogDetJets: Long)

  private def mark(work: TrialBandedWork): Mark =
    Mark(work.snapshot.attempted, work.bandFactorAttempts, work.bandFactorFailures, work.membershipRightHandSides,
      work.helperSolveAttempts, work.helperRightHandSides, work.helperMembershipRightHandSides,
      work.helperConditionFactorAttempts, work.helperLogDetJetAttempts)

  private def delta(before: Mark, after: Mark, refused: Boolean): TrialMlWork =
    val a = before.attempted
    val b = after.attempted
    val bands = after.bandFactorAttempts - before.bandFactorAttempts
    val helperRhs = after.helperRightHandSides - before.helperRightHandSides
    val helperMembership = after.helperMembership - before.helperMembership
    TrialMlWork(
      referenceAttempts = b.referenceAttempts - a.referenceAttempts,
      nFactorAttempts = bands,
      nFactorFailures = after.bandFactorFailures - before.bandFactorFailures,
      solveAttempts = b.solveAttempts - a.solveAttempts + after.helperSolves - before.helperSolves,
      rightHandSideAttempts = b.rightHandSideAttempts - a.rightHandSideAttempts + helperRhs,
      membershipRightHandSides = after.membershipRightHandSides - before.membershipRightHandSides + helperMembership,
      derivativeRightHandSides = helperRhs - helperMembership,
      smallFactorAttempts = b.factorAttempts - a.factorAttempts - bands +
        after.helperConditionFactors - before.helperConditionFactors,
      logDetRecursionAttempts = after.helperLogDetJets - before.helperLogDetJets,
      failures = if refused then 1L else 0L)

  /** Requires a fixed finite positive intrinsic lambda, and full determinant-jet
    * support at every node; there is no scalar-only ML backend. Node setup work
    * is the legacy node-bank construction plus the helper's node determinant jets.
    */
  def make(objective: TrialBandedObjective): TrialMlAttempt[TrialBandedMlBackend] =
    val preparation = objective.preparation
    val c = preparation.conditions.toLong
    val setup = objective.setupReceipt.work
    val nodeReferences = objective.setupReceipt.nodeReferenceAttempts
    // A successful bank built one accepted band and one scalar determinant per node reference.
    var work = TrialMlWork(
      referenceAttempts = setup.referenceAttempts,
      nFactorAttempts = nodeReferences,
      solveAttempts = setup.solveAttempts,
      rightHandSideAttempts = setup.rightHandSideAttempts,
      membershipRightHandSides = nodeReferences * c,
      smallFactorAttempts = setup.factorAttempts - nodeReferences)
    var nodeStart: Option[Mark] = None
    // A refusal reports the setup work done so far, including partial node determinant work.
    def refuse(error: TrialMlFailure): TrialMlAttempt[TrialBandedMlBackend] =
      val partial = nodeStart.fold(TrialMlWork())(start => delta(start, mark(objective.work), refused = false))
      TrialMlAttempt(Left(error), work + partial + TrialMlWork(failures = 1L))
    val owner = CriterionOwner.checked(preparation.lambda) match
      case Left(error) => return refuse(TrialMlFailure.Criterion(error))
      case Right(value) => value
    val cached = Vector.newBuilder[NodeDeterminant]
    val beforeNodes = mark(objective.work)
    nodeStart = Some(beforeNodes)
    var node = 0
    while node < objective.grid.count do
      val attempt = objective.mlNodeDeterminant(node) match
        case Left(error) => return refuse(TrialMlFailure.Backend(error.message))
        case Right(value) => value
      val jet = attempt.outcome match
        case Left(error) => return refuse(TrialMlFailure.Backend(s"node $node determinant: ${error.message}"))
        case Right(value) => value
      val coordinates = objective.grid.point(node).coordinates
      val entry = for
        reference <- CriterionReference.checked(owner, coordinates, CriterionDerivativeOrder.Full)
        determinant <- DeterminantJet.checked(reference, jet.value, jet.gradient, jet.hessian)
      yield NodeDeterminant(reference, determinant)
      entry match
        case Left(error) => return refuse(TrialMlFailure.Criterion(error))
        case Right(value) => cached += value
      node += 1
    // The node determinant jets were charged to the objective's counters: they are setup work.
    work = work + delta(beforeNodes, mark(objective.work), refused = false)
    val nodes = cached.result()
    TrialMlAttempt(Right(new TrialBandedMlBackend(objective, owner, nodes, work)), work)
