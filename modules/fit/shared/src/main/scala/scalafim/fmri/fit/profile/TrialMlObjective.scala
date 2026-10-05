package scalafim.fmri.fit.profile

import gale.linalg.DMat
import gale.linalg.verifySymmetric
import gale.spectral.{Eigen, EigenSelection, EigenVectors}

/** Attempt accounting supplied by the core, including refused attempts. Setup
  * and per-response work remain separate. These are counts, never factor data.
  * Residual-energy counts include native criterion and native readout evaluation.
  * Source values count sparse trial-basis visits; nuisance values count their own
  * row visits. Response copies count pointAt's owned copy only; residualRows also
  * counts the response-to-residual copy performed at each energy evaluation.
  */
final case class TrialMlWork(
    referenceAttempts: Long = 0L,
    nFactorAttempts: Long = 0L,
    nFactorFailures: Long = 0L,
    solveAttempts: Long = 0L,
    rightHandSideAttempts: Long = 0L,
    membershipRightHandSides: Long = 0L,
    derivativeRightHandSides: Long = 0L,
    smallFactorAttempts: Long = 0L,
    logDetRecursionAttempts: Long = 0L,
    failures: Long = 0L,
    residualEnergyEvaluations: Long = 0L,
    residualEnergyRows: Long = 0L,
    residualEnergySourceValues: Long = 0L,
    responseCopyValues: Long = 0L,
    residualEnergyCoefficientProducts: Long = 0L,
    residualEnergyNuisanceValues: Long = 0L):
  def +(other: TrialMlWork): TrialMlWork = TrialMlWork(
    referenceAttempts + other.referenceAttempts, nFactorAttempts + other.nFactorAttempts,
    nFactorFailures + other.nFactorFailures, solveAttempts + other.solveAttempts,
    rightHandSideAttempts + other.rightHandSideAttempts, membershipRightHandSides + other.membershipRightHandSides,
    derivativeRightHandSides + other.derivativeRightHandSides, smallFactorAttempts + other.smallFactorAttempts,
    logDetRecursionAttempts + other.logDetRecursionAttempts, failures + other.failures,
    residualEnergyEvaluations + other.residualEnergyEvaluations, residualEnergyRows + other.residualEnergyRows,
    residualEnergySourceValues + other.residualEnergySourceValues, responseCopyValues + other.responseCopyValues,
    residualEnergyCoefficientProducts + other.residualEnergyCoefficientProducts,
    residualEnergyNuisanceValues + other.residualEnergyNuisanceValues)

private[profile] enum TrialMlFailure:
  case MissingCapability
  case Criterion(error: CriterionJet.Error)
  case Backend(detail: String)
  case InvalidPolicy(detail: String)
  case IncoherentRepeatedJet(coordinates: Vector[Double])
  case LedgerLimit

  def message: String = s"trial ML objective refused: $this"

private[profile] final case class TrialMlAttempt[+A](result: Either[TrialMlFailure, A], work: TrialMlWork)
private[profile] final case class TrialMlRefusal(error: TrialMlFailure, work: TrialMlWork)

/** Required full capability; no scalar-only implementation is admitted.
  * Each operation atomically returns E and D from ONE owned accepted factor.
  * The native owner must mint reference tokens from that bundle, own derivative
  * storage until evaluation finishes, and count all work including duplication.
  * This declaration and analytic implementations do not admit native ML.
  */
private[profile] trait CoherentTrialMlBackend:
  def grid: NodeGrid
  def amplitudeCount: Int
  def owner: CriterionOwner
  final def intrinsicLambda: Double = owner.intrinsicLambda
  def pointAt(response: Array[Double]): TrialMlAttempt[CriterionEpoch]
  def valueAtNode(node: Int): TrialMlAttempt[CoherentCriterionValue]
  def jetAtNode(node: Int): TrialMlAttempt[CoherentCriterionJet]
  def valueAt(coordinates: Vector[Double]): TrialMlAttempt[CoherentCriterionValue]
  def jetAt(coordinates: Vector[Double]): TrialMlAttempt[CoherentCriterionJet]
  def setupReceipt: TrialMlWork
  def workerWorkSnapshot: TrialMlWork

private[profile] enum TerminalEvidence:
  case Available(criterion: CriterionJet)
  case Unavailable

/** Legacy energy/dataHessian are J/HJ. Raw E/HE and D live in terminalEvidence;
  * augmentedHessian is HJ + 2P. Conditional SD uses 2 sigma2 HJ^-1 only.
  */
private[profile] final case class TrialMlDecodeResult(
    decoded: ShapeDecodeResult,
    terminalEvidence: TerminalEvidence,
    retainedJetCount: Int,
    nodeEnergies: Vector[Double],
    augmentedNodeEnergies: Vector[Double],
    setupReceipt: TrialMlWork,
    workerWorkSnapshot: TrialMlWork,
    lastRefusal: Option[TrialMlRefusal])

/** Internal decoder translation. Consumers use the frozen-variance facade. */
private[profile] final class TrialMlObjective private[profile] (
    backend: CoherentTrialMlBackend, sigma2: Double, maxJets: Int) extends ShapeObjective:
  def grid: NodeGrid = backend.grid
  def amplitudeCount: Int = backend.amplitudeCount
  private var epoch: Option[CriterionEpoch] = None
  private var lastSequence = 0L
  private var ledger = Vector.empty[CriterionJet]
  private var fatal: Option[TrialMlFailure] = None
  private var refusal: Option[TrialMlRefusal] = None
  private var work = TrialMlWork()

  def retainedJetCount: Int = ledger.length
  def attemptWork: TrialMlWork = work
  def lastRefusal: Option[TrialMlRefusal] = refusal
  def coherenceFailure: Option[TrialMlFailure] = fatal

  private def refuse(error: TrialMlFailure, attempted: TrialMlWork): Unit =
    refusal = Some(TrialMlRefusal(error, attempted))

  private def checked[A](attempt: TrialMlAttempt[A]): Either[TrialMlFailure, A] =
    work = work + attempt.work
    attempt.result.left.map { error =>
      refuse(error, attempt.work)
      error
    }

  def pointAt(response: Array[Double]): Either[TrialMlFailure, Unit] =
    epoch = None
    ledger = Vector.empty
    fatal = None
    refusal = None
    work = TrialMlWork()
    val attempt = backend.pointAt(response)
    checked(attempt).flatMap { next =>
      val valid =
        if !(next.owner eq backend.owner) then Left(CriterionJet.Error.WrongOwner)
        else if lastSequence >= next.sequence then Left(CriterionJet.Error.WrongEpoch)
        else Right(())
      valid.left.map(TrialMlFailure.Criterion.apply).map { _ => epoch = Some(next); lastSequence = next.sequence }
        .left.map { error => refuse(error, attempt.work); error }
    }

  private def binding(reference: CriterionReference, responseEpoch: CriterionEpoch, coordinates: Vector[Double])
      : Either[TrialMlFailure, Unit] =
    val checked =
      if !(reference.owner eq backend.owner) then Left(CriterionJet.Error.WrongOwner)
      else if reference.coordinates != coordinates then Left(CriterionJet.Error.WrongCoordinates)
      else if !epoch.exists(_ eq responseEpoch) then Left(CriterionJet.Error.WrongEpoch)
      else Right(())
    checked.left.map(TrialMlFailure.Criterion.apply)

  private def nodeCoordinates(node: Int): Either[TrialMlFailure, Vector[Double]] =
    if node < 0 || node >= grid.count then Left(TrialMlFailure.InvalidPolicy(s"invalid node $node"))
    else Right(grid.point(node).coordinates)

  private def validCoordinates(coordinates: Vector[Double]): Either[TrialMlFailure, Unit] =
    if coordinates.length != grid.dimension then Left(TrialMlFailure.Criterion(CriterionJet.Error.Length("coordinates", grid.dimension, coordinates.length)))
    else CriterionJet.validateCoordinates(coordinates).left.map(TrialMlFailure.Criterion.apply)

  private def clear(out: ProfileJetBuffer): Unit =
    out.energy = Double.PositiveInfinity
    java.util.Arrays.fill(out.gradient, Double.NaN)
    java.util.Arrays.fill(out.hessian, Double.NaN)
    java.util.Arrays.fill(out.amplitudes, Double.NaN)
    out.curvature = CurvatureStatus.GramNotPositiveDefinite

  private def bufferValid(out: ProfileJetBuffer): Either[TrialMlFailure, Unit] =
    if out.dimension != grid.dimension || out.amplitudeCount != amplitudeCount then
      Left(TrialMlFailure.InvalidPolicy("decoder buffer dimensions do not match the backend"))
    else Right(())

  private def sameEvaluation(a: CriterionJet, b: CriterionJet): Boolean =
    a.raw.jet == b.raw.jet && a.determinant.map(d => (d.value, d.gradient, d.hessian)) ==
      b.determinant.map(d => (d.value, d.gradient, d.hessian)) && a.minimizationJet == b.minimizationJet

  private def retain(jet: CriterionJet): Either[TrialMlFailure, Unit] =
    if ledger.exists(previous => previous.raw.reference.coordinates == jet.raw.reference.coordinates && !sameEvaluation(previous, jet)) then
      val error = TrialMlFailure.IncoherentRepeatedJet(jet.raw.reference.coordinates)
      fatal = Some(error)
      Left(error)
    else if ledger.length >= maxJets then
      fatal = Some(TrialMlFailure.LedgerLimit)
      Left(TrialMlFailure.LedgerLimit)
    else
      ledger = ledger :+ jet
      Right(())

  private def full(coordinates: Vector[Double], out: ProfileJetBuffer)(evaluate: => TrialMlAttempt[CoherentCriterionJet]): Boolean =
    clear(out)
    var attempted = TrialMlWork()
    val result = for
      _ <- bufferValid(out)
      _ <- validCoordinates(coordinates)
      _ <- fatal.toLeft(())
      pair <-
        val attempt = evaluate
        attempted = attempt.work
        checked(attempt)
      _ <- binding(pair.raw.reference, pair.raw.epoch, coordinates)
      _ <- CriterionJet.validateAmplitudes(pair.raw.jet.amplitudes, amplitudeCount).left.map(TrialMlFailure.Criterion.apply)
      jet <- CriterionJet.assemble(sigma2, CriterionJet.Input.TrialMl(pair)).left.map(TrialMlFailure.Criterion.apply)
      _ <- retain(jet)
    yield jet.minimizationJet
    result match
      case Left(error) => refuse(error, attempted); false
      case Right(jet) =>
        out.energy = jet.energy
        jet.gradient.copyToArray(out.gradient)
        jet.hessian.copyToArray(out.hessian)
        jet.amplitudes.copyToArray(out.amplitudes)
        out.curvature = jet.curvature
        true

  private def value(coordinates: Vector[Double], out: Option[ProfileJetBuffer])(evaluate: => TrialMlAttempt[CoherentCriterionValue]): Double =
    out.foreach(clear)
    var attempted = TrialMlWork()
    val result = for
      _ <- out.fold[Either[TrialMlFailure, Unit]](Right(()))(bufferValid)
      _ <- validCoordinates(coordinates)
      _ <- fatal.toLeft(())
      pair <-
        val attempt = evaluate
        attempted = attempt.work
        checked(attempt)
      _ <- binding(pair.raw.reference, pair.raw.epoch, coordinates)
      _ <- CriterionJet.validateAmplitudes(pair.raw.amplitudes, amplitudeCount).left.map(TrialMlFailure.Criterion.apply)
      assembled <- CriterionJet.assembleValue(sigma2, pair.raw.energy, pair.determinant.value).left.map(TrialMlFailure.Criterion.apply)
    yield (assembled._1, pair.raw.amplitudes)
    result match
      case Left(error) => refuse(error, attempted); Double.PositiveInfinity
      case Right((j, amplitudes)) =>
        out.foreach { buffer => buffer.energy = j; amplitudes.copyToArray(buffer.amplitudes) }
        j

  def scoreNode(node: Int): Double = nodeCoordinates(node) match
    case Left(error) => refuse(error, TrialMlWork()); Double.PositiveInfinity
    case Right(coords) => value(coords, None)(backend.valueAtNode(node))

  def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean = nodeCoordinates(node) match
    case Left(error) => clear(out); refuse(error, TrialMlWork()); false
    case Right(coords) => full(coords, out)(backend.jetAtNode(node))

  def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
    full(coordinates.toVector, out)(backend.jetAt(coordinates.toVector))

  def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
    value(coordinates.toVector, Some(out))(backend.valueAt(coordinates.toVector))

  def terminal(result: ShapeDecodeResult): TerminalEvidence =
    ledger.find(jet => jet.raw.reference.coordinates == result.coordinates && jet.minimizationJet.hessian == result.dataHessian) match
      case Some(jet) if result.status != DecodeStatus.NoAdmissibleNode => TerminalEvidence.Available(jet)
      case _ => TerminalEvidence.Unavailable

/** One frozen sigma2 binds objective assembly, decoder search, and SD. */
private[profile] final class TrialMlDecoder private (
    backend: CoherentTrialMlBackend, val sigma2: Double, budget: DecodeBudget, prior: Option[ShapePrior]):
  private val objective = new TrialMlObjective(backend, sigma2, budget.maxJets)
  private val decoder = new ShapeDecoder(objective, budget, prior, sigma2)

  def decode(response: Array[Double], counters: DecoderCounters): TrialMlAttempt[TrialMlDecodeResult] =
    val result = objective.pointAt(response).flatMap { _ =>
      val decoded = decoder.decode(counters)
      objective.coherenceFailure.toLeft(TrialMlDecodeResult(decoded, objective.terminal(decoded), objective.retainedJetCount,
        decoder.lastNodeEnergies.toVector, decoder.lastAugmentedNodeEnergies.toVector,
        backend.setupReceipt, backend.workerWorkSnapshot, objective.lastRefusal))
    }
    TrialMlAttempt(result, objective.attemptWork)

object TrialMlDecoder:
  private[profile] def checked(backend: Option[CoherentTrialMlBackend], sigma2: Double, budget: DecodeBudget, prior: Option[ShapePrior])
      : Either[TrialMlFailure, TrialMlDecoder] =
    for
      _ <- CriterionJet.validateSigma2(sigma2).left.map(TrialMlFailure.Criterion.apply)
      core <- backend.toRight(TrialMlFailure.MissingCapability)
      _ <- validatePolicy(core, budget, prior)
    yield new TrialMlDecoder(core, sigma2, budget, prior)

  private def validatePolicy(backend: CoherentTrialMlBackend, budget: DecodeBudget, prior: Option[ShapePrior]): Either[TrialMlFailure, Unit] =
    val d = backend.grid.dimension
    val count = backend.grid.nodesPerAxis.foldLeft(1L)(_ * _)
    if d < 1 || d > 3 || count <= 0L || count > 100000L || backend.amplitudeCount < 1 then
      Left(TrialMlFailure.InvalidPolicy("grid must have dimension 1..3, 2..100000 nodes, and positive amplitude count"))
    else if budget.weakSdLimit.nonEmpty && budget.weakSdLimit.length != d then
      Left(TrialMlFailure.InvalidPolicy("weak SD limits must match the chart"))
    else prior match
      case None => Right(())
      case Some(p) if p.dimension != d || p.mean.exists(!_.isFinite) || p.precision.exists(!_.isFinite) =>
        Left(TrialMlFailure.InvalidPolicy("prior mean and precision must be finite and chart-dimensional"))
      case Some(p) =>
        val precision = DMat.tabulate(d, d)((i, j) => p.precision(i * d + j))
        for
          _ <- precision.verifySymmetric(0.0).left.map(_ => TrialMlFailure.InvalidPolicy("prior precision must be symmetric"))
          spectrum <- Eigen.eigSymmetric(precision, EigenSelection.All, EigenVectors.ValuesOnly)
            .left.map(error => TrialMlFailure.InvalidPolicy(s"prior spectrum failed: $error"))
          _ <- if (0 until spectrum.size).forall(i => spectrum.eigenvalues(i) >= 0.0) then Right(())
            else Left(TrialMlFailure.InvalidPolicy("prior precision must be positive semidefinite"))
        yield ()
