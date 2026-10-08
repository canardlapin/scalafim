package scalafim.fmri.mvpa.pattern

import gale.backend.Backend.given
import gale.linalg.{DMat, QR, QROptions, QRPivoting}
import multivar.core.SemanticSpace
import multivar.inference.{CanonicalResidualBasis, CanonicalResidualMethod, InferenceError, PermutationAction, ReplicateId, RowCount}
import resample4s.kernel.{Permutation, Seed}
import scalafim.fmri.mvpa.{AxisDigest, EvidenceIdentity, MultiResponse, Observations}
import scalafim.fmri.mvpa.analysis.{EvidenceExposure, ExposureControl, ExposureScope, PlanId}
import scalafim.fmri.mvpa.execution.*
import scalafim.fmri.mvpa.measurement.MeasurementId

enum VoxelFamilyError:
  case Invalid(detail: String)
  case Binding(detail: String)
  case Exposure(detail: String)
  case Unavailable(detail: String)
  case Budget(required: BigInt, allowed: Long)
  case Provider(cause: InferenceError)
  case Numerical(detail: String)
  case Evidence(detail: String)
  case Execution(cause: AddressError)
  case Incomplete(detail: String)

enum VoxelFamilyReferenceMode:
  case DistinctNonIdentityMonteCarlo
  case ExactGroupIncludingIdentity

final case class VoxelFamilyBudget(batchVoxels: Int = 128, maximumOwnedCells: Long = 10000000L,
    maximumTransformIndices: Long = 1000000L, maximumCandidates: Int = 100000):
  require(batchVoxels > 0 && maximumOwnedCells >= 0L && maximumTransformIndices >= 0L && maximumCandidates >= 0)

/** Caller-declared separable joint Gaussian brain errors, conditional on the
  * fixed confirmation targets and nuisance: E ~ MN(0, rowShape, voxelShape).
  * The row shape is known; voxel covariance is arbitrary. This declaration
  * strengthens marginal Gaussian columns and does not authenticate origin.
  * Each hypothesis is zero ALL target coefficients of its named voxel.
  * Component-specific zero coefficients and component association are not
  * this omnibus null. Strong control remains a qualification candidate. */
final class FrozenVoxelOmnibusReference private (
    val design: ConfirmationDesign[?, ?], val members: Vector[String],
    val brainSource: EvidenceIdentity, val targetSource: EvidenceIdentity,
    val jointGaussianReceipt: String, val seed: Seed, val draws: Int, val alpha: Double,
    val replay: PatternReplay, val budget: VoxelFamilyBudget, val executionPlan: PlanId, val identity: String,
    val actionSeed: Seed, val actionSeedLineage: String
):
  val mode: VoxelFamilyReferenceMode = VoxelFamilyReferenceMode.DistinctNonIdentityMonteCarlo
  val nullHypothesis: String = "all frozen-target forward coefficients equal zero at each named voxel"
  val cachePolicy: String = "condition on targets, nuisance, known row shape and discovery; freeze their geometry; refit every brain response under each common action"

object FrozenVoxelOmnibusReference:
  def freeze(design: ConfirmationDesign[?, ?], members: Vector[String],
      brainSource: EvidenceIdentity, targetSource: EvidenceIdentity, exposure: EvidenceExposure,
      jointGaussianReceipt: String, seed: Seed, draws: Int, replay: PatternReplay, alpha: Double = 0.05,
      budget: VoxelFamilyBudget = VoxelFamilyBudget(),
      mode: VoxelFamilyReferenceMode = VoxelFamilyReferenceMode.DistinctNonIdentityMonteCarlo
  ): Either[VoxelFamilyError, FrozenVoxelOmnibusReference] =
    val rows = design.confirmation.samples.rows.descriptor
    val neural = design.discovery.artifact.factors.neuralAxis.descriptor
    val target = design.discovery.targetProjection.input.descriptor
    val known = design.errorLaw match
      case ConfirmationErrorLaw.IndependentGaussian => true
      case ConfirmationErrorLaw.DependentTime(bound) => bound.status == TemporalCovarianceStatus.KnownGaussian
      case ConfirmationErrorLaw.RepeatedSubjects(bound) => bound.covariance.status == TemporalCovarianceStatus.KnownGaussian
    for
      _ <- if mode == VoxelFamilyReferenceMode.DistinctNonIdentityMonteCarlo then Right(())
        else Left(VoxelFamilyError.Unavailable("complete group enumeration is not supplied by this provider action"))
      _ <- if members == design.contract.multiplicityFamily && members.size == neural.size && members.distinct.size == members.size then Right(())
        else Left(VoxelFamilyError.Binding("complete ordered one-omnibus-per-voxel contract family required"))
      _ <- if brainSource.rows == rows && targetSource.rows == rows && brainSource.columns == neural && targetSource.columns == target then Right(())
        else Left(VoxelFamilyError.Binding("actual row/neural/target source endpoints"))
      _ <- if exposure.reference.evidenceIdentity == design.confirmation.identity then Right(())
        else Left(VoxelFamilyError.Exposure("actual confirmation identity"))
      _ <- ExposureControl.untouchedConfirmation(exposure, ExposureScope.Holdout).left.map(error => VoxelFamilyError.Exposure(error.toString))
      _ <- if known && jointGaussianReceipt.trim.nonEmpty then Right(())
        else Left(VoxelFamilyError.Unavailable("known common row shape and explicit separable joint Gaussian conditional law required"))
      _ <- replay match
        case PatternReplay.SinglePass if budget.batchVoxels < neural.size =>
          Left(VoxelFamilyError.Invalid("multiple brain capture batches require an explicit Repeatable source receipt"))
        case PatternReplay.Repeatable(receipt) if receipt.trim.isEmpty =>
          Left(VoxelFamilyError.Invalid("repeatable source capture requires a nonblank receipt"))
        case _ => Right(())
      _ <- if draws > 0 && draws <= budget.maximumCandidates && alpha.isFinite && alpha > 0.0 && alpha < 1.0 then Right(())
        else Left(VoxelFamilyError.Invalid("positive fixed B within candidate budget and frozen alpha in (0,1)"))
    yield
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.voxel-omnibus-family.v1"); writer.string(design.identity)
        members.foreach(writer.string); brainSource.writeFramed(writer); targetSource.writeFramed(writer)
        writer.string(exposure.identity.text); writer.string(exposure.reference.plan.text)
        writer.string(jointGaussianReceipt); writer.string(seed.value.toString); writer.intLE(draws)
        writer.string(java.lang.Double.toHexString(alpha)); writer.string(replay.toString); writer.string(budget.toString)
        writer.string("conditional-fixed-targets; separable-joint-Gaussian; whiten-Huh-Jhun; omnibus-upper-tail; distinct-nonidentity-plus-one; pending-release")
      val address = WorkAddress(exposure.reference.plan, SplitCoordinate(0).toOption.get,
        ReplicateCoordinate(0).toOption.get, StageCoordinate(0).toOption.get,
        MeasurementId.unsafe(s"voxel-family-$identity"), MeasurementCoordinate(0).toOption.get)
      val actionSeed = WorkAddress.seed(seed, address)
      val lineage = s"WorkAddress.seed/seed-path-v1; plan=${exposure.reference.plan.text}; family=$identity; split=0; replicate=0; stage=0; measurement=voxel-family-$identity; ordinal=0; provider-candidate-index"
      new FrozenVoxelOmnibusReference(design, members, brainSource, targetSource, jointGaussianReceipt,
        seed, draws, alpha, replay, budget, exposure.reference.plan, identity, actionSeed, lineage)

final case class VoxelOmnibusEstimate(coefficients: Vector[Double], residualScale: Double, omnibusF: Double):
  require(coefficients.nonEmpty && coefficients.forall(_.isFinite))
  require(residualScale.isFinite && residualScale > 0.0 && omnibusF.isFinite && omnibusF >= 0.0)

final case class VoxelActionReceipt(replicate: Int, candidate: Int, permutationDigest: String, actionSeed: Long):
  require(replicate >= 0 && candidate >= 0 && permutationDigest.nonEmpty)

/** Compact contribution: no retained replicate-by-voxel statistic matrix. */
final case class VoxelReplicateReceipt(action: VoxelActionReceipt, maximum: Option[Double],
    fieldDigest: String, fieldMembers: Int, completeMembers: Int, failedMembers: Int,
    firstFailure: Option[(Int, VoxelFamilyError)]):
  require(fieldDigest.nonEmpty && fieldMembers > 0 && completeMembers >= 0 && failedMembers >= 0)
  require(completeMembers.toLong + failedMembers == fieldMembers.toLong)
  require(firstFailure.nonEmpty == (failedMembers > 0))
  require(firstFailure.forall((index, _) => index >= 0 && index < fieldMembers))
  require(maximum.nonEmpty == (completeMembers > 0))
  require(maximum.forall(value => value.isFinite && value >= 0.0))

/** Constructible only after the complete observed family and every fixed-B
  * action succeed. This is an execution proof, not an admitted C1 claim. */
final class CompletedVoxelFamily private[pattern] (
    val reference: FrozenVoxelOmnibusReference, val observed: Vector[Double],
    val observedDigest: String, val replicates: Vector[VoxelReplicateReceipt], val identity: String
):
  val maxima: Vector[Double] = replicates.map(_.maximum.get)
  def matchesObservedDigest: Boolean = observedDigest == VoxelFamilyRandomization.fieldDigest(observed.map(Right(_)))
  def admittedC1: Either[VoxelFamilyError, Nothing] =
    Left(VoxelFamilyError.Unavailable("released family C1 requires frozen M4.09 qualification"))

final class VoxelFamilyRun private[pattern] (
    val prepared: PreparedVoxelFamily,
    val execution: FamilyState[VoxelReplicateReceipt, Nothing, Vector[VoxelReplicateReceipt]]
):
  val localEstimates: Vector[Either[VoxelFamilyError, VoxelOmnibusEstimate]] = prepared.localEstimates
  def completed: Either[VoxelFamilyError, CompletedVoxelFamily] =
    if localEstimates.exists(_.isLeft) then Left(VoxelFamilyError.Incomplete("observed family contains failed voxels; local estimates retained"))
    else execution match
      case FamilyState.Complete(value) =>
        val fields = value.reduction
        val p = prepared.reference.members.size
        if fields.size != prepared.reference.draws || fields.zipWithIndex.exists((field, i) =>
            field.action.replicate != i || field.fieldMembers != p || field.completeMembers != p ||
              field.failedMembers != 0 || field.firstFailure.nonEmpty || field.maximum.isEmpty) then
          Left(VoxelFamilyError.Incomplete("fixed B has failed or missing family members"))
        else
          val observed = localEstimates.map(_.toOption.get.omnibusF)
          val digest = VoxelFamilyRandomization.fieldDigest(observed.map(Right(_)))
          val identity = AxisDigest.sha256Hex: writer =>
            writer.string("scalafim.completed-voxel-family.v1"); writer.string(prepared.reference.identity)
            writer.string(digest); writer.string(prepared.cacheReceipt)
            fields.foreach: field =>
              writer.intLE(field.action.replicate); writer.intLE(field.action.candidate)
              writer.string(field.action.permutationDigest); writer.string(field.action.actionSeed.toString); writer.string(field.fieldDigest)
              writer.intLE(field.fieldMembers); writer.intLE(field.completeMembers); writer.intLE(field.failedMembers)
              writer.string(java.lang.Double.toHexString(field.maximum.get))
          Right(new CompletedVoxelFamily(prepared.reference, observed, digest, fields, identity))
      case _ => Left(VoxelFamilyError.Incomplete("cancelled, partial, failed or conflicting retry execution cannot report completed B"))

final class PreparedVoxelFamily private[pattern] (
    val reference: FrozenVoxelOmnibusReference, val localEstimates: Vector[Either[VoxelFamilyError, VoxelOmnibusEstimate]],
    val plannedOwnedCells: Long, val transformIndices: Long, val residualRows: Int,
    val residualDegreesOfFreedom: Int, val candidateDraws: Int, val identityDraws: Int,
    val duplicateDraws: Int, val actions: Vector[VoxelActionReceipt], val cacheReceipt: String,
    private val brain: DMat, private val target: DMat, private val factor: QR,
    private val permutations: Vector[Permutation]
):
  private val sourceEligibility = localEstimates.map(_.map(_ => ()))
  val plannedFailureSummaryEntries: Int = reference.draws
  /** The schedule contains replicate ordinals, not new randomization draws.
    * Reordering, omission and retry use the already frozen common action. */
  def run(schedule: Option[Vector[Int]] = None, cancellationRequested: () => Boolean = () => false,
      beforeCommit: BeforeCommit = BeforeCommit.allow): Either[VoxelFamilyError, VoxelFamilyRun] =
    val work = Vector.tabulate(reference.draws): index =>
      new WorkUnit[VoxelReplicateReceipt, Nothing]:
        val address = WorkAddress(reference.executionPlan, SplitCoordinate(0).toOption.get,
          ReplicateCoordinate(index).toOption.get, StageCoordinate(0).toOption.get,
          MeasurementId.unsafe(s"voxel-family-${reference.identity}"), MeasurementCoordinate(0).toOption.get)
        def acquire(): Either[UnitError, ExecutionResource] = Right(new ExecutionResource:
          def close(): Unit = ())
        def compute(resource: ExecutionResource, seed: Seed): Either[UnitError, UnitEvaluation[VoxelReplicateReceipt, Nothing]] =
          val field = VoxelFamilyRandomization.evaluate(brain, target, factor, residualDegreesOfFreedom,
            permutations(index), reference.budget.batchVoxels, sourceEligibility)
          val statistics = field.map(_.map(_.omnibusF))
          val valid = statistics.flatMap(_.toOption)
          val firstFailure = statistics.zipWithIndex.collectFirst { case (Left(error), i) => i -> error }
          val receipt = VoxelReplicateReceipt(actions(index), valid.maxOption,
            VoxelFamilyRandomization.fieldDigest(statistics), statistics.size, valid.size,
            statistics.size - valid.size, firstFailure)
          Right(UnitEvaluation.Complete(UnitContribution(receipt.fieldDigest, receipt, Vector.empty)))
    if schedule.exists(_.exists(index => index < 0 || index >= reference.draws)) then Left(VoxelFamilyError.Invalid("unknown replicate in schedule"))
    else
      val reduction = new Reduction[VoxelReplicateReceipt, Vector[VoxelReplicateReceipt]]:
        def empty: Vector[VoxelReplicateReceipt] = Vector.empty
        def add(current: Vector[VoxelReplicateReceipt], value: VoxelReplicateReceipt): Vector[VoxelReplicateReceipt] = current :+ value
      LocalExecution.run(work, reference.seed, reduction, schedule.map(_.map(work(_))), cancellationRequested, beforeCommit)
        .left.map(VoxelFamilyError.Execution.apply).map(state => new VoxelFamilyRun(this, state))

object VoxelFamilyRandomization:
  /** Budget is a conservative simultaneous owned numeric-cell bound. It
    * includes retained prepared n*p data, B*residual-row action indices and
    * O(B) maxima, but never B*p statistics. Borrowed input, Scala collection
    * overhead and private Gale kernel scratch are excluded explicitly. */
  def prepare[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace](
      reference: FrozenVoxelOmnibusReference, observations: Observations[S, N], targets: MultiResponse[S, Q]
  ): Either[VoxelFamilyError, PreparedVoxelFamily] =
    val design = reference.design; val n = observations.rows; val p = observations.columns
    val r = design.discovery.targetProjection.matrix.cols; val z = design.nuisance.matrix.cols.toLong + 1L
    val b = math.min(p, reference.budget.batchVoxels)
    val transforms = BigInt(reference.draws) * n
    val covariance = design.errorLaw match
      case ConfirmationErrorLaw.IndependentGaussian => None
      case ConfirmationErrorLaw.DependentTime(bound) => Some(bound.covariance)
      case ConfirmationErrorLaw.RepeatedSubjects(bound) => Some(bound.covariance.covariance)
    val covarianceCells = covariance.map(value => BigInt(value.storedCells) + 4 * (BigInt(n) + value.rank) * math.max(b.toLong, z + r)).getOrElse(BigInt(0))
    val cells = 32 * BigInt(n) * n + 32 * BigInt(n) * (z + r) + 48 * BigInt(z + r) * (z + r) +
      2 * BigInt(n) * p + 12 * BigInt(n) * b + BigInt(p) * b + 6 * BigInt(p) * r + 8 * BigInt(p) +
      transforms + 8 * BigInt(reference.draws) + covarianceCells
    def whiten(value: DMat): Either[VoxelFamilyError, DMat] = covariance match
      case None => Right(value)
      case Some(model) => model.whiten(value).left.map(error => VoxelFamilyError.Numerical(error.toString))
    for
      _ <- if observations.identity == reference.brainSource && targets.identity == reference.targetSource then Right(())
        else Left(VoxelFamilyError.Binding("actual sources differ from frozen sources"))
      _ <- if n > 0 && p > 0 && r > 0 && cells <= reference.budget.maximumOwnedCells &&
          Vector(BigInt(n) * n, BigInt(n) * p, BigInt(p) * b, BigInt(n) * (z + r)).forall(_ <= Int.MaxValue) then Right(())
        else Left(VoxelFamilyError.Budget(cells, reference.budget.maximumOwnedCells))
      _ <- if transforms <= reference.budget.maximumTransformIndices && transforms <= Int.MaxValue then Right(())
        else Left(VoxelFamilyError.Budget(transforms, reference.budget.maximumTransformIndices))
      augmented = DMat.tabulate(n, z.toInt)((i, j) => if j == 0 then 1.0 else design.nuisance.matrix(i, j - 1))
      weightedNuisance <- whiten(augmented)
      basis <- CanonicalResidualBasis.from(weightedNuisance, CanonicalResidualMethod.HuhJhun,
        reference.budget.maximumOwnedCells).left.map(VoxelFamilyError.Provider.apply)
      _ <- if basis.matrix.cols > r then Right(()) else Left(VoxelFamilyError.Numerical("positive full-model residual degrees of freedom required"))
      scores <- targets.targets(design.discovery.targetProjection.matrix).left.map(error => VoxelFamilyError.Evidence(error.toString))
      weightedTargets <- whiten(scores)
      projectedTargets <- basis.project(weightedTargets, reference.budget.maximumOwnedCells).left.map(VoxelFamilyError.Provider.apply)
      qr = projectedTargets.qr(QROptions(QRPivoting.Column, Some(1e-12)))
      _ <- if qr.diagnostics.rank.contains(r) then Right(()) else Left(VoxelFamilyError.Numerical("target/nuisance alias or deficient frozen target design"))
      rows <- RowCount(basis.matrix.cols).left.map(VoxelFamilyError.Provider.apply)
      actionPlan <- sample(reference, PermutationAction.unrestricted(rows))
      acquired <- acquireBrain(observations, b, basis, whiten, reference.budget.maximumOwnedCells)
      (brain, sourceFailures) = acquired
      identity <- Permutation.identity(basis.matrix.cols)
        .left.map(error => VoxelFamilyError.Numerical(error.toString))
      df = basis.matrix.cols - r
      locals = evaluate(brain, projectedTargets, qr, df, identity, b, sourceFailures)
    yield
      val (permutations, receipts, candidates, identities, duplicates) = actionPlan
      val cache = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.voxel-omnibus-cache.v1"); writer.string(reference.identity)
        writer.string(reference.actionSeed.value.toString); writer.string(reference.actionSeedLineage)
        writer.string(reference.replay.toString); writer.string("source replay belongs only to initial capture; null fits read owned data")
        writer.intLE(reference.draws); writer.string("one-bounded-failure-summary-per-replicate; all-member-statuses-in-field-digest")
        writer.string("targets-conditioned-fixed; known-row-shape-fixed; nuisance-geometry-fixed; brain-column-fits-recomputed")
        Vector(basis.matrix, projectedTargets, brain).foreach: matrix =>
          writer.intLE(matrix.rows); writer.intLE(matrix.cols)
          var i = 0
          while i < matrix.rows do
            var j = 0
            while j < matrix.cols do
              writer.string(java.lang.Double.toHexString(matrix(i, j))); j += 1
            i += 1
      new PreparedVoxelFamily(reference, locals, cells.toLong, permutations.size.toLong * basis.matrix.cols,
        basis.matrix.cols, df, candidates, identities, duplicates, receipts, cache, brain, projectedTargets, qr, permutations)

  private def sample(reference: FrozenVoxelOmnibusReference, action: PermutationAction):
      Either[VoxelFamilyError, (Vector[Permutation], Vector[VoxelActionReceipt], Int, Int, Int)] =
    val permutations = Vector.newBuilder[Permutation]; val receipts = Vector.newBuilder[VoxelActionReceipt]
    val seen = scala.collection.mutable.HashSet.empty[Permutation]
    var accepted = 0; var candidates = 0; var identities = 0; var duplicates = 0
    while accepted < reference.draws && candidates < reference.budget.maximumCandidates do
      val candidate = ReplicateId(candidates).toOption.get
      val permutation = action.draw(reference.actionSeed, candidate) match
        case Left(error) => return Left(VoxelFamilyError.Provider(error))
        case Right(value) => value
      val order = permutation.toIArray
      val identity = (0 until order.length).forall(i => order(i) == i)
      if identity then identities += 1
      else if !seen.add(permutation) then duplicates += 1
      else
        val digest = AxisDigest.sha256Hex: writer =>
          writer.string("scalafim.voxel-permutation.v1"); writer.intLE(order.length)
          var i = 0
          while i < order.length do
            writer.intLE(order(i)); i += 1
        permutations += permutation
        receipts += VoxelActionReceipt(accepted, candidates, digest, reference.actionSeed.value)
        accepted += 1
      candidates += 1
    if accepted != reference.draws then Left(VoxelFamilyError.Incomplete(s"candidate cap $candidates accepted $accepted of fixed B=${reference.draws}; no shortened denominator"))
    else Right((permutations.result(), receipts.result(), candidates, identities, duplicates))

  private def acquireBrain[S <: SemanticSpace, N <: SemanticSpace](observations: Observations[S, N], batch: Int,
      basis: CanonicalResidualBasis, whiten: DMat => Either[VoxelFamilyError, DMat], maximum: Long
  ): Either[VoxelFamilyError, (DMat, Vector[Either[VoxelFamilyError, Unit]])] =
    val n = observations.rows; val p = observations.columns
    val out = DMat.newBuilder(basis.matrix.cols, p)
    val failures = Array.fill[Option[VoxelFamilyError]](p)(None)
    var first = 0
    while first < p do
      val count = math.min(batch, p - first)
      val selectors = DMat.tabulate(p, count)((i, j) => if i == first + j then 1.0 else 0.0)
      observations.patterns(selectors) match
        case Left(error) =>
          var j = 0
          while j < count do
            failures(first + j) = Some(VoxelFamilyError.Evidence(error.toString)); j += 1
        case Right(values) =>
          var j = 0
          while j < count do
            if (0 until n).exists(i => !values(i, j).isFinite) then failures(first + j) = Some(VoxelFamilyError.Evidence("nonfinite voxel source"))
            j += 1
          val clean = DMat.tabulate(n, count)((i, j) => if failures(first + j).nonEmpty then 0.0 else values(i, j))
          val projected = whiten(clean).flatMap(value => basis.project(value, maximum).left.map(VoxelFamilyError.Provider.apply))
          projected match
            case Left(error) =>
              j = 0
              while j < count do
                failures(first + j) = Some(error); j += 1
            case Right(value) =>
              var i = 0
              while i < value.rows do
                j = 0
                while j < count do
                  out.update(i, first + j, value(i, j)); j += 1
                i += 1
      first += count
    val markers = failures.toVector.map(_.toLeft(()))
    Right((out.result(), markers))

  private[pattern] def evaluate(brain: DMat, target: DMat, factor: QR, df: Int,
      permutation: Permutation, batch: Int,
      source: Vector[Either[VoxelFamilyError, Unit]]
  ): Vector[Either[VoxelFamilyError, VoxelOmnibusEstimate]] =
    val out = Vector.newBuilder[Either[VoxelFamilyError, VoxelOmnibusEstimate]]
    val order = permutation.toIArray
    var first = 0
    while first < brain.cols do
      val count = math.min(batch, brain.cols - first)
      val block = DMat.tabulate(brain.rows, count)((i, j) => brain(order(i), first + j))
      factor.solveLeastSquares(block) match
        case Left(error) =>
          var j = 0
          while j < count do
            out += Left(VoxelFamilyError.Numerical(error.toString)); j += 1
        case Right(beta) =>
          val fitted = target * beta; val residual = block - fitted
          var j = 0
          while j < count do
            source(first + j) match
              case Left(error) => out += Left(error)
              case Right(_) =>
                var rss = 0.0; var explained = 0.0; var i = 0
                while i < brain.rows do
                  rss += residual(i, j) * residual(i, j)
                  explained += fitted(i, j) * fitted(i, j); i += 1
                val variance = rss / df
                val statistic = explained / (target.cols * variance)
                if !variance.isFinite || variance <= 0.0 || !statistic.isFinite || statistic < 0.0 then
                  out += Left(VoxelFamilyError.Numerical("degenerate residual or nonfinite omnibus F"))
                else
                  val coefficients = Vector.tabulate(beta.rows)(i => beta(i, j))
                  if coefficients.exists(value => !value.isFinite) then out += Left(VoxelFamilyError.Numerical("nonfinite voxel coefficient"))
                  else out += Right(VoxelOmnibusEstimate(coefficients, math.sqrt(variance), statistic))
            j += 1
      first += count
    out.result()

  private[pattern] def fieldDigest(field: Vector[Either[VoxelFamilyError, Double]]): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.voxel-family-field.v1"); writer.intLE(field.size)
      field.foreach:
        case Right(value) => writer.string("statistic"); writer.string(java.lang.Double.toHexString(value))
        case Left(error) => writer.string("failure"); writer.string(error.toString)
