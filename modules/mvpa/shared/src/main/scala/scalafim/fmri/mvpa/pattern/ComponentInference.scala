package scalafim.fmri.mvpa.pattern

import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.core.SemanticSpace
import multivar.inference.{Alpha, CanonicalRankSampling, CanonicalResidualBasis, CanonicalResidualMethod, FixedCanonicalRank, FixedCanonicalRankResult, InferenceError, MonteCarloDraws, PermutationAction, RowCount}
import multivar.inference.StepwiseCanonicalRank
import resample4s.kernel.Seed
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, EvidenceIdentity, MultiResponse, Observations}
import scalafim.fmri.mvpa.analysis.{EvidenceExposure, ExposureControl, ExposureScope}

enum ComponentInferenceError:
  case Invalid(detail: String)
  case AxisMismatch(detail: String)
  case SourceBinding(detail: String)
  case Exposure(detail: String)
  case Unavailable(detail: String)
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case Arithmetic(cause: ComponentConfirmationError)
  case Provider(cause: InferenceError)
  case Covariance(detail: String)
  case NonEstimable(detail: String)
  case Numerical(detail: String)

/** Independent numerical qualification does not establish M4.09 release. */
enum ComponentInferenceRelease:
  case PendingFrozenProtocol

final case class ComponentInferenceBudget(maximumOwnedCells: Long = 10000000L,
    maximumNullValues: Long = 1000000L, maximumTransformCandidates: Int = 100000):
  require(maximumOwnedCells >= 0L && maximumNullValues >= 0L && maximumTransformCandidates >= 0)

/** This is a caller-declared joint score law, not a consequence of a voxel
  * covariance. Huh-Jhun coordinates permit unrestricted Gaussian row actions. */
final class FrozenComponentAssociationReference private (
    val plan: FrozenComponentConfirmation, val jointLaw: RankJointGaussian,
    val brainSource: EvidenceIdentity, val targetSource: EvidenceIdentity,
    val seed: Seed, val draws: MonteCarloDraws, val budget: ComponentInferenceBudget,
    val exposureIdentity: String, val identity: String
)
object FrozenComponentAssociationReference:
  def freeze(plan: FrozenComponentConfirmation, jointLaw: RankJointGaussian,
      brainSource: EvidenceIdentity, targetSource: EvidenceIdentity, exposure: EvidenceExposure,
      seed: Seed, draws: MonteCarloDraws, budget: ComponentInferenceBudget = ComponentInferenceBudget()
  ): Either[ComponentInferenceError, FrozenComponentAssociationReference] =
    for
      _ <- ComponentInference.untouched(plan, exposure)
      _ <- ComponentInference.sourceAxes(plan, brainSource, targetSource)
      _ <- if plan.design.errorLaw == ConfirmationErrorLaw.IndependentGaussian && jointLaw.designIdentity == plan.design.identity then Right(())
        else Left(ComponentInferenceError.Unavailable("association requires design-bound spherical joint Gaussian independent rows"))
      _ <- if draws.value <= budget.maximumTransformCandidates then Right(())
        else Left(ComponentInferenceError.Budget(BigInt(draws.value), budget.maximumTransformCandidates.toLong))
    yield
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.component-association-reference.v1"); writer.string(plan.identity)
        writer.string(jointLaw.identity); brainSource.writeFramed(writer); targetSource.writeFramed(writer)
        writer.string(exposure.identity.text); writer.string(seed.value.toString); writer.intLE(draws.value)
        writer.string("paired-rank-one; Huh-Jhun; common-distinct-nonidentity-actions; inclusive-plus-one; pointwise-only")
        writer.string(budget.toString)
      new FrozenComponentAssociationReference(plan, jointLaw, brainSource, targetSource, seed, draws, budget, exposure.identity.text, identity)

enum ComponentOutcomeCovarianceStatus:
  case KnownConditionalGaussian
  case Estimated
  case Unknown

enum ComponentOutcomeConditioning:
  /** Discovery, all fitted heads and actual confirmation predictors are fixed. */
  case DiscoveryHeadsAndConfirmationPredictors
  case MarginalOutcomes

/** Covariance of row-major vec(Y), including cross-target and within-unit
  * dependence. Independent units must have exactly zero cross-unit covariance.
  * Known is a scientific caller declaration, not authenticated source origin.
  * Admission here bounds the owned covariance copy; borrowed input, private
  * Gale Cholesky scratch and object overhead are outside this bound. */
final class FrozenComponentPredictionReference private (
    val heads: ComponentPredictionHeads, val rows: AxisDescriptor, val targets: AxisDescriptor,
    val brainSource: EvidenceIdentity, val targetSource: EvidenceIdentity,
    val covariance: DMat, val covarianceReceipt: String, val exposureIdentity: String,
    val plannedOwnedCells: Long, val identity: String
):
  val conditioning: ComponentOutcomeConditioning = ComponentOutcomeConditioning.DiscoveryHeadsAndConfirmationPredictors
  val alpha: Double = 0.05
  val intervalCoverage: Double = 0.95
  val nullHypothesis: String = "conditional expected reduced-minus-full loss <= 0"
  val alternative: String = "conditional expected reduced-minus-full loss > 0"

object FrozenComponentPredictionReference:
  def freeze(heads: ComponentPredictionHeads, rows: AxisDescriptor, targets: AxisDescriptor,
      brainSource: EvidenceIdentity, targetSource: EvidenceIdentity, covariance: DMat,
      status: ComponentOutcomeCovarianceStatus, conditioning: ComponentOutcomeConditioning,
      covarianceReceipt: String, exposure: EvidenceExposure,
      maximumOwnedCells: Long = 10000000L
  ): Either[ComponentInferenceError, FrozenComponentPredictionReference] =
    val plan = heads.plan
    val n = plan.design.confirmation.samples.rows.size
    val q = plan.targetMetric.size
    val d = BigInt(n) * q
    val cells = d * d
    for
      _ <- ComponentInference.untouched(plan, exposure)
      _ <- ComponentInference.sourceAxes(plan, brainSource, targetSource)
      _ <- if status == ComponentOutcomeCovarianceStatus.KnownConditionalGaussian && conditioning == ComponentOutcomeConditioning.DiscoveryHeadsAndConfirmationPredictors then Right(())
        else Left(ComponentInferenceError.Unavailable("known Gaussian response covariance conditional on discovery, heads and confirmation predictors is required"))
      _ <- if covarianceReceipt.trim.nonEmpty then Right(()) else Left(ComponentInferenceError.Invalid("known covariance source receipt"))
      _ <- if rows == plan.design.confirmation.samples.rows.descriptor && targets == plan.design.discovery.targetProjection.input.descriptor then Right(())
        else Left(ComponentInferenceError.AxisMismatch("known covariance row and target order"))
      _ <- if maximumOwnedCells >= 0L && cells <= maximumOwnedCells && cells <= Int.MaxValue && d <= Int.MaxValue then Right(())
        else Left(ComponentInferenceError.Budget(cells, maximumOwnedCells))
      _ <- if covariance.rows == d.toInt && covariance.cols == d.toInt then Right(())
        else Left(ComponentInferenceError.AxisMismatch("covariance must name row-major vec(Y), not only a diagonal or row covariance"))
      _ <- if ResidualCovariance.finite(covariance) then Right(()) else Left(ComponentInferenceError.Covariance("nonfinite response covariance"))
      _ <- validateBlocks(covariance, plan.design.confirmation.samples.rowUnitOrdinals, q)
      _ <- covariance.cholesky.left.map(error => ComponentInferenceError.Covariance(s"response covariance must be positive definite: $error")).map(_ => ())
      owned = DMat.tabulate(covariance.rows, covariance.cols)((i, j) => covariance(i, j))
    yield
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.component-prediction-reference.v1"); writer.string(plan.identity); writer.string(heads.identity)
        writer.string(rows.coordinateSignature.value); writer.string(targets.coordinateSignature.value)
        brainSource.writeFramed(writer); targetSource.writeFramed(writer)
        writer.string(covarianceReceipt); writer.string(exposure.identity.text)
        writer.string("row-major-Y; conditional-Gaussian; known-full-covariance; mean<=0-vs->0; alpha=.05; CI=.95; equal-units")
        writer.intLE(owned.rows)
        var i = 0
        while i < owned.rows do
          var j = 0
          while j < owned.cols do
            writer.string(java.lang.Double.toHexString(owned(i, j))); j += 1
          i += 1
      new FrozenComponentPredictionReference(heads, rows, targets, brainSource, targetSource, owned, covarianceReceipt,
        exposure.identity.text, cells.toLong, identity)

  private def validateBlocks(covariance: DMat, mapping: Vector[Int], q: Int): Either[ComponentInferenceError, Unit] =
    var i = 0
    while i < covariance.rows do
      var j = 0
      while j < covariance.cols do
        if math.abs(covariance(i, j) - covariance(j, i)) > 0.0 then
          return Left(ComponentInferenceError.Covariance("known covariance must be symmetric; no silent covariance repair"))
        if mapping(i / q) != mapping(j / q) && math.abs(covariance(i, j)) > 0.0 then
          return Left(ComponentInferenceError.Covariance("claimed independent units have nonzero cross-unit covariance"))
        j += 1
      i += 1
    Right(())

/** Actual pointwise permutation calculations, with no corrected-family or
  * released significance artifact. Provider receipts retain all sampled IDs. */
final class ComponentAssociationInference private[pattern] (
    val reference: FrozenComponentAssociationReference, val arithmetic: ComponentAssociationResult,
    val candidateCalculations: Vector[FixedCanonicalRankResult], val residualBasisIdentity: String,
    val residualRows: Int, val plannedOwnedCells: Long,
    val residualBasis: CanonicalResidualBasis, val preparedScores: DMat
):
  val release: ComponentInferenceRelease = ComponentInferenceRelease.PendingFrozenProtocol
  def admittedC1: Either[ComponentInferenceError, Nothing] = ComponentInference.releaseUnavailable
  def associationIntervals: Either[ComponentInferenceError, Nothing] =
    Left(ComponentInferenceError.Unavailable("association interval reference is not implemented by this permutation procedure"))

enum ComponentGaussianPointDecision:
  /** Candidate point null only; not a released or family-adjusted claim. */
  case AboveFixedFivePercentBoundary
  case NotAboveFixedFivePercentBoundary

final case class ComponentGaussianCalculation private[pattern] (
    meanImprovement: Double, knownVariance: Double, standardError: Double,
    z: Double, lower95: Double, upper95: Double, candidateDecision: ComponentGaussianPointDecision
):
  require(Vector(meanImprovement, knownVariance, standardError, z, lower95, upper95).forall(_.isFinite))
  require(knownVariance > 0.0 && standardError > 0.0 && lower95 <= upper95)

final class ComponentPredictionInference private[pattern] (
    val reference: FrozenComponentPredictionReference, val arithmetic: ComponentIncrementalResult,
    val knownMeanCovariance: DMat, val candidateCalculations: Vector[ComponentGaussianCalculation],
    val plannedOwnedCells: Long
):
  val release: ComponentInferenceRelease = ComponentInferenceRelease.PendingFrozenProtocol
  def pValues: Either[ComponentInferenceError, Nothing] =
    Left(ComponentInferenceError.Unavailable("this fixed .05/.95 reference provides no CDF or p-value calculation"))
  def admittedC1: Either[ComponentInferenceError, Nothing] = ComponentInference.releaseUnavailable

/** All r association and r predictive-value members are retained together.
  * Different null mechanisms do not create separate uncorrected family claims. */
final class ComponentInferenceFamily private[pattern] (
    val planIdentity: String, val members: Vector[String],
    val association: ComponentAssociationInference, val incremental: ComponentPredictionInference
):
  val release: ComponentInferenceRelease = ComponentInferenceRelease.PendingFrozenProtocol
  def correctedFamilyInference: Either[ComponentInferenceError, Nothing] =
    Left(ComponentInferenceError.Unavailable("complete-family multiplicity integration belongs to M4.08"))
  def admittedC1: Either[ComponentInferenceError, Nothing] = ComponentInference.releaseUnavailable

object ComponentInference:
  // Frozen domain protocol policy, independently qualified with R qnorm.
  // These constants are not a generic probability approximation or quantile API.
  val GaussianOneSidedFivePercentCritical: Double = 1.6448536269514722
  val GaussianTwoSided95Critical: Double = 1.959963984540054

  private[pattern] def releaseUnavailable: Either[ComponentInferenceError, Nothing] =
    Left(ComponentInferenceError.Unavailable("released C1 inference requires M4.08 and frozen M4.09 scientific qualification"))

  private[pattern] def untouched(plan: FrozenComponentConfirmation, exposure: EvidenceExposure): Either[ComponentInferenceError, Unit] =
    if exposure.reference.evidenceIdentity != plan.design.confirmation.identity then Left(ComponentInferenceError.Exposure("reference does not name actual confirmation"))
    else ExposureControl.untouchedConfirmation(exposure, ExposureScope.Holdout).left.map(error => ComponentInferenceError.Exposure(error.toString))

  def association[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace](reference: FrozenComponentAssociationReference,
      observations: Observations[S, N], targets: MultiResponse[S, Q]
  ): Either[ComponentInferenceError, ComponentAssociationInference] =
    val plan = reference.plan; val n = observations.rows; val r = plan.associationMembers.size
    val z = plan.design.nuisance.matrix.cols.toLong + 1L
    val cells = ComponentConfirmation.associationOwnedCells(plan, n) +
      32 * BigInt(n) * n + 48 * BigInt(n) * (z + 2L * r) + 96 * BigInt(z) * z + 160 * BigInt(r) +
      BigInt(reference.draws.value) * n + BigInt(reference.draws.value) * r
    val nulls = BigInt(reference.draws.value) * r
    val budget = reference.budget
    for
      _ <- sources(reference.brainSource, reference.targetSource, observations.identity, targets.identity)
      _ <- admit(cells, budget.maximumOwnedCells)
      _ <- if nulls <= budget.maximumNullValues then Right(()) else Left(ComponentInferenceError.Budget(nulls, budget.maximumNullValues))
      arithmetic <- ComponentConfirmation.association(plan, observations, targets,
        ComponentConfirmationBudget(budget.maximumOwnedCells)).left.map(ComponentInferenceError.Arithmetic.apply)
      basis <- CanonicalResidualBasis.from(arithmetic.nuisanceWorkingDesign, CanonicalResidualMethod.HuhJhun, budget.maximumOwnedCells).left.map(ComponentInferenceError.Provider.apply)
      projected <- basis.project(arithmetic.residualScores, budget.maximumOwnedCells).left.map(ComponentInferenceError.Provider.apply)
      count <- RowCount(basis.matrix.cols).left.map(ComponentInferenceError.Provider.apply)
      alpha <- Alpha(0.05).left.map(ComponentInferenceError.Provider.apply)
      calculations <-
        val out = Vector.newBuilder[FixedCanonicalRankResult]
        var k = 0; var failed: Option[ComponentInferenceError] = None
        while k < r && failed.isEmpty do
          val left = DMat.tabulate(projected.rows, 1)((i, _) => projected(i, k))
          val right = DMat.tabulate(projected.rows, 1)((i, _) => projected(i, r + k))
          val calculation = for
            problem <- StepwiseCanonicalRank.from(left, right, budget.maximumOwnedCells).left.map(ComponentInferenceError.Provider.apply)
            value <- FixedCanonicalRank.run(problem, PermutationAction.unrestricted(count), reference.seed, reference.draws, alpha,
              budget.maximumNullValues, CanonicalRankSampling.DistinctNonIdentity(budget.maximumTransformCandidates),
              budget.maximumOwnedCells).left.map(ComponentInferenceError.Provider.apply)
          yield value
          calculation match
            case Left(error) => failed = Some(error)
            case Right(value) => out += value
          k += 1
        failed.toLeft(out.result())
      _ <- if calculations.map(_.sampledCandidateIds).distinct.size == 1 then Right(())
        else Left(ComponentInferenceError.Numerical("association family did not use common sampled actions"))
    yield
      val basisIdentity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.component-Huh-Jhun.v1"); writer.string(reference.identity)
        writer.intLE(basis.nuisanceRank); writer.intLE(basis.matrix.rows); writer.intLE(basis.matrix.cols)
        var i = 0
        while i < basis.matrix.rows do
          var j = 0
          while j < basis.matrix.cols do
            writer.string(java.lang.Double.toHexString(basis.matrix(i, j))); j += 1
          i += 1
      new ComponentAssociationInference(reference, arithmetic, calculations, basisIdentity, basis.matrix.cols, cells.toLong,
        basis, projected)

  def incremental[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace](reference: FrozenComponentPredictionReference,
      observations: Observations[S, N], targets: MultiResponse[S, Q], budget: ComponentInferenceBudget = ComponentInferenceBudget()
  ): Either[ComponentInferenceError, ComponentPredictionInference] =
    val r = reference.heads.reduced.size
    val d = BigInt(observations.rows) * targets.columns
    val cells = ComponentConfirmation.incrementalOwnedCells(reference.heads, observations.rows, targets.columns) +
      reference.plannedOwnedCells + 8 * d * r + 16 * BigInt(r) * r + 16 * BigInt(r)
    for
      _ <- sources(reference.brainSource, reference.targetSource, observations.identity, targets.identity)
      _ <- if observations.sampleAxis == reference.rows && targets.featureAxis == reference.targets then Right(()) else Left(ComponentInferenceError.AxisMismatch("actual covariance endpoints"))
      _ <- admit(cells, budget.maximumOwnedCells)
      arithmetic <- ComponentConfirmation.incremental(reference.heads, observations, targets,
        ComponentConfirmationBudget(budget.maximumOwnedCells)).left.map(ComponentInferenceError.Arithmetic.apply)
      _ <- if arithmetic.headIdentity == reference.heads.identity && arithmetic.planIdentity == reference.heads.plan.identity then Right(()) else Left(ComponentInferenceError.SourceBinding("frozen prediction head or plan changed"))
      known = arithmetic.lossContrasts.t * reference.covariance * arithmetic.lossContrasts
      _ <- if ResidualCovariance.finite(known) then Right(()) else Left(ComponentInferenceError.Numerical("nonfinite known mean covariance"))
      calculations <-
        val out = Vector.newBuilder[ComponentGaussianCalculation]
        var k = 0; var failed: Option[ComponentInferenceError] = None
        while k < r && failed.isEmpty do
          gaussian(arithmetic.meanImprovements(k), known(k, k)) match
            case Left(error) => failed = Some(error)
            case Right(value) => out += value
          k += 1
        failed.toLeft(out.result())
    yield new ComponentPredictionInference(reference, arithmetic, known, calculations, cells.toLong)

  def complete(plan: FrozenComponentConfirmation, association: ComponentAssociationInference,
      incremental: ComponentPredictionInference): Either[ComponentInferenceError, ComponentInferenceFamily] =
    if association.reference.plan.identity != plan.identity || incremental.reference.heads.plan.identity != plan.identity then
      Left(ComponentInferenceError.SourceBinding("component family plan identities"))
    else if association.arithmetic.brainEvidenceIdentity != incremental.arithmetic.brainEvidenceIdentity ||
        association.arithmetic.targetEvidenceIdentity != incremental.arithmetic.targetEvidenceIdentity then
      Left(ComponentInferenceError.SourceBinding("association and incremental outcomes must name the same confirmation sources"))
    else if association.arithmetic.members ++ incremental.arithmetic.members != plan.design.contract.multiplicityFamily then
      Left(ComponentInferenceError.Invalid("complete ordered association and incremental family required"))
    else Right(new ComponentInferenceFamily(plan.identity, plan.design.contract.multiplicityFamily, association, incremental))

  private[pattern] def gaussian(mean: Double, variance: Double): Either[ComponentInferenceError, ComponentGaussianCalculation] =
    if !mean.isFinite || !variance.isFinite || variance <= 0.0 then Left(ComponentInferenceError.NonEstimable("finite mean and strictly positive known contrast variance required; deterministic zero variance is unavailable"))
    else
      val se = math.sqrt(variance); val z = mean / se
      val low = mean - GaussianTwoSided95Critical * se; val high = mean + GaussianTwoSided95Critical * se
      if !Vector(se, z, low, high).forall(_.isFinite) then Left(ComponentInferenceError.Numerical("nonfinite fixed Gaussian calculation"))
      else Right(ComponentGaussianCalculation(mean, variance, se, z, low, high,
        if z > GaussianOneSidedFivePercentCritical then ComponentGaussianPointDecision.AboveFixedFivePercentBoundary
        else ComponentGaussianPointDecision.NotAboveFixedFivePercentBoundary))

  private def sources(expectedBrain: EvidenceIdentity, expectedTarget: EvidenceIdentity,
      actualBrain: EvidenceIdentity, actualTarget: EvidenceIdentity): Either[ComponentInferenceError, Unit] =
    if expectedBrain == actualBrain && expectedTarget == actualTarget then Right(())
    else Left(ComponentInferenceError.SourceBinding("actual evidence source identities differ from the frozen reference"))

  private[pattern] def sourceAxes(plan: FrozenComponentConfirmation, brain: EvidenceIdentity,
      target: EvidenceIdentity): Either[ComponentInferenceError, Unit] =
    val rows = plan.design.confirmation.samples.rows.descriptor
    if brain.rows == rows && target.rows == rows && brain.columns == plan.design.discovery.brainProjection.input.descriptor &&
        target.columns == plan.design.discovery.targetProjection.input.descriptor then Right(())
    else Left(ComponentInferenceError.AxisMismatch("frozen evidence identity row and brain/target endpoints"))

  private def admit(cells: BigInt, maximum: Long): Either[ComponentInferenceError, Unit] =
    if maximum < 0L || cells > maximum || cells > Int.MaxValue then Left(ComponentInferenceError.Budget(cells, maximum)) else Right(())
