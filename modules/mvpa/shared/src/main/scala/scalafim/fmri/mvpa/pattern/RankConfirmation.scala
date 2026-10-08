package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.SemanticSpace
import multivar.inference.{Alpha, CanonicalRankMethod, CanonicalRankResult, CanonicalRankSampling, CanonicalRankSpectrum, CanonicalResidualBasis, CanonicalResidualMethod, FixedCanonicalRank, GaussianCanonicalRank, InferenceError, MonteCarloDraws, PermutationAction, RowCount, StepwiseCanonicalRank}
import resample4s.kernel.Seed
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, EvidenceIdentity, MultiResponse, Observations}
import scalafim.fmri.mvpa.analysis.{EvidenceExposure, ExposureControl, ExposureScope}

enum RankConfirmationError:
  case Invalid(field: String)
  case AxisMismatch(field: String)
  case Exposure(detail: String)
  case Unavailable(detail: String)
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case Evidence(detail: String)
  case Numerical(cause: InferenceError)

/** The two candidate spaces need not have the same dimension. The numeric
  * projections and exposure snapshot are frozen against actual discovery.
  * This binds caller declarations; it cannot authenticate their physical origin. */
final class RankFrozenSubspaces private (
    val discoveryIdentity: String, val brain: FrozenProjection[?, ?], val target: FrozenProjection[?, ?],
    val receipt: String, val identity: String
)
object RankFrozenSubspaces:
  def freeze(discovery: DiscoverySnapshot[?, ?], brain: FrozenProjection[?, ?], target: FrozenProjection[?, ?],
      exposure: EvidenceExposure, receipt: String, maximumOwnedCells: Long = 10000000L): Either[RankConfirmationError, RankFrozenSubspaces] =
    val cells = BigInt(12) * (BigInt(brain.matrix.rows) * brain.matrix.cols + BigInt(target.matrix.rows) * target.matrix.cols) + BigInt(24) * (BigInt(brain.matrix.cols) * brain.matrix.cols + BigInt(target.matrix.cols) * target.matrix.cols)
    if receipt.trim.isEmpty then Left(RankConfirmationError.Invalid("candidate freeze receipt"))
    else if brain.matrix.cols <= 0 || target.matrix.cols <= 0 then Left(RankConfirmationError.Invalid("positive candidate dimensions"))
    else if maximumOwnedCells < 0L || cells > maximumOwnedCells then Left(RankConfirmationError.Budget(cells, maximumOwnedCells))
    else if brain.input.descriptor != discovery.artifact.factors.neuralAxis.descriptor || target.input.descriptor != discovery.artifact.factors.targetAxis.descriptor then Left(RankConfirmationError.AxisMismatch("discovery candidate endpoints"))
    else if exposure.reference.evidenceIdentity != discovery.identity || exposure.initialScope != ExposureScope.Training || ExposureControl.untouchedConfirmation(exposure, ExposureScope.Holdout).isLeft then Left(RankConfirmationError.Exposure("candidate spaces require discovery-bound training exposure without possible holdout access"))
    else if !TargetGeometry.fullColumnRank(brain.matrix, 1e-12) || !TargetGeometry.fullColumnRank(target.matrix, 1e-12) then Left(RankConfirmationError.Invalid("candidate projections must have full column rank"))
    else
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.rank-frozen-subspaces.v1"); writer.string(discovery.identity)
        writer.string(brain.identity); writer.string(target.identity); writer.string(exposure.identity.text); writer.string(receipt)
      Right(new RankFrozenSubspaces(discovery.identity, brain, target, receipt, identity))

/** A separate caller declaration that both projected blocks have a joint
  * Gaussian error law over independent confirmation rows. Dependence cannot
  * be licensed by a voxel-only covariance declaration. */
enum RankExchangeabilityAssumption:
  case SphericalJointGaussianRows

final class RankJointGaussian private (
    val designIdentity: String, val receipt: String, val identity: String
):
  val assumption: RankExchangeabilityAssumption = RankExchangeabilityAssumption.SphericalJointGaussianRows
object RankJointGaussian:
  def declare(design: ConfirmationDesign[?, ?], receipt: String): Either[RankConfirmationError, RankJointGaussian] =
    if receipt.trim.isEmpty then Left(RankConfirmationError.Invalid("joint Gaussian common-row-shape receipt"))
    else if design.errorLaw != ConfirmationErrorLaw.IndependentGaussian then Left(RankConfirmationError.Unavailable("dependent time or repeated-unit rank confirmation requires a separately admitted compatible block action; known voxel covariance is insufficient"))
    else
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.rank-joint-gaussian.v1"); writer.string(design.identity); writer.string(receipt)
      Right(new RankJointGaussian(design.identity, receipt, identity))

enum RankCalibrationStatus:
  case PendingFrozenProtocol

final case class RankConfirmationBudget(maximumOwnedCells: Long = 10000000L, maximumNullValues: Long = 1000000L, maximumTransformCandidates: Int = 100000):
  require(maximumOwnedCells >= 0L && maximumNullValues >= 0L && maximumTransformCandidates >= 0)

/** Candidate arithmetic is retained for calibration. No significance or
  * population-rank claim is admitted until the frozen protocol qualifies it.
  * Detectable rank concerns these fixed candidate spaces, never an upper bound
  * on the association rank in the original feature spaces. */
final class RankConfirmationResult private[pattern] (
    val candidateArithmetic: CanonicalRankResult,
    val brainCandidateAxis: AxisDescriptor, val targetCandidateAxis: AxisDescriptor,
    val confirmationRows: AxisDescriptor, val independentUnits: AxisDescriptor,
    val rowUnitOrdinals: Vector[Int], val residualRows: Int, val nuisanceRank: Int,
    val designIdentity: String, val candidateIdentity: String, val jointLawIdentity: String,
    val brainEvidenceIdentity: EvidenceIdentity, val targetEvidenceIdentity: EvidenceIdentity,
    val plannedOwnedCells: Long, val residualBasis: CanonicalResidualBasis,
    val nuisanceWorkingDesign: DMat, val residualBasisIdentity: String, val jointLaw: RankJointGaussian
):
  val method: CanonicalRankMethod = candidateArithmetic.method
  val calibrationStatus: RankCalibrationStatus = RankCalibrationStatus.PendingFrozenProtocol
  def admittedDetectableRank: Either[RankConfirmationError, Int] =
    Left(RankConfirmationError.Unavailable("rank calibration under the frozen protocol is pending"))

object RankConfirmation:
  /** One projection application per source, without materializing the full
    * brain matrix. The residual design includes the intercept and all nuisance columns.
    * Gaussian Huh-Jhun coordinates preserve iid zero-mean joint Gaussian rows.
    * The default interlacing reference has a conservative Gaussian argument;
    * the score-completed permutation comparator still needs partial-null qualification.
    * Owned numeric storage excludes borrowed evidence, private provider/Gale
    * scratch, and receipt collection overhead. */
  def run[S <: SemanticSpace, N <: SemanticSpace, Q <: SemanticSpace, U](
      design: ConfirmationDesign[?, U], candidates: RankFrozenSubspaces, jointLaw: RankJointGaussian,
      observations: Observations[S, N], targets: MultiResponse[S, Q],
      seed: Seed, draws: MonteCarloDraws, alpha: Alpha, budget: RankConfirmationBudget = RankConfirmationBudget(),
      method: CanonicalRankMethod = CanonicalRankMethod.GaussianInterlacingWilksV1
  ): Either[RankConfirmationError, RankConfirmationResult] =
    val rows = design.confirmation.samples.rows.descriptor
    val n = BigInt(observations.rows); val p = BigInt(candidates.brain.matrix.cols); val q = BigInt(candidates.target.matrix.cols)
    val z = BigInt(design.nuisance.matrix.cols) + 1; val k = p.min(q)
    val retained = k * (p + q) - k * (k - 1)
    val transformElements = method match
      case CanonicalRankMethod.ScoreOrthogonalPermutationV2 => BigInt(draws.value) * n
      case CanonicalRankMethod.GaussianInterlacingWilksV1 => BigInt(0)
    val cells = 16 * n * n + 24 * n * (p + q + z) + 4 * n * retained + 48 * (p * p + q * q + p * q + z * z) + transformElements + 4 * BigInt(draws.value) * k
    val nullValues = BigInt(draws.value) * k
    if observations.sampleAxis != rows || targets.sampleAxis != rows then Left(RankConfirmationError.AxisMismatch("actual confirmation rows"))
    else if observations.neuralAxis != candidates.brain.input.descriptor || targets.featureAxis != candidates.target.input.descriptor then Left(RankConfirmationError.AxisMismatch("actual frozen candidate endpoints"))
    else if candidates.discoveryIdentity != design.discovery.identity || jointLaw.designIdentity != design.identity then Left(RankConfirmationError.AxisMismatch("candidate discovery or joint-law design binding"))
    else if method == CanonicalRankMethod.ScoreOrthogonalPermutationV2 && budget.maximumTransformCandidates < draws.value then Left(RankConfirmationError.Numerical(InferenceError.CanonicalDrawBudgetExhausted(0, 0, draws.value)))
    else if cells > budget.maximumOwnedCells || nullValues > budget.maximumNullValues || nullValues > Int.MaxValue || Vector(n * n, n * p, n * q, n * z, p * p, q * q, z * z).exists(_ > Int.MaxValue) then Left(RankConfirmationError.Budget(cells.max(nullValues), if nullValues > budget.maximumNullValues then budget.maximumNullValues else budget.maximumOwnedCells))
    else
      val augmented = DMat.tabulate(observations.rows, z.toInt)((i, j) => if j == 0 then 1.0 else design.nuisance.matrix(i, j - 1))
      for
        basis <- CanonicalResidualBasis.from(augmented, CanonicalResidualMethod.HuhJhun, budget.maximumOwnedCells).left.map(RankConfirmationError.Numerical.apply)
        _ <- if basis.matrix.cols.toLong > p.toLong + q.toLong then Right(()) else Left(RankConfirmationError.Unavailable("insufficient nuisance-residual rows for the candidate spaces"))
        y <- targets.targets(candidates.target.matrix).left.map(error => RankConfirmationError.Evidence(error.toString))
        ry <- basis.project(y, budget.maximumOwnedCells).left.map(RankConfirmationError.Numerical.apply)
        x <- observations.patterns(candidates.brain.matrix).left.map(error => RankConfirmationError.Evidence(error.toString))
        rx <- basis.project(x, budget.maximumOwnedCells).left.map(RankConfirmationError.Numerical.apply)
        result <- (method match
          case CanonicalRankMethod.GaussianInterlacingWilksV1 =>
            CanonicalRankSpectrum.from(rx, ry, budget.maximumOwnedCells).flatMap: problem =>
              GaussianCanonicalRank.run(problem, seed, draws, alpha, budget.maximumNullValues, budget.maximumOwnedCells)
          case CanonicalRankMethod.ScoreOrthogonalPermutationV2 =>
            for
              problem <- StepwiseCanonicalRank.from(rx, ry, budget.maximumOwnedCells)
              count <- RowCount(basis.matrix.cols)
              result <- FixedCanonicalRank.run(problem, PermutationAction.unrestricted(count), seed, draws, alpha, budget.maximumNullValues,
                CanonicalRankSampling.DistinctNonIdentity(budget.maximumTransformCandidates), budget.maximumOwnedCells)
            yield result
        ).left.map(RankConfirmationError.Numerical.apply)
        basisIdentity = AxisDigest.sha256Hex: writer =>
          writer.string("scalafim.rank-residual-basis.v1"); writer.string(design.identity)
          writer.string(basis.method.toString); writer.string(java.lang.Double.toHexString(basis.qrRankTolerance)); writer.string(java.lang.Double.toHexString(basis.lawTolerance))
          writer.intLE(basis.matrix.rows); writer.intLE(basis.matrix.cols)
          var row = 0
          while row < basis.matrix.rows do
            var column = 0
            while column < basis.matrix.cols do
              val bits = java.lang.Double.doubleToRawLongBits(basis.matrix(row, column))
              writer.intLE(bits.toInt); writer.intLE((bits >>> 32).toInt)
              column += 1
            row += 1
      yield new RankConfirmationResult(result, candidates.brain.output.descriptor, candidates.target.output.descriptor,
        rows, design.confirmation.samples.units.descriptor, design.confirmation.samples.rowUnitOrdinals, basis.matrix.cols, basis.nuisanceRank,
        design.identity, candidates.identity, jointLaw.identity, observations.identity, targets.identity, cells.toLong, basis, augmented, basisIdentity, jointLaw)
