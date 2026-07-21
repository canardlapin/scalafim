package scalafim.inference

sealed trait OperatorFitFamily
sealed trait PcaFitFamily extends OperatorFitFamily
sealed trait GpcaFitFamily extends OperatorFitFamily
sealed trait PlscFitFamily extends OperatorFitFamily
sealed trait CcaFitFamily extends OperatorFitFamily
sealed trait ReducedRankRegressionFitFamily extends OperatorFitFamily
sealed trait GeneralizedEigenFitFamily extends OperatorFitFamily
sealed trait CpcaBlockFitFamily extends OperatorFitFamily
sealed trait MultiblockConsensusFitFamily extends OperatorFitFamily

trait FitDescriptor[F]:
  def label: String

object FitDescriptor:
  case object Pca extends FitDescriptor[PcaFitFamily]:
    override val label: String = "pca"

  case object GenPca extends FitDescriptor[GpcaFitFamily]:
    override val label: String = "genpca"

  case object Plsc extends FitDescriptor[PlscFitFamily]:
    override val label: String = "plsc"

  case object Cca extends FitDescriptor[CcaFitFamily]:
    override val label: String = "cca"

  case object ReducedRankRegression extends FitDescriptor[ReducedRankRegressionFitFamily]:
    override val label: String = "reduced-rank-regression"

  case object GeneralizedEigen extends FitDescriptor[GeneralizedEigenFitFamily]:
    override val label: String = "generalized-eigen"

  case object CpcaBlock extends FitDescriptor[CpcaBlockFitFamily]:
    override val label: String = "cpca-block"

  case object MultiblockConsensus extends FitDescriptor[MultiblockConsensusFitFamily]:
    override val label: String = "multiblock-consensus"

enum SequentialBoundary:
  case FromAlpha
  case Explicit(value: ExceedanceBoundary)

enum MonteCarloPolicy:
  case Fixed(draws: MonteCarloDraws)
  case Sequential(
      maxDraws: MonteCarloDraws,
      alpha: Alpha,
      batchSize: BatchSize,
      boundary: SequentialBoundary
  )

enum RequestedEvidence:
  case SignificanceOnly
  case StabilityOnly
  case SignificanceAndStability

final case class InferenceSpec[
    F,
    K <: TargetKind,
    N <: NullKind,
    D <: DesignKind
] private (
    fit: FitDescriptor[F],
    target: TargetSpec[K],
    nullHypothesis: NullSpec[N],
    design: ResamplingDesign[D],
    units: UnitPolicy,
    monteCarlo: MonteCarloPolicy,
    evidence: RequestedEvidence,
    seed: RootSeed
)

object InferenceSpec:
  def of[F, K <: TargetKind, N <: NullKind, D <: DesignKind](
      fit: FitDescriptor[F],
      target: TargetSpec[K],
      nullHypothesis: NullSpec[N],
      design: ResamplingDesign[D],
      units: UnitPolicy,
      monteCarlo: MonteCarloPolicy,
      evidence: RequestedEvidence,
      seed: RootSeed
  ): InferenceSpec[F, K, N, D] =
    InferenceSpec(fit, target, nullHypothesis, design, units, monteCarlo, evidence, seed)

final case class ProblemSummary(
    fit: String,
    target: TargetLabel,
    nullHypothesis: NullLabel,
    rows: RowCount,
    unitPolicy: String
)
