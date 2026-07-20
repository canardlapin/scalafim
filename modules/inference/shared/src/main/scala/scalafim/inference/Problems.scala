package scalafim.inference

import scalafim.multivar.CcaFit
import scalafim.multivar.GenPcaFit
import scalafim.multivar.PcaFit
import scalafim.multivar.PlscFit
import scalafim.multivar.ReducedRankRegressionFit

trait FitDescriptor[F]:
  def label: String

object FitDescriptor:
  case object Pca extends FitDescriptor[PcaFit]:
    override val label: String = "pca"

  case object GenPca extends FitDescriptor[GenPcaFit]:
    override val label: String = "genpca"

  case object Plsc extends FitDescriptor[PlscFit]:
    override val label: String = "plsc"

  case object Cca extends FitDescriptor[CcaFit]:
    override val label: String = "cca"

  case object ReducedRankRegression extends FitDescriptor[ReducedRankRegressionFit]:
    override val label: String = "reduced-rank-regression"

  case object GeneralizedEigen extends FitDescriptor[GeneralizedEigenFit]:
    override val label: String = "generalized-eigen"

  case object CpcaBlock extends FitDescriptor[CpcaInferenceFit]:
    override val label: String = "cpca-block"

  case object MultiblockConsensus extends FitDescriptor[MultiblockInferenceFit]:
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
