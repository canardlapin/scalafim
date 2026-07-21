package scalafim.inference

trait SupportsTarget[F, K <: TargetKind]

object SupportsTarget:
  given SupportsTarget[PcaFitFamily, TargetKind.VarianceRoots] with {}
  given SupportsTarget[GpcaFitFamily, TargetKind.VarianceRoots] with {}
  given SupportsTarget[PlscFitFamily, TargetKind.CovarianceRoots] with {}
  given SupportsTarget[CcaFitFamily, TargetKind.CanonicalCorrelations] with {}
  given SupportsTarget[ReducedRankRegressionFitFamily, TargetKind.PredictiveGain] with {}
  given SupportsTarget[GeneralizedEigenFitFamily, TargetKind.GeneralizedEigenRoots] with {}
  given SupportsTarget[CpcaBlockFitFamily, TargetKind.ConstrainedInertiaRoots] with {}
  given SupportsTarget[MultiblockConsensusFitFamily, TargetKind.MultiblockConsensusRoots] with {}

trait SupportsNull[K <: TargetKind, N <: NullKind]:
  def validity: ValidityClaim

object SupportsNull:
  given SupportsNull[TargetKind.VarianceRoots, NullKind.RowPermutation] with
    override val validity: ValidityClaim = ValidityClaim.Exact

  given SupportsNull[TargetKind.CovarianceRoots, NullKind.PairedIndependence] with
    override val validity: ValidityClaim = ValidityClaim.Exact

  given SupportsNull[TargetKind.CanonicalCorrelations, NullKind.PairedIndependence] with
    override val validity: ValidityClaim = ValidityClaim.Exact

  given SupportsNull[TargetKind.PredictiveGain, NullKind.ResponsePermutation] with
    override val validity: ValidityClaim = ValidityClaim.Conditional

  given SupportsNull[TargetKind.GeneralizedEigenRoots, NullKind.RowPermutation] with
    override val validity: ValidityClaim = ValidityClaim.Exact

  given SupportsNull[TargetKind.ConstrainedInertiaRoots, NullKind.RowPermutation] with
    override val validity: ValidityClaim = ValidityClaim.Conditional

  given SupportsNull[TargetKind.MultiblockConsensusRoots, NullKind.BlockIndependence] with
    override val validity: ValidityClaim = ValidityClaim.Exact

trait SupportsDesign[N <: NullKind, D <: DesignKind]:
  def validity(base: ValidityClaim): ValidityClaim = base

object SupportsDesign:
  given SupportsDesign[NullKind.RowPermutation, DesignKind.ExchangeableRows] with {}
  given SupportsDesign[NullKind.PairedIndependence, DesignKind.ExchangeableRows] with {}
  given SupportsDesign[NullKind.ResponsePermutation, DesignKind.ExchangeableRows] with {}
  given SupportsDesign[NullKind.BlockIndependence, DesignKind.ExchangeableRows] with {}

  given SupportsDesign[NullKind.RowPermutation, DesignKind.WithinBlockRows] with {}
  given SupportsDesign[NullKind.PairedIndependence, DesignKind.WithinBlockRows] with {}
  given SupportsDesign[NullKind.ResponsePermutation, DesignKind.WithinBlockRows] with {}

  given SupportsDesign[NullKind.RowPermutation, DesignKind.WithinStrataRows] with {}
  given SupportsDesign[NullKind.PairedIndependence, DesignKind.WithinStrataRows] with {}
  given SupportsDesign[NullKind.ResponsePermutation, DesignKind.WithinStrataRows] with {}

  given SupportsDesign[NullKind.RowPermutation, DesignKind.ClusterRows] with {}
  given SupportsDesign[NullKind.PairedIndependence, DesignKind.ClusterRows] with {}
  given SupportsDesign[NullKind.ResponsePermutation, DesignKind.ClusterRows] with {}

  private def conditionedValidity(base: ValidityClaim): ValidityClaim =
    base match
      case ValidityClaim.Exact => ValidityClaim.Conditional
      case other               => other

  given SupportsDesign[NullKind.RowPermutation, DesignKind.ConditionedRows] with
    override def validity(base: ValidityClaim): ValidityClaim =
      conditionedValidity(base)

  given SupportsDesign[NullKind.PairedIndependence, DesignKind.ConditionedRows] with
    override def validity(base: ValidityClaim): ValidityClaim =
      conditionedValidity(base)

  given SupportsDesign[NullKind.ResponsePermutation, DesignKind.ConditionedRows] with
    override def validity(base: ValidityClaim): ValidityClaim =
      conditionedValidity(base)

final case class InferenceProgram[
    F,
    K <: TargetKind,
    N <: NullKind,
    D <: DesignKind
] private[inference] (
    spec: InferenceSpec[F, K, N, D],
    summary: ProblemSummary,
    validity: ValidityClaim
)

object InferenceCompiler:
  def compile[F, K <: TargetKind, N <: NullKind, D <: DesignKind](
      spec: InferenceSpec[F, K, N, D]
  )(using
      supportedTarget: SupportsTarget[F, K],
      supportedNull: SupportsNull[K, N],
      supportedDesign: SupportsDesign[N, D]
  ): InferenceProgram[F, K, N, D] =
    InferenceProgram(
      spec,
      ProblemSummary(
        fit = spec.fit.label,
        target = spec.target.label,
        nullHypothesis = spec.nullHypothesis.label,
        rows = spec.design.rowCount,
        unitPolicy = unitPolicyLabel(spec.units)
      ),
      supportedDesign.validity(supportedNull.validity)
    )

  def compileDynamic(spec: DynamicInferenceSpec): Either[InferenceError, AnyInferenceProgram] =
    spec.design match
      case DynamicDesign.ExchangeableRows(rows) =>
        val design = ResamplingDesign.exchangeableRows(rows)
        (spec.fit, spec.target, spec.nullHypothesis) match
          case (BuiltInFit.Pca, BuiltInTarget.VarianceRoots, BuiltInNull.PermuteRows) =>
            Right(AnyInferenceProgram.Pca(compile(InferenceSpec.of(
              FitDescriptor.Pca,
              TargetSpec.VarianceRoots,
              NullSpec.PermuteRows,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case (BuiltInFit.Gpca, BuiltInTarget.VarianceRoots, BuiltInNull.PermuteRows) =>
            Right(AnyInferenceProgram.Gpca(compile(InferenceSpec.of(
              FitDescriptor.Gpca,
              TargetSpec.VarianceRoots,
              NullSpec.PermuteRows,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case (BuiltInFit.Plsc, BuiltInTarget.CovarianceRoots, BuiltInNull.BreakX) =>
            Right(AnyInferenceProgram.Plsc(compile(InferenceSpec.of(
              FitDescriptor.Plsc,
              TargetSpec.CovarianceRoots,
              NullSpec.BreakX,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case (BuiltInFit.Plsc, BuiltInTarget.CovarianceRoots, BuiltInNull.BreakY) =>
            Right(AnyInferenceProgram.Plsc(compile(InferenceSpec.of(
              FitDescriptor.Plsc,
              TargetSpec.CovarianceRoots,
              NullSpec.BreakY,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case (BuiltInFit.Cca, BuiltInTarget.CanonicalCorrelations, BuiltInNull.BreakX) =>
            Right(AnyInferenceProgram.Cca(compile(InferenceSpec.of(
              FitDescriptor.Cca,
              TargetSpec.CanonicalCorrelations,
              NullSpec.BreakX,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case (BuiltInFit.Cca, BuiltInTarget.CanonicalCorrelations, BuiltInNull.BreakY) =>
            Right(AnyInferenceProgram.Cca(compile(InferenceSpec.of(
              FitDescriptor.Cca,
              TargetSpec.CanonicalCorrelations,
              NullSpec.BreakY,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case _ =>
            Left(InferenceError.UnsupportedProblem(spec.description))
      case DynamicDesign.WithinBlocks(partition) =>
        val design = ResamplingDesign.withinBlocks(partition)
        (spec.fit, spec.target, spec.nullHypothesis) match
          case (BuiltInFit.Pca, BuiltInTarget.VarianceRoots, BuiltInNull.PermuteRows) =>
            Right(AnyInferenceProgram.PcaBlocks(compile(InferenceSpec.of(
              FitDescriptor.Pca,
              TargetSpec.VarianceRoots,
              NullSpec.PermuteRows,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case (BuiltInFit.Gpca, BuiltInTarget.VarianceRoots, BuiltInNull.PermuteRows) =>
            Right(AnyInferenceProgram.GpcaBlocks(compile(InferenceSpec.of(
              FitDescriptor.Gpca,
              TargetSpec.VarianceRoots,
              NullSpec.PermuteRows,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case (BuiltInFit.Plsc, BuiltInTarget.CovarianceRoots, BuiltInNull.BreakX) =>
            Right(AnyInferenceProgram.PlscBlocks(compile(InferenceSpec.of(
              FitDescriptor.Plsc,
              TargetSpec.CovarianceRoots,
              NullSpec.BreakX,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case (BuiltInFit.Plsc, BuiltInTarget.CovarianceRoots, BuiltInNull.BreakY) =>
            Right(AnyInferenceProgram.PlscBlocks(compile(InferenceSpec.of(
              FitDescriptor.Plsc,
              TargetSpec.CovarianceRoots,
              NullSpec.BreakY,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case (BuiltInFit.Cca, BuiltInTarget.CanonicalCorrelations, BuiltInNull.BreakX) =>
            Right(AnyInferenceProgram.CcaBlocks(compile(InferenceSpec.of(
              FitDescriptor.Cca,
              TargetSpec.CanonicalCorrelations,
              NullSpec.BreakX,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case (BuiltInFit.Cca, BuiltInTarget.CanonicalCorrelations, BuiltInNull.BreakY) =>
            Right(AnyInferenceProgram.CcaBlocks(compile(InferenceSpec.of(
              FitDescriptor.Cca,
              TargetSpec.CanonicalCorrelations,
              NullSpec.BreakY,
              design,
              spec.units,
              spec.monteCarlo,
              spec.evidence,
              spec.seed
            ))))
          case _ =>
            Left(InferenceError.UnsupportedProblem(spec.description))

  private def unitPolicyLabel(value: UnitPolicy): String =
    value match
      case UnitPolicy.SingleAxes       => "single-axes"
      case UnitPolicy.GroupNearTies(_) => "group-near-ties"
      case UnitPolicy.Declared(_)      => "declared"

enum BuiltInFit:
  case Pca
  case Gpca
  case Plsc
  case Cca
  case ReducedRankRegression

enum BuiltInTarget:
  case VarianceRoots
  case CovarianceRoots
  case CanonicalCorrelations
  case PredictiveGain

enum BuiltInNull:
  case PermuteRows
  case BreakX
  case BreakY
  case PermuteResponse

enum DynamicDesign:
  case ExchangeableRows(rows: RowCount)
  case WithinBlocks(partition: RowPartition)

final case class DynamicInferenceSpec(
    fit: BuiltInFit,
    target: BuiltInTarget,
    nullHypothesis: BuiltInNull,
    design: DynamicDesign,
    units: UnitPolicy,
    monteCarlo: MonteCarloPolicy,
    evidence: RequestedEvidence,
    seed: RootSeed
):
  def description: String =
    s"fit=$fit target=$target null=$nullHypothesis design=${design.productPrefix}"

enum AnyInferenceProgram:
  case Pca(value: InferenceProgram[
      PcaFitFamily,
      TargetKind.VarianceRoots,
      NullKind.RowPermutation,
      DesignKind.ExchangeableRows
  ])
  case Gpca(value: InferenceProgram[
      GpcaFitFamily,
      TargetKind.VarianceRoots,
      NullKind.RowPermutation,
      DesignKind.ExchangeableRows
  ])
  case Plsc(value: InferenceProgram[
      PlscFitFamily,
      TargetKind.CovarianceRoots,
      NullKind.PairedIndependence,
      DesignKind.ExchangeableRows
  ])
  case Cca(value: InferenceProgram[
      CcaFitFamily,
      TargetKind.CanonicalCorrelations,
      NullKind.PairedIndependence,
      DesignKind.ExchangeableRows
  ])
  case PcaBlocks(value: InferenceProgram[
      PcaFitFamily,
      TargetKind.VarianceRoots,
      NullKind.RowPermutation,
      DesignKind.WithinBlockRows
  ])
  case GpcaBlocks(value: InferenceProgram[
      GpcaFitFamily,
      TargetKind.VarianceRoots,
      NullKind.RowPermutation,
      DesignKind.WithinBlockRows
  ])
  case PlscBlocks(value: InferenceProgram[
      PlscFitFamily,
      TargetKind.CovarianceRoots,
      NullKind.PairedIndependence,
      DesignKind.WithinBlockRows
  ])
  case CcaBlocks(value: InferenceProgram[
      CcaFitFamily,
      TargetKind.CanonicalCorrelations,
      NullKind.PairedIndependence,
      DesignKind.WithinBlockRows
  ])
