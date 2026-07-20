package scalafim.inference

trait TargetKind

object TargetKind:
  sealed trait VarianceRoots extends TargetKind
  sealed trait CovarianceRoots extends TargetKind
  sealed trait CanonicalCorrelations extends TargetKind
  sealed trait PredictiveGain extends TargetKind
  sealed trait GeneralizedEigenRoots extends TargetKind
  sealed trait ConstrainedInertiaRoots extends TargetKind
  sealed trait MultiblockConsensusRoots extends TargetKind

enum Alternative:
  case Greater
  case Less
  case TwoSided

enum TargetInvariance:
  case AxisOrientation
  case OrthogonalSubspaceRotation
  case PredictionCoordinates

trait TargetSpec[K <: TargetKind]:
  def label: TargetLabel
  def alternative: Alternative
  def invariance: TargetInvariance

object TargetSpec:
  case object VarianceRoots extends TargetSpec[TargetKind.VarianceRoots]:
    override val label: TargetLabel = TargetLabel.unsafe("ordered-variance-roots")
    override val alternative: Alternative = Alternative.Greater
    override val invariance: TargetInvariance = TargetInvariance.OrthogonalSubspaceRotation

  case object CovarianceRoots extends TargetSpec[TargetKind.CovarianceRoots]:
    override val label: TargetLabel = TargetLabel.unsafe("ordered-covariance-roots")
    override val alternative: Alternative = Alternative.Greater
    override val invariance: TargetInvariance = TargetInvariance.OrthogonalSubspaceRotation

  case object CanonicalCorrelations extends TargetSpec[TargetKind.CanonicalCorrelations]:
    override val label: TargetLabel = TargetLabel.unsafe("ordered-canonical-correlations")
    override val alternative: Alternative = Alternative.Greater
    override val invariance: TargetInvariance = TargetInvariance.OrthogonalSubspaceRotation

  case object PredictiveGain extends TargetSpec[TargetKind.PredictiveGain]:
    override val label: TargetLabel = TargetLabel.unsafe("predictive-gain")
    override val alternative: Alternative = Alternative.Greater
    override val invariance: TargetInvariance = TargetInvariance.PredictionCoordinates

  case object GeneralizedEigenRoots extends TargetSpec[TargetKind.GeneralizedEigenRoots]:
    override val label: TargetLabel = TargetLabel.unsafe("ordered-generalized-eigen-roots")
    override val alternative: Alternative = Alternative.Greater
    override val invariance: TargetInvariance = TargetInvariance.OrthogonalSubspaceRotation

  case object ConstrainedInertiaRoots extends TargetSpec[TargetKind.ConstrainedInertiaRoots]:
    override val label: TargetLabel = TargetLabel.unsafe("ordered-constrained-inertia-roots")
    override val alternative: Alternative = Alternative.Greater
    override val invariance: TargetInvariance = TargetInvariance.OrthogonalSubspaceRotation

  case object MultiblockConsensusRoots extends TargetSpec[TargetKind.MultiblockConsensusRoots]:
    override val label: TargetLabel = TargetLabel.unsafe("ordered-multiblock-consensus-roots")
    override val alternative: Alternative = Alternative.Greater
    override val invariance: TargetInvariance = TargetInvariance.OrthogonalSubspaceRotation

trait NullKind

object NullKind:
  sealed trait RowPermutation extends NullKind
  sealed trait PairedIndependence extends NullKind
  sealed trait ResponsePermutation extends NullKind
  sealed trait BlockIndependence extends NullKind

trait NullSpec[N <: NullKind]:
  def label: NullLabel

object NullSpec:
  case object PermuteRows extends NullSpec[NullKind.RowPermutation]:
    override val label: NullLabel = NullLabel.unsafe("row-permutation")

  case object BreakX extends NullSpec[NullKind.PairedIndependence]:
    override val label: NullLabel = NullLabel.unsafe("paired-independence-break-x")

  case object BreakY extends NullSpec[NullKind.PairedIndependence]:
    override val label: NullLabel = NullLabel.unsafe("paired-independence-break-y")

  case object PermuteResponse extends NullSpec[NullKind.ResponsePermutation]:
    override val label: NullLabel = NullLabel.unsafe("response-permutation")

  case object BreakBlocks extends NullSpec[NullKind.BlockIndependence]:
    override val label: NullLabel = NullLabel.unsafe("multiblock-independence")
