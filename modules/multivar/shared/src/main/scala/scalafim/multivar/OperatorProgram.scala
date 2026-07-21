package scalafim.multivar

/** Stable identity for one free frame variable in an [[OperatorProgram]]. */
opaque type ParameterId = String

object ParameterId:
  def apply(value: String): Either[ProgramError, ParameterId] =
    Identifier.validate("parameter id", value).left.map(error => ProgramError.InvalidIdentifier(error.message))

  def unsafe(value: String): ParameterId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ParameterId)
    inline def value: String = id

/** Positive finite coefficient for a structural penalty. */
opaque type PenaltyWeight = Double

object PenaltyWeight:
  def apply(value: Double): Either[ProgramError, PenaltyWeight] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(ProgramError.InvalidWeight(value))

  def unsafe(value: Double): PenaltyWeight =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (weight: PenaltyWeight)
    inline def value: Double = weight

opaque type UnitFraction = Double

object UnitFraction:
  def apply(value: Double): Either[ProgramError, UnitFraction] =
    if value.isFinite && value >= 0.0 && value <= 1.0 then Right(value)
    else Left(ProgramError.InvalidParameterization(s"fraction must be finite and in [0, 1], got $value"))

  def unsafe(value: Double): UnitFraction =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (fraction: UnitFraction)
    inline def value: Double = fraction

final case class ClosedInterval private (lower: Double, upper: Double)

object ClosedInterval:
  def from(lower: Double, upper: Double): Either[ProgramError, ClosedInterval] =
    if !lower.isFinite || !upper.isFinite || lower > upper then
      Left(ProgramError.InvalidParameterization(s"closed interval requires finite lower <= upper, got [$lower, $upper]"))
    else Right(ClosedInterval(lower, upper))

enum ProgramError:
  case InvalidIdentifier(reason: String)
  case InvalidWeight(value: Double)
  case EmptyProgram
  case DuplicateParameter(id: ParameterId)
  case UnknownParameter(id: ParameterId)
  case ComponentSpaceMismatch(id: ParameterId, expected: MvSpace, actual: MvSpace)
  case FeatureSpaceMismatch(id: ParameterId, expected: MvSpace, actual: MvSpace)
  case DuplicateNormalization(id: ParameterId)
  case MissingNormalization(id: ParameterId)
  case InvalidParameterization(reason: String)
  case InvalidResult(reason: String)

  def message: String =
    this match
      case InvalidIdentifier(reason) => reason
      case InvalidWeight(value) => s"penalty weight must be finite and positive, got $value"
      case EmptyProgram => "an operator program requires at least one frame parameter"
      case DuplicateParameter(id) => s"duplicate frame parameter '${id.value}'"
      case UnknownParameter(id) => s"objective or term references unknown frame parameter '${id.value}'"
      case ComponentSpaceMismatch(id, expected, actual) =>
        s"parameter '${id.value}' has component space ${actual.id.value}, expected ${expected.id.value}"
      case FeatureSpaceMismatch(id, expected, actual) =>
        s"parameter '${id.value}' has feature space ${actual.id.value}, expected ${expected.id.value}"
      case DuplicateNormalization(id) => s"parameter '${id.value}' has more than one normalization"
      case MissingNormalization(id) => s"parameter '${id.value}' has no normalization"
      case InvalidParameterization(reason) => reason
      case InvalidResult(reason) => reason

/** The semantic optimization variable `W : K -> C*`; this is a declaration,
  * not a matrix allocated before fitting.
  */
final class FrameVariable[Feature <: SemanticSpace, Component <: SemanticSpace] private (
    val id: ParameterId,
    val featureSpace: SpaceEvidence[Feature],
    val componentSpace: SpaceEvidence[Component]
)

object FrameVariable:
  def from[Feature <: SemanticSpace, Component <: SemanticSpace](
      id: ParameterId,
      featureSpace: SpaceEvidence[Feature],
      componentSpace: SpaceEvidence[Component]
  ): Either[ProgramError, FrameVariable[Feature, Component]] =
    if componentSpace.dimension > featureSpace.dimension then
      Left(
        ProgramError.InvalidParameterization(
          s"parameter '${id.value}' requests ${componentSpace.dimension} components in ${featureSpace.dimension} feature dimensions"
        )
      )
    else Right(new FrameVariable(id, featureSpace, componentSpace))

enum ParameterizationGauge:
  case Unique
  case SignedPermutation
  case Orthogonal
  case GeneralLinear

enum ParameterizationKind:
  case Identity
  case KnownSupport(embedding: ValueIdentity, injective: Boolean)
  case SharedBasis(basis: ValueIdentity, injective: Boolean)
  case FixedRank(rank: ComponentCount, gauge: ParameterizationGauge)
  case BlockDiagonal(blocks: Vector[ParameterId])
  case NullSpace(basis: ValueIdentity, rankTolerance: CertificateTolerance)

/** Typed declaration of the map from a free coordinate to the semantic frame.
  * Exact linear reductions accept an `Op` at construction and retain its stable
  * identity; the solver-facing lowering remains a later concern.
  */
final case class FrameParameterization[Feature <: SemanticSpace, Component <: SemanticSpace] private (
    variable: FrameVariable[Feature, Component],
    freeFeatureSpace: MvSpace,
    kind: ParameterizationKind
)

object FrameParameterization:
  def identity[Feature <: SemanticSpace, Component <: SemanticSpace](
      variable: FrameVariable[Feature, Component]
  ): FrameParameterization[Feature, Component] =
    FrameParameterization(variable, variable.featureSpace.descriptor, ParameterizationKind.Identity)

  def knownSupport[
      Feature <: SemanticSpace,
      FreeFeature <: SemanticSpace,
      Component <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      variable: FrameVariable[Feature, Component],
      freeFeatureSpace: SpaceEvidence[FreeFeature],
      embedding: Op[Dual[FreeFeature], Dual[Feature], R, E],
      injective: Boolean
  ): FrameParameterization[Feature, Component] =
    FrameParameterization(
      variable,
      freeFeatureSpace.descriptor,
      ParameterizationKind.KnownSupport(embedding.valueIdentity, injective)
    )

  def nullSpace[
      Feature <: SemanticSpace,
      FreeFeature <: SemanticSpace,
      Component <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      variable: FrameVariable[Feature, Component],
      freeFeatureSpace: SpaceEvidence[FreeFeature],
      basis: Op[Dual[FreeFeature], Dual[Feature], R, E],
      rankTolerance: CertificateTolerance
  ): FrameParameterization[Feature, Component] =
    FrameParameterization(
      variable,
      freeFeatureSpace.descriptor,
      ParameterizationKind.NullSpace(basis.valueIdentity, rankTolerance)
    )

  def sharedBasis[
      Feature <: SemanticSpace,
      FreeFeature <: SemanticSpace,
      Component <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      variable: FrameVariable[Feature, Component],
      freeFeatureSpace: SpaceEvidence[FreeFeature],
      basis: Op[Dual[FreeFeature], Dual[Feature], R, E],
      injective: Boolean
  ): FrameParameterization[Feature, Component] =
    FrameParameterization(
      variable,
      freeFeatureSpace.descriptor,
      ParameterizationKind.SharedBasis(basis.valueIdentity, injective)
    )

  def fixedRank[Feature <: SemanticSpace, Component <: SemanticSpace](
      variable: FrameVariable[Feature, Component],
      rank: ComponentCount,
      gauge: ParameterizationGauge = ParameterizationGauge.GeneralLinear
  ): Either[ProgramError, FrameParameterization[Feature, Component]] =
    if rank.value > Math.min(variable.featureSpace.dimension, variable.componentSpace.dimension) then
      Left(ProgramError.InvalidParameterization(s"fixed rank ${rank.value} exceeds the frame dimensions"))
    else Right(FrameParameterization(variable, variable.featureSpace.descriptor, ParameterizationKind.FixedRank(rank, gauge)))

  def blockDiagonal[Feature <: SemanticSpace, Component <: SemanticSpace](
      variable: FrameVariable[Feature, Component],
      blocks: Vector[ParameterId]
  ): Either[ProgramError, FrameParameterization[Feature, Component]] =
    if blocks.isEmpty || blocks.distinct.length != blocks.length then
      Left(ProgramError.InvalidParameterization("block-diagonal parameterization requires distinct non-empty block ids"))
    else Right(FrameParameterization(variable, variable.featureSpace.descriptor, ParameterizationKind.BlockDiagonal(blocks)))

private final case class ObjectiveBinding(parameter: ParameterId, featureSpace: MvSpace, componentSpace: MvSpace)

/** Symbolic `W* S W`; the component operator is derived only after a candidate
  * frame exists, never stored as if it preceded the optimization variable.
  */
final case class SelfCompressionExpression[
    Feature <: SemanticSpace,
    Component <: SemanticSpace,
    R <: OperatorRoleTag,
    E <: OperatorEvidence
](
    parameter: FrameVariable[Feature, Component],
    secondOrder: Op[Dual[Feature], Primal[Feature], R, E]
):
  def evaluate[EW <: OperatorEvidence](
      frame: OpFrame[Feature, Component, EW]
  ): Op[Primal[Component], Dual[Component], ComponentOperatorRole, UncheckedEvidence] =
    OperatorAlgebra.compress(frame, secondOrder, frame)

/** Symbolic `W_s* S_st W_t` for a pair of frame variables. */
final case class CrossCompressionExpression[
    SourceFeature <: SemanticSpace,
    TargetFeature <: SemanticSpace,
    SourceComponent <: SemanticSpace,
    TargetComponent <: SemanticSpace,
    R <: OperatorRoleTag,
    E <: OperatorEvidence
](
    source: FrameVariable[SourceFeature, SourceComponent],
    target: FrameVariable[TargetFeature, TargetComponent],
    secondOrder: Op[Dual[TargetFeature], Primal[SourceFeature], R, E]
):
  def evaluate[ES <: OperatorEvidence, ET <: OperatorEvidence](
      sourceFrame: OpFrame[SourceFeature, SourceComponent, ES],
      targetFrame: OpFrame[TargetFeature, TargetComponent, ET]
  ): Op[Primal[TargetComponent], Dual[SourceComponent], ComponentOperatorRole, UncheckedEvidence] =
    OperatorAlgebra.compress(sourceFrame, secondOrder, targetFrame)

/** Closed catalog of objectives that lower to the common operator core. */
enum BaseObjective:
  case MaximizeTrace[F <: SemanticSpace, K <: SemanticSpace, R <: OperatorRoleTag, E <: OperatorEvidence](
      expression: SelfCompressionExpression[F, K, R, E]
  )
  case MaximizeCrossTrace[
      SF <: SemanticSpace,
      TF <: SemanticSpace,
      SK <: SemanticSpace,
      TK <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      expression: CrossCompressionExpression[SF, TF, SK, TK, R, E]
  )
  case GeneralizedRayleigh[
      F <: SemanticSpace,
      K <: SemanticSpace,
      RN <: OperatorRoleTag,
      EN <: OperatorEvidence,
      RD <: OperatorRoleTag,
      ED <: SpdEvidence
  ](
      numerator: SelfCompressionExpression[F, K, RN, EN],
      denominator: SelfCompressionExpression[F, K, RD, ED]
  )
  case TraceRatio[
      F <: SemanticSpace,
      K <: SemanticSpace,
      RN <: OperatorRoleTag,
      EN <: OperatorEvidence,
      RD <: OperatorRoleTag,
      ED <: SpdEvidence
  ](
      numerator: SelfCompressionExpression[F, K, RN, EN],
      denominator: SelfCompressionExpression[F, K, RD, ED]
  )
  case RatioTrace[
      F <: SemanticSpace,
      K <: SemanticSpace,
      RN <: OperatorRoleTag,
      EN <: OperatorEvidence,
      RD <: OperatorRoleTag,
      ED <: SpdEvidence
  ](
      numerator: SelfCompressionExpression[F, K, RN, EN],
      denominator: SelfCompressionExpression[F, K, RD, ED]
  )
  case MinimizeDisagreement[F <: SemanticSpace, K <: SemanticSpace, R <: OperatorRoleTag, E <: OperatorEvidence](
      expression: SelfCompressionExpression[F, K, R, E]
  )
  case SequentialCrossRegression[
      SF <: SemanticSpace,
      TF <: SemanticSpace,
      SK <: SemanticSpace,
      TK <: SemanticSpace,
      RC <: OperatorRoleTag,
      EC <: OperatorEvidence,
      RP <: OperatorRoleTag,
      EP <: SpdEvidence
  ](
      cross: CrossCompressionExpression[SF, TF, SK, TK, RC, EC],
      predictor: SelfCompressionExpression[SF, SK, RP, EP]
  )

  def label: String =
    this match
      case MaximizeTrace(_) => "maximize-trace"
      case MaximizeCrossTrace(_) => "maximize-cross-trace"
      case GeneralizedRayleigh(_, _) => "generalized-rayleigh"
      case TraceRatio(_, _) => "trace-ratio"
      case RatioTrace(_, _) => "ratio-trace"
      case MinimizeDisagreement(_) => "minimize-disagreement"
      case SequentialCrossRegression(_, _) => "sequential-cross-regression"

  private[multivar] def bindings: Vector[ObjectiveBinding] =
    this match
      case MaximizeTrace(expression) =>
        Vector(binding(expression.parameter))
      case MaximizeCrossTrace(expression) =>
        Vector(
          binding(expression.source),
          binding(expression.target)
        )
      case GeneralizedRayleigh(numerator, denominator) =>
        Vector(binding(numerator.parameter), binding(denominator.parameter))
      case TraceRatio(numerator, denominator) =>
        Vector(binding(numerator.parameter), binding(denominator.parameter))
      case RatioTrace(numerator, denominator) =>
        Vector(binding(numerator.parameter), binding(denominator.parameter))
      case MinimizeDisagreement(expression) =>
        Vector(binding(expression.parameter))
      case SequentialCrossRegression(cross, predictor) =>
        Vector(
          binding(cross.source),
          binding(cross.target),
          binding(predictor.parameter)
        )

  private def binding(variable: FrameVariable[?, ?]): ObjectiveBinding =
    ObjectiveBinding(variable.id, variable.featureSpace.descriptor, variable.componentSpace.descriptor)

enum TargetCapability:
  case Linear
  case Affine
  case Smooth
  case General

final case class TargetExpression private (
    parameters: Vector[ParameterId],
    capability: TargetCapability,
    operation: String,
    operators: Vector[ValueIdentity],
    equivariance: FrameSymmetry
):
  require(parameters.nonEmpty, "target expression requires at least one parameter")
  def parameter: ParameterId = parameters.head
  def operator: Option[ValueIdentity] = operators.headOption

object TargetExpression:
  def frame(parameter: ParameterId): TargetExpression =
    TargetExpression(Vector(parameter), TargetCapability.Linear, "frame", Vector.empty, FrameSymmetry.Orthogonal)

  def linear[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag, E <: OperatorEvidence](
      parameter: ParameterId,
      operation: String,
      operator: Op[From, To, R, E]
  ): Either[ProgramError, TargetExpression] =
    val clean = operation.trim
    if clean.isEmpty then Left(ProgramError.InvalidParameterization("target operation must be non-empty"))
    else Right(
      TargetExpression(
        Vector(parameter),
        TargetCapability.Linear,
        clean,
        Vector(operator.valueIdentity),
        FrameSymmetry.Orthogonal
      )
    )

  def affine(parameter: ParameterId, operation: String): Either[ProgramError, TargetExpression] =
    named(parameter, TargetCapability.Affine, operation)

  def smooth(parameter: ParameterId, operation: String): Either[ProgramError, TargetExpression] =
    named(parameter, TargetCapability.Smooth, operation)

  def general(parameter: ParameterId, operation: String): Either[ProgramError, TargetExpression] =
    named(parameter, TargetCapability.General, operation)

  def typed[A](expression: TypedExpression[A]): TargetExpression =
    TargetExpression(
      expression.parameterIds,
      expression.capability match
        case MapCapability.Linear => TargetCapability.Linear
        case MapCapability.Affine => TargetCapability.Affine
        case MapCapability.Smooth => TargetCapability.Smooth
        case MapCapability.General => TargetCapability.General,
      expression.operations.mkString("/"),
      expression.operatorIdentities,
      expression.equivariance
    )

  private def named(
      parameter: ParameterId,
      capability: TargetCapability,
      operation: String
  ): Either[ProgramError, TargetExpression] =
    val clean = operation.trim
    if clean.isEmpty then Left(ProgramError.InvalidParameterization("target operation must be non-empty"))
    else Right(TargetExpression(Vector(parameter), capability, clean, Vector.empty, FrameSymmetry.Orthogonal))

enum FunctionalKind:
  case SquaredNorm(geometry: ValueIdentity)
  case L1
  case GroupL21
  case GroupL2(groups: ValueIdentity)
  case SparseGroup(l1Fraction: UnitFraction, groups: ValueIdentity)
  case ElasticNet(l1Fraction: UnitFraction)
  case Huber(delta: PenaltyWeight)
  case TotalVariation
  case NuclearNorm
  case NegativeLogDet

  def symmetry: FrameSymmetry =
    this match
      case SquaredNorm(_) | GroupL21 | GroupL2(_) | NuclearNorm | NegativeLogDet => FrameSymmetry.Orthogonal
      case L1 | SparseGroup(_, _) | ElasticNet(_) | Huber(_) | TotalVariation => FrameSymmetry.SignedPermutation

  def traits: FunctionalTraits =
    this match
      case SquaredNorm(_) =>
        FunctionalTraits(
          ConvexityTrait.Convex,
          SmoothnessTrait.Smooth,
          HomogeneityTrait.DegreeTwo,
          SeparabilityTrait.Nonseparable,
          Set(OracleCapability.Gradient, OracleCapability.HessianVector),
          symmetry
        )
      case L1 | GroupL21 | GroupL2(_) | TotalVariation | NuclearNorm =>
        FunctionalTraits(
          ConvexityTrait.Convex,
          SmoothnessTrait.Nonsmooth,
          HomogeneityTrait.DegreeOne,
          this match
            case L1 => SeparabilityTrait.Elementwise
            case GroupL21 => SeparabilityTrait.Rowwise
            case GroupL2(_) => SeparabilityTrait.Blockwise
            case NuclearNorm => SeparabilityTrait.Spectral
            case _ => SeparabilityTrait.Nonseparable,
          Set(OracleCapability.Proximal, OracleCapability.Conic),
          symmetry
        )
      case ElasticNet(_) | SparseGroup(_, _) =>
        FunctionalTraits(
          ConvexityTrait.Convex,
          SmoothnessTrait.Nonsmooth,
          HomogeneityTrait.None,
          this match
            case ElasticNet(_) => SeparabilityTrait.Elementwise
            case _ => SeparabilityTrait.Blockwise,
          Set(OracleCapability.Proximal, OracleCapability.Conic),
          symmetry
        )
      case Huber(_) =>
        FunctionalTraits(
          ConvexityTrait.Convex,
          SmoothnessTrait.Smooth,
          HomogeneityTrait.None,
          SeparabilityTrait.Elementwise,
          Set(OracleCapability.Gradient, OracleCapability.Proximal, OracleCapability.Conic),
          symmetry
        )
      case NegativeLogDet =>
        FunctionalTraits(
          ConvexityTrait.Convex,
          SmoothnessTrait.Smooth,
          HomogeneityTrait.None,
          SeparabilityTrait.Spectral,
          Set(OracleCapability.Gradient, OracleCapability.HessianVector, OracleCapability.Conic),
          symmetry
        )

enum FeasibleSetKind:
  case ZeroSubspace
  case NonnegativeOrthant
  case Simplex
  case Monotone(order: ValueIdentity)
  case Box(bounds: ClosedInterval)
  case NormBall(radius: PenaltyWeight)
  case PsdCone
  case Stiefel
  case FixedSupport(indices: IndexSet)
  case RankBounded(rank: ComponentCount)

  def symmetry: FrameSymmetry =
    this match
      case ZeroSubspace | NormBall(_) | PsdCone | Stiefel | FixedSupport(_) | RankBounded(_) =>
        FrameSymmetry.Orthogonal
      case NonnegativeOrthant | Simplex | Monotone(_) | Box(_) =>
        FrameSymmetry.Permutation

  def traits: FeasibleSetTraits =
    this match
      case ZeroSubspace | NonnegativeOrthant | Simplex | Monotone(_) | Box(_) | NormBall(_) =>
        FeasibleSetTraits(
          SetConvexity.Convex,
          closed = true,
          SetStructure.Euclidean,
          this match
            case NonnegativeOrthant | Box(_) => SeparabilityTrait.Elementwise
            case Monotone(_) => SeparabilityTrait.Blockwise
            case _ => SeparabilityTrait.Nonseparable,
          Set(SetCapability.Projection, SetCapability.Conic, SetCapability.NormalCone),
          symmetry
        )
      case PsdCone =>
        FeasibleSetTraits(
          SetConvexity.Convex,
          closed = true,
          SetStructure.Cone,
          SeparabilityTrait.Spectral,
          Set(SetCapability.Projection, SetCapability.Conic, SetCapability.NormalCone),
          symmetry
        )
      case Stiefel =>
        FeasibleSetTraits(
          SetConvexity.Nonconvex,
          closed = true,
          SetStructure.Manifold,
          SeparabilityTrait.Nonseparable,
          Set(SetCapability.Projection, SetCapability.NormalCone),
          symmetry
        )
      case FixedSupport(_) | RankBounded(_) =>
        FeasibleSetTraits(
          SetConvexity.Nonconvex,
          closed = true,
          SetStructure.Discrete,
          SeparabilityTrait.Nonseparable,
          Set(SetCapability.Projection),
          symmetry
        )

enum FrameSymmetry:
  case Orthogonal
  case SignedPermutation
  case Permutation
  case Identity

object FrameSymmetry:
  def meet(left: FrameSymmetry, right: FrameSymmetry): FrameSymmetry =
    (left, right) match
      case (FrameSymmetry.Identity, _) | (_, FrameSymmetry.Identity) => FrameSymmetry.Identity
      case (FrameSymmetry.Permutation, _) | (_, FrameSymmetry.Permutation) => FrameSymmetry.Permutation
      case (FrameSymmetry.SignedPermutation, _) | (_, FrameSymmetry.SignedPermutation) => FrameSymmetry.SignedPermutation
      case _ => FrameSymmetry.Orthogonal

final case class PenaltyTerm(
    target: TargetExpression,
    functional: FunctionalKind,
    weight: PenaltyWeight
):
  def symmetry: FrameSymmetry = FrameSymmetry.meet(target.equivariance, functional.symmetry)

object PenaltyTerm:
  def typed[Z](
      target: TypedExpression[Z],
      functional: TypedFunctional[Z],
      weight: PenaltyWeight
  ): PenaltyTerm =
    PenaltyTerm(TargetExpression.typed(target), functional.kind, weight)

final case class ConstraintTerm(
    target: TargetExpression,
    feasibleSet: FeasibleSetKind
):
  def symmetry: FrameSymmetry = FrameSymmetry.meet(target.equivariance, feasibleSet.symmetry)

object ConstraintTerm:
  def typed[Z](target: TypedExpression[Z], feasibleSet: TypedFeasibleSet[Z]): ConstraintTerm =
    ConstraintTerm(TargetExpression.typed(target), feasibleSet.kind)

final class FrameNormalization[Feature <: SemanticSpace, Component <: SemanticSpace, E <: SpdEvidence] private (
    val parameter: FrameVariable[Feature, Component],
    val geometry: Op[Dual[Feature], Primal[Feature], ? <: OperatorRoleTag, E]
)

object FrameNormalization:
  def apply[
      Feature <: SemanticSpace,
      Component <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: SpdEvidence
  ](
      parameter: FrameVariable[Feature, Component],
      geometry: Op[Dual[Feature], Primal[Feature], R, E]
  ): FrameNormalization[Feature, Component, E] =
    new FrameNormalization(parameter, geometry)

enum RepresentativeRule:
  case DeterministicSign
  case OrderedSpectrumThenSign
  case ProcrustesToReference(reference: ValueIdentity)
  case PredictionMap
  case ObjectiveValueOnly

enum PredictionMetric:
  case SquaredError
  case Correlation
  case Mahalanobis(metric: ValueIdentity)

enum ResultEquivalence:
  case ValueEquivalent(tolerance: CertificateTolerance)
  case OperatorEquivalent(domain: CoordinateDescriptor, codomain: CoordinateDescriptor, tolerance: CertificateTolerance)
  case SubspaceEquivalent(projectorTolerance: CertificateTolerance, principalAngleTolerance: CertificateTolerance)
  case FrameEquivalent(symmetry: FrameSymmetry, tolerance: CertificateTolerance)
  case PredictionEquivalent(metric: PredictionMetric, tolerance: CertificateTolerance)
  case ObjectiveEquivalent(tolerance: CertificateTolerance)

enum SolverGuarantee:
  case GlobalSpectralOptimum
  case GlobalConvexOptimum
  case StationaryPoint
  case FeasiblePoint
  case CoordinatewiseStationary
  case LocallyOptimal
  case HeuristicFeasible
  case Unresolved

final case class ResultSemantics(
    equivalence: ResultEquivalence,
    representative: RepresentativeRule,
    guarantee: SolverGuarantee,
    parameterIdentifiability: ParameterIdentifiability = ParameterIdentifiability.identified
)

final case class ParameterIdentifiability(
    redundantCoordinates: Boolean,
    gauges: Vector[ParameterizationGauge]
)

object ParameterIdentifiability:
  val identified: ParameterIdentifiability = ParameterIdentifiability(redundantCoordinates = false, Vector.empty)

  private[multivar] def infer(
      parameterizations: Vector[FrameParameterization[? <: SemanticSpace, ? <: SemanticSpace]]
  ): ParameterIdentifiability =
    val gauges = parameterizations.flatMap: parameterization =>
      parameterization.kind match
        case ParameterizationKind.FixedRank(_, gauge) if gauge != ParameterizationGauge.Unique => Vector(gauge)
        case _ => Vector.empty
    val redundant = parameterizations.exists: parameterization =>
      parameterization.kind match
        case ParameterizationKind.FixedRank(_, gauge) => gauge != ParameterizationGauge.Unique
        case ParameterizationKind.KnownSupport(_, injective) => !injective
        case ParameterizationKind.SharedBasis(_, _) => false
        case _ => false
    ParameterIdentifiability(redundant, gauges.distinct)

object ResultSemantics:
  private[multivar] def infer(
      objective: BaseObjective,
      parameterizations: Vector[FrameParameterization[? <: SemanticSpace, ? <: SemanticSpace]],
      penalties: Vector[PenaltyTerm],
      constraints: Vector[ConstraintTerm]
  ): ResultSemantics =
    val symmetry =
      (penalties.map(_.symmetry) ++ constraints.map(_.symmetry)).foldLeft(FrameSymmetry.Orthogonal)(FrameSymmetry.meet)
    val smoothSpectral = penalties.isEmpty && constraints.isEmpty
    objective match
      case BaseObjective.SequentialCrossRegression(_, _) =>
        ResultSemantics(
          ResultEquivalence.PredictionEquivalent(PredictionMetric.SquaredError, CertificateTolerance.strict),
          RepresentativeRule.PredictionMap,
          if smoothSpectral then SolverGuarantee.GlobalSpectralOptimum else SolverGuarantee.StationaryPoint,
          ParameterIdentifiability.infer(parameterizations)
        )
      case BaseObjective.MaximizeCrossTrace(_) =>
        ResultSemantics(
          ResultEquivalence.FrameEquivalent(symmetry, CertificateTolerance.strict),
          RepresentativeRule.OrderedSpectrumThenSign,
          if smoothSpectral then SolverGuarantee.GlobalSpectralOptimum else SolverGuarantee.StationaryPoint,
          ParameterIdentifiability.infer(parameterizations)
        )
      case _ =>
        val equivalence =
          if symmetry == FrameSymmetry.Orthogonal then
            ResultEquivalence.SubspaceEquivalent(CertificateTolerance.strict, CertificateTolerance.strict)
          else ResultEquivalence.FrameEquivalent(symmetry, CertificateTolerance.strict)
        ResultSemantics(
          equivalence,
          RepresentativeRule.OrderedSpectrumThenSign,
          if smoothSpectral then SolverGuarantee.GlobalSpectralOptimum else SolverGuarantee.StationaryPoint,
          ParameterIdentifiability.infer(parameterizations)
        )

final case class OperatorProgramDescriptor(
    parameters: Vector[(String, MvSpace, MvSpace, ParameterizationKind)],
    objective: String,
    normalizations: Vector[(String, ValueIdentity)],
    penalties: Vector[PenaltyTerm],
    constraints: Vector[ConstraintTerm],
    result: ResultSemantics
)

final class OperatorProgram private (
    val parameters: Vector[FrameParameterization[? <: SemanticSpace, ? <: SemanticSpace]],
    val objective: BaseObjective,
    val normalizations: Vector[FrameNormalization[? <: SemanticSpace, ? <: SemanticSpace, ? <: SpdEvidence]],
    val penalties: Vector[PenaltyTerm],
    val constraints: Vector[ConstraintTerm],
    val resultSemantics: ResultSemantics,
    val provenance: SemanticProvenance
):
  def descriptor: OperatorProgramDescriptor =
    OperatorProgramDescriptor(
      parameters.map(parameter => (
        parameter.variable.id.value,
        parameter.variable.featureSpace.descriptor,
        parameter.variable.componentSpace.descriptor,
        parameter.kind
      )),
      objective.label,
      normalizations.map(normalization => normalization.parameter.id.value -> normalization.geometry.valueIdentity),
      penalties,
      constraints,
      resultSemantics
    )

object OperatorProgram:
  def from(
      parameters: Vector[FrameParameterization[? <: SemanticSpace, ? <: SemanticSpace]],
      objective: BaseObjective,
      normalizations: Vector[FrameNormalization[? <: SemanticSpace, ? <: SemanticSpace, ? <: SpdEvidence]],
      penalties: Vector[PenaltyTerm] = Vector.empty,
      constraints: Vector[ConstraintTerm] = Vector.empty,
      provenance: SemanticProvenance = SemanticProvenance.source("operator-program")
  ): Either[ProgramError, OperatorProgram] =
    val ids = parameters.map(_.variable.id)
    if parameters.isEmpty then Left(ProgramError.EmptyProgram)
    else if ids.distinct.length != ids.length then Left(ProgramError.DuplicateParameter(firstDuplicate(ids)))
    else
      for
        _ <- validateObjective(parameters, objective)
        _ <- validateNormalizations(parameters, normalizations)
        _ <- validateTerms(ids, penalties.map(_.target) ++ constraints.map(_.target))
      yield
        new OperatorProgram(
          parameters,
          objective,
          normalizations,
          penalties,
          constraints,
          ResultSemantics.infer(objective, parameters, penalties, constraints),
          provenance
        )

  private def validateObjective(
      parameters: Vector[FrameParameterization[? <: SemanticSpace, ? <: SemanticSpace]],
      objective: BaseObjective
  ): Either[ProgramError, Unit] =
    val bindings = objective.bindings
    validateObjectiveIdentities(objective).flatMap: _ =>
      bindings.foldLeft[Either[ProgramError, Unit]](Right(())): (result, binding) =>
        result.flatMap: _ =>
          parameters.find(_.variable.id == binding.parameter) match
            case None => Left(ProgramError.UnknownParameter(binding.parameter))
            case Some(parameter) =>
              val actualFeature = parameter.variable.featureSpace.descriptor
              val actualComponent = parameter.variable.componentSpace.descriptor
              if actualFeature != binding.featureSpace then
                Left(ProgramError.FeatureSpaceMismatch(binding.parameter, binding.featureSpace, actualFeature))
              else if actualComponent != binding.componentSpace then
                Left(ProgramError.ComponentSpaceMismatch(binding.parameter, binding.componentSpace, actualComponent))
              else Right(())

  private def validateObjectiveIdentities(objective: BaseObjective): Either[ProgramError, Unit] =
    objective match
      case BaseObjective.MaximizeCrossTrace(expression) if expression.source.id == expression.target.id =>
        Left(ProgramError.InvalidParameterization(s"${objective.label} requires distinct frame parameters"))
      case BaseObjective.GeneralizedRayleigh(numerator, denominator)
          if numerator.parameter.id != denominator.parameter.id =>
        Left(ProgramError.InvalidParameterization("generalized-rayleigh numerator and denominator must bind the same frame parameter"))
      case BaseObjective.TraceRatio(numerator, denominator)
          if numerator.parameter.id != denominator.parameter.id =>
        Left(ProgramError.InvalidParameterization("trace-ratio numerator and denominator must bind the same frame parameter"))
      case BaseObjective.RatioTrace(numerator, denominator)
          if numerator.parameter.id != denominator.parameter.id =>
        Left(ProgramError.InvalidParameterization("ratio-trace numerator and denominator must bind the same frame parameter"))
      case BaseObjective.SequentialCrossRegression(cross, _)
          if cross.source.id == cross.target.id =>
        Left(ProgramError.InvalidParameterization(s"${objective.label} requires distinct source and target frame parameters"))
      case BaseObjective.SequentialCrossRegression(cross, predictor)
          if cross.source.id != predictor.parameter.id =>
        Left(ProgramError.InvalidParameterization("sequential-cross-regression predictor must bind the cross-expression source parameter"))
      case _ => Right(())

  private def validateNormalizations(
      parameters: Vector[FrameParameterization[? <: SemanticSpace, ? <: SemanticSpace]],
      normalizations: Vector[FrameNormalization[? <: SemanticSpace, ? <: SemanticSpace, ? <: SpdEvidence]]
  ): Either[ProgramError, Unit] =
    val ids = parameters.map(_.variable.id)
    normalizations.find(normalization => !ids.contains(normalization.parameter.id)) match
      case Some(normalization) => Left(ProgramError.UnknownParameter(normalization.parameter.id))
      case None =>
        parameters.foldLeft[Either[ProgramError, Unit]](Right(())): (result, parameterization) =>
          result.flatMap: _ =>
            val parameter = parameterization.variable
            val matches = normalizations.filter(_.parameter.id == parameter.id)
            if matches.isEmpty then Left(ProgramError.MissingNormalization(parameter.id))
            else if matches.length > 1 then Left(ProgramError.DuplicateNormalization(parameter.id))
            else
              val actual = matches.head.geometry.domain.descriptor.space
              val expected = parameter.featureSpace.descriptor
              if actual != expected then Left(ProgramError.FeatureSpaceMismatch(parameter.id, expected, actual))
              else Right(())

  private def validateTerms(ids: Vector[ParameterId], targets: Vector[TargetExpression]): Either[ProgramError, Unit] =
    targets.flatMap(_.parameters).find(parameter => !ids.contains(parameter)) match
      case Some(parameter) => Left(ProgramError.UnknownParameter(parameter))
      case None => Right(())

  private def firstDuplicate(ids: Vector[ParameterId]): ParameterId =
    ids.find(id => ids.count(_ == id) > 1).get

final case class NumericalIdentifiability(
    retainedRank: Int,
    spectralClusters: Vector[Vector[Int]],
    residual: Double,
    context: CertificateContext
)

/** Evidence for the guarantee actually achieved by one solver run.
  *
  * A program's result semantics state the guarantee required of a conforming
  * fit. The attestation is created only after the returned frames and their
  * numerical residual have been checked, so a fit cannot merely repeat that
  * declaration without solver evidence.
  */
final case class SolverAttestation private (
    guarantee: SolverGuarantee,
    certificate: NumericalCertificate
)

object SolverAttestation:
  private[multivar] def certified(
      guarantee: SolverGuarantee,
      certificate: NumericalCertificate
  ): SolverAttestation =
    new SolverAttestation(guarantee, certificate)

final case class FittedFrame[
    Feature <: SemanticSpace,
    Component <: SemanticSpace,
    E <: OperatorEvidence
](
    parameter: FrameVariable[Feature, Component],
    frame: FunctionalFrame[Feature, Component, E]
)

final case class OperatorProgramFit(
    program: OperatorProgram,
    frames: Vector[FittedFrame[? <: SemanticSpace, ? <: SemanticSpace, ? <: OperatorEvidence]],
    objectiveValue: Double,
    identifiability: NumericalIdentifiability,
    solverAttestation: SolverAttestation,
    provenance: SemanticProvenance
)

object OperatorProgramFit:
  def from(
      program: OperatorProgram,
      frames: Vector[FittedFrame[? <: SemanticSpace, ? <: SemanticSpace, ? <: OperatorEvidence]],
      objectiveValue: Double,
      identifiability: NumericalIdentifiability,
      provenance: SemanticProvenance
  ): Either[ProgramError, OperatorProgramFit] =
    val expected = program.parameters.map(_.variable.id).toSet
    val actual = frames.map(_.parameter.id)
    val fitIdentity = ValueIdentity.derived(
      "operator-program-fit",
      frames.map(_.frame.weights.valueIdentity)*
    )
    if !objectiveValue.isFinite then Left(ProgramError.InvalidResult("objective value must be finite"))
    else if identifiability.retainedRank < 0 || !identifiability.residual.isFinite || identifiability.residual < 0.0 then
      Left(ProgramError.InvalidResult("identifiability rank and residual must be finite and non-negative"))
    else if actual.distinct.length != actual.length || actual.toSet != expected then
      Left(ProgramError.InvalidResult("fitted frames must match the program parameters exactly once"))
    else if frames.exists: fitted =>
        program.parameters.find(_.variable.id == fitted.parameter.id).exists: declared =>
          declared.variable.featureSpace.descriptor != fitted.parameter.featureSpace.descriptor ||
            declared.variable.componentSpace.descriptor != fitted.parameter.componentSpace.descriptor
    then Left(ProgramError.InvalidResult("fitted frame spaces must match their declared program parameter"))
    else
      Certificate
        .converged(
          fitIdentity,
          iterations = 0,
          identifiability.residual,
          Math.max(1.0, Math.abs(objectiveValue)),
          identifiability.context
        )
        .left
        .map(error => ProgramError.InvalidResult(s"solver convergence was not certified: ${error.message}"))
        .map: certificate =>
          OperatorProgramFit(
            program,
            frames,
            objectiveValue,
            identifiability,
            SolverAttestation.certified(program.resultSemantics.guarantee, certificate.runtime),
            provenance
          )

/** Named method constructors assemble the shared program vocabulary; they do
  * not introduce method-specific solver or matrix representations.
  */
object OperatorPrograms:
  def gpca[
      Feature <: SemanticSpace,
      Component <: SemanticSpace,
      RO <: OperatorRoleTag,
      EO <: OperatorEvidence,
      EN <: SpdEvidence
  ](
      parameterization: FrameParameterization[Feature, Component],
      covariance: Op[Dual[Feature], Primal[Feature], RO, EO],
      normalization: FrameNormalization[Feature, Component, EN]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(parameterization),
      BaseObjective.MaximizeTrace(SelfCompressionExpression(parameterization.variable, covariance)),
      Vector(normalization),
      provenance = SemanticProvenance.source("gpca-program")
    )

  def ldaRayleigh[
      Feature <: SemanticSpace,
      Component <: SemanticSpace,
      RB <: OperatorRoleTag,
      EB <: OperatorEvidence,
      RW <: OperatorRoleTag,
      EW <: SpdEvidence,
      EN <: SpdEvidence
  ](
      parameterization: FrameParameterization[Feature, Component],
      between: Op[Dual[Feature], Primal[Feature], RB, EB],
      within: Op[Dual[Feature], Primal[Feature], RW, EW],
      normalization: FrameNormalization[Feature, Component, EN]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(parameterization),
      BaseObjective.GeneralizedRayleigh(
        SelfCompressionExpression(parameterization.variable, between),
        SelfCompressionExpression(parameterization.variable, within)
      ),
      Vector(normalization),
      provenance = SemanticProvenance.source("lda-rayleigh-program")
    )

  def ldaTraceRatio[
      Feature <: SemanticSpace,
      Component <: SemanticSpace,
      RB <: OperatorRoleTag,
      EB <: OperatorEvidence,
      RW <: OperatorRoleTag,
      EW <: SpdEvidence,
      EN <: SpdEvidence
  ](
      parameterization: FrameParameterization[Feature, Component],
      between: Op[Dual[Feature], Primal[Feature], RB, EB],
      within: Op[Dual[Feature], Primal[Feature], RW, EW],
      normalization: FrameNormalization[Feature, Component, EN]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(parameterization),
      BaseObjective.TraceRatio(
        SelfCompressionExpression(parameterization.variable, between),
        SelfCompressionExpression(parameterization.variable, within)
      ),
      Vector(normalization),
      provenance = SemanticProvenance.source("lda-trace-ratio-program")
    )

  def cca[
      SourceFeature <: SemanticSpace,
      TargetFeature <: SemanticSpace,
      SourceComponent <: SemanticSpace,
      TargetComponent <: SemanticSpace,
      RC <: OperatorRoleTag,
      EC <: OperatorEvidence,
      ENS <: SpdEvidence,
      ENT <: SpdEvidence
  ](
      source: FrameParameterization[SourceFeature, SourceComponent],
      target: FrameParameterization[TargetFeature, TargetComponent],
      cross: Op[Dual[TargetFeature], Primal[SourceFeature], RC, EC],
      sourceNormalization: FrameNormalization[SourceFeature, SourceComponent, ENS],
      targetNormalization: FrameNormalization[TargetFeature, TargetComponent, ENT]
  ): Either[ProgramError, OperatorProgram] =
    paired("cca-program", source, target, cross, sourceNormalization, targetNormalization)

  def plsc[
      SourceFeature <: SemanticSpace,
      TargetFeature <: SemanticSpace,
      SourceComponent <: SemanticSpace,
      TargetComponent <: SemanticSpace,
      RC <: OperatorRoleTag,
      EC <: OperatorEvidence,
      ENS <: SpdEvidence,
      ENT <: SpdEvidence
  ](
      source: FrameParameterization[SourceFeature, SourceComponent],
      target: FrameParameterization[TargetFeature, TargetComponent],
      cross: Op[Dual[TargetFeature], Primal[SourceFeature], RC, EC],
      sourceNormalization: FrameNormalization[SourceFeature, SourceComponent, ENS],
      targetNormalization: FrameNormalization[TargetFeature, TargetComponent, ENT]
  ): Either[ProgramError, OperatorProgram] =
    paired("plsc-program", source, target, cross, sourceNormalization, targetNormalization)

  def multiset[
      Feature <: SemanticSpace,
      Component <: SemanticSpace,
      RO <: OperatorRoleTag,
      EO <: OperatorEvidence,
      EN <: SpdEvidence
  ](
      parameterization: FrameParameterization[Feature, Component],
      association: Op[Dual[Feature], Primal[Feature], RO, EO],
      normalization: FrameNormalization[Feature, Component, EN]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(parameterization),
      BaseObjective.MaximizeTrace(SelfCompressionExpression(parameterization.variable, association)),
      Vector(normalization),
      provenance = SemanticProvenance.source("multiset-program")
    )

  def reducedRankRegression[
      SourceFeature <: SemanticSpace,
      TargetFeature <: SemanticSpace,
      SourceComponent <: SemanticSpace,
      TargetComponent <: SemanticSpace,
      RC <: OperatorRoleTag,
      EC <: OperatorEvidence,
      RD <: OperatorRoleTag,
      ED <: SpdEvidence,
      ENS <: SpdEvidence,
      ENT <: SpdEvidence
  ](
      source: FrameParameterization[SourceFeature, SourceComponent],
      target: FrameParameterization[TargetFeature, TargetComponent],
      cross: Op[Dual[TargetFeature], Primal[SourceFeature], RC, EC],
      sourceDenominator: Op[Dual[SourceFeature], Primal[SourceFeature], RD, ED],
      sourceNormalization: FrameNormalization[SourceFeature, SourceComponent, ENS],
      targetNormalization: FrameNormalization[TargetFeature, TargetComponent, ENT]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(source, target),
      BaseObjective.SequentialCrossRegression(
        CrossCompressionExpression(source.variable, target.variable, cross),
        SelfCompressionExpression(source.variable, sourceDenominator)
      ),
      Vector(sourceNormalization, targetNormalization),
      provenance = SemanticProvenance.source("reduced-rank-regression-program")
    )

  private def paired[
      SourceFeature <: SemanticSpace,
      TargetFeature <: SemanticSpace,
      SourceComponent <: SemanticSpace,
      TargetComponent <: SemanticSpace,
      RC <: OperatorRoleTag,
      EC <: OperatorEvidence,
      ENS <: SpdEvidence,
      ENT <: SpdEvidence
  ](
      provenance: String,
      source: FrameParameterization[SourceFeature, SourceComponent],
      target: FrameParameterization[TargetFeature, TargetComponent],
      cross: Op[Dual[TargetFeature], Primal[SourceFeature], RC, EC],
      sourceNormalization: FrameNormalization[SourceFeature, SourceComponent, ENS],
      targetNormalization: FrameNormalization[TargetFeature, TargetComponent, ENT]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(source, target),
      BaseObjective.MaximizeCrossTrace(CrossCompressionExpression(source.variable, target.variable, cross)),
      Vector(sourceNormalization, targetNormalization),
      provenance = SemanticProvenance.source(provenance)
    )
