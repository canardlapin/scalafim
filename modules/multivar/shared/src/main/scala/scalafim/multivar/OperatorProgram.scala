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

private final case class ObjectiveBinding(parameter: ParameterId, componentSpace: MvSpace)

/** Closed catalog of objectives that lower to the common operator core. */
enum BaseObjective:
  case MaximizeTrace[K <: SemanticSpace, E <: OperatorEvidence](
      parameter: ParameterId,
      operator: Op[Primal[K], Dual[K], ComponentOperatorRole, E]
  )
  case MaximizeCrossTrace[Source <: SemanticSpace, Target <: SemanticSpace, E <: OperatorEvidence](
      source: ParameterId,
      target: ParameterId,
      operator: Op[Primal[Target], Dual[Source], ComponentOperatorRole, E]
  )
  case GeneralizedRayleigh[K <: SemanticSpace, EN <: OperatorEvidence, ED <: SpdEvidence](
      parameter: ParameterId,
      numerator: Op[Primal[K], Dual[K], ComponentOperatorRole, EN],
      denominator: Op[Primal[K], Dual[K], ComponentOperatorRole, ED]
  )
  case TraceRatio[K <: SemanticSpace, EN <: OperatorEvidence, ED <: SpdEvidence](
      parameter: ParameterId,
      numerator: Op[Primal[K], Dual[K], ComponentOperatorRole, EN],
      denominator: Op[Primal[K], Dual[K], ComponentOperatorRole, ED]
  )
  case RatioTrace[K <: SemanticSpace, EN <: OperatorEvidence, ED <: SpdEvidence](
      parameter: ParameterId,
      numerator: Op[Primal[K], Dual[K], ComponentOperatorRole, EN],
      denominator: Op[Primal[K], Dual[K], ComponentOperatorRole, ED]
  )
  case MinimizeDisagreement[K <: SemanticSpace, E <: OperatorEvidence](
      parameter: ParameterId,
      operator: Op[Primal[K], Dual[K], ComponentOperatorRole, E]
  )
  case SequentialCrossRegression[Source <: SemanticSpace, Target <: SemanticSpace, EC <: OperatorEvidence, EP <: SpdEvidence](
      source: ParameterId,
      target: ParameterId,
      cross: Op[Primal[Target], Dual[Source], ComponentOperatorRole, EC],
      predictor: Op[Primal[Source], Dual[Source], ComponentOperatorRole, EP]
  )

  def label: String =
    this match
      case MaximizeTrace(_, _) => "maximize-trace"
      case MaximizeCrossTrace(_, _, _) => "maximize-cross-trace"
      case GeneralizedRayleigh(_, _, _) => "generalized-rayleigh"
      case TraceRatio(_, _, _) => "trace-ratio"
      case RatioTrace(_, _, _) => "ratio-trace"
      case MinimizeDisagreement(_, _) => "minimize-disagreement"
      case SequentialCrossRegression(_, _, _, _) => "sequential-cross-regression"

  private[multivar] def bindings: Vector[ObjectiveBinding] =
    this match
      case MaximizeTrace(parameter, operator) =>
        Vector(ObjectiveBinding(parameter, operator.domain.descriptor.space))
      case MaximizeCrossTrace(source, target, operator) =>
        Vector(
          ObjectiveBinding(source, operator.codomain.descriptor.space),
          ObjectiveBinding(target, operator.domain.descriptor.space)
        )
      case GeneralizedRayleigh(parameter, numerator, _) =>
        Vector(ObjectiveBinding(parameter, numerator.domain.descriptor.space))
      case TraceRatio(parameter, numerator, _) =>
        Vector(ObjectiveBinding(parameter, numerator.domain.descriptor.space))
      case RatioTrace(parameter, numerator, _) =>
        Vector(ObjectiveBinding(parameter, numerator.domain.descriptor.space))
      case MinimizeDisagreement(parameter, operator) =>
        Vector(ObjectiveBinding(parameter, operator.domain.descriptor.space))
      case SequentialCrossRegression(source, target, cross, _) =>
        Vector(
          ObjectiveBinding(source, cross.codomain.descriptor.space),
          ObjectiveBinding(target, cross.domain.descriptor.space)
        )

enum TargetCapability:
  case Linear
  case Affine
  case Smooth
  case General

final case class TargetExpression private (
    parameter: ParameterId,
    capability: TargetCapability,
    operation: String,
    operator: Option[ValueIdentity]
)

object TargetExpression:
  def frame(parameter: ParameterId): TargetExpression =
    TargetExpression(parameter, TargetCapability.Linear, "frame", None)

  def linear[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag, E <: OperatorEvidence](
      parameter: ParameterId,
      operation: String,
      operator: Op[From, To, R, E]
  ): Either[ProgramError, TargetExpression] =
    val clean = operation.trim
    if clean.isEmpty then Left(ProgramError.InvalidParameterization("target operation must be non-empty"))
    else Right(TargetExpression(parameter, TargetCapability.Linear, clean, Some(operator.valueIdentity)))

  def affine(parameter: ParameterId, operation: String): Either[ProgramError, TargetExpression] =
    named(parameter, TargetCapability.Affine, operation)

  def smooth(parameter: ParameterId, operation: String): Either[ProgramError, TargetExpression] =
    named(parameter, TargetCapability.Smooth, operation)

  def general(parameter: ParameterId, operation: String): Either[ProgramError, TargetExpression] =
    named(parameter, TargetCapability.General, operation)

  private def named(
      parameter: ParameterId,
      capability: TargetCapability,
      operation: String
  ): Either[ProgramError, TargetExpression] =
    val clean = operation.trim
    if clean.isEmpty then Left(ProgramError.InvalidParameterization("target operation must be non-empty"))
    else Right(TargetExpression(parameter, capability, clean, None))

enum FunctionalKind:
  case SquaredNorm(geometry: ValueIdentity)
  case L1
  case GroupL21
  case ElasticNet(l1Fraction: UnitFraction)
  case Huber(delta: PenaltyWeight)
  case TotalVariation
  case NuclearNorm
  case NegativeLogDet

  def symmetry: FrameSymmetry =
    this match
      case SquaredNorm(_) | GroupL21 | NuclearNorm | NegativeLogDet => FrameSymmetry.Orthogonal
      case L1 | ElasticNet(_) | Huber(_) | TotalVariation => FrameSymmetry.SignedPermutation

enum FeasibleSetKind:
  case ZeroSubspace
  case NonnegativeOrthant
  case Simplex
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
      case NonnegativeOrthant | Simplex | Box(_) =>
        FrameSymmetry.Permutation

enum FrameSymmetry:
  case Orthogonal
  case SignedPermutation
  case Permutation
  case Identity

final case class PenaltyTerm(
    target: TargetExpression,
    functional: FunctionalKind,
    weight: PenaltyWeight
):
  def symmetry: FrameSymmetry = functional.symmetry

final case class ConstraintTerm(
    target: TargetExpression,
    feasibleSet: FeasibleSetKind
):
  def symmetry: FrameSymmetry = feasibleSet.symmetry

final case class FrameNormalization[Feature <: SemanticSpace, Component <: SemanticSpace, E <: SpdEvidence] private (
    parameter: FrameVariable[Feature, Component],
    geometry: OpMetric[Feature, E]
)

object FrameNormalization:
  def apply[Feature <: SemanticSpace, Component <: SemanticSpace, E <: SpdEvidence](
      parameter: FrameVariable[Feature, Component],
      geometry: OpMetric[Feature, E]
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

final case class ResultSemantics(
    equivalence: ResultEquivalence,
    representative: RepresentativeRule,
    guarantee: SolverGuarantee
)

object ResultSemantics:
  private[multivar] def infer(
      objective: BaseObjective,
      penalties: Vector[PenaltyTerm],
      constraints: Vector[ConstraintTerm]
  ): ResultSemantics =
    val symmetry = (penalties.map(_.symmetry) ++ constraints.map(_.symmetry)).foldLeft(FrameSymmetry.Orthogonal)(meet)
    val smoothSpectral = penalties.isEmpty && constraints.isEmpty
    objective match
      case BaseObjective.SequentialCrossRegression(_, _, _, _) =>
        ResultSemantics(
          ResultEquivalence.PredictionEquivalent(PredictionMetric.SquaredError, CertificateTolerance.strict),
          RepresentativeRule.PredictionMap,
          if smoothSpectral then SolverGuarantee.GlobalSpectralOptimum else SolverGuarantee.StationaryPoint
        )
      case BaseObjective.MaximizeCrossTrace(_, _, _) =>
        ResultSemantics(
          ResultEquivalence.FrameEquivalent(symmetry, CertificateTolerance.strict),
          RepresentativeRule.OrderedSpectrumThenSign,
          if smoothSpectral then SolverGuarantee.GlobalSpectralOptimum else SolverGuarantee.StationaryPoint
        )
      case _ =>
        val equivalence =
          if symmetry == FrameSymmetry.Orthogonal then
            ResultEquivalence.SubspaceEquivalent(CertificateTolerance.strict, CertificateTolerance.strict)
          else ResultEquivalence.FrameEquivalent(symmetry, CertificateTolerance.strict)
        ResultSemantics(
          equivalence,
          RepresentativeRule.OrderedSpectrumThenSign,
          if smoothSpectral then SolverGuarantee.GlobalSpectralOptimum else SolverGuarantee.StationaryPoint
        )

  private def meet(left: FrameSymmetry, right: FrameSymmetry): FrameSymmetry =
    (left, right) match
      case (FrameSymmetry.Identity, _) | (_, FrameSymmetry.Identity) => FrameSymmetry.Identity
      case (FrameSymmetry.Permutation, _) | (_, FrameSymmetry.Permutation) => FrameSymmetry.Permutation
      case (FrameSymmetry.SignedPermutation, _) | (_, FrameSymmetry.SignedPermutation) => FrameSymmetry.SignedPermutation
      case _ => FrameSymmetry.Orthogonal

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
          ResultSemantics.infer(objective, penalties, constraints),
          provenance
        )

  private def validateObjective(
      parameters: Vector[FrameParameterization[? <: SemanticSpace, ? <: SemanticSpace]],
      objective: BaseObjective
  ): Either[ProgramError, Unit] =
    val bindings = objective.bindings
    if bindings.length > 1 && bindings.map(_.parameter).distinct.length != bindings.length then
      Left(ProgramError.InvalidParameterization(s"${objective.label} requires distinct frame parameters"))
    else
      bindings.foldLeft[Either[ProgramError, Unit]](Right(())): (result, binding) =>
        result.flatMap: _ =>
          parameters.find(_.variable.id == binding.parameter) match
            case None => Left(ProgramError.UnknownParameter(binding.parameter))
            case Some(parameter) =>
              val actual = parameter.variable.componentSpace.descriptor
              if actual != binding.componentSpace then
                Left(ProgramError.ComponentSpaceMismatch(binding.parameter, binding.componentSpace, actual))
              else Right(())

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
    targets.find(target => !ids.contains(target.parameter)) match
      case Some(target) => Left(ProgramError.UnknownParameter(target.parameter))
      case None => Right(())

  private def firstDuplicate(ids: Vector[ParameterId]): ParameterId =
    ids.find(id => ids.count(_ == id) > 1).get

final case class NumericalIdentifiability(
    retainedRank: Int,
    spectralClusters: Vector[Vector[Int]],
    residual: Double,
    context: CertificateContext
)

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
    else Right(OperatorProgramFit(program, frames, objectiveValue, identifiability, provenance))

/** Named method constructors assemble the shared program vocabulary; they do
  * not introduce method-specific solver or matrix representations.
  */
object OperatorPrograms:
  def gpca[Feature <: SemanticSpace, Component <: SemanticSpace, EO <: OperatorEvidence, EN <: SpdEvidence](
      parameterization: FrameParameterization[Feature, Component],
      covariance: Op[Primal[Component], Dual[Component], ComponentOperatorRole, EO],
      normalization: FrameNormalization[Feature, Component, EN]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(parameterization),
      BaseObjective.MaximizeTrace(parameterization.variable.id, covariance),
      Vector(normalization),
      provenance = SemanticProvenance.source("gpca-program")
    )

  def ldaRayleigh[Feature <: SemanticSpace, Component <: SemanticSpace, EB <: OperatorEvidence, EW <: SpdEvidence, EN <: SpdEvidence](
      parameterization: FrameParameterization[Feature, Component],
      between: Op[Primal[Component], Dual[Component], ComponentOperatorRole, EB],
      within: Op[Primal[Component], Dual[Component], ComponentOperatorRole, EW],
      normalization: FrameNormalization[Feature, Component, EN]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(parameterization),
      BaseObjective.GeneralizedRayleigh(parameterization.variable.id, between, within),
      Vector(normalization),
      provenance = SemanticProvenance.source("lda-rayleigh-program")
    )

  def ldaTraceRatio[Feature <: SemanticSpace, Component <: SemanticSpace, EB <: OperatorEvidence, EW <: SpdEvidence, EN <: SpdEvidence](
      parameterization: FrameParameterization[Feature, Component],
      between: Op[Primal[Component], Dual[Component], ComponentOperatorRole, EB],
      within: Op[Primal[Component], Dual[Component], ComponentOperatorRole, EW],
      normalization: FrameNormalization[Feature, Component, EN]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(parameterization),
      BaseObjective.TraceRatio(parameterization.variable.id, between, within),
      Vector(normalization),
      provenance = SemanticProvenance.source("lda-trace-ratio-program")
    )

  def cca[
      SourceFeature <: SemanticSpace,
      TargetFeature <: SemanticSpace,
      SourceComponent <: SemanticSpace,
      TargetComponent <: SemanticSpace,
      EC <: OperatorEvidence,
      ENS <: SpdEvidence,
      ENT <: SpdEvidence
  ](
      source: FrameParameterization[SourceFeature, SourceComponent],
      target: FrameParameterization[TargetFeature, TargetComponent],
      cross: Op[Primal[TargetComponent], Dual[SourceComponent], ComponentOperatorRole, EC],
      sourceNormalization: FrameNormalization[SourceFeature, SourceComponent, ENS],
      targetNormalization: FrameNormalization[TargetFeature, TargetComponent, ENT]
  ): Either[ProgramError, OperatorProgram] =
    paired("cca-program", source, target, cross, sourceNormalization, targetNormalization)

  def plsc[
      SourceFeature <: SemanticSpace,
      TargetFeature <: SemanticSpace,
      SourceComponent <: SemanticSpace,
      TargetComponent <: SemanticSpace,
      EC <: OperatorEvidence,
      ENS <: SpdEvidence,
      ENT <: SpdEvidence
  ](
      source: FrameParameterization[SourceFeature, SourceComponent],
      target: FrameParameterization[TargetFeature, TargetComponent],
      cross: Op[Primal[TargetComponent], Dual[SourceComponent], ComponentOperatorRole, EC],
      sourceNormalization: FrameNormalization[SourceFeature, SourceComponent, ENS],
      targetNormalization: FrameNormalization[TargetFeature, TargetComponent, ENT]
  ): Either[ProgramError, OperatorProgram] =
    paired("plsc-program", source, target, cross, sourceNormalization, targetNormalization)

  def multiset[Feature <: SemanticSpace, Component <: SemanticSpace, EO <: OperatorEvidence, EN <: SpdEvidence](
      parameterization: FrameParameterization[Feature, Component],
      association: Op[Primal[Component], Dual[Component], ComponentOperatorRole, EO],
      normalization: FrameNormalization[Feature, Component, EN]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(parameterization),
      BaseObjective.MaximizeTrace(parameterization.variable.id, association),
      Vector(normalization),
      provenance = SemanticProvenance.source("multiset-program")
    )

  private def paired[
      SourceFeature <: SemanticSpace,
      TargetFeature <: SemanticSpace,
      SourceComponent <: SemanticSpace,
      TargetComponent <: SemanticSpace,
      EC <: OperatorEvidence,
      ENS <: SpdEvidence,
      ENT <: SpdEvidence
  ](
      provenance: String,
      source: FrameParameterization[SourceFeature, SourceComponent],
      target: FrameParameterization[TargetFeature, TargetComponent],
      cross: Op[Primal[TargetComponent], Dual[SourceComponent], ComponentOperatorRole, EC],
      sourceNormalization: FrameNormalization[SourceFeature, SourceComponent, ENS],
      targetNormalization: FrameNormalization[TargetFeature, TargetComponent, ENT]
  ): Either[ProgramError, OperatorProgram] =
    OperatorProgram.from(
      Vector(source, target),
      BaseObjective.MaximizeCrossTrace(source.variable.id, target.variable.id, cross),
      Vector(sourceNormalization, targetNormalization),
      provenance = SemanticProvenance.source(provenance)
    )
