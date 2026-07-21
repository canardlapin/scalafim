package scalafim.multivar.ir

import scalafim.multivar.*

final case class ProgramSpaceIr(id: String, role: String, dimension: Int)

final case class ProgramParameterIr(
    id: String,
    featureSpace: ProgramSpaceIr,
    componentSpace: ProgramSpaceIr,
    parameterization: String,
    operatorIdentities: Vector[String]
)

final case class ProgramNormalizationIr(parameterId: String, operatorIdentity: String)

final case class ProgramPenaltyIr(
    parameterId: String,
    target: String,
    capability: String,
    functional: String,
    weight: Double,
    symmetry: String
)

final case class ProgramConstraintIr(
    parameterId: String,
    target: String,
    capability: String,
    feasibleSet: String,
    symmetry: String
)

final case class ProgramResultIr(
    equivalence: String,
    representative: String,
    guarantee: String
)

/** Descriptor-only wire seam for the universal program. Numeric payloads stay
  * in the existing operator records and are referenced by stable identity.
  */
final case class OperatorProgramIr(
    schema: String,
    parameters: Vector[ProgramParameterIr],
    objective: String,
    normalizations: Vector[ProgramNormalizationIr],
    penalties: Vector[ProgramPenaltyIr],
    constraints: Vector[ProgramConstraintIr],
    result: ProgramResultIr
)

object OperatorProgramIr:
  val schemaV01: String = "scalafim-operator-program-ir/0.1"

  def from(program: OperatorProgram): OperatorProgramIr =
    val descriptor = program.descriptor
    OperatorProgramIr(
      schemaV01,
      descriptor.parameters.map: (id, feature, component, parameterization) =>
        val (kind, identities) = parameterizationIr(parameterization)
        ProgramParameterIr(id, space(feature), space(component), kind, identities)
      ,
      descriptor.objective,
      descriptor.normalizations.map((parameter, operator) => ProgramNormalizationIr(parameter, operator.stableKey)),
      descriptor.penalties.map: term =>
        ProgramPenaltyIr(
          term.target.parameter.value,
          term.target.operation,
          capability(term.target.capability),
          functional(term.functional),
          term.weight.value,
          symmetry(term.symmetry)
        )
      ,
      descriptor.constraints.map: term =>
        ProgramConstraintIr(
          term.target.parameter.value,
          term.target.operation,
          capability(term.target.capability),
          feasibleSet(term.feasibleSet),
          symmetry(term.symmetry)
        )
      ,
      result(descriptor.result)
    )

  private def space(value: MvSpace): ProgramSpaceIr =
    ProgramSpaceIr(value.id.value, value.role.label, value.size)

  private def parameterizationIr(value: ParameterizationKind): (String, Vector[String]) =
    value match
      case ParameterizationKind.Identity => "identity" -> Vector.empty
      case ParameterizationKind.KnownSupport(embedding, injective) =>
        (if injective then "known-support-injective" else "known-support") -> Vector(embedding.stableKey)
      case ParameterizationKind.FixedRank(rank, gauge) =>
        s"fixed-rank:${rank.value}:${gaugeTag(gauge)}" -> Vector.empty
      case ParameterizationKind.BlockDiagonal(blocks) =>
        s"block-diagonal:${blocks.map(_.value).mkString(",")}" -> Vector.empty
      case ParameterizationKind.NullSpace(basis, tolerance) =>
        s"null-space:${tolerance.absolute}:${tolerance.relative}" -> Vector(basis.stableKey)

  private def capability(value: TargetCapability): String =
    value match
      case TargetCapability.Linear => "linear"
      case TargetCapability.Affine => "affine"
      case TargetCapability.Smooth => "smooth"
      case TargetCapability.General => "general"

  private def functional(value: FunctionalKind): String =
    value match
      case FunctionalKind.SquaredNorm(geometry) => s"squared-norm:${geometry.stableKey}"
      case FunctionalKind.L1 => "l1"
      case FunctionalKind.GroupL21 => "group-l21"
      case FunctionalKind.GroupL2(groups) => s"group-l2:${groups.stableKey}"
      case FunctionalKind.SparseGroup(fraction, groups) => s"sparse-group:${fraction.value}:${groups.stableKey}"
      case FunctionalKind.ElasticNet(fraction) => s"elastic-net:${fraction.value}"
      case FunctionalKind.Huber(delta) => s"huber:${delta.value}"
      case FunctionalKind.TotalVariation => "total-variation"
      case FunctionalKind.NuclearNorm => "nuclear-norm"
      case FunctionalKind.NegativeLogDet => "negative-log-det"

  private def feasibleSet(value: FeasibleSetKind): String =
    value match
      case FeasibleSetKind.ZeroSubspace => "zero-subspace"
      case FeasibleSetKind.NonnegativeOrthant => "nonnegative-orthant"
      case FeasibleSetKind.Simplex => "simplex"
      case FeasibleSetKind.Monotone(order) => s"monotone:${order.stableKey}"
      case FeasibleSetKind.Box(bounds) => s"box:${bounds.lower}:${bounds.upper}"
      case FeasibleSetKind.NormBall(radius) => s"norm-ball:${radius.value}"
      case FeasibleSetKind.PsdCone => "psd-cone"
      case FeasibleSetKind.Stiefel => "stiefel"
      case FeasibleSetKind.FixedSupport(indices) => s"fixed-support:${indices.indices.mkString(",")}"
      case FeasibleSetKind.RankBounded(rank) => s"rank-bounded:${rank.value}"

  private def result(value: ResultSemantics): ProgramResultIr =
    ProgramResultIr(equivalence(value.equivalence), representative(value.representative), guarantee(value.guarantee))

  private def equivalence(value: ResultEquivalence): String =
    value match
      case ResultEquivalence.ValueEquivalent(_) => "value"
      case ResultEquivalence.OperatorEquivalent(_, _, _) => "operator"
      case ResultEquivalence.SubspaceEquivalent(_, _) => "subspace"
      case ResultEquivalence.FrameEquivalent(group, _) => s"frame:${symmetry(group)}"
      case ResultEquivalence.PredictionEquivalent(metric, _) => s"prediction:${predictionMetric(metric)}"
      case ResultEquivalence.ObjectiveEquivalent(_) => "objective"

  private def representative(value: RepresentativeRule): String =
    value match
      case RepresentativeRule.DeterministicSign => "deterministic-sign"
      case RepresentativeRule.OrderedSpectrumThenSign => "ordered-spectrum-then-sign"
      case RepresentativeRule.ProcrustesToReference(reference) => s"procrustes:${reference.stableKey}"
      case RepresentativeRule.PredictionMap => "prediction-map"
      case RepresentativeRule.ObjectiveValueOnly => "objective-value-only"

  private def guarantee(value: SolverGuarantee): String =
    value match
      case SolverGuarantee.GlobalSpectralOptimum => "global-spectral-optimum"
      case SolverGuarantee.GlobalConvexOptimum => "global-convex-optimum"
      case SolverGuarantee.StationaryPoint => "stationary-point"
      case SolverGuarantee.FeasiblePoint => "feasible-point"

  private def symmetry(value: FrameSymmetry): String =
    value match
      case FrameSymmetry.Orthogonal => "orthogonal"
      case FrameSymmetry.SignedPermutation => "signed-permutation"
      case FrameSymmetry.Permutation => "permutation"
      case FrameSymmetry.Identity => "identity"

  private def gaugeTag(value: ParameterizationGauge): String =
    value match
      case ParameterizationGauge.Unique => "unique"
      case ParameterizationGauge.SignedPermutation => "signed-permutation"
      case ParameterizationGauge.Orthogonal => "orthogonal"
      case ParameterizationGauge.GeneralLinear => "general-linear"

  private def predictionMetric(value: PredictionMetric): String =
    value match
      case PredictionMetric.SquaredError => "squared-error"
      case PredictionMetric.Correlation => "correlation"
      case PredictionMetric.Mahalanobis(metric) => s"mahalanobis:${metric.stableKey}"

object OperatorProgramIrCodec:
  import IrJson.*

  def encode(value: OperatorProgramIr): String =
    IrJson.render(
      obj(
        "schema" -> Str(value.schema),
        "parameters" -> arr(value.parameters.map(parameter)),
        "objective" -> Str(value.objective),
        "normalizations" -> arr(value.normalizations.map(normalization)),
        "penalties" -> arr(value.penalties.map(penalty)),
        "constraints" -> arr(value.constraints.map(constraint)),
        "result" -> result(value.result)
      )
    )

  private def parameter(value: ProgramParameterIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "feature_space" -> space(value.featureSpace),
      "component_space" -> space(value.componentSpace),
      "parameterization" -> Str(value.parameterization),
      "operator_identities" -> arr(value.operatorIdentities.map(Str.apply))
    )

  private def space(value: ProgramSpaceIr): IrJson =
    obj("id" -> Str(value.id), "role" -> Str(value.role), "dimension" -> Num(value.dimension.toDouble))

  private def normalization(value: ProgramNormalizationIr): IrJson =
    obj("parameter_id" -> Str(value.parameterId), "operator_identity" -> Str(value.operatorIdentity))

  private def penalty(value: ProgramPenaltyIr): IrJson =
    obj(
      "parameter_id" -> Str(value.parameterId),
      "target" -> Str(value.target),
      "capability" -> Str(value.capability),
      "functional" -> Str(value.functional),
      "weight" -> Num(value.weight),
      "symmetry" -> Str(value.symmetry)
    )

  private def constraint(value: ProgramConstraintIr): IrJson =
    obj(
      "parameter_id" -> Str(value.parameterId),
      "target" -> Str(value.target),
      "capability" -> Str(value.capability),
      "feasible_set" -> Str(value.feasibleSet),
      "symmetry" -> Str(value.symmetry)
    )

  private def result(value: ProgramResultIr): IrJson =
    obj(
      "equivalence" -> Str(value.equivalence),
      "representative" -> Str(value.representative),
      "guarantee" -> Str(value.guarantee)
    )

  private def obj(fields: (String, IrJson)*): IrJson =
    Obj(fields.toVector)

  private def arr(values: Vector[IrJson]): IrJson =
    Arr(values)
