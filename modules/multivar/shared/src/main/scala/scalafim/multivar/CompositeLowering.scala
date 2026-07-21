package scalafim.multivar

import gale.linalg.DMat

opaque type AuxiliaryVariableId = String

object AuxiliaryVariableId:
  def apply(value: String): Either[CompositeLoweringError, AuxiliaryVariableId] =
    val clean = value.trim
    if clean.nonEmpty then Right(clean)
    else Left(CompositeLoweringError.InvalidDefinition("auxiliary variable id must be non-empty"))

  def unsafe(value: String): AuxiliaryVariableId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (value: AuxiliaryVariableId)
    inline def stringValue: String = value

enum SplitMethod:
  case PrimalDual
  case Admm
  case AugmentedLagrangian
  case Conic

enum SplitRequest:
  case Automatic
  case Require(method: SplitMethod)

final case class SplitSolverCapabilities private (methods: Set[SplitMethod]):
  def supports(method: SplitMethod): Boolean = methods.contains(method)

object SplitSolverCapabilities:
  def from(methods: Set[SplitMethod]): Either[CompositeLoweringError, SplitSolverCapabilities] =
    if methods.nonEmpty then Right(SplitSolverCapabilities(methods))
    else Left(CompositeLoweringError.InvalidDefinition("split solver capabilities must not be empty"))

  val portableReference: SplitSolverCapabilities =
    SplitSolverCapabilities(Set(SplitMethod.PrimalDual))

enum AuxiliaryEquation:
  case TargetCopy
  case LatentGroupSum(groups: ValueIdentity)

final case class AuxiliaryConstraint(
    variable: AuxiliaryVariableId,
    target: TargetExpression,
    equation: AuxiliaryEquation
):
  def rendered: String =
    equation match
      case AuxiliaryEquation.TargetCopy => s"${variable.stringValue} = T(theta)"
      case AuxiliaryEquation.LatentGroupSum(_) => s"T(theta) = sum_g E_g* ${variable.stringValue}_g"

enum CompositeFunctional:
  case ElementwiseL1
  case RowGroupL21
  case Huber(delta: Double)
  case LatentOverlappingGroups(groups: GroupStructure)

enum CompositeLoweringError:
  case InvalidDefinition(reason: String)
  case TargetMismatch(expected: ValueIdentity, actual: Vector[ValueIdentity])
  case NonlinearTarget(capability: TargetCapability)
  case FunctionalUnsupported(functional: FunctionalKind)
  case MissingSolverCapability(requested: SplitMethod, available: Set[SplitMethod])
  case GroupStructureMismatch(expected: ValueIdentity, actual: ValueIdentity)
  case OverlappingGroupsRequired(actual: GroupOverlap)
  case ReferenceMethodUnsupported(method: SplitMethod)
  case NumericalFailure(reason: String)
  case SolverBoundary(error: scalafim.linalg.FirstOrderError)
  case Chart(error: ChartError)
  case Semantic(error: SemanticError)

  def message: String =
    this match
      case InvalidDefinition(reason) => reason
      case TargetMismatch(expected, actual) =>
        s"composed target ${expected.stableKey} is absent from ${actual.map(_.stableKey).mkString(", ")}"
      case NonlinearTarget(capability) => s"composed split lowering requires a linear target, got $capability"
      case FunctionalUnsupported(functional) => s"functional $functional has no supported composite lowering"
      case MissingSolverCapability(requested, available) =>
        s"split method $requested is unavailable; capabilities are ${available.mkString(", ")}"
      case GroupStructureMismatch(expected, actual) =>
        s"group structure ${actual.stableKey} does not match requested ${expected.stableKey}"
      case OverlappingGroupsRequired(actual) => s"latent group lifting requires overlapping groups, got $actual"
      case ReferenceMethodUnsupported(method) => s"portable reference execution does not implement $method"
      case NumericalFailure(reason) => reason
      case SolverBoundary(error) => error.message
      case Chart(error) => error.message
      case Semantic(error) => error.message

/** A semantic lowering of `phi(T(theta))` to an auxiliary equation. The target
  * remains an operator expression; the selected split method is a solver
  * capability and never becomes a semantic map node.
  */
final class CompositePenaltyPlan[Source <: SemanticSpace, Target <: SemanticSpace] private (
    val original: PenaltyTerm,
    val targetOperator: Op[Dual[Source], Primal[Target], ? <: OperatorRoleTag, ? <: OperatorEvidence],
    val auxiliary: AuxiliaryConstraint,
    val functional: CompositeFunctional,
    val method: SplitMethod,
    val provenance: SemanticProvenance
)

object CompositePenaltyPlan:
  def from[
      Source <: SemanticSpace,
      Target <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      original: PenaltyTerm,
      targetOperator: Op[Dual[Source], Primal[Target], R, E],
      auxiliaryId: AuxiliaryVariableId,
      request: SplitRequest,
      capabilities: SplitSolverCapabilities,
      groups: Option[GroupStructure] = None
  ): Either[CompositeLoweringError, CompositePenaltyPlan[Source, Target]] =
    for
      _ <-
        if original.target.capability == TargetCapability.Linear then Right(())
        else Left(CompositeLoweringError.NonlinearTarget(original.target.capability))
      _ <-
        if original.target.operators.contains(targetOperator.valueIdentity) then Right(())
        else Left(CompositeLoweringError.TargetMismatch(targetOperator.valueIdentity, original.target.operators))
      functional <- compositeFunctional(original.functional, groups)
      method <- selectMethod(request, capabilities, original.functional.traits.supports(OracleCapability.Conic))
      equation = functional match
        case CompositeFunctional.LatentOverlappingGroups(structure) =>
          AuxiliaryEquation.LatentGroupSum(structure.valueIdentity)
        case _ => AuxiliaryEquation.TargetCopy
      auxiliary = AuxiliaryConstraint(auxiliaryId, original.target, equation)
      provenance = targetOperator.provenance.append(
        SemanticProvenanceEvent.Derived(
          s"composite-${method.toString.toLowerCase}-split",
          Vector(targetOperator.valueIdentity)
        )
      )
    yield new CompositePenaltyPlan(original, targetOperator, auxiliary, functional, method, provenance)

  private def compositeFunctional(
      functional: FunctionalKind,
      groups: Option[GroupStructure]
  ): Either[CompositeLoweringError, CompositeFunctional] =
    functional match
      case FunctionalKind.L1 | FunctionalKind.TotalVariation => Right(CompositeFunctional.ElementwiseL1)
      case FunctionalKind.GroupL21 => Right(CompositeFunctional.RowGroupL21)
      case FunctionalKind.Huber(delta) => Right(CompositeFunctional.Huber(delta.value))
      case FunctionalKind.GroupL2(identity) =>
        groups match
          case None => Left(CompositeLoweringError.InvalidDefinition("group functional requires its group structure"))
          case Some(structure) if structure.valueIdentity != identity =>
            Left(CompositeLoweringError.GroupStructureMismatch(identity, structure.valueIdentity))
          case Some(structure) if structure.overlap != GroupOverlap.Overlapping =>
            Left(CompositeLoweringError.OverlappingGroupsRequired(structure.overlap))
          case Some(structure) => Right(CompositeFunctional.LatentOverlappingGroups(structure))
      case other => Left(CompositeLoweringError.FunctionalUnsupported(other))

  private[multivar] def selectMethod(
      request: SplitRequest,
      capabilities: SplitSolverCapabilities,
      conicRepresentable: Boolean
  ): Either[CompositeLoweringError, SplitMethod] =
    request match
      case SplitRequest.Require(method) =>
        if capabilities.supports(method) && (method != SplitMethod.Conic || conicRepresentable) then Right(method)
        else Left(CompositeLoweringError.MissingSolverCapability(method, capabilities.methods))
      case SplitRequest.Automatic =>
        Vector(
          SplitMethod.PrimalDual,
          SplitMethod.Admm,
          SplitMethod.AugmentedLagrangian,
          SplitMethod.Conic
        ).find(method => capabilities.supports(method) && (method != SplitMethod.Conic || conicRepresentable))
          .toRight(CompositeLoweringError.InvalidDefinition("no compatible split method is available"))

final class CompositeConstraintPlan[Source <: SemanticSpace, Target <: SemanticSpace] private (
    val original: ConstraintTerm,
    val targetOperator: Op[Dual[Source], Primal[Target], ? <: OperatorRoleTag, ? <: OperatorEvidence],
    val auxiliary: AuxiliaryConstraint,
    val method: SplitMethod
)

object CompositeConstraintPlan:
  def from[
      Source <: SemanticSpace,
      Target <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      original: ConstraintTerm,
      targetOperator: Op[Dual[Source], Primal[Target], R, E],
      auxiliaryId: AuxiliaryVariableId,
      request: SplitRequest,
      capabilities: SplitSolverCapabilities
  ): Either[CompositeLoweringError, CompositeConstraintPlan[Source, Target]] =
    for
      _ <-
        if original.target.capability == TargetCapability.Linear then Right(())
        else Left(CompositeLoweringError.NonlinearTarget(original.target.capability))
      _ <-
        if original.target.operators.contains(targetOperator.valueIdentity) then Right(())
        else Left(CompositeLoweringError.TargetMismatch(targetOperator.valueIdentity, original.target.operators))
      method <- CompositePenaltyPlan.selectMethod(
        request,
        capabilities,
        original.feasibleSet.traits.supports(SetCapability.Conic)
      )
    yield new CompositeConstraintPlan(
      original,
      targetOperator,
      AuxiliaryConstraint(auxiliaryId, original.target, AuxiliaryEquation.TargetCopy),
      method
    )

/** Latent overlapping group lasso uses group-local variables and the exact
  * equation `coordinates = sum E_g* z_g`. The group prox is separable only in
  * the lifted variables, never in the original overlapping coordinates.
  */
final class OverlappingGroupLift private (
    val structure: GroupStructure,
    multiplicity: Array[Int]
):
  private val offsets: Vector[Int] =
    structure.groups.scanLeft(0)((offset, group) => offset + group.indices.length)

  val auxiliaryRows: Int = offsets.last

  val normUpperBound: Double =
    Math.sqrt(multiplicity.max.toDouble)

  def feasibleLift(coordinates: DMat): Either[CompositeLoweringError, DMat] =
    if coordinates.rows != structure.coordinateDimension then
      Left(CompositeLoweringError.InvalidDefinition("coordinates do not match the overlapping group structure"))
    else
      val output = new Array[Double](auxiliaryRows * coordinates.cols)
      var groupIndex = 0
      while groupIndex < structure.groups.length do
        val group = structure.groups(groupIndex)
        var local = 0
        while local < group.indices.length do
          val source = group.indices.indices(local)
          var column = 0
          while column < coordinates.cols do
            output((offsets(groupIndex) + local) * coordinates.cols + column) =
              coordinates(source, column) / multiplicity(source).toDouble
            column += 1
          local += 1
        groupIndex += 1
      Right(GaleNumerics.matrixFromRowMajor(auxiliaryRows, coordinates.cols, output))

  def aggregate(auxiliary: DMat): Either[CompositeLoweringError, DMat] =
    if auxiliary.rows != auxiliaryRows then
      Left(CompositeLoweringError.InvalidDefinition("auxiliary rows do not match the overlapping group lift"))
    else
      val output = new Array[Double](structure.coordinateDimension * auxiliary.cols)
      var groupIndex = 0
      while groupIndex < structure.groups.length do
        val group = structure.groups(groupIndex)
        var local = 0
        while local < group.indices.length do
          val target = group.indices.indices(local)
          var column = 0
          while column < auxiliary.cols do
            output(target * auxiliary.cols + column) += auxiliary(offsets(groupIndex) + local, column)
            column += 1
          local += 1
        groupIndex += 1
      Right(GaleNumerics.matrixFromRowMajor(structure.coordinateDimension, auxiliary.cols, output))

  def adjoint(coordinates: DMat): Either[CompositeLoweringError, DMat] =
    if coordinates.rows != structure.coordinateDimension then
      Left(CompositeLoweringError.InvalidDefinition("coordinates do not match the overlapping group structure"))
    else
      val output = new Array[Double](auxiliaryRows * coordinates.cols)
      var groupIndex = 0
      while groupIndex < structure.groups.length do
        val group = structure.groups(groupIndex)
        var local = 0
        while local < group.indices.length do
          val source = group.indices.indices(local)
          var column = 0
          while column < coordinates.cols do
            output((offsets(groupIndex) + local) * coordinates.cols + column) = coordinates(source, column)
            column += 1
          local += 1
        groupIndex += 1
      Right(GaleNumerics.matrixFromRowMajor(auxiliaryRows, coordinates.cols, output))

  def value(auxiliary: DMat): Either[CompositeLoweringError, Double] =
    if auxiliary.rows != auxiliaryRows then
      Left(CompositeLoweringError.InvalidDefinition("auxiliary rows do not match the overlapping group lift"))
    else
      var result = 0.0
      var groupIndex = 0
      while groupIndex < structure.groups.length do
        val start = offsets(groupIndex)
        val end = offsets(groupIndex + 1)
        var squared = 0.0
        var row = start
        while row < end do
          var column = 0
          while column < auxiliary.cols do
            val current = auxiliary(row, column)
            squared += current * current
            column += 1
          row += 1
        result += Math.sqrt(squared)
        groupIndex += 1
      Right(result)

  def proximal(auxiliary: DMat, threshold: Double): Either[CompositeLoweringError, DMat] =
    if auxiliary.rows != auxiliaryRows || !threshold.isFinite || threshold < 0.0 then
      Left(CompositeLoweringError.InvalidDefinition("invalid lifted group proximal input"))
    else
      val output = matrixData(auxiliary)
      var groupIndex = 0
      while groupIndex < structure.groups.length do
        val start = offsets(groupIndex)
        val end = offsets(groupIndex + 1)
        var normSquared = 0.0
        var row = start
        while row < end do
          var column = 0
          while column < auxiliary.cols do
            val current = auxiliary(row, column)
            normSquared += current * current
            column += 1
          row += 1
        val norm = Math.sqrt(normSquared)
        val factor = if norm == 0.0 then 0.0 else Math.max(0.0, 1.0 - threshold / norm)
        row = start
        while row < end do
          var column = 0
          while column < auxiliary.cols do
            output(row * auxiliary.cols + column) *= factor
            column += 1
          row += 1
        groupIndex += 1
      Right(GaleNumerics.matrixFromRowMajor(auxiliary.rows, auxiliary.cols, output))

object OverlappingGroupLift:
  def from(structure: GroupStructure): Either[CompositeLoweringError, OverlappingGroupLift] =
    if structure.overlap != GroupOverlap.Overlapping then
      Left(CompositeLoweringError.OverlappingGroupsRequired(structure.overlap))
    else
      val multiplicity = Array.fill(structure.coordinateDimension)(0)
      structure.groups.foreach: group =>
        group.indices.indices.foreach(index => multiplicity(index) += 1)
      val uncovered = multiplicity.indices.filter(index => multiplicity(index) == 0).toVector
      if uncovered.nonEmpty then
        Left(
          CompositeLoweringError.InvalidDefinition(
            s"overlapping group lift does not cover coordinates ${uncovered.mkString(", ")}"
          )
        )
      else Right(new OverlappingGroupLift(structure, multiplicity))

final class AlignedScoreTarget private (
    val expression: TypedExpression[DMat],
    val erased: TargetExpression,
    val map: TypedLinearMap[(DMat, DMat), DMat]
):
  def l1(weight: PenaltyWeight): PenaltyTerm =
    PenaltyTerm.typed(expression, TypedFunctional.l1[DMat], weight)

  def groupL21(weight: PenaltyWeight): PenaltyTerm =
    PenaltyTerm.typed(expression, TypedFunctional.groupL21[DMat], weight)

  def huber(delta: PenaltyWeight, weight: PenaltyWeight): PenaltyTerm =
    PenaltyTerm.typed(expression, TypedFunctional.huber[DMat](delta), weight)

  def bounded(radius: PenaltyWeight): ConstraintTerm =
    ConstraintTerm.typed(expression, TypedFeasibleSet.normBall[DMat](radius))

  def equality: ConstraintTerm =
    ConstraintTerm.typed(expression, TypedFeasibleSet.zero[DMat])

object AlignedScoreTarget:
  def from[
      Source <: SemanticSpace,
      Target <: SemanticSpace,
      Entity <: SemanticSpace,
      RL <: OperatorRoleTag,
      EL <: OperatorEvidence,
      RR <: OperatorRoleTag,
      ER <: OperatorEvidence
  ](
      source: ParameterId,
      target: ParameterId,
      sourceScores: Op[Dual[Source], Primal[Entity], RL, EL],
      targetScores: Op[Dual[Target], Primal[Entity], RR, ER]
  ): AlignedScoreTarget =
    val descriptor = TypedMapDescriptor(
      "aligned-score-difference",
      MapCapability.Linear,
      Vector(sourceScores.valueIdentity, targetScores.valueIdentity),
      FrameSymmetry.Orthogonal
    )
    val map = TypedLinearMap.instance[(DMat, DMat), DMat](
      descriptor,
      value =>
        MatrixOps.subtract(
          applyOrThrow(sourceScores, value._1),
          applyOrThrow(targetScores, value._2)
        ),
      cotangent =>
        (
          applyOrThrow(sourceScores.dual, cotangent),
          MatrixOps.scale(applyOrThrow(targetScores.dual, cotangent), -1.0)
        )
    )
    val expression =
      ParameterExpression[DMat](source, "functional-frame")
        .product(ParameterExpression[DMat](target, "functional-frame"))
        .through(map)
    new AlignedScoreTarget(expression, TargetExpression.typed(expression), map)

  private def applyOrThrow[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](operator: Op[From, To, R, E], value: DMat): DMat =
    operator(value).fold(error => throw new IllegalArgumentException(error.message), identity)

opaque type IterationBudget = Int

object IterationBudget:
  def apply(value: Int): Either[CompositeLoweringError, IterationBudget] =
    if value > 0 then Right(value)
    else Left(CompositeLoweringError.InvalidDefinition("iteration budget must be positive"))

  def unsafe(value: Int): IterationBudget =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (value: IterationBudget)
    inline def intValue: Int = value

final case class PrimalDualConfig(
    iterations: IterationBudget,
    tolerance: CertificateTolerance,
    extrapolation: UnitFraction
)

object PrimalDualConfig:
  val portable: PrimalDualConfig =
    PrimalDualConfig(IterationBudget.unsafe(10000), CertificateTolerance.strict, UnitFraction.unsafe(1.0))

final case class SplitResidualCertificate(
    stationarity: Double,
    dualFeasibility: Double,
    complementarity: Double,
    primalDualGap: Double,
    iterations: Int,
    tolerance: CertificateTolerance
):
  require(stationarity.isFinite && stationarity >= 0.0, "stationarity residual must be finite and non-negative")
  require(dualFeasibility.isFinite && dualFeasibility >= 0.0, "dual residual must be finite and non-negative")
  require(complementarity.isFinite && complementarity >= 0.0, "complementarity residual must be finite and non-negative")
  require(primalDualGap.isFinite && primalDualGap >= 0.0, "duality gap must be finite and non-negative")

final case class InfeasibilityCertificate(
    constraints: Vector[String],
    reason: String,
    witness: Option[Double]
):
  require(constraints.nonEmpty, "infeasibility certificate requires constraints")
  require(reason.trim.nonEmpty, "infeasibility reason must be non-empty")

enum SplitStoppingStatus:
  case Converged(certificate: SplitResidualCertificate)
  case IterationLimit(certificate: SplitResidualCertificate)
  case Infeasible(certificate: InfeasibilityCertificate)
  case NumericalFailure(reason: String)

final case class PrimalDualSolution(
    parameter: DMat,
    auxiliary: DMat,
    dual: DMat,
    objective: Double,
    status: SplitStoppingStatus,
    numericalCertificate: scalafim.linalg.FirstOrderCertificate
)

/** Portable reference solver for
  * `min_x 0.5 ||x-y||_F^2 + lambda ||T x||_1`.
  *
  * The primal-dual step uses the Frobenius norm as a certified upper bound for
  * `||T||_2`, so the step-size product is valid on JVM and Scala.js without a
  * platform solver dependency.
  */
object PrimalDualL1Reference:
  def solve[Source <: SemanticSpace, Target <: SemanticSpace](
      plan: CompositePenaltyPlan[Source, Target],
      observation: DMat,
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, PrimalDualSolution] =
    VariationalSolverCompiler
      .compileL1(plan, observation)
      .flatMap(_.solve(config))

object ConstraintFeasibility:
  def intersectScalarBoxes(
      constraints: Vector[(String, ClosedInterval)]
  ): Either[SplitStoppingStatus.Infeasible, ClosedInterval] =
    if constraints.isEmpty then
      Left(
        SplitStoppingStatus.Infeasible(
          InfeasibilityCertificate(Vector("empty-system"), "no scalar constraints were supplied", None)
        )
      )
    else
      val lower = constraints.map(_._2.lower).max
      val upper = constraints.map(_._2.upper).min
      if lower <= upper then
        ClosedInterval.from(lower, upper).left.map: error =>
          SplitStoppingStatus.Infeasible(
            InfeasibilityCertificate(constraints.map(_._1), error.message, Some(lower - upper))
          )
      else
        Left(
          SplitStoppingStatus.Infeasible(
            InfeasibilityCertificate(
              constraints.map(_._1),
              s"scalar bounds have empty intersection [$lower, $upper]",
              Some(lower - upper)
            )
          )
        )

private def matrixData(value: DMat): Array[Double] =
  val output = new Array[Double](value.rows * value.cols)
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      output(row * value.cols + column) = value(row, column)
      column += 1
    row += 1
  output
