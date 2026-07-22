package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

enum SparseFunctionalFactorizationError:
  case InvalidDefinition(detail: String)
  case Program(error: ProgramError)
  case Multivar(error: MultivarError)
  case Semantic(error: SemanticError)
  case Quadratic(error: QuadraticLoweringError)
  case Composite(error: CompositePenaltyCompileError)
  case Guarantee(error: OptimizationGuaranteeError)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case Program(error) => error.message
      case Multivar(error) => error.message
      case Semantic(error) => error.message
      case Quadratic(error) => error.message
      case Composite(error) => error.message
      case Guarantee(error) => error.message

enum FactorGeometryBase:
  case Euclidean
  case Generalized

final case class FactorSmoothness(
    family: QuadraticFamily,
    operator: ValueIdentity,
    weight: PenaltyWeight
)

/** Certified factor norm used in both the objective pairing and unit-ball
  * constraint of GMD/GPMF. Quadratic smoothness is part of this norm, not a
  * post-hoc shrinkage of a fitted loading.
  */
final class StructuredFactorGeometry[Space <: SemanticSpace] private (
    val operator: OpMetric[Space, CertifiedSpd],
    val base: FactorGeometryBase,
    val smoothness: Vector[FactorSmoothness],
    val provenance: SemanticProvenance
)

object StructuredFactorGeometry:
  def euclidean[Space <: SemanticSpace](
      space: SpaceEvidence[Space],
      identity: ValueIdentity
  ): Either[SparseFunctionalFactorizationError, StructuredFactorGeometry[Space]] =
    val value = DMat.eye(space.dimension)
    for
      unchecked <- Op
        .fromDense(
          value,
          CoordinateEvidence.primal(space),
          CoordinateEvidence.dual(space),
          OperatorRoleWitness.metric,
          identity,
          SemanticProvenance.source("euclidean-factor-geometry")
        )
        .left
        .map(SparseFunctionalFactorizationError.Semantic.apply)
      linear <- Lin
        .fromDenseMatrix(
          value,
          CoordinateEvidence.primal(space),
          CoordinateEvidence.dual(space),
          identity
        )
        .left
        .map(SparseFunctionalFactorizationError.Semantic.apply)
      certificate <- FormCertificates
        .spd(linear)
        .left
        .map(SparseFunctionalFactorizationError.Semantic.apply)
      operator <- Op
        .certifiedSpd(unchecked, certificate)
        .left
        .map(SparseFunctionalFactorizationError.Semantic.apply)
    yield
      new StructuredFactorGeometry(
        operator,
        FactorGeometryBase.Euclidean,
        Vector.empty,
        operator.provenance
      )

  def generalized[Space <: SemanticSpace](
      operator: OpMetric[Space, CertifiedSpd]
  ): StructuredFactorGeometry[Space] =
    new StructuredFactorGeometry(
      operator,
      FactorGeometryBase.Generalized,
      Vector.empty,
      operator.provenance
    )

  def addSmoothness[Space <: SemanticSpace](
      geometry: StructuredFactorGeometry[Space],
      lowering: QuadraticLowering[Space]
  ): Either[SparseFunctionalFactorizationError, StructuredFactorGeometry[Space]] =
    if lowering.placement != QuadraticPlacement.ObjectiveRidge then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          "factor smoothness must be an objective quadratic before it is folded into the normalization geometry"
        )
      )
    else if lowering.pulledBack.rows != geometry.operator.rows then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          "factor smoothness and factor geometry dimensions do not agree"
        )
      )
    else
      val identity = ValueIdentity.derived(
        "structured-factor-smoothness",
        geometry.operator.valueIdentity,
        lowering.pulledBack.valueIdentity
      )
      val raw = new Op[Primal[Space], Dual[Space], MetricOperatorRole, UncheckedEvidence](
        WeightedSumKernel(
          geometry.operator.kernel,
          lowering.pulledBack.kernel,
          1.0,
          lowering.original.weight.value
        ),
        geometry.operator.domain,
        geometry.operator.codomain,
        OperatorRoleWitness.metric,
        OperatorCertificate.unchecked(identity),
        identity,
        (geometry.provenance ++ lowering.pulledBack.provenance).append(
          SemanticProvenanceEvent.Derived(
            "add-factor-smoothness",
            Vector(geometry.operator.valueIdentity, lowering.pulledBack.valueIdentity)
          )
        )
      )
      for
        evidence <- spdEvidence(geometry.operator, identity)
        certified <- Op
          .certifiedSpd(raw, evidence)
          .left
          .map(SparseFunctionalFactorizationError.Semantic.apply)
      yield
        new StructuredFactorGeometry(
          certified,
          geometry.base,
          geometry.smoothness :+ FactorSmoothness(
            lowering.family,
            lowering.pulledBack.valueIdentity,
            lowering.original.weight
          ),
          certified.provenance
        )

  private def spdEvidence[Space <: SemanticSpace](
      base: OpMetric[Space, CertifiedSpd],
      identity: ValueIdentity
  ): Either[SparseFunctionalFactorizationError, Certificate[SpdProperty]] =
    base.certificate.claims.collectFirst:
      case NumericalCertificate(_, CertificateClaim.PositiveDefinite(minimum, _, scale), context)
          if minimum > 0.0 =>
        Certificate.unsafe[SpdProperty](
          identity,
          CertificateClaim.PositiveDefinite(minimum, 0.0, scale),
          context
        )
    .toRight(
      SparseFunctionalFactorizationError.InvalidDefinition(
        "base factor geometry has no positive-definite numerical certificate"
      )
    )

/** One-homogeneous nonsmooth terms admitted by the Allen GPMF normalization
  * theorem. Non-homogeneous penalties must use a different constrained block
  * solver and are rejected here.
  */
final class HomogeneousFactorPenalties[Space <: SemanticSpace] private (
    val parameter: ParameterId,
    val direct: Vector[ExactDirectPenalty[Space]],
    val split: Vector[SplitL1Penalty[Space]]
):
  def identities: Vector[ValueIdentity] =
    direct.map(_.identity) ++ split.map(_.identity)

  private[multivar] def value(at: DMat): Either[SparseFunctionalFactorizationError, Double] =
    for
      directValues <- sfTraverse(direct): term =>
        term.value(at).left.map(SparseFunctionalFactorizationError.Composite.apply)
      splitValues <- sfTraverse(split): term =>
        term
          .forward(at)
          .left
          .map(SparseFunctionalFactorizationError.Composite.apply)
          .map(value => term.weight * sfL1(value))
    yield directValues.sum + splitValues.sum

object HomogeneousFactorPenalties:
  def from[Space <: SemanticSpace](
      parameter: ParameterId,
      direct: Vector[ExactDirectPenalty[Space]] = Vector.empty,
      split: Vector[SplitL1Penalty[Space]] = Vector.empty
  ): Either[SparseFunctionalFactorizationError, HomogeneousFactorPenalties[Space]] =
    val terms = direct.map(_.original) ++ split.map(_.original)
    val wrongParameter = terms.exists(_.target.parameters != Vector(parameter))
    val nonHomogeneous = terms.find(_.functional.traits.homogeneity != HomogeneityTrait.DegreeOne)
    val identities = direct.map(_.identity) ++ split.map(_.identity)
    if direct.length > 1 then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          "a factor block admits at most one exact direct proximal term"
        )
      )
    else if wrongParameter then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          "every factor penalty must bind exactly the declared factor parameter"
        )
      )
    else if nonHomogeneous.nonEmpty then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          s"GPMF normalization requires degree-one penalties, got ${nonHomogeneous.get.functional}"
        )
      )
    else if identities.distinct.length != identities.length then
      Left(SparseFunctionalFactorizationError.InvalidDefinition("factor penalty identities must be distinct"))
    else Right(new HomogeneousFactorPenalties(parameter, direct, split))

  def empty[Space <: SemanticSpace](parameter: ParameterId): HomogeneousFactorPenalties[Space] =
    new HomogeneousFactorPenalties(parameter, Vector.empty, Vector.empty)

enum RankOneFactorStatus:
  case NonZero
  case ZeroSolution

enum AlternatingFactorizationStopping:
  case Converged
  case IterationLimit
  case ZeroSolution

enum FactorGaugeConvention:
  case SignedPairLargestColumnCoordinatePositive

enum FactorOrderingConvention:
  case SingleFactor
  case ExtractionOrder
  case DescendingStrength

enum MetricOrthogonalityConvention:
  case UnitMetricBalls
  case GreedyDeflationNoOrthogonalityClaim
  case GeneralizedStiefel

enum StructuredRankEstimand:
  case RankOneAlternating
  case GreedyDeflation
  case JointRankK

final class RankOneFactorInitialization[Space <: SemanticSpace] private (
    val value: DMat,
    val space: SpaceEvidence[Space],
    val identity: ValueIdentity
)

object RankOneFactorInitialization:
  def from[Space <: SemanticSpace](
      value: DMat,
      space: SpaceEvidence[Space],
      identity: ValueIdentity
  ): Either[SparseFunctionalFactorizationError, RankOneFactorInitialization[Space]] =
    if value.rows != space.dimension || value.cols != 1 then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          s"rank-one initialization for ${space.id.value} must be ${space.dimension} x 1, " +
            s"got ${value.rows} x ${value.cols}"
        )
      )
    else
      sfFirstNonFinite(value) match
        case Some((row, column, actual)) =>
          Left(
            SparseFunctionalFactorizationError.InvalidDefinition(
              s"initial factor value ($row,$column) is not finite: $actual"
            )
          )
        case None if sfSquaredNorm(value) == 0.0 =>
          Left(SparseFunctionalFactorizationError.InvalidDefinition("rank-one initialization must be nonzero"))
        case None => Right(new RankOneFactorInitialization(value, space, identity))

final case class AlternatingFactorizationConfig private (
    iterations: IterationBudget,
    tolerance: CertificateTolerance,
    block: PrimalDualConfig
)

object AlternatingFactorizationConfig:
  def from(
      iterations: Int,
      tolerance: CertificateTolerance,
      block: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[SparseFunctionalFactorizationError, AlternatingFactorizationConfig] =
    IterationBudget
      .apply(iterations)
      .left
      .map(SparseFunctionalFactorizationError.InvalidDefinition.apply compose (_.message))
      .map(AlternatingFactorizationConfig(_, tolerance, block))

  val portable: AlternatingFactorizationConfig =
    AlternatingFactorizationConfig(
      IterationBudget.unsafe(1000),
      CertificateTolerance.strict,
      PrimalDualConfig.portable
    )

final case class FactorBlockCertificate(
    parameter: ParameterId,
    regression: CompositeSparseSmoothCertificate,
    regressionGuarantee: AchievedOptimizationGuarantee,
    metricNorm: Double,
    constraintResidual: Double,
    coordinateResidual: Double,
    zero: Boolean
):
  require(metricNorm.isFinite && metricNorm >= 0.0)
  require(constraintResidual.isFinite && constraintResidual >= 0.0)
  require(coordinateResidual.isFinite && coordinateResidual >= 0.0)

final case class RankOneFactorizationCertificate(
    row: FactorBlockCertificate,
    column: FactorBlockCertificate,
    objectiveHistory: Vector[Double],
    iterations: Int,
    stopping: AlternatingFactorizationStopping,
    tolerance: CertificateTolerance
):
  require(objectiveHistory.nonEmpty && objectiveHistory.forall(_.isFinite))
  require(iterations > 0)

  def coordinateResidual: Double =
    Math.max(row.coordinateResidual, column.coordinateResidual)

final case class RankOneStructuredFit[
    Rows <: SemanticSpace,
    Columns <: SemanticSpace,
    Component <: SemanticSpace
](
    rowFactor: Op[Primal[Component], Primal[Rows], AxisOperatorRole, UncheckedEvidence],
    columnFactor: Op[Primal[Component], Primal[Columns], AxisOperatorRole, UncheckedEvidence],
    strength: Double,
    reconstruction: OpTable[Rows, Columns, UncheckedEvidence],
    status: RankOneFactorStatus,
    estimand: StructuredRankEstimand,
    gauge: FactorGaugeConvention,
    ordering: FactorOrderingConvention,
    orthogonality: MetricOrthogonalityConvention,
    achievement: AchievedOptimizationGuarantee,
    certificate: RankOneFactorizationCertificate,
    resultIdentity: ValueIdentity,
    provenance: SemanticProvenance
):
  require(strength.isFinite && strength >= 0.0)

final class RankOneStructuredFactorization[
    Rows <: SemanticSpace,
    Columns <: SemanticSpace,
    E <: OperatorEvidence
] private (
    val data: OpTable[Rows, Columns, E],
    val rowGeometry: StructuredFactorGeometry[Rows],
    val columnGeometry: StructuredFactorGeometry[Columns],
    val rowPenalties: HomogeneousFactorPenalties[Rows],
    val columnPenalties: HomogeneousFactorPenalties[Columns],
    val programIdentity: ValueIdentity,
    val provenance: SemanticProvenance
):
  def solve(
      initialization: RankOneFactorInitialization[Columns],
      config: AlternatingFactorizationConfig = AlternatingFactorizationConfig.portable
  ): Either[SparseFunctionalFactorizationError, RankOneStructuredFit[Rows, Columns, ? <: SemanticSpace]] =
    if initialization.space.descriptor != data.domain.descriptor.space then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          "rank-one initialization space does not match the data column space"
        )
      )
    else
      for
        initialColumn <- normalize(initialization.value, columnGeometry.operator)
        loop <- alternating(initialColumn, config)
        finalized <- finalizeFit(loop, config)
      yield finalized

  private def alternating(
      initialColumn: DMat,
      config: AlternatingFactorizationConfig
  ): Either[SparseFunctionalFactorizationError, AlternatingState] =
    var row = DMat.zeros(data.rows, 1)
    var column = initialColumn
    var rowUpdate = Option.empty[NormalizedBlockUpdate]
    var columnUpdate = Option.empty[NormalizedBlockUpdate]
    var history = Vector.empty[Double]
    var iteration = 0
    var stopping = AlternatingFactorizationStopping.IterationLimit
    var done = false
    var failure = Option.empty[SparseFunctionalFactorizationError]
    while iteration < config.iterations.intValue && !done && failure.isEmpty do
      val updated =
        for
          nextRow <- updateRow(column, config.block)
          nextColumn <- updateColumn(nextRow.normalized, config.block)
          objective <-
            if nextRow.zero || nextColumn.zero then Right(0.0)
            else objective(nextRow.normalized, nextColumn.normalized)
        yield (nextRow, nextColumn, objective)
      updated match
        case Left(error) => failure = Some(error)
        case Right((nextRow, nextColumn, nextObjective)) =>
          val residual = Math.max(
            sfMaxAbs(MatrixOps.subtract(nextRow.normalized, row)),
            sfMaxAbs(MatrixOps.subtract(nextColumn.normalized, column))
          )
          val objectiveChange = history.lastOption.fold(Double.MaxValue)(previous => Math.abs(nextObjective - previous))
          row = nextRow.normalized
          column = nextColumn.normalized
          rowUpdate = Some(nextRow)
          columnUpdate = Some(nextColumn)
          history = history :+ nextObjective
          iteration += 1
          if nextRow.zero || nextColumn.zero then
            stopping = AlternatingFactorizationStopping.ZeroSolution
            done = true
          else
            val scale = Math.max(1.0, Math.max(sfMaxAbs(row), sfMaxAbs(column)))
            val objectiveScale = Math.max(1.0, Math.abs(nextObjective))
            if residual <= config.tolerance.threshold(scale) &&
                objectiveChange <= config.tolerance.threshold(objectiveScale)
            then
              stopping = AlternatingFactorizationStopping.Converged
              done = true
    failure match
      case Some(error) => Left(error)
      case None =>
        Right(
          AlternatingState(
            row,
            column,
            rowUpdate.get,
            columnUpdate.get,
            history,
            iteration,
            stopping
          )
        )

  private def updateRow(
      column: DMat,
      config: PrimalDualConfig
  ): Either[SparseFunctionalFactorizationError, NormalizedBlockUpdate] =
    for
      weightedColumn <- columnGeometry.operator(column).left.map(SparseFunctionalFactorizationError.Semantic.apply)
      target <- data(weightedColumn).left.map(SparseFunctionalFactorizationError.Semantic.apply)
      update <- solveBlock(target, rowGeometry, rowPenalties, config, "row-factor-block")
    yield update

  private def updateColumn(
      row: DMat,
      config: PrimalDualConfig
  ): Either[SparseFunctionalFactorizationError, NormalizedBlockUpdate] =
    for
      weightedRow <- rowGeometry.operator(row).left.map(SparseFunctionalFactorizationError.Semantic.apply)
      target <- data.dual(weightedRow).left.map(SparseFunctionalFactorizationError.Semantic.apply)
      update <- solveBlock(target, columnGeometry, columnPenalties, config, "column-factor-block")
    yield update

  private def solveBlock[Space <: SemanticSpace](
      target: DMat,
      geometry: StructuredFactorGeometry[Space],
      penalties: HomogeneousFactorPenalties[Space],
      config: PrimalDualConfig,
      label: String
  ): Either[SparseFunctionalFactorizationError, NormalizedBlockUpdate] =
    for
      anchor <- StronglyConvexAnchor
        .metric(target, ValueIdentity.derived(label, data.valueIdentity, geometry.operator.valueIdentity), geometry.operator)
        .left
        .map(SparseFunctionalFactorizationError.Composite.apply)
      program <- CompositeSparseSmoothProgram
        .fromAnchor(
          penalties.parameter,
          anchor,
          Vector.empty,
          penalties.direct,
          penalties.split,
          provenance.append(
            SemanticProvenanceEvent.Derived(label, Vector(data.valueIdentity, geometry.operator.valueIdentity))
          )
        )
        .left
        .map(SparseFunctionalFactorizationError.Composite.apply)
      compiled <- program.compile().left.map(SparseFunctionalFactorizationError.Composite.apply)
      fit <- compiled.solve(config).left.map(SparseFunctionalFactorizationError.Composite.apply)
      metricNorm <- norm(fit.parameter, geometry.operator)
      normalized =
        if metricNorm == 0.0 then DMat.zeros(target.rows, target.cols)
        else MatrixOps.scale(fit.parameter, 1.0 / metricNorm)
    yield
      NormalizedBlockUpdate(
        penalties.parameter,
        normalized,
        metricNorm,
        metricNorm == 0.0,
        fit
      )

  private def finalizeFit(
      state: AlternatingState,
      config: AlternatingFactorizationConfig
  ): Either[SparseFunctionalFactorizationError, RankOneStructuredFit[Rows, Columns, ? <: SemanticSpace]] =
    val zero = state.stopping == AlternatingFactorizationStopping.ZeroSolution
    for
      rowCheck <- if zero then Right(state.rowUpdate) else updateRow(state.column, config.block)
      columnCheck <- if zero then Right(state.columnUpdate) else updateColumn(state.row, config.block)
      rowNorm <- norm(state.row, rowGeometry.operator)
      columnNorm <- norm(state.column, columnGeometry.operator)
      rowResidual = if zero then 0.0 else sfMaxAbs(MatrixOps.subtract(state.row, rowCheck.normalized))
      columnResidual = if zero then 0.0 else sfMaxAbs(MatrixOps.subtract(state.column, columnCheck.normalized))
      oriented <- orient(state.row, state.column, config.tolerance)
      association <- if zero then Right(0.0) else association(oriented._1, oriented._2)
      component <- SpaceRef
        .of(s"${programIdentity.stableKey}.rank-one-component", SpaceRole.Latent, 1)
        .left
        .map(SparseFunctionalFactorizationError.Multivar.apply)
      resultIdentity = ValueIdentity.derived(
        "joint-sparse-functional-rank-one-fit",
        programIdentity,
        data.valueIdentity
      )
      fitProvenance = provenance.append(
        SemanticProvenanceEvent.Derived(
          "joint-sparse-functional-rank-one",
          Vector(data.valueIdentity, rowGeometry.operator.valueIdentity, columnGeometry.operator.valueIdentity)
        )
      )
      rowAxis <- Op
        .fromDense(
          oriented._1,
          CoordinateEvidence.primal(component.evidence),
          data.codomain,
          OperatorRoleWitness.axis,
          ValueIdentity.derived("structured-row-factor", resultIdentity),
          fitProvenance
        )
        .left
        .map(SparseFunctionalFactorizationError.Semantic.apply)
      columnAxis <- Op
        .fromDense(
          oriented._2,
          CoordinateEvidence.primal(component.evidence),
          data.domain.star,
          OperatorRoleWitness.axis,
          ValueIdentity.derived("structured-column-factor", resultIdentity),
          fitProvenance
        )
        .left
        .map(SparseFunctionalFactorizationError.Semantic.apply)
      reconstruction <- Op
        .lowRank(
          MatrixOps.scale(oriented._1, association),
          oriented._2,
          data.domain,
          data.codomain,
          OperatorRoleWitness.table,
          ValueIdentity.derived("structured-rank-one-reconstruction", resultIdentity)
        )
        .left
        .map(SparseFunctionalFactorizationError.Semantic.apply)
      rowCertificate = FactorBlockCertificate(
        rowPenalties.parameter,
        rowCheck.fit.certificate,
        rowCheck.fit.achievement,
        rowNorm,
        if zero then rowNorm else Math.abs(rowNorm - 1.0),
        rowResidual,
        zero
      )
      columnCertificate = FactorBlockCertificate(
        columnPenalties.parameter,
        columnCheck.fit.certificate,
        columnCheck.fit.achievement,
        columnNorm,
        if zero then columnNorm else Math.abs(columnNorm - 1.0),
        columnResidual,
        zero
      )
      certificate = RankOneFactorizationCertificate(
        rowCertificate,
        columnCertificate,
        state.history,
        state.iterations,
        state.stopping,
        config.tolerance
      )
      achievement <- admitAchievement(resultIdentity, certificate)
    yield
      RankOneStructuredFit(
        rowAxis,
        columnAxis,
        association,
        reconstruction,
        if zero then RankOneFactorStatus.ZeroSolution else RankOneFactorStatus.NonZero,
        StructuredRankEstimand.RankOneAlternating,
        FactorGaugeConvention.SignedPairLargestColumnCoordinatePositive,
        FactorOrderingConvention.SingleFactor,
        MetricOrthogonalityConvention.UnitMetricBalls,
        achievement,
        certificate,
        resultIdentity,
        fitProvenance
      )

  private def objective(row: DMat, column: DMat): Either[SparseFunctionalFactorizationError, Double] =
    for
      value <- association(row, column)
      rowPenalty <- rowPenalties.value(row)
      columnPenalty <- columnPenalties.value(column)
    yield value - rowPenalty - columnPenalty

  private def association(row: DMat, column: DMat): Either[SparseFunctionalFactorizationError, Double] =
    for
      weightedColumn <- columnGeometry.operator(column).left.map(SparseFunctionalFactorizationError.Semantic.apply)
      projected <- data(weightedColumn).left.map(SparseFunctionalFactorizationError.Semantic.apply)
      weightedRow <- rowGeometry.operator(row).left.map(SparseFunctionalFactorizationError.Semantic.apply)
      value = sfInner(weightedRow, projected)
      _ <-
        if value >= -1e-10 then Right(())
        else Left(
          SparseFunctionalFactorizationError.InvalidDefinition(
            s"alternating factor association is unexpectedly negative: $value"
          )
        )
    yield Math.max(0.0, value)

  private def admitAchievement(
      resultIdentity: ValueIdentity,
      certificate: RankOneFactorizationCertificate
  ): Either[SparseFunctionalFactorizationError, AchievedOptimizationGuarantee] =
    val contract = MathematicalContractCatalog.jointSparseFunctionalFactorization
    val operators = (
      Vector(data.valueIdentity, rowGeometry.operator.valueIdentity, columnGeometry.operator.valueIdentity) ++
        rowPenalties.identities ++ columnPenalties.identities
    ).distinct
    for
      bindings <- OptimizationIdentityBindings
        .from(
          contract.id,
          programIdentity,
          data.valueIdentity,
          ObservationMaskIdentity.Complete,
          operators,
          Vector(rowPenalties.parameter, columnPenalties.parameter),
          resultIdentity
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      rowResidual <- NonNegativeProofBound
        .residual(certificate.row.coordinateResidual)
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      columnResidual <- NonNegativeProofBound
        .residual(certificate.column.coordinateResidual)
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      evidence <- SemanticOptimizationEvidence
        .from(
          bindings,
          certificate.stopping match
            case AlternatingFactorizationStopping.Converged => NumericalTermination.Converged
            case AlternatingFactorizationStopping.IterationLimit => NumericalTermination.IterationLimit
            case AlternatingFactorizationStopping.ZeroSolution => NumericalTermination.Converged,
          blockStationarity = Vector(
            rowPenalties.parameter -> rowResidual,
            columnPenalties.parameter -> columnResidual
          ),
          feasibility = Some(
            NonNegativeProofBound.unsafeResidual(
              Math.max(certificate.row.constraintResidual, certificate.column.constraintResidual)
            )
          ),
          numericalCertificates = rowGeometry.operator.certificate.claims ++
            columnGeometry.operator.certificate.claims
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      claim =
        certificate.stopping match
          case AlternatingFactorizationStopping.Converged | AlternatingFactorizationStopping.ZeroSolution =>
            OptimizationClaimClass.CoordinatewiseStationary
          case AlternatingFactorizationStopping.IterationLimit => OptimizationClaimClass.Unresolved
      achievement <- OptimizationGuaranteeAdmission
        .admit(
          contract,
          claim,
          OptimizationAssumptions.empty(bindings),
          Set.empty,
          evidence,
          None
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
    yield achievement

  private def normalize[Space <: SemanticSpace](
      value: DMat,
      geometry: OpMetric[Space, CertifiedSpd]
  ): Either[SparseFunctionalFactorizationError, DMat] =
    norm(value, geometry).flatMap: current =>
      if current == 0.0 then
        Left(SparseFunctionalFactorizationError.InvalidDefinition("initial factor has zero metric norm"))
      else Right(MatrixOps.scale(value, 1.0 / current))

  private def norm[Space <: SemanticSpace](
      value: DMat,
      geometry: OpMetric[Space, CertifiedSpd]
  ): Either[SparseFunctionalFactorizationError, Double] =
    geometry(value)
      .left
      .map(SparseFunctionalFactorizationError.Semantic.apply)
      .flatMap: image =>
        val squared = sfInner(value, image)
        if squared < -1e-10 || !squared.isFinite then
          Left(
            SparseFunctionalFactorizationError.InvalidDefinition(
              s"certified factor geometry produced invalid squared norm $squared"
            )
          )
        else Right(Math.sqrt(Math.max(0.0, squared)))

  private def orient(
      row: DMat,
      column: DMat,
      tolerance: CertificateTolerance
  ): Either[SparseFunctionalFactorizationError, (DMat, DMat)] =
    if sfSquaredNorm(row) == 0.0 || sfSquaredNorm(column) == 0.0 then Right(row -> column)
    else
      var pivot = 0
      var pivotMagnitude = Math.abs(column(0, 0))
      var index = 1
      while index < column.rows do
        val magnitude = Math.abs(column(index, 0))
        if magnitude > pivotMagnitude then
          pivot = index
          pivotMagnitude = magnitude
        index += 1
      if pivotMagnitude <= tolerance.threshold(1.0) then
        Left(SparseFunctionalFactorizationError.InvalidDefinition("nonzero column factor has no stable sign pivot"))
      else if column(pivot, 0) < 0.0 then
        Right(MatrixOps.scale(row, -1.0) -> MatrixOps.scale(column, -1.0))
      else Right(row -> column)

private final case class NormalizedBlockUpdate(
    parameter: ParameterId,
    normalized: DMat,
    regressionNorm: Double,
    zero: Boolean,
    fit: CompositeSparseSmoothFit
)

private final case class AlternatingState(
    row: DMat,
    column: DMat,
    rowUpdate: NormalizedBlockUpdate,
    columnUpdate: NormalizedBlockUpdate,
    history: Vector[Double],
    iterations: Int,
    stopping: AlternatingFactorizationStopping
)

object RankOneStructuredFactorization:
  def from[
      Rows <: SemanticSpace,
      Columns <: SemanticSpace,
      E <: OperatorEvidence
  ](
      data: OpTable[Rows, Columns, E],
      rowGeometry: StructuredFactorGeometry[Rows],
      columnGeometry: StructuredFactorGeometry[Columns],
      rowPenalties: HomogeneousFactorPenalties[Rows],
      columnPenalties: HomogeneousFactorPenalties[Columns]
  ): Either[
    SparseFunctionalFactorizationError,
    RankOneStructuredFactorization[Rows, Columns, E]
  ] =
    if rowGeometry.operator.rows != data.rows then
      Left(SparseFunctionalFactorizationError.InvalidDefinition("row factor geometry does not match the data row space"))
    else if columnGeometry.operator.rows != data.cols then
      Left(SparseFunctionalFactorizationError.InvalidDefinition("column factor geometry does not match the data column space"))
    else if rowPenalties.parameter == columnPenalties.parameter then
      Left(SparseFunctionalFactorizationError.InvalidDefinition("row and column factors require distinct parameter identities"))
    else
      val inputs = Vector(
        data.valueIdentity,
        rowGeometry.operator.valueIdentity,
        columnGeometry.operator.valueIdentity
      ) ++ rowPenalties.identities ++ columnPenalties.identities
      val identity = ValueIdentity.derived("joint-sparse-functional-rank-one-program", inputs*)
      val provenance = (data.provenance ++ rowGeometry.provenance ++ columnGeometry.provenance).append(
        SemanticProvenanceEvent.Derived("joint-sparse-functional-rank-one-program", inputs)
      )
      Right(
        new RankOneStructuredFactorization(
          data,
          rowGeometry,
          columnGeometry,
          rowPenalties,
          columnPenalties,
          identity,
          provenance
        )
      )

enum GreedyDeflationRule:
  case SubtractFittedRankOne

final case class GreedyStructuredFit[Rows <: SemanticSpace, Columns <: SemanticSpace](
    components: Vector[RankOneStructuredFit[Rows, Columns, ? <: SemanticSpace]],
    reconstruction: OpTable[Rows, Columns, UncheckedEvidence],
    requested: ComponentCount,
    extracted: Int,
    estimand: StructuredRankEstimand,
    ordering: FactorOrderingConvention,
    orthogonality: MetricOrthogonalityConvention,
    rule: GreedyDeflationRule,
    provenance: SemanticProvenance
)

/** Sequential rank-one extraction. This is intentionally not the joint
  * generalized-Stiefel rank-k estimand below.
  */
final class GreedyStructuredFactorization[
    Rows <: SemanticSpace,
    Columns <: SemanticSpace,
    E <: OperatorEvidence
] private (
    rankOne: RankOneStructuredFactorization[Rows, Columns, E],
    val components: ComponentCount,
    val rule: GreedyDeflationRule
):
  def solve(
      initializations: Vector[RankOneFactorInitialization[Columns]],
      config: AlternatingFactorizationConfig = AlternatingFactorizationConfig.portable
  ): Either[SparseFunctionalFactorizationError, GreedyStructuredFit[Rows, Columns]] =
    if initializations.length != components.value then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          s"greedy rank-${components.value} fitting requires ${components.value} initializations"
        )
      )
    else
      var data = uncheckedData(rankOne.data, "greedy-structured-input")
      var fits = Vector.empty[RankOneStructuredFit[Rows, Columns, ? <: SemanticSpace]]
      var index = 0
      var failure = Option.empty[SparseFunctionalFactorizationError]
      while index < components.value && failure.isEmpty do
        val current = RankOneStructuredFactorization.from(
          data,
          rankOne.rowGeometry,
          rankOne.columnGeometry,
          rankOne.rowPenalties,
          rankOne.columnPenalties
        )
        current.flatMap(_.solve(initializations(index), config)) match
          case Left(error) => failure = Some(error)
          case Right(fit) =>
            fits = fits :+ fit
            data = subtract(data, fit.reconstruction, index)
        index += 1
      failure match
        case Some(error) => Left(error)
        case None =>
          val reconstruction = combine(fits)
          Right(
            GreedyStructuredFit(
              fits,
              reconstruction,
              components,
              fits.count(_.status == RankOneFactorStatus.NonZero),
              StructuredRankEstimand.GreedyDeflation,
              FactorOrderingConvention.ExtractionOrder,
              MetricOrthogonalityConvention.GreedyDeflationNoOrthogonalityClaim,
              rule,
              rankOne.provenance.append(
                SemanticProvenanceEvent.Derived(
                  "greedy-structured-factorization",
                  fits.map(_.resultIdentity)
                )
              )
            )
          )

  private def subtract(
      left: OpTable[Rows, Columns, UncheckedEvidence],
      right: OpTable[Rows, Columns, UncheckedEvidence],
      index: Int
  ): OpTable[Rows, Columns, UncheckedEvidence] =
    val identity = ValueIdentity.derived(s"greedy-deflation-$index", left.valueIdentity, right.valueIdentity)
    new Op(
      WeightedSumKernel(left.kernel, right.kernel, 1.0, -1.0),
      left.domain,
      left.codomain,
      OperatorRoleWitness.table,
      OperatorCertificate.unchecked(identity),
      identity,
      (left.provenance ++ right.provenance).append(
        SemanticProvenanceEvent.Derived("subtract-fitted-rank-one", Vector(left.valueIdentity, right.valueIdentity))
      )
    )

  private def combine(
      fits: Vector[RankOneStructuredFit[Rows, Columns, ? <: SemanticSpace]]
  ): OpTable[Rows, Columns, UncheckedEvidence] =
    fits.tail.foldLeft(fits.head.reconstruction): (combined, fit) =>
      val identity = ValueIdentity.derived(
        "greedy-reconstruction-sum",
        combined.valueIdentity,
        fit.reconstruction.valueIdentity
      )
      new Op(
        WeightedSumKernel(combined.kernel, fit.reconstruction.kernel, 1.0, 1.0),
        combined.domain,
        combined.codomain,
        OperatorRoleWitness.table,
        OperatorCertificate.unchecked(identity),
        identity,
        combined.provenance ++ fit.reconstruction.provenance
      )

  private def uncheckedData[EE <: OperatorEvidence](
      source: OpTable[Rows, Columns, EE],
      label: String
  ): OpTable[Rows, Columns, UncheckedEvidence] =
    val identity = ValueIdentity.derived(label, source.valueIdentity)
    new Op(
      source.kernel,
      source.domain,
      source.codomain,
      OperatorRoleWitness.table,
      OperatorCertificate.unchecked(identity),
      identity,
      source.provenance.append(SemanticProvenanceEvent.Adapted(label))
    )

object GreedyStructuredFactorization:
  def from[
      Rows <: SemanticSpace,
      Columns <: SemanticSpace,
      E <: OperatorEvidence
  ](
      rankOne: RankOneStructuredFactorization[Rows, Columns, E],
      components: ComponentCount,
      rule: GreedyDeflationRule = GreedyDeflationRule.SubtractFittedRankOne
  ): Either[SparseFunctionalFactorizationError, GreedyStructuredFactorization[Rows, Columns, E]] =
    if components.value > Math.min(rankOne.data.rows, rankOne.data.cols) then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          s"requested ${components.value} greedy components beyond data rank bound"
        )
      )
    else Right(new GreedyStructuredFactorization(rankOne, components, rule))

final case class JointRankKFactorizationCertificate(
    retainedRank: Int,
    rowMetricResidual: Double,
    columnMetricResidual: Double,
    generalizedSvdResidual: Double,
    objectiveValue: Double,
    numerical: NumericalCertificate
):
  require(retainedRank > 0)
  require(rowMetricResidual.isFinite && rowMetricResidual >= 0.0)
  require(columnMetricResidual.isFinite && columnMetricResidual >= 0.0)
  require(generalizedSvdResidual.isFinite && generalizedSvdResidual >= 0.0)
  require(objectiveValue.isFinite && objectiveValue >= 0.0)

  def residual: Double =
    Math.max(generalizedSvdResidual, Math.max(rowMetricResidual, columnMetricResidual))

final case class JointRankKStructuredFit[
    Rows <: SemanticSpace,
    Columns <: SemanticSpace,
    Component <: SemanticSpace
](
    rowFactors: Op[Primal[Component], Primal[Rows], AxisOperatorRole, UncheckedEvidence],
    columnFactors: Op[Primal[Component], Primal[Columns], AxisOperatorRole, UncheckedEvidence],
    strengths: DVec,
    reconstruction: OpTable[Rows, Columns, UncheckedEvidence],
    estimand: StructuredRankEstimand,
    gauge: FactorGaugeConvention,
    ordering: FactorOrderingConvention,
    orthogonality: MetricOrthogonalityConvention,
    achievement: AchievedOptimizationGuarantee,
    certificate: JointRankKFactorizationCertificate,
    resultIdentity: ValueIdentity,
    provenance: SemanticProvenance
)

/** Simultaneous rank-k estimator on a product generalized-Stiefel manifold.
  * The empty-nonsmooth-penalty case has an exact generalized cross-SVD
  * reduction. Nonsmooth joint rank-k fitting requires a manifold/PALM solver
  * and is rejected rather than routed through greedy deflation.
  */
final class JointRankKStructuredFactorization[
    Rows <: SemanticSpace,
    Columns <: SemanticSpace,
    E <: OperatorEvidence
] private (
    val data: OpTable[Rows, Columns, E],
    val rowGeometry: StructuredFactorGeometry[Rows],
    val columnGeometry: StructuredFactorGeometry[Columns],
    val rowPenalties: HomogeneousFactorPenalties[Rows],
    val columnPenalties: HomogeneousFactorPenalties[Columns],
    val components: ComponentCount,
    val estimand: StructuredRankEstimand,
    val ordering: FactorOrderingConvention,
    val orthogonality: MetricOrthogonalityConvention,
    val programIdentity: ValueIdentity
):
  def solveExact(
      solver: SvdSolver = DenseSolvers.svd,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[
    SparseFunctionalFactorizationError,
    JointRankKStructuredFit[Rows, Columns, ? <: SemanticSpace]
  ] =
    if rowPenalties.identities.nonEmpty || columnPenalties.identities.nonEmpty then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          "joint rank-k generalized cross-SVD admits metric smoothness but no nonsmooth factor penalties; " +
            "use a joint manifold/PALM program when that solver is available"
        )
      )
    else
      for
        dataDense <- data.toDense.left.map(SparseFunctionalFactorizationError.Semantic.apply)
        rowMetric <- rowGeometry.operator.toDense.left.map(SparseFunctionalFactorizationError.Semantic.apply)
        columnMetric <- columnGeometry.operator.toDense.left.map(SparseFunctionalFactorizationError.Semantic.apply)
        rowInverseHalf <- MatrixOps
          .inverseSquareRoot(rowMetric, eigenSolver, 1e-12)
          .left
          .map(SparseFunctionalFactorizationError.Multivar.apply)
        columnInverseHalf <- MatrixOps
          .inverseSquareRoot(columnMetric, eigenSolver, 1e-12)
          .left
          .map(SparseFunctionalFactorizationError.Multivar.apply)
        generalizedCross = GaleNumerics.multiply(rowMetric, GaleNumerics.multiply(dataDense, columnMetric))
        whitened = GaleNumerics.multiply(
          rowInverseHalf,
          GaleNumerics.multiply(generalizedCross, columnInverseHalf)
        )
        svd <- solver
          .decompose(MatrixView.dense(whitened), components)
          .left
          .map(SparseFunctionalFactorizationError.Multivar.apply)
        _ <-
          if svd.singularValues.length == components.value then Right(())
          else
            Left(
              SparseFunctionalFactorizationError.InvalidDefinition(
                s"joint rank ${components.value} exceeds the numerical generalized-cross rank " +
                  s"${svd.singularValues.length}"
              )
            )
        rowValues = GaleNumerics.multiply(rowInverseHalf, svd.u)
        columnValues = GaleNumerics.multiply(columnInverseHalf, svd.v)
        rowResidual = sfMetricFrameResidual(rowValues, rowMetric)
        columnResidual = sfMetricFrameResidual(columnValues, columnMetric)
        crossResidual = sfGeneralizedSvdResidual(
          generalizedCross,
          rowMetric,
          columnMetric,
          rowValues,
          columnValues,
          svd.singularValues
        )
        objectiveValue = sfSum(svd.singularValues)
        component <- SpaceRef
          .of(
            s"${programIdentity.stableKey}.joint-rank-${components.value}",
            SpaceRole.Latent,
            components.value
          )
          .left
          .map(SparseFunctionalFactorizationError.Multivar.apply)
        resultIdentity = ValueIdentity.derived(
          "joint-rank-k-generalized-cross-svd-fit",
          programIdentity,
          data.valueIdentity
        )
        fitProvenance = (
          data.provenance ++ rowGeometry.provenance ++ columnGeometry.provenance
        ).append(
          SemanticProvenanceEvent.Derived(
            "joint-rank-k-generalized-cross-svd",
            Vector(data.valueIdentity, rowGeometry.operator.valueIdentity, columnGeometry.operator.valueIdentity)
          )
        )
        rowFactors <- Op
          .fromDense(
            rowValues,
            CoordinateEvidence.primal(component.evidence),
            data.codomain,
            OperatorRoleWitness.axis,
            ValueIdentity.derived("joint-structured-row-factors", resultIdentity),
            fitProvenance
          )
          .left
          .map(SparseFunctionalFactorizationError.Semantic.apply)
        columnFactors <- Op
          .fromDense(
            columnValues,
            CoordinateEvidence.primal(component.evidence),
            data.domain.star,
            OperatorRoleWitness.axis,
            ValueIdentity.derived("joint-structured-column-factors", resultIdentity),
            fitProvenance
          )
          .left
          .map(SparseFunctionalFactorizationError.Semantic.apply)
        reconstruction <- Op
          .lowRank(
            MatrixOps.scaleColumns(rowValues, svd.singularValues),
            columnValues,
            data.domain,
            data.codomain,
            OperatorRoleWitness.table,
            ValueIdentity.derived("joint-structured-reconstruction", resultIdentity)
          )
          .left
          .map(SparseFunctionalFactorizationError.Semantic.apply)
        context <- CertificateContext
          .from(
            CertificateTolerance.strict,
            CertificateNorm.Frobenius,
            "joint-rank-k-generalized-cross-svd",
            "gale",
            NumericalPrecision.Float64,
            None
          )
          .left
          .map(SparseFunctionalFactorizationError.Semantic.apply)
        numerical <- Certificate
          .converged(
            resultIdentity,
            iterations = 0,
            Math.max(crossResidual, Math.max(rowResidual, columnResidual)),
            Math.max(1.0, objectiveValue),
            context
          )
          .left
          .map(SparseFunctionalFactorizationError.Semantic.apply)
        certificate = JointRankKFactorizationCertificate(
          components.value,
          rowResidual,
          columnResidual,
          crossResidual,
          objectiveValue,
          numerical.runtime
        )
        achievement <- admitExact(resultIdentity, certificate)
      yield
        JointRankKStructuredFit(
          rowFactors,
          columnFactors,
          svd.singularValues,
          reconstruction,
          StructuredRankEstimand.JointRankK,
          FactorGaugeConvention.SignedPairLargestColumnCoordinatePositive,
          FactorOrderingConvention.DescendingStrength,
          MetricOrthogonalityConvention.GeneralizedStiefel,
          achievement,
          certificate,
          resultIdentity,
          fitProvenance
        )

  private def admitExact(
      resultIdentity: ValueIdentity,
      certificate: JointRankKFactorizationCertificate
  ): Either[SparseFunctionalFactorizationError, AchievedOptimizationGuarantee] =
    val contract = MathematicalContractCatalog.exactSpectralFrame
    val operators = Vector(
      data.valueIdentity,
      rowGeometry.operator.valueIdentity,
      columnGeometry.operator.valueIdentity
    )
    for
      bindings <- OptimizationIdentityBindings
        .from(
          contract.id,
          programIdentity,
          data.valueIdentity,
          ObservationMaskIdentity.Complete,
          operators,
          Vector(rowPenalties.parameter, columnPenalties.parameter),
          resultIdentity
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      emptyPenalties <- TheoremAssumptionWitness
        .from(
          bindings,
          ContractReference.unsafeAssumption("empty-factor-penalties"),
          Vector(programIdentity),
          TheoremAssumptionEvidence.AlgorithmicReduction("empty nonsmooth penalty bundle")
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      rowSpd <- TheoremAssumptionWitness
        .from(
          bindings,
          ContractReference.unsafeAssumption("spd-left-normalization"),
          Vector(rowGeometry.operator.valueIdentity),
          TheoremAssumptionEvidence.StaticType
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      columnSpd <- TheoremAssumptionWitness
        .from(
          bindings,
          ContractReference.unsafeAssumption("spd-right-normalization"),
          Vector(columnGeometry.operator.valueIdentity),
          TheoremAssumptionEvidence.StaticType
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      spectrum <- TheoremAssumptionWitness
        .from(
          bindings,
          ContractReference.unsafeAssumption("certified-generalized-cross-svd"),
          Vector(resultIdentity),
          TheoremAssumptionEvidence.Numerical(certificate.numerical)
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      assumptions <- OptimizationAssumptions
        .from(bindings, theoremAssumptions = Vector(emptyPenalties, rowSpd, columnSpd, spectrum))
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      feasibility <- NonNegativeProofBound
        .residual(Math.max(certificate.rowMetricResidual, certificate.columnMetricResidual))
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      evidence <- SemanticOptimizationEvidence
        .from(
          bindings,
          NumericalTermination.Converged,
          feasibility = Some(feasibility),
          numericalCertificates = Vector(certificate.numerical)
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      witness <- GlobalOptimalityWitness
        .from(
          bindings,
          ContractReference.unsafeTheorem("generalized-cross-spectrum"),
          assumptions.assumptionReferences,
          OracleFamily.Analytic
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
      achievement <- OptimizationGuaranteeAdmission
        .admit(
          contract,
          OptimizationClaimClass.ExactGlobal,
          assumptions,
          Set.empty,
          evidence,
          Some(witness)
        )
        .left
        .map(SparseFunctionalFactorizationError.Guarantee.apply)
    yield achievement

object JointRankKStructuredFactorization:
  def from[
      Rows <: SemanticSpace,
      Columns <: SemanticSpace,
      E <: OperatorEvidence
  ](
      rankOne: RankOneStructuredFactorization[Rows, Columns, E],
      components: ComponentCount
  ): Either[
    SparseFunctionalFactorizationError,
    JointRankKStructuredFactorization[Rows, Columns, E]
  ] =
    if components.value <= 1 then
      Left(SparseFunctionalFactorizationError.InvalidDefinition("joint rank-k requires at least two components"))
    else if components.value > Math.min(rankOne.data.rows, rankOne.data.cols) then
      Left(
        SparseFunctionalFactorizationError.InvalidDefinition(
          s"joint rank ${components.value} exceeds the data dimension bound"
        )
      )
    else
      Right(
        new JointRankKStructuredFactorization(
          rankOne.data,
          rankOne.rowGeometry,
          rankOne.columnGeometry,
          rankOne.rowPenalties,
          rankOne.columnPenalties,
          components,
          StructuredRankEstimand.JointRankK,
          FactorOrderingConvention.DescendingStrength,
          MetricOrthogonalityConvention.GeneralizedStiefel,
          ValueIdentity.derived("joint-rank-k-structured-program", rankOne.programIdentity)
        )
      )

private def sfL1(value: DMat): Double =
  var result = 0.0
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      result += Math.abs(value(row, column))
      column += 1
    row += 1
  result

private def sfMetricFrameResidual(factors: DMat, metric: DMat): Double =
  val gram = GaleNumerics.transposeMultiply(factors, GaleNumerics.multiply(metric, factors))
  var result = 0.0
  var row = 0
  while row < gram.rows do
    var column = 0
    while column < gram.cols do
      val expected = if row == column then 1.0 else 0.0
      result = Math.max(result, Math.abs(gram(row, column) - expected))
      column += 1
    row += 1
  result

private def sfGeneralizedSvdResidual(
    cross: DMat,
    rowMetric: DMat,
    columnMetric: DMat,
    rowFactors: DMat,
    columnFactors: DMat,
    strengths: DVec
): Double =
  val rowEquationLeft = GaleNumerics.multiply(cross, columnFactors)
  val rowEquationRight = MatrixOps.scaleColumns(
    GaleNumerics.multiply(rowMetric, rowFactors),
    strengths
  )
  val columnEquationLeft = GaleNumerics.multiply(cross.t, rowFactors)
  val columnEquationRight = MatrixOps.scaleColumns(
    GaleNumerics.multiply(columnMetric, columnFactors),
    strengths
  )
  Math.max(
    sfRelativeFrobeniusResidual(rowEquationLeft, rowEquationRight),
    sfRelativeFrobeniusResidual(columnEquationLeft, columnEquationRight)
  )

private def sfRelativeFrobeniusResidual(left: DMat, right: DMat): Double =
  var differenceSquared = 0.0
  var leftSquared = 0.0
  var rightSquared = 0.0
  var row = 0
  while row < left.rows do
    var column = 0
    while column < left.cols do
      val difference = left(row, column) - right(row, column)
      differenceSquared += difference * difference
      leftSquared += left(row, column) * left(row, column)
      rightSquared += right(row, column) * right(row, column)
      column += 1
    row += 1
  Math.sqrt(differenceSquared) /
    Math.max(1.0, Math.max(Math.sqrt(leftSquared), Math.sqrt(rightSquared)))

private def sfSum(values: DVec): Double =
  var result = 0.0
  var index = 0
  while index < values.length do
    result += values(index)
    index += 1
  result

private def sfInner(left: DMat, right: DMat): Double =
  var result = 0.0
  var row = 0
  while row < left.rows do
    var column = 0
    while column < left.cols do
      result += left(row, column) * right(row, column)
      column += 1
    row += 1
  result

private def sfSquaredNorm(value: DMat): Double = sfInner(value, value)

private def sfMaxAbs(value: DMat): Double =
  var result = 0.0
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      result = Math.max(result, Math.abs(value(row, column)))
      column += 1
    row += 1
  result

private def sfFirstNonFinite(value: DMat): Option[(Int, Int, Double)] =
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      if !value(row, column).isFinite then return Some((row, column, value(row, column)))
      column += 1
    row += 1
  None

private def sfTraverse[A, B](values: Vector[A])(
    function: A => Either[SparseFunctionalFactorizationError, B]
): Either[SparseFunctionalFactorizationError, Vector[B]] =
  values.foldLeft[Either[SparseFunctionalFactorizationError, Vector[B]]](Right(Vector.empty)):
    (result, value) =>
      for
        completed <- result
        next <- function(value)
      yield completed :+ next
