package scalafim.multivar
package core

import gale.linalg.{CholeskyOptions, DMat, DVec, DoubleLinearOperator, LinAlgError, MutableDVec}

sealed trait TableOperatorRole extends OperatorRoleTag
sealed trait MetricOperatorRole extends OperatorRoleTag
sealed trait CometricOperatorRole extends OperatorRoleTag
sealed trait CovarianceOperatorRole extends OperatorRoleTag
sealed trait ScatterOperatorRole extends OperatorRoleTag
sealed trait PenaltyOperatorRole extends OperatorRoleTag
sealed trait KernelOperatorRole extends OperatorRoleTag
sealed trait RowLinkOperatorRole extends OperatorRoleTag
sealed trait FrameOperatorRole extends OperatorRoleTag
sealed trait CrossOperatorRole extends OperatorRoleTag
sealed trait ComponentOperatorRole extends OperatorRoleTag
sealed trait ScoreOperatorRole extends OperatorRoleTag
sealed trait AxisOperatorRole extends OperatorRoleTag
sealed trait CoefficientOperatorRole extends OperatorRoleTag
sealed trait SynthesisOperatorRole extends OperatorRoleTag
sealed trait ConstraintOperatorRole extends OperatorRoleTag
sealed trait ComposedOperatorRole[A <: OperatorRoleTag, B <: OperatorRoleTag] extends OperatorRoleTag
sealed trait DualOperatorRole[A <: OperatorRoleTag] extends OperatorRoleTag
sealed trait MetricAdjointOperatorRole[A <: OperatorRoleTag] extends OperatorRoleTag

/** Compile-time witness that an operator connects one nominal space to its
  * algebraic dual. Only such operators may acquire symmetry or definiteness
  * evidence.
  */
sealed trait SelfDualPorts[From <: Coordinate, To <: Coordinate]

object SelfDualPorts:
  given primalToDual[S <: SemanticSpace]: SelfDualPorts[Primal[S], Dual[S]] with {}
  given dualToPrimal[S <: SemanticSpace]: SelfDualPorts[Dual[S], Primal[S]] with {}

enum OperatorRole:
  case Table
  case Metric
  case Cometric
  case Covariance
  case Scatter
  case Penalty
  case Kernel
  case RowLink
  case Frame
  case Cross
  case Component
  case Score
  case Axis
  case Coefficient
  case Synthesis
  case ConstraintMap
  case Composed(first: OperatorRole, second: OperatorRole)
  case Dual(of: OperatorRole)
  case MetricAdjoint(of: OperatorRole)

final class OperatorRoleWitness[R <: OperatorRoleTag] private (val value: OperatorRole)

object OperatorRoleWitness:
  val table: OperatorRoleWitness[TableOperatorRole] = new OperatorRoleWitness(OperatorRole.Table)
  val metric: OperatorRoleWitness[MetricOperatorRole] = new OperatorRoleWitness(OperatorRole.Metric)
  val cometric: OperatorRoleWitness[CometricOperatorRole] = new OperatorRoleWitness(OperatorRole.Cometric)
  val covariance: OperatorRoleWitness[CovarianceOperatorRole] = new OperatorRoleWitness(OperatorRole.Covariance)
  val scatter: OperatorRoleWitness[ScatterOperatorRole] = new OperatorRoleWitness(OperatorRole.Scatter)
  val penalty: OperatorRoleWitness[PenaltyOperatorRole] = new OperatorRoleWitness(OperatorRole.Penalty)
  val kernel: OperatorRoleWitness[KernelOperatorRole] = new OperatorRoleWitness(OperatorRole.Kernel)
  val rowLink: OperatorRoleWitness[RowLinkOperatorRole] = new OperatorRoleWitness(OperatorRole.RowLink)
  val frame: OperatorRoleWitness[FrameOperatorRole] = new OperatorRoleWitness(OperatorRole.Frame)
  val cross: OperatorRoleWitness[CrossOperatorRole] = new OperatorRoleWitness(OperatorRole.Cross)
  val component: OperatorRoleWitness[ComponentOperatorRole] = new OperatorRoleWitness(OperatorRole.Component)
  val score: OperatorRoleWitness[ScoreOperatorRole] = new OperatorRoleWitness(OperatorRole.Score)
  val axis: OperatorRoleWitness[AxisOperatorRole] = new OperatorRoleWitness(OperatorRole.Axis)
  val coefficient: OperatorRoleWitness[CoefficientOperatorRole] =
    new OperatorRoleWitness(OperatorRole.Coefficient)
  val synthesis: OperatorRoleWitness[SynthesisOperatorRole] =
    new OperatorRoleWitness(OperatorRole.Synthesis)
  val constraint: OperatorRoleWitness[ConstraintOperatorRole] =
    new OperatorRoleWitness(OperatorRole.ConstraintMap)

  private[multivar] def derived[R <: OperatorRoleTag](value: OperatorRole): OperatorRoleWitness[R] =
    new OperatorRoleWitness(value)

/** Runtime-authoritative evidence bound to one immutable operator identity. */
final class OperatorCertificate[E <: OperatorEvidence] private (
    val valueIdentity: ValueIdentity,
    val status: EvidenceStatus,
    val claims: Vector[NumericalCertificate]
):
  private[multivar] def rebind(identity: ValueIdentity): OperatorCertificate[E] =
    new OperatorCertificate(identity, status, claims.map(_.copy(valueIdentity = identity)))

object OperatorCertificate:
  private[multivar] def unchecked(identity: ValueIdentity): OperatorCertificate[UncheckedEvidence] =
    new OperatorCertificate(identity, EvidenceStatus.Unchecked, Vector.empty)

  private[multivar] def certified[E <: OperatorEvidence](
      identity: ValueIdentity,
      certificate: NumericalCertificate
  ): Either[SemanticError, OperatorCertificate[E]] =
    if certificate.valueIdentity != identity then
      Left(SemanticError.CertificateValueMismatch(identity, certificate.valueIdentity))
    else Right(new OperatorCertificate(identity, EvidenceStatus.Certified, Vector(certificate)))

  private[multivar] def assumed[E <: OperatorEvidence](
      identity: ValueIdentity
  ): OperatorCertificate[E] =
    new OperatorCertificate(identity, EvidenceStatus.Assumed, Vector.empty)

/** The role/evidence-refined operator. It uses `SemanticKernel` as its sole
  * numeric substrate; `Lin` and the legacy form wrappers are compatibility
  * views over that same kernel during migration, not a second engine.
  */
final class Op[
    From <: Coordinate,
    To <: Coordinate,
    R <: OperatorRoleTag,
    E <: OperatorEvidence
] private[multivar] (
    private[multivar] val kernel: SemanticKernel,
    val domain: CoordinateEvidence[From],
    val codomain: CoordinateEvidence[To],
    val role: OperatorRoleWitness[R],
    val certificate: OperatorCertificate[E],
    val valueIdentity: ValueIdentity,
    val provenance: SemanticProvenance
):
  require(kernel.rows == codomain.dimension && kernel.cols == domain.dimension, "operator kernel must match coordinates")
  require(certificate.valueIdentity == valueIdentity, "operator certificate must match value identity")

  def rows: Int = codomain.dimension
  def cols: Int = domain.dimension
  def representation: OperatorRepresentation = kernel.representation

  def apply(input: DMat): Either[SemanticError, DMat] =
    kernel.forward(input)

  def toDense: Either[SemanticError, DMat] =
    apply(DMat.eye(cols))

  def andThen[Next <: Coordinate, R2 <: OperatorRoleTag, E2 <: OperatorEvidence](
      next: Op[To, Next, R2, E2]
  ): Op[From, Next, ComposedOperatorRole[R, R2], UncheckedEvidence] =
    val identity = ValueIdentity.compose(valueIdentity, next.valueIdentity)
    new Op(
      SemanticKernel.compose(kernel, next.kernel),
      domain,
      next.codomain,
      OperatorRoleWitness.derived(OperatorRole.Composed(role.value, next.role.value)),
      OperatorCertificate.unchecked(identity),
      identity,
      (provenance ++ next.provenance).append(
        SemanticProvenanceEvent.Derived("compose", Vector(valueIdentity, next.valueIdentity))
      )
    )

  def dual: Op[DualOf[To], DualOf[From], DualOperatorRole[R], E] =
    val identity = valueIdentity.star
    new Op(
      kernel.adjoint,
      codomain.star,
      domain.star,
      OperatorRoleWitness.derived(OperatorRole.Dual(role.value)),
      certificate.rebind(identity),
      identity,
      provenance.append(SemanticProvenanceEvent.Derived("algebraic-dual", Vector(valueIdentity)))
    )

  private[multivar] def retag[RR <: OperatorRoleTag](
      target: OperatorRoleWitness[RR],
      operation: String
  ): Op[From, To, RR, E] =
    val identity = ValueIdentity.derived(operation, valueIdentity)
    new Op(
      kernel,
      domain,
      codomain,
      target,
      certificate.rebind(identity),
      identity,
      provenance.append(SemanticProvenanceEvent.Derived(operation, Vector(valueIdentity)))
    )

object Op:
  def fromDense[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag
  ](
      matrix: DMat,
      domain: CoordinateEvidence[From],
      codomain: CoordinateEvidence[To],
      role: OperatorRoleWitness[R],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("dense-operator")
  ): Either[SemanticError, Op[From, To, R, UncheckedEvidence]] =
    fromKernel(DenseMatrixKernel(matrix), domain, codomain, role, valueIdentity, provenance)

  def fromLinearMap[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag
  ](
      operator: DoubleLinearOperator,
      domain: CoordinateEvidence[From],
      codomain: CoordinateEvidence[To],
      role: OperatorRoleWitness[R],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("linear-operator")
  ): Either[SemanticError, Op[From, To, R, UncheckedEvidence]] =
    fromKernel(LinearMapKernel(operator), domain, codomain, role, valueIdentity, provenance)

  def fromMatrixView[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag
  ](
      view: MatrixView,
      domain: CoordinateEvidence[From],
      codomain: CoordinateEvidence[To],
      role: OperatorRoleWitness[R],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("matrix-view-operator")
  ): Either[SemanticError, Op[From, To, R, UncheckedEvidence]] =
    fromKernel(MatrixViewKernel(view), domain, codomain, role, valueIdentity, provenance)

  def fromLin[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag](
      linear: Lin[From, To],
      role: OperatorRoleWitness[R]
  ): Op[From, To, R, UncheckedEvidence] =
    new Op(
      linear.kernel,
      linear.domain,
      linear.codomain,
      role,
      OperatorCertificate.unchecked(linear.valueIdentity),
      linear.valueIdentity,
      linear.provenance.append(SemanticProvenanceEvent.Adapted("Lin"))
    )

  def certifiedSymmetric[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag](
      unchecked: Op[From, To, R, UncheckedEvidence],
      certificate: Certificate[SymmetryProperty]
  )(using SelfDualPorts[From, To]): Either[SemanticError, Op[From, To, R, CertifiedSymmetric]] =
    certify(unchecked, certificate.runtime)

  def certifiedPsd[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag](
      unchecked: Op[From, To, R, UncheckedEvidence],
      certificate: Certificate[PsdProperty]
  )(using SelfDualPorts[From, To]): Either[SemanticError, Op[From, To, R, CertifiedPsd]] =
    certify(unchecked, certificate.runtime)

  def certifiedSpd[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag](
      unchecked: Op[From, To, R, UncheckedEvidence],
      certificate: Certificate[SpdProperty]
  )(using SelfDualPorts[From, To]): Either[SemanticError, Op[From, To, R, CertifiedSpd]] =
    certify(unchecked, certificate.runtime)

  def lowRank[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag](
      left: DMat,
      right: DMat,
      domain: CoordinateEvidence[From],
      codomain: CoordinateEvidence[To],
      role: OperatorRoleWitness[R],
      valueIdentity: ValueIdentity
  ): Either[SemanticError, Op[From, To, R, UncheckedEvidence]] =
    if left.cols != right.cols then
      Left(SemanticError.OperatorShapeMismatch(left.rows, left.cols, right.rows, right.cols))
    else fromKernel(LowRankKernel(left, right), domain, codomain, role, valueIdentity, SemanticProvenance.source("low-rank-operator"))

  private def certify[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      unchecked: Op[From, To, R, UncheckedEvidence],
      numerical: NumericalCertificate
  ): Either[SemanticError, Op[From, To, R, E]] =
    OperatorCertificate.certified[E](unchecked.valueIdentity, numerical).map: certificate =>
      new Op(
        unchecked.kernel,
        unchecked.domain,
        unchecked.codomain,
        unchecked.role,
        certificate,
        unchecked.valueIdentity,
        unchecked.provenance.append(SemanticProvenanceEvent.Certified(numerical.claim.property, numerical.context.method))
      )

  private[multivar] def assume[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      unchecked: Op[From, To, R, UncheckedEvidence],
      property: String,
      reason: String
  )(using SelfDualPorts[From, To]): Either[SemanticError, Op[From, To, R, E]] =
    val cleanReason = reason.trim
    if cleanReason.isEmpty then
      Left(SemanticError.InvalidCertificateMetadata("unsafe assumptions require a non-empty reason"))
    else
      Right(
        new Op(
          unchecked.kernel,
          unchecked.domain,
          unchecked.codomain,
          unchecked.role,
          OperatorCertificate.assumed[E](unchecked.valueIdentity),
          unchecked.valueIdentity,
          unchecked.provenance.append(SemanticProvenanceEvent.UnsafeAssumption(property, cleanReason))
        )
      )

  private def fromKernel[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag
  ](
      kernel: SemanticKernel,
      domain: CoordinateEvidence[From],
      codomain: CoordinateEvidence[To],
      role: OperatorRoleWitness[R],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[SemanticError, Op[From, To, R, UncheckedEvidence]] =
    if kernel.rows != codomain.dimension || kernel.cols != domain.dimension then
      Left(SemanticError.OperatorShapeMismatch(codomain.dimension, domain.dimension, kernel.rows, kernel.cols))
    else
      Right(
        new Op(
          kernel,
          domain,
          codomain,
          role,
          OperatorCertificate.unchecked(valueIdentity),
          valueIdentity,
          provenance
        )
      )

private[multivar] final case class LowRankKernel(left: DMat, right: DMat) extends SemanticKernel:
  private val operator = LowRankLinearOperator(left, right)
  override def rows: Int = left.rows
  override def cols: Int = right.rows
  override def representation: OperatorRepresentation = OperatorRepresentation.LowRank
  override def linearMap: DoubleLinearOperator = operator
  override def forward(input: DMat): Either[SemanticError, DMat] =
    Right(left * (right.t * input))
  override def adjoint: SemanticKernel = LowRankKernel(right, left)

private final case class LowRankLinearOperator(left: DMat, right: DMat) extends DoubleLinearOperator:
  override def rows: Int = left.rows
  override def cols: Int = right.rows
  override def applyTo(input: DVec, output: MutableDVec): Unit =
    multiply(left, right, input, output)
  override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
    multiply(right, left, input, output)

  private def multiply(a: DMat, b: DMat, input: DVec, output: MutableDVec): Unit =
    if input.length != b.rows then throw LinAlgError.VectorLengthMismatch(b.rows, input.length)
    if output.length != a.rows then throw LinAlgError.VectorLengthMismatch(a.rows, output.length)
    val latent = new Array[Double](b.cols)
    var component = 0
    while component < b.cols do
      var row = 0
      while row < b.rows do
        latent(component) += b(row, component) * input(row)
        row += 1
      component += 1
    var row = 0
    while row < a.rows do
      var value = 0.0
      component = 0
      while component < a.cols do
        value += a(row, component) * latent(component)
        component += 1
      output(row) = value
      row += 1

type OpTable[Rows <: SemanticSpace, Columns <: SemanticSpace, E <: OperatorEvidence] =
  Op[Dual[Columns], Primal[Rows], TableOperatorRole, E]
type OpMetric[S <: SemanticSpace, E <: SpdEvidence] =
  Op[Primal[S], Dual[S], MetricOperatorRole, E]
type OpCometric[S <: SemanticSpace, E <: SpdEvidence] =
  Op[Dual[S], Primal[S], CometricOperatorRole, E]
type OpCovariance[S <: SemanticSpace, E <: PsdEvidence] =
  Op[Dual[S], Primal[S], CovarianceOperatorRole, E]
type OpScatter[S <: SemanticSpace, E <: SymmetricEvidence] =
  Op[Dual[S], Primal[S], ScatterOperatorRole, E]
type OpRowLink[Source <: SemanticSpace, Target <: SemanticSpace, E <: OperatorEvidence] =
  Op[Primal[Target], Dual[Source], RowLinkOperatorRole, E]
type OpFrame[Feature <: SemanticSpace, Component <: SemanticSpace, E <: OperatorEvidence] =
  Op[Primal[Component], Dual[Feature], FrameOperatorRole, E]
type OpCoefficient[
    SourceFeature <: SemanticSpace,
    TargetFeature <: SemanticSpace,
    E <: OperatorEvidence
] =
  Op[Dual[TargetFeature], Dual[SourceFeature], CoefficientOperatorRole, E]
type OpConstraint[S <: SemanticSpace, E <: OperatorEvidence] =
  Op[Primal[S], Primal[S], ConstraintOperatorRole, E]

object OperatorAlgebra:
  def secondOrder[
      SourceRows <: SemanticSpace,
      TargetRows <: SemanticSpace,
      SourceFeatures <: SemanticSpace,
      TargetFeatures <: SemanticSpace,
      EX <: OperatorEvidence,
      EL <: OperatorEvidence,
      ET <: OperatorEvidence
  ](
      source: OpTable[SourceRows, SourceFeatures, EX],
      relationship: OpRowLink[SourceRows, TargetRows, EL],
      target: OpTable[TargetRows, TargetFeatures, ET]
  ): Op[Dual[TargetFeatures], Primal[SourceFeatures], CrossOperatorRole, UncheckedEvidence] =
    target
      .andThen(relationship)
      .andThen(source.dual)
      .retag(OperatorRoleWitness.cross, "second-order")

  def compress[
      SourceFeatures <: SemanticSpace,
      TargetFeatures <: SemanticSpace,
      SourceComponents <: SemanticSpace,
      TargetComponents <: SemanticSpace,
      EWS <: OperatorEvidence,
      ES <: OperatorEvidence,
      EWT <: OperatorEvidence,
      RS <: OperatorRoleTag
  ](
      sourceFrame: OpFrame[SourceFeatures, SourceComponents, EWS],
      secondOrder: Op[Dual[TargetFeatures], Primal[SourceFeatures], RS, ES],
      targetFrame: OpFrame[TargetFeatures, TargetComponents, EWT]
  ): Op[Primal[TargetComponents], Dual[SourceComponents], ComponentOperatorRole, UncheckedEvidence] =
    targetFrame
      .andThen(secondOrder)
      .andThen(sourceFrame.dual)
      .retag(OperatorRoleWitness.component, "compress")

  def metricAdjoint[
      Source <: SemanticSpace,
      Target <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence,
      ED <: SpdEvidence,
      EC <: SpdEvidence
  ](
      operator: Op[Primal[Source], Primal[Target], R, E],
      domainMetric: OpMetric[Source, ED],
      codomainMetric: OpMetric[Target, EC]
  ): Either[SemanticError, Op[Primal[Target], Primal[Source], MetricAdjointOperatorRole[R], UncheckedEvidence]] =
    for
      dual <- operator.dual.toDense
      codomain <- codomainMetric.toDense
      domain <- domainMetric.toDense
      factor <- domain
        .cholesky(CholeskyOptions())
        .left
        .map(SemanticError.LinearMapFailure.apply)
      result <- factor
        .solve(dual * codomain)
        .left
        .map(SemanticError.LinearMapFailure.apply)
      out <- Op.fromDense(
        result,
        operator.codomain,
        operator.domain,
        OperatorRoleWitness.derived[MetricAdjointOperatorRole[R]](OperatorRole.MetricAdjoint(operator.role.value)),
        ValueIdentity.derived("metric-adjoint", operator.valueIdentity, domainMetric.valueIdentity, codomainMetric.valueIdentity),
        operator.provenance.append(
          SemanticProvenanceEvent.Derived(
            "metric-adjoint",
            Vector(operator.valueIdentity, domainMetric.valueIdentity, codomainMetric.valueIdentity)
          )
        )
      )
    yield out

final case class FunctionalFrame[
    Feature <: SemanticSpace,
    Component <: SemanticSpace,
    E <: OperatorEvidence
] private (
    weights: OpFrame[Feature, Component, E],
    cometric: Option[OpCometric[Feature, ? <: SpdEvidence]]
):
  def scores[Rows <: SemanticSpace, ET <: OperatorEvidence](
      table: OpTable[Rows, Feature, ET]
  ): Op[Primal[Component], Primal[Rows], ScoreOperatorRole, UncheckedEvidence] =
    weights.andThen(table).retag(OperatorRoleWitness.score, "frame-scores")

  def axes: Option[Op[Primal[Component], Primal[Feature], AxisOperatorRole, UncheckedEvidence]] =
    cometric.map(metric => weights.andThen(metric).retag(OperatorRoleWitness.axis, "frame-axes"))

object FunctionalFrame:
  def apply[Feature <: SemanticSpace, Component <: SemanticSpace, E <: OperatorEvidence](
      weights: OpFrame[Feature, Component, E],
      cometric: Option[OpCometric[Feature, ? <: SpdEvidence]] = None
  ): FunctionalFrame[Feature, Component, E] =
    new FunctionalFrame(weights, cometric)
