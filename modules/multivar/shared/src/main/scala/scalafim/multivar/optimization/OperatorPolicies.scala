package scalafim.multivar
package optimization

import scalafim.multivar.core.*

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}

opaque type PolicyId = String

object PolicyId:
  def apply(value: String): Either[OperatorPolicyError, PolicyId] =
    val clean = value.trim
    if clean.nonEmpty then Right(clean)
    else Left(OperatorPolicyError.InvalidDefinition("policy id must be non-empty"))

  def unsafe(value: String): PolicyId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (value: PolicyId)
    inline def stringValue: String = value

enum PolicySelection:
  case Fixed(strength: UnitFraction)
  case FoldSelected(hook: PolicySelectionHook)

final case class PolicySelectionHook private (
    id: PolicyId,
    candidates: Vector[UnitFraction]
)

object PolicySelectionHook:
  def from(id: PolicyId, candidates: Vector[UnitFraction]): Either[OperatorPolicyError, PolicySelectionHook] =
    if candidates.isEmpty || candidates.map(_.value).distinct.length != candidates.length then
      Left(OperatorPolicyError.InvalidDefinition("fold-selected policy requires distinct non-empty candidates"))
    else Right(PolicySelectionHook(id, candidates))

enum ScaleMatching:
  case None
  case MatchTrace
  case MatchDiagonalMean
  case Fixed(value: Double)

enum PolicyScope:
  case SingleOperator
  case JointSystem
  case BlockwiseUnsafe

enum PreservationClaim:
  case PsdPreserved
  case SpdPreserved
  case BlockAdjointsPreserved
  case SharedGaugePreserved
  case SupportRestricted
  case GaugeFixed
  case EvidenceDowngraded(reason: String)

final case class OperatorPolicyRecord(
    id: PolicyId,
    kind: String,
    inputIdentities: Vector[ValueIdentity],
    outputIdentities: Vector[ValueIdentity],
    selection: PolicySelection,
    scaleMatching: ScaleMatching,
    scope: PolicyScope,
    preservation: Vector[PreservationClaim],
    provenance: SemanticProvenance
)

enum OperatorPolicyError:
  case InvalidDefinition(reason: String)
  case RequiresFoldSelection(hook: PolicySelectionHook)
  case ScaleMatchingFailed(reason: String)
  case PsdRepairFailed(reason: String)
  case JointPsdRejected(minimumEigenvalue: Double, threshold: Double)
  case Semantic(error: SemanticError)

  def message: String =
    this match
      case InvalidDefinition(reason) => reason
      case RequiresFoldSelection(hook) => s"policy selection '${hook.id.stringValue}' must be fitted inside a training fold"
      case ScaleMatchingFailed(reason) => reason
      case PsdRepairFailed(reason) => reason
      case JointPsdRejected(minimum, threshold) =>
        s"joint block covariance minimum eigenvalue $minimum is below -$threshold"
      case Semantic(error) => error.message

final case class OperatorPolicyResult[
    From <: Coordinate,
    To <: Coordinate,
    R <: OperatorRoleTag,
    E <: OperatorEvidence
](
    operator: Op[From, To, R, E],
    record: OperatorPolicyRecord
)

object LinearShrinkagePolicy:
  def apply[
      Space <: SemanticSpace,
      R <: OperatorRoleTag,
      RT <: OperatorRoleTag,
      E <: CertifiedPsd,
      ET <: CertifiedPsd
  ](
      id: PolicyId,
      input: Op[Dual[Space], Primal[Space], R, E],
      target: Op[Dual[Space], Primal[Space], RT, ET],
      selection: PolicySelection,
      scaleMatching: ScaleMatching
  ): Either[
    OperatorPolicyError,
    OperatorPolicyResult[Dual[Space], Primal[Space], R, CertifiedPsd]
  ] =
    selection match
      case PolicySelection.FoldSelected(hook) => Left(OperatorPolicyError.RequiresFoldSelection(hook))
      case PolicySelection.Fixed(strength) =>
        for
          inputDense <- input.toDense.left.map(OperatorPolicyError.Semantic.apply)
          targetDense <- target.toDense.left.map(OperatorPolicyError.Semantic.apply)
          scale <- matchingScale(inputDense, targetDense, scaleMatching)
          mixed = MatrixOps.subtract(
            MatrixOps.scale(inputDense, 1.0 - strength.value),
            MatrixOps.scale(targetDense, -strength.value * scale)
          )
          identity = ValueIdentity.derived("linear-shrinkage", input.valueIdentity, target.valueIdentity)
          unchecked <- Op
            .fromDense(mixed, input.domain, input.codomain, input.role, identity)
            .left
            .map(OperatorPolicyError.Semantic.apply)
          certified <- certifyPsd(unchecked, "linear-shrinkage").left.map(OperatorPolicyError.Semantic.apply)
        yield
          OperatorPolicyResult(
            certified,
            OperatorPolicyRecord(
              id,
              "linear-shrinkage",
              Vector(input.valueIdentity, target.valueIdentity),
              Vector(certified.valueIdentity),
              selection,
              scaleMatching,
              PolicyScope.SingleOperator,
              Vector(PreservationClaim.PsdPreserved),
              (input.provenance ++ target.provenance).append(
                SemanticProvenanceEvent.Derived("linear-shrinkage", Vector(input.valueIdentity, target.valueIdentity))
              )
            )
          )

  private def matchingScale(
      input: DMat,
      target: DMat,
      matching: ScaleMatching
  ): Either[OperatorPolicyError, Double] =
    matching match
      case ScaleMatching.None => Right(1.0)
      case ScaleMatching.Fixed(value) =>
        if value.isFinite && value > 0.0 then Right(value)
        else Left(OperatorPolicyError.ScaleMatchingFailed(s"fixed target scale must be finite and positive, got $value"))
      case ScaleMatching.MatchTrace | ScaleMatching.MatchDiagonalMean =>
        val denominator = trace(target)
        if denominator == 0.0 || !denominator.isFinite then
          Left(OperatorPolicyError.ScaleMatchingFailed("target trace must be finite and non-zero"))
        else Right(trace(input) / denominator)

  private def trace(value: DMat): Double =
    var result = 0.0
    var index = 0
    while index < Math.min(value.rows, value.cols) do
      result += value(index, index)
      index += 1
    result

object PsdRepairPolicy:
  def nearestPsd[
      Space <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      id: PolicyId,
      input: Op[Dual[Space], Primal[Space], R, E],
      tolerance: CertificateTolerance
  ): Either[
    OperatorPolicyError,
    OperatorPolicyResult[Dual[Space], Primal[Space], R, CertifiedPsd]
  ] =
    for
      dense <- input.toDense.left.map(OperatorPolicyError.Semantic.apply)
      symmetric = MatrixOps.scale(dense + dense.t, 0.5)
      eigen <- DenseSolvers.symmetricEigen
        .decompose(symmetric)
        .left
        .map(error => OperatorPolicyError.PsdRepairFailed(error.getMessage))
      clipped = clip(eigen.values)
      repaired = eigen.vectors * MatrixOps.diagonal(clipped) * eigen.vectors.t
      identity = ValueIdentity.derived("nearest-psd-repair", input.valueIdentity)
      unchecked <- Op
        .fromDense(repaired, input.domain, input.codomain, input.role, identity)
        .left
        .map(OperatorPolicyError.Semantic.apply)
      certified <- certifyPsd(unchecked, "nearest-psd-repair", tolerance).left.map(OperatorPolicyError.Semantic.apply)
    yield
      val minimum = vectorMinimum(eigen.values)
      OperatorPolicyResult(
        certified,
        OperatorPolicyRecord(
          id,
          "nearest-psd-repair",
          Vector(input.valueIdentity),
          Vector(certified.valueIdentity),
          PolicySelection.Fixed(UnitFraction.unsafe(1.0)),
          ScaleMatching.None,
          PolicyScope.SingleOperator,
          Vector(
            PreservationClaim.PsdPreserved,
            PreservationClaim.EvidenceDowngraded(
              s"input evidence ${input.certificate.status}; clipped minimum $minimum at tolerance ${tolerance.absolute}/${tolerance.relative}"
            )
          ),
          input.provenance.append(SemanticProvenanceEvent.Derived("nearest-psd-repair", Vector(input.valueIdentity)))
        )
      )

  private def clip(values: DVec): DVec =
    val output = new Array[Double](values.length)
    var index = 0
    while index < values.length do
      output(index) = Math.max(0.0, values(index))
      index += 1
    GaleNumerics.vectorFromArray(output)

  private def vectorMinimum(values: DVec): Double =
    var result = Double.PositiveInfinity
    var index = 0
    while index < values.length do
      result = Math.min(result, values(index))
      index += 1
    result

object SupportRestrictionPolicy:
  def apply[
      Full <: SemanticSpace,
      Supported <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: CertifiedPsd,
      RE <: OperatorRoleTag,
      EE <: OperatorEvidence
  ](
      id: PolicyId,
      input: Op[Dual[Full], Primal[Full], R, E],
      embedding: Op[Dual[Supported], Dual[Full], RE, EE]
  ): Either[
    OperatorPolicyError,
    OperatorPolicyResult[Dual[Supported], Primal[Supported], R, CertifiedPsd]
  ] =
    val raw = embedding.andThen(input).andThen(embedding.dual).retag(input.role, "support-restriction")
    certifyPsd(raw, "support-restriction").left.map(OperatorPolicyError.Semantic.apply).map: certified =>
      OperatorPolicyResult(
        certified,
        OperatorPolicyRecord(
          id,
          "support-restriction",
          Vector(input.valueIdentity, embedding.valueIdentity),
          Vector(certified.valueIdentity),
          PolicySelection.Fixed(UnitFraction.unsafe(1.0)),
          ScaleMatching.None,
          PolicyScope.SingleOperator,
          Vector(PreservationClaim.PsdPreserved, PreservationClaim.SupportRestricted),
          (input.provenance ++ embedding.provenance).append(
            SemanticProvenanceEvent.Derived("support-restriction", Vector(input.valueIdentity, embedding.valueIdentity))
          )
        )
      )

object GaugeFixingPolicy:
  def traceOne[
      Space <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: CertifiedPsd
  ](
      id: PolicyId,
      input: Op[Dual[Space], Primal[Space], R, E]
  ): Either[
    OperatorPolicyError,
    OperatorPolicyResult[Dual[Space], Primal[Space], R, CertifiedPsd]
  ] =
    input.toDense.left.map(OperatorPolicyError.Semantic.apply).flatMap: dense =>
      val currentTrace = diagonalTrace(dense)
      if !currentTrace.isFinite || currentTrace <= 0.0 then
        Left(OperatorPolicyError.ScaleMatchingFailed("trace gauge requires finite positive trace"))
      else
        val identity = ValueIdentity.derived("trace-one-gauge", input.valueIdentity)
        val raw = new Op(
          ScaledPolicyKernel(input.kernel, 1.0 / currentTrace),
          input.domain,
          input.codomain,
          input.role,
          OperatorCertificate.unchecked(identity),
          identity,
          input.provenance.append(SemanticProvenanceEvent.Derived("trace-one-gauge", Vector(input.valueIdentity)))
        )
        certifyPsd(raw, "trace-one-gauge").left.map(OperatorPolicyError.Semantic.apply).map: certified =>
          OperatorPolicyResult(
            certified,
            OperatorPolicyRecord(
              id,
              "trace-one-gauge",
              Vector(input.valueIdentity),
              Vector(certified.valueIdentity),
              PolicySelection.Fixed(UnitFraction.unsafe(1.0)),
              ScaleMatching.Fixed(1.0 / currentTrace),
              PolicyScope.SingleOperator,
              Vector(PreservationClaim.PsdPreserved, PreservationClaim.GaugeFixed),
              certified.provenance
            )
          )

private def diagonalTrace(value: DMat): Double =
  var result = 0.0
  var index = 0
  while index < Math.min(value.rows, value.cols) do
    result += value(index, index)
    index += 1
  result

private def certifyPsd[
    Space <: SemanticSpace,
    R <: OperatorRoleTag
](
    value: Op[Dual[Space], Primal[Space], R, UncheckedEvidence],
    method: String,
    tolerance: CertificateTolerance = CertificateTolerance.strict
): Either[SemanticError, Op[Dual[Space], Primal[Space], R, CertifiedPsd]] =
  CertificateContext
    .from(
      tolerance,
      CertificateNorm.Frobenius,
      method,
      "operator-policy",
      NumericalPrecision.Float64
    )
    .flatMap: context =>
      Op.certifiedPsd(
        value,
        Certificate.unsafe[PsdProperty](
          value.valueIdentity,
          CertificateClaim.PositiveSemidefinite(0.0, 0.0, 1.0),
          context
        )
      )

object LdaWithinScatterPolicy:
  def shrink[
      Space <: SemanticSpace,
      E <: CertifiedPsd,
      ET <: CertifiedPsd
  ](
      id: PolicyId,
      within: Op[Dual[Space], Primal[Space], ScatterOperatorRole, E],
      target: Op[Dual[Space], Primal[Space], CometricOperatorRole, ET],
      selection: PolicySelection,
      scaleMatching: ScaleMatching
  ): Either[
    OperatorPolicyError,
    OperatorPolicyResult[Dual[Space], Primal[Space], ScatterOperatorRole, CertifiedPsd]
  ] =
    LinearShrinkagePolicy(id, within, target, selection, scaleMatching).map: result =>
      result.copy(record = result.record.copy(kind = "lda-within-scatter-shrinkage"))

private[multivar] final case class ScaledPolicyKernel(input: SemanticKernel, scale: Double) extends SemanticKernel:
  private val operator = ScaledPolicyLinearOperator(input.linearMap, scale)
  val rows: Int = input.rows
  val cols: Int = input.cols
  val representation: OperatorRepresentation = input.representation
  val linearMap: DoubleLinearOperator = operator
  def forward(value: DMat): Either[SemanticError, DMat] =
    input.forward(value).map(MatrixOps.scale(_, scale))
  def adjoint: SemanticKernel = ScaledPolicyKernel(input.adjoint, scale)

private final case class ScaledPolicyLinearOperator(input: DoubleLinearOperator, scale: Double)
    extends DoubleLinearOperator:
  val rows: Int = input.rows
  val cols: Int = input.cols
  def applyTo(value: DVec, output: MutableDVec): Unit =
    input.applyTo(value, output)
    scaleOutput(output)
  override def transposeApplyTo(value: DVec, output: MutableDVec): Unit =
    input.transposeApplyTo(value, output)
    scaleOutput(output)
  private def scaleOutput(output: MutableDVec): Unit =
    var index = 0
    while index < output.length do
      output(index) *= scale
      index += 1

final case class JointPsdCertificate(
    minimumEigenvalue: Double,
    tolerance: CertificateTolerance,
    blockAdjointResidual: Double,
    gauge: ValueIdentity
)

final class JointBlockCovariance[X <: SemanticSpace, Y <: SemanticSpace] private (
    val xx: Op[Dual[X], Primal[X], CovarianceOperatorRole, CertifiedPsd],
    val xy: Op[Dual[Y], Primal[X], CrossOperatorRole, UncheckedEvidence],
    val yy: Op[Dual[Y], Primal[Y], CovarianceOperatorRole, CertifiedPsd],
    val certificate: JointPsdCertificate,
    val provenance: SemanticProvenance
):
  def yx: Op[Dual[X], Primal[Y], ? <: OperatorRoleTag, UncheckedEvidence] = xy.dual

  def dense: Either[OperatorPolicyError, DMat] =
    for
      left <- xx.toDense.left.map(OperatorPolicyError.Semantic.apply)
      cross <- xy.toDense.left.map(OperatorPolicyError.Semantic.apply)
      right <- yy.toDense.left.map(OperatorPolicyError.Semantic.apply)
    yield block(left, cross, cross.t, right)

object JointBlockCovariance:
  def from[
      X <: SemanticSpace,
      Y <: SemanticSpace,
      EX <: CertifiedPsd,
      EC <: OperatorEvidence,
      EY <: CertifiedPsd
  ](
      xx: Op[Dual[X], Primal[X], CovarianceOperatorRole, EX],
      xy: Op[Dual[Y], Primal[X], CrossOperatorRole, EC],
      yy: Op[Dual[Y], Primal[Y], CovarianceOperatorRole, EY],
      gauge: ValueIdentity,
      tolerance: CertificateTolerance = CertificateTolerance.strict
  ): Either[OperatorPolicyError, JointBlockCovariance[X, Y]] =
    for
      left <- xx.toDense.left.map(OperatorPolicyError.Semantic.apply)
      cross <- xy.toDense.left.map(OperatorPolicyError.Semantic.apply)
      right <- yy.toDense.left.map(OperatorPolicyError.Semantic.apply)
      joint = block(left, cross, cross.t, right)
      eigen <- DenseSolvers.symmetricEigen
        .decompose(joint)
        .left
        .map(error => OperatorPolicyError.PsdRepairFailed(error.getMessage))
      minimum = minimumEigenvalue(eigen.values)
      _ <-
        if minimum >= -tolerance.threshold(1.0) then Right(())
        else Left(OperatorPolicyError.JointPsdRejected(minimum, tolerance.threshold(1.0)))
      certifiedX <- covariance(xx, "joint-xx")
      certifiedY <- covariance(yy, "joint-yy")
      erasedCross = crossOperator(xy, "joint-xy")
    yield
      new JointBlockCovariance(
        certifiedX,
        erasedCross,
        certifiedY,
        JointPsdCertificate(minimum, tolerance, 0.0, gauge),
        (xx.provenance ++ xy.provenance ++ yy.provenance).append(
          SemanticProvenanceEvent.Derived("joint-block-covariance", Vector(xx.valueIdentity, xy.valueIdentity, yy.valueIdentity))
        )
      )

  def shrink[X <: SemanticSpace, Y <: SemanticSpace](
      id: PolicyId,
      input: JointBlockCovariance[X, Y],
      target: JointBlockCovariance[X, Y],
      selection: PolicySelection
  ): Either[OperatorPolicyError, (JointBlockCovariance[X, Y], OperatorPolicyRecord)] =
    selection match
      case PolicySelection.FoldSelected(hook) => Left(OperatorPolicyError.RequiresFoldSelection(hook))
      case PolicySelection.Fixed(strength) =>
        for
          inputDense <- input.dense
          targetDense <- target.dense
          mixed = MatrixOps.subtract(
            MatrixOps.scale(inputDense, 1.0 - strength.value),
            MatrixOps.scale(targetDense, -strength.value)
          )
          derivationInputs = Vector(
            input.xx.valueIdentity,
            input.xy.valueIdentity,
            input.yy.valueIdentity,
            target.xx.valueIdentity,
            target.xy.valueIdentity,
            target.yy.valueIdentity
          )
          result <- fromDenseBlocks(
            input,
            mixed,
            ValueIdentity.derived(s"joint-shrinkage-gauge-${strength.value}", derivationInputs*),
            derivationInputs,
            strength
          )
        yield
          result -> OperatorPolicyRecord(
            id,
            "joint-block-shrinkage",
            Vector(input.xx.valueIdentity, input.xy.valueIdentity, input.yy.valueIdentity),
            Vector(result.xx.valueIdentity, result.xy.valueIdentity, result.yy.valueIdentity),
            selection,
            ScaleMatching.None,
            PolicyScope.JointSystem,
            Vector(
              PreservationClaim.PsdPreserved,
              PreservationClaim.BlockAdjointsPreserved,
              PreservationClaim.SharedGaugePreserved
            ),
            (input.provenance ++ target.provenance).append(
              SemanticProvenanceEvent.Derived("joint-block-shrinkage", derivationInputs)
            )
          )

  def blockwiseUnsafe[X <: SemanticSpace, Y <: SemanticSpace](
      id: PolicyId,
      input: JointBlockCovariance[X, Y],
      marginalStrength: UnitFraction,
      crossStrength: UnitFraction
  ): Either[OperatorPolicyError, UnsafeBlockPolicyResult[X, Y]] =
    for
      xx <- input.xx.toDense.left.map(OperatorPolicyError.Semantic.apply)
      xy <- input.xy.toDense.left.map(OperatorPolicyError.Semantic.apply)
      yy <- input.yy.toDense.left.map(OperatorPolicyError.Semantic.apply)
      adjustedX = MatrixOps.subtract(MatrixOps.scale(xx, 1.0 - marginalStrength.value), MatrixOps.scale(DMat.eye(xx.rows), -marginalStrength.value))
      adjustedY = MatrixOps.subtract(MatrixOps.scale(yy, 1.0 - marginalStrength.value), MatrixOps.scale(DMat.eye(yy.rows), -marginalStrength.value))
      adjustedCross = MatrixOps.scale(xy, 1.0 - crossStrength.value)
      derivationInputs = Vector(input.xx.valueIdentity, input.xy.valueIdentity, input.yy.valueIdentity)
      identityTag = s"unsafe-block-${id.stringValue}-${marginalStrength.value}-${crossStrength.value}"
      x <- covarianceDense(
        adjustedX,
        input.xx.domain,
        input.xx.codomain,
        ValueIdentity.derived(s"$identityTag-xx", derivationInputs*),
        "unsafe-block-xx"
      )
      y <- covarianceDense(
        adjustedY,
        input.yy.domain,
        input.yy.codomain,
        ValueIdentity.derived(s"$identityTag-yy", derivationInputs*),
        "unsafe-block-yy"
      )
      cross <- crossDense(
        adjustedCross,
        input.xy.domain,
        input.xy.codomain,
        ValueIdentity.derived(s"$identityTag-xy", derivationInputs*),
        "unsafe-block-xy"
      )
    yield
      UnsafeBlockPolicyResult(
        x,
        cross,
        y,
        OperatorPolicyRecord(
          id,
          "blockwise-shrinkage",
          Vector(input.xx.valueIdentity, input.xy.valueIdentity, input.yy.valueIdentity),
          Vector(x.valueIdentity, cross.valueIdentity, y.valueIdentity),
          PolicySelection.Fixed(marginalStrength),
          ScaleMatching.None,
          PolicyScope.BlockwiseUnsafe,
          Vector(
            PreservationClaim.BlockAdjointsPreserved,
            PreservationClaim.EvidenceDowngraded(
              s"independent marginal ${marginalStrength.value} and cross ${crossStrength.value} strengths do not prove joint PSD or shared gauge"
            )
          ),
          input.provenance.append(SemanticProvenanceEvent.Derived("blockwise-shrinkage", Vector(input.xx.valueIdentity, input.xy.valueIdentity, input.yy.valueIdentity)))
        )
      )

  private def fromDenseBlocks[X <: SemanticSpace, Y <: SemanticSpace](
      template: JointBlockCovariance[X, Y],
      dense: DMat,
      gauge: ValueIdentity,
      derivationInputs: Vector[ValueIdentity],
      strength: UnitFraction
  ): Either[OperatorPolicyError, JointBlockCovariance[X, Y]] =
    val xSize = template.xx.rows
    val ySize = template.yy.rows
    val xx = slice(dense, 0, xSize, 0, xSize)
    val xy = slice(dense, 0, xSize, xSize, xSize + ySize)
    val yy = slice(dense, xSize, xSize + ySize, xSize, xSize + ySize)
    for
      x <- covarianceDense(
        xx,
        template.xx.domain,
        template.xx.codomain,
        ValueIdentity.derived(s"joint-shrinkage-xx-${strength.value}", derivationInputs*),
        "joint-shrinkage-xx"
      )
      y <- covarianceDense(
        yy,
        template.yy.domain,
        template.yy.codomain,
        ValueIdentity.derived(s"joint-shrinkage-yy-${strength.value}", derivationInputs*),
        "joint-shrinkage-yy"
      )
      cross <- crossDense(
        xy,
        template.xy.domain,
        template.xy.codomain,
        ValueIdentity.derived(s"joint-shrinkage-xy-${strength.value}", derivationInputs*),
        "joint-shrinkage-xy"
      )
      result <- from(x, cross, y, gauge)
    yield result

  private def covariance[X <: SemanticSpace, E <: CertifiedPsd](
      value: Op[Dual[X], Primal[X], CovarianceOperatorRole, E],
      operation: String
  ): Either[OperatorPolicyError, Op[Dual[X], Primal[X], CovarianceOperatorRole, CertifiedPsd]] =
    value.toDense.left.map(OperatorPolicyError.Semantic.apply).flatMap: dense =>
      covarianceDense(
        dense,
        value.domain,
        value.codomain,
        ValueIdentity.derived(operation, value.valueIdentity),
        operation
      )

  private def covarianceDense[X <: SemanticSpace](
      dense: DMat,
      domain: CoordinateEvidence[Dual[X]],
      codomain: CoordinateEvidence[Primal[X]],
      identity: ValueIdentity,
      operation: String
  ): Either[OperatorPolicyError, Op[Dual[X], Primal[X], CovarianceOperatorRole, CertifiedPsd]] =
    Op
      .fromDense(dense, domain, codomain, OperatorRoleWitness.covariance, identity)
      .left
      .map(OperatorPolicyError.Semantic.apply)
      .flatMap(value => certifyPsd(value, operation).left.map(OperatorPolicyError.Semantic.apply))

  private def crossOperator[X <: SemanticSpace, Y <: SemanticSpace, E <: OperatorEvidence](
      value: Op[Dual[Y], Primal[X], CrossOperatorRole, E],
      operation: String
  ): Op[Dual[Y], Primal[X], CrossOperatorRole, UncheckedEvidence] =
    val identity = ValueIdentity.derived(operation, value.valueIdentity)
    new Op(
      value.kernel,
      value.domain,
      value.codomain,
      value.role,
      OperatorCertificate.unchecked(identity),
      identity,
      value.provenance.append(SemanticProvenanceEvent.Derived(operation, Vector(value.valueIdentity)))
    )

  private def crossDense[X <: SemanticSpace, Y <: SemanticSpace](
      dense: DMat,
      domain: CoordinateEvidence[Dual[Y]],
      codomain: CoordinateEvidence[Primal[X]],
      identity: ValueIdentity,
      operation: String
  ): Either[OperatorPolicyError, Op[Dual[Y], Primal[X], CrossOperatorRole, UncheckedEvidence]] =
    Op.fromDense(
      dense,
      domain,
      codomain,
      OperatorRoleWitness.cross,
      identity
    ).left.map(OperatorPolicyError.Semantic.apply)

final case class UnsafeBlockPolicyResult[X <: SemanticSpace, Y <: SemanticSpace](
    xx: Op[Dual[X], Primal[X], CovarianceOperatorRole, CertifiedPsd],
    xy: Op[Dual[Y], Primal[X], CrossOperatorRole, UncheckedEvidence],
    yy: Op[Dual[Y], Primal[Y], CovarianceOperatorRole, CertifiedPsd],
    record: OperatorPolicyRecord
):
  def yx: Op[Dual[X], Primal[Y], ? <: OperatorRoleTag, UncheckedEvidence] = xy.dual

private def block(xx: DMat, xy: DMat, yx: DMat, yy: DMat): DMat =
  val rows = xx.rows + yy.rows
  val columns = xx.cols + yy.cols
  val values = new Array[Double](rows * columns)
  copyBlock(xx, values, columns, 0, 0)
  copyBlock(xy, values, columns, 0, xx.cols)
  copyBlock(yx, values, columns, xx.rows, 0)
  copyBlock(yy, values, columns, xx.rows, xx.cols)
  GaleNumerics.matrixFromRowMajor(rows, columns, values)

private def copyBlock(value: DMat, output: Array[Double], stride: Int, rowOffset: Int, columnOffset: Int): Unit =
  var row = 0
  while row < value.rows do
    var column = 0
    while column < value.cols do
      output((rowOffset + row) * stride + columnOffset + column) = value(row, column)
      column += 1
    row += 1

private def slice(value: DMat, rowStart: Int, rowEnd: Int, columnStart: Int, columnEnd: Int): DMat =
  val rows = rowEnd - rowStart
  val columns = columnEnd - columnStart
  val output = new Array[Double](rows * columns)
  var row = 0
  while row < rows do
    var column = 0
    while column < columns do
      output(row * columns + column) = value(rowStart + row, columnStart + column)
      column += 1
    row += 1
  GaleNumerics.matrixFromRowMajor(rows, columns, output)

private def minimumEigenvalue(values: DVec): Double =
  var minimum = Double.PositiveInfinity
  var index = 0
  while index < values.length do
    minimum = Math.min(minimum, values(index))
    index += 1
  minimum
