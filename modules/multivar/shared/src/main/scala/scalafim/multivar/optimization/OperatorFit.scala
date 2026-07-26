package scalafim.multivar
package optimization

import scalafim.multivar.core.*

import gale.linalg.DMat

enum DerivedOperatorKind:
  case FunctionalFrame
  case Scores
  case Axes
  case SecondOrder
  case Component
  case Projection
  case Coefficient
  case Constraint

/** Portable semantic snapshot of one fitted or derived operator.
  *
  * The dense payload is an explicit export representation. Domain, codomain,
  * role, source representation, evidence, identity, and provenance remain
  * attached, so importing it cannot silently turn a matrix into a different
  * mathematical object.
  */
final case class OperatorSnapshot private (
    label: String,
    kind: DerivedOperatorKind,
    domain: CoordinateDescriptor,
    codomain: CoordinateDescriptor,
    role: OperatorRole,
    sourceRepresentation: OperatorRepresentation,
    sourceIdentity: ValueIdentity,
    evidence: EvidenceStatus,
    certificates: Vector[NumericalCertificate],
    values: DMat,
    provenance: SemanticProvenance
):
  require(label.nonEmpty, "operator snapshot label must be non-empty")
  require(values.rows == codomain.dimension && values.cols == domain.dimension, "operator snapshot payload must match its ports")
  require(certificates.forall(_.valueIdentity == sourceIdentity), "operator snapshot certificates must describe the source value")

  def lift[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag](
      expectedDomain: CoordinateEvidence[From],
      expectedCodomain: CoordinateEvidence[To],
      expectedRole: OperatorRoleWitness[R]
  ): Either[MultivarError, Op[From, To, R, UncheckedEvidence]] =
    if domain != expectedDomain.descriptor then
      Left(MultivarError.InvalidMap(s"snapshot '$label' domain does not match the requested coordinate"))
    else if codomain != expectedCodomain.descriptor then
      Left(MultivarError.InvalidMap(s"snapshot '$label' codomain does not match the requested coordinate"))
    else if role != expectedRole.value then
      Left(MultivarError.InvalidMap(s"snapshot '$label' role is $role, not ${expectedRole.value}"))
    else
      OperatorFitAdapters.semantic(
        Op.fromDense(
          values,
          expectedDomain,
          expectedCodomain,
          expectedRole,
          ValueIdentity.derived("snapshot-lift", sourceIdentity),
          provenance.append(SemanticProvenanceEvent.Adapted("OperatorSnapshot"))
        )
      )

object OperatorSnapshot:
  def from[
      From <: Coordinate,
      To <: Coordinate,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      label: String,
      kind: DerivedOperatorKind,
      operator: Op[From, To, R, E]
  ): Either[MultivarError, OperatorSnapshot] =
    val cleanLabel = label.trim
    if cleanLabel.isEmpty then Left(MultivarError.InvalidId("operator snapshot label", label, "must be non-empty"))
    else
      OperatorFitAdapters.semantic(operator.toDense).map { dense =>
        OperatorSnapshot(
          cleanLabel,
          kind,
          operator.domain.descriptor,
          operator.codomain.descriptor,
          operator.role.value,
          operator.representation,
          operator.valueIdentity,
          operator.certificate.status,
          operator.certificate.claims,
          dense,
          operator.provenance
        )
      }

final case class FitDiagnostic private (name: String, value: Double, tolerance: Option[Double]):
  require(name.nonEmpty, "fit diagnostic name must be non-empty")
  require(value.isFinite && value >= 0.0, "fit diagnostic value must be finite and non-negative")
  require(tolerance.forall(value => value.isFinite && value >= 0.0), "fit diagnostic tolerance must be finite and non-negative")

object FitDiagnostic:
  def from(name: String, value: Double, tolerance: Option[Double] = None): Either[MultivarError, FitDiagnostic] =
    val clean = name.trim
    if clean.isEmpty then Left(MultivarError.InvalidId("fit diagnostic", name, "must be non-empty"))
    else if !value.isFinite then Left(MultivarError.NonFiniteValue(s"fit diagnostic '$clean'", 0, value))
    else if value < 0.0 then Left(MultivarError.InvalidTolerance(s"fit diagnostic '$clean'", value))
    else if tolerance.exists(value => !value.isFinite || value < 0.0) then
      Left(MultivarError.InvalidTolerance(s"fit diagnostic '$clean'", tolerance.get))
    else Right(FitDiagnostic(clean, value, tolerance))

/** Generic certified fit view shared by every named method. Parameter frames
  * come from `OperatorProgramFit`; derived operators, diagnostics, semantics,
  * and provenance are carried without method-specific result hierarchies.
  */
final case class OperatorFitBundle private (
    programFit: OperatorProgramFit,
    parameterFrames: Vector[OperatorSnapshot],
    derivedOperators: Vector[OperatorSnapshot],
    diagnostics: Vector[FitDiagnostic],
    provenance: SemanticProvenance
):
  require(parameterFrames.nonEmpty, "operator fit bundle must contain at least one parameter frame")
  require(parameterFrames.forall(_.kind == DerivedOperatorKind.FunctionalFrame), "parameter snapshots must be functional frames")
  require((parameterFrames ++ derivedOperators).map(_.label).distinct.length == parameterFrames.length + derivedOperators.length,
    "operator fit bundle labels must be unique")

  def resultSemantics: ResultSemantics =
    programFit.program.resultSemantics

  def operator(label: String): Option[OperatorSnapshot] =
    (parameterFrames ++ derivedOperators).find(_.label == label)

object OperatorFitBundle:
  def from(
      programFit: OperatorProgramFit,
      derivedOperators: Vector[OperatorSnapshot],
      diagnostics: Vector[FitDiagnostic],
      provenance: SemanticProvenance
  ): Either[MultivarError, OperatorFitBundle] =
    val snapshots = Vector.newBuilder[OperatorSnapshot]
    var index = 0
    var error = Option.empty[MultivarError]
    while index < programFit.frames.length && error.isEmpty do
      val fitted = programFit.frames(index)
      OperatorSnapshot.from(
        s"parameter:${fitted.parameter.id.value}",
        DerivedOperatorKind.FunctionalFrame,
        fitted.frame.weights
      ) match
        case Left(value)     => error = Some(value)
        case Right(snapshot) => snapshots += snapshot
      index += 1
    error match
      case Some(value) => Left(value)
      case None =>
        val parameters = snapshots.result()
        val labels = (parameters ++ derivedOperators).map(_.label)
        if labels.distinct.length != labels.length then
          Left(MultivarError.InvalidMap("operator fit bundle labels must be unique"))
        else Right(OperatorFitBundle(programFit, parameters, derivedOperators, diagnostics, provenance))

private[multivar] object OperatorFitAdapters:
  def semantic[A](value: Either[SemanticError, A]): Either[MultivarError, A] =
    value.left.map(error => MultivarError.InvalidMap(error.message))
