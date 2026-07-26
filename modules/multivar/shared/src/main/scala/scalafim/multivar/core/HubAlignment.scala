package scalafim.multivar
package core

import gale.linalg.DMat

final case class AdjointConsistencyCertificate(
    forward: ValueIdentity,
    reverse: ValueIdentity,
    residual: Double,
    context: CertificateContext,
    proof: String
)

final case class HubFactorizationCertificate(
    entitySpace: MvSpace,
    entityForm: ValueIdentity,
    maps: Vector[ValueIdentity],
    proof: String
)

final case class GlobalBlockPsdCertificate(
    entityFormCertificates: Vector[NumericalCertificate],
    factorization: HubFactorizationCertificate,
    proof: String
)

final case class ViewAlignmentReport(
    viewId: BlockId,
    rowSpace: MvSpace,
    support: RelationshipSupport,
    normalization: RelationshipNormalization,
    marginals: Option[CouplingMarginals]
)

final case class StudyAlignmentReport(
    views: Vector[ViewAlignmentReport],
    matchedMass: Double,
    unmatchedMass: Double,
    duplicateKeys: Vector[String],
    entityCoverage: Int,
    entityCount: Int,
    everyEntityRepresented: Boolean
)

sealed trait EntityMapEntry[E <: SemanticSpace]:
  type Rows <: SemanticSpace
  def viewId: BlockId
  def map: TypedRowMap[Rows, E]

object EntityMapEntry:
  def apply[R <: SemanticSpace, E <: SemanticSpace](
      id: BlockId,
      rowMap: TypedRowMap[R, E]
  ): EntityMapEntry[E] { type Rows = R } =
    new EntityMapEntry[E]:
      type Rows = R
      override val viewId: BlockId = id
      override val map: TypedRowMap[R, E] = rowMap

final case class HubLinkedPair[Left <: SemanticSpace, Right <: SemanticSpace](
    leftToRight: TypedRowLink[Left, Right],
    rightToLeft: TypedRowLink[Right, Left],
    adjointCertificate: AdjointConsistencyCertificate
)

object HubAlignment:
  /** Construct `P_s* A_E P_t : O_t -> O_s*` without identifying row spaces by position. */
  def inducedLink[Left <: SemanticSpace, Right <: SemanticSpace, E <: SemanticSpace](
      left: TypedRowMap[Left, E],
      entityForm: DiagramGeometry[E],
      right: TypedRowMap[Right, E]
  ): Either[AlignmentError, TypedRowLink[Left, Right]] =
    if left.descriptor.codomain != entityForm.space.descriptor ||
        right.descriptor.codomain != entityForm.space.descriptor
    then Left(AlignmentError.InvalidRelationship("hub maps and entity form must share one entity space"))
    else
      val operator = right.operator.andThen(entityForm.operator).andThen(left.operator.star)
      for
        matrix <- RelationshipMatrices.matrixOf(operator)
        stats <- RelationshipMatrices.validateMatrix(
          matrix,
          left.descriptor.domain.size,
          right.descriptor.domain.size,
          requireNonnegative = false
        )
      yield
        new TypedRowLink(
          operator,
          DenseMatrixView(matrix),
          RowRelationshipDescriptor(
            RowRelationshipKind.HubInducedLink,
            right.descriptor.domain,
            left.descriptor.domain,
            operator.descriptor,
            stats.support,
            RelationshipNormalization.Unnormalized,
            None,
            AlignmentOrigin.ExternallySupplied,
            operator.valueIdentity
          ),
          (left.provenance ++ right.provenance).append(
            SemanticProvenanceEvent.Derived(
              "hub-induced-row-link",
              Vector(
                left.operator.valueIdentity,
                entityForm.operator.valueIdentity,
                right.operator.valueIdentity
              )
            )
          )
        )

  def inducedPair[Left <: SemanticSpace, Right <: SemanticSpace, E <: SemanticSpace](
      left: TypedRowMap[Left, E],
      entityForm: DiagramGeometry[E],
      right: TypedRowMap[Right, E],
      context: CertificateContext = CertificateContext.portableFloat64
  ): Either[AlignmentError, HubLinkedPair[Left, Right]] =
    inducedLink(left, entityForm, right).map { forward =>
      val reverseOperator = forward.operator.star
      val reverseMatrix = forward.matrix.transposeView
      val reverse = new TypedRowLink(
        reverseOperator,
        reverseMatrix,
        forward.descriptor.copy(
          domain = forward.descriptor.codomain,
          codomain = forward.descriptor.domain,
          orientation = reverseOperator.descriptor,
          valueIdentity = reverseOperator.valueIdentity
        ),
        forward.provenance.append(
          SemanticProvenanceEvent.Derived("hub-link-adjoint", Vector(forward.operator.valueIdentity))
        )
      )
      HubLinkedPair(
        forward,
        reverse,
        AdjointConsistencyCertificate(
          forward.operator.valueIdentity,
          reverse.operator.valueIdentity,
          0.0,
          context,
          "reverse link is constructed as the algebraic adjoint of the hub-factorized forward link"
        )
      )
    }

final class EntityAlignedStudy[E <: SemanticSpace] private (
    val entitySpace: SpaceEvidence[E],
    val entityForm: DiagramGeometry[E],
    val views: Vector[EntityMapEntry[E]],
    val report: StudyAlignmentReport,
    val hubCertificate: HubFactorizationCertificate,
    val globalBlockPsdCertificate: GlobalBlockPsdCertificate,
    val provenance: SemanticProvenance
):
  def inducedLink[Left <: SemanticSpace, Right <: SemanticSpace](
      left: TypedRowMap[Left, E],
      right: TypedRowMap[Right, E]
  ): Either[AlignmentError, TypedRowLink[Left, Right]] =
    HubAlignment.inducedLink(left, entityForm, right)

object EntityAlignedStudy:
  def from[E <: SemanticSpace](
      entitySpace: SpaceEvidence[E],
      entityForm: DiagramGeometry[E],
      views: Vector[EntityMapEntry[E]],
      provenance: SemanticProvenance = SemanticProvenance.source("entity-aligned-study")
  ): Either[AlignmentError, EntityAlignedStudy[E]] =
    if views.isEmpty then Left(AlignmentError.InvalidRelationship("entity-aligned study requires at least one view"))
    else if entityForm.space.descriptor != entitySpace.descriptor then
      Left(AlignmentError.InvalidRelationship("entity form belongs to a different space"))
    else if views.map(_.viewId).distinct.length != views.length then
      Left(AlignmentError.InvalidRelationship("entity-aligned study view ids must be unique"))
    else if views.exists(_.map.descriptor.codomain != entitySpace.descriptor) then
      Left(AlignmentError.InvalidRelationship("every view map must target the study entity space"))
    else
      alignmentReport(entitySpace, views).map { report =>
        val factorization = HubFactorizationCertificate(
          entitySpace.descriptor,
          entityForm.operator.valueIdentity,
          views.map(_.map.operator.valueIdentity),
          "all pairwise links factor as P_s* A_E P_t"
        )
        val global = GlobalBlockPsdCertificate(
          entityForm.certificates,
          factorization,
          "L = P* A_E P is PSD because A_E is certified PSD"
        )
        new EntityAlignedStudy(
          entitySpace,
          entityForm,
          views,
          report,
          factorization,
          global,
          provenance
        )
      }

  private def alignmentReport[E <: SemanticSpace](
      entitySpace: SpaceEvidence[E],
      views: Vector[EntityMapEntry[E]]
  ): Either[AlignmentError, StudyAlignmentReport] =
    val represented = Array.fill(entitySpace.dimension)(false)
    val reports = Vector.newBuilder[ViewAlignmentReport]
    var index = 0
    var matchedMass = 0.0
    var unmatchedMass = 0.0
    val duplicateKeys = Vector.newBuilder[String]
    var error = Option.empty[AlignmentError]
    while index < views.length && error.isEmpty do
      val entry = views(index)
      entry.map.matrix.toDense(StoragePolicy.AllowDense) match
        case Left(value) => error = Some(AlignmentError.Multivar(value))
        case Right(matrix) =>
          var entity = 0
          while entity < matrix.rows do
            var source = 0
            var hasMass = false
            while source < matrix.cols && !hasMass do
              if matrix(entity, source) != 0.0 then hasMass = true
              source += 1
            if hasMass then represented(entity) = true
            entity += 1
          val descriptor = entry.map.descriptor
          reports += ViewAlignmentReport(
            entry.viewId,
            descriptor.domain,
            descriptor.support,
            descriptor.normalization,
            descriptor.marginals
          )
          matchedMass += descriptor.support.matchedMass
          unmatchedMass += descriptor.support.unmatchedSourceMass
          duplicateKeys ++= descriptor.support.duplicateKeys
      index += 1
    error match
      case Some(value) => Left(value)
      case None =>
        val coverage = represented.count(identity)
        Right(
          StudyAlignmentReport(
            reports.result(),
            matchedMass,
            unmatchedMass,
            duplicateKeys.result().distinct,
            coverage,
            entitySpace.dimension,
            coverage == entitySpace.dimension
          )
        )
