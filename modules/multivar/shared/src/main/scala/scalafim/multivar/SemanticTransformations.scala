package scalafim.multivar

import gale.linalg.DMat

final case class ColumnTransformationDescriptor(
    space: MvSpace,
    operator: LinDescriptor,
    valueIdentity: ValueIdentity
)

/** A data-coordinate transformation is not a column form. The induced geometry is
  * created only by the named [[inducedGeometry]] operation below.
  */
final class ColumnTransformation[S <: SemanticSpace] private (
    val space: SpaceEvidence[S],
    val operator: Lin[Primal[S], Primal[S]],
    val descriptor: ColumnTransformationDescriptor,
    val provenance: SemanticProvenance
)

object ColumnTransformation:
  def fromLin[S <: SemanticSpace](
      space: SpaceEvidence[S],
      operator: Lin[Primal[S], Primal[S]],
      provenance: SemanticProvenance = SemanticProvenance.source("column-transformation")
  ): Either[DiagramError, ColumnTransformation[S]] =
    if operator.domain.descriptor.space != space.descriptor ||
        operator.codomain.descriptor.space != space.descriptor
    then
      Left(DiagramError.InvalidGeometry("column transformation coordinates do not match its declared space"))
    else
      Right(
        new ColumnTransformation(
          space,
          operator,
          ColumnTransformationDescriptor(space.descriptor, operator.descriptor, operator.valueIdentity),
          provenance
        )
      )

final case class InducedFormRelation(
    transformation: ValueIdentity,
    form: ValueIdentity,
    formula: String
)

sealed trait InducedColumnGeometry[S <: SemanticSpace]:
  def transformation: ColumnTransformation[S]
  def relation: InducedFormRelation
  def diagramGeometry: DiagramGeometry[S]

object InducedColumnGeometry:
  final case class FullRank[S <: SemanticSpace](
      transformation: ColumnTransformation[S],
      metric: MetricForm[S, CertifiedSpd],
      relation: InducedFormRelation
  ) extends InducedColumnGeometry[S]:
    override def diagramGeometry: DiagramGeometry[S] =
      DiagramGeometry.metric(metric)

  final case class RankDeficient[S <: SemanticSpace](
      transformation: ColumnTransformation[S],
      semiMetric: SemiMetric[S, CertifiedPsd],
      relation: InducedFormRelation
  ) extends InducedColumnGeometry[S]:
    override def diagramGeometry: DiagramGeometry[S] =
      DiagramGeometry.semiMetric(semiMetric)

def inducedGeometry[S <: SemanticSpace](
    transformation: ColumnTransformation[S],
    context: CertificateContext = CertificateContext.portableFloat64,
    eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
): Either[DiagramError, InducedColumnGeometry[S]] =
  for
    matrix <- transformation.operator(DMat.eye(transformation.space.dimension)).left.map(DiagramError.Semantic.apply)
    induced = GaleNumerics.multiply(matrix, matrix.transpose)
    legacy <- MvMetric
      .denseSymmetric(induced, MetricValidation.Structural, Some(transformation.space.descriptor))
      .left
      .map(DiagramError.Multivar.apply)
    identity = ValueIdentity.derived(
      "induced-form-m-m-star",
      transformation.operator.valueIdentity
    )
    operator <- FormOperator
      .primal(
        legacy,
        transformation.space,
        identity,
        transformation.provenance.append(
          SemanticProvenanceEvent.Derived(
            "induced-column-form",
            Vector(transformation.operator.valueIdentity)
          )
        )
      )
      .left
      .map(DiagramError.Semantic.apply)
    relation = InducedFormRelation(
      transformation.operator.valueIdentity,
      identity,
      "R = M M*"
    )
    result <- FormCertificates.spd(operator, context, eigenSolver) match
      case Right(spd) =>
        Form
          .metric(operator, transformation.space, spd)
          .left
          .map(DiagramError.Semantic.apply)
          .map(InducedColumnGeometry.FullRank(transformation, _, relation))
      case Left(_) =>
        for
          psd <- FormCertificates.psd(operator, context, eigenSolver).left.map(DiagramError.Semantic.apply)
          form <- Form.semiMetric(operator, transformation.space, psd).left.map(DiagramError.Semantic.apply)
        yield InducedColumnGeometry.RankDeficient(transformation, form, relation)
  yield result
