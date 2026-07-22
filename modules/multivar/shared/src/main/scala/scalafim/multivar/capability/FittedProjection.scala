package scalafim.multivar
package capability

import scalafim.multivar.core.*

import gale.linalg.CholeskyOptions
import gale.linalg.DMat

/** Stable identity of one fitted feature coordinate. */
opaque type FeatureId = String

object FeatureId:
  def apply(value: String): Either[MultivarError, FeatureId] =
    Identifier.validate("feature id", value)

  def unsafe(value: String): FeatureId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: FeatureId)
    inline def value: String = id

enum FeatureIdentityMode:
  case Named
  case Positional

/** Ordered feature identities bound to one fitted semantic space and frame. */
final class FeatureSchema private (
    val space: MvSpace,
    val identities: Vector[FeatureId],
    val valueIdentity: ValueIdentity,
    val mode: FeatureIdentityMode
):
  require(identities.length == space.size, "feature schema size must match its semantic space")
  require(identities.distinct.length == identities.length, "feature identities must be unique")

object FeatureSchema:
  def from(
      space: MvSpace,
      identities: Vector[FeatureId],
      valueIdentity: ValueIdentity
  ): Either[MultivarError, FeatureSchema] =
    if identities.length != space.size then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"feature schema for '${space.id.value}' has ${identities.length} identities, expected ${space.size}"
        )
      )
    else
      firstDuplicate(identities) match
        case Some(id) => Left(MultivarError.FeatureIdentityMismatch(s"duplicate feature identity '${id.value}'"))
        case None     => Right(new FeatureSchema(space, identities, valueIdentity, FeatureIdentityMode.Named))

  def positional(space: MvSpace, valueIdentity: ValueIdentity): Either[MultivarError, FeatureSchema] =
    val identities = Vector.tabulate(space.size)(index => FeatureId.unsafe(s"${space.id.value}.feature-$index"))
    Right(new FeatureSchema(space, identities, valueIdentity, FeatureIdentityMode.Positional))

  private[multivar] def restricted(
      source: FeatureSchema,
      space: MvSpace,
      identities: Vector[FeatureId],
      valueIdentity: ValueIdentity
  ): Either[MultivarError, FeatureSchema] =
    from(space, identities, valueIdentity).map(schema => new FeatureSchema(space, identities, valueIdentity, source.mode))

  private def firstDuplicate(identities: Vector[FeatureId]): Option[FeatureId] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    var index = 0
    var duplicate = Option.empty[FeatureId]
    while index < identities.length && duplicate.isEmpty do
      val id = identities(index)
      if seen.contains(id.value) then duplicate = Some(id)
      else seen += id.value
      index += 1
    duplicate

/** A matrix whose ordered columns are explicitly bound to a fitted feature schema. */
final case class IdentifiedFeatureMatrix private (values: MatrixView, schema: FeatureSchema)

object IdentifiedFeatureMatrix:
  def from(values: MatrixView, schema: FeatureSchema): Either[MultivarError, IdentifiedFeatureMatrix] =
    if values.cols != schema.identities.length then
      Left(
        MultivarError.MatrixShapeMismatch(
          s"identified feature matrix has ${values.cols} columns but schema has ${schema.identities.length}"
        )
      )
    else Right(IdentifiedFeatureMatrix(values, schema))

enum PartialScorePolicy:
  case Contribution
  case LeastSquares(metric: MetricSpec, ridge: Ridge)

object PartialScorePolicy:
  def euclideanLeastSquares(featureCount: Int, ridge: Double): Either[MultivarError, PartialScorePolicy] =
    for
      metric <- MetricSpec.identity(featureCount)
      strength <- Ridge(ridge)
    yield PartialScorePolicy.LeastSquares(metric, strength)

final case class FeatureProjectionProvenance(
    sourceFrame: ValueIdentity,
    sourceFeatureSpace: MvSpace,
    restrictedFeatureSpace: MvSpace,
    sourceSchema: ValueIdentity,
    selectedFeatures: Vector[FeatureId],
    policy: PartialScorePolicy,
    semantic: SemanticProvenance
)

sealed trait PartialScoreResult:
  def values: DMat
  def projectionProvenance: FeatureProjectionProvenance

final case class PartialFeatureContribution private[multivar] (
    values: DMat,
    projectionProvenance: FeatureProjectionProvenance
) extends PartialScoreResult

final case class PartialLeastSquaresScores private[multivar] (
    values: DMat,
    projectionProvenance: FeatureProjectionProvenance
) extends PartialScoreResult

/** Identity-bound restriction `C_S* -> C*` of a fitted feature space. */
final class FeatureRestriction[Source <: SemanticSpace] private[multivar] (
    val sourceSpace: SpaceEvidence[Source],
    val sourceSchema: FeatureSchema,
    val columns: IndexSet,
    val restrictedSpace: SpaceRef,
    val restrictedSchema: FeatureSchema,
    val fittedPreprocessor: FittedPreprocessor,
    val sourceFrameIdentity: ValueIdentity,
    val provenance: SemanticProvenance
)(
    val embedding: Op[
      Dual[restrictedSpace.Id],
      Dual[Source],
      ConstraintOperatorRole,
      UncheckedEvidence
    ]
):
  def bind(values: MatrixView): Either[MultivarError, IdentifiedFeatureMatrix] =
    restrictedSchema.mode match
      case FeatureIdentityMode.Positional => IdentifiedFeatureMatrix.from(values, restrictedSchema)
      case FeatureIdentityMode.Named =>
        Left(
          MultivarError.FeatureIdentityMismatch(
            "named feature inputs must supply their schema explicitly; positional binding is unavailable"
          )
        )

  def bind(values: MatrixView, schema: FeatureSchema): Either[MultivarError, IdentifiedFeatureMatrix] =
    IdentifiedFeatureMatrix.from(values, schema)

/** Partial projection through a single restricted view of an existing functional frame. */
final class RestrictedFrameTransform[
    Source <: SemanticSpace,
    Component <: SemanticSpace
] private (
    val restriction: FeatureRestriction[Source],
    val componentSpace: SpaceEvidence[Component],
    val provenance: SemanticProvenance
)(
    val frame: FunctionalFrame[restriction.restrictedSpace.Id, Component, UncheckedEvidence],
    val weights: DMat
):
  def contribution(input: IdentifiedFeatureMatrix): Either[MultivarError, PartialFeatureContribution] =
    for
      _ <- validateInput(input)
      processed <- restriction.fittedPreprocessor.transform(input.values)
      scores <- scoreProcessed(processed, "partial-feature-contribution")
    yield
      PartialFeatureContribution(
        scores,
        resultProvenance(PartialScorePolicy.Contribution)
      )

  def recover(
      input: IdentifiedFeatureMatrix,
      metric: MetricSpec,
      ridge: Ridge
  ): Either[MultivarError, PartialLeastSquaresScores] =
    val policy = PartialScorePolicy.LeastSquares(metric, ridge)
    if metric.dim != restriction.columns.length then
      Left(
        MultivarError.MetricShapeMismatch(
          IndexAxis.Feature,
          restriction.columns.length,
          metric.dim
        )
      )
    else
      for
        direct <- contribution(input)
        metricWeights <- metric.matvec(weights)
        gram = MatrixOps.addRidge(GaleNumerics.transposeMultiply(weights, metricWeights), ridge.value)
        factor <- LinalgErrorAdapter.adapt(gram.cholesky(CholeskyOptions()))
        transposed <- LinalgErrorAdapter.adapt(factor.solve(direct.values.transpose))
      yield
        PartialLeastSquaresScores(
          transposed.transpose,
          resultProvenance(policy)
        )

  def recoverEuclidean(
      input: IdentifiedFeatureMatrix,
      ridge: Ridge
  ): Either[MultivarError, PartialLeastSquaresScores] =
    MetricSpec.identity(restriction.columns.length).flatMap(metric => recover(input, metric, ridge))

  def score(
      input: IdentifiedFeatureMatrix,
      policy: PartialScorePolicy
  ): Either[MultivarError, PartialScoreResult] =
    policy match
      case PartialScorePolicy.Contribution => contribution(input)
      case PartialScorePolicy.LeastSquares(metric, ridge) => recover(input, metric, ridge)

  private def validateInput(input: IdentifiedFeatureMatrix): Either[MultivarError, Unit] =
    val expected = restriction.restrictedSchema
    val actual = input.schema
    if actual.space != expected.space then
      Left(
        MultivarError.FeatureIdentityMismatch(
          s"restricted input belongs to feature space '${actual.space.id.value}', expected '${expected.space.id.value}'"
        )
      )
    else if actual.valueIdentity != expected.valueIdentity then
      Left(MultivarError.FeatureIdentityMismatch("restricted input belongs to a foreign fitted feature schema"))
    else if actual.identities != expected.identities then
      Left(
        MultivarError.FeatureIdentityMismatch(
          s"restricted input feature order ${actual.identities.map(_.value).mkString("[", ", ", "]")} does not match " +
            expected.identities.map(_.value).mkString("[", ", ", "]")
        )
      )
    else Right(())

  private def scoreProcessed(
      processed: MatrixView,
      operation: String
  ): Either[MultivarError, DMat] =
    for
      rows <- SpaceRef.of(
        s"${restriction.restrictedSpace.descriptor.id.value}.projection-rows",
        SpaceRole.Samples,
        processed.rows
      )
      table <- projectionSemantic(
        Op.fromMatrixView(
          processed,
          CoordinateEvidence.dual(restriction.restrictedSpace.evidence),
          CoordinateEvidence.primal(rows.evidence),
          OperatorRoleWitness.table,
          ValueIdentity.derived(operation, restriction.sourceFrameIdentity),
          provenance.append(
            SemanticProvenanceEvent.Derived(operation, Vector(restriction.sourceFrameIdentity))
          )
        )
      )
      values <- projectionSemantic(frame.scores(table).toDense)
    yield values

  private def resultProvenance(policy: PartialScorePolicy): FeatureProjectionProvenance =
    FeatureProjectionProvenance(
      restriction.sourceFrameIdentity,
      restriction.sourceSpace.descriptor,
      restriction.restrictedSpace.descriptor,
      restriction.sourceSchema.valueIdentity,
      restriction.restrictedSchema.identities,
      policy,
      provenance
    )

object RestrictedFrameTransform:
  def from(
      transform: FittedFrameTransform,
      columns: IndexSet
  ): Either[MultivarError, RestrictedFrameTransform[transform.featureSpace.Id, transform.componentSpace.Id]] =
    for
      checked <- MatrixView.requireColumnIndexSet(columns, transform.featureSpace.descriptor.size)
      fullWeights <- projectionSemantic(transform.frame.weights.toDense)
      restrictedWeights = GaleNumerics.selectRows(fullWeights, checked.indices)
      fittedPreprocessor <- transform.preprocessor.restrict(checked)
      restrictedSpace <- SpaceRef.of(
        s"${transform.featureSpace.descriptor.id.value}.restriction-${checked.indices.mkString("-")}",
        transform.featureSpace.descriptor.role,
        checked.length
      )
      restrictedSchema <- FeatureSchema.restricted(
        transform.featureSchema,
        restrictedSpace.descriptor,
        checked.indices.map(transform.featureSchema.identities),
        ValueIdentity.derived("feature-restriction", transform.featureSchema.valueIdentity, transform.frame.weights.valueIdentity)
      )
      embedding <- makeEmbedding(transform, checked, restrictedSpace)
      restriction = new FeatureRestriction(
        transform.featureSpace.evidence,
        transform.featureSchema,
        checked,
        restrictedSpace,
        restrictedSchema,
        fittedPreprocessor,
        transform.frame.weights.valueIdentity,
        transform.provenance
      )(embedding)
      restrictedOperator <- projectionSemantic(
        Op.fromDense(
          restrictedWeights,
          CoordinateEvidence.primal(transform.componentSpace.evidence),
          CoordinateEvidence.dual(restriction.restrictedSpace.evidence),
          OperatorRoleWitness.frame,
          ValueIdentity.derived("restricted-frame", transform.frame.weights.valueIdentity, embedding.valueIdentity),
          transform.provenance.append(
            SemanticProvenanceEvent.Derived(
              "restrict-functional-frame",
              Vector(transform.frame.weights.valueIdentity, embedding.valueIdentity)
            )
          )
        )
      )
      frame = FunctionalFrame(restrictedOperator)
    yield
      new RestrictedFrameTransform(
        restriction,
        transform.componentSpace.evidence,
        transform.provenance
      )(frame, restrictedWeights)

  private def makeEmbedding(
      transform: FittedFrameTransform,
      columns: IndexSet,
      restrictedSpace: SpaceRef
  ): Either[
    MultivarError,
    Op[
      Dual[restrictedSpace.Id],
      Dual[transform.featureSpace.Id],
      ConstraintOperatorRole,
      UncheckedEvidence
    ]
  ] =
    val values = Array.ofDim[Double](transform.featureSpace.descriptor.size * columns.length)
    var local = 0
    while local < columns.length do
      values(columns.indices(local) * columns.length + local) = 1.0
      local += 1
    val matrix = GaleNumerics.matrixFromRowMajor(transform.featureSpace.descriptor.size, columns.length, values)
    projectionSemantic(
      Op.fromDense(
        matrix,
        CoordinateEvidence.dual(restrictedSpace.evidence),
        CoordinateEvidence.dual(transform.featureSpace.evidence),
        OperatorRoleWitness.constraint,
        ValueIdentity.derived("feature-restriction-embedding", transform.frame.weights.valueIdentity),
        transform.provenance.append(
          SemanticProvenanceEvent.Derived("feature-restriction-embedding", Vector(transform.frame.weights.valueIdentity))
        )
      )
    )

private def projectionSemantic[A](value: Either[SemanticError, A]): Either[MultivarError, A] =
  value.left.map:
    case SemanticError.MultivarFailure(error)  => error
    case SemanticError.LinearMapFailure(error) => LinalgErrorAdapter.toMultivarError(error)
    case error                                 => MultivarError.InvalidMap(error.message)
