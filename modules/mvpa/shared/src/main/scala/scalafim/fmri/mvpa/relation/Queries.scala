package scalafim.fmri.mvpa.relation

import gale.backend.Backend.given
import gale.linalg.{DMat, DVec, DoubleLinearOperator, LinAlgError, MutableDVec}
import multivar.core.{CoordinateEvidence, Lin, SemanticProvenance, SemanticProvenanceEvent, SemanticSpace, SpaceEvidence, Table, ValueIdentity}
import scalafim.fmri.mvpa.{AxisDescriptor, AxisRef, EvidenceError}

/** A first-order contrast has an explicitly identified contrast output axis.
  * It cannot be used as a second-order effect query.
  */
final class FirstOrderQuery[E <: SemanticSpace, C <: SemanticSpace] private[relation] (
    val contrasts: SpaceEvidence[C],
    val contrastAxis: AxisDescriptor,
    val weights: Table[E, C]
):
  def apply[N <: SemanticSpace](relation: Relation[E, N]): Either[EvidenceError, FirstOrderPattern[C, N]] =
    QueryEvaluation.table(
      contrasts, relation.neural, "first-order-pattern",
      Vector(weights.valueIdentity, relation.estimate.valueIdentity),
      input => relation.estimate(input).flatMap(weights.star.apply),
      input => weights(input).flatMap(relation.estimate.star.apply)
    ).map(pattern => new FirstOrderPattern(contrasts, contrastAxis, relation.neural, relation.neuralAxis, pattern, relation.origins))

object FirstOrderQuery:
  def apply[EK, CK](
      effects: AxisRef[EK],
      contrasts: AxisRef[CK],
      weights: Table[effects.Id, contrasts.Id]
  ): FirstOrderQuery[effects.Id, contrasts.Id] =
    new FirstOrderQuery(contrasts.evidence, contrasts.descriptor, weights)

/** A contrast-pattern result. It preserves the contrast and neural axes and is
  * deliberately distinct from a relation form.
  */
final class FirstOrderPattern[C <: SemanticSpace, N <: SemanticSpace] private[relation] (
    val contrasts: SpaceEvidence[C],
    val contrastAxis: AxisDescriptor,
    val neural: SpaceEvidence[N],
    val neuralAxis: AxisDescriptor,
    val values: Table[C, N],
    val origins: RelationOrigins
)

/** Ordered left/right relation endpoints. Reversal changes endpoint order; it
  * does not assume matching dimensions or symmetrize either endpoint.
  */
final class RelationPair[
    EL <: SemanticSpace,
    NL <: SemanticSpace,
    ER <: SemanticSpace,
    NR <: SemanticSpace
] private[relation] (
    val left: Relation[EL, NL],
    val right: Relation[ER, NR]
):
  def reverse: RelationPair[ER, NR, EL, NL] =
    new RelationPair(right, left)

object RelationPair:
  def apply[
      EL <: SemanticSpace,
      NL <: SemanticSpace,
      ER <: SemanticSpace,
      NR <: SemanticSpace
  ](left: Relation[EL, NL], right: Relation[ER, NR]): RelationPair[EL, NL, ER, NR] =
    new RelationPair(left, right)

/** An open relational transport. Holding this value performs no relation
  * evaluation and does not construct a Kronecker product or full geometry.
  */
final class OpenRelationTransport[
    EL <: SemanticSpace,
    NL <: SemanticSpace,
    ER <: SemanticSpace,
    NR <: SemanticSpace
] private[relation] (val pair: RelationPair[EL, NL, ER, NR]):
  def reverse: OpenRelationTransport[ER, NR, EL, NL] =
    new OpenRelationTransport(pair.reverse)

  def closeNeural(metric: Table[NL, NR]): Either[EvidenceError, EffectForm[EL, ER]] =
    SecondOrderQuery(pair, metric = Some(metric)).effectForm

  /** The Frobenius-adjoint closure: H maps to B_L^T H B_R. */
  def closeExperimental(query: Table[EL, ER]): Either[EvidenceError, NeuralForm[NL, NR]] =
    SecondOrderQuery(pair, query = Some(query)).neuralForm

/** Effect-space cross-form. This may be rectangular and signed; it makes no
  * symmetry, PSD, or distance claim.
  */
final class EffectForm[L <: SemanticSpace, R <: SemanticSpace] private[relation] (
    val left: SpaceEvidence[L],
    val right: SpaceEvidence[R],
    val values: Table[L, R],
    val leftOrigins: RelationOrigins,
    val rightOrigins: RelationOrigins
):
  def reverse: Either[EvidenceError, EffectForm[R, L]] =
    Right(new EffectForm(right, left, values.star, rightOrigins, leftOrigins))

  def adjoint: Either[EvidenceError, EffectForm[R, L]] =
    reverse

/** Neural-space cross-form. This may be rectangular and signed; it makes no
  * symmetry, PSD, or distance claim.
  */
final class NeuralForm[L <: SemanticSpace, R <: SemanticSpace] private[relation] (
    val left: SpaceEvidence[L],
    val right: SpaceEvidence[R],
    val values: Table[L, R],
    val leftOrigins: RelationOrigins,
    val rightOrigins: RelationOrigins
):
  def reverse: Either[EvidenceError, NeuralForm[R, L]] =
    Right(new NeuralForm(right, left, values.star, rightOrigins, leftOrigins))

  def adjoint: Either[EvidenceError, NeuralForm[R, L]] =
    reverse

/** The scalar closure of one ordered relational pair. Its value is exactly
  * `tr(H^T B_L K B_R^T)` for the supplied typed experimental and neural forms.
  */
final case class ScalarRelationStatistic(value: Double)

/** Typed second-order queries for an ordered, potentially rectangular relation
  * pair. `query` closes experimental endpoints; `metric` closes neural
  * endpoints. Their nominal type parameters make same-sized foreign axes
  * inapplicable at compile time.
  */
final class SecondOrderQuery[
    EL <: SemanticSpace,
    NL <: SemanticSpace,
    ER <: SemanticSpace,
    NR <: SemanticSpace
] private[relation] (
    val pair: RelationPair[EL, NL, ER, NR],
    val query: Option[Table[EL, ER]],
    val metric: Option[Table[NL, NR]]
):
  def open: OpenRelationTransport[EL, NL, ER, NR] =
    new OpenRelationTransport(pair)

  def reverse: SecondOrderQuery[ER, NR, EL, NL] =
    new SecondOrderQuery(pair.reverse, query.map(_.star), metric.map(_.star))

  def effectForm: Either[EvidenceError, EffectForm[EL, ER]] =
    metric match
      case None => Left(EvidenceError.InvalidAxis("neural metric", "an effect form requires a neural closure"))
      case Some(value) =>
        QueryEvaluation.table(
          pair.left.effects, pair.right.effects, "effect-form",
          Vector(pair.left.estimate.valueIdentity, value.valueIdentity, pair.right.estimate.valueIdentity),
          input => pair.right.estimate.star(input).flatMap(value.apply).flatMap(pair.left.estimate.apply),
          input => pair.left.estimate.star(input).flatMap(value.star.apply).flatMap(pair.right.estimate.apply)
        ).map(form => new EffectForm(pair.left.effects, pair.right.effects, form, pair.left.origins, pair.right.origins))

  def neuralForm: Either[EvidenceError, NeuralForm[NL, NR]] =
    query match
      case None => Left(EvidenceError.InvalidAxis("experimental query", "a neural form requires an experimental closure"))
      case Some(value) =>
        QueryEvaluation.table(
          pair.left.neural, pair.right.neural, "neural-form",
          Vector(pair.left.estimate.valueIdentity, value.valueIdentity, pair.right.estimate.valueIdentity),
          input => pair.right.estimate(input).flatMap(value.apply).flatMap(pair.left.estimate.star.apply),
          input => pair.left.estimate(input).flatMap(value.star.apply).flatMap(pair.right.estimate.star.apply)
        ).map(form => new NeuralForm(pair.left.neural, pair.right.neural, form, pair.left.origins, pair.right.origins))

  /** Contract one right-effect basis column at a time. This does not require
    * either complete cross-form or a quadratic identity allocation.
    * The operator-backed endpoints must support the stated applications;
    * this method does not grant replay ownership to a one-shot relation.
    */
  def scalar: Either[EvidenceError, ScalarRelationStatistic] =
    (query, metric) match
      case (Some(_), Some(_)) if
          pair.left.origins.access == RelationAccess.OneShot || pair.right.origins.access == RelationAccess.OneShot =>
        Left(EvidenceError.InvalidSource("scalar contraction requires declared owned replay for both relation endpoints"))
      case (Some(experimental), Some(neural)) =>
        var index = 0
        var total = 0.0
        var failure: Option[EvidenceError] = None
        while index < pair.right.effectAxis.size && failure.isEmpty do
          val basis = DMat.tabulate(pair.right.effectAxis.size, 1)((row, _) => if row == index then 1.0 else 0.0)
          val diagonal = pair.right.estimate.star(basis)
            .flatMap(neural.apply)
            .flatMap(pair.left.estimate.apply)
            .flatMap(experimental.star.apply)
          diagonal match
            case Left(error) => failure = Some(EvidenceError.SemanticFailure(error))
            case Right(value) => total += value(index, 0)
          index += 1
        failure match
          case Some(error) => Left(error)
          case None => Right(ScalarRelationStatistic(total))
      case _ =>
        Left(EvidenceError.InvalidAxis("second-order closure", "a scalar requires experimental and neural closures"))

object SecondOrderQuery:
  def apply[
      EL <: SemanticSpace,
      NL <: SemanticSpace,
      ER <: SemanticSpace,
      NR <: SemanticSpace
  ](
      pair: RelationPair[EL, NL, ER, NR],
      query: Option[Table[EL, ER]] = None,
      metric: Option[Table[NL, NR]] = None
  ): SecondOrderQuery[EL, NL, ER, NR] =
    new SecondOrderQuery(pair, query, metric)

/** Domain-specific closure adapter: the numerical operations remain calls to
  * the admitted Multivar maps. No operator storage is exposed or copied.
  */
private object QueryEvaluation:
  def table[R <: SemanticSpace, C <: SemanticSpace](
      rows: SpaceEvidence[R],
      columns: SpaceEvidence[C],
      operation: String,
      inputs: Vector[ValueIdentity],
      forward: DMat => Either[multivar.core.SemanticError, DMat],
      backward: DMat => Either[multivar.core.SemanticError, DMat]
  ): Either[EvidenceError, Table[R, C]] =
    val rowsDimension = rows.dimension
    val columnsDimension = columns.dimension
    val operator = new DoubleLinearOperator:
      def rows: Int = rowsDimension
      def cols: Int = columnsDimension
      override def applyTo(input: DMat): Either[LinAlgError, DMat] =
        forward(input).left.map(error => LinAlgError.InvalidArgument(error.message))
      override def transposeApplyTo(input: DMat): Either[LinAlgError, DMat] =
        backward(input).left.map(error => LinAlgError.InvalidArgument(error.message))
      def applyTo(input: DVec, output: MutableDVec): Unit =
        copy(forward, input, output)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
        copy(backward, input, output)
      private def copy(
          evaluate: DMat => Either[multivar.core.SemanticError, DMat],
          input: DVec,
          output: MutableDVec
      ): Unit =
        val result = evaluate(DMat.tabulate(input.length, 1)((row, _) => input(row)))
          .fold(error => throw LinAlgError.InvalidArgument(error.message), identity)
        if result.rows != output.length then
          throw LinAlgError.VectorLengthMismatch(result.rows, output.length)
        var row = 0
        while row < output.length do
          output(row) = result(row, 0)
          row += 1
    Lin.fromLinearMap(
      operator, CoordinateEvidence.dual(columns), CoordinateEvidence.primal(rows),
      ValueIdentity.derived(operation, inputs*),
      SemanticProvenance.source("scalafim-mvpa-relation-query").append(
        SemanticProvenanceEvent.Derived(operation, inputs)
      )
    ).left.map(EvidenceError.SemanticFailure.apply)
