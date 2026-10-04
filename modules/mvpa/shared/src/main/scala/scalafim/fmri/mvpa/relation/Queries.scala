package scalafim.fmri.mvpa.relation

import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.core.{Dual, Lin, Primal, SemanticSpace, SpaceEvidence, Table}
import scalafim.fmri.mvpa.{AxisDescriptor, AxisRef, EvidenceError}

/** A signed cross-space covariant closure. It need not be square, symmetric,
  * or positive. Its primal-to-dual orientation is checked by Multivar.
  */
type CrossClosure[L <: SemanticSpace, R <: SemanticSpace] = Lin[Primal[R], Dual[L]]

/** A first-order contrast has an explicitly identified contrast output axis.
  * It cannot be used as a second-order effect query.
  */
final class FirstOrderQuery[E <: SemanticSpace, C <: SemanticSpace] private[relation] (
    val effectAxis: AxisDescriptor,
    val contrasts: SpaceEvidence[C],
    val contrastAxis: AxisDescriptor,
    val weights: Lin[Primal[E], Primal[C]]
):
  def apply[N <: SemanticSpace](relation: Relation[E, N]): Either[EvidenceError, FirstOrderPattern[C, N]] =
    Right(new FirstOrderPattern(
      contrasts, contrastAxis, relation.neural, relation.neuralAxis,
      relation.estimate.andThen(weights), relation.origins
    ))

object FirstOrderQuery:
  def apply[EK, CK](
      effects: AxisRef[EK],
      contrasts: AxisRef[CK],
      weights: Lin[Primal[effects.Id], Primal[contrasts.Id]]
  ): FirstOrderQuery[effects.Id, contrasts.Id] =
    new FirstOrderQuery(effects.descriptor, contrasts.evidence, contrasts.descriptor, weights)

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

  def closeNeural(metric: CrossClosure[NL, NR]): Either[EvidenceError, EffectForm[EL, ER]] =
    SecondOrderQuery(pair, metric = Some(metric)).effectForm

  /** The Frobenius-adjoint closure: H maps to B_L^T H B_R. */
  def closeExperimental(query: CrossClosure[EL, ER]): Either[EvidenceError, NeuralForm[NL, NR]] =
    SecondOrderQuery(pair, query = Some(query)).neuralForm

/** Effect-space cross-form. This may be rectangular and signed; it makes no
  * symmetry, PSD, or distance claim.
  */
final class EffectForm[L <: SemanticSpace, R <: SemanticSpace] private[relation] (
    val left: SpaceEvidence[L],
    val right: SpaceEvidence[R],
    val values: Table[L, R],
    val leftOrigins: RelationOrigins,
    val rightOrigins: RelationOrigins,
    val leftAxis: AxisDescriptor,
    val rightAxis: AxisDescriptor,
    val leftEstimability: Vector[EffectEstimability],
    val rightEstimability: Vector[EffectEstimability],
    /** Conservative live adapter cells for one effect-column application.
      * This excludes provider-private scratch and already resident operators.
      */
    val oneColumnAdapterCells: Long
):
  def reverse: Either[EvidenceError, EffectForm[R, L]] =
    Right(new EffectForm(right, left, values.star, rightOrigins, leftOrigins,
      rightAxis, leftAxis, rightEstimability, leftEstimability, oneColumnAdapterCells))

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
    val query: Option[CrossClosure[EL, ER]],
    val metric: Option[CrossClosure[NL, NR]]
):
  def open: OpenRelationTransport[EL, NL, ER, NR] =
    new OpenRelationTransport(pair)

  def reverse: SecondOrderQuery[ER, NR, EL, NL] =
    new SecondOrderQuery(pair.reverse, query.map(_.star), metric.map(_.star))

  def effectForm: Either[EvidenceError, EffectForm[EL, ER]] =
    metric match
      case None => Left(EvidenceError.InvalidAxis("neural metric", "an effect form requires a neural closure"))
      case Some(value) =>
        Right(new EffectForm(
          pair.left.effects, pair.right.effects,
          pair.right.estimate.star.andThen(value).andThen(pair.left.estimate),
          pair.left.origins, pair.right.origins,
          pair.left.effectAxis, pair.right.effectAxis, pair.left.estimability, pair.right.estimability,
          32L * (pair.left.effectAxis.size.toLong + pair.right.effectAxis.size + pair.left.neuralAxis.size + pair.right.neuralAxis.size)
        ))

  def neuralForm: Either[EvidenceError, NeuralForm[NL, NR]] =
    query match
      case None => Left(EvidenceError.InvalidAxis("experimental query", "a neural form requires an experimental closure"))
      case Some(value) =>
        Right(new NeuralForm(
          pair.left.neural, pair.right.neural,
          pair.right.estimate.andThen(value).andThen(pair.left.estimate.star),
          pair.left.origins, pair.right.origins
        ))

  /** Contract one right-effect basis column at a time. This does not require
    * either complete cross-form or a quadratic identity allocation.
    * The operator-backed endpoints must support the stated applications;
    * this method does not grant replay ownership to a one-shot relation.
    */
  def scalar: Either[EvidenceError, ScalarRelationStatistic] =
    (query, metric) match
      case (Some(experimental), Some(neural)) =>
        for
          _ <- RelationAccess.admitImmediate(pair.left.origins.access)
          _ <- RelationAccess.admitImmediate(pair.right.origins.access)
          result <- scalarAdmitted(experimental, neural)
        yield result
      case _ =>
        Left(EvidenceError.InvalidAxis("second-order closure", "a scalar requires experimental and neural closures"))

  private def scalarAdmitted(experimental: CrossClosure[EL, ER], neural: CrossClosure[NL, NR]): Either[EvidenceError, ScalarRelationStatistic] =
    val contraction = pair.right.estimate.star.andThen(neural)
      .andThen(pair.left.estimate).andThen(experimental.star)
    var index = 0
    var total = 0.0
    var compensation = 0.0
    var failure: Option[EvidenceError] = None
    while index < pair.right.effectAxis.size && failure.isEmpty do
      val basis = DMat.tabulate(pair.right.effectAxis.size, 1)((row, _) => if row == index then 1.0 else 0.0)
      val diagonal = contraction(basis)
      diagonal match
        case Left(error) => failure = Some(EvidenceError.SemanticFailure(error))
        case Right(value) =>
          val term = value(index, 0)
          if !term.isFinite then failure = Some(EvidenceError.InvalidSource("second-order scalar contribution is non-finite"))
          else
            val next = total + term
            if math.abs(total) >= math.abs(term) then compensation += (total - next) + term
            else compensation += (term - next) + total
            total = next
      index += 1
    failure match
      case Some(error) => Left(error)
      case None =>
        val result = total + compensation
        if result.isFinite then Right(ScalarRelationStatistic(result))
        else Left(EvidenceError.InvalidSource("second-order scalar accumulation is non-finite"))

object SecondOrderQuery:
  def apply[
      EL <: SemanticSpace,
      NL <: SemanticSpace,
      ER <: SemanticSpace,
      NR <: SemanticSpace
  ](
      pair: RelationPair[EL, NL, ER, NR],
      query: Option[CrossClosure[EL, ER]] = None,
      metric: Option[CrossClosure[NL, NR]] = None
  ): SecondOrderQuery[EL, NL, ER, NR] =
    new SecondOrderQuery(pair, query, metric)
