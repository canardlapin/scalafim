package scalafim.fmri.mvpa.relation

import gale.linalg.{DMat, Matrix}
import multivar.core.SemanticSpace
import scalafim.fmri.mvpa.{EvidenceError, LabeledRdm, RdmMethod, RdmModel, RdmScorer}

/** Detached ordinary geometry for one explicitly selected relation. Unlike
  * crossvalidated signed products, Euclidean and correlation dissimilarities
  * are computed on the selected effect-by-neural mean table.
  */
final case class OrdinaryRelationRdm private[relation] (
    observed: LabeledRdm,
    method: RdmMethod,
    origins: RelationOrigins
):
  def score(model: RdmModel, scorer: RdmScorer): Either[EvidenceError, Double] =
    (for
      aligned <- model.alignTo(observed.items)
      value <- scorer.score(observed, aligned)
    yield value).left.map(error => EvidenceError.InvalidSource(error.message))

object OrdinaryRelationGeometry:
  /** The budget covers copied mean patterns, the detached RDM, and adapter
    * column buffers. It excludes resident/provider storage and private scratch.
    * No sample-by-neural table or neural-square identity is materialized.
    */
  def compute[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K](
      relations: RelationSet[P, E, N], partition: PartitionCoordinate[P, K],
      method: RdmMethod, budget: RelationConsumerBudget
  ): Either[EvidenceError, OrdinaryRelationRdm] =
    val e = BigInt(relations.effectAxis.size)
    val n = BigInt(relations.neuralAxis.size)
    val pairs = e * (e - 1) / 2
    val workspace = 2 * e * n + 4 * pairs + 4 * (e + n)
    if partition.selection.parentAxis != relations.partitionAxis then
      Left(EvidenceError.AxisMismatch("ordinary geometry partition", relations.partitionAxis.stableKey, partition.selection.parentAxis.stableKey))
    else if e < 2 || n < 1 then Left(EvidenceError.InvalidSource("ordinary geometry requires at least two effects and one neural feature"))
    else if pairs > budget.maximumValues || workspace > budget.maximumWorkspaceCells || Vector(e * n, pairs, workspace).exists(_ > Int.MaxValue) then
      Left(EvidenceError.InvalidSource("ordinary geometry adapter budget or Int capacity refusal"))
    else
      val relation = relations.relations(partition.ordinal)
      val missing = relation.estimability.zipWithIndex.collect:
        case (EffectEstimability.NotEstimable(reason), ordinal) => s"$ordinal: $reason"
      if missing.nonEmpty then Left(EvidenceError.InvalidSource(s"ordinary geometry has non-estimable effects: ${missing.mkString(", ")}"))
      else RelationAccess.admitImmediate(relation.origins.access).flatMap: _ =>
        val means = Matrix.newBuilder(relations.effectAxis.size, relations.neuralAxis.size)
        var effect = 0
        var failure: Option[EvidenceError] = None
        while effect < relations.effectAxis.size && failure.isEmpty do
          val selected = effect
          val basis = DMat.tabulate(relations.effectAxis.size, 1)((row, _) => if row == selected then 1.0 else 0.0)
          relation.estimate.star(basis).left.map(EvidenceError.SemanticFailure.apply) match
            case Left(error) => failure = Some(error)
            case Right(column) =>
              var feature = 0
              while feature < relations.neuralAxis.size do
                means(effect, feature) = column(feature, 0)
                feature += 1
          effect += 1
        failure.toLeft(()).flatMap: _ =>
          (for
            rdm <- method.compute(means.result())
            labeled <- LabeledRdm(relations.effectKeys, rdm)
          yield OrdinaryRelationRdm(labeled, method, relation.origins))
            .left.map(error => EvidenceError.InvalidSource(error.message))
