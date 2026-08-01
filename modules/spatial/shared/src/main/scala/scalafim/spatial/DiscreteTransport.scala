package scalafim.spatial

import scalafim.locus.{Relation, TotalMap}

/** An exact point-to-point transport between finite sampled domains.
  *
  * This value is deliberately distinct from both a crisp relation and a
  * sampled [[SpatialOperator]]. It carries no interpolation semantics.
  */
final class ExactSpatialMap[X, Y] private (
    val source: DomainId,
    val target: DomainId,
    val mapping: TotalMap[X, Y]
)

object ExactSpatialMap:
  def build[X, Y](
      source: Domain,
      target: Domain,
      mapping: TotalMap[X, Y]
  ): Either[SpatialError, ExactSpatialMap[X, Y]] =
    if !source.locus.space.sameRuntimeOwnerAs(mapping.from) then
      Left(
        SpatialError.OperatorAssemblyFailed(
          s"exact map source space does not match domain ${source.id.value}"
        )
      )
    else if !target.locus.space.sameRuntimeOwnerAs(mapping.to) then
      Left(
        SpatialError.OperatorAssemblyFailed(
          s"exact map target space does not match domain ${target.id.value}"
        )
      )
    else
      Right(new ExactSpatialMap(source.id, target.id, mapping))

/** A crisp many-valued correspondence between finite sampled domains.
  *
  * Rows are Boolean neighborhoods. This value is not an interpolating or
  * weighted operator.
  */
final class CrispSpatialRelation[X, Y] private (
    val source: DomainId,
    val target: DomainId,
    val relation: Relation[X, Y]
)

object CrispSpatialRelation:
  def build[X, Y](
      source: Domain,
      target: Domain,
      relation: Relation[X, Y]
  ): Either[SpatialError, CrispSpatialRelation[X, Y]] =
    if !source.locus.space.sameRuntimeOwnerAs(relation.from) then
      Left(
        SpatialError.OperatorAssemblyFailed(
          s"relation source space does not match domain ${source.id.value}"
        )
      )
    else if !target.locus.space.sameRuntimeOwnerAs(relation.to) then
      Left(
        SpatialError.OperatorAssemblyFailed(
          s"relation target space does not match domain ${target.id.value}"
        )
      )
    else
      Right(new CrispSpatialRelation(source.id, target.id, relation))
