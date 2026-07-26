package scalafim.locus.laws

import scalafim.locus.*

object LocusDifferential:
  def region[S](value: Region[S]): ReferenceRegion =
    ReferenceRegion(value.space.size, value.ordinalsInDomainOrder.toSet)

  def totalMap[X, Y](value: TotalMap[X, Y]): ReferenceTotalMap =
    ReferenceTotalMap(value.from.size, value.to.size, value.targetOrdinals.toVector)

  def relation[X, Y](value: Relation[X, Y]): ReferenceRelation =
    val pairs =
      value.ordinalRows.zipWithIndex.flatMap: (row, source) =>
        row.map(target => (source, target))
    ReferenceRelation(value.from.size, value.to.size, pairs.toSet)

  def parcellation[X, P](value: Parcellation[X, P]): ReferenceParcellation =
    ReferenceParcellation(
      value.ambient.size,
      value.parcels.size,
      value.assignmentOrdinals
    )

  def field[S, A](value: IndexedField[S, A]): ReferenceField[A] =
    ReferenceField(value.space.points.map(value.apply).toVector)

  def section[S, A](value: Section[S, A]): ReferenceSection[A] =
    ReferenceSection(field(value.field), region(value.support))
