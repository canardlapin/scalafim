package scalafim.locus.laws

final case class ReferenceRegion(size: Int, members: Set[Int]):
  require(size >= 0, "reference region size must be non-negative")
  require(members.forall(i => i >= 0 && i < size), "reference region member out of bounds")

  def union(that: ReferenceRegion): ReferenceRegion =
    require(size == that.size, "reference region sizes must match")
    ReferenceRegion(size, members union that.members)

  def intersect(that: ReferenceRegion): ReferenceRegion =
    require(size == that.size, "reference region sizes must match")
    ReferenceRegion(size, members intersect that.members)

  def diff(that: ReferenceRegion): ReferenceRegion =
    require(size == that.size, "reference region sizes must match")
    ReferenceRegion(size, members diff that.members)

  def complement: ReferenceRegion =
    ReferenceRegion(size, (0 until size).toSet diff members)

  def subsetOf(that: ReferenceRegion): Boolean =
    require(size == that.size, "reference region sizes must match")
    members.subsetOf(that.members)

final case class ReferenceTotalMap(
    fromSize: Int,
    toSize: Int,
    targets: Vector[Int]
):
  require(fromSize >= 0 && toSize >= 0, "reference map sizes must be non-negative")
  require(targets.length == fromSize, "reference map target count must match source size")
  require(targets.forall(i => i >= 0 && i < toSize), "reference map target out of bounds")

  def andThen(that: ReferenceTotalMap): ReferenceTotalMap =
    require(toSize == that.fromSize, "reference map middle sizes must match")
    ReferenceTotalMap(fromSize, that.toSize, targets.map(that.targets))

  def pullback(region: ReferenceRegion): ReferenceRegion =
    require(toSize == region.size, "reference map target and region sizes must match")
    ReferenceRegion(fromSize, targets.indices.filter(i => region.members.contains(targets(i))).toSet)

  def existsAlong(region: ReferenceRegion): ReferenceRegion =
    require(fromSize == region.size, "reference map source and region sizes must match")
    ReferenceRegion(toSize, region.members.map(targets))

  def forallAlong(region: ReferenceRegion): ReferenceRegion =
    require(fromSize == region.size, "reference map source and region sizes must match")
    val members = (0 until toSize).filter: target =>
      targets.indices.forall(source => targets(source) != target || region.members.contains(source))
    ReferenceRegion(toSize, members.toSet)

  def forallAlongOnImage(region: ReferenceRegion): ReferenceRegion =
    forallAlong(region).intersect(ReferenceRegion(toSize, targets.toSet))

  def isSurjective: Boolean =
    targets.toSet == (0 until toSize).toSet

final case class ReferenceRelation(
    fromSize: Int,
    toSize: Int,
    pairs: Set[(Int, Int)]
):
  require(fromSize >= 0 && toSize >= 0, "reference relation sizes must be non-negative")
  require(
    pairs.forall((source, target) =>
      source >= 0 && source < fromSize && target >= 0 && target < toSize
    ),
    "reference relation pair out of bounds"
  )

  def row(source: Int): ReferenceRegion =
    require(source >= 0 && source < fromSize, "reference relation source out of bounds")
    ReferenceRegion(toSize, pairs.collect { case (`source`, target) => target })

  def andThen(that: ReferenceRelation): ReferenceRelation =
    require(toSize == that.fromSize, "reference relation middle sizes must match")
    val composed =
      for
        (source, middle) <- pairs
        (nextSource, target) <- that.pairs
        if nextSource == middle
      yield (source, target)
    ReferenceRelation(fromSize, that.toSize, composed)

  def converse: ReferenceRelation =
    ReferenceRelation(toSize, fromSize, pairs.map((source, target) => (target, source)))

  def union(that: ReferenceRelation): ReferenceRelation =
    require(fromSize == that.fromSize && toSize == that.toSize, "reference relation sizes must match")
    ReferenceRelation(fromSize, toSize, pairs union that.pairs)

  def image(region: ReferenceRegion): ReferenceRegion =
    require(fromSize == region.size, "reference relation source and region sizes must match")
    ReferenceRegion(
      toSize,
      pairs.collect { case (source, target) if region.members.contains(source) => target }
    )

  def allRelatedInside(region: ReferenceRegion): ReferenceRegion =
    require(toSize == region.size, "reference relation target and region sizes must match")
    ReferenceRegion(
      fromSize,
      (0 until fromSize).filter(source => row(source).subsetOf(region)).toSet
    )

object ReferenceRelation:
  def identity(size: Int): ReferenceRelation =
    ReferenceRelation(size, size, (0 until size).map(i => (i, i)).toSet)

final case class ReferenceField[A](values: Vector[A]):
  def size: Int =
    values.length

  def map[B](f: A => B): ReferenceField[B] =
    ReferenceField(values.map(f))

  def restrict(region: ReferenceRegion): ReferenceSection[A] =
    require(size == region.size, "reference field and region sizes must match")
    ReferenceSection(this, region)

final case class ReferenceSection[A](
    field: ReferenceField[A],
    support: ReferenceRegion
):
  require(field.size == support.size, "reference section field and support sizes must match")

  def restrict(region: ReferenceRegion): ReferenceSection[A] =
    ReferenceSection(field, support.intersect(region))

  def map[B](f: A => B): ReferenceSection[B] =
    ReferenceSection(field.map(f), support)

  def valuesInDomainOrder: Vector[A] =
    support.members.toVector.sorted.map(field.values)

final case class ReferenceParcellation(
    ambientSize: Int,
    parcelCount: Int,
    assignments: Vector[Option[Int]]
):
  require(ambientSize >= 0 && parcelCount >= 0, "reference parcellation sizes must be non-negative")
  require(assignments.length == ambientSize, "reference assignment count must match ambient size")
  require(
    assignments.flatten.forall(i => i >= 0 && i < parcelCount),
    "reference parcel assignment out of bounds"
  )
  require(
    assignments.flatten.toSet == (0 until parcelCount).toSet,
    "every reference parcel must have a non-empty fiber"
  )

  def support: ReferenceRegion =
    ReferenceRegion(ambientSize, assignments.indices.filter(i => assignments(i).nonEmpty).toSet)

  def fiber(parcel: Int): ReferenceRegion =
    require(parcel >= 0 && parcel < parcelCount, "reference parcel out of bounds")
    ReferenceRegion(ambientSize, assignments.indices.filter(i => assignments(i).contains(parcel)).toSet)

  def coarsen(mapping: ReferenceTotalMap): ReferenceParcellation =
    require(mapping.fromSize == parcelCount, "reference coarsening source size must match parcels")
    require(mapping.isSurjective, "reference coarsening must be surjective")
    ReferenceParcellation(
      ambientSize,
      mapping.toSize,
      assignments.map(_.map(mapping.targets))
    )
