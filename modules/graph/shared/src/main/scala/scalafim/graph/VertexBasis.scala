package scalafim.graph

import scala.collection.mutable

/** An immutable ordered correspondence between stable domain keys and local
  * dense vertex coordinates.
  */
final class VertexBasis[K, V] private (
    val entries: Vector[(K, V)],
    private val indexByKey: Map[K, VertexIx]
):
  def size: Int =
    entries.length

  def isEmpty: Boolean =
    entries.isEmpty

  def nonEmpty: Boolean =
    entries.nonEmpty

  def indices: Vector[VertexIx] =
    Vector.tabulate(size)(VertexIx.unsafe)

  def keys: Vector[K] =
    entries.map(_._1)

  def values: Vector[V] =
    entries.map(_._2)

  def contains(key: K): Boolean =
    indexByKey.contains(key)

  def contains(index: VertexIx): Boolean =
    index.toInt >= 0 && index.toInt < size

  def indexOf(key: K): Option[VertexIx] =
    indexByKey.get(key)

  def keyAt(index: VertexIx): K =
    requireIndex(index)
    entries(index.toInt)._1

  def valueAt(index: VertexIx): V =
    requireIndex(index)
    entries(index.toInt)._2

  def entryAt(index: VertexIx): (K, V) =
    requireIndex(index)
    entries(index.toInt)

  def sameKeyOrderAs[V2](other: VertexBasis[K, V2]): Boolean =
    keys == other.keys

  def sameKeySetAs[V2](other: VertexBasis[K, V2]): Boolean =
    indexByKey.keySet == other.keySet

  def sameMetadataAs(other: VertexBasis[K, V]): Boolean =
    entries == other.entries

  def mapValues[V2](f: (K, V) => V2): VertexBasis[K, V2] =
    val mapped = entries.map { case (key, value) => key -> f(key, value) }
    VertexBasis.unsafe(mapped)

  def permutationTo[V2](other: VertexBasis[K, V2]): Either[BasisError[K], BasisPermutation[K]] =
    val sourceOnly = keys.filterNot(other.contains)
    val targetOnly = other.keys.filterNot(contains)
    if sourceOnly.nonEmpty || targetOnly.nonEmpty then
      Left(BasisError.KeySetMismatch(sourceOnly, targetOnly))
    else
      val targetBySource = keys.map(key => other.indexOf(key).get)
      val sourceByTarget = other.keys.map(key => indexOf(key).get)
      Right(new BasisPermutation(keys, other.keys, targetBySource, sourceByTarget))

  def inducedByKeys(selectedKeys: Iterable[K]): Either[BasisError[K], InducedVertexBasis[K, V]] =
    val requested = selectedKeys.toVector
    firstDuplicate(requested) match
      case Some(key) =>
        Left(BasisError.DuplicateSelection(key))
      case None =>
        requested.find(key => !contains(key)) match
          case Some(key) =>
            Left(BasisError.UnknownKey(key))
          case None =>
            val selected = requested.toSet
            val sourceIndices = indices.filter(index => selected.contains(keyAt(index)))
            val inducedEntries = sourceIndices.map(entryAt)
            Right(new InducedVertexBasis(VertexBasis.unsafe(inducedEntries), sourceIndices, size))

  def reindex(orderedKeys: Iterable[K]): Either[BasisError[K], ReindexedVertexBasis[K, V]] =
    val requested = orderedKeys.toVector
    firstDuplicate(requested) match
      case Some(key) =>
        Left(BasisError.DuplicateSelection(key))
      case None =>
        val sourceOnly = keys.filterNot(requested.toSet)
        val targetOnly = requested.filterNot(contains)
        if sourceOnly.nonEmpty || targetOnly.nonEmpty then
          Left(BasisError.KeySetMismatch(sourceOnly, targetOnly))
        else
          val reordered = VertexBasis.unsafe(requested.map(key => key -> valueAt(indexOf(key).get)))
          permutationTo(reordered).map(permutation => new ReindexedVertexBasis(reordered, permutation))

  private[graph] def keySet: Set[K] =
    indexByKey.keySet

  private def requireIndex(index: VertexIx): Unit =
    require(contains(index), s"vertex index ${index.toInt} out of bounds for basis size $size")

  private def firstDuplicate(values: Vector[K]): Option[K] =
    val seen = mutable.HashSet.empty[K]
    values.find(value => !seen.add(value))

  override def equals(other: Any): Boolean =
    other match
      case that: VertexBasis[?, ?] => entries == that.entries
      case _                       => false

  override def hashCode(): Int =
    entries.hashCode

  override def toString: String =
    s"VertexBasis(${entries.mkString(",")})"

object VertexBasis:
  def from[K, V](entries: Iterable[(K, V)]): Either[BasisError[K], VertexBasis[K, V]] =
    val values = entries.toVector
    val firstByKey = mutable.HashMap.empty[K, Int]
    var index = 0
    var error = Option.empty[BasisError[K]]
    while index < values.length && error.isEmpty do
      val key = values(index)._1
      firstByKey.get(key) match
        case Some(first) =>
          error = Some(BasisError.DuplicateKey(key, first, index))
        case None =>
          firstByKey(key) = index
      index += 1

    error match
      case Some(value) => Left(value)
      case None        => Right(unsafe(values))

  def fromKeys[K](keys: Iterable[K]): Either[BasisError[K], VertexBasis[K, Unit]] =
    from(keys.iterator.map(_ -> ()).toVector)

  private[graph] def unsafe[K, V](entries: Vector[(K, V)]): VertexBasis[K, V] =
    val positions = entries.zipWithIndex.map { case ((key, _), index) => key -> VertexIx.unsafe(index) }.toMap
    new VertexBasis(entries, positions)

final class BasisPermutation[K] private[graph] (
    val sourceKeys: Vector[K],
    val targetKeys: Vector[K],
    val targetIndicesBySource: Vector[VertexIx],
    val sourceIndicesByTarget: Vector[VertexIx]
):
  require(sourceKeys.length == targetKeys.length, "permutation bases must have equal sizes")
  require(targetIndicesBySource.length == sourceKeys.length, "source permutation length must match source basis")
  require(sourceIndicesByTarget.length == targetKeys.length, "target permutation length must match target basis")

  def size: Int =
    sourceKeys.length

  def targetOf(source: VertexIx): VertexIx =
    require(source.toInt >= 0 && source.toInt < size, s"source index ${source.toInt} out of bounds for permutation size $size")
    targetIndicesBySource(source.toInt)

  def sourceOf(target: VertexIx): VertexIx =
    require(target.toInt >= 0 && target.toInt < size, s"target index ${target.toInt} out of bounds for permutation size $size")
    sourceIndicesByTarget(target.toInt)

  def inverse: BasisPermutation[K] =
    new BasisPermutation(targetKeys, sourceKeys, sourceIndicesByTarget, targetIndicesBySource)

  def andThen(next: BasisPermutation[K]): Either[BasisError[K], BasisPermutation[K]] =
    if targetKeys != next.sourceKeys then
      Left(BasisError.IncompatiblePermutation(targetKeys, next.sourceKeys))
    else
      val composedTarget = targetIndicesBySource.map(next.targetOf)
      val composedSource = next.sourceIndicesByTarget.map(sourceOf)
      Right(new BasisPermutation(sourceKeys, next.targetKeys, composedTarget, composedSource))

  override def equals(other: Any): Boolean =
    other match
      case that: BasisPermutation[?] =>
        sourceKeys == that.sourceKeys &&
          targetKeys == that.targetKeys &&
          targetIndicesBySource == that.targetIndicesBySource &&
          sourceIndicesByTarget == that.sourceIndicesByTarget
      case _ => false

  override def hashCode(): Int =
    var result = sourceKeys.hashCode
    result = 31 * result + targetKeys.hashCode
    result = 31 * result + targetIndicesBySource.hashCode
    31 * result + sourceIndicesByTarget.hashCode

final class InducedVertexBasis[K, V] private[graph] (
    val basis: VertexBasis[K, V],
    val sourceIndicesByInduced: Vector[VertexIx],
    val sourceSize: Int
):
  require(basis.size == sourceIndicesByInduced.length, "induced basis mapping length must match basis size")
  require(sourceIndicesByInduced.forall(index => index.toInt >= 0 && index.toInt < sourceSize), "induced source indices must be in bounds")
  require(sourceIndicesByInduced.map(_.toInt).distinct.length == sourceIndicesByInduced.length, "induced source indices must be unique")

  private val inducedBySource: Map[Int, VertexIx] =
    sourceIndicesByInduced.zipWithIndex.map { case (source, induced) => source.toInt -> VertexIx.unsafe(induced) }.toMap

  def sourceOf(induced: VertexIx): VertexIx =
    require(basis.contains(induced), s"induced index ${induced.toInt} out of bounds for basis size ${basis.size}")
    sourceIndicesByInduced(induced.toInt)

  def inducedOf(source: VertexIx): Option[VertexIx] =
    if source.toInt < 0 || source.toInt >= sourceSize then None
    else inducedBySource.get(source.toInt)

  override def equals(other: Any): Boolean =
    other match
      case that: InducedVertexBasis[?, ?] =>
        basis == that.basis &&
          sourceIndicesByInduced == that.sourceIndicesByInduced &&
          sourceSize == that.sourceSize
      case _ => false

  override def hashCode(): Int =
    var result = basis.hashCode
    result = 31 * result + sourceIndicesByInduced.hashCode
    31 * result + sourceSize

final class ReindexedVertexBasis[K, V] private[graph] (
    val basis: VertexBasis[K, V],
    val permutation: BasisPermutation[K]
):
  require(basis.keys == permutation.targetKeys, "reindexed basis must match permutation target keys")

  override def equals(other: Any): Boolean =
    other match
      case that: ReindexedVertexBasis[?, ?] => basis == that.basis && permutation == that.permutation
      case _                                => false

  override def hashCode(): Int =
    31 * basis.hashCode + permutation.hashCode
