package scalafim.bids

import scala.collection.immutable.VectorMap

final case class BidsEntities private (values: VectorMap[EntityKey, String]):
  def get(key: EntityKey): Option[String] = values.get(key)
  def apply(key: EntityKey): String = values(key)
  def contains(key: EntityKey): Boolean = values.contains(key)
  def keys: Vector[EntityKey] = values.keys.toVector
  def isEmpty: Boolean = values.isEmpty

  def updated(key: EntityKey, value: String): Either[BidsError, BidsEntities] =
    BidsEntities.from(values.updated(key, value))

  def renderParts: Vector[String] =
    val ordered =
      EntityKey.StandardOrder.flatMap { key =>
        values.get(key).map(value => s"${key.short}-$value")
      }
    val custom =
      values.iterator
        .collect { case (key @ EntityKey.Custom(_, _), value) => key.short -> value }
        .toVector
        .sortBy(_._1)
        .map((key, value) => s"$key-$value")
    ordered ++ custom

object BidsEntities:
  val Empty: BidsEntities = BidsEntities(VectorMap.empty)

  def from(pairs: IterableOnce[(EntityKey, String)]): Either[BidsError, BidsEntities] =
    val builder = VectorMap.newBuilder[EntityKey, String]
    val seen = scala.collection.mutable.HashSet.empty[EntityKey]
    var error: Option[BidsError] = None

    for (key, value0) <- pairs.iterator if error.isEmpty do
      val value = value0.trim
      if value.isEmpty then
        error = Some(BidsError.InvalidEntityValue(key.short, value0, "value must be non-empty"))
      else if seen(key) then
        error = Some(BidsError.DuplicateEntity(key.short))
      else
        seen += key
        builder += key -> value

    error match
      case Some(err) => Left(err)
      case None      => Right(BidsEntities(builder.result()))

  def of(pairs: (EntityKey, String)*): Either[BidsError, BidsEntities] =
    from(pairs)
