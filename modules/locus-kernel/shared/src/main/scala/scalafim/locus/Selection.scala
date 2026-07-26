package scalafim.locus

final class Selection[S] private (
    val space: FiniteSpace[S],
    private val ordered: Array[Int],
    val region: Region[S]
):
  def size: Int =
    ordered.length

  def isEmpty: Boolean =
    ordered.isEmpty

  def apply(index: Int): Point[S] =
    Point.unsafe(ordered(index))

  def points: Iterator[Point[S]] =
    ordered.iterator.map(Point.unsafe[S])

  def ordinals: Array[Int] =
    ordered.clone()

  override def equals(other: Any): Boolean =
    other match
      case that: Selection[?] =>
        space == that.space && Selection.sameOrdinals(ordered, that.ordered)
      case _ =>
        false

  override def hashCode(): Int =
    var result = space.hashCode()
    var i = 0
    while i < ordered.length do
      result = 31 * result + ordered(i)
      i += 1
    result

  override def toString: String =
    s"Selection(${space.key.value}, size=$size)"

object Selection:
  def empty[S](space: FiniteSpace[S]): Selection[S] =
    fromOwned(space, Array.emptyIntArray)

  def fromRegion[S](region: Region[S]): Selection[S] =
    new Selection(region.space, region.ordinalsInDomainOrder, region)

  def fromOrdinals[S](
      space: FiniteSpace[S],
      ordinals: IterableOnce[Int]
  ): Either[SelectionError, Selection[S]] =
    val input = ordinals.iterator.toArray
    val seen = scala.collection.mutable.HashSet.empty[Int]
    var i = 0
    var error = Option.empty[SelectionError]
    while i < input.length && error.isEmpty do
      val ordinal = input(i)
      if !space.containsOrdinal(ordinal) then
        error = Some(SelectionError.OutOfBounds(i, ordinal, space.size))
      else if seen.contains(ordinal) then
        error = Some(SelectionError.DuplicateOrdinal(ordinal))
      else
        seen += ordinal
      i += 1

    error match
      case Some(value) =>
        Left(value)
      case None =>
        Right(fromOwned(space, input))

  def fromPoints[S](
      space: FiniteSpace[S],
      points: IterableOnce[Point[S]]
  ): Either[SelectionError, Selection[S]] =
    fromOrdinals(space, points.iterator.map(_.ordinal))

  private def fromOwned[S](space: FiniteSpace[S], ordered: Array[Int]): Selection[S] =
    new Selection(space, ordered, Region.fromValidatedOrdinals(space, ordered))

  private def sameOrdinals(left: Array[Int], right: Array[Int]): Boolean =
    if left.length != right.length then false
    else
      var i = 0
      var same = true
      while i < left.length && same do
        same = left(i) == right(i)
        i += 1
      same
