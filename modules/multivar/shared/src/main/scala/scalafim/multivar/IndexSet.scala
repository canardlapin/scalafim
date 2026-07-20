package scalafim.multivar

import scala.collection.mutable

final class IndexSet private (
    val axis: IndexAxis,
    private val values: Vector[Int]
):
  /** Sorted copy of the indices for O(log n) membership queries. */
  private val sorted: Array[Int] =
    val out = new Array[Int](values.length)
    var i = 0
    while i < values.length do
      out(i) = values(i)
      i += 1
    java.util.Arrays.sort(out)
    out

  def indices: Vector[Int] =
    values

  def length: Int =
    values.length

  def isEmpty: Boolean =
    values.isEmpty

  def contains(index: Int): Boolean =
    java.util.Arrays.binarySearch(sorted, index) >= 0

  def requireWithin(limit: Int): Either[MultivarError, IndexSet] =
    IndexSet.from(values, axis = axis, limit = Some(limit))

  override def equals(other: Any): Boolean =
    other match
      case that: IndexSet => axis == that.axis && values == that.values
      case _              => false

  override def hashCode(): Int =
    31 * axis.hashCode + values.hashCode

  override def toString: String =
    s"IndexSet(${axis.label}, ${values.mkString("[", ", ", "]")})"

object IndexSet:
  def from(
      indices: Iterable[Int],
      axis: IndexAxis = IndexAxis.Column,
      limit: Option[Int] = None
  ): Either[MultivarError, IndexSet] =
    val values = indices.toVector
    if values.isEmpty then Left(MultivarError.EmptyIndexSet(axis))
    else
      val seen = mutable.HashSet.empty[Int]
      var i = 0
      var error = Option.empty[MultivarError]
      while i < values.length && error.isEmpty do
        val index = values(i)
        if index < 0 then error = Some(MultivarError.IndexOutOfBounds(axis, index, limit.getOrElse(0)))
        else
          limit match
            case Some(size) if index >= size =>
              error = Some(MultivarError.IndexOutOfBounds(axis, index, size))
            case _ =>
              if seen.contains(index) then error = Some(MultivarError.DuplicateIndex(axis, index))
              else seen += index
        i += 1

      error match
        case Some(value) => Left(value)
        case None        => Right(new IndexSet(axis, values))

  def contiguous(size: Int, axis: IndexAxis = IndexAxis.Column): Either[MultivarError, IndexSet] =
    if size <= 0 then Left(MultivarError.EmptyIndexSet(axis))
    else Right(new IndexSet(axis, (0 until size).toVector))

  def unsafe(indices: Iterable[Int], axis: IndexAxis = IndexAxis.Column): IndexSet =
    from(indices, axis).fold(error => throw new IllegalArgumentException(error.message), identity)
