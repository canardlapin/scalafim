package scalafim.locus

opaque type SpaceKey = String

object SpaceKey:
  def make(value: String): Either[SpaceError, SpaceKey] =
    if value.trim.isEmpty then Left(SpaceError.EmptyKey)
    else Right(value)

  def unsafe(value: String): SpaceKey =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (key: SpaceKey)
    def value: String =
      key

opaque type Point[S] = Int

object Point:
  private[locus] inline def unsafe[S](ordinal: Int): Point[S] =
    ordinal

  extension [S](point: Point[S])
    inline def ordinal: Int =
      point

final class FiniteSpace[S] private (
    val key: SpaceKey,
    val size: Int
):
  def point(ordinal: Int): Option[Point[S]] =
    if containsOrdinal(ordinal) then Some(Point.unsafe(ordinal))
    else None

  def requirePoint(ordinal: Int): Either[PointError, Point[S]] =
    point(ordinal).toRight(PointError.OutOfBounds(ordinal, size))

  def points: Iterator[Point[S]] =
    Iterator.range(0, size).map(Point.unsafe[S])

  def contains(point: Point[S]): Boolean =
    containsOrdinal(point.ordinal)

  def sameIdentityAs[T](that: FiniteSpace[T]): Boolean =
    sameSpace(that)

  private[locus] def containsOrdinal(ordinal: Int): Boolean =
    ordinal >= 0 && ordinal < size

  private[locus] def sameSpace[T](that: FiniteSpace[T]): Boolean =
    key == that.key && size == that.size

  private[locus] def mismatch[T](actual: FiniteSpace[T]): SpaceMismatch =
    SpaceMismatch(key, size, actual.key, actual.size)

  override def equals(other: Any): Boolean =
    other match
      case that: FiniteSpace[?] =>
        sameSpace(that)
      case _ =>
        false

  override def hashCode(): Int =
    31 * key.hashCode() + size

  override def toString: String =
    s"FiniteSpace(${key.value}, $size)"

object FiniteSpace:
  def make[S](key: SpaceKey, size: Int): Either[SpaceError, FiniteSpace[S]] =
    if size < 0 then Left(SpaceError.NegativeSize(size))
    else Right(new FiniteSpace(key, size))

  private[locus] def unsafe[S](key: SpaceKey, size: Int): FiniteSpace[S] =
    new FiniteSpace(key, size)

trait SomeFiniteSpace:
  type S
  val value: FiniteSpace[S]

object SomeFiniteSpace:
  def make(key: SpaceKey, size: Int): Either[SpaceError, SomeFiniteSpace] =
    final class FreshSpace
    FiniteSpace.make[FreshSpace](key, size).map(pack)

  def pack[A](space: FiniteSpace[A]): SomeFiniteSpace { type S = A } =
    new SomeFiniteSpace:
      type S = A
      val value: FiniteSpace[A] = space
