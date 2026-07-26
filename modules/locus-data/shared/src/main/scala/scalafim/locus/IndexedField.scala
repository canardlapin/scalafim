package scalafim.locus

enum IndexedFieldError:
  case WrongValueCount(expected: Int, actual: Int)

  def message: String =
    this match
      case WrongValueCount(expected, actual) =>
        s"indexed field requires $expected values, found $actual"

enum SectionSelectionError:
  case WrongSpace(error: SpaceMismatch)
  case OutsideSupport(pointOrdinal: Int)

  def message: String =
    this match
      case WrongSpace(error) =>
        error.message
      case OutsideSupport(pointOrdinal) =>
        s"selection point $pointOrdinal is outside the section support"

trait IndexedField[S, +A]:
  def space: FiniteSpace[S]
  def apply(point: Point[S]): A

  def map[B](f: A => B): IndexedField[S, B] =
    IndexedField.tabulate(space)(point => f(apply(point)))

  def restrict(region: Region[S]): Either[SpaceMismatch, Section[S, A]] =
    Section.make(this, region)

object IndexedField:
  def tabulate[S, A](
      finiteSpace: FiniteSpace[S]
  )(valueAt: Point[S] => A): IndexedField[S, A] =
    new IndexedField[S, A]:
      val space: FiniteSpace[S] = finiteSpace
      def apply(point: Point[S]): A =
        valueAt(point)

  def fromValues[S, A](
      finiteSpace: FiniteSpace[S],
      values: IterableOnce[A]
  ): Either[IndexedFieldError, IndexedField[S, A]] =
    val owned = Vector.from(values)
    if owned.length != finiteSpace.size then
      Left(IndexedFieldError.WrongValueCount(finiteSpace.size, owned.length))
    else
      Right:
        new IndexedField[S, A]:
          val space: FiniteSpace[S] = finiteSpace
          def apply(point: Point[S]): A =
            owned(point.ordinal)

final class Section[S, +A] private (
    val field: IndexedField[S, A],
    val support: Region[S]
):
  def apply(point: Point[S]): Option[A] =
    Option.when(support.contains(point))(field(point))

  def map[B](f: A => B): Section[S, B] =
    new Section(field.map(f), support)

  def restrict(region: Region[S]): Either[SpaceMismatch, Section[S, A]] =
    support.intersect(region).map(intersection => new Section(field, intersection))

  def valuesInDomainOrder: Iterator[A] =
    support.pointsInDomainOrder.map(field.apply)

  def valuesIn(
      selection: Selection[S]
  ): Either[SectionSelectionError, Iterator[A]] =
    if !support.space.sameIdentityAs(selection.space) then
      Left:
        SectionSelectionError.WrongSpace:
          SpaceMismatch(
            support.space.key,
            support.space.size,
            selection.space.key,
            selection.space.size
          )
    else
      selection.points.find(point => !support.contains(point)) match
        case Some(point) =>
          Left(SectionSelectionError.OutsideSupport(point.ordinal))
        case None =>
          Right(selection.points.map(field.apply))

object Section:
  private[locus] def make[S, A](
      field: IndexedField[S, A],
      support: Region[S]
  ): Either[SpaceMismatch, Section[S, A]] =
    if field.space.sameIdentityAs(support.space) then
      Right(new Section(field, support))
    else
      Left:
        SpaceMismatch(
          field.space.key,
          field.space.size,
          support.space.key,
          support.space.size
        )
