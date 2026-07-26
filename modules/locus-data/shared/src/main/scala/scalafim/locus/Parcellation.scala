package scalafim.locus

enum ParcellationError:
  case WrongAssignmentCount(expected: Int, actual: Int)
  case ParcelOutOfBounds(ambientOrdinal: Int, parcelOrdinal: Int, parcelCount: Int)
  case UnusedParcel(parcelOrdinal: Int)

  def message: String =
    this match
      case WrongAssignmentCount(expected, actual) =>
        s"parcellation requires $expected assignments, found $actual"
      case ParcelOutOfBounds(ambient, parcel, parcelCount) =>
        s"ambient point $ambient has parcel ordinal $parcel outside [0, $parcelCount)"
      case UnusedParcel(parcel) =>
        s"parcel ordinal $parcel has an empty fiber"

final class Parcellation[X, P] private (
    val ambient: FiniteSpace[X],
    val parcels: FiniteSpace[P],
    private val parcelOrdinalAt: Array[Int]
):
  def parcelAt(point: Point[X]): Option[Point[P]] =
    val parcel = parcelOrdinalAt(point.ordinal)
    if parcel < 0 then None else parcels.point(parcel)

  def support: Region[X] =
    Region.tabulate(ambient)(point => parcelOrdinalAt(point.ordinal) >= 0)

  def fiber(parcel: Point[P]): Region[X] =
    Region.tabulate(ambient)(point => parcelOrdinalAt(point.ordinal) == parcel.ordinal)

  def quotientRelation: Relation[X, P] =
    val rows = Array.tabulate(ambient.size): source =>
      val parcel = parcelOrdinalAt(source)
      if parcel < 0 then Array.emptyIntArray else Array(parcel)
    Relation.fromOrdinalRows(ambient, parcels, rows).toOption.get

  def assignmentOrdinals: Vector[Option[Int]] =
    parcelOrdinalAt.toVector.map(ordinal => Option.when(ordinal >= 0)(ordinal))

  def coarsen[Q](
      mapping: Surjection[P, Q]
  ): Either[SpaceMismatch, Parcellation[X, Q]] =
    if parcels.sameIdentityAs(mapping.mapping.from) then
      val targets = mapping.mapping.targetOrdinals
      val assignments = parcelOrdinalAt.map: parcel =>
        if parcel < 0 then -1 else targets(parcel)
      Right(Parcellation.fromValidated(ambient, mapping.mapping.to, assignments))
    else
      Left:
        SpaceMismatch(
          parcels.key,
          parcels.size,
          mapping.mapping.from.key,
          mapping.mapping.from.size
        )

  def sameBlocksAs[Q](
      that: Parcellation[X, Q]
  ): Either[SpaceMismatch, Boolean] =
    if !ambient.sameIdentityAs(that.ambient) then
      Left:
        SpaceMismatch(
          ambient.key,
          ambient.size,
          that.ambient.key,
          that.ambient.size
        )
    else if parcels.size != that.parcels.size then
      Right(false)
    else
      val rightByLeft = Array.fill(parcels.size)(-1)
      val leftByRight = Array.fill(that.parcels.size)(-1)
      var ambientOrdinal = 0
      var same = true
      while ambientOrdinal < ambient.size && same do
        val left = parcelOrdinalAt(ambientOrdinal)
        val right = that.parcelOrdinalAt(ambientOrdinal)
        if (left < 0) != (right < 0) then
          same = false
        else if left >= 0 then
          if rightByLeft(left) < 0 then rightByLeft(left) = right
          else if rightByLeft(left) != right then same = false
          if leftByRight(right) < 0 then leftByRight(right) = left
          else if leftByRight(right) != left then same = false
        ambientOrdinal += 1
      Right(same)

  override def equals(other: Any): Boolean =
    other match
      case that: Parcellation[?, ?] =>
        ambient == that.ambient &&
          parcels == that.parcels &&
          Parcellation.sameAssignments(parcelOrdinalAt, that.parcelOrdinalAt)
      case _ =>
        false

  override def hashCode(): Int =
    var result = 31 * ambient.hashCode() + parcels.hashCode()
    var i = 0
    while i < parcelOrdinalAt.length do
      result = 31 * result + parcelOrdinalAt(i)
      i += 1
    result

object Parcellation:
  def fromAssignments[X, P](
      ambient: FiniteSpace[X],
      parcels: FiniteSpace[P],
      assignments: IterableOnce[Option[Int]]
  ): Either[ParcellationError, Parcellation[X, P]] =
    val publicAssignments = Vector.from(assignments)
    if publicAssignments.length != ambient.size then
      Left(ParcellationError.WrongAssignmentCount(ambient.size, publicAssignments.length))
    else
      val encoded = Array.fill(ambient.size)(-1)
      val used = Array.fill(parcels.size)(false)
      var ambientOrdinal = 0
      var error = Option.empty[ParcellationError]
      while ambientOrdinal < publicAssignments.length && error.isEmpty do
        publicAssignments(ambientOrdinal) match
          case Some(parcel) if parcel < 0 || parcel >= parcels.size =>
            error = Some:
              ParcellationError.ParcelOutOfBounds(ambientOrdinal, parcel, parcels.size)
          case Some(parcel) =>
            encoded(ambientOrdinal) = parcel
            used(parcel) = true
          case None =>
            ()
        ambientOrdinal += 1

      var parcel = 0
      while parcel < used.length && error.isEmpty do
        if !used(parcel) then error = Some(ParcellationError.UnusedParcel(parcel))
        parcel += 1

      error match
        case Some(value) => Left(value)
        case None => Right(fromValidated(ambient, parcels, encoded))

  def fromSurjection[P, Q](
      mapping: Surjection[P, Q]
  ): Parcellation[P, Q] =
    val assignments = mapping.mapping.targetOrdinals
    fromValidated(mapping.mapping.from, mapping.mapping.to, assignments)

  private[locus] def fromValidated[X, P](
      ambient: FiniteSpace[X],
      parcels: FiniteSpace[P],
      assignments: Array[Int]
  ): Parcellation[X, P] =
    new Parcellation(ambient, parcels, assignments)

  private def sameAssignments(left: Array[Int], right: Array[Int]): Boolean =
    if left.length != right.length then false
    else
      var i = 0
      var same = true
      while i < left.length && same do
        same = left(i) == right(i)
        i += 1
      same
