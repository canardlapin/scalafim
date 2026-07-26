package scalafim.locus

import cats.kernel.CommutativeMonoid

object Aggregation:
  def foldMapBy[X, P, A, M](
      parcellation: Parcellation[X, P],
      field: IndexedField[X, A]
  )(
      contribution: A => M
  )(using monoid: CommutativeMonoid[M]): Either[SpaceMismatch, IndexedField[P, M]] =
    if !parcellation.ambient.sameIdentityAs(field.space) then
      Left:
        SpaceMismatch(
          parcellation.ambient.key,
          parcellation.ambient.size,
          field.space.key,
          field.space.size
        )
    else
      val accumulated =
        scala.collection.mutable.ArrayBuffer.fill(parcellation.parcels.size)(monoid.empty)
      var ambientOrdinal = 0
      while ambientOrdinal < parcellation.ambient.size do
        val ambientPoint = parcellation.ambient.point(ambientOrdinal).get
        parcellation.parcelAt(ambientPoint).foreach: parcel =>
          val next = contribution(field(ambientPoint))
          accumulated(parcel.ordinal) =
            monoid.combine(accumulated(parcel.ordinal), next)
        ambientOrdinal += 1
      Right:
        IndexedField.fromValues(parcellation.parcels, accumulated).toOption.get
