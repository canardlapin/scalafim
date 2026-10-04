package scalafim.atlas

import locus4s.DomainAlignmentError
import locus4s.SpaceMismatch
import locus4s.data.Field
import locus4s.data.VectorField

/** A derived display row, never an independent source of atlas metadata. */
final case class ParcelRecord[A](region: AtlasRegionMetadata, value: A)

/** Full namespaced parcel identity, as published by a realization. */
opaque type AtlasParcelKey = String

object AtlasParcelKey:
  def from(value: String): Either[ParcelFieldError, AtlasParcelKey] =
    if value.trim.nonEmpty then Right(value)
    else Left(ParcelFieldError.InvalidKey(value))

  extension (key: AtlasParcelKey)
    inline def value: String = key

  private[atlas] def unsafe(value: String): AtlasParcelKey = value

enum UnknownParcelPolicy:
  case Reject, Drop

enum MissingParcelPolicy[+A]:
  case Reject
  case Fill(value: A)

enum ParcelFieldError:
  case InvalidKey(value: String)
  case WrongOwner(cause: SpaceMismatch)
  case Alignment(cause: DomainAlignmentError)
  case DuplicateKeys(keys: Vector[AtlasParcelKey])
  case UnknownKeys(keys: Vector[AtlasParcelKey])
  case MissingKeys(keys: Vector[AtlasParcelKey])
  case DuplicateSourceIds(ids: Vector[RegionId])
  case UnknownSourceIds(ids: Vector[RegionId])
  case ExpectedGlasserIdentity(actual: ParcelIdentity)
  case DuplicateGlasserKeys(keys: Vector[GlasserParcelKey])
  case UnknownGlasserKeys(keys: Vector[GlasserParcelKey])
  case GlasserIdentity(cause: ParcelIdentityError)

  def message: String = this match
    case InvalidKey(value) => s"empty parcel key '$value'"
    case WrongOwner(cause) => s"parcel field has a different live owner: $cause"
    case Alignment(cause) => s"parcel domains cannot align: $cause"
    case DuplicateKeys(keys) => s"duplicate parcel keys: ${keys.map(_.value).mkString(", ")}"
    case UnknownKeys(keys) => s"unknown parcel keys: ${keys.map(_.value).mkString(", ")}"
    case MissingKeys(keys) => s"missing parcel keys: ${keys.map(_.value).mkString(", ")}"
    case DuplicateSourceIds(ids) => s"duplicate source IDs: ${ids.mkString(", ")}"
    case UnknownSourceIds(ids) => s"unknown source IDs: ${ids.mkString(", ")}"
    case ExpectedGlasserIdentity(actual) => s"Glasser key import requires Glasser identity, got $actual"
    case DuplicateGlasserKeys(keys) => s"duplicate Glasser keys: ${keys.map(_.value).mkString(", ")}"
    case UnknownGlasserKeys(keys) => s"unknown Glasser keys: ${keys.map(_.value).mkString(", ")}"
    case GlasserIdentity(cause) => cause.message

/** Atlas admission and presentation policy over locus4s fields. Storage order
  * is canonical domain order; only records use the realization's display order.
  */
object ParcelFields:
  def keys(realization: AtlasRealization): Field[realization.P, AtlasParcelKey] =
    realization.parcelKeys.map(AtlasParcelKey.unsafe)

  def records[A](realization: AtlasRealization)(
      values: Field[realization.P, A]
  ): Either[ParcelFieldError, Vector[ParcelRecord[A]]] =
    if !realization.parcelDomain.sameRuntimeOwnerAs(values.space) then
      Left(ParcelFieldError.WrongOwner(SpaceMismatch.between(realization.parcelDomain, values.space)))
    else
      Right(realization.displayOrder.indices.map: parcel =>
        ParcelRecord(realization.metadata(parcel), values(parcel))
      .toVector)

  /** Explicit transport through exact persistent ordered identity. */
  def alignTo[Q, A](target: AtlasRealization)(
      values: Field[Q, A]
  ): Either[ParcelFieldError, Field[target.P, A]] =
    values.space.align(target.parcelDomain)
      .left.map(ParcelFieldError.Alignment.apply)
      .map(values.rebind)

  def fromKeys[A](target: AtlasRealization)(
      entries: Vector[(AtlasParcelKey, A)],
      unknown: UnknownParcelPolicy = UnknownParcelPolicy.Reject,
      missing: MissingParcelPolicy[A] = MissingParcelPolicy.Reject
  ): Either[ParcelFieldError, Field[target.P, A]] =
    val duplicates = duplicateKeys(entries.map(_._1))
    if duplicates.nonEmpty then Left(ParcelFieldError.DuplicateKeys(duplicates))
    else
      val expected = keys(target).toVector
      val expectedSet = expected.toSet
      val unknownKeys = entries.map(_._1).filterNot(expectedSet)
      if unknown == UnknownParcelPolicy.Reject && unknownKeys.nonEmpty then
        Left(ParcelFieldError.UnknownKeys(unknownKeys))
      else
        val indexed = entries.toMap
        val absent = expected.filterNot(indexed.contains)
        missing match
          case MissingParcelPolicy.Reject if absent.nonEmpty => Left(ParcelFieldError.MissingKeys(absent))
          case MissingParcelPolicy.Reject =>
            Right(VectorField.tabulate(target.parcelDomain)(p => indexed(AtlasParcelKey.unsafe(target.parcelKeys(p)))))
          case MissingParcelPolicy.Fill(value) =>
            Right(VectorField.tabulate(target.parcelDomain)(p => indexed.getOrElse(AtlasParcelKey.unsafe(target.parcelKeys(p)), value)))

  /** Integer labels are qualified by this source's actual ID-to-key mapping.
    * Canonical domain equality does not imply equal integer encodings.
    */
  def fromSourceIds[A](target: AtlasRealization)(
      source: AtlasRealization,
      entries: Vector[(RegionId, A)],
      unknown: UnknownParcelPolicy = UnknownParcelPolicy.Reject,
      missing: MissingParcelPolicy[A] = MissingParcelPolicy.Reject
  ): Either[ParcelFieldError, Field[target.P, A]] =
    val duplicates = duplicateKeys(entries.map(_._1))
    if duplicates.nonEmpty then Left(ParcelFieldError.DuplicateSourceIds(duplicates))
    else
      val encoding = source.parcelDomain.indices.map: p =>
        source.metadata(p).id -> AtlasParcelKey.unsafe(source.parcelKeys(p))
      .toMap
      val unknownIds = entries.map(_._1).filterNot(encoding.contains)
      if unknown == UnknownParcelPolicy.Reject && unknownIds.nonEmpty then
        Left(ParcelFieldError.UnknownSourceIds(unknownIds))
      else
        val keyed = entries.flatMap((id, value) => encoding.get(id).map(_ -> value))
        fromKeys(target)(keyed, unknown, missing)

  /** Anatomical names are checked and normalized before this boundary. Their
    * namespace comes from the admitted target, never a duplicated key codec.
    */
  def fromGlasserKeys[A](target: AtlasRealization)(
      entries: Vector[(GlasserParcelKey, A)],
      unknown: UnknownParcelPolicy = UnknownParcelPolicy.Reject,
      missing: MissingParcelPolicy[A] = MissingParcelPolicy.Reject
  ): Either[ParcelFieldError, Field[target.P, A]] =
    val duplicates = duplicateKeys(entries.map(_._1))
    if duplicates.nonEmpty then Left(ParcelFieldError.DuplicateGlasserKeys(duplicates))
    else if target.ref.parcelIdentity != ParcelIdentity.GlasserHcpMmp1 then
      Left(ParcelFieldError.ExpectedGlasserIdentity(target.ref.parcelIdentity))
    else
      val builder = Map.newBuilder[GlasserParcelKey, AtlasParcelKey]
      val parcels = target.parcelDomain.indices.iterator
      var failure: Option[ParcelFieldError] = None
      while parcels.hasNext && failure.isEmpty do
        val p = parcels.next()
        GlasserParcelKey.fromRegion(target.metadata(p)) match
          case Left(error) => failure = Some(ParcelFieldError.GlasserIdentity(error))
          case Right(key) => builder += key -> AtlasParcelKey.unsafe(target.parcelKeys(p))
      failure match
        case Some(error) => Left(error)
        case None =>
          val encoding = builder.result()
          val unknownKeys = entries.map(_._1).filterNot(encoding.contains)
          if unknown == UnknownParcelPolicy.Reject && unknownKeys.nonEmpty then
            Left(ParcelFieldError.UnknownGlasserKeys(unknownKeys))
          else fromKeys(target)(entries.flatMap((key, value) => encoding.get(key).map(_ -> value)), unknown, missing)

  private def duplicateKeys[K](keys: Vector[K]): Vector[K] =
    val seen = scala.collection.mutable.HashSet.empty[K]
    val duplicates = scala.collection.mutable.LinkedHashSet.empty[K]
    keys.foreach(key => if !seen.add(key) then duplicates += key)
    duplicates.toVector
