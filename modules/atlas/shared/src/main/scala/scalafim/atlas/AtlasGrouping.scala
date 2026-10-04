package scalafim.atlas

import locus4s.DomainError
import locus4s.DomainRecord
import locus4s.DomainRestoreError
import locus4s.FiniteDomain
import locus4s.PartialMap
import locus4s.PartialMapError
import locus4s.SpaceMismatch
import locus4s.data.Aggregation
import locus4s.data.Field
import locus4s.data.VectorField

/** The metadata dimension used to group parcel-level values. */
enum AtlasGroupingKind:
  case Hemisphere, Network

  def tag: String = this match
    case Hemisphere => "hemisphere"
    case Network => "network"

/** What to do when a parcel has no annotation for the requested grouping. */
enum MissingGroupingAnnotationPolicy:
  case Reject, Drop

  def tag: String = this match
    case Reject => "reject"
    case Drop => "drop"

/** Mean weighting over already reduced parcel values.
  *
  * `EqualParcels` gives every annotated parcel one vote. `SpatialSupport`
  * gives it one vote per sample in the realization's authoritative hard
  * assignment. They intentionally have different scientific meanings when
  * parcels have unequal support sizes.
  */
enum AtlasGroupingWeight:
  case EqualParcels, SpatialSupport

  def tag: String = this match
    case EqualParcels => "equal-parcels"
    case SpatialSupport => "spatial-support"

/** Persistable identifier of a grouping level, namespaced by its kind. */
opaque type AtlasGroupId = String

object AtlasGroupId:
  extension (id: AtlasGroupId)
    inline def value: String = id

  private[atlas] def hemisphere(value: Hemisphere): AtlasGroupId =
    value match
      case Hemisphere.Left => "hemisphere:left"
      case Hemisphere.Right => "hemisphere:right"
      case Hemisphere.Bilateral => "hemisphere:bilateral"
      case Hemisphere.Midline => "hemisphere:midline"

  private[atlas] def network(value: NetworkId): AtlasGroupId =
    s"network:${value.value}"

enum AtlasGroupingError:
  case MissingAnnotations(kind: AtlasGroupingKind, parcels: Vector[AtlasParcelKey])
  case ParcelOwner(cause: SpaceMismatch)
  case NonFiniteParcelValue(parcel: AtlasParcelKey, value: Double)
  case Domain(cause: DomainError)
  case DomainRestore(cause: DomainRestoreError)
  case Mapping(cause: PartialMapError)

  def message: String = this match
    case MissingAnnotations(kind, parcels) =>
      s"${kind.tag} grouping lacks annotations for: ${parcels.map(_.value).mkString(", ")}"
    case ParcelOwner(cause) => s"parcel field has a different live owner: $cause"
    case NonFiniteParcelValue(parcel, value) =>
      s"cannot group non-finite value $value for parcel ${parcel.value}"
    case Domain(cause) => cause.message
    case DomainRestore(cause) => cause.message
    case Mapping(cause) => cause.message

/** Evidence that a grouped result is derived from one exact realization. */
final case class AtlasGroupingProvenance(
    parent: AtlasRealizationIdentity,
    parentRef: AtlasRef,
    kind: AtlasGroupingKind,
    missingAnnotations: MissingGroupingAnnotationPolicy,
    weighting: AtlasGroupingWeight
)

/** A checked, possibly partial grouping of one realization's parcel domain.
  *
  * Missing annotations under `Drop` remain undefined in `parcelToGroup`; they
  * are never assigned to an invented "unknown" group. The group owner is fresh
  * but the parcel owner is exactly the parent's live parcel owner.
  */
sealed trait AtlasGrouping[P0]:
  type G

  val parent: AtlasRealization { type P = P0 }
  val kind: AtlasGroupingKind
  val missingAnnotations: MissingGroupingAnnotationPolicy
  val groupDomain: FiniteDomain[G]
  val groupIds: Field[G, AtlasGroupId]
  val parcelToGroup: PartialMap[P0, G]
  val spatialSupportCounts: Field[P0, Long]

  final def provenance(weighting: AtlasGroupingWeight): AtlasGroupingProvenance =
    AtlasGroupingProvenance(parent.identity, parent.ref, kind, missingAnnotations, weighting)

  final def atlasProvenance(weighting: AtlasGroupingWeight): AtlasProvenance =
    parent.provenance.withDerivationStep(
      DerivationStep.GroupedParcels(
        parent.identity,
        kind.tag,
        missingAnnotations.tag,
        weighting.tag
      )
    )

/** Grouped scalar parcel results. */
final case class GroupedParcelValues[G](
    values: Field[G, Double],
    provenance: AtlasGroupingProvenance
)

object AtlasGrouping:
  def hemisphere(
      parent: AtlasRealization,
      missingAnnotations: MissingGroupingAnnotationPolicy = MissingGroupingAnnotationPolicy.Reject
  ): Either[AtlasGroupingError, AtlasGrouping[parent.P]] =
    build(parent, AtlasGroupingKind.Hemisphere, missingAnnotations)(_.hemisphere.map(AtlasGroupId.hemisphere))

  /** Build directly from parcel metadata instead of `networkAssignment`.
    * That realization field is deliberately absent unless every parcel is
    * annotated; this constructor preserves known annotations under `Drop`.
    */
  def network(
      parent: AtlasRealization,
      missingAnnotations: MissingGroupingAnnotationPolicy = MissingGroupingAnnotationPolicy.Reject
  ): Either[AtlasGroupingError, AtlasGrouping[parent.P]] =
    build(parent, AtlasGroupingKind.Network, missingAnnotations)(_.network.map(AtlasGroupId.network))

  def mean[P0](
      grouping: AtlasGrouping[P0]
  )(
      values: Field[P0, Double],
      weighting: AtlasGroupingWeight
  ): Either[AtlasGroupingError, GroupedParcelValues[grouping.G]] =
    if !grouping.parent.parcelDomain.sameRuntimeOwnerAs(values.space) then
      Left(AtlasGroupingError.ParcelOwner(SpaceMismatch.between(grouping.parent.parcelDomain, values.space)))
    else
      finiteValues(grouping, values).map: _ =>
        val weights: Field[P0, Long] =
          weighting match
            case AtlasGroupingWeight.EqualParcels => values.map(_ => 1L)
            case AtlasGroupingWeight.SpatialSupport => grouping.spatialSupportCounts
        val totals = Aggregation.foldMapBy(grouping.parcelToGroup, values.zipWith(weights): (value, weight) =>
          WeightedTotal(value * weight.toDouble, weight)
        )(WeightedTotal.empty)(identity)(_.combine(_))
        GroupedParcelValues(
          totals.map(total => total.sum / total.weight.toDouble),
          grouping.provenance(weighting)
        )

  private def finiteValues[P0](
      grouping: AtlasGrouping[P0],
      values: Field[P0, Double]
  ): Either[AtlasGroupingError, Unit] =
    var failure = Option.empty[AtlasGroupingError]
    grouping.parent.parcelDomain.foreachIndex: parcel =>
      val value = values(parcel)
      if failure.isEmpty && !value.isFinite then
        failure = Some(AtlasGroupingError.NonFiniteParcelValue(
          AtlasParcelKey.unsafe(grouping.parent.parcelKeys(parcel)), value
        ))
    failure.toLeft(())

  private def build(
      source: AtlasRealization,
      groupingKind: AtlasGroupingKind,
      missing: MissingGroupingAnnotationPolicy
  )(
      annotation: AtlasRegionMetadata => Option[AtlasGroupId]
  ): Either[AtlasGroupingError, AtlasGrouping[source.P]] =
    val parcelGroups = source.parcelDomain.indices.map(parcel => annotation(source.metadata(parcel))).toVector
    val absent = source.parcelDomain.indices.zip(parcelGroups).collect:
      case (parcel, None) => AtlasParcelKey.unsafe(source.parcelKeys(parcel))
    if missing == MissingGroupingAnnotationPolicy.Reject && absent.nonEmpty then
      Left(AtlasGroupingError.MissingAnnotations(groupingKind, absent.toVector))
    else
      val ids = parcelGroups.flatten.distinct
      val groupOrdinals = ids.zipWithIndex.toMap
      val targetOrdinals = parcelGroups.map(_.map(groupOrdinals))
      val domainId =
        s"grouping:${source.parcelDomainRecord.elementKeys.mkString(",")}:${groupingKind.tag}:${ids.map(_.value).mkString(",")}:${targetOrdinals.map(_.getOrElse(-1)).mkString(",")}"
      for
        record <- DomainRecord.parse(
          id = domainId,
          name = s"${source.parcelDomain.name.value} ${groupingKind.tag} groups",
          size = ids.size
        ).left.map(AtlasGroupingError.Domain.apply)
        resolution <- source.registry.restore(record).left.map(AtlasGroupingError.DomainRestore.apply)
        mapping <- PartialMap.fromOptionalTargetOrdinals(source.parcelDomain, resolution.space, targetOrdinals)
          .left.map(AtlasGroupingError.Mapping.apply)
      yield
        type Group = resolution.S
        val groups = VectorField.tabulate(resolution.space)(index => ids(index.ordinal))
        val supportCounts = spatialSupportCounts(source)
        new AtlasGrouping[source.P]:
          type G = Group
          val parent: AtlasRealization { type P = source.P } = source
          val kind: AtlasGroupingKind = groupingKind
          val missingAnnotations: MissingGroupingAnnotationPolicy = missing
          val groupDomain: FiniteDomain[Group] = resolution.space
          val groupIds: Field[Group, AtlasGroupId] = groups
          val parcelToGroup: PartialMap[source.P, Group] = mapping
          val spatialSupportCounts: Field[source.P, Long] = supportCounts

  private def spatialSupportCounts(parent: AtlasRealization): Field[parent.P, Long] =
    val counts = Array.fill[Long](parent.parcelDomain.size)(0L)
    parent.parcelAssignment.toPartialMap.foreachDefined: (_, parcel) =>
      counts(parcel.ordinal) += 1L
    VectorField.tabulate(parent.parcelDomain)(parcel => counts(parcel.ordinal))

  private final case class WeightedTotal(sum: Double, weight: Long):
    def combine(that: WeightedTotal): WeightedTotal =
      WeightedTotal(sum + that.sum, weight + that.weight)

  private object WeightedTotal:
    val empty: WeightedTotal = WeightedTotal(0.0, 0L)
