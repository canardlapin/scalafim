package scalafim.atlas

import scalafim.image.{
  ClusteredNeuroVol,
  GridMismatch,
  NeuroSpaceError,
  VolumeDomain,
  VolumeOrdinalBridgeError,
  VolumeSpace
}
import locus4s.{
  Bijection,
  CertifiedMapError,
  DomainAlignmentError,
  DomainRegistry,
  FiniteDomain,
  PartialMapError,
  PartialSurjection,
  SelectionError,
  SpaceMismatch,
  Surjection,
  TotalMapError
}
import locus4s.data.FieldConstructionError
import scalafim.locus.{
  DomainFactory,
  DomainFactoryError,
  FiniteSpace,
  IndexedField,
  Point,
  Region as LocusRegion,
  Selection,
  SpaceKey
}

trait AtlasNetworkAssignment[P]:
  type N
  val networkIds: IndexedField[N, NetworkId]
  val parcelToNetwork: Surjection[P, N]

  def networkPoint(id: NetworkId): Option[Point[N]] =
    networkIds.space.indices.find(index => networkIds(index) == id)

trait AtlasNetworkQuotient[X]:
  type N
  val assignment: PartialSurjection[X, N]
  val networkIds: IndexedField[N, NetworkId]

trait AtlasQuotient:
  type X
  type P

  val registry: DomainRegistry
  val ref: AtlasRef
  val provenance: AtlasProvenance
  val parcelAssignment: PartialSurjection[X, P]
  val parcelKeys: IndexedField[P, String]
  val regionIds: IndexedField[P, RegionId]
  val metadata: IndexedField[P, AtlasRegionMetadata]
  val displayOrder: Selection[P]
  val networkAssignment: Option[AtlasNetworkAssignment[P]]
  val parcelDomainRecord: NeuropublishFiniteIndexedDomainV1
  val neuropublishSupportDomains: Vector[NeuropublishSpatialDomainV1]
  val neuropublishAssignments: Vector[NeuropublishHardAssignmentV1]

  final def parcelDomain: FiniteDomain[P] =
    parcelAssignment.to

  final def support: LocusRegion[X] =
    parcelAssignment.support

  final def parcelPoint(id: RegionId): Option[Point[P]] =
    regionIds.space.indices.find(index => regionIds(index) == id)

  final def parcelPointByKey(key: String): Option[Point[P]] =
    parcelKeys.space.indices.find(index => parcelKeys(index) == key)

  final def region(id: RegionId): Option[LocusRegion[X]] =
    parcelPoint(id).map(parcelAssignment.fiber)

  /** Retarget through exact persistent ordered identity. Equal size or labels
    * are insufficient; locus4s must produce alignment evidence.
    */
  final def assignmentAlignedTo[Q](
      target: FiniteDomain[Q]
  ): Either[DomainAlignmentError, PartialSurjection[X, Q]] =
    parcelDomain
      .align(target)
      .map(parcelAssignment.rebindTo)

  /** Retarget a different ordered parcel domain only through a caller-supplied
    * certified bijection whose source is this exact live parcel owner.
    */
  final def assignmentRetargeted[Q](
      bijection: Bijection[P, Q]
  ): Either[SpaceMismatch, PartialSurjection[X, Q]] =
    if parcelDomain.sameRuntimeOwnerAs(bijection.from) then
      Right(parcelAssignment.andThen(bijection.toSurjection))
    else Left(SpaceMismatch.between(parcelDomain, bijection.from))

  final def networkQuotient: Option[AtlasNetworkQuotient[X]] =
    networkAssignment.map: network =>
      val coarsened = parcelAssignment.andThen(network.parcelToNetwork)
      new AtlasNetworkQuotient[X]:
        type N = network.N
        val assignment: PartialSurjection[X, N] = coarsened
        val networkIds: IndexedField[N, NetworkId] = network.networkIds

  final def networkRegion(id: NetworkId): Option[LocusRegion[X]] =
    networkQuotient.flatMap: network =>
      network.networkIds.space.indices
        .find(index => network.networkIds(index) == id)
        .map(network.assignment.fiber)

  final lazy val neuropublishProjection: NeuropublishAtlasProjectionV1 =
    val order =
      displayOrder.indices.map(parcelKeys.apply).toVector
    val hierarchy =
      networkAssignment.map: network =>
        NeuropublishParcelHierarchyV1(
          networkKeys =
            network.networkIds.space.indices
              .map(point => network.networkIds(point).value)
              .toVector,
          parcelToNetworkOrdinals =
            network.parcelToNetwork.toTotalMap.targetOrdinals.toVector
        )
    NeuropublishAtlasProjectionV1(
      atlasProvenance =
        AtlasPublicationProjection.provenance(ref, provenance),
      parcelDomain = parcelDomainRecord,
      parcelMetadata =
        AtlasPublicationProjection.metadata(parcelKeys, metadata),
      displayOrder = order,
      hierarchy = hierarchy,
      supportDomains = neuropublishSupportDomains,
      assignments = neuropublishAssignments
    )

  final def validateNeuropublishProjection(
      projection: NeuropublishAtlasProjectionV1
  ): Either[AtlasPublicationError, Unit] =
    AtlasPublicationProjection.validate(neuropublishProjection, projection)

trait VolumeAtlasQuotient extends AtlasQuotient:
  val domain: VolumeDomain[X]
  val volumeSupportDomain: NeuropublishVolumeGridDomainV1

trait SurfaceAtlasQuotient extends AtlasQuotient:
  val leftVertexCount: Int
  val rightVertexCount: Int
  val leftSupportDomain: NeuropublishSurfaceVerticesDomainV1
  val rightSupportDomain: NeuropublishSurfaceVerticesDomainV1

type AtlasRealization = AtlasQuotient
type VolumeAtlasRealization = VolumeAtlasQuotient
type SurfaceAtlasRealization = SurfaceAtlasQuotient

enum AtlasQuotientError:
  case InvalidAtlas(error: AtlasError)
  case InvalidVolumeSpace(error: NeuroSpaceError)
  case VolumeDomain(error: VolumeOrdinalBridgeError)
  case VolumeField(error: GridMismatch)
  case PartialMap(error: PartialMapError)
  case TotalMap(error: TotalMapError)
  case CertifiedMap(error: CertifiedMapError)
  case Field(error: FieldConstructionError)
  case Selection(error: SelectionError)
  case DomainFactory(error: DomainFactoryError)
  case Publication(error: AtlasPublicationError)

  def message: String =
    this match
      case InvalidAtlas(error) => error.message
      case InvalidVolumeSpace(error) => error.message
      case VolumeDomain(error) => error.message
      case VolumeField(error) => error.message
      case PartialMap(error) => error.message
      case TotalMap(error) => error.message
      case CertifiedMap(error) => error.message
      case Field(error) => error.message
      case Selection(error) => error.message
      case DomainFactory(error) => error.message
      case Publication(error) => error.message

object AtlasQuotient:
  /** Throwing convenience constructor for callers with already-validated
    * atlas inputs. Use `volumeIn` at trust boundaries.
    */
  def volume(
      ref: AtlasRef,
      regions: RegionIndex,
      volume: ClusteredNeuroVol,
      provenance: AtlasProvenance
  ): VolumeAtlasQuotient =
    volumeIn(
      DomainRegistry.empty,
      ref,
      regions,
      volume,
      provenance
    ).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Checked volume realization constructor. */
  def volumeIn(
      registry: DomainRegistry,
      atlasRef: AtlasRef,
      regions: RegionIndex,
      volume: ClusteredNeuroVol,
      atlasProvenance: AtlasProvenance
  ): Either[AtlasQuotientError, VolumeAtlasQuotient] =
    val checkedVolumeSpace =
      for
        _ <- validateProvenanceCoherence(
          atlasRef,
          regions,
          atlasProvenance,
          AtlasRepresentation.Volume
        )
        volumeSpace <-
          VolumeSpace
            .fromSpatialPart(volume.space)
            .left
            .map(AtlasQuotientError.InvalidVolumeSpace.apply)
      yield volumeSpace
    checkedVolumeSpace.flatMap: volumeSpace =>
        VolumeDomain
          .canonicalIn(
            registry,
            volumeSpace
          )
          .left
          .map(AtlasQuotientError.VolumeDomain.apply)
          .flatMap: packedVolumeDomain =>
            type Voxel = packedVolumeDomain.S
            val volumeDomain: VolumeDomain[Voxel] = packedVolumeDomain.value
            AtlasParcelDomain
              .restore(
                packedVolumeDomain.registry,
                atlasRef,
                atlasProvenance,
                regions
              )
              .left
              .map(AtlasQuotientError.Publication.apply)
              .flatMap: parcelResolution =>
                type Parcel = parcelResolution.P
                val parcels: FiniteSpace[Parcel] = parcelResolution.space
                val parcelOrdinalById =
                  regions.ids.zipWithIndex.toMap
                val dense = volume.toDense
                volumeDomain
                  .indexedField(dense)
                  .left
                  .map(AtlasQuotientError.VolumeField.apply)
                  .flatMap: labels =>
                    volumeAssignments(
                      volumeDomain.finiteSpace,
                      labels,
                      parcelOrdinalById
                    ).flatMap: assignments =>
                      PartialSurjection
                        .fromOptionalTargetOrdinals(
                          volumeDomain.finiteSpace,
                          parcels,
                          assignments
                        )
                        .left
                        .map(partialAssignmentError)
                        .flatMap: assignmentValue =>
                          parcelFields(parcels, regions).flatMap: fields =>
                            val volumeSupport =
                              AtlasPublicationProjection.volumeDomain(
                                atlasRef.coordSpace.value,
                                volumeDomain.gridDomainRecord
                              )
                            val publicationAssignment =
                              NeuropublishHardAssignmentV1(
                                localId = "volume-hard-assignment",
                                source = volumeSupport.identity,
                                target = parcelResolution.publication.identity,
                                targetOrdinals = assignments.map(_.getOrElse(-1)),
                                coverage = NeuropublishTargetCoverageV1.Complete,
                                provenance =
                                  AtlasPublicationProjection.assignmentProvenance(
                                    regions,
                                    parcelResolution.publication
                                  )
                              )
                            networkAssignment(
                              parcelResolution.registry,
                              parcels,
                              regions
                            ).map: networkResult =>
                              new VolumeAtlasQuotient:
                                type X = Voxel
                                type P = Parcel
                                val registry: DomainRegistry = networkResult._1
                                val ref: AtlasRef = atlasRef
                                val provenance: AtlasProvenance = atlasProvenance
                                val domain: VolumeDomain[Voxel] = volumeDomain
                                val volumeSupportDomain: NeuropublishVolumeGridDomainV1 =
                                  volumeSupport
                                val parcelAssignment: PartialSurjection[Voxel, Parcel] =
                                  assignmentValue
                                val parcelKeys: IndexedField[Parcel, String] =
                                  parcelResolution.keys
                                val regionIds: IndexedField[Parcel, RegionId] =
                                  fields.regionIds
                                val metadata: IndexedField[Parcel, AtlasRegionMetadata] =
                                  fields.metadata
                                val displayOrder: Selection[Parcel] =
                                  fields.displayOrder
                                val networkAssignment: Option[AtlasNetworkAssignment[Parcel]] =
                                  networkResult._2
                                val parcelDomainRecord: NeuropublishFiniteIndexedDomainV1 =
                                  parcelResolution.publication
                                val neuropublishSupportDomains: Vector[NeuropublishSpatialDomainV1] =
                                  Vector(volumeSupport)
                                val neuropublishAssignments: Vector[NeuropublishHardAssignmentV1] =
                                  Vector(publicationAssignment)

  /** Throwing convenience constructor for callers with already-validated
    * atlas inputs. Use `surfaceIn` at trust boundaries.
    */
  def surface(
      ref: AtlasRef,
      regions: RegionIndex,
      payload: SurfaceAtlasPayload,
      provenance: AtlasProvenance
  ): SurfaceAtlasQuotient =
    surfaceIn(
      DomainRegistry.empty,
      ref,
      regions,
      payload,
      provenance
    ).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Checked surface realization constructor. */
  def surfaceIn(
      registry: DomainRegistry,
      atlasRef: AtlasRef,
      regions: RegionIndex,
      payload: SurfaceAtlasPayload,
      atlasProvenance: AtlasProvenance
  ): Either[AtlasQuotientError, SurfaceAtlasQuotient] =
    validateProvenanceCoherence(
      atlasRef,
      regions,
      atlasProvenance,
      AtlasRepresentation.Surface
    ).flatMap(_ =>
      surfaceValidatedIn(
        registry,
        atlasRef,
        regions,
        payload,
        atlasProvenance
      )
    )

  private def surfaceValidatedIn(
      registry: DomainRegistry,
      atlasRef: AtlasRef,
      regions: RegionIndex,
      payload: SurfaceAtlasPayload,
      atlasProvenance: AtlasProvenance
  ): Either[AtlasQuotientError, SurfaceAtlasQuotient] =
    val leftCount = payload.left.geometry.vertexCount
    val rightCount = payload.right.geometry.vertexCount
    val leftSupport =
      AtlasPublicationProjection.surfaceDomain(
        "left-surface-support",
        atlasRef.coordSpace.value,
        scalafim.surface.Hemisphere.Left,
        payload.left
      )
    val rightSupport =
      AtlasPublicationProjection.surfaceDomain(
        "right-surface-support",
        atlasRef.coordSpace.value,
        scalafim.surface.Hemisphere.Right,
        payload.right
      )
    AtlasPublicationProjection
      .bilateralRecord(atlasRef.coordSpace.value, leftSupport, rightSupport)
      .left
      .map(AtlasQuotientError.Publication.apply)
      .flatMap: ambientRecord =>
        registry
          .restore(ambientRecord)
          .left
          .map(error =>
            AtlasQuotientError.Publication(
              AtlasPublicationError.SupportDomainRestore(error)
            )
          )
      .flatMap: ambientResolution =>
        type Vertex = ambientResolution.S
        val ambient: FiniteSpace[Vertex] = ambientResolution.space
        AtlasParcelDomain
          .restore(
            ambientResolution.registry,
            atlasRef,
            atlasProvenance,
            regions
          )
          .left
          .map(AtlasQuotientError.Publication.apply)
          .flatMap: parcelResolution =>
            type Parcel = parcelResolution.P
            val parcels: FiniteSpace[Parcel] = parcelResolution.space
            val parcelOrdinalById =
              regions.ids.zipWithIndex.toMap
            val assignments =
              Array.fill[Option[Int]](ambient.size)(None)
            for
              _ <- assignSurface(
                payload.left,
                offset = 0,
                assignments,
                parcelOrdinalById
              )
              _ <- assignSurface(
                payload.right,
                offset = leftCount,
                assignments,
                parcelOrdinalById
              )
              assignmentValue <-
                PartialSurjection
                  .fromOptionalTargetOrdinals(ambient, parcels, assignments)
                  .left
                  .map(partialAssignmentError)
              fields <- parcelFields(parcels, regions)
              networkResult <-
                networkAssignment(parcelResolution.registry, parcels, regions)
            yield
              val allTargets = assignments.toVector.map(_.getOrElse(-1))
              val leftTargets = allTargets.take(leftCount)
              val rightTargets = allTargets.drop(leftCount)
              val leftAssignment =
                NeuropublishHardAssignmentV1(
                  localId = "left-surface-hard-assignment",
                  source = leftSupport.identity,
                  target = parcelResolution.publication.identity,
                  targetOrdinals = leftTargets,
                  coverage =
                    coverage(
                      leftTargets,
                      parcelResolution.publication.elementKeys
                    ),
                  provenance =
                    AtlasPublicationProjection.assignmentProvenance(
                      regions,
                      parcelResolution.publication
                    )
                )
              val rightAssignment =
                NeuropublishHardAssignmentV1(
                  localId = "right-surface-hard-assignment",
                  source = rightSupport.identity,
                  target = parcelResolution.publication.identity,
                  targetOrdinals = rightTargets,
                  coverage =
                    coverage(
                      rightTargets,
                      parcelResolution.publication.elementKeys
                    ),
                  provenance =
                    AtlasPublicationProjection.assignmentProvenance(
                      regions,
                      parcelResolution.publication
                    )
                )
              new SurfaceAtlasQuotient:
                type X = Vertex
                type P = Parcel
                val registry: DomainRegistry = networkResult._1
                val ref: AtlasRef = atlasRef
                val provenance: AtlasProvenance = atlasProvenance
                val leftVertexCount: Int = leftCount
                val rightVertexCount: Int = rightCount
                val leftSupportDomain: NeuropublishSurfaceVerticesDomainV1 =
                  leftSupport
                val rightSupportDomain: NeuropublishSurfaceVerticesDomainV1 =
                  rightSupport
                val parcelAssignment: PartialSurjection[Vertex, Parcel] =
                  assignmentValue
                val parcelKeys: IndexedField[Parcel, String] =
                  parcelResolution.keys
                val regionIds: IndexedField[Parcel, RegionId] =
                  fields.regionIds
                val metadata: IndexedField[Parcel, AtlasRegionMetadata] =
                  fields.metadata
                val displayOrder: Selection[Parcel] =
                  fields.displayOrder
                val networkAssignment: Option[AtlasNetworkAssignment[Parcel]] =
                  networkResult._2
                val parcelDomainRecord: NeuropublishFiniteIndexedDomainV1 =
                  parcelResolution.publication
                val neuropublishSupportDomains: Vector[NeuropublishSpatialDomainV1] =
                  Vector(leftSupport, rightSupport)
                val neuropublishAssignments: Vector[NeuropublishHardAssignmentV1] =
                  Vector(leftAssignment, rightAssignment)

  private def validateProvenanceCoherence(
      ref: AtlasRef,
      regions: RegionIndex,
      provenance: AtlasProvenance,
      expectedRepresentation: AtlasRepresentation
  ): Either[AtlasQuotientError, Unit] =
    val error = AtlasPublicationError.AtlasProvenanceMismatch.apply
    val expectedEncoding =
      expectedRepresentation match
        case AtlasRepresentation.Volume => LabelEncoding.VolumeIntegerLabels
        case AtlasRepresentation.Surface => LabelEncoding.SurfaceIntegerLabels
        case AtlasRepresentation.Derived => LabelEncoding.DerivedLabels
    val supportMatches =
      (expectedRepresentation, provenance.support) match
        case (
              AtlasRepresentation.Volume,
              SpatialSupport.Volume(template, coordinate, _, _)
            ) =>
          template == ref.templateSpace && coordinate == ref.coordSpace
        case (
              AtlasRepresentation.Surface,
              SpatialSupport.Surface(template, _, _)
            ) =>
          template == ref.templateSpace
        case (
              AtlasRepresentation.Derived,
              SpatialSupport.Derived(template, coordinate)
            ) =>
          template == ref.templateSpace && coordinate == ref.coordSpace
        case _ => false
    val detail =
      if ref.representation != expectedRepresentation then
        Some("atlas reference representation disagrees with realization kind")
      else if provenance.identity.family != ref.family ||
          provenance.identity.model != ref.model
      then Some("atlas identity disagrees with atlas reference")
      else if provenance.labels.encoding != expectedEncoding then
        Some("label encoding disagrees with realization kind")
      else if provenance.labels.regionIds != regions.ids.sorted then
        Some("label-schema region ids differ from region metadata")
      else if provenance.labels.background != Some(0) then
        Some("hard-label realizations require source background label 0")
      else if !supportMatches then
        Some("declared spatial support disagrees with atlas reference")
      else None
    detail match
      case Some(value) =>
        Left(AtlasQuotientError.Publication(error(value)))
      case None => Right(())

  private final case class ParcelFields[P](
      regionIds: IndexedField[P, RegionId],
      metadata: IndexedField[P, AtlasRegionMetadata],
      displayOrder: Selection[P]
  )

  private def parcelFields[P](
      parcels: FiniteSpace[P],
      regions: RegionIndex
  ): Either[AtlasQuotientError, ParcelFields[P]] =
    for
      ids <-
        IndexedField
          .fromValues(parcels, regions.ids)
          .left
          .map(AtlasQuotientError.Field.apply)
      metadata <-
        IndexedField
          .fromValues(parcels, regions.regions)
          .left
          .map(AtlasQuotientError.Field.apply)
      displayOrder <-
        Selection
          .fromOrdinals(parcels, regions.regions.indices)
          .left
          .map(AtlasQuotientError.Selection.apply)
    yield ParcelFields(ids, metadata, displayOrder)

  private def networkAssignment[P](
      registry: DomainRegistry,
      parcels: FiniteSpace[P],
      regions: RegionIndex
  ): Either[
      AtlasQuotientError,
      (DomainRegistry, Option[AtlasNetworkAssignment[P]])
  ] =
    val assigned = regions.regions.map(_.network)
    if assigned.exists(_.isEmpty) then Right((registry, None))
    else
      val ids = assigned.flatten.distinct
      DomainFactory
        .restore(
          registry,
          SpaceKey.unsafe(
            s"${parcels.id.value}:networks:${ids.map(_.value).mkString(",")}"
          ),
          ids.length
        )
        .left
        .map(AtlasQuotientError.DomainFactory.apply)
        .flatMap: networkResolution =>
          type Network = networkResolution.S
          val networks: FiniteSpace[Network] = networkResolution.space
          val ordinalById = ids.zipWithIndex.toMap
          for
            surjection <-
              Surjection
                .fromTargetOrdinals(
                  parcels,
                  networks,
                  assigned.flatten.map(ordinalById).toArray
                )
                .left
                .map(totalAssignmentError)
            idField <-
              IndexedField
                .fromValues(networks, ids)
                .left
                .map(AtlasQuotientError.Field.apply)
          yield
            val assignment =
              new AtlasNetworkAssignment[P]:
                type N = Network
                val networkIds: IndexedField[Network, NetworkId] = idField
                val parcelToNetwork: Surjection[P, Network] = surjection
            (networkResolution.registry, Some(assignment))

  private def volumeAssignments[X](
      space: FiniteSpace[X],
      labels: IndexedField[X, Int],
      parcelOrdinalById: Map[RegionId, Int]
  ): Either[AtlasQuotientError, Vector[Option[Int]]] =
    val assignments = Array.fill[Option[Int]](space.size)(None)
    var failure = Option.empty[AtlasQuotientError]
    space.foreachIndex: point =>
      if failure.isEmpty then
        val label = labels(point)
        if label < 0 then
          failure = Some(
            AtlasQuotientError.InvalidAtlas(
              AtlasError.InvalidRegionMetadata(
                s"atlas volume contains negative region id $label"
              )
            )
          )
        else if label != 0 then
          val id = RegionId(label)
          parcelOrdinalById.get(id) match
            case Some(parcelOrdinal) =>
              assignments(point.ordinal) = Some(parcelOrdinal)
            case None =>
              failure = Some(
                AtlasQuotientError.InvalidAtlas(
                  AtlasError.MissingRegionId(id)
                )
              )
    failure match
      case Some(error) => Left(error)
      case None => Right(assignments.toVector)

  private def partialAssignmentError(
      error: PartialMapError | CertifiedMapError
  ): AtlasQuotientError =
    error match
      case value: PartialMapError => AtlasQuotientError.PartialMap(value)
      case value: CertifiedMapError => AtlasQuotientError.CertifiedMap(value)

  private def totalAssignmentError(
      error: TotalMapError | CertifiedMapError
  ): AtlasQuotientError =
    error match
      case value: TotalMapError => AtlasQuotientError.TotalMap(value)
      case value: CertifiedMapError => AtlasQuotientError.CertifiedMap(value)

  private def coverage(
      targetOrdinals: Vector[Int],
      parcelKeys: Vector[String]
  ): NeuropublishTargetCoverageV1 =
    val reached = Array.fill(parcelKeys.length)(false)
    targetOrdinals.foreach: ordinal =>
      if ordinal >= 0 then reached(ordinal) = true
    val empty =
      parcelKeys.indices.collect {
        case ordinal if !reached(ordinal) => parcelKeys(ordinal)
      }.toVector
    if empty.isEmpty then NeuropublishTargetCoverageV1.Complete
    else NeuropublishTargetCoverageV1.AllowEmpty(empty)

  private def assignSurface(
      labeled: scalafim.surface.LabeledSurface,
      offset: Int,
      assignments: Array[Option[Int]],
      parcelOrdinalById: Map[RegionId, Int]
  ): Either[AtlasQuotientError, Unit] =
    var row = 0
    var failure = Option.empty[AtlasQuotientError]
    while row < labeled.indices.length && failure.isEmpty do
      val label = labeled.labels(row)
      val ordinal = offset + labeled.indices(row)
      if ordinal < 0 || ordinal >= assignments.length then
        failure = Some(
          AtlasQuotientError.InvalidAtlas(
            AtlasError.InvalidRegionMetadata(
              s"surface vertex ordinal $ordinal is outside the bilateral support"
            )
          )
        )
      else if label < 0 then
        failure = Some(
          AtlasQuotientError.InvalidAtlas(
            AtlasError.InvalidRegionMetadata(
              s"surface atlas contains negative region id $label"
            )
          )
        )
      else if label != 0 then
        val id = RegionId(label)
        parcelOrdinalById.get(id) match
          case Some(parcelOrdinal) =>
            assignments(ordinal) = Some(parcelOrdinal)
          case None =>
            failure = Some(
              AtlasQuotientError.InvalidAtlas(
                AtlasError.MissingRegionId(id)
              )
            )
      row += 1
    failure match
      case Some(error) => Left(error)
      case None => Right(())
