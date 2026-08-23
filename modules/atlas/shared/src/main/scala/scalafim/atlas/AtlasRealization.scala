package scalafim.atlas

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.locus.GridDomain
import image4s.locus.GridDomainError
import locus4s.Bijection
import locus4s.CertifiedMapError
import locus4s.DomainAlignmentError
import locus4s.DomainError
import locus4s.DomainRecord
import locus4s.DomainRegistry
import locus4s.DomainRestoreError
import locus4s.FiniteDomain
import locus4s.FiniteSpace
import locus4s.Index
import locus4s.PartialMapError
import locus4s.PartialSurjection
import locus4s.Region as LocusRegion
import locus4s.Selection
import locus4s.SelectionError
import locus4s.SpaceMismatch
import locus4s.Surjection
import locus4s.TotalMapError
import locus4s.data.Field
import locus4s.data.FieldConstructionError
import locus4s.data.VectorField
import scalafim.image.AnyNeuroVolume
import scalafim.image.SomeLabelVolume
import scalafim.image.VolumeParcellation
import scalafim.image.VolumeParcellationError
import scalafim.surface.Hemisphere as SurfaceHemisphere

trait AtlasNetworkAssignment[P]:
  type N
  val networkIds: Field[N, NetworkId]
  val parcelToNetwork: Surjection[P, N]

  def networkPoint(id: NetworkId): Option[Index[N]] =
    networkIds.space.indices.find(index => networkIds(index) == id)

trait AtlasNetworkRealization[X]:
  type N
  val assignment: PartialSurjection[X, N]
  val networkIds: Field[N, NetworkId]

/** One atlas realization over one exact live spatial owner.
  *
  * `parcelAssignment` is the only retained source of membership truth. Parcel
  * regions, network regions, dense labels, and publication assignment payloads
  * are derived from it.
  */
trait AtlasRealization:
  type X
  type P

  val registry: DomainRegistry
  val ref: AtlasRef
  val provenance: AtlasProvenance
  val parcelAssignment: PartialSurjection[X, P]
  val parcelKeys: Field[P, String]
  val metadata: Field[P, AtlasRegionMetadata]
  val displayOrder: Selection[P]
  val networkAssignment: Option[AtlasNetworkAssignment[P]]
  val parcelDomainRecord: NeuropublishFiniteIndexedDomainV1
  val neuropublishSupportDomains: Vector[NeuropublishSpatialDomainV1]
  val neuropublishAssignments: Vector[NeuropublishHardAssignmentV1]

  final def parcelDomain: FiniteDomain[P] =
    parcelAssignment.to

  final def regionIds: Field[P, RegionId] =
    metadata.map(_.id)

  final def support: LocusRegion[X] =
    parcelAssignment.support

  final def parcelPoint(id: RegionId): Option[Index[P]] =
    parcelDomain.indices.find(index => metadata(index).id == id)

  final def parcelPointByKey(key: String): Option[Index[P]] =
    parcelDomain.indices.find(index => parcelKeys(index) == key)

  final def region(id: RegionId): Option[LocusRegion[X]] =
    parcelPoint(id).map(parcelAssignment.fiber)

  /** Retarget through exact persistent ordered identity. Equal cardinality is
    * deliberately insufficient.
    */
  final def assignmentAlignedTo[Q](
      target: FiniteDomain[Q]
  ): Either[DomainAlignmentError, PartialSurjection[X, Q]] =
    parcelDomain
      .align(target)
      .map(parcelAssignment.rebindTo)

  /** Retarget a different ordered domain only through a certified bijection. */
  final def assignmentRetargeted[Q](
      bijection: Bijection[P, Q]
  ): Either[SpaceMismatch, PartialSurjection[X, Q]] =
    if parcelDomain.sameRuntimeOwnerAs(bijection.from) then
      Right(parcelAssignment.andThen(bijection.toSurjection))
    else Left(SpaceMismatch.between(parcelDomain, bijection.from))

  final def networkRealization: Option[AtlasNetworkRealization[X]] =
    networkAssignment.map: network =>
      new AtlasNetworkRealization[X]:
        type N = network.N
        val assignment: PartialSurjection[X, N] =
          parcelAssignment.andThen(network.parcelToNetwork)
        val networkIds: Field[N, NetworkId] =
          network.networkIds

  final def networkRegion(id: NetworkId): Option[LocusRegion[X]] =
    networkRealization.flatMap: network =>
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
              .map(index => network.networkIds(index).value)
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

trait VolumeAtlasRealization extends AtlasRealization:
  type F <: Frame[D3]

  val domain: GridDomain[F, D3, X]
  val parcellation: VolumeParcellation[F, X, P, AtlasRegionMetadata]
  val volumeSupportDomain: NeuropublishVolumeGridDomainV1

trait SurfaceAtlasRealization extends AtlasRealization:
  val leftVertexCount: Int
  val rightVertexCount: Int
  val leftSupportDomain: NeuropublishSurfaceVerticesDomainV1
  val rightSupportDomain: NeuropublishSurfaceVerticesDomainV1

enum AtlasRealizationError:
  case InvalidAtlas(error: AtlasError)
  case GridDomain(error: GridDomainError)
  case InvalidDomain(error: DomainError)
  case DomainRestore(error: DomainRestoreError)
  case PartialMap(error: PartialMapError)
  case TotalMap(error: TotalMapError)
  case CertifiedMap(error: CertifiedMapError)
  case Field(error: FieldConstructionError)
  case Selection(error: SelectionError)
  case Parcellation(error: VolumeParcellationError)
  case Publication(error: AtlasPublicationError)

  def message: String =
    this match
      case InvalidAtlas(error) => error.message
      case GridDomain(error) => error.message
      case InvalidDomain(error) => error.message
      case DomainRestore(error) => error.message
      case PartialMap(error) => error.message
      case TotalMap(error) => error.message
      case CertifiedMap(error) => error.message
      case Field(error) => error.message
      case Selection(error) => error.message
      case Parcellation(error) => error.message
      case Publication(error) => error.message

object AtlasRealization:
  /** Register the image's exact grid owner and build one direct volume
    * realization from a categorical image4s-backed label volume.
    */
  def volumeFromLabelsIn(
      registry: DomainRegistry,
      atlasRef: AtlasRef,
      regions: RegionIndex,
      labels: SomeLabelVolume[Int],
      atlasProvenance: AtlasProvenance
  ): Either[AtlasRealizationError, VolumeAtlasRealization] =
    volumeFromImageIn(
      registry,
      atlasRef,
      regions,
      labels,
      atlasProvenance
    )

  private[atlas] def volumeFromImageIn(
      registry: DomainRegistry,
      atlasRef: AtlasRef,
      regions: RegionIndex,
      labels: AnyNeuroVolume[Int],
      atlasProvenance: AtlasProvenance
  ): Either[AtlasRealizationError, VolumeAtlasRealization] =
    GridDomain
      .register(
        labels.grid,
        s"${atlasRef.name} voxels",
        registry
      )
      .left
      .map(AtlasRealizationError.GridDomain.apply)
      .flatMap: resolution =>
        volumeIn(
          resolution.registry,
          atlasRef,
          regions,
          resolution.value,
          labels,
          atlasProvenance
        )

  def volumeFromLabels(
      atlasRef: AtlasRef,
      regions: RegionIndex,
      labels: SomeLabelVolume[Int],
      atlasProvenance: AtlasProvenance
  ): VolumeAtlasRealization =
    volumeFromLabelsIn(
      DomainRegistry.empty,
      atlasRef,
      regions,
      labels,
      atlasProvenance
    ).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Build against a caller-owned exact grid domain. The image must share the
    * same live grid owner; serialized geometry equality is not enough.
    */
  def volumeIn[F0 <: Frame[D3], S](
      registry: DomainRegistry,
      atlasRef: AtlasRef,
      regions: RegionIndex,
      gridDomain: GridDomain[F0, D3, S],
      labels: AnyNeuroVolume[Int],
      atlasProvenance: AtlasProvenance
  ): Either[AtlasRealizationError, VolumeAtlasRealization] =
    for
      _ <- validateProvenanceCoherence(
        atlasRef,
        regions,
        atlasProvenance,
        AtlasRepresentation.Volume
      )
      labelField <- gridDomain
        .spatialField(labels)
        .left
        .map(AtlasRealizationError.GridDomain.apply)
      parcelResolution <- AtlasParcelDomain
        .restore(registry, atlasRef, atlasProvenance, regions)
        .left
        .map(AtlasRealizationError.Publication.apply)
      assignments <- volumeAssignments(
        gridDomain.space,
        labelField,
        regions.ids.zipWithIndex.toMap
      )
      _ <- validateVolumeCoverage(assignments, regions)
      assignmentValue <- PartialSurjection
        .fromOptionalTargetOrdinals(
          gridDomain.space,
          parcelResolution.space,
          assignments
        )
        .left
        .map(partialAssignmentError)
      metadataField <- VectorField
        .fromValues(parcelResolution.space, regions.regions)
        .left
        .map(AtlasRealizationError.Field.apply)
      parcellationValue <- VolumeParcellation
        .create(
          gridDomain,
          assignmentValue,
          metadataField,
          labels.metadata
        )
        .left
        .map(AtlasRealizationError.Parcellation.apply)
      displayOrderValue <- Selection
        .fromOrdinals(parcelResolution.space, regions.regions.indices)
        .left
        .map(AtlasRealizationError.Selection.apply)
      networkResult <- networkAssignment(
        parcelResolution.registry,
        parcelResolution.space,
        regions
      )
    yield
      type Parcel = parcelResolution.P
      val volumeSupport =
        AtlasPublicationProjection.volumeDomain(
          atlasRef.coordSpace.value,
          gridDomain.record
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
      new VolumeAtlasRealization:
        type F = F0
        type X = S
        type P = Parcel
        val registry: DomainRegistry = networkResult._1
        val ref: AtlasRef = atlasRef
        val provenance: AtlasProvenance = atlasProvenance
        val domain: GridDomain[F0, D3, S] = gridDomain
        val parcellation: VolumeParcellation[
          F0,
          S,
          Parcel,
          AtlasRegionMetadata
        ] = parcellationValue
        val parcelAssignment: PartialSurjection[S, Parcel] =
          parcellationValue.assignment
        val parcelKeys: Field[Parcel, String] =
          parcelResolution.keys
        val metadata: Field[Parcel, AtlasRegionMetadata] =
          parcellationValue.parcelMetadata
        val displayOrder: Selection[Parcel] = displayOrderValue
        val networkAssignment: Option[AtlasNetworkAssignment[Parcel]] =
          networkResult._2
        val parcelDomainRecord: NeuropublishFiniteIndexedDomainV1 =
          parcelResolution.publication
        val volumeSupportDomain: NeuropublishVolumeGridDomainV1 =
          volumeSupport
        val neuropublishSupportDomains: Vector[NeuropublishSpatialDomainV1] =
          Vector(volumeSupport)
        val neuropublishAssignments: Vector[NeuropublishHardAssignmentV1] =
          Vector(publicationAssignment)

  private def validateVolumeCoverage(
      assignments: Vector[Option[Int]],
      regions: RegionIndex
  ): Either[AtlasRealizationError, Unit] =
    val present = assignments.iterator.flatten.toSet
    val missing =
      regions.ids.zipWithIndex.collect:
        case (id, ordinal) if !present.contains(ordinal) => id
    if missing.isEmpty then Right(())
    else
      Left(
        AtlasRealizationError.InvalidAtlas(
          AtlasError.MissingPayloadRegionIds(missing)
        )
      )

  def surfaceIn(
      registry: DomainRegistry,
      atlasRef: AtlasRef,
      regions: RegionIndex,
      payload: SurfaceAtlasPayload,
      atlasProvenance: AtlasProvenance
  ): Either[AtlasRealizationError, SurfaceAtlasRealization] =
    for
      _ <- validateProvenanceCoherence(
        atlasRef,
        regions,
        atlasProvenance,
        AtlasRepresentation.Surface
      )
      realization <- surfaceValidatedIn(
        registry,
        atlasRef,
        regions,
        payload,
        atlasProvenance
      )
    yield realization

  def surface(
      atlasRef: AtlasRef,
      regions: RegionIndex,
      payload: SurfaceAtlasPayload,
      atlasProvenance: AtlasProvenance
  ): SurfaceAtlasRealization =
    surfaceIn(
      DomainRegistry.empty,
      atlasRef,
      regions,
      payload,
      atlasProvenance
    ).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def surfaceValidatedIn(
      registry: DomainRegistry,
      atlasRef: AtlasRef,
      regions: RegionIndex,
      payload: SurfaceAtlasPayload,
      atlasProvenance: AtlasProvenance
  ): Either[AtlasRealizationError, SurfaceAtlasRealization] =
    val leftCount = payload.left.geometry.vertexCount
    val rightCount = payload.right.geometry.vertexCount
    val leftSupport =
      AtlasPublicationProjection.surfaceDomain(
        "left-surface-support",
        atlasRef.coordSpace.value,
        SurfaceHemisphere.Left,
        payload.left
      )
    val rightSupport =
      AtlasPublicationProjection.surfaceDomain(
        "right-surface-support",
        atlasRef.coordSpace.value,
        SurfaceHemisphere.Right,
        payload.right
      )
    for
      ambientRecord <- AtlasPublicationProjection
        .bilateralRecord(
          atlasRef.coordSpace.value,
          leftSupport,
          rightSupport
        )
        .left
        .map(AtlasRealizationError.Publication.apply)
      ambientResolution <- registry
        .restore(ambientRecord)
        .left
        .map(AtlasRealizationError.DomainRestore.apply)
      parcelResolution <- AtlasParcelDomain
        .restore(
          ambientResolution.registry,
          atlasRef,
          atlasProvenance,
          regions
        )
        .left
        .map(AtlasRealizationError.Publication.apply)
      assignments <- surfaceAssignments(
        payload,
        ambientResolution.space.size,
        leftCount,
        regions.ids.zipWithIndex.toMap
      )
      _ <- validateVolumeCoverage(assignments, regions)
      assignmentValue <- PartialSurjection
        .fromOptionalTargetOrdinals(
          ambientResolution.space,
          parcelResolution.space,
          assignments
        )
        .left
        .map(partialAssignmentError)
      metadataField <- VectorField
        .fromValues(parcelResolution.space, regions.regions)
        .left
        .map(AtlasRealizationError.Field.apply)
      displayOrderValue <- Selection
        .fromOrdinals(parcelResolution.space, regions.regions.indices)
        .left
        .map(AtlasRealizationError.Selection.apply)
      networkResult <- networkAssignment(
        parcelResolution.registry,
        parcelResolution.space,
        regions
      )
    yield
      type Vertex = ambientResolution.S
      type Parcel = parcelResolution.P
      val allTargets = assignments.map(_.getOrElse(-1))
      val leftTargets = allTargets.take(leftCount)
      val rightTargets = allTargets.drop(leftCount)
      val assignmentProvenance =
        AtlasPublicationProjection.assignmentProvenance(
          regions,
          parcelResolution.publication
        )
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
          provenance = assignmentProvenance
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
          provenance = assignmentProvenance
        )
      new SurfaceAtlasRealization:
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
        val parcelKeys: Field[Parcel, String] = parcelResolution.keys
        val metadata: Field[Parcel, AtlasRegionMetadata] = metadataField
        val displayOrder: Selection[Parcel] = displayOrderValue
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
  ): Either[AtlasRealizationError, Unit] =
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
        Left(AtlasRealizationError.Publication(error(value)))
      case None => Right(())

  private def volumeAssignments[S](
      space: FiniteDomain[S],
      labels: Field[S, Int],
      parcelOrdinalById: Map[RegionId, Int]
  ): Either[AtlasRealizationError, Vector[Option[Int]]] =
    val assignments = Array.fill[Option[Int]](space.size)(None)
    var failure = Option.empty[AtlasRealizationError]
    space.foreachIndex: index =>
      if failure.isEmpty then
        val label = labels(index)
        if label < 0 then
          failure = Some(
            AtlasRealizationError.InvalidAtlas(
              AtlasError.InvalidRegionMetadata(
                s"atlas volume contains negative region id $label"
              )
            )
          )
        else if label != 0 then
          parcelOrdinalById.get(RegionId(label)) match
            case Some(parcelOrdinal) =>
              assignments(index.ordinal) = Some(parcelOrdinal)
            case None =>
              failure = Some(
                AtlasRealizationError.InvalidAtlas(
                  AtlasError.MissingRegionId(RegionId(label))
                )
              )
    failure.toLeft(assignments.toVector)

  private def surfaceAssignments(
      payload: SurfaceAtlasPayload,
      ambientSize: Int,
      leftCount: Int,
      parcelOrdinalById: Map[RegionId, Int]
  ): Either[AtlasRealizationError, Vector[Option[Int]]] =
    val assignments = Array.fill[Option[Int]](ambientSize)(None)
    for
      _ <- assignSurface(
        payload.left,
        0,
        assignments,
        parcelOrdinalById
      )
      _ <- assignSurface(
        payload.right,
        leftCount,
        assignments,
        parcelOrdinalById
      )
    yield assignments.toVector

  private def assignSurface(
      labeled: scalafim.surface.LabeledSurface,
      offset: Int,
      assignments: Array[Option[Int]],
      parcelOrdinalById: Map[RegionId, Int]
  ): Either[AtlasRealizationError, Unit] =
    var row = 0
    var failure = Option.empty[AtlasRealizationError]
    while row < labeled.indices.length && failure.isEmpty do
      val label = labeled.labels(row)
      val ordinal = offset + labeled.indices(row)
      if ordinal < 0 || ordinal >= assignments.length then
        failure = Some(
          AtlasRealizationError.InvalidAtlas(
            AtlasError.InvalidRegionMetadata(
              s"surface vertex ordinal $ordinal is outside the bilateral support"
            )
          )
        )
      else if label < 0 then
        failure = Some(
          AtlasRealizationError.InvalidAtlas(
            AtlasError.InvalidRegionMetadata(
              s"surface atlas contains negative region id $label"
            )
          )
        )
      else if label != 0 then
        parcelOrdinalById.get(RegionId(label)) match
          case Some(parcelOrdinal) =>
            assignments(ordinal) = Some(parcelOrdinal)
          case None =>
            failure = Some(
              AtlasRealizationError.InvalidAtlas(
                AtlasError.MissingRegionId(RegionId(label))
              )
            )
      row += 1
    failure.toLeft(())

  private def networkAssignment[P](
      registry: DomainRegistry,
      parcels: FiniteSpace[P],
      regions: RegionIndex
  ): Either[
    AtlasRealizationError,
    (DomainRegistry, Option[AtlasNetworkAssignment[P]])
  ] =
    val assigned = regions.regions.map(_.network)
    if assigned.exists(_.isEmpty) then Right((registry, None))
    else
      val ids = assigned.flatten.distinct
      val domainId =
        s"${parcels.id.value}:networks:${ids.map(_.value).mkString(",")}"
      for
        record <- DomainRecord
          .parse(
            id = domainId,
            name = s"${parcels.name.value} networks",
            size = ids.length
          )
          .left
          .map(AtlasRealizationError.InvalidDomain.apply)
        resolution <- registry
          .restore(record)
          .left
          .map(AtlasRealizationError.DomainRestore.apply)
        surjection <- Surjection
          .fromTargetOrdinals(
            parcels,
            resolution.space,
            assigned.flatten.map(ids.zipWithIndex.toMap)
          )
          .left
          .map(totalAssignmentError)
        idField <- VectorField
          .fromValues(resolution.space, ids)
          .left
          .map(AtlasRealizationError.Field.apply)
      yield
        type Network = resolution.S
        val value =
          new AtlasNetworkAssignment[P]:
            type N = Network
            val networkIds: VectorField[Network, NetworkId] = idField
            val parcelToNetwork: Surjection[P, Network] = surjection
        (resolution.registry, Some(value))

  private def partialAssignmentError(
      error: PartialMapError | CertifiedMapError
  ): AtlasRealizationError =
    error match
      case value: PartialMapError => AtlasRealizationError.PartialMap(value)
      case value: CertifiedMapError => AtlasRealizationError.CertifiedMap(value)

  private def totalAssignmentError(
      error: TotalMapError | CertifiedMapError
  ): AtlasRealizationError =
    error match
      case value: TotalMapError => AtlasRealizationError.TotalMap(value)
      case value: CertifiedMapError => AtlasRealizationError.CertifiedMap(value)

  private def coverage(
      targetOrdinals: Vector[Int],
      parcelKeys: Vector[String]
  ): NeuropublishTargetCoverageV1 =
    val reached = Array.fill(parcelKeys.length)(false)
    targetOrdinals.foreach: ordinal =>
      if ordinal >= 0 then reached(ordinal) = true
    val empty =
      parcelKeys.indices
        .filter(ordinal => !reached(ordinal))
        .map(parcelKeys)
        .toVector
    if empty.isEmpty then NeuropublishTargetCoverageV1.Complete
    else NeuropublishTargetCoverageV1.AllowEmpty(empty)
