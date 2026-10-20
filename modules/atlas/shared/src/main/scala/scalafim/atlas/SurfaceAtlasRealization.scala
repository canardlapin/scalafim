package scalafim.atlas

import neuroatlas4s.*

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
import neuroim4s.image.SomeNeuroVolume
import neuroim4s.image.SomeLabelVolume
import neuroim4s.image.VolumeParcellation
import neuroim4s.image.VolumeParcellationError

trait SurfaceAtlasRealization extends PublishedAtlasRealization:
  val leftVertexCount: Int
  val rightVertexCount: Int
  val leftSupportDomain: NeuropublishSurfaceVerticesDomainV1
  val rightSupportDomain: NeuropublishSurfaceVerticesDomainV1

object SurfaceAtlasRealizations:
  import AtlasRealizations.{validateProvenanceCoherence, validateVolumeCoverage, networkAssignment, partialAssignmentError}

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
    ).fold(error => throw new IllegalArgumentException(error.message), value => value)

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
        "left",
        payload.left.geometry.mesh.vertexCount,
        payload.left.geometry.mesh.faceCount,
        Vector.tabulate(payload.left.geometry.mesh.faceIndices.length)(payload.left.geometry.mesh.faceIndices.apply)
      )
    val rightSupport =
      AtlasPublicationProjection.surfaceDomain(
        "right-surface-support",
        atlasRef.coordSpace.value,
        "right",
        payload.right.geometry.mesh.vertexCount,
        payload.right.geometry.mesh.faceCount,
        Vector.tabulate(payload.right.geometry.mesh.faceIndices.length)(payload.right.geometry.mesh.faceIndices.apply)
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
