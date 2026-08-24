package scalafim.atlas

import image4s.geometry.{CoordinateConvention, LengthUnit}
import image4s.locus.GridDomainRecord
import locus4s.{
  DomainError,
  DomainRecord,
  DomainRegistry,
  DomainRestoreError,
  FiniteSpace
}
import locus4s.data.Field
import scalafim.locus.IndexedField
import scalafim.surface.{
  Hemisphere as SurfaceHemisphere,
  LabeledSurface
}

/** The exact descriptor-specific bytes which Neuropublish hashes for one
  * domain. This is deliberately a neutral value: it contains no live Scala
  * owner, case-class codec, JSON canonicalization, or runtime hash code.
  */
final case class NeuropublishDomainIdentityV1(
    descriptorId: String,
    descriptorVersion: String,
    size: Int,
    structuralFingerprint: String,
    identityPreimage: Vector[Byte]
) derives CanEqual

final case class NeuropublishAtlasReleaseV1(
    label: String,
    year: Option[Int],
    version: Option[String],
    commit: Option[String],
    date: Option[String]
) derives CanEqual

final case class NeuropublishAtlasIdentityV1(
    family: String,
    model: String,
    variant: Option[String],
    parcelVariant: Option[String],
    release: Option[NeuropublishAtlasReleaseV1]
) derives CanEqual

enum NeuropublishDeclaredSupportV1 derives CanEqual:
  case Volume(
      templateSpaceId: String,
      coordinateSpaceId: String,
      voxelSizeMillimeters: Option[Vector[Double]],
      dimensions: Option[Vector[Int]]
  )
  case Surface(
      surfaceSpaceId: String,
      density: String,
      verticesPerHemisphere: Option[Int],
      hemisphereCoverage: String
  )
  case Derived(templateSpaceId: String, coordinateSpaceId: String)

final case class NeuropublishAtlasLabelSchemaV1(
    encoding: String,
    regionIds: Vector[Int],
    backgroundSourceLabel: Option[Int],
    labelTableArtifactId: Option[String],
    attributes: Vector[(String, String)]
) derives CanEqual

final case class NeuropublishDigestV1(
    algorithm: String,
    value: String
) derives CanEqual

enum NeuropublishLicenseV1 derives CanEqual:
  case Known(id: String)
  case Restricted(label: String, termsUri: Option[String])
  case Unspecified(reason: String)
  case Missing

final case class NeuropublishSourceArtifactV1(
    id: String,
    role: String,
    sourceName: String,
    sourceRef: String,
    sourceUri: Option[String],
    citationDoi: Option[String],
    license: NeuropublishLicenseV1,
    digest: Option[NeuropublishDigestV1],
    notes: Option[String]
) derives CanEqual

enum NeuropublishAtlasDerivationV1 derives CanEqual:
  case DeclaredDescriptor(details: String)
  case Loaded(artifactId: String)
  case ParsedLabels(
      artifactId: String,
      tableName: String,
      columns: Vector[String]
  )
  case ValidatedLabels(regionIds: Vector[Int])
  case FilteredLabels(kept: Vector[Int], dropped: Vector[Int])
  case Resampled(
      fromSpaceId: String,
      toSpaceId: String,
      transformKind: String,
      status: String,
      confidence: String
  )
  case ProjectedVolumeToSurface(
      fromSpaceId: String,
      toSpaceId: String,
      surfaceKind: String,
      samplingMethod: String,
      samplingNotes: Option[String],
      status: String
  )
  case LegacyHistory(
      action: String,
      fromTemplateSpaceId: String,
      toTemplateSpaceId: String,
      fromCoordinateSpaceId: String,
      toCoordinateSpaceId: String,
      status: String,
      confidence: String,
      details: String
  )

final case class NeuropublishCitationV1(
    doi: Option[String],
    text: Option[String]
) derives CanEqual

final case class NeuropublishAtlasProvenanceV1(
    localId: String,
    identity: NeuropublishAtlasIdentityV1,
    declaredSupport: NeuropublishDeclaredSupportV1,
    labels: NeuropublishAtlasLabelSchemaV1,
    sourceArtifacts: Vector[NeuropublishSourceArtifactV1],
    derivation: Vector[NeuropublishAtlasDerivationV1],
    citations: Vector[NeuropublishCitationV1],
    confidence: String
) derives CanEqual

final case class NeuropublishAssignmentProvenanceV1(
    atlasProvenanceId: String,
    converterId: String,
    converterVersion: String,
    sourceBackgroundLabel: Option[Int],
    sourceLabelToParcelKeys: Vector[(Int, String)]
) derives CanEqual

final case class NeuropublishFiniteIndexedDomainV1(
    localId: String,
    identity: NeuropublishDomainIdentityV1,
    elementKeys: Vector[String]
) derives CanEqual

sealed trait NeuropublishSpatialDomainV1 derives CanEqual:
  def localId: String
  def identity: NeuropublishDomainIdentityV1

final case class NeuropublishVolumeGridDomainV1(
    localId: String,
    identity: NeuropublishDomainIdentityV1,
    coordinateSpaceId: String,
    coordinateConvention: String,
    spatialUnit: String,
    ordinalLayout: String,
    shape: Vector[Int],
    affineRowMajor: Vector[Double]
) extends NeuropublishSpatialDomainV1 derives CanEqual

final case class NeuropublishSurfaceVerticesDomainV1(
    localId: String,
    identity: NeuropublishDomainIdentityV1,
    surfaceSpaceId: String,
    hemisphere: String,
    vertexCount: Int,
    faceCount: Int,
    faceIndices: Vector[Int]
) extends NeuropublishSpatialDomainV1 derives CanEqual

enum NeuropublishTargetCoverageV1 derives CanEqual:
  case Complete
  case AllowEmpty(emptyParcelKeys: Vector[String])

final case class NeuropublishHardAssignmentV1(
    localId: String,
    source: NeuropublishDomainIdentityV1,
    target: NeuropublishDomainIdentityV1,
    targetOrdinals: Vector[Int],
    coverage: NeuropublishTargetCoverageV1,
    provenance: NeuropublishAssignmentProvenanceV1,
    mediaType: String = NeuropublishHardAssignmentV1.MediaType
) derives CanEqual:
  def assetBytes: Vector[Byte] =
    AtlasPublicationBinaryV1.int32LittleEndian(targetOrdinals)

  def assetSha256: String =
    AtlasPublicationSha256.hex(assetBytes)

object NeuropublishHardAssignmentV1:
  val MediaType: String =
    "application/vnd.neuropublish.hard-assignment-i32le-v1"

final case class NeuropublishParcelMetadataV1(
    key: String,
    id: Int,
    label: String,
    fullLabel: String,
    hemisphere: Option[String],
    network: Option[String],
    color: Option[(Int, Int, Int)],
    attributes: Vector[(String, String)]
) derives CanEqual

final case class NeuropublishParcelHierarchyV1(
    networkKeys: Vector[String],
    parcelToNetworkOrdinals: Vector[Int]
) derives CanEqual

/** Wire-independent publication projection. A JSON, CBOR, database, or asset
  * codec is downstream policy; the fixed identity and assignment bytes here
  * are the cross-language contract.
  */
final case class NeuropublishAtlasProjectionV1(
    atlasProvenance: NeuropublishAtlasProvenanceV1,
    parcelDomain: NeuropublishFiniteIndexedDomainV1,
    parcelMetadata: Vector[NeuropublishParcelMetadataV1],
    displayOrder: Vector[String],
    hierarchy: Option[NeuropublishParcelHierarchyV1],
    supportDomains: Vector[NeuropublishSpatialDomainV1],
    assignments: Vector[NeuropublishHardAssignmentV1]
) derives CanEqual

enum AtlasPublicationError:
  case InvalidParcelDomain(error: DomainError)
  case ParcelDomainRestore(error: DomainRestoreError)
  case InvalidSupportDomain(error: DomainError)
  case SupportDomainRestore(error: DomainRestoreError)
  case AtlasProvenanceMismatch(detail: String)
  case ParcelDomainMismatch(detail: String)
  case ParcelMetadataMismatch(detail: String)
  case DisplayOrderMismatch(detail: String)
  case HierarchyMismatch(detail: String)
  case SupportDomainMismatch(detail: String)
  case AssignmentMismatch(detail: String)

  def message: String =
    this match
      case InvalidParcelDomain(error) =>
        s"invalid atlas parcel domain: ${error.message}"
      case ParcelDomainRestore(error) =>
        s"atlas parcel domain restoration failed: ${error.message}"
      case InvalidSupportDomain(error) =>
        s"invalid atlas support domain: ${error.message}"
      case SupportDomainRestore(error) =>
        s"atlas support domain restoration failed: ${error.message}"
      case AtlasProvenanceMismatch(detail) =>
        s"atlasProvenance: $detail"
      case ParcelDomainMismatch(detail) =>
        s"parcelDomain: $detail"
      case ParcelMetadataMismatch(detail) =>
        s"parcelMetadata: $detail"
      case DisplayOrderMismatch(detail) =>
        s"displayOrder: $detail"
      case HierarchyMismatch(detail) =>
        s"hierarchy: $detail"
      case SupportDomainMismatch(detail) =>
        s"supportDomains: $detail"
      case AssignmentMismatch(detail) =>
        s"assignments: $detail"

private[atlas] trait AtlasParcelDomainResolution:
  type P
  val registry: DomainRegistry
  val space: FiniteSpace[P]
  val keys: IndexedField[P, String]
  val publication: NeuropublishFiniteIndexedDomainV1

private[atlas] object AtlasParcelDomain:
  private val DescriptorId = "org.neuropublish.domain/finite-indexed"
  private val DescriptorVersion = "1"

  def restore(
      registry: DomainRegistry,
      ref: AtlasRef,
      provenance: AtlasProvenance,
      regions: RegionIndex
  ): Either[AtlasPublicationError, AtlasParcelDomainResolution] =
    val namespace = namespaceFor(ref, provenance)
    val elementKeys =
      regions.ids.map(id => parcelKey(namespace, id))
    val preimage =
      AtlasPublicationBinaryV1.finiteIndexed(
        DescriptorId,
        DescriptorVersion,
        elementKeys
      )
    val identity =
      NeuropublishDomainIdentityV1(
        DescriptorId,
        DescriptorVersion,
        elementKeys.length,
        AtlasPublicationSha256.hex(preimage),
        preimage
      )
    val id =
      s"scalafim:atlas:parcels:v1:${identity.structuralFingerprint}"
    val fingerprint =
      s"neuropublish-finite-indexed-sha256/v1:${identity.structuralFingerprint}"

    DomainRecord
      .parse(
        id = id,
        name = s"${ref.name} parcels",
        size = elementKeys.length,
        fingerprint = Some(fingerprint)
      )
      .left
      .map(AtlasPublicationError.InvalidParcelDomain.apply)
      .flatMap: record =>
        registry
          .restore(record)
          .left
          .map(AtlasPublicationError.ParcelDomainRestore.apply)
          .map: resolution =>
            new AtlasParcelDomainResolution:
              type P = resolution.S
              val registry: DomainRegistry = resolution.registry
              val space: FiniteSpace[P] = resolution.space
              val keys: IndexedField[P, String] =
                IndexedField.tabulate(space)(point => elementKeys(point.ordinal))
              val publication: NeuropublishFiniteIndexedDomainV1 =
                NeuropublishFiniteIndexedDomainV1(
                  localId = "parcel-domain",
                  identity = identity,
                  elementKeys = elementKeys
                )

  private def namespaceFor(
      ref: AtlasRef,
      provenance: AtlasProvenance
  ): String =
    val identity = provenance.identity
    val release =
      identity.release match
        case None => "unspecified"
        case Some(value) =>
          Vector(
            value.label,
            value.year.fold("")(_.toString),
            value.version.getOrElse(""),
            value.commit.getOrElse(""),
            value.date.getOrElse("")
          ).map(component).mkString
    Vector(
      "org.scalafim.atlas/parcel-namespace/v1",
      identity.family,
      identity.model,
      ref.parcelVariant.getOrElse("default"),
      release
    ).map(component).mkString

  private def parcelKey(namespace: String, id: RegionId): String =
    s"org.scalafim.atlas/parcel/v1:${component(namespace)}:${id.value}"

  private def component(value: String): String =
    s"${value.length}:$value"

private[atlas] object AtlasPublicationProjection:
  private val ProvenanceLocalId = "atlas-provenance"
  private val ConverterId = "org.scalafim.atlas/hard-label-realization"
  private val ConverterVersion = "1"

  def provenance(
      ref: AtlasRef,
      value: AtlasProvenance
  ): NeuropublishAtlasProvenanceV1 =
    val identity = value.identity
    NeuropublishAtlasProvenanceV1(
      localId = ProvenanceLocalId,
      identity = NeuropublishAtlasIdentityV1(
        family = identity.family,
        model = identity.model,
        variant = identity.variant,
        parcelVariant = ref.parcelVariant,
        release = identity.release.map: release =>
          NeuropublishAtlasReleaseV1(
            release.label,
            release.year,
            release.version,
            release.commit,
            release.date
          )
      ),
      declaredSupport = declaredSupport(value.support),
      labels = NeuropublishAtlasLabelSchemaV1(
        encoding = labelEncoding(value.labels.encoding),
        regionIds = value.labels.regionIds.map(_.value),
        backgroundSourceLabel = value.labels.background,
        labelTableArtifactId = value.labels.labelTableArtifactId,
        attributes = value.labels.attributes.toVector.sortBy(_._1)
      ),
      sourceArtifacts = value.sourceArtifacts.map(sourceArtifact),
      derivation = value.derivation.map(derivation),
      citations = value.citations.map: citation =>
        NeuropublishCitationV1(citation.doi, citation.text),
      confidence = confidence(value.confidence)
    )

  def assignmentProvenance(
      regions: RegionIndex,
      parcelDomain: NeuropublishFiniteIndexedDomainV1
  ): NeuropublishAssignmentProvenanceV1 =
    NeuropublishAssignmentProvenanceV1(
      atlasProvenanceId = ProvenanceLocalId,
      converterId = ConverterId,
      converterVersion = ConverterVersion,
      sourceBackgroundLabel = Some(0),
      sourceLabelToParcelKeys =
        regions.ids.map(_.value).zip(parcelDomain.elementKeys)
    )

  def volumeDomain(
      coordinateSpaceId: String,
      record: GridDomainRecord
  ): NeuropublishVolumeGridDomainV1 =
    val grid = record.grid.key
    val convention =
      grid.frame.convention match
        case CoordinateConvention.Unspecified => "unspecified"
        case CoordinateConvention.RAS => "RAS"
        case CoordinateConvention.LPS => "LPS"
    val unit =
      grid.frame.unit match
        case LengthUnit.Millimeter => "millimeter"
        case LengthUnit.Meter => "meter"
        case LengthUnit.Micrometer => "micrometer"
    val affine = grid.indexToFrame.rowMajor.map(normalizeZero)
    val preimage =
      AtlasPublicationBinaryV1.volumeGrid(
        coordinateSpaceId,
        convention,
        unit,
        record.layout.id,
        grid.shape,
        affine
      )
    val identity =
      NeuropublishDomainIdentityV1(
        "org.neuropublish.domain/volume-grid",
        "1",
        record.domain.size,
        AtlasPublicationSha256.hex(preimage),
        preimage
      )
    NeuropublishVolumeGridDomainV1(
      localId = "volume-support",
      identity = identity,
      coordinateSpaceId = coordinateSpaceId,
      coordinateConvention = convention,
      spatialUnit = unit,
      ordinalLayout = record.layout.id,
      shape = grid.shape,
      affineRowMajor = affine
    )

  def surfaceDomain(
      localId: String,
      surfaceSpaceId: String,
      hemisphere: SurfaceHemisphere,
      surface: LabeledSurface
  ): NeuropublishSurfaceVerticesDomainV1 =
    val hemisphereId =
      hemisphere match
        case SurfaceHemisphere.Left => "left"
        case SurfaceHemisphere.Right => "right"
        case other => other.code
    val mesh = surface.geometry.mesh
    val faces =
      Vector.tabulate(mesh.faceIndices.length)(mesh.faceIndices.apply)
    val preimage =
      AtlasPublicationBinaryV1.surfaceVertices(
        surfaceSpaceId,
        hemisphereId,
        mesh.vertexCount,
        mesh.faceCount,
        faces
      )
    val identity =
      NeuropublishDomainIdentityV1(
        "org.neuropublish.domain/surface-vertices",
        "1",
        mesh.vertexCount,
        AtlasPublicationSha256.hex(preimage),
        preimage
      )
    NeuropublishSurfaceVerticesDomainV1(
      localId = localId,
      identity = identity,
      surfaceSpaceId = surfaceSpaceId,
      hemisphere = hemisphereId,
      vertexCount = mesh.vertexCount,
      faceCount = mesh.faceCount,
      faceIndices = faces
    )

  def bilateralRecord(
      coordinateSpaceId: String,
      left: NeuropublishSurfaceVerticesDomainV1,
      right: NeuropublishSurfaceVerticesDomainV1
  ): Either[AtlasPublicationError, DomainRecord] =
    val preimage =
      AtlasPublicationBinaryV1.bilateralSurface(
        coordinateSpaceId,
        left.identity.identityPreimage,
        right.identity.identityPreimage
      )
    val id =
      s"scalafim:atlas:bilateral-surface:v1:${left.identity.structuralFingerprint}:${right.identity.structuralFingerprint}"
    DomainRecord
      .parse(
        id = id,
        name = s"$coordinateSpaceId bilateral surface vertices",
        size = left.vertexCount + right.vertexCount,
        fingerprint = Some(
          s"scalafim-bilateral-surface-sha256/v1:${AtlasPublicationSha256.hex(preimage)}"
        )
      )
      .left
      .map(AtlasPublicationError.InvalidSupportDomain.apply)

  def validate(
      expected: NeuropublishAtlasProjectionV1,
      actual: NeuropublishAtlasProjectionV1
  ): Either[AtlasPublicationError, Unit] =
    validateRecord(actual).flatMap: _ =>
      if actual.parcelDomain != expected.parcelDomain then
        Left(
          AtlasPublicationError.ParcelDomainMismatch(
            "exact ordered finite-indexed identity differs"
          )
        )
      else if actual.atlasProvenance != expected.atlasProvenance then
        Left(AtlasPublicationError.AtlasProvenanceMismatch("record differs"))
      else if actual.parcelMetadata != expected.parcelMetadata then
        Left(AtlasPublicationError.ParcelMetadataMismatch("indexed rows differ"))
      else if actual.displayOrder != expected.displayOrder then
        Left(AtlasPublicationError.DisplayOrderMismatch("stable-key order differs"))
      else if actual.hierarchy != expected.hierarchy then
        Left(AtlasPublicationError.HierarchyMismatch("network mapping differs"))
      else if actual.supportDomains != expected.supportDomains then
        Left(AtlasPublicationError.SupportDomainMismatch("exact spatial identity differs"))
      else if actual.assignments != expected.assignments then
        Left(AtlasPublicationError.AssignmentMismatch("endpoint, coverage, or ordinal payload differs"))
      else Right(())

  private def validateRecord(
      projection: NeuropublishAtlasProjectionV1
  ): Either[AtlasPublicationError, Unit] =
    for
      _ <- validateParcelDomain(projection.parcelDomain)
      _ <- validateParcelMetadata(
        projection.parcelDomain,
        projection.parcelMetadata
      )
      _ <- validateAtlasProvenance(
        projection.atlasProvenance,
        projection.parcelMetadata,
        projection.supportDomains
      )
      _ <- validateDisplayOrder(
        projection.parcelDomain,
        projection.displayOrder
      )
      _ <- validateHierarchy(
        projection.parcelDomain,
        projection.parcelMetadata,
        projection.hierarchy
      )
      _ <- validateSupportDomains(projection.supportDomains)
      _ <- validateAssignments(
        projection.parcelDomain,
        projection.parcelMetadata,
        projection.supportDomains,
        projection.assignments,
        projection.atlasProvenance
      )
    yield ()

  private def validateParcelDomain(
      domain: NeuropublishFiniteIndexedDomainV1
  ): Either[AtlasPublicationError, Unit] =
    val keys = domain.elementKeys
    val error = AtlasPublicationError.ParcelDomainMismatch.apply
    if domain.localId.trim.isEmpty then Left(error("local id is empty"))
    else if keys.exists(_.trim.isEmpty) then
      Left(error("element keys must be non-empty"))
    else if keys.distinct.length != keys.length then
      Left(error("element keys must be unique"))
    else
      validateIdentity(
        domain.identity,
        descriptorId = "org.neuropublish.domain/finite-indexed",
        descriptorVersion = "1",
        expectedSize = keys.length,
        expectedPreimage = AtlasPublicationBinaryV1.finiteIndexed(
          "org.neuropublish.domain/finite-indexed",
          "1",
          keys
        ),
        error
      )

  private def validateParcelMetadata(
      domain: NeuropublishFiniteIndexedDomainV1,
      metadata: Vector[NeuropublishParcelMetadataV1]
  ): Either[AtlasPublicationError, Unit] =
    val error = AtlasPublicationError.ParcelMetadataMismatch.apply
    if metadata.map(_.key) != domain.elementKeys then
      Left(error("keys must match parcel-domain order exactly"))
    else if metadata.exists(row => row.id <= 0) then
      Left(error("region ids must be positive"))
    else if metadata.map(_.id).distinct.length != metadata.length then
      Left(error("region ids must be unique"))
    else if metadata.exists(row => row.label.trim.isEmpty || row.fullLabel.trim.isEmpty) then
      Left(error("labels must be non-empty"))
    else if metadata.exists: row =>
        row.color.exists: color =>
          val values = Vector(color._1, color._2, color._3)
          values.exists(value => value < 0 || value > 255)
    then Left(error("color components must be in [0,255]"))
    else if metadata.exists: row =>
        val keys = row.attributes.map(_._1)
        keys.exists(_.trim.isEmpty) || keys.distinct.length != keys.length
    then Left(error("attribute keys must be non-empty and unique"))
    else Right(())

  private def validateAtlasProvenance(
      provenance: NeuropublishAtlasProvenanceV1,
      metadata: Vector[NeuropublishParcelMetadataV1],
      supportDomains: Vector[NeuropublishSpatialDomainV1]
  ): Either[AtlasPublicationError, Unit] =
    val error = AtlasPublicationError.AtlasProvenanceMismatch.apply
    val identity = provenance.identity
    val labels = provenance.labels
    val artifacts = provenance.sourceArtifacts
    val allowedConfidence = Set("exact", "high", "approximate", "uncertain")
    if provenance.localId.trim.isEmpty then Left(error("local id is empty"))
    else if identity.family.trim.isEmpty || identity.model.trim.isEmpty then
      Left(error("atlas family and model must be non-empty"))
    else if identity.variant.exists(_.trim.isEmpty) ||
        identity.parcelVariant.exists(_.trim.isEmpty)
    then Left(error("atlas variants must be non-empty when present"))
    else if identity.release.exists(_.label.trim.isEmpty) then
      Left(error("release label must be non-empty"))
    else if !allowedConfidence.contains(provenance.confidence) then
      Left(error("confidence is unsupported"))
    else if labels.regionIds.exists(_ <= 0) ||
        labels.regionIds.distinct.length != labels.regionIds.length
    then Left(error("label-schema region ids must be positive and unique"))
    else if labels.regionIds.toSet != metadata.map(_.id).toSet then
      Left(error("label-schema region ids differ from parcel metadata"))
    else if labels.backgroundSourceLabel.exists(_ < 0) then
      Left(error("background source label must be non-negative"))
    else if labels.attributes.exists(_._1.trim.isEmpty) ||
        labels.attributes.map(_._1).distinct.length != labels.attributes.length
    then Left(error("label-schema attribute keys must be non-empty and unique"))
    else if artifacts.isEmpty then
      Left(error("at least one source artifact is required"))
    else if artifacts.map(_.id).exists(_.trim.isEmpty) ||
        artifacts.map(_.id).distinct.length != artifacts.length
    then Left(error("source artifact ids must be non-empty and unique"))
    else if artifacts.exists(value =>
        value.role.trim.isEmpty ||
          value.sourceName.trim.isEmpty ||
          value.sourceRef.trim.isEmpty
      )
    then Left(error("source artifact role, name, and reference must be non-empty"))
    else if artifacts.exists(_.digest.exists(value =>
        value.algorithm.trim.isEmpty || value.value.trim.isEmpty
      ))
    then Left(error("source artifact digests must name an algorithm and value"))
    else if labels.labelTableArtifactId.exists(id => !artifacts.exists(_.id == id)) then
      Left(error("label-table artifact reference is unresolved"))
    else if provenance.derivation.isEmpty then
      Left(error("derivation history is empty"))
    else if provenance.citations.exists(value =>
        !value.doi.exists(_.trim.nonEmpty) && !value.text.exists(_.trim.nonEmpty)
      )
    then Left(error("citations require a DOI or text"))
    else
      validateDeclaredSupport(
        provenance.declaredSupport,
        labels.encoding,
        supportDomains,
        error
      )

  private def validateDeclaredSupport(
      declared: NeuropublishDeclaredSupportV1,
      labelEncoding: String,
      supportDomains: Vector[NeuropublishSpatialDomainV1],
      error: String => AtlasPublicationError
  ): Either[AtlasPublicationError, Unit] =
    declared match
      case NeuropublishDeclaredSupportV1.Volume(
            templateSpaceId,
            coordinateSpaceId,
            voxelSize,
            dimensions
          ) =>
        val volumeSupports =
          supportDomains.collect { case value: NeuropublishVolumeGridDomainV1 => value }
        if templateSpaceId.trim.isEmpty || coordinateSpaceId.trim.isEmpty then
          Left(error("declared volume spaces must be non-empty"))
        else if voxelSize.exists(values =>
            values.length != 3 ||
              values.exists(value => value <= 0.0 || value.isNaN || value.isInfinite)
          )
        then Left(error("declared voxel size must contain three positive finite values"))
        else if dimensions.exists(values =>
            values.length != 3 || values.exists(_ <= 0)
          )
        then Left(error("declared volume dimensions must contain three positive values"))
        else if volumeSupports.length != supportDomains.length || volumeSupports.isEmpty then
          Left(error("declared volume support requires volume-grid records"))
        else if volumeSupports.exists(_.coordinateSpaceId != coordinateSpaceId) then
          Left(error("declared coordinate space differs from exact volume support"))
        else if dimensions.exists(_ != volumeSupports.head.shape) then
          Left(error("declared dimensions differ from exact volume support"))
        else if labelEncoding != "volume-integer-labels" then
          Left(error("label encoding disagrees with volume support"))
        else Right(())
      case NeuropublishDeclaredSupportV1.Surface(
            surfaceSpaceId,
            density,
            verticesPerHemisphere,
            hemisphereCoverage
          ) =>
        val surfaceSupports =
          supportDomains.collect { case value: NeuropublishSurfaceVerticesDomainV1 => value }
        val allowedCoverage = Set("left-only", "right-only", "bilateral", "unknown")
        if surfaceSpaceId.trim.isEmpty || density.trim.isEmpty then
          Left(error("declared surface space and density must be non-empty"))
        else if verticesPerHemisphere.exists(_ <= 0) then
          Left(error("declared vertices per hemisphere must be positive"))
        else if !allowedCoverage.contains(hemisphereCoverage) then
          Left(error("declared hemisphere coverage is unsupported"))
        else if surfaceSupports.length != supportDomains.length || surfaceSupports.isEmpty then
          Left(error("declared surface support requires surface-vertices records"))
        else if surfaceSupports.exists(_.surfaceSpaceId != surfaceSpaceId) then
          Left(error("declared surface space differs from exact surface support"))
        else if verticesPerHemisphere.exists(value =>
            surfaceSupports.exists(_.vertexCount != value)
          )
        then Left(error("declared surface density differs from exact support"))
        else if labelEncoding != "surface-integer-labels" then
          Left(error("label encoding disagrees with surface support"))
        else Right(())
      case NeuropublishDeclaredSupportV1.Derived(templateSpaceId, coordinateSpaceId) =>
        if templateSpaceId.trim.isEmpty || coordinateSpaceId.trim.isEmpty then
          Left(error("declared derived spaces must be non-empty"))
        else if labelEncoding != "derived-labels" then
          Left(error("label encoding disagrees with derived support"))
        else Right(())

  private def validateDisplayOrder(
      domain: NeuropublishFiniteIndexedDomainV1,
      displayOrder: Vector[String]
  ): Either[AtlasPublicationError, Unit] =
    val error = AtlasPublicationError.DisplayOrderMismatch.apply
    if displayOrder.length != domain.elementKeys.length then
      Left(error("must contain every parcel key exactly once"))
    else if displayOrder.distinct.length != displayOrder.length then
      Left(error("contains duplicate parcel keys"))
    else if displayOrder.toSet != domain.elementKeys.toSet then
      Left(error("contains foreign or missing parcel keys"))
    else Right(())

  private def validateHierarchy(
      domain: NeuropublishFiniteIndexedDomainV1,
      metadata: Vector[NeuropublishParcelMetadataV1],
      hierarchy: Option[NeuropublishParcelHierarchyV1]
  ): Either[AtlasPublicationError, Unit] =
    hierarchy match
      case None => Right(())
      case Some(value) =>
        val error = AtlasPublicationError.HierarchyMismatch.apply
        val networkKeys = value.networkKeys
        val ordinals = value.parcelToNetworkOrdinals
        if networkKeys.isEmpty then Left(error("network keys are empty"))
        else if networkKeys.exists(_.trim.isEmpty) then
          Left(error("network keys must be non-empty"))
        else if networkKeys.distinct.length != networkKeys.length then
          Left(error("network keys must be unique"))
        else if ordinals.length != domain.elementKeys.length then
          Left(error("parcel mapping length differs from parcel-domain size"))
        else if ordinals.exists(ordinal => ordinal < 0 || ordinal >= networkKeys.length) then
          Left(error("parcel mapping contains an out-of-range network ordinal"))
        else if ordinals.distinct.length != networkKeys.length then
          Left(error("network mapping is not surjective"))
        else if metadata.zip(ordinals).exists: pair =>
            pair._1.network.exists(_ != networkKeys(pair._2))
        then Left(error("metadata network disagrees with hierarchy mapping"))
        else Right(())

  private def validateSupportDomains(
      domains: Vector[NeuropublishSpatialDomainV1]
  ): Either[AtlasPublicationError, Unit] =
    val error = AtlasPublicationError.SupportDomainMismatch.apply
    if domains.isEmpty then Left(error("at least one exact support domain is required"))
    else if domains.map(_.localId).exists(_.trim.isEmpty) then
      Left(error("local ids must be non-empty"))
    else if domains.map(_.localId).distinct.length != domains.length then
      Left(error("local ids must be unique"))
    else if domains.map(_.identity).distinct.length != domains.length then
      Left(error("exact support identities must be unique"))
    else
      domains.foldLeft[Either[AtlasPublicationError, Unit]](Right(())):
        (result, domain) =>
          result.flatMap: _ =>
            domain match
              case volume: NeuropublishVolumeGridDomainV1 =>
                validateVolumeDomain(volume)
              case surface: NeuropublishSurfaceVerticesDomainV1 =>
                validateSurfaceDomain(surface)

  private def validateVolumeDomain(
      domain: NeuropublishVolumeGridDomainV1
  ): Either[AtlasPublicationError, Unit] =
    val error = AtlasPublicationError.SupportDomainMismatch.apply
    val allowedConventions = Set("unspecified", "RAS", "LPS")
    val allowedUnits = Set("millimeter", "meter", "micrometer")
    if domain.coordinateSpaceId.trim.isEmpty then
      Left(error("volume coordinate-space id is empty"))
    else if !allowedConventions.contains(domain.coordinateConvention) then
      Left(error("volume coordinate convention is unsupported"))
    else if !allowedUnits.contains(domain.spatialUnit) then
      Left(error("volume spatial unit is unsupported"))
    else if domain.ordinalLayout != "row-major-last-axis-fastest/v1" then
      Left(error("volume ordinal layout is unsupported"))
    else if domain.shape.length != 3 || domain.shape.exists(_ <= 0) then
      Left(error("volume shape must contain three positive dimensions"))
    else if domain.affineRowMajor.length != 16 then
      Left(error("volume affine must contain 16 row-major values"))
    else if domain.affineRowMajor.exists(value => value.isNaN || value.isInfinite) then
      Left(error("volume affine values must be finite"))
    else
      val maximum = Int.MaxValue.toLong
      val size =
        domain.shape.foldLeft(1L): (product, dimension) =>
          if product > maximum / dimension.toLong then maximum + 1L
          else product * dimension.toLong
      if size > maximum then Left(error("volume domain size exceeds Int capacity"))
      else
        validateIdentity(
          domain.identity,
          descriptorId = "org.neuropublish.domain/volume-grid",
          descriptorVersion = "1",
          expectedSize = size.toInt,
          expectedPreimage = AtlasPublicationBinaryV1.volumeGrid(
            domain.coordinateSpaceId,
            domain.coordinateConvention,
            domain.spatialUnit,
            domain.ordinalLayout,
            domain.shape,
            domain.affineRowMajor
          ),
          error
        )

  private def validateSurfaceDomain(
      domain: NeuropublishSurfaceVerticesDomainV1
  ): Either[AtlasPublicationError, Unit] =
    val error = AtlasPublicationError.SupportDomainMismatch.apply
    val expectedIndexCount = domain.faceCount.toLong * 3L
    if domain.surfaceSpaceId.trim.isEmpty then
      Left(error("surface-space id is empty"))
    else if domain.hemisphere.trim.isEmpty then
      Left(error("surface hemisphere is empty"))
    else if domain.vertexCount <= 0 then
      Left(error("surface vertex count must be positive"))
    else if domain.faceCount < 0 then
      Left(error("surface face count must be non-negative"))
    else if expectedIndexCount != domain.faceIndices.length.toLong then
      Left(error("surface face-index count differs from three times face count"))
    else if domain.faceIndices.exists(index => index < 0 || index >= domain.vertexCount) then
      Left(error("surface face indices contain an out-of-range vertex ordinal"))
    else
      validateIdentity(
        domain.identity,
        descriptorId = "org.neuropublish.domain/surface-vertices",
        descriptorVersion = "1",
        expectedSize = domain.vertexCount,
        expectedPreimage = AtlasPublicationBinaryV1.surfaceVertices(
          domain.surfaceSpaceId,
          domain.hemisphere,
          domain.vertexCount,
          domain.faceCount,
          domain.faceIndices
        ),
        error
      )

  private def validateAssignments(
      parcelDomain: NeuropublishFiniteIndexedDomainV1,
      parcelMetadata: Vector[NeuropublishParcelMetadataV1],
      supportDomains: Vector[NeuropublishSpatialDomainV1],
      assignments: Vector[NeuropublishHardAssignmentV1],
      atlasProvenance: NeuropublishAtlasProvenanceV1
  ): Either[AtlasPublicationError, Unit] =
    val error = AtlasPublicationError.AssignmentMismatch.apply
    val supportIdentities = supportDomains.map(_.identity)
    if assignments.length != supportDomains.length then
      Left(error("each support domain must have exactly one assignment"))
    else if assignments.map(_.localId).exists(_.trim.isEmpty) then
      Left(error("local ids must be non-empty"))
    else if assignments.map(_.localId).distinct.length != assignments.length then
      Left(error("local ids must be unique"))
    else if assignments.map(_.source).distinct.length != assignments.length then
      Left(error("source identities must be unique"))
    else if assignments.exists(assignment => !supportIdentities.contains(assignment.source)) then
      Left(error("source identity does not match an exact support domain"))
    else
      val individuallyValid =
        assignments.foldLeft[Either[AtlasPublicationError, Unit]](Right(())):
          (result, assignment) =>
            result.flatMap(_ =>
              validateAssignment(
                parcelDomain,
                parcelMetadata,
                atlasProvenance,
                assignment
              )
            )
      individuallyValid.flatMap: _ =>
        val covered =
          assignments.iterator
            .flatMap(_.targetOrdinals.iterator)
            .filter(_ >= 0)
            .toSet
        if covered.size != parcelDomain.elementKeys.length then
          Left(error("combined assignments do not cover every parcel"))
        else Right(())

  private def validateAssignment(
      parcelDomain: NeuropublishFiniteIndexedDomainV1,
      parcelMetadata: Vector[NeuropublishParcelMetadataV1],
      atlasProvenance: NeuropublishAtlasProvenanceV1,
      assignment: NeuropublishHardAssignmentV1
  ): Either[AtlasPublicationError, Unit] =
    val error = AtlasPublicationError.AssignmentMismatch.apply
    val targetSize = parcelDomain.elementKeys.length
    val provenance = assignment.provenance
    val expectedSourceTable =
      parcelMetadata.map(row => row.id -> row.key)
    if assignment.target != parcelDomain.identity then
      Left(error("target identity differs from the exact parcel domain"))
    else if assignment.mediaType != NeuropublishHardAssignmentV1.MediaType then
      Left(error("media type is unsupported"))
    else if provenance.atlasProvenanceId != atlasProvenance.localId then
      Left(error("atlas provenance reference is unresolved"))
    else if provenance.converterId != "org.scalafim.atlas/hard-label-realization" ||
        provenance.converterVersion != "1"
    then Left(error("assignment converter is unsupported"))
    else if provenance.sourceBackgroundLabel != atlasProvenance.labels.backgroundSourceLabel then
      Left(error("source background label differs from atlas provenance"))
    else if provenance.sourceLabelToParcelKeys != expectedSourceTable then
      Left(error("source-label-to-parcel-key table differs from parcel metadata"))
    else if assignment.targetOrdinals.length != assignment.source.size then
      Left(error("payload length differs from source-domain size"))
    else if assignment.targetOrdinals.exists(value => value < -1 || value >= targetSize) then
      Left(error("payload contains an out-of-range target ordinal"))
    else
      val covered = assignment.targetOrdinals.iterator.filter(_ >= 0).toSet
      val emptyKeys =
        parcelDomain.elementKeys.zipWithIndex.collect:
          case (key, ordinal) if !covered.contains(ordinal) => key
      val expectedCoverage =
        if emptyKeys.isEmpty then NeuropublishTargetCoverageV1.Complete
        else NeuropublishTargetCoverageV1.AllowEmpty(emptyKeys)
      if assignment.coverage != expectedCoverage then
        Left(error("declared target coverage differs from ordinal payload"))
      else Right(())

  private def validateIdentity(
      identity: NeuropublishDomainIdentityV1,
      descriptorId: String,
      descriptorVersion: String,
      expectedSize: Int,
      expectedPreimage: Vector[Byte],
      error: String => AtlasPublicationError
  ): Either[AtlasPublicationError, Unit] =
    if identity.descriptorId != descriptorId then
      Left(error("descriptor id is unsupported"))
    else if identity.descriptorVersion != descriptorVersion then
      Left(error("descriptor version is unsupported"))
    else if identity.size != expectedSize then
      Left(error("declared size differs from structural fields"))
    else if identity.structuralFingerprint != AtlasPublicationSha256.hex(identity.identityPreimage) then
      Left(error("structural fingerprint does not match identity preimage"))
    else if identity.identityPreimage != expectedPreimage then
      Left(error("identity preimage does not match structural fields"))
    else Right(())

  def metadata[P](
      keys: Field[P, String],
      metadata: Field[P, AtlasRegionMetadata]
  ): Vector[NeuropublishParcelMetadataV1] =
    keys.space.indices
      .map: point =>
        val region = metadata(point)
        NeuropublishParcelMetadataV1(
          key = keys(point),
          id = region.id.value,
          label = region.label,
          fullLabel = region.fullLabel,
          hemisphere = region.hemisphere.map(_.toString.toLowerCase),
          network = region.network.map(_.value),
          color = region.color.map(value => (value.red, value.green, value.blue)),
          attributes = region.attributes.toVector.sortBy(_._1)
        )
      .toVector

  private def declaredSupport(
      support: SpatialSupport
  ): NeuropublishDeclaredSupportV1 =
    support match
      case SpatialSupport.Volume(template, coordinate, resolution, dimensions) =>
        NeuropublishDeclaredSupportV1.Volume(
          template.value,
          coordinate.value,
          resolution.map(value => Vector(value.xMm, value.yMm, value.zMm)),
          dimensions.map(_.values)
        )
      case SpatialSupport.Surface(template, density, coverage) =>
        NeuropublishDeclaredSupportV1.Surface(
          template.value,
          density.label,
          density.verticesPerHemisphere,
          hemisphereCoverage(coverage)
        )
      case SpatialSupport.Derived(template, coordinate) =>
        NeuropublishDeclaredSupportV1.Derived(
          template.value,
          coordinate.value
        )

  private def sourceArtifact(
      artifact: SourceArtifact
  ): NeuropublishSourceArtifactV1 =
    NeuropublishSourceArtifactV1(
      id = artifact.id,
      role = artifact.role.legacy,
      sourceName = artifact.sourceName,
      sourceRef = artifact.sourceRef,
      sourceUri = artifact.sourceUri,
      citationDoi = artifact.citationDoi,
      license = license(artifact.licenseStatus),
      digest = artifact.digest.map(value =>
        NeuropublishDigestV1(value.algorithm, value.value)
      ),
      notes = artifact.notes
    )

  private def license(value: LicenseInfo): NeuropublishLicenseV1 =
    value match
      case LicenseInfo.Known(id) => NeuropublishLicenseV1.Known(id)
      case LicenseInfo.Restricted(label, termsUri) =>
        NeuropublishLicenseV1.Restricted(label, termsUri)
      case LicenseInfo.Unspecified(reason) =>
        NeuropublishLicenseV1.Unspecified(reason)
      case LicenseInfo.Missing => NeuropublishLicenseV1.Missing

  private def derivation(
      value: DerivationStep
  ): NeuropublishAtlasDerivationV1 =
    value match
      case DerivationStep.DeclaredDescriptor(details) =>
        NeuropublishAtlasDerivationV1.DeclaredDescriptor(details)
      case DerivationStep.Loaded(artifactId) =>
        NeuropublishAtlasDerivationV1.Loaded(artifactId)
      case DerivationStep.ParsedLabels(artifactId, schema) =>
        NeuropublishAtlasDerivationV1.ParsedLabels(
          artifactId,
          schema.name,
          schema.columns
        )
      case DerivationStep.ValidatedLabels(regionIds) =>
        NeuropublishAtlasDerivationV1.ValidatedLabels(
          regionIds.map(_.value)
        )
      case DerivationStep.FilteredLabels(kept, dropped) =>
        NeuropublishAtlasDerivationV1.FilteredLabels(
          kept.map(_.value),
          dropped.map(_.value)
        )
      case DerivationStep.Resampled(from, to, kind, status, certainty) =>
        NeuropublishAtlasDerivationV1.Resampled(
          from.value,
          to.value,
          transformKind(kind),
          transformStatus(status),
          confidence(certainty)
        )
      case DerivationStep.ProjectedVolumeToSurface(from, to, sampling, status) =>
        NeuropublishAtlasDerivationV1.ProjectedVolumeToSurface(
          from.value,
          to.value,
          sampling.surfaceKind.label,
          samplingMethod(sampling.method),
          sampling.notes,
          transformStatus(status)
        )
      case DerivationStep.LegacyHistory(
            action,
            fromTemplate,
            toTemplate,
            fromCoordinate,
            toCoordinate,
            status,
            certainty,
            details
          ) =>
        NeuropublishAtlasDerivationV1.LegacyHistory(
          action,
          fromTemplate.value,
          toTemplate.value,
          fromCoordinate.value,
          toCoordinate.value,
          transformStatus(status),
          confidence(certainty),
          details
        )

  private def labelEncoding(value: LabelEncoding): String =
    value match
      case LabelEncoding.VolumeIntegerLabels => "volume-integer-labels"
      case LabelEncoding.SurfaceIntegerLabels => "surface-integer-labels"
      case LabelEncoding.DerivedLabels => "derived-labels"

  private def hemisphereCoverage(value: HemisphereCoverage): String =
    value match
      case HemisphereCoverage.LeftOnly => "left-only"
      case HemisphereCoverage.RightOnly => "right-only"
      case HemisphereCoverage.Bilateral => "bilateral"
      case HemisphereCoverage.Unknown => "unknown"

  private def confidence(value: Confidence): String =
    value match
      case Confidence.Exact => "exact"
      case Confidence.High => "high"
      case Confidence.Approximate => "approximate"
      case Confidence.Uncertain => "uncertain"

  private def transformKind(value: TransformKind): String =
    value match
      case TransformKind.Identity => "identity"
      case TransformKind.Affine => "affine"
      case TransformKind.NonlinearWarp => "nonlinear-warp"
      case TransformKind.SphereResample => "sphere-resample"
      case TransformKind.VolToSurf => "volume-to-surface"
      case TransformKind.SurfToVol => "surface-to-volume"

  private def transformStatus(value: TransformStatus): String =
    value match
      case TransformStatus.Available => "available"
      case TransformStatus.Planned => "planned"

  private def samplingMethod(value: SurfaceSamplingMethod): String =
    value match
      case SurfaceSamplingMethod.NearestLabel => "nearest-label"
      case SurfaceSamplingMethod.RibbonMode => "ribbon-mode"
      case SurfaceSamplingMethod.RibbonMean => "ribbon-mean"

  private def normalizeZero(value: Double): Double =
    if value == 0.0 then 0.0 else value

private[atlas] object AtlasPublicationBinaryV1:
  private val Header = "NPUDOM1" + 0.toChar

  def finiteIndexed(
      descriptorId: String,
      version: String,
      elementKeys: Vector[String]
  ): Vector[Byte] =
    val out = Vector.newBuilder[Byte]
    appendRawString(out, Header)
    appendString(out, descriptorId)
    appendString(out, version)
    appendLong(out, elementKeys.length.toLong)
    elementKeys.foreach(appendString(out, _))
    out.result()

  def volumeGrid(
      coordinateSpaceId: String,
      convention: String,
      unit: String,
      layout: String,
      shape: Vector[Int],
      affine: Vector[Double]
  ): Vector[Byte] =
    val out = Vector.newBuilder[Byte]
    appendRawString(out, Header)
    appendString(out, "org.neuropublish.domain/volume-grid")
    appendString(out, "1")
    appendString(out, coordinateSpaceId)
    appendString(out, convention)
    appendString(out, unit)
    appendString(out, layout)
    shape.foreach(appendInt(out, _))
    affine.foreach: value =>
      val normalized = if value == 0.0 then 0.0 else value
      appendLong(out, java.lang.Double.doubleToRawLongBits(normalized))
    out.result()

  def surfaceVertices(
      surfaceSpaceId: String,
      hemisphere: String,
      vertexCount: Int,
      faceCount: Int,
      faceIndices: Vector[Int]
  ): Vector[Byte] =
    val out = Vector.newBuilder[Byte]
    appendRawString(out, Header)
    appendString(out, "org.neuropublish.domain/surface-vertices")
    appendString(out, "1")
    appendString(out, surfaceSpaceId)
    appendString(out, hemisphere)
    appendLong(out, vertexCount.toLong)
    appendLong(out, faceCount.toLong)
    faceIndices.foreach(appendInt(out, _))
    out.result()

  def bilateralSurface(
      coordinateSpaceId: String,
      left: Vector[Byte],
      right: Vector[Byte]
  ): Vector[Byte] =
    val out = Vector.newBuilder[Byte]
    appendRawString(out, "SCALAFIM-BILATERAL-SURFACE1" + 0.toChar)
    appendString(out, coordinateSpaceId)
    appendLong(out, left.length.toLong)
    out ++= left
    appendLong(out, right.length.toLong)
    out ++= right
    out.result()

  def int32LittleEndian(values: Vector[Int]): Vector[Byte] =
    val out = Vector.newBuilder[Byte]
    values.foreach(appendInt(out, _))
    out.result()

  def hex(bytes: Vector[Byte]): String =
    val out = new StringBuilder(bytes.length * 2)
    bytes.foreach: value =>
      val raw = Integer.toHexString(value & 0xff)
      if raw.length == 1 then out.append('0')
      out.append(raw)
    out.toString

  private def appendString(
      out: scala.collection.mutable.Builder[Byte, Vector[Byte]],
      value: String
  ): Unit =
    val bytes = value.getBytes("UTF-8")
    appendInt(out, bytes.length)
    out ++= bytes

  private def appendRawString(
      out: scala.collection.mutable.Builder[Byte, Vector[Byte]],
      value: String
  ): Unit =
    out ++= value.getBytes("UTF-8")

  private def appendInt(
      out: scala.collection.mutable.Builder[Byte, Vector[Byte]],
      value: Int
  ): Unit =
    var shift = 0
    while shift < 32 do
      out += ((value >>> shift) & 0xff).toByte
      shift += 8

  private def appendLong(
      out: scala.collection.mutable.Builder[Byte, Vector[Byte]],
      value: Long
  ): Unit =
    var shift = 0
    while shift < 64 do
      out += ((value >>> shift) & 0xffL).toByte
      shift += 8

/** Minimal platform-independent FIPS 180-4 digest used only for the
  * Neuropublish structural fingerprint. The identity preimage remains exposed
  * above so another language can recompute and audit the digest.
  */
private[atlas] object AtlasPublicationSha256:
  private val Constants: Array[Int] = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  )

  def hex(message: Vector[Byte]): String =
    val state = Array(
      0x6a09e667,
      0xbb67ae85,
      0x3c6ef372,
      0xa54ff53a,
      0x510e527f,
      0x9b05688c,
      0x1f83d9ab,
      0x5be0cd19
    )
    val bitLength = message.length.toLong * 8L
    val remainder = (message.length + 1) % 64
    val padding = if remainder <= 56 then 56 - remainder else 120 - remainder
    val bytes = Array.ofDim[Byte](message.length + 1 + padding + 8)
    var index = 0
    while index < message.length do
      bytes(index) = message(index)
      index += 1
    bytes(message.length) = 0x80.toByte
    index = 0
    while index < 8 do
      bytes(bytes.length - 1 - index) =
        ((bitLength >>> (8 * index)) & 0xffL).toByte
      index += 1

    val words = Array.ofDim[Int](64)
    var block = 0
    while block < bytes.length do
      var round = 0
      while round < 16 do
        val offset = block + round * 4
        words(round) =
          ((bytes(offset) & 0xff) << 24) |
            ((bytes(offset + 1) & 0xff) << 16) |
            ((bytes(offset + 2) & 0xff) << 8) |
            (bytes(offset + 3) & 0xff)
        round += 1
      while round < 64 do
        val sigma0 =
          Integer.rotateRight(words(round - 15), 7) ^
            Integer.rotateRight(words(round - 15), 18) ^
            (words(round - 15) >>> 3)
        val sigma1 =
          Integer.rotateRight(words(round - 2), 17) ^
            Integer.rotateRight(words(round - 2), 19) ^
            (words(round - 2) >>> 10)
        words(round) =
          words(round - 16) + sigma0 + words(round - 7) + sigma1
        round += 1

      var a = state(0)
      var b = state(1)
      var c = state(2)
      var d = state(3)
      var e = state(4)
      var f = state(5)
      var g = state(6)
      var h = state(7)
      round = 0
      while round < 64 do
        val upperE =
          Integer.rotateRight(e, 6) ^
            Integer.rotateRight(e, 11) ^
            Integer.rotateRight(e, 25)
        val choose = (e & f) ^ (~e & g)
        val first = h + upperE + choose + Constants(round) + words(round)
        val upperA =
          Integer.rotateRight(a, 2) ^
            Integer.rotateRight(a, 13) ^
            Integer.rotateRight(a, 22)
        val majority = (a & b) ^ (a & c) ^ (b & c)
        val second = upperA + majority
        h = g
        g = f
        f = e
        e = d + first
        d = c
        c = b
        b = a
        a = first + second
        round += 1

      state(0) += a
      state(1) += b
      state(2) += c
      state(3) += d
      state(4) += e
      state(5) += f
      state(6) += g
      state(7) += h
      block += 64

    val out = new StringBuilder(64)
    state.foreach: value =>
      val raw = Integer.toHexString(value)
      out.append("0" * (8 - raw.length)).append(raw)
    out.toString
