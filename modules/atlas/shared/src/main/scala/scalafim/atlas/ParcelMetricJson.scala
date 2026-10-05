package scalafim.atlas

import locus4s.SpaceMismatch
import locus4s.data.{Field, VectorField}
import scala.util.control.NonFatal
import upickle.default.{ReadWriter, macroRW, read, readwriter, write}

/** Portable, versioned scalar metric storage over authoritative locus4s fields.
  * The digest covers the exact UTF-8 payload string, not JSON number printing.
  * A self-contained digest detects corruption; an externally retained expected
  * digest also detects replacement. Neither establishes authenticated authorship.
  */
object ParcelMetricJson:
  import ParcelMetricWire.*

  val Format: String = "org.scalafim.atlas/parcel-metric/v1"
  val ValueEncoding: String = "float64-ieee754-hex-v1"

  def encode(realization: AtlasRealization)(
      values: Field[realization.P, Double],
      schema: ParcelMetricSchema
  ): Either[ParcelMetricError, String] =
    encodeOwned(realization)(values, schema, captureOrigin(realization))

  /** Re-save a restored measure without changing its computational origin. */
  def encode(realization: AtlasRealization)(
      metric: RestoredParcelMetric[realization.P]
  ): Either[ParcelMetricError, String] =
    val o = metric.origin
    encodeOwned(realization)(metric.values, metric.schema,
      Origin(o.provenance, o.representation, o.identityPolicy, o.source,
        o.parcelMetadata, o.displayOrder, o.support, o.assignments))

  private def captureOrigin(realization: AtlasRealization): Origin =
    val projection = realization.neuropublishProjection
    Origin(projection.atlasProvenance, realization.ref.representation,
      realization.ref.parcelIdentity, realization.ref.source, projection.parcelMetadata,
      projection.displayOrder, projection.supportDomains.map(supportReference),
      projection.assignments.map(a => ParcelMetricAssignmentReference(reference(a.source), reference(a.target), a.assetSha256)))

  private def encodeOwned(realization: AtlasRealization)(
      values: Field[realization.P, Double],
      schema: ParcelMetricSchema,
      origin: => Origin
  ): Either[ParcelMetricError, String] =
    if !realization.parcelDomain.sameRuntimeOwnerAs(values.space) then
      Left(ParcelMetricError.WrongOwner(SpaceMismatch.between(realization.parcelDomain, values.space)))
    else
      try
        // Fully evaluate before publication, even for callback-backed Fields.
        val snapshot = values.toVector.map(Float64.encode)
        val payload = Payload(1, Schema(schema.name, schema.unit, schema.attributes.toMap.toVector.sortBy(_._1)),
          realization.parcelDomainRecord, origin, ValueEncoding, snapshot)
        validate(payload).map: _ =>
          val body = write(payload)
          write(Envelope(Format, body, digest(body)))
      catch
        case NonFatal(error) => Left(ParcelMetricError.EvaluationFailed(error.toString))

  def decode(target: AtlasRealization)(
      document: String,
      expectedSha256: Option[String] = None
  ): Either[ParcelMetricError, RestoredParcelMetric[target.P]] =
    try
      val envelope = read[Envelope](document)
      if envelope.format != Format then Left(ParcelMetricError.InvalidDocument(s"unsupported format ${envelope.format}"))
      else if !isDigest(envelope.sha256) || digest(envelope.payload) != envelope.sha256 ||
          expectedSha256.exists(_ != envelope.sha256)
      then Left(ParcelMetricError.IntegrityMismatch)
      else
        val payload = read[Payload](envelope.payload)
        for
          schema <- validate(payload)
          _ <- if payload.parcelDomain.identity == target.parcelDomainRecord.identity &&
              payload.parcelDomain.elementKeys == target.parcelDomainRecord.elementKeys
            then Right(()) else Left(ParcelMetricError.DomainMismatch)
          decoded <- payload.values.zipWithIndex.foldLeft[Either[ParcelMetricError, Vector[Double]]](Right(Vector.empty)):
            case (result, (bits, ordinal)) =>
              for
                values <- result
                value <- Float64.decode(bits, ordinal)
              yield values :+ value
        yield
          val o = payload.origin
          RestoredParcelMetric(VectorField.tabulate(target.parcelDomain)(p => decoded(p.ordinal)), schema,
            new ParcelMetricOrigin(o.provenance, o.representation, o.identityPolicy, o.source,
              o.parcelMetadata, o.displayOrder, o.support, o.assignments), envelope.sha256)
    catch
      case NonFatal(error) => Left(ParcelMetricError.InvalidDocument(error.toString))

  private def validate(payload: Payload): Either[ParcelMetricError, ParcelMetricSchema] =
    if payload.version != 1 || payload.valueEncoding != ValueEncoding then
      Left(ParcelMetricError.InvalidDocument("unsupported version or value encoding"))
    else if payload.values.length != payload.parcelDomain.elementKeys.length then
      Left(ParcelMetricError.InvalidDocument("value count differs from parcel domain"))
    else if payload.schema.attributes.map(_._1).distinct.length != payload.schema.attributes.length then
      Left(ParcelMetricError.InvalidSchema("duplicate attribute keys"))
    else
      for
        schema <- ParcelMetricSchema.from(payload.schema.name, payload.schema.unit, payload.schema.attributes.toMap)
        _ <- AtlasPublicationProjection.validateParcelDomain(payload.parcelDomain)
          .left.map(e => ParcelMetricError.InvalidDocument(e.message))
        _ <- AtlasPublicationProjection.validateParcelMetadata(payload.parcelDomain, payload.origin.parcelMetadata)
          .left.map(e => ParcelMetricError.InvalidOrigin(e.message))
        _ <- AtlasPublicationProjection.validateDisplayOrder(payload.parcelDomain, payload.origin.displayOrder)
          .left.map(e => ParcelMetricError.InvalidOrigin(e.message))
        _ <- AtlasPublicationProjection.validateProvenanceRecord(payload.origin.provenance, payload.origin.parcelMetadata)
          .left.map(e => ParcelMetricError.InvalidOrigin(e.message))
        _ <- validateOrigin(payload.parcelDomain, payload.origin)
      yield schema

  private def validateOrigin(domain: NeuropublishFiniteIndexedDomainV1, origin: Origin): Either[ParcelMetricError, Unit] =
    def invalid(message: String) = Left(ParcelMetricError.InvalidOrigin(message))
    try
      val p = origin.provenance
      val i = p.identity
      val identity = AtlasIdentity(i.family, i.model, i.variant, i.release.map(r => AtlasRelease(r.label, r.year, r.version, r.commit, r.date)))
      val byKey = origin.parcelMetadata.map(row => row.key -> row).toMap
      val ordered = origin.displayOrder.map(byKey)
      val regions = RegionIndex(origin.parcelMetadata.map: row =>
        AtlasRegionMetadata.fromStrings(RegionId(row.id), row.label, Some(row.fullLabel),
          row.hemisphere.map(hemisphere), row.network.map(NetworkId.apply),
          row.color.map((r, g, b) => Rgb(r, g, b)), row.attributes.toMap))
      if p.labels.backgroundSourceLabel != Some(0) then invalid("hard-label origin requires background source label 0")
      else if ordered.map(_.id).sorted != p.labels.regionIds.sorted then invalid("display IDs differ from label-schema IDs")
      else if origin.source.exists(_.trim.isEmpty) then invalid("source must be non-empty when present")
      else if origin.support.isEmpty || origin.support.map(_.domain).distinct.length != origin.support.length then invalid("support references must be non-empty and unique")
      else if origin.support.exists(s => !validReference(s.domain)) then invalid("invalid support identity reference")
      else if origin.assignments.length != origin.support.length ||
          origin.assignments.map(_.source) != origin.support.map(_.domain) ||
          origin.assignments.exists(a => a.target != reference(domain.identity) || !isDigest(a.sha256))
      then invalid("assignment references differ from support or parcel identity")
      else
        for
          keys <- AtlasParcelDomain.keysForOrigin(identity, i.parcelVariant, origin.identityPolicy,
            origin.representation, origin.source, regions).left.map(e => ParcelMetricError.InvalidOrigin(e.message))
          _ <- if keys == domain.elementKeys then Right(()) else invalid("canonical keys disagree with origin namespace or identity policy")
          _ <- validateSpaceKinds(origin)
          _ <- validateSupport(origin)
          _ <- AtlasCompose.validateSupport(p, origin.support.map(_.domain))
            .left.map(e => ParcelMetricError.InvalidOrigin(e.message))
        yield ()
    catch
      case NonFatal(error) => invalid(error.toString)

  private def validateSpaceKinds(origin: Origin): Either[ParcelMetricError, Unit] =
    val (template, coordinate) = origin.provenance.declaredSupport match
      case NeuropublishDeclaredSupportV1.Volume(t, c, _, _) => (t, c)
      case NeuropublishDeclaredSupportV1.Surface(t, c, _, _, _) => (t, c)
      case NeuropublishDeclaredSupportV1.Derived(t, c) => (t, c)
    val identity = origin.provenance.identity
    val checked = for
      t <- SpaceId.from(template)
      c <- SpaceId.from(coordinate)
      _ <- AtlasRef.checked(AtlasDetails(identity.family, identity.model,
        source = origin.source, parcelVariant = identity.parcelVariant,
        parcelIdentity = origin.identityPolicy), origin.representation, t, c)
    yield ()
    checked.left.map(error => ParcelMetricError.InvalidOrigin(error.message))

  private def validateSupport(origin: Origin): Either[ParcelMetricError, Unit] =
    def invalid = Left(ParcelMetricError.InvalidOrigin("declared support differs from compact support references"))
    val p = origin.provenance
    p.declaredSupport match
      case NeuropublishDeclaredSupportV1.Volume(template, coordinate, voxelSize, dimensions) =>
        val volumes = origin.support.collect { case v: ParcelMetricSupport.Volume => v }
        if origin.representation != AtlasRepresentation.Volume || p.labels.encoding != "volume-integer-labels" ||
            template.trim.isEmpty || coordinate.trim.isEmpty || volumes.length != 1 || origin.support.length != 1 ||
            voxelSize.exists(v => v.length != 3 || v.exists(x => !x.isFinite || x <= 0.0)) ||
            volumes.exists(v => v.coordinateSpaceId != coordinate || v.shape.length != 3 || v.shape.exists(_ <= 0) ||
              v.shape.foldLeft(1L)((size, dimension) => math.min(size * dimension, Int.MaxValue.toLong + 1L)) != v.domain.size.toLong ||
              dimensions.exists(_ != v.shape) ||
              v.domain.descriptorId != "org.neuropublish.domain/volume-grid" || v.domain.descriptorVersion != "1")
        then invalid else Right(())
      case NeuropublishDeclaredSupportV1.Surface(template, coordinate, density, vertices, coverage) =>
        val surfaces = origin.support.collect { case s: ParcelMetricSupport.Surface => s }
        if origin.representation != AtlasRepresentation.Surface || p.labels.encoding != "surface-integer-labels" ||
            template.trim.isEmpty || coordinate.trim.isEmpty || density.trim.isEmpty ||
            surfaces.length != 2 || origin.support.length != 2 || surfaces.map(_.hemisphere) != Vector("left", "right") ||
            !Set("left-only", "right-only", "bilateral", "unknown").contains(coverage) || vertices.exists(_ <= 0) ||
            surfaces.exists(s => s.coordinateSpaceId != coordinate || s.vertexCount <= 0 ||
              s.vertexCount != s.domain.size || vertices.exists(_ != s.vertexCount) ||
              s.domain.descriptorId != "org.neuropublish.domain/surface-vertices" || s.domain.descriptorVersion != "1")
        then invalid else Right(())
      case NeuropublishDeclaredSupportV1.Derived(_, _) => invalid

  private def reference(identity: NeuropublishDomainIdentityV1): ParcelMetricDomainReference =
    ParcelMetricDomainReference(identity.descriptorId, identity.descriptorVersion, identity.size, identity.structuralFingerprint)

  private def hemisphere(value: String): Hemisphere = value match
    case "left" => Hemisphere.Left
    case "right" => Hemisphere.Right
    case "bilateral" => Hemisphere.Bilateral
    case "midline" => Hemisphere.Midline
    case other => throw new IllegalArgumentException(s"unsupported hemisphere $other")

  private def supportReference(domain: NeuropublishSpatialDomainV1): ParcelMetricSupport = domain match
    case v: NeuropublishVolumeGridDomainV1 => ParcelMetricSupport.Volume(reference(v.identity), v.coordinateSpaceId, v.shape)
    case s: NeuropublishSurfaceVerticesDomainV1 => ParcelMetricSupport.Surface(reference(s.identity), s.surfaceSpaceId, s.hemisphere, s.vertexCount)

  private def validReference(ref: ParcelMetricDomainReference): Boolean =
    ref.descriptorId.trim.nonEmpty && ref.descriptorVersion.trim.nonEmpty && ref.size > 0 && isDigest(ref.structuralFingerprint)

  private def isDigest(value: String): Boolean =
    value.length == 64 && value.forall(c => c >= '0' && c <= '9' || c >= 'a' && c <= 'f')

  private[atlas] def digest(value: String): String =
    AtlasPublicationSha256.hex(value.getBytes("UTF-8").toVector)

  private[atlas] object Float64:
    def encode(value: Double): String =
      val bits = if value.isNaN then 0x7ff8000000000000L else java.lang.Double.doubleToRawLongBits(value)
      val raw = java.lang.Long.toHexString(bits)
      "0" * (16 - raw.length) + raw

    def decode(value: String, ordinal: Int): Either[ParcelMetricError, Double] =
      if value.length != 16 || !value.forall(c => c >= '0' && c <= '9' || c >= 'a' && c <= 'f') then
        Left(ParcelMetricError.InvalidValue(ordinal, "expected exactly 16 lowercase hexadecimal digits"))
      else
        var bits = 0L
        var i = 0
        while i < 16 do
          val c = value.charAt(i)
          bits = (bits << 4) | (if c <= '9' then c - '0' else c - 'a' + 10).toLong
          i += 1
        val decoded = java.lang.Double.longBitsToDouble(bits)
        if decoded.isNaN && value != "7ff8000000000000" then
          Left(ParcelMetricError.InvalidValue(ordinal, "NaN must use the canonical quiet-NaN encoding"))
        else Right(decoded)

private[atlas] object ParcelMetricWire:
  final case class Envelope(format: String, payload: String, sha256: String)
  final case class Schema(name: String, unit: Option[String], attributes: Vector[(String, String)])
  final case class Origin(provenance: NeuropublishAtlasProvenanceV1, representation: AtlasRepresentation,
      identityPolicy: ParcelIdentity, source: Option[String], parcelMetadata: Vector[NeuropublishParcelMetadataV1],
      displayOrder: Vector[String], support: Vector[ParcelMetricSupport], assignments: Vector[ParcelMetricAssignmentReference])
  final case class Payload(version: Int, schema: Schema, parcelDomain: NeuropublishFiniteIndexedDomainV1,
      origin: Origin, valueEncoding: String, values: Vector[String])

  given ReadWriter[AtlasRepresentation] = readwriter[String].bimap(_.toString, AtlasRepresentation.valueOf)
  given ReadWriter[ParcelIdentity] = readwriter[String].bimap(_.toString, ParcelIdentity.valueOf)
  given ReadWriter[NeuropublishDeclaredSupportV1.Volume] = macroRW
  given ReadWriter[NeuropublishDeclaredSupportV1.Surface] = macroRW
  given ReadWriter[NeuropublishDeclaredSupportV1.Derived] = macroRW
  given ReadWriter[NeuropublishAtlasReleaseV1] = macroRW
  given ReadWriter[NeuropublishAtlasIdentityV1] = macroRW
  given ReadWriter[NeuropublishDeclaredSupportV1] = macroRW
  given ReadWriter[NeuropublishAtlasLabelSchemaV1] = macroRW
  given ReadWriter[NeuropublishDigestV1] = macroRW
  given ReadWriter[NeuropublishLicenseV1.Known] = macroRW
  given ReadWriter[NeuropublishLicenseV1.Restricted] = macroRW
  given ReadWriter[NeuropublishLicenseV1.Unspecified] = macroRW
  given ReadWriter[NeuropublishLicenseV1.Missing.type] = macroRW
  given ReadWriter[NeuropublishLicenseV1] = macroRW
  given ReadWriter[NeuropublishSourceArtifactV1] = macroRW
  given ReadWriter[NeuropublishDomainIdentityV1] = macroRW
  given ReadWriter[NeuropublishVolumeGridDomainV1] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.DeclaredDescriptor] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.Loaded] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.ParsedLabels] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.ValidatedLabels] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.FilteredLabels] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.SelectedParcels] = macroRW
  given ReadWriter[AtlasCompositionOverlap] = readwriter[String].bimap(_.toString, AtlasCompositionOverlap.valueOf)
  given ReadWriter[AtlasCompositionOccluded] = readwriter[String].bimap(_.toString, AtlasCompositionOccluded.valueOf)
  given ReadWriter[NeuropublishCompositionParentV1] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.ComposedParcels] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.GroupedParcels] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.DilatedParcels] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.Resampled] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.ProjectedVolumeToSurface] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1.LegacyHistory] = macroRW
  given ReadWriter[NeuropublishAtlasDerivationV1] = macroRW
  given ReadWriter[NeuropublishCitationV1] = macroRW
  given ReadWriter[NeuropublishAtlasProvenanceV1] = macroRW
  given ReadWriter[NeuropublishFiniteIndexedDomainV1] = macroRW
  given ReadWriter[NeuropublishParcelMetadataV1] = macroRW
  given ReadWriter[ParcelMetricDomainReference] = macroRW
  given metricVolumeCodec: ReadWriter[ParcelMetricSupport.Volume] = macroRW
  given metricSurfaceCodec: ReadWriter[ParcelMetricSupport.Surface] = macroRW
  given ReadWriter[ParcelMetricSupport] = macroRW
  given ReadWriter[ParcelMetricAssignmentReference] = macroRW
  given ReadWriter[Envelope] = macroRW
  given ReadWriter[Schema] = macroRW
  given ReadWriter[Origin] = macroRW
  given ReadWriter[Payload] = macroRW
