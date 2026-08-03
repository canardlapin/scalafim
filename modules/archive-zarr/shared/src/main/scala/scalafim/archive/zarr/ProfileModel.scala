package scalafim.archive.zarr

import zarr4s.*

enum NeuroArchiveZarrError:
  case Kernel(cause: ZarrError)
  case InvalidManifest(path: String, detail: String)
  case InvalidCanonicalAxes(found: Vector[Option[String]])
  case ManifestArrayMismatch(detail: String)
  case InvalidProfileLayout(detail: String)
  case IncompletePublication(detail: String)
  case DigestMismatch(subject: String, expected: Sha256Digest, actual: Sha256Digest)
  case PublicationFailure(detail: String)

  def message: String = this match
    case Kernel(cause) => cause.message
    case InvalidManifest(path, detail) => s"invalid NeuroArchive manifest at $path: $detail"
    case InvalidCanonicalAxes(found) =>
      s"canonical BOLD axes must be [t,z,y,x], found ${found.mkString("[", ",", "]")}"
    case ManifestArrayMismatch(detail) => s"manifest/array mismatch: $detail"
    case InvalidProfileLayout(detail) => s"invalid NeuroArchive Zarr layout: $detail"
    case IncompletePublication(detail) => s"incomplete publication: $detail"
    case DigestMismatch(subject, expected, actual) =>
      s"$subject SHA-256 mismatch: expected ${expected.value}, found ${actual.value}"
    case PublicationFailure(detail) => s"publication failed: $detail"

opaque type AcquisitionId = String

object AcquisitionId:
  def from(value: String): Either[NeuroArchiveZarrError, AcquisitionId] =
    Identifier.checked(value, "acquisition id")

  def unsafe(value: String): AcquisitionId =
    from(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (id: AcquisitionId) inline def value: String = id

opaque type PayloadId = String

object PayloadId:
  def from(value: String): Either[NeuroArchiveZarrError, PayloadId] =
    Identifier.checked(value, "payload id")

  def unsafe(value: String): PayloadId =
    from(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (id: PayloadId) inline def value: String = id

opaque type Sha256Digest = String

object Sha256Digest:
  def from(value: String): Either[NeuroArchiveZarrError, Sha256Digest] =
    val normalized = value.toLowerCase
    if normalized.length == 64 && normalized.forall(character => Character.digit(character, 16) >= 0)
    then Right(normalized)
    else Left(NeuroArchiveZarrError.InvalidManifest("sha256", "expected 64 hexadecimal digits"))

  def unsafe(value: String): Sha256Digest =
    from(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (digest: Sha256Digest) inline def value: String = digest

opaque type ContentRevision = Sha256Digest

object ContentRevision:
  def from(value: String): Either[NeuroArchiveZarrError, ContentRevision] =
    Sha256Digest.from(value)

  def unsafe(value: String): ContentRevision =
    from(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (revision: ContentRevision) inline def value: String = revision

private object Identifier:
  private val Pattern = "^[A-Za-z0-9][A-Za-z0-9_.:-]*$".r

  def checked(value: String, label: String): Either[NeuroArchiveZarrError, String] =
    val normalized = value.trim
    if Pattern.matches(normalized) then Right(normalized)
    else Left(NeuroArchiveZarrError.InvalidManifest(label, "contains invalid characters"))

enum SignalUnits(val id: String):
  case Arbitrary extends SignalUnits("arbitrary")
  case PercentSignal extends SignalUnits("percent-signal")

enum SpatialUnits(val id: String):
  case Millimeter extends SpatialUnits("millimeter")
  case Meter extends SpatialUnits("meter")

enum TimeUnits(val id: String):
  case Second extends TimeUnits("second")
  case Millisecond extends TimeUnits("millisecond")

final case class ScalarCalibration private (
    storedDataType: String,
    scale: Double,
    offset: Double,
    physicalDataType: String,
    units: SignalUnits
):
  inline def apply(stored: Double): Double = stored * scale + offset

object ScalarCalibration:
  def apply(
      storedDataType: String,
      scale: Double,
      offset: Double,
      units: SignalUnits = SignalUnits.Arbitrary
  ): Either[NeuroArchiveZarrError, ScalarCalibration] =
    if !scale.isFinite || scale == 0.0 then
      Left(NeuroArchiveZarrError.InvalidManifest("canonical.calibration.scale", "must be finite and non-zero"))
    else if !offset.isFinite then
      Left(NeuroArchiveZarrError.InvalidManifest("canonical.calibration.offset", "must be finite"))
    else if !BuiltInDataTypes.all.exists(_.name == storedDataType) then
      Left(NeuroArchiveZarrError.InvalidManifest("canonical.calibration.stored_data_type", s"unsupported type $storedDataType"))
    else Right(new ScalarCalibration(storedDataType, scale, offset, "float64", units))

final class Affine4x4 private (private val entries: Vector[Double]):
  def apply(row: Int, column: Int): Double = entries(row * 4 + column)
  def toVector: Vector[Double] = entries

  override def equals(other: Any): Boolean = other match
    case that: Affine4x4 => entries == that.entries
    case _ => false

  override def hashCode(): Int = entries.hashCode()

object Affine4x4:
  def apply(entries: Seq[Double]): Either[NeuroArchiveZarrError, Affine4x4] =
    val copied = entries.toVector
    if copied.length != 16 then
      Left(NeuroArchiveZarrError.InvalidManifest("canonical.geometry.voxel_to_world", "must contain 16 values"))
    else if copied.exists(!_.isFinite) then
      Left(NeuroArchiveZarrError.InvalidManifest("canonical.geometry.voxel_to_world", "must contain only finite values"))
    else Right(new Affine4x4(copied))

final case class VoxelGeometry private (
    spatialShape: Shape,
    voxelToWorld: Affine4x4,
    units: SpatialUnits
)

object VoxelGeometry:
  def apply(
      spatialShape: Shape,
      voxelToWorld: Affine4x4,
      units: SpatialUnits = SpatialUnits.Millimeter
  ): Either[NeuroArchiveZarrError, VoxelGeometry] =
    if spatialShape.rank.toInt != 3 then
      Left(NeuroArchiveZarrError.InvalidManifest("canonical.geometry.spatial_shape", "must have rank three [z,y,x]"))
    else if spatialShape.toVector.exists(_ <= 0L) then
      Left(NeuroArchiveZarrError.InvalidManifest("canonical.geometry.spatial_shape", "dimensions must be positive"))
    else Right(new VoxelGeometry(spatialShape, voxelToWorld, units))

enum AcquisitionTiming:
  case Regular(origin: Double, step: Double, count: Long, units: TimeUnits)
  case Explicit(coordinates: Vector[Double], units: TimeUnits)

  def sampleCount: Long = this match
    case Regular(_, _, found, _) => found
    case Explicit(found, _) => found.length.toLong

  def validate: Either[NeuroArchiveZarrError, AcquisitionTiming] = this match
    case regular @ Regular(origin, step, count, _) =>
      if !origin.isFinite then Left(NeuroArchiveZarrError.InvalidManifest("canonical.timing.origin", "must be finite"))
      else if !step.isFinite || step <= 0.0 then Left(NeuroArchiveZarrError.InvalidManifest("canonical.timing.step", "must be finite and positive"))
      else if count <= 0L then Left(NeuroArchiveZarrError.InvalidManifest("canonical.timing.count", "must be positive"))
      else Right(regular)
    case explicit @ Explicit(coordinates, _) =>
      if coordinates.isEmpty then Left(NeuroArchiveZarrError.InvalidManifest("canonical.timing.coordinates", "must be non-empty"))
      else if coordinates.exists(!_.isFinite) then Left(NeuroArchiveZarrError.InvalidManifest("canonical.timing.coordinates", "must contain only finite values"))
      else if coordinates.sliding(2).exists(pair => pair(1) <= pair(0)) then Left(NeuroArchiveZarrError.InvalidManifest("canonical.timing.coordinates", "must be strictly increasing"))
      else Right(explicit)

final case class SourceArtifact private (relativePath: String, sha256: Sha256Digest)

object SourceArtifact:
  def apply(relativePath: String, sha256: Sha256Digest): Either[NeuroArchiveZarrError, SourceArtifact] =
    val normalized = relativePath.replace('\\', '/').trim
    if normalized.isEmpty || normalized.startsWith("/") || normalized.split('/').exists(segment => segment.isEmpty || segment == "." || segment == "..") then
      Left(NeuroArchiveZarrError.InvalidManifest("source.relative_path", "must be a confined relative path"))
    else Right(new SourceArtifact(normalized, sha256))

final case class NeuroArchiveManifest private (
    acquisitionId: AcquisitionId,
    payloadId: PayloadId,
    contentRevision: ContentRevision,
    source: SourceArtifact,
    shape: Shape,
    calibration: ScalarCalibration,
    geometry: VoxelGeometry,
    timing: AcquisitionTiming,
    logicalPayloadHash: Sha256Digest
)

object NeuroArchiveManifest:
  val profileId = "neuroarchive-zarr-0.1"
  val canonicalAxes = Vector("t", "z", "y", "x")

  def apply(
      acquisitionId: AcquisitionId,
      payloadId: PayloadId,
      contentRevision: ContentRevision,
      source: SourceArtifact,
      shape: Shape,
      calibration: ScalarCalibration,
      geometry: VoxelGeometry,
      timing: AcquisitionTiming,
      logicalPayloadHash: Sha256Digest
  ): Either[NeuroArchiveZarrError, NeuroArchiveManifest] =
    if shape.rank.toInt != 4 then
      Left(NeuroArchiveZarrError.InvalidManifest("canonical.shape", "must have rank four [t,z,y,x]"))
    else if shape.toVector.exists(_ <= 0L) then
      Left(NeuroArchiveZarrError.InvalidManifest("canonical.shape", "dimensions must be positive"))
    else if geometry.spatialShape.toVector != shape.toVector.drop(1) then
      Left(NeuroArchiveZarrError.InvalidManifest("canonical.geometry.spatial_shape", "must equal canonical [z,y,x] shape"))
    else timing.validate.flatMap: validTiming =>
      if validTiming.sampleCount != shape.axis(0) then
        Left(NeuroArchiveZarrError.InvalidManifest("canonical.timing", "count must equal the t-axis length"))
      else Right(new NeuroArchiveManifest(
        acquisitionId,
        payloadId,
        contentRevision,
        source,
        shape,
        calibration,
        geometry,
        validTiming,
        logicalPayloadHash
      ))

final class CanonicalBold private (
    val array: ArrayDescriptor,
    val manifest: NeuroArchiveManifest
)

object CanonicalBold:
  def refine(
      array: ArrayDescriptor,
      manifest: NeuroArchiveManifest
  ): Either[NeuroArchiveZarrError, CanonicalBold] =
    val names = array.dimensionNames.getOrElse(Vector.empty)
    if array.shape.rank.toInt != 4 then
      Left(NeuroArchiveZarrError.ManifestArrayMismatch("canonical array must have rank four"))
    else if names != NeuroArchiveManifest.canonicalAxes.map(Some(_)) then
      Left(NeuroArchiveZarrError.InvalidCanonicalAxes(names))
    else if array.shape.toVector.exists(_ <= 0L) then
      Left(NeuroArchiveZarrError.ManifestArrayMismatch("canonical array dimensions must be positive"))
    else if array.shape != manifest.shape then
      Left(NeuroArchiveZarrError.ManifestArrayMismatch("shape differs from neuroarchive.json"))
    else if array.dataType.name != manifest.calibration.storedDataType then
      Left(NeuroArchiveZarrError.ManifestArrayMismatch("stored data type differs from calibration"))
    else ProfileLayout.validate(array).map(_ => new CanonicalBold(array, manifest))

  private object ProfileLayout:
    def validate(array: ArrayDescriptor): Either[NeuroArchiveZarrError, Unit] = array.layout match
      case PhysicalLayout.Direct(codecs) => portable(codecs)
      case PhysicalLayout.Sharded(_, innerCodecs, _, IndexLocation.Start, outerCodecs) =>
        if outerCodecs.nonEmpty then Left(NeuroArchiveZarrError.InvalidProfileLayout("outer shard codecs are not permitted"))
        else portable(innerCodecs)
      case PhysicalLayout.Sharded(_, _, _, IndexLocation.End, _) =>
        Left(NeuroArchiveZarrError.InvalidProfileLayout("canonical shards must place the index at the start"))

    private def portable(codecs: CodecProgram): Either[NeuroArchiveZarrError, Unit] = codecs.stages match
      case Vector(BytesCodec(Some(Endianness.Little)), _: GzipCodec, Crc32cCodec) => Right(())
      case _ => Left(NeuroArchiveZarrError.InvalidProfileLayout(
        "codec chain must be little-endian bytes, gzip, CRC32C"
      ))

final case class PublishedObject(key: StoreKey, length: ByteCount, sha256: Sha256Digest)

final case class PublicationReceipt private (
    contentRevision: ContentRevision,
    logicalPayloadHash: Sha256Digest,
    rootMetadataHash: Sha256Digest,
    manifestHash: Sha256Digest,
    canonicalMetadataHash: Sha256Digest,
    expectedOuterObjects: Long,
    objects: Vector[PublishedObject],
    writerVersion: String
)

object PublicationReceipt:
  val version = 1

  def complete(
      contentRevision: ContentRevision,
      logicalPayloadHash: Sha256Digest,
      rootMetadataHash: Sha256Digest,
      manifestHash: Sha256Digest,
      canonicalMetadataHash: Sha256Digest,
      expectedOuterObjects: Long,
      objects: Vector[PublishedObject],
      writerVersion: String
  ): Either[NeuroArchiveZarrError, PublicationReceipt] =
    if expectedOuterObjects < 0L then
      Left(NeuroArchiveZarrError.InvalidManifest("publication.expected_outer_objects", "must be non-negative"))
    else if objects.length.toLong != expectedOuterObjects then
      Left(NeuroArchiveZarrError.IncompletePublication(
        s"expected $expectedOuterObjects outer payload objects, receipt contains ${objects.length}"
      ))
    else if objects.map(_.key.value).distinct.length != objects.length then
      Left(NeuroArchiveZarrError.InvalidManifest("publication.objects", "contains duplicate keys"))
    else if objects.exists(!_.key.value.startsWith("canonical/c")) then
      Left(NeuroArchiveZarrError.InvalidManifest("publication.objects", "payload keys must be under canonical/c"))
    else Identifier.checked(writerVersion, "writer version").map: checkedWriter =>
      new PublicationReceipt(
        contentRevision,
        logicalPayloadHash,
        rootMetadataHash,
        manifestHash,
        canonicalMetadataHash,
        expectedOuterObjects,
        objects.sortBy(_.key.value),
        checkedWriter
      )
