package scalafim.archive.zarr

import scalafim.archive.{
  ArchiveError,
  CanonicalKey,
  CanonicalValue,
  PayloadRoleId,
  PersistedRepresentation,
  RepresentationKey,
  RepresentationMetadata
}
import scalafim.zarr.ZarrMetadataRenderer

/** Archive-neutral representation metadata for canonical dense BOLD.
  *
  * The archive module records storage and scientific profile facts as
  * canonical values. The response interop module owns their interpretation as
  * a `ResponseSchema` and `ResponseSource`.
  */
object DenseBoldRevisionMetadata:
  val Representation: RepresentationKey =
    RepresentationKey.unsafe("org.scalafim/dense-bold@1")

  val PayloadRole: PayloadRoleId =
    PayloadRoleId
      .from("org.scalafim.zarr", "canonical-response")
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )

  val DescriptorFormat: String =
    "org.scalafim/neuroarchive-dense-zarr-binding@1"

  val OutputSchemaFormat: String =
    "org.scalafim/neuroarchive-dense-response-schema@1"

  def attributes(
      opened: AsyncOpenedCanonicalBold
  ): Either[ArchiveError, CanonicalValue] =
    val profile = opened.canonical
    val manifest = profile.manifest
    for
      representation <- persistedRepresentation(opened)
      attributes <- CanonicalValue.obj(Vector(
        CanonicalKey.unsafe("profile") ->
          CanonicalValue.string(NeuroArchiveManifest.profileId),
        CanonicalKey.unsafe("acquisition-id") ->
          CanonicalValue.string(manifest.acquisitionId.value),
        CanonicalKey.unsafe("content-revision") ->
          CanonicalValue.string(manifest.contentRevision.value),
        CanonicalKey.unsafe("logical-payload-sha256") ->
          CanonicalValue.string(manifest.logicalPayloadHash.value),
        RepresentationMetadata.DescriptorAttribute ->
          representation.descriptor,
        RepresentationMetadata.OutputSchemaAttribute ->
          representation.outputSchema
      ))
    yield attributes

  def persistedRepresentation(
      opened: AsyncOpenedCanonicalBold
  ): Either[ArchiveError, PersistedRepresentation] =
    for
      arrayMetadata <- ZarrMetadataRenderer
        .array(opened.canonical.array)
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
      descriptor <- descriptorValue(opened, arrayMetadata)
      outputSchema <- outputSchemaValue(opened.canonical.manifest)
    yield PersistedRepresentation(
      Representation,
      descriptor,
      outputSchema
    )

  private def descriptorValue(
      opened: AsyncOpenedCanonicalBold,
      arrayMetadata: String
  ): Either[ArchiveError, CanonicalValue] =
    val manifest = opened.canonical.manifest
    for
      calibration <- CanonicalValue.obj(Vector(
        CanonicalKey.unsafe("stored-data-type") ->
          CanonicalValue.string(manifest.calibration.storedDataType),
        CanonicalKey.unsafe("physical-data-type") ->
          CanonicalValue.string(manifest.calibration.physicalDataType),
        CanonicalKey.unsafe("scale") ->
          CanonicalValue.float64(manifest.calibration.scale),
        CanonicalKey.unsafe("offset") ->
          CanonicalValue.float64(manifest.calibration.offset),
        CanonicalKey.unsafe("units") ->
          CanonicalValue.string(manifest.calibration.units.id)
      ))
      objects <- sequence(opened.publication.objects.map: published =>
        CanonicalValue.obj(Vector(
          CanonicalKey.unsafe("id") ->
            CanonicalValue.string(published.key.value),
          CanonicalKey.unsafe("length") ->
            CanonicalValue.int64(published.length.toLong)
        ))
      )
      descriptor <- CanonicalValue.obj(Vector(
        CanonicalKey.unsafe("format") ->
          CanonicalValue.string(DescriptorFormat),
        CanonicalKey.unsafe("profile") ->
          CanonicalValue.string(NeuroArchiveManifest.profileId),
        CanonicalKey.unsafe("canonical-path") ->
          CanonicalValue.string("canonical"),
        CanonicalKey.unsafe("payload-id") ->
          CanonicalValue.string(manifest.payloadId.value),
        CanonicalKey.unsafe("array-metadata") ->
          CanonicalValue.string(arrayMetadata),
        CanonicalKey.unsafe("calibration") -> calibration,
        CanonicalKey.unsafe("objects") ->
          CanonicalValue.array(objects)
      ))
    yield descriptor

  private def outputSchemaValue(
      manifest: NeuroArchiveManifest
  ): Either[ArchiveError, CanonicalValue] =
    val acquisition = manifest.acquisitionId.value
    val revisionPrefix = manifest.contentRevision.value.take(16)
    val geometryIdentity = geometryDigest(manifest)
    for
      spatialCount <- checkedSpatialCount(manifest.shape.toVector.drop(1))
      timing <- timingValue(manifest.timing, acquisition)
      samples <- CanonicalValue.obj(Vector(
        CanonicalKey.unsafe("id") ->
          CanonicalValue.string(s"$acquisition:voxels:$geometryIdentity"),
        CanonicalKey.unsafe("count") ->
          CanonicalValue.int64(spatialCount),
        CanonicalKey.unsafe("kind") ->
          CanonicalValue.string("volume"),
        CanonicalKey.unsafe("space-namespace") ->
          CanonicalValue.string("neuroarchive-zarr-space"),
        CanonicalKey.unsafe("space-value") ->
          CanonicalValue.string(geometryIdentity),
        CanonicalKey.unsafe("mask") ->
          CanonicalValue.Null,
        CanonicalKey.unsafe("ordering-namespace") ->
          CanonicalValue.string("neuroarchive-zarr-ordering"),
        CanonicalKey.unsafe("ordering-value") ->
          CanonicalValue.string("canonical-z-y-x")
      ))
      signal <- CanonicalValue.obj(Vector(
        CanonicalKey.unsafe("units") ->
          CanonicalValue.string(manifest.calibration.units.id),
        CanonicalKey.unsafe("calibration") ->
          CanonicalValue.string("applied"),
        CanonicalKey.unsafe("non-finite") ->
          CanonicalValue.string("preserve")
      ))
      schema <- CanonicalValue.obj(Vector(
        CanonicalKey.unsafe("format") ->
          CanonicalValue.string(OutputSchemaFormat),
        CanonicalKey.unsafe("id") ->
          CanonicalValue.string(
            s"$acquisition:dense-response:$revisionPrefix"
          ),
        CanonicalKey.unsafe("time") -> timing,
        CanonicalKey.unsafe("samples") -> samples,
        CanonicalKey.unsafe("signal") -> signal
      ))
    yield schema

  private def checkedSpatialCount(
      dimensions: Vector[Long]
  ): Either[ArchiveError, Long] =
    var product = 1L
    var index = 0
    while index < dimensions.length do
      val dimension = dimensions(index)
      if dimension != 0L && product > Long.MaxValue / dimension then
        return Left(ArchiveError.ShapeMismatch(
          "canonical dense spatial sample count overflows Long"
        ))
      product *= dimension
      index += 1
    Right(product)

  private def timingValue(
      timing: AcquisitionTiming,
      acquisition: String
  ): Either[ArchiveError, CanonicalValue] =
    timing match
      case AcquisitionTiming.Regular(origin, step, count, units) =>
        CanonicalValue.obj(Vector(
          CanonicalKey.unsafe("id") ->
            CanonicalValue.string(s"$acquisition:time"),
          CanonicalKey.unsafe("kind") ->
            CanonicalValue.string("regular"),
          CanonicalKey.unsafe("units") ->
            CanonicalValue.string(units.id),
          CanonicalKey.unsafe("count") ->
            CanonicalValue.int64(count),
          CanonicalKey.unsafe("origin") ->
            CanonicalValue.float64(origin),
          CanonicalKey.unsafe("step") ->
            CanonicalValue.float64(step)
        ))
      case AcquisitionTiming.Explicit(coordinates, units) =>
        CanonicalValue.obj(Vector(
          CanonicalKey.unsafe("id") ->
            CanonicalValue.string(s"$acquisition:time"),
          CanonicalKey.unsafe("kind") ->
            CanonicalValue.string("explicit"),
          CanonicalKey.unsafe("units") ->
            CanonicalValue.string(units.id),
          CanonicalKey.unsafe("count") ->
            CanonicalValue.int64(coordinates.length.toLong),
          CanonicalKey.unsafe("coordinates") ->
            CanonicalValue.array(coordinates.map(CanonicalValue.float64))
        ))

  private def geometryDigest(
      manifest: NeuroArchiveManifest
  ): String =
    val fields =
      manifest.geometry.spatialShape.toVector.map(_.toString) ++
        manifest.geometry.voxelToWorld.toVector.map(value =>
          java.lang.Long.toHexString(
            java.lang.Double.doubleToRawLongBits(value)
          )
        ) :+
        manifest.geometry.units.id
    Sha256.digestUtf8(fields.mkString("|")).value

  private def sequence(
      values: Vector[Either[ArchiveError, CanonicalValue]]
  ): Either[ArchiveError, Vector[CanonicalValue]] =
    val result = Vector.newBuilder[CanonicalValue]
    var index = 0
    while index < values.length do
      values(index) match
        case Left(error) =>
          return Left(error)
        case Right(value) =>
          result += value
      index += 1
    Right(result.result())
