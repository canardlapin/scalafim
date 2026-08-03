package scalafim.archive.lna

import scalafim.archive.{
  ArchiveError,
  ArchiveFormatKey,
  ArchiveManifest,
  CanonicalKey,
  CanonicalValue,
  IntegrityManifest,
  ObjectKey,
  ObjectTypeId,
  PayloadDescriptor,
  PayloadId,
  PayloadRoleId,
  PersistedRepresentation,
  RepresentationKey,
  RepresentationMetadata,
  ScalarTypeId
}

object LnaPayloadRole:
  private val Namespace =
    "org.scalafim.lna"

  def fromDatasetRole(
      role: DatasetRole
  ): PayloadRoleId =
    PayloadRoleId
      .from(Namespace, canonicalName(role.value))
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )

  val TemporalBasis: PayloadRoleId =
    fromDatasetRole(DatasetRole.TemporalBasis)

  val Loadings: PayloadRoleId =
    fromDatasetRole(DatasetRole.Loadings)

  val SampleOffset: PayloadRoleId =
    fromDatasetRole(DatasetRole.SampleOffset)

  private def canonicalName(value: String): String =
    value
      .toLowerCase
      .map(character =>
        if character.isLetterOrDigit then character else '-'
      )

object LnaArchiveManifestAdapter:
  val Format: ArchiveFormatKey =
    ArchiveFormatKey.unsafe("lna-hdf5@2")

  val ObjectType: ObjectTypeId =
    ObjectTypeId.unsafe("org.scalafim/fmri-response")

  val PipelineRepresentation: RepresentationKey =
    RepresentationKey.unsafe("org.scalafim/lna-pipeline@2")

  val TemporalDctRepresentation: RepresentationKey =
    RepresentationKey.unsafe("org.scalafim/temporal-dct@1")

  def adapt(
      manifest: LnaManifest
  ): Either[ArchiveError, ArchiveManifest] =
    for
      attributes <- manifestAttributes(manifest)
      payloads <- traverse(manifest.datasets): reference =>
        PayloadDescriptor.from(
          PayloadId.unsafe(reference.path.value),
          LnaPayloadRole.fromDatasetRole(reference.role),
          ScalarTypeId.unsafe(
            reference.dtype.fold("unknown")(_.toString.toLowerCase)
          ),
          reference.dims.map(_.toLong)
        )
      representation = persistedRepresentation(manifest)
      key <- ObjectKey.from(
        ObjectType,
        schemaMajor = 2,
        Some(representation.key)
      )
      translated <- ArchiveManifest.from(
        Format,
        key,
        Some(representation),
        attributes,
        payloads,
        IntegrityManifest.Empty
      )
    yield translated

  private def persistedRepresentation(
      manifest: LnaManifest
  ): PersistedRepresentation =
    manifest.header
      .get(RepresentationMetadata.DescriptorHeader) match
      case Some(descriptor) =>
        PersistedRepresentation(
          TemporalDctRepresentation,
          CanonicalValue.string(descriptor),
          manifest.header
            .get(RepresentationMetadata.OutputSchemaHeader)
            .map(CanonicalValue.string)
            .getOrElse(CanonicalValue.Null)
        )
      case None =>
        PersistedRepresentation(
          PipelineRepresentation,
          CanonicalValue.string(LnaManifestCodec.render(manifest)),
          CanonicalValue.Null
        )

  private def manifestAttributes(
      manifest: LnaManifest
  ): Either[ArchiveError, CanonicalValue] =
    for
      header <- CanonicalValue.Object.from:
        manifest.header.iterator
          .map: (key, value) =>
            CanonicalKey.unsafe(key) -> CanonicalValue.string(value)
          .toVector
      transforms = CanonicalValue.array(
        manifest.requiredTransforms.map(kind =>
          CanonicalValue.string(kind.value)
        )
      )
      representation =
        manifest.header
          .get(RepresentationMetadata.DescriptorHeader)
          .map(value =>
            RepresentationMetadata.DescriptorAttribute ->
              CanonicalValue.string(value)
          )
      outputSchema =
        manifest.header
          .get(RepresentationMetadata.OutputSchemaHeader)
          .map(value =>
            RepresentationMetadata.OutputSchemaAttribute ->
              CanonicalValue.string(value)
          )
      attributes <- CanonicalValue.obj(
        Vector(
          CanonicalKey.unsafe("lna-version") ->
            CanonicalValue.string(manifest.version.id),
          CanonicalKey.unsafe("required-transforms") -> transforms,
          CanonicalKey.unsafe("header") -> header
        ) ++ representation ++ outputSchema
      )
    yield attributes

  private def traverse[A, B](
      values: Iterable[A]
  )(
      operation: A => Either[ArchiveError, B]
  ): Either[ArchiveError, Vector[B]] =
    val result = Vector.newBuilder[B]
    val iterator = values.iterator
    while iterator.hasNext do
      operation(iterator.next()) match
        case Left(error) =>
          return Left(error)
        case Right(value) =>
          result += value
    Right(result.result())
