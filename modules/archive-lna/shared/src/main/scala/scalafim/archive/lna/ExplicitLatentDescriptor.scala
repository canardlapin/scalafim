package scalafim.archive.lna

object ExplicitLatentDescriptor:
  val MetadataKindKey: String =
    "lna.response.kind"

  val MetadataKindValue: String =
    "explicit_latent"

  def matches(
      descriptor: TransformDescriptor
  ): Boolean =
    descriptor.kind == TransformKind.Embed &&
      (
        descriptor.datasets.exists(_.role == DatasetRole.Loadings) ||
          hasMetadataMarker(descriptor)
      )

  private def hasMetadataMarker(
      descriptor: TransformDescriptor
  ): Boolean =
    descriptor.params match
      case params: TransformParams.Embed =>
        params.metadata
          .get(MetadataKindKey)
          .contains(MetadataKindValue)
      case _ =>
        false
