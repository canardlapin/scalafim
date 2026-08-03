package scalafim.archive.lna

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.image.NeuroSpace

object LnaTemporalDct:
  def archive(
      response: LnaExplicitLatent.Response,
      params: TemporalDctParams,
      space: NeuroSpace,
      runLabel: RunLabel = RunLabel.indexed(0),
      creator: String = "scalafim-archive"
  ): Either[ArchiveError, LnaArchive] =
    validateTemporalDct(response, params).flatMap { valid =>
      val stamped =
        valid.copy(metadata = valid.metadata ++ temporalMetadata(valid, params))

      LnaExplicitLatent
        .archive(stamped, space, runLabel, creator)
        .flatMap(rewriteTemporalDescriptor(_, stamped, params))
    }

  private def validateTemporalDct(
      response: LnaExplicitLatent.Response,
      params: TemporalDctParams
  ): Either[ArchiveError, LnaExplicitLatent.Response] =
    if params.components > response.timepoints then
      Left(ArchiveError.ShapeMismatch(s"temporal DCT components ${params.components} exceed timepoints ${response.timepoints}"))
    else if response.coefficients != params.components then
      Left(
        ArchiveError.ShapeMismatch(
          s"temporal DCT components ${params.components} do not match response coefficients ${response.coefficients}"
        )
      )
    else Right(response)

  private def rewriteTemporalDescriptor(
      archive: LnaArchive,
      response: LnaExplicitLatent.Response,
      params: TemporalDctParams
  ): Either[ArchiveError, LnaArchive] =
    archive.manifest.transforms match
      case Vector(basisDesc, embedDesc) if LnaExplicitLatent.isExplicitEmbed(embedDesc) =>
        val temporalDesc =
          basisDesc.copy(
            name = "00_temporal_dct.json",
            kind = TransformKind.Temporal,
            params = TransformParams.TemporalDct(params),
            inputs = Vector("time"),
            outputs = Vector("temporal_basis")
          )

        val temporalEmbed =
          embedDesc.copy(
            name = "01_temporal_dct_embed.json",
            inputs = Vector("temporal_basis", "loadings"),
            outputs = Vector("latent_response")
          )

        archive
          .copy(
            manifest = archive.manifest.copy(
              requiredTransforms = Vector(TransformKind.Temporal, TransformKind.Embed),
              transforms = Vector(temporalDesc, temporalEmbed),
              header = archive.manifest.header ++ temporalHeader(response, params)
            )
          )
          .validate
      case _ =>
        Left(ArchiveError.InvalidArchive("temporal DCT archive expected explicit latent basis/embed descriptors"))

  private def temporalMetadata(
      response: LnaExplicitLatent.Response,
      params: TemporalDctParams
  ): Map[String, String] =
    Map(
      "family" -> "time_dct",
      "basis" -> "dct",
      "timepoints" -> response.timepoints.toString,
      "components" -> params.components.toString,
      "norm" -> params.norm.value,
      "center" -> params.center.toString,
      "ridge" -> params.ridge.toString
    )

  private def temporalHeader(
      response: LnaExplicitLatent.Response,
      params: TemporalDctParams
  ): Map[String, String] =
    Map(
      "temporal.basis" -> "dct",
      "temporal.timepoints" -> response.timepoints.toString,
      "temporal.components" -> params.components.toString,
      "temporal.norm" -> params.norm.value,
      "temporal.center" -> params.center.toString,
      "temporal.ridge" -> params.ridge.toString
    )
