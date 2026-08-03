package scalafim.dataset

import scalafim.image.{DMat, GridCompatibility, Mask, NeuroSpace}
import scalafim.latent.{LatentResponse, LatentSelection}
import scalafim.response.OperationId

final class LatentResponseDatasetBackend private (
    val id: DatasetId,
    val response: LatentResponse,
    val space: NeuroSpace,
    val mask: Mask.MaskVol,
    val metadata: DatasetMetadata,
    override val shape: DatasetShape,
    private val sampleMap: VoxelSampleMap,
    override val voxelDomain: VoxelDomain
) extends DatasetBackend:

  override def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    for
      resolved <- selection.resolveEither(acquisitionDomain)
      samples <- sampleMap.samplesFor(resolved.voxelIndexValues)
      decoded <- response
        .reconstruct(LatentSelection(timepoints = Some(resolved.timepoints), samples = Some(samples)))
        .left
        .map(error =>
          DatasetError.AdapterFailure(
            OperationId.unsafe("latent-response"),
            error.message
          )
        )
      series <- FmriSeries.make(
        data = DatasetMatrices.fromGale(decoded),
        voxelIndices = resolved.voxelIndexValues,
        timepoints = resolved.timepointIndices,
        shape = shape,
        metadata = metadata
      )
    yield series

object LatentResponseDatasetBackend:
  def make(
      id: DatasetId,
      response: LatentResponse,
      space: NeuroSpace,
      mask: Mask.MaskVol,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, LatentResponseDatasetBackend] =
    for
      shape <- DatasetShape.make(space, response.shape.timepoints)
      _ <- validateMaskSpace(mask, shape)
      sampleMap <- VoxelSampleMap.fromMask(mask, response.shape.samples)
      voxelDomain <- VoxelDomain.active(sampleMap.spatialSize, sampleMap.sampleVoxels)
    yield new LatentResponseDatasetBackend(
      id,
      response,
      space,
      mask,
      metadata,
      shape,
      sampleMap,
      voxelDomain
    )

  def make(
      id: DatasetId,
      response: LatentResponse,
      space: NeuroSpace,
      metadata: DatasetMetadata
  ): Either[DatasetError, LatentResponseDatasetBackend] =
    make(
      id = id,
      response = response,
      space = space,
      mask = Mask.all(space),
      metadata = metadata
    )

  def make(
      id: DatasetId,
      response: LatentResponse,
      space: NeuroSpace
  ): Either[DatasetError, LatentResponseDatasetBackend] =
    make(id, response, space, DatasetMetadata.Empty)

  def unsafe(
      id: DatasetId,
      response: LatentResponse,
      space: NeuroSpace,
      mask: Mask.MaskVol,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): LatentResponseDatasetBackend =
    make(id, response, space, mask, metadata)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafe(
      id: DatasetId,
      response: LatentResponse,
      space: NeuroSpace,
      metadata: DatasetMetadata
  ): LatentResponseDatasetBackend =
    make(id, response, space, metadata)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafe(
      id: DatasetId,
      response: LatentResponse,
      space: NeuroSpace
  ): LatentResponseDatasetBackend =
    make(id, response, space)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

private def validateMaskSpace(
    mask: Mask.MaskVol,
    shape: DatasetShape
): Either[DatasetError, Unit] =
  GridCompatibility
    .spatial(shape.space, mask.space)
    .left
    .map(error => DatasetError.ShapeMismatch(error.message))
