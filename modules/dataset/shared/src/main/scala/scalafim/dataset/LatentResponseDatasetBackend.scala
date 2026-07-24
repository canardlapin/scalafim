package scalafim.dataset

import scalafim.image.{DMat, GridCompatibility, Mask, NeuroSpace}
import scalafim.latent.{LatentResponse, LatentSelection}

final case class LatentResponseDatasetBackend(
    id: DatasetId,
    response: LatentResponse,
    space: NeuroSpace,
    mask: Mask.MaskVol,
    metadata: DatasetMetadata = DatasetMetadata.Empty
) extends DatasetBackend:

  private lazy val shapeEither: Either[DatasetError, DatasetShape] =
    DatasetShape.make(space, response.shape.timepoints)

  override lazy val shape: DatasetShape =
    shapeEither.fold(error => throw new IllegalArgumentException(error.message), identity)

  private lazy val sampleMapEither: Either[DatasetError, VoxelSampleMap] =
    for
      checkedShape <- shapeEither
      _ <- validateMaskSpace(mask, checkedShape)
      sampleMap <- VoxelSampleMap.fromMask(mask, response.shape.samples)
    yield sampleMap

  private val sampleMap: VoxelSampleMap =
    sampleMapEither.fold(error => throw new IllegalArgumentException(error.message), identity)

  override lazy val voxelDomain: VoxelDomain =
    VoxelDomain.activeUnsafe(sampleMap.spatialSize, sampleMap.sampleVoxels)

  override def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    for
      checkedShape <- shapeEither
      resolved <- selection.resolveEither(checkedShape, voxelDomain)
      samples <- sampleMap.samplesFor(resolved.voxelIndexValues)
      decoded <- response
        .reconstruct(LatentSelection(timepoints = Some(resolved.timepoints), samples = Some(samples)))
        .left
        .map(DatasetError.LatentFailure.apply)
      series <- FmriSeries.make(
        data = DatasetMatrices.fromGale(decoded),
        voxelIndices = resolved.voxelIndexValues,
        timepoints = resolved.timepointIndices,
        shape = checkedShape,
        metadata = metadata
      )
    yield series

object LatentResponseDatasetBackend:
  def apply(
      id: DatasetId,
      response: LatentResponse,
      space: NeuroSpace,
      metadata: DatasetMetadata
  ): LatentResponseDatasetBackend =
    LatentResponseDatasetBackend(
      id = id,
      response = response,
      space = space,
      mask = Mask.all(space),
      metadata = metadata
    )

  def apply(
      id: DatasetId,
      response: LatentResponse,
      space: NeuroSpace
  ): LatentResponseDatasetBackend =
    LatentResponseDatasetBackend(id, response, space, DatasetMetadata.Empty)

private def validateMaskSpace(
    mask: Mask.MaskVol,
    shape: DatasetShape
): Either[DatasetError, Unit] =
  GridCompatibility
    .spatial(shape.space, mask.space)
    .left
    .map(error => DatasetError.ShapeMismatch(error.message))
