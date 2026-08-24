package scalafim.dataset

import gale.linalg.DMat
import scalafim.image.{Mask, SomeSampleSpace}

final case class InMemoryDatasetBackend(
    id: DatasetId,
    data: DMat,
    space: SomeSampleSpace,
    metadata: DatasetMetadata = DatasetMetadata.Empty
) extends DatasetBackend:
  override val shape: DatasetShape = DatasetShape.unsafe(space, data.rows)
  require(data.cols == shape.spatialSize, "data columns must match spatial voxel count")

  override val mask: Mask.MaskVol = Mask.all(space)
  override val voxelDomain: VoxelDomain = VoxelDomain.fullUnsafe(shape)

  override def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    val resolvedEither = selection.resolveEither(acquisitionDomain)
    resolvedEither.flatMap { resolved =>
      FmriSeries.make(
        data = DMat.tabulate(resolved.timepoints.length, resolved.voxels.length): (row, column) =>
          data(resolved.timepoints(row), resolved.voxels(column)),
        voxelIndices = resolved.voxelIndexValues,
        timepoints = resolved.timepointIndices,
        shape = shape,
        metadata = metadata
      )
    }
