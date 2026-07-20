package scalafim.dataset

import scalafim.image.{DMat, Mask, NeuroSpace}

final case class InMemoryDatasetBackend(
    id: DatasetId,
    data: DMat,
    space: NeuroSpace,
    metadata: DatasetMetadata = DatasetMetadata.Empty
) extends DatasetBackend:
  override val shape: DatasetShape = DatasetShape.unsafe(space, data.rows)
  require(data.cols == shape.spatialSize, "data columns must match spatial voxel count")

  override val mask: Mask.MaskVol = Mask.all(space)
  override val voxelDomain: VoxelDomain = VoxelDomain.fullUnsafe(shape)

  override def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    val resolvedEither = selection.resolveEither(shape, voxelDomain)
    resolvedEither.flatMap { resolved =>
      val rows =
        resolved.timepoints.map { r =>
          resolved.voxels.map(c => data(r, c))
        }
      FmriSeries.make(
        data = DMat.fromRows(rows),
        voxelIndices = resolved.voxelIndexValues,
        timepoints = resolved.timepointIndices,
        shape = shape,
        metadata = metadata
      )
    }
