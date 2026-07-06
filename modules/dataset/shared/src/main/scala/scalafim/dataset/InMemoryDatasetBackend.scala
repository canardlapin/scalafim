package scalafim.dataset

import scalafim.image.{DMat, Mask, NeuroSpace}

final case class InMemoryDatasetBackend(
    id: DatasetId,
    data: DMat,
    space: NeuroSpace,
    metadata: DatasetMetadata = DatasetMetadata.Empty
) extends DatasetBackend:
  override val shape: DatasetShape = DatasetShape(space, data.rows)
  require(data.cols == shape.spatialSize, "data columns must match spatial voxel count")

  override val mask: Mask.MaskVol = Mask.all(space)

  override def read(selection: DataSelection = DataSelection.All): FmriSeries =
    val resolved = selection.resolve(shape)
    val rows =
      resolved.timepoints.map { r =>
        resolved.voxels.map(c => data(r, c))
      }
    FmriSeries(
      data = DMat.fromRows(rows),
      voxelIndices = resolved.voxels,
      timepoints = resolved.timepoints,
      shape = shape,
      metadata = metadata
    )
