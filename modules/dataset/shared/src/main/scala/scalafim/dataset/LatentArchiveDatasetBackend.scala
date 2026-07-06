package scalafim.dataset

import scalafim.archive.RunLabel
import scalafim.archive.lna.{LnaArchive, LnaPipeline}
import scalafim.image.{DMat, Mask}

final case class LatentArchiveDatasetBackend(
    id: DatasetId,
    archive: LnaArchive,
    run: RunLabel = RunLabel.indexed(0),
    metadata: DatasetMetadata = DatasetMetadata.Empty
) extends DatasetBackend:

  private lazy val runInfo =
    archive
      .run(run)
      .getOrElse(throw new IllegalArgumentException(s"archive run '${run.value}' not found"))

  private lazy val dense: DMat =
    LnaPipeline
      .reconstruct(archive, run)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  override lazy val shape: DatasetShape =
    DatasetShape(runInfo.shape.space, runInfo.shape.timepoints)

  override lazy val mask: Mask.MaskVol =
    Mask.all(runInfo.shape.space)

  override def read(selection: DataSelection = DataSelection.All): FmriSeries =
    val resolved = selection.resolve(shape)
    val rows =
      resolved.timepoints.map { r =>
        resolved.voxels.map(c => dense(r, c))
      }
    FmriSeries(
      data = DMat.fromRows(rows),
      voxelIndices = resolved.voxels,
      timepoints = resolved.timepoints,
      shape = shape,
      metadata = metadata
    )
