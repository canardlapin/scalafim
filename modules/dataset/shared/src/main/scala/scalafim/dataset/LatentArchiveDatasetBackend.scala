package scalafim.dataset

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{LnaArchive, LnaPipeline}
import scalafim.image.{DMat, Mask}

final case class LatentArchiveDatasetBackend(
    id: DatasetId,
    archive: LnaArchive,
    run: RunLabel = RunLabel.indexed(0),
    metadata: DatasetMetadata = DatasetMetadata.Empty
) extends DatasetBackend:

  private lazy val runInfoEither =
    archive
      .run(run)
      .toRight(DatasetError.ArchiveFailure(ArchiveError.InvalidArchive(s"run '${run.value}' not found")))

  private lazy val denseEither: Either[DatasetError, DMat] =
    LnaPipeline
      .reconstruct(archive, run)
      .left
      .map(DatasetError.ArchiveFailure.apply)

  private lazy val shapeEither: Either[DatasetError, DatasetShape] =
    runInfoEither.flatMap(runInfo => DatasetShape.make(runInfo.shape.space, runInfo.shape.timepoints))

  override lazy val shape: DatasetShape =
    shapeEither.fold(error => throw new IllegalArgumentException(error.message), identity)

  override lazy val mask: Mask.MaskVol =
    Mask.all(shape.space)

  override def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    for
      checkedShape <- shapeEither
      dense <- denseEither
      resolved <- selection.resolveEither(checkedShape)
      series <- FmriSeries.make(
        data = DMat.fromRows(
          resolved.timepoints.map { r =>
            resolved.voxels.map(c => dense(r, c))
          }
        ),
        voxelIndices = resolved.voxelIndexValues,
        timepoints = resolved.timepointIndices,
        shape = checkedShape,
        metadata = metadata
      )
    yield series
