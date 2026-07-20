package scalafim.dataset

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{LnaArchive, LnaPipeline}
import scalafim.image.{DMat, Mask}
import scalafim.latent.{LatentArchiveCodec, LatentArchivePlan, LatentSelection}

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

  private lazy val latentPlanEither: Either[DatasetError, Option[LatentArchivePlan]] =
    LatentArchiveCodec
      .maybeOpenPlan(archive, run)
      .left
      .map(DatasetError.ArchiveFailure.apply)

  private lazy val shapeEither: Either[DatasetError, DatasetShape] =
    runInfoEither.flatMap(runInfo => DatasetShape.make(runInfo.shape.space, runInfo.shape.timepoints))

  override lazy val shape: DatasetShape =
    shapeEither.fold(error => throw new IllegalArgumentException(error.message), identity)

  override lazy val mask: Mask.MaskVol =
    Mask.all(shape.space)

  override lazy val voxelDomain: VoxelDomain =
    VoxelDomain.fullUnsafe(shape)

  override def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    for
      checkedShape <- shapeEither
      resolved <- selection.resolveEither(checkedShape, voxelDomain)
      data <- selectedData(resolved)
      series <- FmriSeries.make(
        data = data,
        voxelIndices = resolved.voxelIndexValues,
        timepoints = resolved.timepointIndices,
        shape = checkedShape,
        metadata = metadata
      )
    yield series

  private def selectedData(resolved: ResolvedDataSelection): Either[DatasetError, DMat] =
    latentPlanEither.flatMap {
      case Some(plan) =>
        plan
          .capability
          .selectionResponse
          .left
          .map(DatasetError.ArchiveFailure.apply)
          .flatMap { response =>
            response
              .reconstruct(LatentSelection(timepoints = Some(resolved.timepoints), samples = Some(resolved.voxels)))
              .left
              .map(DatasetError.LatentFailure.apply)
              .map(DatasetMatrices.fromGale)
          }
      case None =>
        denseEither.map { dense =>
          DMat.fromRows(
            resolved.timepoints.map { r =>
              resolved.voxels.map(c => dense(r, c))
            }
          )
        }
    }
