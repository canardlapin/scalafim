package scalafim.dataset

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{LnaArchive, LnaPipeline}
import scalafim.image.{DMat, Mask}
import scalafim.latent.{LatentArchiveCodec, LatentArchivePlan, LatentSelection}

final class LatentArchiveDatasetBackend private (
    val id: DatasetId,
    val archive: LnaArchive,
    val run: RunLabel,
    val metadata: DatasetMetadata,
    override val shape: DatasetShape,
    val mask: Mask.MaskVol,
    override val voxelDomain: VoxelDomain
) extends DatasetBackend:

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

  override def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    for
      resolved <- selection.resolveEither(acquisitionDomain)
      data <- selectedData(resolved)
      series <- FmriSeries.make(
        data = data,
        voxelIndices = resolved.voxelIndexValues,
        timepoints = resolved.timepointIndices,
        shape = shape,
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

object LatentArchiveDatasetBackend:
  def make(
      id: DatasetId,
      archive: LnaArchive,
      run: RunLabel,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, LatentArchiveDatasetBackend] =
    for
      runInfo <- archive
        .run(run)
        .toRight(DatasetError.ArchiveFailure(ArchiveError.InvalidArchive(s"run '${run.value}' not found")))
      shape <- DatasetShape.make(runInfo.shape.space, runInfo.shape.timepoints)
      voxelDomain <- VoxelDomain.full(shape)
    yield new LatentArchiveDatasetBackend(
      id = id,
      archive = archive,
      run = run,
      metadata = metadata,
      shape = shape,
      mask = Mask.all(shape.space),
      voxelDomain = voxelDomain
    )

  def unsafe(
      id: DatasetId,
      archive: LnaArchive,
      run: RunLabel,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): LatentArchiveDatasetBackend =
    make(id, archive, run, metadata)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
