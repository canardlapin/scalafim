package scalafim.dataset

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{LnaArchive, LnaPipeline}
import gale.linalg.DMat
import scalafim.image.Mask
import scalafim.latent.{
  LatentArchivePlan,
  LatentArchiveRegistry,
  LatentSelection
}
import scalafim.response.OperationId

final class LatentArchiveDatasetBackend private (
    val id: DatasetId,
    val archive: LnaArchive,
    val run: RunLabel,
    val registry: LatentArchiveRegistry,
    val metadata: DatasetMetadata,
    override val shape: DatasetShape,
    val mask: Mask.MaskVol,
    override val voxelDomain: VoxelDomain
) extends DatasetBackend:

  private lazy val denseEither: Either[DatasetError, DMat] =
    LnaPipeline
      .reconstruct(archive, run)
      .left
      .map(error =>
        DatasetError.AdapterFailure(
          OperationId.unsafe("lna-archive"),
          error.message
        )
      )

  private lazy val latentPlanEither: Either[DatasetError, Option[LatentArchivePlan]] =
    registry
      .maybeOpenPlan(archive, run)
      .left
      .map(error =>
        DatasetError.AdapterFailure(
          OperationId.unsafe("lna-archive"),
          error.message
        )
      )

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
          .map(error =>
            DatasetError.AdapterFailure(
              OperationId.unsafe("lna-archive"),
              error.message
            )
          )
          .flatMap { response =>
            response
              .reconstruct(LatentSelection(timepoints = Some(resolved.timepoints), samples = Some(resolved.voxels)))
              .left
              .map(error =>
                DatasetError.AdapterFailure(
                  OperationId.unsafe("latent-representation"),
                  error.message
                )
              )
          }
      case None =>
        denseEither.map { dense =>
          DMat.tabulate(resolved.timepoints.length, resolved.voxels.length): (row, column) =>
            dense(resolved.timepoints(row), resolved.voxels(column))
        }
    }

object LatentArchiveDatasetBackend:
  def make(
      id: DatasetId,
      archive: LnaArchive,
      run: RunLabel,
      registry: LatentArchiveRegistry,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): Either[DatasetError, LatentArchiveDatasetBackend] =
    for
      runInfo <- archive
        .run(run)
        .toRight(DatasetError.AdapterFailure(
          OperationId.unsafe("lna-archive"),
          ArchiveError.InvalidArchive(s"run '${run.value}' not found").message
        ))
      shape <- DatasetShape.make(runInfo.shape.space, runInfo.shape.timepoints)
      voxelDomain <- VoxelDomain.full(shape)
    yield new LatentArchiveDatasetBackend(
      id = id,
      archive = archive,
      run = run,
      registry = registry,
      metadata = metadata,
      shape = shape,
      mask = Mask.all(shape.space),
      voxelDomain = voxelDomain
    )

  def unsafe(
      id: DatasetId,
      archive: LnaArchive,
      run: RunLabel,
      registry: LatentArchiveRegistry,
      metadata: DatasetMetadata = DatasetMetadata.Empty
  ): LatentArchiveDatasetBackend =
    make(id, archive, run, registry, metadata)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
