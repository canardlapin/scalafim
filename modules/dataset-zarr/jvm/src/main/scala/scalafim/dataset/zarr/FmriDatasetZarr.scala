package scalafim.dataset.zarr

import java.nio.file.Path
import scalafim.archive.zarr.NeuroArchiveZarr
import scalafim.dataset.*
import scalafim.image.{Mask, NArrayUtil}
import scalafim.zarr.{JvmCodecRuntime, JvmFileStore, ReadLimits}

final case class ZarrDatasetOpenOptions(
    precision: SamplingPrecisionPolicy = SamplingPrecisionPolicy.Default,
    voxelDomain: Option[VoxelDomain] = None,
    metadata: DatasetMetadata = DatasetMetadata.Empty,
    readLimits: ReadLimits = ReadLimits()
)

extension (factory: FmriDataset.type)
  def openZarr(
      root: Path,
      run: RunId,
      options: ZarrDatasetOpenOptions = ZarrDatasetOpenOptions()
  ): Either[DatasetError, FmriDataset] =
    for
      store <- JvmFileStore.open(root).left.map(DatasetError.StorageFailure.apply)
      opened <- NeuroArchiveZarr
        .openCanonical(store, runtime = JvmCodecRuntime.portable)
        .left
        .map(error => DatasetError.StorageFailure(error.message))
      samplingFrame <- CanonicalBoldSampling.refine(
        opened.canonical.manifest.timing,
        options.precision
      )
      metadata = options.metadata.withProvenance(provenance(opened, options.precision))
      source <- ZarrResponseBlockSource.open(
        opened = opened,
        voxelDomain = options.voxelDomain,
        metadata = metadata,
        readLimits = options.readLimits
      )
      mask = maskFor(source)
      backend <- ResponseBlockDatasetBackend.make(
        id = DatasetId(opened.canonical.manifest.acquisitionId.value),
        source = source,
        mask = mask,
        metadata = metadata
      )
      dataset <- FmriDataset.open(
        backend = backend,
        samplingFrame = samplingFrame,
        runId = run
      )
    yield dataset

private def maskFor(source: ZarrResponseBlockSource): Mask.MaskVol =
  if source.voxelDomain.isFullSpatial then Mask.all(source.shape.space)
  else
    Mask.fromIndices(
      source.shape.space,
      NArrayUtil.fromArray(source.voxelDomain.indices.toArray),
      label = "neuroarchive-zarr-domain"
    )

private def provenance(
    opened: scalafim.archive.zarr.OpenedCanonicalBold,
    precision: SamplingPrecisionPolicy
): NeuroArchiveZarrProvenance =
  val manifest = opened.canonical.manifest
  NeuroArchiveZarrProvenance(
    acquisitionId = manifest.acquisitionId.value,
    payloadId = manifest.payloadId.value,
    contentRevision = manifest.contentRevision.value,
    logicalPayloadHash = manifest.logicalPayloadHash.value,
    precisionPolicy = precision
  )
