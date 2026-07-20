package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.Mask

trait DatasetBackend:
  def id: DatasetId
  def shape: DatasetShape
  def mask: Mask.MaskVol
  def voxelDomain: VoxelDomain =
    VoxelDomain.fromMask(mask, shape).fold(error => throw new IllegalArgumentException(error.message), identity)
  def metadata: DatasetMetadata
  def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries]
  def read(selection: DataSelection = DataSelection.All): FmriSeries =
    readEither(selection).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class FmriDataset(
    backend: DatasetBackend,
    samplingFrame: SamplingFrame,
    events: DatasetEvents = DatasetEvents.Empty,
    timeAxis: DatasetTimeAxis
):
  require(samplingFrame.blockLens.sum == backend.shape.timepoints, "sampling frame rows must match dataset timepoints")
  require(timeAxis.timepoints == backend.shape.timepoints, "time axis rows must match dataset timepoints")
  require(timeAxis.blockLengths == samplingFrame.blockLens, "time axis blocks must match sampling frame block lengths")
  events.validateAgainst(timeAxis).fold(error => throw new IllegalArgumentException(error.message), _ => ())

  def id: DatasetId = backend.id
  def shape: DatasetShape = backend.shape
  def metadata: DatasetMetadata = backend.metadata
  def seriesEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    backend.readEither(selection)
  def series(selection: DataSelection = DataSelection.All): FmriSeries =
    seriesEither(selection).fold(error => throw new IllegalArgumentException(error.message), identity)
  def runPartitionsFor(timepoints: Vector[TimepointIndex]): Either[DatasetError, Vector[DatasetRunPartition]] =
    timeAxis.partitions(timepoints)
  def runPartitionsEither(selection: DataSelection = DataSelection.All): Either[DatasetError, Vector[DatasetRunPartition]] =
    for
      resolved <- selection.resolveEither(shape, backend.voxelDomain)
      partitions <- timeAxis.partitions(resolved.timepointIndices)
    yield partitions
  def runPartitions(selection: DataSelection = DataSelection.All): Vector[DatasetRunPartition] =
    runPartitionsEither(selection).fold(error => throw new IllegalArgumentException(error.message), identity)

object FmriDataset:
  def apply(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame
  ): FmriDataset =
    FmriDataset(backend, samplingFrame, DatasetEvents.Empty)

  def apply(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      events: DatasetEvents
  ): FmriDataset =
    new FmriDataset(
      backend = backend,
      samplingFrame = samplingFrame,
      events = events,
      timeAxis = DatasetTimeAxis.unsafe(samplingFrame)
    )

  def apply(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      timeAxis: DatasetTimeAxis
  ): FmriDataset =
    new FmriDataset(
      backend = backend,
      samplingFrame = samplingFrame,
      events = DatasetEvents.Empty,
      timeAxis = timeAxis
    )
