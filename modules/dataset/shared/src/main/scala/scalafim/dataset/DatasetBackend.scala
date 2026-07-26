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
  lazy val acquisitionDomain: DatasetAcquisitionDomain =
    DatasetAcquisitionDomain
      .semantic(id, shape, voxelDomain)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
  def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries]
  def read(selection: DataSelection = DataSelection.All): FmriSeries =
    readEither(selection).fold(error => throw new IllegalArgumentException(error.message), identity)

final class FmriDataset private (
    val backend: DatasetBackend,
    val samplingFrame: SamplingFrame,
    val events: DatasetEvents,
    val timeAxis: DatasetTimeAxis
):
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
      resolved <- selection.resolveEither(backend.acquisitionDomain)
      partitions <- timeAxis.partitions(resolved.timepointIndices)
    yield partitions
  def runPartitions(selection: DataSelection = DataSelection.All): Vector[DatasetRunPartition] =
    runPartitionsEither(selection).fold(error => throw new IllegalArgumentException(error.message), identity)

object FmriDataset:
  def open(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runIds: Vector[RunId],
      events: DatasetEvents = DatasetEvents.Empty
  ): Either[DatasetError, FmriDataset] =
    for
      timeAxis <- DatasetTimeAxis.fromSamplingFrame(samplingFrame, runIds)
      _ <-
        if backend.shape.timepoints == timeAxis.timepoints then Right(())
        else
          Left(
            DatasetError.ShapeMismatch(
              s"sampling frame has ${timeAxis.timepoints} timepoints but backend has ${backend.shape.timepoints}"
            )
          )
      _ <- events.validateAgainst(timeAxis)
    yield new FmriDataset(backend, samplingFrame, events, timeAxis)

  def open(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runId: RunId,
      events: DatasetEvents
  ): Either[DatasetError, FmriDataset] =
    open(backend, samplingFrame, Vector(runId), events)

  def open(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runId: RunId
  ): Either[DatasetError, FmriDataset] =
    open(backend, samplingFrame, Vector(runId))

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runIds: Vector[RunId],
      events: DatasetEvents = DatasetEvents.Empty
  ): FmriDataset =
    open(backend, samplingFrame, runIds, events)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      events: DatasetEvents
  ): FmriDataset =
    DatasetTimeAxis
      .fromSamplingFrame(samplingFrame)
      .flatMap(timeAxis => open(backend, samplingFrame, timeAxis.runIds, events))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame
  ): FmriDataset =
    unsafe(backend, samplingFrame, DatasetEvents.Empty)

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runId: RunId,
      events: DatasetEvents
  ): FmriDataset =
    unsafe(backend, samplingFrame, Vector(runId), events)

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runId: RunId
  ): FmriDataset =
    unsafe(backend, samplingFrame, Vector(runId))
