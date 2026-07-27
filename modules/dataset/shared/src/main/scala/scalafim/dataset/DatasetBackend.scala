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

trait DatasetSeriesReader:
  def dataset: FmriDataset

  def seriesEither(
      selection: DataSelection = DataSelection.All
  ): Either[DatasetError, FmriSeries]

  final def series(
      selection: DataSelection = DataSelection.All
  ): FmriSeries =
    seriesEither(selection)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

class FmriDataset private[dataset] (
    val id: DatasetId,
    val shape: DatasetShape,
    val voxelDomain: VoxelDomain,
    val metadata: DatasetMetadata,
    val samplingFrame: SamplingFrame,
    val events: DatasetEvents,
    val timeAxis: DatasetTimeAxis
):
  lazy val acquisitionDomain: DatasetAcquisitionDomain =
    DatasetAcquisitionDomain
      .semantic(id, shape, voxelDomain)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def resolve(
      selection: DataSelection = DataSelection.All
  ): Either[DatasetError, ResolvedDataSelection] =
    selection.resolveEither(acquisitionDomain)

  def runPartitionsFor(timepoints: Vector[TimepointIndex]): Either[DatasetError, Vector[DatasetRunPartition]] =
    timeAxis.partitions(timepoints)

  def runPartitionsEither(selection: DataSelection = DataSelection.All): Either[DatasetError, Vector[DatasetRunPartition]] =
    for
      resolved <- resolve(selection)
      partitions <- timeAxis.partitions(resolved.timepointIndices)
    yield partitions

  def runPartitions(selection: DataSelection = DataSelection.All): Vector[DatasetRunPartition] =
    runPartitionsEither(selection).fold(error => throw new IllegalArgumentException(error.message), identity)

object FmriDataset:
  extension (dataset: FmriDataset)
    /** Compatibility only; new code names the synchronous or opened reader. */
    def seriesEither(
        selection: DataSelection = DataSelection.All
    ): Either[DatasetError, FmriSeries] =
      SynchronousFmriDataset
        .readerFor(dataset)
        .flatMap(_.seriesEither(selection))

    /** Compatibility only; new code names the synchronous or opened reader. */
    def series(
        selection: DataSelection = DataSelection.All
    ): FmriSeries =
      seriesEither(selection)
        .fold(error => throw new IllegalArgumentException(error.message), identity)

  def describe(
      id: DatasetId,
      shape: DatasetShape,
      voxelDomain: VoxelDomain,
      metadata: DatasetMetadata,
      samplingFrame: SamplingFrame,
      runIds: Vector[RunId],
      events: DatasetEvents = DatasetEvents.Empty
  ): Either[DatasetError, FmriDataset] =
    for
      _ <-
        if voxelDomain.spatialSize == shape.spatialSize then Right(())
        else
          Left(DatasetError.ShapeMismatch(
            s"voxel domain has spatial size ${voxelDomain.spatialSize} but dataset shape has ${shape.spatialSize}"
          ))
      timeAxis <- DatasetTimeAxis.fromSamplingFrame(samplingFrame, runIds)
      _ <-
        if shape.timepoints == timeAxis.timepoints then Right(())
        else
          Left(
            DatasetError.ShapeMismatch(
              s"sampling frame has ${timeAxis.timepoints} timepoints but dataset shape has ${shape.timepoints}"
            )
          )
      _ <- events.validateAgainst(timeAxis)
    yield
      new FmriDataset(
        id,
        shape,
        voxelDomain,
        metadata,
        samplingFrame,
        events,
        timeAxis
      )

  def describe(
      id: DatasetId,
      shape: DatasetShape,
      voxelDomain: VoxelDomain,
      metadata: DatasetMetadata,
      samplingFrame: SamplingFrame,
      runId: RunId,
      events: DatasetEvents
  ): Either[DatasetError, FmriDataset] =
    describe(id, shape, voxelDomain, metadata, samplingFrame, Vector(runId), events)

  def describe(
      id: DatasetId,
      shape: DatasetShape,
      voxelDomain: VoxelDomain,
      metadata: DatasetMetadata,
      samplingFrame: SamplingFrame,
      runId: RunId
  ): Either[DatasetError, FmriDataset] =
    describe(id, shape, voxelDomain, metadata, samplingFrame, Vector(runId))

  def open(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runIds: Vector[RunId],
      events: DatasetEvents = DatasetEvents.Empty
  ): Either[DatasetError, SynchronousFmriDataset] =
    describe(
      backend.id,
      backend.shape,
      backend.voxelDomain,
      backend.metadata,
      samplingFrame,
      runIds,
      events
    ).map(SynchronousFmriDataset.fromChecked(_, backend))

  def open(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runId: RunId,
      events: DatasetEvents
  ): Either[DatasetError, SynchronousFmriDataset] =
    open(backend, samplingFrame, Vector(runId), events)

  def open(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runId: RunId
  ): Either[DatasetError, SynchronousFmriDataset] =
    open(backend, samplingFrame, Vector(runId))

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runIds: Vector[RunId],
      events: DatasetEvents = DatasetEvents.Empty
  ): SynchronousFmriDataset =
    open(backend, samplingFrame, runIds, events)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      events: DatasetEvents
  ): SynchronousFmriDataset =
    DatasetTimeAxis
      .fromSamplingFrame(samplingFrame)
      .flatMap(timeAxis => open(backend, samplingFrame, timeAxis.runIds, events))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame
  ): SynchronousFmriDataset =
    unsafe(backend, samplingFrame, DatasetEvents.Empty)

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runId: RunId,
      events: DatasetEvents
  ): SynchronousFmriDataset =
    unsafe(backend, samplingFrame, Vector(runId), events)

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runId: RunId
  ): SynchronousFmriDataset =
    unsafe(backend, samplingFrame, Vector(runId))

final class SynchronousFmriDataset private (
    description: FmriDataset,
    val backend: DatasetBackend
) extends FmriDataset(
      description.id,
      description.shape,
      description.voxelDomain,
      description.metadata,
      description.samplingFrame,
      description.events,
      description.timeAxis
    ),
    DatasetSeriesReader:
  val dataset: FmriDataset =
    this

  require(id == backend.id, "synchronous backend id must match dataset")
  require(shape == backend.shape, "synchronous backend shape must match dataset")
  require(
    voxelDomain.indices == backend.voxelDomain.indices,
    "synchronous backend voxel domain must match dataset"
  )

  def seriesEither(
      selection: DataSelection = DataSelection.All
  ): Either[DatasetError, FmriSeries] =
    backend.readEither(selection)

object SynchronousFmriDataset:
  private[dataset] def fromChecked(
      dataset: FmriDataset,
      backend: DatasetBackend
  ): SynchronousFmriDataset =
    new SynchronousFmriDataset(dataset, backend)

  def attach(
      dataset: FmriDataset,
      backend: DatasetBackend
  ): Either[DatasetError, SynchronousFmriDataset] =
    if dataset.id != backend.id then
      Left(DatasetError.DatasetIdentityMismatch(dataset.id, backend.id))
    else if dataset.shape != backend.shape then
      Left(DatasetError.ShapeMismatch(
        "synchronous backend shape does not match the dataset description"
      ))
    else if dataset.voxelDomain.indices != backend.voxelDomain.indices then
      Left(DatasetError.SampleOrderingMismatch(
        dataset.voxelDomain.indices,
        backend.voxelDomain.indices
      ))
    else
      Right(new SynchronousFmriDataset(dataset, backend))

  def readerFor(
      dataset: FmriDataset
  ): Either[DatasetError, SynchronousFmriDataset] =
    dataset match
      case synchronous: SynchronousFmriDataset =>
        Right(synchronous)
      case _ =>
        Left(DatasetError.SynchronousReaderNotFound(dataset.id))
