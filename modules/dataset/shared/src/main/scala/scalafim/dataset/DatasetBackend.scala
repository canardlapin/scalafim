package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.Mask

trait DatasetBackend:
  def id: DatasetId
  def shape: DatasetShape
  def mask: Mask.MaskVol
  def metadata: DatasetMetadata
  def readEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries]
  def read(selection: DataSelection = DataSelection.All): FmriSeries =
    readEither(selection).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class FmriDataset(
    backend: DatasetBackend,
    samplingFrame: SamplingFrame,
    events: DatasetEvents = DatasetEvents.Empty
):
  require(samplingFrame.blockLens.sum == backend.shape.timepoints, "sampling frame rows must match dataset timepoints")

  def id: DatasetId = backend.id
  def shape: DatasetShape = backend.shape
  def metadata: DatasetMetadata = backend.metadata
  def seriesEither(selection: DataSelection = DataSelection.All): Either[DatasetError, FmriSeries] =
    backend.readEither(selection)
  def series(selection: DataSelection = DataSelection.All): FmriSeries =
    seriesEither(selection).fold(error => throw new IllegalArgumentException(error.message), identity)
