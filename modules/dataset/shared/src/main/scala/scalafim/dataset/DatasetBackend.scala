package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.Mask

trait DatasetBackend:
  def id: DatasetId
  def shape: DatasetShape
  def mask: Mask.MaskVol
  def metadata: DatasetMetadata
  def read(selection: DataSelection = DataSelection.All): FmriSeries

final case class FmriDataset(
    backend: DatasetBackend,
    samplingFrame: SamplingFrame,
    events: DatasetEvents = DatasetEvents.Empty
):
  require(samplingFrame.blockLens.sum == backend.shape.timepoints, "sampling frame rows must match dataset timepoints")

  def id: DatasetId = backend.id
  def shape: DatasetShape = backend.shape
  def metadata: DatasetMetadata = backend.metadata
  def series(selection: DataSelection = DataSelection.All): FmriSeries =
    backend.read(selection)
