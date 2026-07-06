package scalafim.fmri.model

import scalafim.dataset.FmriDataset
import scalafim.fmri.design.baseline.BaselineModel
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.linalg.Mat

final case class FmriModel(
    eventModel: EventModel,
    baselineModel: BaselineModel,
    dataset: FmriDataset
):
  private val nRows = dataset.shape.timepoints
  require(eventModel.designMatrix.rows == nRows, "event model rows must match dataset timepoints")
  require(baselineModel.designMatrix.rows == nRows, "baseline model rows must match dataset timepoints")

  def nTimepoints: Int = nRows
  def designMatrix: Mat = eventModel.designMatrix ++ baselineModel.designMatrix
  def columnNames: Vector[String] = eventModel.columnNames ++ baselineModel.columnNames
  def nPredictors: Int = columnNames.length
