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

  val designBlock: DesignBlock =
    DesignBlock
      .fromModel(eventModel, baselineModel, nRows)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def nTimepoints: Int = nRows
  def designMatrix: Mat = designBlock.matrix
  def columnNames: Vector[String] = designBlock.columnNames
  def nPredictors: Int = designBlock.cols

object FmriModel:
  def make(
      eventModel: EventModel,
      baselineModel: BaselineModel,
      dataset: FmriDataset
  ): Either[ModelError, FmriModel] =
    val nRows = dataset.shape.timepoints
    DesignBlock
      .fromModel(eventModel, baselineModel, nRows)
      .map(_ => FmriModel(eventModel, baselineModel, dataset))
