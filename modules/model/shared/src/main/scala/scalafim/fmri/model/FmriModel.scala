package scalafim.fmri.model

import scalafim.dataset.FmriDataset
import scalafim.fmri.design.{DesignFingerprint, DesignSchema, RowLayout}
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
  require(eventModel.designSchema.rows.rows == nRows, "event RowLayout rows must match dataset timepoints")
  require(baselineModel.designSchema.rows.rows == nRows, "baseline RowLayout rows must match dataset timepoints")

  val designBlock: DesignBlock =
    DesignBlock
      .fromModel(eventModel, baselineModel, nRows)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def nTimepoints: Int = nRows
  def designMatrix: Mat = designBlock.matrix
  def columnNames: Vector[String] = designBlock.columnNames
  def nPredictors: Int = designBlock.cols
  def designSchema: Option[DesignSchema] = designBlock.schema
  def rowLayout: Option[RowLayout] = designBlock.schema.map(_.rows)
  def designFingerprint: Option[DesignFingerprint] = designBlock.designFingerprint

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
