package scalafim.fmri.workflow

import bids4s.BidsTable
import scalafim.dataset.RunId

/** Raw companion samples. Units are absent unless declared by the source. */
final case class QcColumn(name: String, units: Option[String], values: Either[String,Vector[Option[Double]]])
final case class FirstLevelCompanionReview(run: RunId, location: Option[ArtifactLocation],
    sourceSha256: Option[String], columns: Vector[QcColumn], rows: Int):
  def available: Boolean = location.nonEmpty

object FirstLevelCompanionReview:
  def bind(run: RunId, expectedRows: Int, table: BidsTable,
      units: Map[String,String] = Map.empty): Either[WorkflowError,Vector[QcColumn]] =
    if expectedRows < 1 || table.nrows != expectedRows then
      Left(WorkflowError.InvalidRun(run.value,s"QC companion has ${table.nrows} rows; expected $expectedRows original scans"))
    else Right(table.columns.zipWithIndex.map { (name,index) =>
      val values = table.rows.zipWithIndex.foldLeft[Either[String,Vector[Option[Double]]]](Right(Vector.empty)) {
        case (result,(row,scan)) => result.flatMap { previous =>
          row(index) match
            case None => Right(previous :+ None)
            case Some(text) => text.toDoubleOption.filter(_.isFinite) match
              case Some(value) => Right(previous :+ Some(value))
              case None => Left(s"$name: source row ${scan+1} is not a finite number")
        }
      }
      QcColumn(name,units.get(name).map(_.trim).filter(_.nonEmpty),values)
    })
